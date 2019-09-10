extern crate websocket;

use std::net::TcpListener;
use std::string::*;
use std::sync::{Arc, Mutex};
use std::thread;

use websocket as ws;
use websocket::header::WebSocketProtocol;
use websocket::message::Type;

use websocket::async::TcpListener as Atl;
use websocket::message::Message;
use websocket::sender;
use websocket::stream::Stream as WebSocketStream;
use websocket::stream::Stream;
use websocket::ws::Sender;


use websocket::sync::Server;
// use websocket::async::Server;

#[macro_use]
extern crate log;

// extern crate iron;

#[macro_use]
mod controls;
mod broadcaster;
pub mod control_updates;
pub mod json;
// mod server;
mod string_defaults;
mod util;

extern crate actix_web;

use control_updates as cu;
use util::{load_string, write_string};

extern crate serde_json;
use serde_json::Value;

extern crate failure;
use failure::Error as FError;

pub trait ControlUpdateProcessor: Send {
  fn on_update_received(&mut self, &cu::UpdateMsg, ci: &ControlInfo) -> ();
}

pub struct ControlInfo {
  cm: controls::ControlMap,
  cnm: controls::ControlNameMap,
  guijson: String,
}

impl ControlInfo {
  pub fn get_name(&self, id: &Vec<i32>) -> Option<String> {
    match self.cm.get(id) {
      Some(ctrl) => Some(String::from(ctrl.name())),
      _ => None,
    }
  }
}

pub struct ControlServer {
  ci: Arc<Mutex<ControlInfo>>,
  bc: broadcaster::Broadcaster,
}

impl ControlServer {
  fn get_cid_by_name(&self, name: &str) -> Option<Vec<i32>> {
    let guard = match self.ci.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };

    match guard.cnm.get(name) {
      Some(cid) => Some(cid.clone()),
      _ => None,
    }
  }

  pub fn get_name(&self, id: &Vec<i32>) -> Option<String> {
    let ci = match self.ci.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };

    match ci.cm.get(id) {
      Some(ctrl) => Some(String::from(ctrl.name())),
      _ => None,
    }
  }

  pub fn make_update_msg(&self, name: &str) -> Option<cu::UpdateMsg> {
    let guard = match self.ci.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };

    match guard.cnm.get(name) {
      Some(cid) => match guard.cm.get(cid) {
        Some(ctrl) => ctrl.empty_update(),
        None => None,
      },
      _ => None,
    }
  }
  pub fn update(&self, updmsg: &cu::UpdateMsg) {
    let mut ci = match self.ci.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };

    match ci.cm.get_mut(controls::get_um_id(&updmsg)) {
      Some(ctl) => {
        (*ctl).update(&updmsg);
        let val = json::encode_update_message(&updmsg);
        match serde_json::ser::to_string(&val) {
          Ok(s) => self.bc.broadcast(Message::text(s)),
          Err(_) => (),
        }
      }
      None => (),
    }
  }

  pub fn update_label(&self, name: &str, label: &str) {
    match self.get_cid_by_name(name) {
      Some(cid) => self.update(&cu::UpdateMsg::Label {
        control_id: cid,
        label: String::from(label),
      }),
      None => (),
    }
  }
  pub fn update_button(&self, name: &str, state: Option<cu::ButtonState>, label: Option<String>) {
    match self.get_cid_by_name(name) {
      Some(cid) => self.update(&cu::UpdateMsg::Button {
        control_id: cid,
        state: state,
        label: label,
      }),
      None => (),
    }
  }
  pub fn update_slider(
    &self,
    name: &str,
    state: Option<cu::SliderState>,
    location: Option<f64>,
    label: Option<String>,
  ) {
    match self.get_cid_by_name(name) {
      Some(cid) => self.update(&cu::UpdateMsg::Slider {
        control_id: cid,
        state: state,
        location: location,
        label: label,
      }),
      None => (),
    }
  }

  pub fn load_gui_string(&self, guistring: &str) -> Result<(), FError> {
    let guival = serde_json::from_str(guistring)?;

    let controltree = json::deserialize_root(&guival)?;
    println!("new control layout recieved!");

    println!(
      "title: {} count: {} ",
      controltree.title,
      controltree.root_control.control_type()
    );
    println!("controls: {:?}", controltree.root_control);

    let mut guard = match self.ci.lock() {
      Ok(guard) => guard,
      Err(poisoned) => poisoned.into_inner(),
    };

    (*guard).cm = controls::make_control_map(&*controltree.root_control);
    (*guard).cnm = controls::control_map_to_name_map(&(*guard).cm);
    (*guard).guijson = guistring.to_string();

    // send the updated gui string to all clients.
    self.bc.broadcast(Message::text(guistring.to_string()));

    Ok(())
  }
}

pub fn start_websocket_server<'a>(
  guistring: &str,
  cup: Box<ControlUpdateProcessor>,
  ip: &str,
  websockets_port: &str,
) -> Result<ControlServer, Box<std::error::Error>> {
  let mut websockets_ip = String::from(ip);
  websockets_ip.push_str(":");
  websockets_ip.push_str(&websockets_port);

  let guival: Value = try!(serde_json::from_str(guistring));

  let blah = try!(json::deserialize_root(&guival));

  println!(
    "title: {} rootcontroltype: {} ",
    blah.title,
    blah.root_control.control_type()
  );
  println!("controls: {:?}", blah.root_control);

  // from control tree, make a map of ids->controls.
  let mapp = controls::make_control_map(&*blah.root_control);
  let cnm = controls::control_map_to_name_map(&mapp);

  let ci = ControlInfo {
    cm: mapp,
    cnm: cnm,
    guijson: String::new() + guistring,
  };

  let cmshare = Arc::new(Mutex::new(ci));
  let wscmshare = cmshare.clone();
  // for sending, bind to this.  if we bind to localhost, we can't
  // send messages to other machines.
  let bc = broadcaster::Broadcaster::new();
  let wsbc = bc.clone();

  let cs_ret = ControlServer {
    ci: cmshare,
    bc: bc,
  };

  // Spawn a thread for the websockets handler.
  thread::spawn(move || {
    match websockets_main(websockets_ip, wscmshare, wsbc, Arc::new(Mutex::new(cup))) {
      Ok(_) => (),
      Err(e) => println!("error in websockets_main: {:?}", e),
    }
  });

  Ok(cs_ret)
}

// need to lock the control structs and stuff, refresh them, then send out the
// updates.

fn stringify(x: u32) -> String { format!("error code: {:?}", x) }
// TODO: refactor to return a (rx/sx) pair for sending, recieving messages.
// library users start the websockets_main and get that pair of things.
// then, can send the various control structs and receive the messages.
fn websockets_main(
  ipaddr: String,
  ci: Arc<Mutex<ControlInfo>>,
  broadcaster: broadcaster::Broadcaster,
  cup: Arc<Mutex<Box<ControlUpdateProcessor>>>,
) -> Result<(), Box<std::error::Error>> {
  let server = Server::bind(&ipaddr[..])?;
  // let server : WsServer<websocket::server::NoTlsAcceptor, std::net::TcpListener>= try!(WsServer::bind(&ipaddr[..]));

  for connection in server {
    // Spawn a new thread for each connection.
    println!("new websockets connection!");
    let half = connection.map_err(|x| format!("error {:?}", x)) ;
    let conn = half?.accept().map_err(|s| format!("wat: {:?}",s))?;
    let sci = ci.clone();
    let broadcaster = broadcaster.clone();
    let cup = cup.clone();
    thread::spawn(
      move || match websockets_client(conn, sci, broadcaster, cup) {
        Ok(_) => (),
        Err(e) => {
          println!("error in websockets thread: {:?}", e);
          ()
        }
      },
    );
  }

  Ok(())
}

fn websockets_client(
  connection: websocket::sync::Client<std::net::TcpStream>,
  ci: Arc<Mutex<ControlInfo>>,
  mut broadcaster: broadcaster::Broadcaster,
  cup: Arc<Mutex<Box<ControlUpdateProcessor>>>,
) -> Result<(), Box<std::error::Error>> {
  // Get the request
  let request = try!(connection.read_request());
  // Keep the headers so we can check them
  let headers = request.headers.clone();

  try!(request.validate()); // Validate the request

  let mut response = request.accept(); // Form a response

  if let Some(&WebSocketProtocol(ref protocols)) = headers.get() {
    if protocols.contains(&("rust-websocket".to_string())) {
      // We have a protocol we want to use
      response
        .headers
        .set(WebSocketProtocol(vec!["rust-websocket".to_string()]));
    }
  }

  let mut client = try!(response.send()); // Send the response

  let ip = try!(client.get_mut_sender().get_mut().peer_addr());

  println!("Websocket connection from {}", ip);

  // send up the json of the current controls.
  {
    let sci = ci.lock().unwrap();

    let updarray = controls::cm_to_update_array(&sci.cm);

    // build json message containing both guijson and the updarray.
    let mut updvals = Vec::new();

    for upd in updarray {
      let um = json::encode_update_message(&upd);
      updvals.push(um);
    }

    let mut guival: Value = try!(serde_json::from_str(&sci.guijson[..]));

    match guival.as_object_mut() {
      Some(obj) => {
        obj.insert("state".to_string(), Value::Array(updvals));
        ()
      }
      None => (),
    }

    let guistring = try!(serde_json::ser::to_string(&guival));
    let message = Message::text(guistring);
    try!(client.send_message(&message));
  }

  let (sender, mut receiver) = client.split();

  let sendmeh = Arc::new(Mutex::new(sender));

  broadcaster.register(sendmeh.clone());

  for msg in receiver.incoming_messages() {
    let message: Message = try!(msg);
    // println!("message: {:?}", message);

    match message.opcode {
      Type::Close => {
        let message = Message::close();
        // let mut sender = try!(sendmeh.lock());
        let mut sender = sendmeh.lock().unwrap();
        try!(sender.send_message(&mut *sender, &message));
        println!("Client {} disconnected", ip);
        return Ok(());
      }
      Type::Ping => {
        println!("Message::Ping(data)");
        let message = Message::pong(message.payload);
        let mut sender = sendmeh.lock().unwrap();
        try!(sender.send_message(*sender, &message));
      }
      Type::Text => {
        let u8 = message.payload.to_owned();
        let str = try!(std::str::from_utf8(&*u8));
        let jsonval: Value = try!(serde_json::from_str(str));
        let s_um = json::decode_update_message(&jsonval);
        match s_um {
          Some(updmsg) => {
            {
              let mut sci = ci.lock().unwrap();
              {
                let mbcntrl = sci.cm.get_mut(controls::get_um_id(&updmsg));
                match mbcntrl {
                  Some(cntrl) => {
                    (*cntrl).update(&updmsg);
                    broadcaster.broadcast_others(&ip, Message::text(str));
                    ()
                  }
                  None => println!("none"),
                }
              }
            }
            let mut scup = cup.lock().unwrap();
            let sci = match ci.lock() {
              Ok(sci) => sci,
              Err(poisoned) => poisoned.into_inner(),
            };

            scup.on_update_received(&updmsg, &*sci);
          }
          _ => println!(
            "decode_update_message failed on websockets msg: {:?}",
            message
          ),
        }
      }
      _ => {
        println!("unknown websockets msg: {:?}", message);
      }
    }
  }

  Ok(())
}
