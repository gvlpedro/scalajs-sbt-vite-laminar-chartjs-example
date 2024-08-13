package testvite

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.{RTCDataChannelInit, RTCIceCandidate, RTCSessionDescriptionInit, RTCConfiguration, Event, MessageEvent}
import org.scalajs.dom.experimental.webrtc.*
import scala.scalajs.js.timers.*
import scala.scalajs.js.JSON
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object Chat {
  val receivedMessages = Var(List("first message by default"))
  val currentMessage = Var("")
  var isDataChannelOpen = Var(false)

  val defaultRoomId = "999"

  def render(): HtmlElement = {
    val btnStyle="padding:5px; color:white; background-color: #222327; margin-right:4px; border-radius: 10px; cursor:pointer"

    div(
      styleAttr := "color:black",
      h1(s"Room $defaultRoomId"),
      div(
        button(styleAttr := btnStyle,"Init room", onClick --> (_ => createRoom())),
        button(styleAttr := btnStyle, "Join Room", onClick --> (_ => joinRoom())),
        div(
          label("Message: "),
          input(
            styleAttr := "color: #222327",
            value <-- currentMessage.signal,
            onInput.mapToValue --> (msg => currentMessage.set(msg))
          ),
          button(styleAttr := btnStyle, "Send Message", onClick --> (_ => sendMessage()))
        ),
        div(
          styleAttr := "color: #222327",
          h3("Messages:"),
          pre(
            child.text <-- receivedMessages.signal.map(_.mkString("\n"))
          )
        )
      )
    )
  }

  def saveRoomToStorage(roomId: String, sdp: RTCSessionDescriptionInit): Unit = {
    val storedRooms = dom.window.localStorage.getItem("rooms")
    val updatedRooms = storedRooms match {
      case null => Map(roomId -> sdp)
      case data =>
        val rooms = JSON.parse(data).asInstanceOf[js.Dictionary[js.Any]].toMap
        rooms + (roomId -> sdp)
    }
    dom.window.localStorage.setItem("rooms", JSON.stringify(updatedRooms.toJSDictionary))
  }

  def getRoomFromStorage(roomId: String): Option[RTCSessionDescriptionInit] = {
    val storedRooms = dom.window.localStorage.getItem("rooms")
    if (storedRooms != null) {
      val rooms = JSON.parse(storedRooms).asInstanceOf[js.Dictionary[js.Dynamic]]
      rooms.get(roomId).map { data =>
        new RTCSessionDescriptionInit {
          sdp = data.sdp.toString
          `type` = data.`type`.toString.asInstanceOf[dom.RTCSdpType]
        }
      }
    } else {
      None
    }
  }

  def listRoomsInStorage(): Unit = {
    val storedRooms = dom.window.localStorage.getItem("rooms")
    if (storedRooms != null) {
      val rooms = JSON.parse(storedRooms).asInstanceOf[js.Dictionary[js.Any]]
      println(s"Available rooms: ${rooms.keys.mkString(", ")}")
    } else {
      println("No rooms available.")
    }
  }

  val rtcConfig = js.Dynamic.literal().asInstanceOf[js.UndefOr[RTCConfiguration]]
  val peerConnection = new dom.RTCPeerConnection(rtcConfig)
  var dataChannel: dom.RTCDataChannel = _

  def createRoom(): Unit = {
    println("Creating room...")
    val dataChannelInit = new RTCDataChannelInit {}
    dataChannel = peerConnection.createDataChannel("chat", dataChannelInit)
    setupDataChannel()

    peerConnection.createOffer().toFuture.foreach { offer =>
      println("Offer created, setting local description.")
      peerConnection.setLocalDescription(offer)
      val offerInit = new RTCSessionDescriptionInit {
        sdp = offer.sdp
        `type` = offer.`type`
      }
      saveRoomToStorage(defaultRoomId, offerInit)
      println(s"Stored offer in localStorage for room $defaultRoomId")
      dom.window.alert(s"Room created with ID: $defaultRoomId")
      listRoomsInStorage()
    }

    peerConnection.onicecandidate = (event: Event) => {
      val iceCandidateEvent = event.asInstanceOf[js.Dynamic]
      if (!js.isUndefined(iceCandidateEvent.candidate) && iceCandidateEvent.candidate != null) {
        val iceCandidate = iceCandidateEvent.candidate.asInstanceOf[RTCIceCandidate]
        println(s"ICE candidate: ${iceCandidate.candidate}")
      } else {
        println("No more ICE candidates.")
      }
    }
  }

  def joinRoom(): Unit = {
    println(s"Attempting to join room ID $defaultRoomId")
    listRoomsInStorage()

    getRoomFromStorage(defaultRoomId) match {
      case Some(remoteOffer) =>
        val description = new dom.RTCSessionDescription(remoteOffer)
        peerConnection.setRemoteDescription(description).toFuture.flatMap { _ =>
          println("Remote description set, creating answer.")
          peerConnection.createAnswer().toFuture
        }.foreach { answer =>
          println("Answer created, setting local description.")
          peerConnection.setLocalDescription(answer)
          val answerInit = new RTCSessionDescriptionInit {
            sdp = answer.sdp
            `type` = answer.`type`
          }
          saveRoomToStorage(defaultRoomId, answerInit)
        }

        peerConnection.ondatachannel = (event: Event) => {
          val dataChannelEvent = event.asInstanceOf[js.Dynamic]
          dataChannel = dataChannelEvent.channel.asInstanceOf[dom.RTCDataChannel]
          setupDataChannel()
        }

        peerConnection.onicecandidate = (event: Event) => {
          val iceCandidateEvent = event.asInstanceOf[js.Dynamic]
          if (js.isUndefined(iceCandidateEvent.candidate) || iceCandidateEvent.candidate == null) {
            println("No more ICE candidates.")
          } else {
            val iceCandidate = iceCandidateEvent.candidate.asInstanceOf[RTCIceCandidate]
            println(s"ICE candidate: ${iceCandidate.candidate}")
          }
        }

      case None =>
        dom.window.alert(s"Room ID $defaultRoomId does not exist.")
    }
  }

  def setupDataChannel(): Unit = {
    dataChannel.onopen = { _ =>
      println("Data channel is open!")
      isDataChannelOpen.set(true)
    }
    dataChannel.onclose = { _ =>
      println("Data channel is closed!")
      isDataChannelOpen.set(false)
    }
    dataChannel.onmessage = (event: dom.MessageEvent) => {
      val message = event.data.toString
      println(s"Message received: $message")
      receivedMessages.update(_ :+ message)
    }
  }

  def sendMessage(): Unit = {
    val message = currentMessage.now()
    if (message.nonEmpty) {
      setTimeout(1000) { // wait 1 sec
        if (isDataChannelOpen.now()) {
          println(s"Trying to send message: $message")
          dataChannel.send(message)
          receivedMessages.update(_ :+ s"Me: $message")
          currentMessage.set("")
        } else {
          dom.window.alert("Data channel is not open or message is empty!")
        }
      }
    }
  }
}
