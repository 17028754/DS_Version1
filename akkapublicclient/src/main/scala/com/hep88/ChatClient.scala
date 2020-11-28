package com.hep88
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, PostStop}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.cluster.typed._
import akka.{actor => classic}
import akka.discovery.{Discovery, Lookup, ServiceDiscovery}
import akka.discovery.ServiceDiscovery.Resolved
import akka.actor.typed.scaladsl.adapter._
import com.hep88.protocol.JsonSerializable
import scalafx.collections.ObservableHashSet
import scalafx.application.Platform
import akka.cluster.ClusterEvent.ReachabilityEvent
import akka.cluster.ClusterEvent.ReachableMember
import akka.cluster.ClusterEvent.UnreachableMember
import akka.cluster.ClusterEvent.MemberEvent
import akka.actor.Address
import com.hep88.model.GaiaGame
import scalafx.scene.shape.Rectangle
import scalafxml.core.{FXMLLoader, NoDependencyResolver}


object ChatClient {
    sealed trait Command extends JsonSerializable
    //internal protocol
    case object start extends Command
    case class StartJoin(name: String) extends Command

    // Help start to check for omission during game
    case class GameOmission(name1: String, target1: ActorRef[ChatClient.Command], name2: String, target2: ActorRef[ChatClient.Command]) extends Command

    // Sending and receive invitation
    final case class SendInvitation(target: ActorRef[ChatClient.Command], name: String) extends Command
    final case class ReceiveInvitation(name: String, from: ActorRef[ChatClient.Command]) extends Command
    // Accept and rejecting invitation
    final case class AcceptInvitation(target: ActorRef[ChatClient.Command]) extends Command
    final case class RejectInvitation(target: ActorRef[ChatClient.Command]) extends Command
    // Display accepted/rejected invitation
    final case class DisplayInvitationResult(result: Boolean) extends Command

    // Starting game
    final case class StartGame(target: ActorRef[ChatClient.Command]) extends Command
    final case class ClientStartR(target: ActorRef[ChatClient.Command]) extends Command



  // Pausing game
  final case class OwnPause(target: ActorRef[ChatClient.Command]) extends Command
  final case class OpponentPause(target: ActorRef[ChatClient.Command]) extends Command
  // Unpause game
  final case class OwnUnpause(target: ActorRef[ChatClient.Command]) extends Command
  final case class OpponentUnpause(target: ActorRef[ChatClient.Command]) extends Command

  // Game over 1st part
  final case class SendGameOver(target: ActorRef[ChatClient.Command], gameOver: Boolean, score: Int) extends Command
  final case class ReceiveGameOver(score: Int, gameOver: Boolean, target: ActorRef[ChatClient.Command]) extends Command
  // Game over 2nd part
  final case class SendGameOverLast(target: ActorRef[ChatClient.Command], score: Int) extends Command
  final case class ReceiveGameOverLast(score: Int, target: ActorRef[ChatClient.Command]) extends Command

  // Update score
  final case class PutScore(target: ActorRef[ChatClient.Command], score: String) extends Command
  final case class TakeScore(score: String, target: ActorRef[ChatClient.Command]) extends Command

  // Notify client about an omission occurrence during game
  final case class TerminateGame() extends Command

  // Remove omission list in server
  case class GameCompleted(name1: String, target1: ActorRef[ChatClient.Command], name2: String, target2: ActorRef[ChatClient.Command]) extends Command

  // Animation for next piece - tell other client (part 1)
  final case class TellNextPiece(target: ActorRef[ChatClient.Command], nextPiece: List[List[Array[Int]]]) extends Command
  final case class ReceiveNextPiece(nextPiece: List[List[Array[Int]]]) extends Command

  // Animation for next piece - tell other client (part 2)
  final case class ClearNextPiece(target: ActorRef[ChatClient.Command]) extends Command
  final case class ReceiveClearNextPiece() extends Command

  // Animation for board - tell other client
  final case class CurrentBoardPiece(target: ActorRef[ChatClient.Command], currentPiece: List[Array[Int]], currentX: Int, currentY: Int) extends Command
  final case class ReceiveCurrentBoardPiece(currentPiece: List[Array[Int]], currentX: Int, currentY: Int) extends Command

  // Animation for board - tell other client refresh board
  final case class RefreshEnemyBoard(target: ActorRef[ChatClient.Command], board: Array[Array[Int]]) extends Command
  final case class ReceiveRefreshEnemyBoard(board: Array[Array[Int]]) extends Command

  // Animation for board - tell other client to paint piece to board
  final case class PaintEnemyBoardPiece(target: ActorRef[ChatClient.Command], currentPiece: List[Array[Int]], currentX: Int, currentY: Int) extends Command
  final case class ReceiveEnemyBoardPiece(currentPiece: List[Array[Int]], currentX: Int, currentY: Int) extends Command

  // Animation for board - tell other client to clear piece from board
  final case class ClearEnemyBoardPiece(target: ActorRef[ChatClient.Command], currentPiece: List[Array[Int]], currentX: Int, currentY: Int, board: Array[Array[Int]]) extends Command
  final case class ReceiveClearEnemyBoardPiece(currentPiece: List[Array[Int]], currentX: Int, currentY: Int, board: Array[Array[Int]]) extends Command


  final case object FindTheServer extends Command
  private case class ListingResponse(listing: Receptionist.Listing) extends Command
  private final case class MemberChange(event: MemberEvent) extends Command
  private final case class ReachabilityChange(reachabilityEvent: ReachabilityEvent) extends Command
  val members = new ObservableHashSet[User]()

  val unreachables = new ObservableHashSet[Address]()
    unreachables.onChange{(ns, _) =>
        Platform.runLater {
            Client.control.updateList(members.toList.filter(y => ! unreachables.exists (x => x == y.ref.path.address)))
        }
    }

  members.onChange{(ns, _) =>
    Platform.runLater {
        Client.control.updateList(ns.toList.filter(y => ! unreachables.exists (x => x == y.ref.path.address)))
    }  
  }




//chat protocol
  final case class MemberList(list: Iterable[User]) extends Command
  final case class Joined(list: Iterable[User]) extends Command

  var defaultBehavior: Option[Behavior[ChatClient.Command]] = None
  var remoteOpt: Option[ActorRef[ChatServer.Command]] = None 
  var nameOpt: Option[String] = None

    def messageStarted(): Behavior[ChatClient.Command] = Behaviors.receive[ChatClient.Command] { (context, message) => 
        message match {
            // Sending invitation
            case SendInvitation(target, name) =>
                target ! ReceiveInvitation(name, context.self)
                Behaviors.same
            // Receiving invitation
            case ReceiveInvitation(name, from) =>
                Platform.runLater {
                    Client.control.receiveInvitation(name, from)
                }
                Behaviors.same
            // Accepting invitation
            case AcceptInvitation(target) =>
              target ! DisplayInvitationResult(true)
              Behaviors.same
             // Rejecting invitation
            case RejectInvitation(target) =>
              target ! DisplayInvitationResult(false)
              Behaviors.same
            // Display invitation result
            case DisplayInvitationResult(result) =>
              Platform.runLater{
                Client.control.displayInvitationResult(result)
              }
              Behaviors.same

            // Starting game
            case StartGame(target) =>
              target ! ClientStartR(context.self)
              Platform.runLater(
                Client.control.loadGame()
              )
              Behaviors.same
            // Receive command to start game also
            case ClientStartR(from)=>
              Platform.runLater(
                Client.control.loadGame()
              )
              Behaviors.same



         // Client's behaviour during a game
            // Pause game
            case OwnPause(target) =>
              target ! OpponentPause(context.self)
              Behaviors.same
            case OpponentPause(target) =>
              Platform.runLater{
                  Client1.control.pauseFromOther()
              }
            Behaviors.same

            // Unpause game
            case OwnUnpause(target) =>
              target ! OpponentUnpause(context.self)
              Behaviors.same
            case OpponentUnpause(target) =>
              Platform.runLater{
                Client1.control.unpauseFromOther()
              }
              Behaviors.same

            // Updating score behaviour
            case PutScore(target, score) =>
              target ! TakeScore(score, context.self)
              Behaviors.same
            case TakeScore(score, target) =>
              Platform.runLater{
                  Client1.control.addScore(score)
              }
              Behaviors.same

            // Game over behaviour
            case SendGameOver(target, gameOver, score) =>
            target ! ReceiveGameOver(score, gameOver, target)
            Behaviors.same
            case ReceiveGameOver(score, gameOver, target) =>
            Platform.runLater{
                  Client1.control.updateGameStatus(score, gameOver)
            }
            Behaviors.same
            case SendGameOverLast(target, score) =>
              target ! ReceiveGameOverLast(score, target)
              Behaviors.same
            case ReceiveGameOverLast(score, target) =>
              Platform.runLater{
                  Client1.control.gameOverFinal(score)
              }
              Behaviors.same

              // Omission during game
            case GameOmission(name1, target1, name2, target2) =>
              remoteOpt.map ( _! ChatServer.JoinGame(name1, target1, name2, target2))
              Behaviors.same
            case TerminateGame() =>
              Platform.runLater{
                  Client1.control.omissionOccured()
              }
              Behaviors.same
            case GameCompleted(name1, target1, name2, target2) =>
              remoteOpt.map ( _! ChatServer.GameCompleted(name1, target1, name2, target2))
              Behaviors.same




              // Animation - tell next piece
            case TellNextPiece(target, nextPiece)  =>
              target ! ReceiveNextPiece(nextPiece)
              Behaviors.same
            case ReceiveNextPiece(nextPiece) =>
              Platform.runLater{
                Client1.control.receiveEnemyNextPiece(nextPiece)
              }
              Behaviors.same
            case ClearNextPiece(target) =>
              target ! ReceiveClearNextPiece()
              Behaviors.same
            case ReceiveClearNextPiece() =>
              Platform.runLater{
                Client1.control.clearEnemyNextPiece()
              }
              Behaviors.same

             // Animation - tell current piece on board (first piece painted on the very top)
            case CurrentBoardPiece(target, current, x, y) =>
              target ! ReceiveCurrentBoardPiece(current,x ,y)
              Behaviors.same
            case ReceiveCurrentBoardPiece(current,x ,y) =>
              Platform.runLater{
                Client1.control.printEnemyCurrentPiece1(current,x ,y)
              }
              Behaviors.same

              // Animation - tell client to refresh board
            case RefreshEnemyBoard(target, board) =>
              target ! ReceiveRefreshEnemyBoard(board)
              Behaviors.same
            case ReceiveRefreshEnemyBoard(board) =>
              Platform.runLater{
                Client1.control.refreshEnemyBoard(board)
              }
              Behaviors.same

              // Animation - tell client to paint piece to board
            case PaintEnemyBoardPiece(target, currentPiece, currentX, currentY) =>
              target ! ReceiveEnemyBoardPiece(currentPiece, currentX, currentY)
              Behaviors.same
            case ReceiveEnemyBoardPiece(currentPiece, x, y) =>
              Platform.runLater{
                Client1.control.paintEnemyPiece(currentPiece, x, y)
              }
              Behaviors.same

             // Animation - tell client to clear piece from board
            case ClearEnemyBoardPiece(target, currentPiece, currentX, currentY, board) =>
              target ! ReceiveClearEnemyBoardPiece(currentPiece, currentX, currentY, board)
              Behaviors.same
            case ReceiveClearEnemyBoardPiece(currentPiece, currentX, currentY, board) =>
              Platform.runLater{
                Client1.control.clearEnemyPiece(currentPiece, currentX, currentY, board)
              }
              Behaviors.same





              // Updating the list of players connected in the game lobby
            case MemberList(list: Iterable[User]) =>
                members.clear()
                members ++= list
                Behaviors.same
        }
    }.receiveSignal {
        case (context, PostStop) =>
            for (name <- nameOpt;
                remote <- remoteOpt){
            remote ! ChatServer.Leave(name, context.self)
            }
            defaultBehavior.getOrElse(Behaviors.same)
    }

    def apply(): Behavior[ChatClient.Command] =
        Behaviors.setup { context =>
        var counter = 0
        // (1) a ServiceKey is a unique identifier for this actor

        Upnp.bindPort(context)
        
          
    val reachabilityAdapter = context.messageAdapter(ReachabilityChange)
    Cluster(context.system).subscriptions ! Subscribe(reachabilityAdapter, classOf[ReachabilityEvent])

       // (2) create an ActorRef that can be thought of as a Receptionist
        // Listing “adapter.” this will be used in the next line of code.
        // the ChatClient.ListingResponse(listing) part of the code tells the
        // Receptionist how to get back in touch with us after we contact
        // it in Step 4 below.
        // also, this line of code is long, so i wrapped it onto two lines
        val listingAdapter: ActorRef[Receptionist.Listing] =
            context.messageAdapter { listing =>
                println(s"listingAdapter:listing: ${listing.toString}")
                ChatClient.ListingResponse(listing)
            }
        //(3) send a message to the Receptionist saying that we want
        // to subscribe to events related to ServerHello.ServerKey, which
        // represents the ChatClient actor.
        context.system.receptionist ! Receptionist.Subscribe(ChatServer.ServerKey, listingAdapter)
        //context.actorOf(RemoteRouterConfig(RoundRobinPool(5), addresses).props(Props[ChatClient.TestActorClassic]()), "testA")
        defaultBehavior = Some(Behaviors.receiveMessage { message =>
            message match {
                case ChatClient.start =>

                    context.self ! FindTheServer 
                    Behaviors.same
                // (4) send a Find message to the Receptionist, saying
                    // that we want to find any/all listings related to
                    // Mouth.MouthKey, i.e., the Mouth actor.
                case FindTheServer =>
                    println(s"Client Hello: got a FindTheServer message")
                    context.system.receptionist !
                        Receptionist.Find(ChatServer.ServerKey, listingAdapter)

                    Behaviors.same
                    // (5) after Step 4, the Receptionist sends us this
                    // ListingResponse message. the `listings` variable is
                    // a Set of ActorRef of type ServerHello.Command, which
                    // you can interpret as “a set of ServerHello ActorRefs.” for
                    // this example i know that there will be at most one
                    // ServerHello actor, but in other cases there may be more
                    // than one actor in this set.
                case ListingResponse(ChatServer.ServerKey.Listing(listings)) =>
                    val xs: Set[ActorRef[ChatServer.Command]] = listings
                    for (x <- xs) {
                        remoteOpt = Some(x)
                    }
                    Behaviors.same
                case StartJoin(name) =>
                    nameOpt = Option(name)
                    remoteOpt.map ( _ ! ChatServer.JoinChat(name, context.self))
                     Behaviors.same
                case ChatClient.Joined(x) =>
                    Platform.runLater {
                        Client.control.displayStatus("Joined Server")
                    }
                    members.clear()
                    members ++= x
                    messageStarted()

                case ReachabilityChange(reachabilityEvent) =>
                reachabilityEvent match {
                    case UnreachableMember(member) =>
                        unreachables += member.address
                        Behaviors.same
                    case ReachableMember(member) =>
                        unreachables -= member.address
                        Behaviors.same
                }                    
                case _=>
                    Behaviors.unhandled
                
            }
        })
        defaultBehavior.get
    }
}
