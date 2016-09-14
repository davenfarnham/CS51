(* CS51
 * Section 8 - Events *)

(******************************************************************************)
(*          PART 0 : Events and Listeners                                     *)
(******************************************************************************)

(* Often, we want a program to react to things happening in our environment
 * rather than just execute a set of sequential instructions.  This paradigm
 * of "reacting to things happening in our environment" is called reactive
 * programming. Another way to think of reactive programming is to think of "a
 * thing happening" as a means for one thread to communicate to another thread.
 *
 * For example, consider a multi-threaded graphics-drawing application.  If one
 * of the threads has finished drawing an outline of a circle in the graphic, it
 * may want to tell other threads that the circle has been drawn so that those
 * other threads can color the circle. The first thread can send out a signal
 * to the application saying "Hey guys, I'm done drawing the circle!"  In other
 * words, the given thread has just communicated to other threads that it has
 * done something, so other threads should now react appropriately (hopefully
 * by coloring the circle). The coloring threads will be waiting for this
 * signal to occur; once they get the "Hey, I'm done" signal, the coloring
 * threads then color the circle.
 *
 * The idea of interpreting reactive programming as inter-thread communication
 * is at the heart of events. EVENTS are defined as the signals that threads
 * send out to an application, indicating to other threads that something has
 * happened and that they should react appropriately.  In order to send out
 * a signal, a thread creates a new event and "fires it," meaning that it
 * broadcasts the signal to the rest of the application.
 *
 * In our graphics example, not all of the threads need to hear that a thread
 * has finished drawing a circle.  (For example, do the square-coloring threads
 * care if some thread has drawn a circle?)  Instead, only the threads that need
 * to know that a circle has been drawn should be notified.
 *
 * In Ocaml, threads can tell the application that they would like to be
 * notified when a particular type of event has been fired, and what actions
 * they would like to take in response to that event.  This is where LISTENERS
 * come in.  Threads can add event listeners to an event, specifying for each
 * listener an EVENT HANDLER, or a method that should be executed when the event
 * is fired.  Whenever an events is fired, the threads that added listeners to
 * it are notified and execute the event handler immediately as a result.
 *
 * The three operations of events (creating events, firing events, and adding
 * listeners for events) make inter-thread communication easy and clean.  Much
 * more complex communication can be achieved by combining these operations.
 * See lecture notes for some examples of this.
 *)

(* AnnoyingFacebookFriend (inspired by javaworld.com) *)

module type EVENT =
sig
  type id
  type 'a event
  type status = Lyrics of string | RelationshipUpdate of string
		| CAPS of string | HashTag of string
  type commentType = Scumbag of string
                     | ContinuesSong of string | Agreement of string
                     | Song of string | Relationship of string
                     | Caps of string | HashTag of string
                     | Troll of string
  type response = Comment of commentType | Like

  val new_event : unit -> 'a event
  val add_listener : 'a event -> ('a -> unit) -> id

  (* Similar to add_listener, but the handler is only called the first time the
   * event is fired. *)
  val add_one_shot : 'a event -> ('a -> unit) -> id

  (* Removes a listener identified by id from the event. *)
  val remove_listener : 'a event -> id -> unit

  (* Signals that the event has occurred *)
  val fire_event : 'a event -> 'a -> unit

  (* Some built-in events for Facebook activities. *)
  val status_event : status event
  val two_statuses_event : (status * status) event
  val response_event : response event

  val terminate : unit -> 'a

  val run : (unit -> unit) -> unit
end

(* We're not going to spend time in section going over a concrete implementation
 * of the EVENT signature, but one implementation of a similar signature can be
 * found in lecture *)
module Event : EVENT = ... end

(* Facebook World *)
(* Annoying Facebook Friend posts statuses to Facebook, and their friends either
 * comment on them or like them. Their friends include Scumbag Steve, Best Friend,
 * Good Guy Greg, and Troll Friend. *)
module Facebook =
struct
  (* Passing the function facebook a unit value will set off a chain of events:
   * we add listeners to status_event, response_event, and two_statuses_event, and
   * then we fire the first status_event. *)
  let facebook () =
    Event.run
      (fun () ->
        (
          ignore (Event.add_listener Event.status_event scumbagSteve);
          ignore (Event.add_listener Event.status_event bestFriend);
          ignore (Event.add_listener Event.status_event goodGuyGreg);
          ignore (Event.add_listener Event.two_statuses_event trollFriend);
          ignore (Event.add_listener Event.response_event AnnoyingFacebookFriend);
          Event.fire_event status_event
            (HashTag ("Too proud of my bestest bud in the entire world"
                  ^ "#sisters #smartypants #gettingudrunkkkkkkTONIGHT"))
        )
      )

  (* Exercise 1 *)
  (* Write the event handlers for each of the members of Facebook, according to
   * the description *)

  (* Exercise 1.1 *)
  (* Annoying Facebook Friend should post a status anytime they receives a response
   * to one of their statuses. They also likes to combine their statuses sometimes,
   * just for fun. Hint: one way to implement this is to make Annoying Facebook
   * Friend post a different status for each kind of response. *)
  let AnnoyingFacebookFriend (r: response) : unit = ???









  (* Exercise 1.2 *)
  (* Scumbag Steve only responds to Annoying Facebook Friend in a scumbag way when
   * they posts a relationship status update. Otherwise, he ignores them. *)
  let scumbagSteve (s: status) : unit = ???










  (* Exercise 1.3 *)
  (* Good Guy Greg always responds to Annoying Facebook Friend's
   * status updates, putting lots of thought into each comment. *)
  let goodGuyGreg (s: status) : unit = ???








  (* Exercise 1.4 *)
  (* Annoying Facebook Friend's best friend likes all of their statuses, regardless
   * of content (since they're bffs, of course).  They like to finish off their
   * friend's song lyrics status updates, and they tends to agree with all the
   * rest of their friend's status updates. *)
  let bestFriend (s: status) : unit = ???









  (* Exercise 1.5 *)
  (* Troll Friend trolls Annoying Facebook Friends whenever they combine different
   * status types. *)
  let trollFriend (s : status * status) : unit = ???









end
