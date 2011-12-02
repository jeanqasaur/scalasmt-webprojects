package cap.scalasmt.webapp

import java.util._
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.collection.mutable.Set

class Session() {
	private var loggedIn : User = null
	private var users : Set[User] = Set() // This should be a database of users

	def login(userName:String, password:String):Boolean = {
		var t = false
		for (u <- users) {
			if (u.username.compareTo(userName) == 0) {
				if (u.validate(password) == true) {
					loggedIn = u
					t = true
				}
			}
		}
		return t
	}
}

class User(var username:String) {
	private var calendars : HashMap[String, Calendar] = new HashMap[String, Calendar]
	private var password : String = null

	def md5(s:String):String = {
		new String(MessageDigest.getInstance("MD5").digest(s.getBytes))
	}

	def setPassword(pass:String) {
		password = md5(pass)
	}

	def validate(pass:String) = {
		password == md5(pass)
	}
	
	def makeCalendar(name:String) {
		calendars += name -> new Calendar(name)
	}
}

class Calendar(var title:String) {
	private var events : HashMap[Int, Event] = new HashMap[Int, Event]
	private var canView : Set[User] = Set()
	private var numEvents : Int = 0 // Unique ID, also tracks the total number of events added

	def addAllowedUser(u:User) {
		canView += u
	}

	def removeAllowedUser(u:User) {
		canView -= u
	}

	def addEvent(e:Event) {
		events += numEvents -> e
		numEvents += 1
	}

	def showAllEvents() {
		for ((id,event) <- events){
			println(id + " " + event.toString())
		}
	}

	def showEventsOnDate(d:Date) {
		for ((id,event) <- events) {
			if (event.date.compareTo(d) == 0)
				println(id + " " + event.toString())
		}
	}

	def removeEvent(e:Event) {
		var removeID = -1
		for ((id,event) <- events) {
			if (event.compareTo(e) == 0) removeID = id
		}
		events -= removeID
	}

	def removeEvent(id:Int) {
		events -= id
	}
}

class Event(var title:String, var date:Date, var description:String, var location:String) {
	override def toString() = { if (date == null) title else title + " at " + date.toString() }

	def compareTo(e:Event):Int = {
		if (date.compareTo(e.date) == 0) return date.compareTo(e.date)
		else if (title.compareTo(e.title) == 0) return title.compareTo(e.title)
		else if (description.compareTo(e.description) == 0) return description.compareTo(e.description) 
		else if (location.compareTo(e.location) == 0) return location.compareTo(e.location)
		else return 0
	}
}

