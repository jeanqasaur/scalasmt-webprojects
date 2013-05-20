//package cap.jeeves.jconf.backend
package models

import javax.mail._
import javax.mail.internet._
import java.util.Properties._

object JConfMail {
  def sendMail (
    sender: String, senderName: String, recipients: String, subject: String
    , bodyText: String)
  : Unit = {
    // Set up the mail object
    val properties = System.getProperties
    properties.put("mail.smtp.host", "outgoing.csail.mit.edu")
    properties.put("mail.smtp.port", "587")
    val session = Session.getDefaultInstance(properties)
      val message = new MimeMessage(session)

      // Set the from, to, subject, body text
    message.setFrom(new InternetAddress(sender, senderName))
    message.setRecipients(Message.RecipientType.TO, recipients)
    message.setSubject(subject)
    message.setText(bodyText)

    // And send it
    Transport.send(message)
  }
}
