package domain

case class Review (id: String, date: String, rating: Int, author:String, title: String, body: List[String])
