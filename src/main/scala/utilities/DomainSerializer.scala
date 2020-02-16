package utilities

import domain.{Review, Annotation}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

class DomainSerializer {

  def reviewToJSON(review: Review)= {
    val json =
      ("id" -> review.id) ~
        ("date" -> review.date) ~
        ("rating" -> review.rating) ~
        ("author" -> review.author) ~
        ("title" -> review.title) ~
        ("body" -> review.body)
    pretty(render(json))
  }

  def allReviewsToJSON(reviews: List[Review], appName: String) = {
    val json =
      ("version" -> "1.0") ~
        ("app" -> appName) ~
        ("reviews" ->
          reviews.map(r =>
            ("id" -> r.id) ~
              ("date" -> r.date) ~
              ("rating" -> r.rating) ~
              ("author" -> r.author) ~
              ("title" -> r.title) ~
              ("body" -> r.body)
          )
          )
    pretty(render(json))
  }

  def serializeAnnotations(annotations: Seq[Annotation]) = {
    val json = ("annotations" ->
      annotations.map(a =>
        ("reviewId" -> a.reviewId) ~
        ("defects" -> a.defects) ~
        ("improvements" -> a.improvements)
      ))
    pretty(render(json))
  }

}


