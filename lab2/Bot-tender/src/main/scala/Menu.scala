import Data.{Products, Brand, Product}

/**
  * This object initializes the menu of the bot
  */
object Menu {
  val defaultBeer = new Brand("boxer", 1)
  val beerBrands: Set[Brand] = Set(
    new Brand("farmer", 1),
    new Brand("wittekop", 2),
    new Brand("punkipa", 3),
    new Brand("jackhammer", 3),
    new Brand("tenebreuse", 4),
    defaultBeer)

  val defaultCroissant = new Brand("maison", 2)
  val croissantBrands: Set[Brand] = Set(
    new Brand("cailler", 2),
    defaultCroissant
  )

  val beer = new Product("biere", beerBrands, defaultBeer)
  var croissant = new Product("croissant", croissantBrands, defaultCroissant)

  Products.addProducts(Seq(beer, croissant))
}
