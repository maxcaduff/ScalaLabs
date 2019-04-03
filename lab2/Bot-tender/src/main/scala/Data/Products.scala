package Data

/**
  * Products is an object that stocks all the products of the menu
  *
  * Authors: Alexandra Korukova, Max Caduff
  */
object Products {
  // TODO: step 2 - here your will have an attribute that will contain the products (e.g. "bière"), their types (e.g. "Boxer"), and their prices (e.g. 2.0).
  // TODO: step 2 - You will also have to find a way to store the default type/brand of a product.
  // Set is chosen to avoid duplicate products
  var products: Set[Product] = Set()

  def addProduct(product: Product) = {
    products = products + product
  }

  def addProducts(products: Seq[Product]) = this.products = this.products ++ products

  def getProductByName(productName: String): Product =
    products.find(p => p.name == productName) match {
      case Some(product) => product
      case None => throw new Error("No product named " + productName)
    }

  override def toString: String = products.foldLeft("") { (str, p) => str + p.toString + "\n" }
}

/**
  * This class represents a product
  * @param name name of the product
  * @param brands the Set containing all the brands of the product
  * @param defaultBrand the default brand of the product
  */
class Product(var name: String, var brands: Set[Brand], var defaultBrand: Brand = null) {

  def addBrand(brand: Brand) = brands = brands + brand


  def addBrands(brands: Seq[Brand]) = this.brands = this.brands ++ brands

  /**
    * Retrieves the product's brand by the brand's name
    * @param brandName name of the brand
    * @return the Brand instance with the given name
    */
  def getBrandByName(brandName: String): Brand = {
    brands.find(b => b.name == brandName) match {
      case Some(brand) => brand
      case None => defaultBrand
    }
  }

  def setDefaultBrand(brand: Brand) = defaultBrand = brand

  override def toString: String = name + " :" + brands.foldLeft(""){ (str, el) =>  str + "\n\t" + el.toString}


}

/**
  * This class represents the brand of the product
  * @param name the name of the brand
  * @param price the price of the product of the given brand
  */
class Brand(var name: String, var price: Float) {

  override def toString: String = name + " : " + price.toString

}
