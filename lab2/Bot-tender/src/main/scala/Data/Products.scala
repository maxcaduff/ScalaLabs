package Data

object Products {
  // TODO: step 2 - here your will have an attribute that will contain the products (e.g. "bière"), their types (e.g. "Boxer"), and their prices (e.g. 2.0).
  // TODO: step 2 - You will also have to find a way to store the default type/brand of a product.
  // Set is chosen to avoid duplicate products
  var products: Set[Product] = Set()

  def addProduct(product: Product) = {
    products = products + product
  }

  def addProducts(products: List[Product]) = {
    this.products = this.products ++ products
  }

  def getProductByName(productName: String): Product = {
    val foundProduct: Option[Product] = products.find(p => p.name == productName)
    foundProduct match {
      case Some(product) => product
      case None => throw new Error("No product named " + productName)
    }
  }

  override def toString: String = {
    products.foldLeft("") {(str, p) => str + p.toString + "\n"}
  }

}

class Product(var name: String, var brands: Set[Brand], var defaultBrand: Brand = null) {

  def == (other: Product): Boolean = {
    this.name == other.name
  }

  def addBrand(brand: Brand) = {
    brands = brands + brand
  }

  def getBrandByName(brandName: String): Brand = {
    val foundBrand: Option[Brand] = brands.find(b => b.name == brandName)
    foundBrand match {
      case Some(brand) => brand
      case None => defaultBrand
    }
  }

  def setDefaultBrand(brand: Brand) = {
    defaultBrand = brand
  }

  override def toString: String = {
    name + " :" + brands.foldLeft(""){ (str, el) =>  str + "\n\t" + el.toString}
  }

}

class Brand(var name: String, var price: Float) {

  def == (other: Brand): Boolean = {
    this.name == other.name
  }

  override def toString: String = {
    name + " : " + price.toString
  }
}
