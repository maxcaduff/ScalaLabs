package Chat

import Chat.Tree.ExprTree
import Data.{Brand, Product, UsersInfo}

// TODO - step 3
/**
  * Modified by: Alexandra Korukova, Max Caduff
  */
object Tree {

  val authenticationDemand: String = "Veuillez d'abord vous identifier."

  /**
    * This sealed trait represents a node of the tree and contains methods to compute it and write its output text in console.
    */
  sealed trait ExprTree {
    /**
      * Compute the price of the current node, then returns it. If the node is not a computational node, the method
      * returns 0.0.
      * For example if we had a "+" node, we would add the values of its two children, then return the result.
      * @return the result of the computation
      */
      def computePrice: Double = this match {
        case Item(product, brand) => brand.price
        case Items(item, n) => n * item.computePrice
        case And(firstCommand, secondCommand) => firstCommand.computePrice + secondCommand.computePrice
        case Or(firstCommand, secondCommand) => Math.min(firstCommand.computePrice, secondCommand.computePrice)
        case _ => 0
      }

      def reply: String = this match {
        // Example cases
        case Thirsty() => "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
        case Hungry() => "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
        case Authentication(name) =>
          UsersInfo.setActiveUser(name)
          "Bonjour, " + name
        case Price(basket) => "Cela coûte CHF " + basket.computePrice
        case BadSyntax(msg) => msg
        case other =>
          if(UsersInfo.activeUser == null)
            authenticationDemand
          else other match {
            case Balance() => "Le montant actuel de votre solde est de CHF " +
              UsersInfo.getUsersBalance(UsersInfo.activeUser)
            case Command(basket) =>
              // Compute the total price of the command
              val totalPrice = basket.computePrice
              // purchase is done only if there is enough money on the user's account
              if(UsersInfo.purchase(UsersInfo.activeUser, totalPrice))
                "Voici donc " + basket + " ! Cela coûte CHF " + totalPrice +
                  " et votre nouveau solde est de CHF " + UsersInfo.getUsersBalance(UsersInfo.activeUser)
              else
                "Solde insuffisant"

            case Price(basket) => "Cela coûte " + basket.computePrice
          }
      }

    }
  }

  /**
    * Declarations of the nodes' types.
    */
  // Example cases
  case class Thirsty() extends ExprTree
  case class Hungry() extends ExprTree
  case class Authentication(user: String) extends ExprTree
  case class Balance() extends ExprTree
  case class Price(basket: ExprTree) extends ExprTree
  case class Command(basket: ExprTree) extends ExprTree
  case class Items(item: Item, n: Int) extends ExprTree {
    override def toString: String = {
      n.toString + " " + item.toString
    }
  }
  case class BadSyntax(msg: String) extends ExprTree
  case class EmptyTree() extends ExprTree


  /**
    * Computational nodes
    */
  case class And(first: ExprTree, second: ExprTree) extends ExprTree {
    override def toString: String = {
      first.toString + " et " + second.toString
    }
  }
  case class Or(first: ExprTree, second: ExprTree) extends ExprTree {
    override def toString: String = {
      first.toString + " ou " + second.toString
    }
  }
  case class Item(product: Product, var brand: Brand = null) extends ExprTree {
    // set the brand to the product's default one in case it is not specified
    if(brand == null)
      if(product.defaultBrand == null)
        throw new Error("No brand specified for item " + product.name)
      else
        brand = product.defaultBrand
    // check if the brand exists in the products brands set
    if(!product.brands(brand))
      throw new Error("Brand " + brand.name + " does not correspond to product " + product.name)

    override def toString: String = {
      product.name + " " + brand.name
    }
  }
