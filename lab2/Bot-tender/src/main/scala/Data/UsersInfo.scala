package Data

/**
  * Modified by: Alexandra Korukova, Max Caduff
  */
object UsersInfo {

  // Will contain the name of the currently active user; default value is null.
  private var _activeUser: String = _

  val initialBalance = 30

  // TODO: step 2 - create an attribute that will contain each user and its current balance.
  /**
    * This Map represents an account manager.
    * Keys of the map are the users' pseudos and the values are their balances
    */
  private var accounts: Map[String, Double] = Map()

  /**
    * Sets the active user.
    * @param user the user to set as active user
    * @return
    */
  def setActiveUser(user: String): Unit = {
    // If the user with a given pseudo does not exist in accounts, add him/her there
    accounts.getOrElse(user, {
      accounts += user -> initialBalance
    })
    // Set the active user
    _activeUser = user
  }

  /**
    * Getter
    * @return the current active user
    */
  def activeUser: String = _activeUser

  /**
    * Returns the user's balance.
    * If the user with a given pseudo does not exist, throws an Error
    * @param user the user in question
    * @return user's balance
    */
  def getUsersBalance(user: String): Double = {
    accounts.getOrElse(user, throw new Error("User " + user + " does not exist"))
  }

  /**
    * Update an account by decreasing its balance.
    * @param user the user whose account will be updated
    * @param amount the amount to decrease
    * @return true if purchase was done, false otherwise
    */
  // TODO: step 2
  def purchase(user: String, amount: Double): Boolean = {
    // get the user's balance or throw an error if the user with a given pseudo does not exist
    var balance = accounts.getOrElse(user, throw new Error("User " + user + " does not exist"))
    // if there is not enough $$, the balance isn't changed
    if (balance < amount)
      return false
    // otherwise, update user's balance and update the account info
    balance -= amount
    accounts += user -> balance
    true
  }
}
