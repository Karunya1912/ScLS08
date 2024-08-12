object Que1 {

  // Function to encrypt a single character with a shift of 1
  def encChar(x: Char): Char = {
    if (x.isLetter) {
      val base = if (x.isLower) 'a' else 'A'
      ((x - base + 1) % 26 + base).toChar
    } else {
      x
    }
  }

  // Function to decrypt a single character with a shift of 1
  def decChar(x: Char): Char = {
    if (x.isLetter) {
      val base = if (x.isLower) 'a' else 'A'
      ((x - base - 1 + 26) % 26 + base).toChar
    } else {
      x
    }
  }

  // Encryption function for the Caesar cipher with a fixed shift of 1
  def encrypt(text: String): String = {
    text.map(x => encChar(x))
  }

  // Decryption function for the Caesar cipher with a fixed shift of 1
  def decrypt(text: String): String = {
    text.map(x => decChar(x))
  }

  // Cipher function that takes an encryption or decryption function to process data
  def cipher(text: String, algo: String => String): String = {
    algo(text)
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, World!"

    // Encrypt the plaintext with a fixed shift of 1
    val encryptedText = cipher(plaintext, encrypt)
    println(s"Encrypted: $encryptedText")

    // Decrypt the ciphertext with a fixed shift of 1
    val decryptedText = cipher(encryptedText, decrypt)
    println(s"Decrypted: $decryptedText")
  }
}
