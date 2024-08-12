object CaesarCipher {

  // Function to encrypt a single character with a shift of 1
  def encryptChar(c: Char): Char = {
    if (c.isLetter) {
      val base = if (c.isLower) 'a' else 'A'
      ((c - base + 1) % 26 + base).toChar
    } else {
      c
    }
  }

  // Function to decrypt a single character with a shift of 1
  def decryptChar(c: Char): Char = {
    if (c.isLetter) {
      val base = if (c.isLower) 'a' else 'A'
      ((c - base - 1 + 26) % 26 + base).toChar
    } else {
      c
    }
  }

  // Encryption function for the Caesar cipher with a fixed shift of 1
  def encrypt(text: String): String = {
    text.map(c => encryptChar(c))
  }

  // Decryption function for the Caesar cipher with a fixed shift of 1
  def decrypt(text: String): String = {
    text.map(c => decryptChar(c))
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
