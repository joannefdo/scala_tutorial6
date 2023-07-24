object CaesarCipher {
  def encrypt(text: String, shift: Int): String = {
    def shiftChar(c: Char): Char = {
      if (c.isLetter) {
        val base = if (c.isUpper) 'A' else 'a'
        ((c - base + shift) % 26 + base).toChar
      } else {
        c
      }
    }

    text.map(shiftChar)
  }

  def decrypt(text: String, shift: Int): String = {
    encrypt(text, 26 - shift)
  }


  def cipher(text: String, shift: Int, func: (String, Int) => String): String = {
    func(text, shift)
  }

  def main(args: Array[String]): Unit = {
    val plaintext = "Hello, World!"
    val shift = 3

    val encryptedText = cipher(plaintext, shift, encrypt)
    println(s"Encrypted: $encryptedText")

    val decryptedText = cipher(encryptedText, shift, decrypt)
    println(s"Decrypted: $decryptedText")
  }
}
