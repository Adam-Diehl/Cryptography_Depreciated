{- Imports -}
import RSA

{- RSA Main File -}
main :: IO()
main = do

  {- User input -}
  -- Get plain text
  print("Enter the plain text...")
  message <- getLine
  let plainText = processString message

  {- Key generation -}
  -- Generate public key
  (p,q) <- rndPrimes 128 -- 128 bit RSA primes

  e <- rndPrime 30 -- encryption key exponent (public): 30 bit prime
  print ("Public Key: " ++ show (p*q, e))

  -- Generate private key
  let d = modInverse e ((p-1)*(q-1)) -- last part of the private key
  print ("Private Key: " ++ show (p, q, d))

  {- Encryption and decryption routines -}
  -- Encrypt message
  let cipherText = [modExp m e (p*q) | m <- (map toInteger plainText)]

  print("Cipher text: " ++ show cipherText)

  -- Decrypt message
  let decryptedText = [modExp cipherText d (p*q) | cipherText <- (map toInteger cipherText)]
  let plainDecryptedText = processInts decryptedText

  print ("Decrypted cipher text: " ++ plainDecryptedText)
