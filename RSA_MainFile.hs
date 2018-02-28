{- Imports -}
import RSA
import Control.Monad (forM, forM_)

{- RSA Main File -}
main :: IO()
main = do

  -- Get plain text
  print("Enter the plain text...")
  message <- getLine
  let plainText = processString message

  -- Generate public key
  (p,q) <- rndPrimes 128 -- 128 bit RSA primes

  e <- rndPrime 30 -- encryption key exponent (public): 30 bit prime
  print ("Public Key: " ++ show (p*q, e))

  -- Generate private key
  let d = modInverse e ((p-1)*(q-1)) -- last part of the private key
  print ("Private Key: " ++ show (p, q, d))

  -- Encrypt message
  let cipherText = [modExp m e (p*q) | m <- (map toInteger plainText)]

  print("Cipher text: " ++ show cipherText)

  -- Decrypt message
  let decryptedText = [modExp cipherText d (p*q) | cipherText <- (map toInteger cipherText)]

  print ("Decrypted cipher text: " ++ show decryptedText)
