import scala.collection.mutable.ArrayBuffer

//val RawInputs = """111100101100
//                  |101100110001
//                  |100110100101
//                  |001101100010
//                  |010111011110
//                  |000111001100
//                  |001000011010
//                  |110111000100
//                  |101011101011
//                  |111100111010
//                  |111101011001
//                  |011000010010
//                  |101011001000
//                  |010011001001
//                  |110001001100
//                  |001010010010
//                  |010111001010
//                  |000000001001
//                  |001001010001
//                  |011100010111
//                  |001110111100
//                  |001110010010
//                  |000101100100
//                  |010101110001
//                  |110100100011
//                  |100001001001
//                  |001011101011
//                  |010010001100
//                  |011001011011
//                  |001111000011
//                  |000000110110
//                  |110011101010
//                  |100110110010
//                  |100000111101
//                  |100000001110
//                  |010101001110
//                  |010111101101
//                  |110001001110
//                  |100100111100
//                  |000001110001
//                  |101100011101
//                  |100010000001
//                  |110010110100
//                  |000010110011
//                  |101010111011
//                  |011000000110
//                  |010000100111
//                  |111011011101
//                  |111100000010
//                  |011010111011
//                  |010101011100
//                  |100011110001
//                  |110011010100
//                  |010101100000
//                  |101101110011
//                  |000011000100
//                  |111000000110
//                  |101110111001
//                  |100111111011
//                  |110011100101
//                  |101010000111
//                  |000011111010
//                  |000001100100
//                  |111000101110
//                  |101001011110
//                  |100101101001
//                  |110100001100
//                  |001010001000
//                  |110110101000
//                  |100101110100
//                  |101000001011
//                  |011111101100
//                  |000110011100
//                  |001001010101
//                  |101000110010
//                  |111101011101
//                  |110010010000
//                  |001100110110
//                  |001111011111
//                  |010001111110
//                  |001110010111
//                  |100101000101
//                  |010100111010
//                  |100011001000
//                  |010110011001
//                  |001001111010
//                  |101101001001
//                  |111010101101
//                  |101001110001
//                  |001001001110
//                  |100100101110
//                  |110111110100
//                  |110100011000
//                  |011001010010
//                  |010111001100
//                  |000100110000
//                  |110001000100
//                  |111010011011
//                  |011111011101
//                  |011110111100
//                  |001101111010
//                  |100010011101
//                  |111000110111
//                  |110110001010
//                  |110000101010
//                  |001101000011
//                  |011010011101
//                  |111100100010
//                  |001001110110
//                  |001001001011
//                  |010010110100
//                  |010101010110
//                  |101000001110
//                  |011011010100
//                  |010000011010
//                  |010001011110
//                  |111100110011
//                  |111000100111
//                  |100001010000
//                  |010101001111
//                  |101110010110
//                  |110100001000
//                  |000001110110
//                  |000100001110
//                  |000010100111
//                  |010010101001
//                  |110110011000
//                  |111001110100
//                  |001100011001
//                  |110100001010
//                  |110101001010
//                  |111011000001
//                  |100110111000
//                  |011110111110
//                  |101010111010
//                  |100001010111
//                  |010101010101
//                  |010001110001
//                  |110010000010
//                  |000101001111
//                  |111010100010
//                  |100111011110
//                  |000010111110
//                  |011011010011
//                  |101010110100
//                  |100000010111
//                  |100110111110
//                  |010011010001
//                  |000011101011
//                  |111011010011
//                  |100001110111
//                  |000100001000
//                  |010110100000
//                  |001110100000
//                  |010010100110
//                  |000110111110
//                  |110111100100
//                  |001111011010
//                  |101011010010
//                  |010111010100
//                  |011011000110
//                  |101011111110
//                  |001111100011
//                  |101101100001
//                  |010111100110
//                  |010110000001
//                  |110011011110
//                  |110111001011
//                  |110100101001
//                  |010011011101
//                  |110011010010
//                  |100111001100
//                  |010010101010
//                  |010111110000
//                  |010111000010
//                  |001101111101
//                  |011010110011
//                  |001001100000
//                  |100001101100
//                  |011111000111
//                  |111011110100
//                  |100101010001
//                  |101001111001
//                  |001000101110
//                  |010011100100
//                  |111100011001
//                  |111010111110
//                  |111001010001
//                  |110000001001
//                  |110110000010
//                  |000100100011
//                  |010101111111
//                  |100101001110
//                  |100111111010
//                  |000111111010
//                  |010101111101
//                  |010010100010
//                  |101110100101
//                  |100001000001
//                  |000010001000
//                  |110100110001
//                  |110111100010
//                  |100110010011
//                  |101110011111
//                  |111101110010
//                  |001000001001
//                  |010100101100
//                  |100101110111
//                  |010001100111
//                  |010001010010
//                  |100010100100
//                  |001000000001
//                  |011011110000
//                  |010100011000
//                  |110010101000
//                  |100010001110
//                  |000110001000
//                  |101101010001
//                  |001111010000
//                  |111110111111
//                  |100100110000
//                  |000110010011
//                  |001010101000
//                  |011010011010
//                  |111011110010
//                  |001010011100
//                  |111010100101
//                  |011101111100
//                  |010011000001
//                  |001111101000
//                  |111110010010
//                  |100011100100
//                  |010011010000
//                  |011111000000
//                  |110001010011
//                  |101011001110
//                  |101001111101
//                  |000110001101
//                  |010001000010
//                  |011000000010
//                  |011000100001
//                  |011111011000
//                  |111000001101
//                  |011010001101
//                  |000001000100
//                  |101010010010
//                  |111111100111
//                  |010111111111
//                  |001011110011
//                  |100110000100
//                  |000100010100
//                  |010110000000
//                  |110011100000
//                  |010011101111
//                  |110101111100
//                  |000010001010
//                  |101001010011
//                  |010101101111
//                  |111010011010
//                  |001101101010
//                  |001100101010
//                  |100100001001
//                  |101100110111
//                  |000001001100
//                  |011111011001
//                  |110100101100
//                  |110110110010
//                  |011110001001
//                  |100110100011
//                  |010111111100
//                  |110111111101
//                  |010111000011
//                  |101001101011
//                  |010101101100
//                  |011100000001
//                  |001000101100
//                  |110111011011
//                  |001101001100
//                  |111001111001
//                  |010000111100
//                  |010101111100
//                  |000100010010
//                  |100010001101
//                  |110000111100
//                  |110011010101
//                  |111101100000
//                  |001101011001
//                  |010111110111
//                  |010001100000
//                  |101111000100
//                  |101101111101
//                  |001000001111
//                  |101011110000
//                  |111000000101
//                  |101000000101
//                  |111101111011
//                  |101010111101
//                  |101111000000
//                  |010111011001
//                  |011100101101
//                  |100100001000
//                  |101111110010
//                  |000001000001
//                  |000100100000
//                  |100100011010
//                  |011001100101
//                  |011010001011
//                  |010001111000
//                  |001110101001
//                  |100111001001
//                  |000111001011
//                  |100101110011
//                  |111010000001
//                  |010000011001
//                  |101110001110
//                  |111011110111
//                  |110010011101
//                  |100011001010
//                  |001001100101
//                  |011101001000
//                  |100100001100
//                  |000111111000
//                  |101100011011
//                  |111101010011
//                  |111100010101
//                  |101111100111
//                  |011011100110
//                  |101010110110
//                  |111001000100
//                  |011101000010
//                  |101100101110
//                  |100011001110
//                  |001100000001
//                  |101001001101
//                  |001100101110
//                  |111111010000
//                  |001110001110
//                  |000011110111
//                  |100001000101
//                  |011110001011
//                  |001100101111
//                  |100100110111
//                  |110100010101
//                  |011100110001
//                  |101101111010
//                  |111010011000
//                  |101000100110
//                  |100100001110
//                  |111000011011
//                  |100001001110
//                  |111001000010
//                  |100110100111
//                  |011111101011
//                  |101001110011
//                  |010000000100
//                  |111011111000
//                  |011101110000
//                  |110111110110
//                  |000000100110
//                  |001100110010
//                  |101010110101
//                  |000010101110
//                  |000011011011
//                  |101111011101
//                  |000101011110
//                  |101010000110
//                  |010010111011
//                  |000111010001
//                  |011001011000
//                  |111111010010
//                  |110000101110
//                  |001111101100
//                  |001001001111
//                  |001011010001
//                  |010110110010
//                  |101101100010
//                  |111100110101
//                  |101101001000
//                  |110110000111
//                  |101010011001
//                  |011000100100
//                  |010010011010
//                  |100000100010
//                  |111000110100
//                  |011100000101
//                  |010010110011
//                  |000011001000
//                  |101001100101
//                  |010100111000
//                  |101111111101
//                  |011110101001
//                  |100011110110
//                  |101111001100
//                  |100110111011
//                  |110000000011
//                  |100011010101
//                  |101010001110
//                  |010011000010
//                  |000011000110
//                  |010101110110
//                  |001111101001
//                  |101000001001
//                  |110011110000
//                  |000001001101
//                  |000010110100
//                  |001111101101
//                  |010110000100
//                  |010010001001
//                  |111010110010
//                  |110011111111
//                  |111000000000
//                  |101010110000
//                  |010110100010
//                  |100000000000
//                  |010110010000
//                  |110111001001
//                  |010101011011
//                  |001110011001
//                  |011101111000
//                  |110011011101
//                  |100000110110
//                  |100110011001
//                  |001111100100
//                  |010100110110
//                  |000110111101
//                  |111010101011
//                  |101001100010
//                  |011011010010
//                  |000010010011
//                  |101000011101
//                  |101110010101
//                  |000000110011
//                  |100111111001
//                  |011000011001
//                  |000000000001
//                  |101011110110
//                  |111111011110
//                  |010010111000
//                  |010110100100
//                  |101100100000
//                  |000010111000
//                  |111110010011
//                  |010110110011
//                  |111111110001
//                  |011011100101
//                  |110110101101
//                  |001011110100
//                  |110001111111
//                  |101010010111
//                  |000110011011
//                  |001000011110
//                  |101101101000
//                  |111111010111
//                  |101010111001
//                  |010100000001
//                  |101100101001
//                  |000111110101
//                  |101011111000
//                  |100110011011
//                  |010000000111
//                  |100111011001
//                  |110111010101
//                  |011110011010
//                  |001011011010
//                  |111101100100
//                  |011001101101
//                  |100001111100
//                  |000011011000
//                  |100011011001
//                  |101100011110
//                  |011111011111
//                  |001001010010
//                  |110101110110
//                  |000111000010
//                  |000110001001
//                  |111010110000
//                  |001100101001
//                  |110011101000
//                  |101011110100
//                  |110000001010
//                  |011110111010
//                  |101101101010
//                  |110011101011
//                  |011011010000
//                  |110111101100
//                  |100000001011
//                  |001110110111
//                  |000000100100
//                  |111110011001
//                  |001110011010
//                  |011101101110
//                  |100011110011
//                  |101110101011
//                  |001010101011
//                  |010111011011
//                  |100110110111
//                  |100000101110
//                  |010011100111
//                  |000010011100
//                  |011111011100
//                  |011000111001
//                  |111011101001
//                  |000110110110
//                  |100011100110
//                  |101101110100
//                  |101111000111
//                  |111110110010
//                  |000010010101
//                  |111110100111
//                  |110001011110
//                  |110101110000
//                  |100110101000
//                  |011000111010
//                  |011111110011
//                  |010010010000
//                  |001000110100
//                  |100010100111
//                  |100101100010
//                  |011000100101
//                  |000101111001
//                  |101100001111
//                  |110111000010
//                  |010100011110
//                  |000101100000
//                  |101111011000
//                  |000010100011
//                  |000010000100
//                  |010011001011
//                  |100010110100
//                  |000000101001
//                  |000010110111
//                  |110000100111
//                  |010001101111
//                  |010111000110
//                  |110011011111
//                  |010100010011
//                  |011110001000
//                  |001001101000
//                  |111000110000
//                  |011001111110
//                  |101001011100
//                  |101110000111
//                  |111110010100
//                  |001101011110
//                  |010010111111
//                  |011110000011
//                  |011001110010
//                  |001011110111
//                  |000000001110
//                  |111000011101
//                  |110111101001
//                  |111111001100
//                  |100111010011
//                  |111111101010
//                  |110111111000
//                  |001101100111
//                  |111011011111
//                  |000010111101
//                  |100010011001
//                  |010110100101
//                  |011011001010
//                  |000001101000
//                  |001011110110
//                  |000001111000
//                  |100011001111
//                  |110000100011
//                  |100001010010
//                  |110110000100
//                  |110110010010
//                  |101001101111
//                  |111100000011
//                  |110000111001
//                  |100001011010
//                  |110110011101
//                  |101010001011
//                  |010110011100
//                  |111000101010
//                  |001111111011
//                  |010101101001
//                  |100010111011
//                  |100101010111
//                  |011111100001
//                  |001100100000
//                  |111101101000
//                  |000110000010
//                  |010111100000
//                  |001000110110
//                  |100011101110
//                  |010101011000
//                  |110000000111
//                  |100011100001
//                  |100001100000
//                  |000111010100
//                  |110101011001
//                  |101111100110
//                  |011111101111
//                  |000110000011
//                  |000000100001
//                  |001010000101
//                  |111100100001
//                  |111000010101
//                  |011011011110
//                  |101001001100
//                  |100010110110
//                  |100001101001
//                  |101100001100
//                  |100111000010
//                  |011001000110
//                  |011110110101
//                  |000000100011
//                  |011100000111
//                  |101110111100
//                  |010000010010
//                  |110111110010
//                  |100101001001
//                  |010001010011
//                  |000011100100
//                  |000100100101
//                  |010100010001
//                  |100111101101
//                  |101110000101
//                  |110100101111
//                  |001111101011
//                  |010011101010
//                  |110000110001
//                  |000101101011
//                  |111010001001
//                  |011111010110
//                  |001001111000
//                  |111110110011
//                  |111010011100
//                  |010011011110
//                  |100001010100
//                  |010111010011
//                  |011110001110
//                  |111010100011
//                  |110001000010
//                  |100000100100
//                  |001001101011
//                  |001011111011
//                  |110000010010
//                  |000011001101
//                  |001101001011
//                  |011110010011
//                  |100001110011
//                  |011101100000
//                  |110101001110
//                  |001000111101
//                  |001101110011
//                  |001100101101
//                  |011110001010
//                  |111111011000
//                  |101001101101
//                  |110011010111
//                  |101110001101
//                  |111001101110
//                  |011010011000
//                  |101010101000
//                  |010110000010
//                  |100000110001
//                  |011001000011
//                  |011100010010
//                  |010001001111
//                  |001011000100
//                  |101000101011
//                  |111101010000
//                  |100111100101
//                  |001011101110
//                  |010001000011
//                  |111101000111
//                  |110010011111
//                  |011011001101
//                  |010110101110
//                  |100011100111
//                  |101111111111
//                  |111011011010
//                  |000100111100
//                  |000111011011
//                  |101010001010
//                  |111110000011
//                  |101110011010
//                  |011100001001
//                  |100000011110
//                  |011000100000
//                  |011001010101
//                  |001110110010
//                  |101110101010
//                  |010111110101
//                  |101110101110
//                  |101111010001
//                  |101101001101
//                  |001010101100
//                  |011011001011
//                  |011001110111
//                  |111101100101
//                  |101011100100
//                  |011111111101
//                  |110000111010
//                  |010010011000
//                  |011010110010
//                  |010001000001
//                  |100111101100
//                  |011001111000
//                  |101011010111
//                  |001111100110
//                  |111100001111
//                  |001101010000
//                  |000011001110
//                  |110010011110
//                  |111010100111
//                  |110101001000
//                  |001000100110
//                  |101001011000
//                  |010010101111
//                  |011000001011
//                  |000110001011
//                  |000010000011
//                  |001001111001
//                  |011000111111
//                  |101000010011
//                  |111010011101
//                  |101111101000
//                  |111100010001
//                  |111011000100
//                  |100111100010
//                  |111000011010
//                  |010010000010
//                  |111110110100
//                  |010010111010
//                  |110001010001
//                  |101110111101
//                  |010000001000
//                  |000011111100
//                  |111001001000
//                  |101011001001
//                  |111011011001
//                  |010100001101
//                  |010001001100
//                  |010011001010
//                  |000101010100
//                  |010100001100
//                  |001000011001
//                  |100110101110
//                  |011001111011
//                  |010101100011
//                  |111100000100
//                  |100100101000
//                  |110010001010
//                  |000100101001
//                  |000101100001
//                  |000010011000
//                  |101001111000
//                  |001001000100
//                  |011001001000
//                  |010110010011
//                  |100001010110
//                  |010010010010
//                  |010001011010
//                  |101100111110
//                  |101001010000
//                  |110101110001
//                  |100111001111
//                  |010110110101
//                  |101011010100
//                  |011110011111
//                  |010000010100
//                  |111011000110
//                  |011111001110
//                  |010110111001
//                  |000100010101
//                  |100110001101
//                  |001001001010
//                  |000010010000
//                  |010001111001
//                  |001011011000
//                  |010101011110
//                  |110101101001
//                  |111101011100
//                  |010011110001
//                  |101000110001
//                  |111110110111
//                  |001110001010
//                  |101110111111
//                  |000111011101
//                  |010001110010
//                  |101010010101
//                  |001000011111
//                  |010000010001
//                  |101100010000
//                  |110111101011
//                  |001001011110
//                  |100011011101
//                  |110010100110
//                  |111101111100
//                  |111100011100
//                  |000111110000
//                  |000101010101
//                  |011001011101
//                  |000101110111
//                  |001100011111
//                  |110010011000
//                  |010110111111
//                  |111101000010
//                  |111011010000
//                  |111001110010
//                  |010111000100
//                  |111111100001
//                  |000000010000
//                  |001100001111
//                  |101101111111
//                  |101100000001
//                  |011001100000
//                  |100010110010
//                  |111111111110
//                  |000110010001
//                  |001001111110
//                  |111110111011
//                  |110101001111
//                  |010000000010
//                  |000001101110
//                  |100001011110
//                  |111111100101
//                  |101100100011
//                  |100000010110
//                  |100011000110
//                  |000010010001
//                  |100010111010
//                  |111010111000
//                  |011110110010
//                  |000011000010
//                  |010010011001
//                  |010010000100
//                  |110101111000
//                  |010000001010
//                  |100000011010
//                  |001010001110
//                  |000011010001
//                  |100100111101
//                  |111011111001
//                  |010001010111
//                  |010000100000
//                  |000100101000
//                  |101011011011
//                  |111111010110
//                  |111110000100
//                  |100011101010
//                  |100111011010
//                  |010101001100
//                  |011010000000
//                  |111100100000
//                  |011010111110
//                  |011111001010
//                  |111011011000
//                  |001011001010
//                  |111101101111
//                  |100111100110
//                  |011000000000
//                  |000001011000
//                  |100111011000
//                  |001111100000
//                  |000010101100
//                  |110111001010
//                  |011010100101
//                  |001010010000
//                  |110110011100
//                  |010101110111
//                  |111111110110
//                  |011100110101
//                  |101000010110
//                  |100110110110
//                  |101001101100
//                  |111001011001
//                  |000001010011
//                  |000110100000
//                  |111101101011
//                  |101111101101
//                  |100010111110
//                  |010110011111
//                  |000010000111
//                  |000101101010
//                  |011100000000
//                  |001110011101
//                  |011011110110
//                  |111111101001
//                  |111111010011
//                  |110000110100
//                  |100000010101
//                  |001010000010
//                  |111010101110
//                  |001110010000
//                  |100100010101
//                  |100110001000
//                  |111100011000
//                  |001011100111
//                  |100100110011
//                  |001000001100
//                  |000111001000
//                  |100111111101
//                  |100110010101
//                  |101000001101
//                  |111110001110
//                  |001011100000
//                  |000011111101
//                  |010010001101
//                  |000010011101
//                  |001100111110
//                  |000001100110
//                  |110001101010
//                  |001101010010
//                  |110000110000
//                  |100011001100
//                  |001110011011
//                  |110001110000
//                  |010000011110
//                  |100011111110
//                  |101001101000
//                  |001111011011
//                  |010001011011
//                  |111111010100
//                  |001011001111
//                  |001100010110
//                  |001010111001
//                  |110101010001
//                  |001001111011
//                  |010110001000
//                  |110100101110
//                  |100100100010
//                  |010001101101
//                  |100100000110
//                  |110010110010
//                  |011111000010
//                  |000110010010
//                  |001111110001
//                  |110111101110
//                  |111111000111
//                  |011011011111
//                  |011010101001
//                  |011001011110
//                  |010100110000
//                  |010000101110
//                  |101110100111
//                  |100001010101
//                  |101101010100
//                  |000111101010
//                  |111010000100
//                  |100000110111
//                  |001101110110
//                  |011011100010
//                  |010100111001
//                  |000110100010
//                  |110111101111
//                  |100111000110
//                  |101100110011
//                  |010001000111
//                  |110000101001
//                  |100101011100
//                  |001011111000
//                  |100001110001
//                  |010000011011
//                  |001010111100
//                  |000101101111
//                  |011011111001
//                  |110110101111
//                  |100101011000
//                  |011000110110
//                  |100000011111
//                  |110011100011
//                  |010111001001
//                  |000101010111
//                  |000010100110
//                  |101010110010
//                  |000000001100
//                  |111110001001
//                  |011111011010
//                  |011000111101
//                  |110110001000
//                  |110011001010
//                  |100101101111
//                  |100111101111
//                  |100101100111
//                  |110001001010
//                  |010110110001
//                  |001101101000
//                  |101110001111
//                  |000001001110
//                  |010101110100
//                  |001101011111
//                  |100010100000
//                  |101000011000
//                  |010100100111
//                  |110011100100
//                  |110111110101
//                  |000110001111
//                  |000001011101
//                  |001011101100
//                  |111110000110
//                  |100001010011
//                  |000100000100
//                  |001010011011
//                  |110000110111""".stripMargin
val RawInputs = """00100
                  |11110
                  |10110
                  |10111
                  |10101
                  |01111
                  |00111
                  |11100
                  |10000
                  |11001
                  |00010
                  |01010""".stripMargin

val k = RawInputs.split("\\n")

def getDim(s:Array[String])={
  val r = s.length
  val c = s(0).length
  (r,c)
}




def insertMultiArray(a: Array[String]): Array[Array[Int]] = {
  val (row,col) = getDim(k)
  val inputArr = Array.ofDim[Int](row,col)

  val o = a.flatten.map {
    case '0' => 0
    case '1' => 1
  }
  var count = 0

  for(i<-0 until row;j <-0 until col){
    inputArr(i)(j) = o(count)
    count+=1
  }

  inputArr
}

insertMultiArray(k)

def getResults(arr:Array[Array[Int]],rowSize:Int,colSize:Int,isGamma:Boolean)={
  var zeros = 0
  var ones = 0
  val gamma:ArrayBuffer[Int] = ArrayBuffer()


  for(j<-0 until colSize){
    for(i<- 0 until rowSize) {
      arr(i)(j) match{
        case 1 => ones+=1
        case 0=> zeros+=1
      }
    }
    if(isGamma){
      gamma.addOne(if(ones > zeros) 1 else 0)
    }
    else{
      gamma.addOne(if(ones > zeros) 0 else 1)
    }

    zeros=0
    ones=0
  }
  gamma
}
def toDec(arr: ArrayBuffer[Int]):Int={
  var counter:Int = 0
  var sum:Int = 0
  arr.reverse.foreach{ bit =>
    if(bit == 1) {
      sum += math.pow(2,counter).toInt
    }
    counter+=1
  }
  sum
}
val (row,col) = getDim(k)









val gamma = toDec(getResults(insertMultiArray(k),row,col,true))
val epsilon  = toDec(getResults(insertMultiArray(k),row,col,false))
val res = gamma*epsilon


