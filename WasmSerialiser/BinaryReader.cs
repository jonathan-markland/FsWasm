using System;
using System.Runtime.InteropServices;

namespace WasmSerialiser
{
    /// <summary>
    /// Supports reading basic lexeme kinds from WebAssembly binary files.
    /// </summary>
    public class BinaryReader
    {
        private byte[] _fileImage;
        private uint _readOffset;

        public BinaryReader(byte[] fileImage)
        {
            _fileImage = fileImage;
            _readOffset = 0;
        }

        public uint ReadOffset
        {
            get { return _readOffset; }
            set { _readOffset = value; }
        }

        public bool EndOfFile
        {
            get { return _readOffset >= _fileImage.Length; }
        }

        public byte PeekByte()
        {
            return _fileImage[_readOffset]; // TODO: Raise a custom exception if off-end of file?
        }

        public byte ReadByte()
        {
            var resultValue = _fileImage[_readOffset]; // TODO: Raise a custom exception if off-end of file?
            ++_readOffset;
            return resultValue;
        }

        public void SkipByte()
        {
            ++_readOffset;
        }

        public void Reverse(uint n)
        {
            _readOffset -= n;
        }

        /*public byte UnsureIfByteOrLeb()
        {
            var thisByte = ReadByte();
            if (thisByte < 0x80) return thisByte;
            throw new Exception("UnsureIfByteOrLeb():  Found byte that could be either.  Must resolve by reading spec!");
        }*/



        private enum LebReaderMode
        {
            Signed, Unsigned
        }



        public int ReadLebSigned32()
        {
            return (int)ReadLeb32(LebReaderMode.Signed);
        }

        public uint ReadLebUnsigned32()
        {
            return ReadLeb32(LebReaderMode.Unsigned);
        }

        public long ReadLebSigned64()
        {
            return (long)ReadLeb64(LebReaderMode.Signed);
        }

        public ulong ReadLebUnsigned64()
        {
            return ReadLeb64(LebReaderMode.Unsigned);
        }



        private uint ReadLeb32(LebReaderMode readerMode)
        {
            // Each byte of the LEB format gives us 7-bits of "real" data.
            // Note:  If full 32-bits are required, then the format is actually giving us *35* bits
            // spread over 5 bytes in the file.  The extra bits (32..34) must be ZERO for the UNSIGNED
            // case, or repetitions of bit 31 for the SIGNED case.
            // Also, we mustn't have more than 5 bytes in the LEB representation anyway for 32-bit targets.

            uint resultValue = 0;
            int positioningShift = 0;
            for (; ; )
            {
                var thisByte = ReadByte();

                if (positioningShift == 28) // ie: 4 x 7
                {
                    RaiseExceptionsIfOverflow32(readerMode, thisByte);
                }

                resultValue |= (uint)((uint)(thisByte & 0x7F) << positioningShift);
                positioningShift += 7;

                if ((thisByte & 0x80) == 0)
                {
                    // This was the final byte of the LEB.

                    if (readerMode == LebReaderMode.Signed
                        && positioningShift < 32 // ie: was this LEB less than 5 bytes long in total.
                        && (thisByte & 0x40) == 0x40) // ie: the sign bit is set
                    {
                        // Apply negative sign-extension because this number is negative.
                        resultValue |= 0xFFFFFFFF << positioningShift;
                    }

                    break;
                }
            }

            return resultValue;
        }

        private void RaiseExceptionsIfOverflow32(LebReaderMode readerMode, byte thisByte)
        {
            // Overflow check.
            if (readerMode == LebReaderMode.Signed)
            {
                var signAndContinuation = thisByte & 0x88;
                if (signAndContinuation == 0x08)
                {
                    // No continuation, bit 31=1, so number is negative.
                    if ((thisByte & 0x70) != 0x70)
                    {
                        throw new BinaryReaderSignedLebException("Invalid signed 32-bit LEB:  Bit 31 says the number is negative, so bits 32,33,34 must be 1.");
                    }
                }
                else if (signAndContinuation == 0x00)
                {
                    // No continuation, bit 31=0, so number is positive.
                    if ((thisByte & 0x70) != 0x00)
                    {
                        throw new BinaryReaderSignedLebException("Invalid signed 32-bit LEB:  Bit 31 says the number is positive, so bits 32,33,34 must be 0.");
                    }
                }
                else
                {
                    throw new BinaryReaderLebLengthException("Invalid signed 32-bit LEB:  The LEB has more than 5 bytes.");
                }
            }
            else // reading unsigned
            {
                if ((thisByte & 0x80) == 0x80)
                {
                    throw new BinaryReaderLebLengthException("Invalid unsigned 32-bit LEB:  The LEB has more than 5 bytes.");
                }
                else if ((thisByte & 0x70) != 0x00)
                {
                    throw new BinaryReaderUnsignedLebException("Invalid unsigned 32-bit LEB:  Bits 32,33,34 must be 0.");
                }
            }
        }






        private ulong ReadLeb64(LebReaderMode readerMode)
        {
            // Each byte of the LEB format gives us 7-bits of "real" data.
            // Note:  If full 64-bits are required, then the format is actually giving us *70* bits
            // spread over 10 bytes in the file.  The extra bits (64..69) must be ZERO for the UNSIGNED
            // case, or repetitions of bit 63 for the SIGNED case.
            // Also, we mustn't have more than 10 bytes in the LEB representation anyway.

            ulong resultValue = 0;
            int positioningShift = 0;
            for (; ; )
            {
                var thisByte = ReadByte();

                if (positioningShift == (9 * 7))
                {
                    RaiseExceptionsIfOverflow64(readerMode, thisByte);
                }

                resultValue |= (ulong)((ulong)(thisByte & 0x7F) << positioningShift);
                positioningShift += 7;

                if ((thisByte & 0x80) == 0)
                {
                    // This was the final byte of the LEB.

                    if (readerMode == LebReaderMode.Signed
                        && positioningShift < 64 // ie: was this LEB less than 10 bytes long in total.
                        && (thisByte & 0x40) == 0x40) // ie: the sign bit is set
                    {
                        // Apply negative sign-extension because this number is negative.
                        resultValue |= 0xFFFFffffFFFFffffu << positioningShift;
                    }

                    break;
                }
            }

            return resultValue;
        }

        private void RaiseExceptionsIfOverflow64(LebReaderMode readerMode, byte thisByte)
        {
            // Overflow check.
            if (readerMode == LebReaderMode.Signed)
            {
                var signAndContinuation = thisByte & 0x81;
                if (signAndContinuation == 0x01)
                {
                    // No continuation, bit 63=1, so number is negative.
                    if ((thisByte & 0x7E) != 0x7E)
                    {
                        throw new BinaryReaderSignedLebException("Invalid signed 64-bit LEB:  Bit 63 says the number is negative, so bits 64-69 must be 1.");
                    }
                }
                else if (signAndContinuation == 0x00)
                {
                    // No continuation, bit 63=0, so number is positive.
                    if ((thisByte & 0x7E) != 0x00)
                    {
                        throw new BinaryReaderSignedLebException("Invalid signed 64-bit LEB:  Bit 63 says the number is positive, so bits 64-69 must be 0.");
                    }
                }
                else
                {
                    throw new BinaryReaderLebLengthException("Invalid signed 64-bit LEB:  The LEB has more than 10 bytes.");
                }
            }
            else // reading unsigned
            {
                if ((thisByte & 0x80) == 0x80)
                {
                    throw new BinaryReaderLebLengthException("Invalid unsigned 64-bit LEB:  The LEB has more than 10 bytes.");
                }
                else if ((thisByte & 0x7E) != 0x00)
                {
                    throw new BinaryReaderUnsignedLebException("Invalid unsigned 64-bit LEB:  Bits 64-69 must be 0.");
                }
            }
        }



        public string ReadString()  // TODO: Do more efficiently
        {
            var stringLength = ReadLebUnsigned32();
            var resultString = String.Empty;
            while (stringLength > 0)
            {
                var thisChar = ReadByte();
                // TODO: Only supporting ASCII for now
                if (thisChar > 0x7F)
                {
                    throw new BinaryReaderStringException("Unsupported byte in string encoding.");
                }
                resultString += (char)thisChar; // TODO: efficiency
                --stringLength;
            }
            return resultString;
        }



        [StructLayout(LayoutKind.Explicit)]
        private struct FloatUintUnion
        {
            [FieldOffset(0)] public uint uint0;
            [FieldOffset(0)] public float float0;
        }
        public float ReadFloat32()
        {
            uint theAccumulator = 0;
            int shiftAmount = 0;
            for (int i = 0; i < 4; i++)
            {
                theAccumulator |= ((uint)ReadByte()) << shiftAmount;
                shiftAmount += 8;
            }
            return new FloatUintUnion { uint0 = theAccumulator }.float0;
        }



        [StructLayout(LayoutKind.Explicit)]
        private struct DoubleULongUnion
        {
            [FieldOffset(0)] public ulong ulong0;
            [FieldOffset(0)] public double double0;
        }
        public double ReadDouble64()
        {
            ulong theAccumulator = 0;
            int shiftAmount = 0;
            for (int i = 0; i < 8; i++)
            {
                theAccumulator |= ((ulong)ReadByte()) << shiftAmount;
                shiftAmount += 8;
            }
            return new DoubleULongUnion { ulong0 = theAccumulator }.double0;
        }



        public byte[] ReadByteVector() // TODO: Do more efficiently
        {
            var byteVectorLength = ReadLebUnsigned32();
            var newArray = new byte[byteVectorLength];
            for (uint i = 0; i < byteVectorLength; i++)
            {
                newArray[i] = ReadByte();
            }
            return newArray;
        }
    }
}
