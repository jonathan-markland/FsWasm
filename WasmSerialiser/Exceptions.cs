
using System;

namespace WasmSerialiser
{
    public class BinaryReaderStringException : Exception
    {
        public BinaryReaderStringException(string message) : base(message)
        {
        }
    }

    public class BinaryReaderLebLengthException : Exception
    {
        public BinaryReaderLebLengthException(string s) : base(s) { }
    }

    public class BinaryReaderUnsignedLebException : Exception
    {
        public BinaryReaderUnsignedLebException(string s) : base(s) { }
    }

    public class BinaryReaderSignedLebException : Exception
    {
        public BinaryReaderSignedLebException(string s) : base(s) { }
    }
}