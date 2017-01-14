using System.Text;

namespace UnwindMC.Util
{
    public static class ByteArrayExtensions
    {
        public static ushort ReadUInt16(this byte[] bytes, int index)
        {
            ushort result = 0;
            for (int i = 1; i >= 0; i--)
            {
                result <<= 8;
                result += bytes[index + i];
            }
            return result;
        }

        public static uint ReadUInt32(this byte[] bytes, int index)
        {
            uint result = 0;
            for (int i = 3; i >= 0; i--)
            {
                result <<= 8;
                result += bytes[index + i];
            }
            return result;
        }

        public static string ReadZString(this byte[] bytes, int index)
        {
            var sb = new StringBuilder();
            byte b;
            while ((b = bytes[index++]) != 0)
            {
                sb.Append((char)b);
            }
            return sb.ToString();
        }
    }
}
