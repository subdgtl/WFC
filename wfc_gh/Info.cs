using System;
using Grasshopper.Kernel;
using System.IO;

namespace wfc_gh
{
    public class Info : GH_AssemblyInfo
    {
        private System.Drawing.Bitmap componentIcon;

        public Info()
        {
            using (var ms = new MemoryStream(Resources.ComponentIcon))
            {
                componentIcon = new System.Drawing.Bitmap(ms);
            }
        }

        public override Guid Id => new Guid("1277e762-d959-464b-a68c-b4881d99b969");
        public override string Name => "WFC Solver";
        // TODO(yan): Use another icon here?
        public override System.Drawing.Bitmap Icon => componentIcon;
        public override string Description => "Wave Function Collapse solver(s)";
        public override string AuthorName => "Subdigital";
        public override string AuthorContact => "www.sub.digital | info@sub.digital";
    }
}
