-- FileParser.ads
-- Main file parsing coordinator - reads Cars.txt and creates employee/vehicle objects

package FileParser is

   -- Read and process the input file (Cars.txt)
   -- Returns True if successful, False if file error
   function Process_File (Filename : String) return Boolean;

end FileParser;
