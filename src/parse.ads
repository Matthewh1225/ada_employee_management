-- parse.ads
-- File parsing for reading Cars.txt

package Parse is

   -- Read and process the Cars.txt file
   -- Returns True if successful, False if error
   function Process_File (Filename : String) return Boolean;

end Parse;
