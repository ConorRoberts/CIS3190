with text_io;
use text_io;

package body imagePGM is 
    function readPGM(fileName:string) return ImageRecord is
        imageRecord: ImageRecord; -- Record to be returned
        file: in_file; -- File pointer
        id: string; --Identifier
        width: int; -- Width of image
        height: int; -- Height of image
        maxValue: int; -- Max value of pixel
        i: int; -- Loop variable
        j: int; -- Loop variable
    begin

        open(file,in_file,fileName);

        get_line(file,id);
        -- get_line(file,width);
    -- generate error if identifier is not P2 or
    -- generate error if file has inconsistencies


    -- return record representing image
        close(fp);
        return imageRecord;

    end readPGM;

    function writePGM(rec:string) return void is
    begin

    -- write image to file as P2 PGM

        -- print hello
    
        print("hello");


        return;
    end writePGM;

end imagePGM;
