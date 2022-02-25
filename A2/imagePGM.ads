package imagePGM is

    type ImageRecord is record
        Image: Image;
        Label: Label;
    end record;

    function readPGM(fileName:string) return ImageRecord;

    function writePGM(rec:string) return void;
end imagePGM;