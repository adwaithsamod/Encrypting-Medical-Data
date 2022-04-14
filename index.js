const express = require("express");
 
const app = express();
 
const multer = require("multer");
 
const path = require('path')
 
const PORT = process.env.PORT || 5000;
 
app.set("view engine", "ejs");
 
var storage = multer.diskStorage({
  destination: function (req, file, cb) {
    cb(null, "public/uploads");
  },
  filename: function (req, file, cb) {
    cb(null, file.fieldname + "-" + Date.now() + path.extname(file.originalname));
  },
});
 
var upload = multer({ storage: storage });
 
var uploadMultiple = upload.fields([{ name: 'myImage' }])
 
 
app.get("/", (req, res) => {
  res.render("index");
});
 
app.post('/uploads', uploadMultiple, function (req, res, next) {
 
    if(req.files){
        // console.log(req.files)
 
        // console.log("files uploaded")

        res.render('index', {
            msg: 'Files Uploaded!',
            
          });
    }
    
})
 
app.listen(PORT, () => {
  console.log(`App is listening on Port ${PORT}`);
});