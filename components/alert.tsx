import * as React from "react";
import Button from "@mui/material/Button";
import Dialog from "@mui/material/Dialog";
import DialogActions from "@mui/material/DialogActions";
import DialogContent from "@mui/material/DialogContent";
import DialogContentText from "@mui/material/DialogContentText";
import DialogTitle from "@mui/material/DialogTitle";
import Link from "next/link";

export function Alert() {
  const [open, setOpen] = React.useState(true);

  const handleClose = () => {
    setOpen(false);
  };

  return (
    <React.Fragment>
      <Dialog
        open={open}
        onClose={handleClose}
        aria-labelledby="alert-dialog-title"
        aria-describedby="alert-dialog-description"
      >
        <DialogTitle id="alert-dialog-title">
          The website will migrate soon
        </DialogTitle>
        <DialogContent>
          <DialogContentText id="alert-dialog-description">
            This domain will expire in June 2024. You can still use this website
            at{" "}
            <Link href="https://thekanjimap.netlify.app">
              thekanjimap.netlify.app
            </Link>
          </DialogContentText>
        </DialogContent>
        <DialogActions>
          <Button onClick={handleClose} autoFocus>
            <Link href="https://thekanjimap.netlify.app">OK</Link>
          </Button>
        </DialogActions>
      </Dialog>
    </React.Fragment>
  );
}
