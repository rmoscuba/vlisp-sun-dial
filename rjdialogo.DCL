rj_dialogoPrincipal : dialog {

label = "  Datos del Reloj de Sol";
: row {
: boxed_radio_column {    // Define el area de botones de opción
  label = " &Tipo de Reloj ";
  : radio_button {       // Define la opción "Horizontal" 
    label = "&Horizontal";
    key = "rj_hrz";
    value = "1";
    width = 15;
    fixed_width = true;
  }
  : radio_button {        // Define la opción "Vertical"
    label = "&Vertical";
    key = "rj_vrt";
    width = 15;
    fixed_width = true;
    is_enabled = false;
  }
  : spacer {
    width = 1;
  }
  : radio_button {       // Define la opción "Ecuatorial" 
    label = "&Ecuatorial";
    key = "rj_ecu";
    width = 15;
    fixed_width = true;
    is_enabled = false;
 }
 : radio_button {       // Define la opción "Libre" 
    label = "&Libre";
    key = "rj_lib";
    width = 15;
    fixed_width = true;
    is_enabled = false;
 }
 : spacer { width = 1;}
 }
 : column {
: boxed_column {
    label = " &Posición ";
    : edit_box {      // Define el cuadro de texto "Latitud"
      label = "L&atitud";
      key = "rj_lat";
      edit_width = 6;
    }
    : edit_box {      // Define el cuadro de texto "Longitud"
      label = "L&ongitud";
      key = "rj_lng";
      edit_width = 6;
      //is_enabled = false;
    }
    }
    : boxed_row {
      //is_enabled = false;
      label = "&Uso horario";
      : popup_list {
        label = "";
        key = "rj_usg";
        edit_width = 2;
        fixed_width = true;
        fixed_width_font = true;
        list = "-\n+";
        //is_enabled = false;
      }
      : popup_list {
        label = "";
        key = "rj_uhr";
        edit_width = 3;
        fixed_width = true;
        fixed_width_font = true;
        list = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12";
        //is_enabled = false;
      }
      : popup_list {
        label = ":";
        key = "rj_umin";
        edit_width = 3;
        fixed_width = true;
        fixed_width_font = true;
        list = "0\n30\n45";
        //is_enabled = false;
      }    
    }
 }
}
: row {          // Define la fila OK/Cancel
  : spacer { width = 1; }
  : button {    // Define el botón OK 
    label = "Ok";
    is_default = true;
    key = "Aceptar";
    width = 12;
    fixed_width = true;
  }
  : button {    // Define el botón Cancel
    label = "Cancelar";
    is_cancel = true;
    key = "Cancelar";
    width = 12;
    fixed_width = true;
  }
  : spacer { width = 1; height = 2;}
}
}