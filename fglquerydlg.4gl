
-- TODO:
-- Preview list with dynamic dialog (Use FIRST)
-- API to define a default criterion
-- Name queries and store in SQLite DB for reuse
-- Save/Restore params.*

-- Field input types
PUBLIC CONSTANT FGLQD_IT_SIMPLE    = 1   -- Simple input field
PUBLIC CONSTANT FGLQD_IT_FROM_TYPE = 2   -- Use widget corresponding to the field type (DATE = DATEEDIT)
PUBLIC CONSTANT FGLQD_IT_PICK_LIST = 3   -- User can choose from predefined list of values (pick list)

PRIVATE CONSTANT PREVIEW_MAXCOLS = 7

PRIVATE CONSTANT MODE_BAS = "B"
PRIVATE CONSTANT MODE_ADV = "A"

PRIVATE CONSTANT VALUE_IGN = 0
PRIVATE CONSTANT VALUE_COL = 1
PRIVATE CONSTANT VALUE_VAL = 2
PRIVATE CONSTANT VALUE_PKL = 3

PRIVATE CONSTANT OPER_AND = "&&"
PRIVATE CONSTANT OPER_OR  = "||"

PRIVATE CONSTANT OPER_ADD = "+"
PRIVATE CONSTANT OPER_SUB = "-"
PRIVATE CONSTANT OPER_MUL = "*"
PRIVATE CONSTANT OPER_DIV = "/"
PRIVATE CONSTANT OPER_CAT = "||"
PRIVATE CONSTANT OPER_RTO = ":"

PRIVATE CONSTANT OPER_EQ = "eq"
PRIVATE CONSTANT OPER_NE = "ne"
PRIVATE CONSTANT OPER_GT = "gt"
PRIVATE CONSTANT OPER_GE = "ge"
PRIVATE CONSTANT OPER_LT = "lt"
PRIVATE CONSTANT OPER_LE = "le"
PRIVATE CONSTANT OPER_BW = "bw"
PRIVATE CONSTANT OPER_EW = "ew"
PRIVATE CONSTANT OPER_CT = "ct"
PRIVATE CONSTANT OPER_RG = "rg"
PRIVATE CONSTANT OPER_NL = "nl"
PRIVATE CONSTANT OPER_NN = "nn"

PRIVATE DEFINE initialized BOOLEAN

PRIVATE TYPE t_crit_row RECORD
                  column STRING,
                  col_label STRING,
                  comp_oper CHAR(2),
                  value STRING,
                  val_label STRING,
                  val_type SMALLINT,
                  val_oper CHAR(2),
                  value2 STRING,
                  val2_label STRING,
                  val2_type SMALLINT,
                  line_oper CHAR(2)
        END RECORD
PRIVATE DEFINE crit DYNAMIC ARRAY OF t_crit_row

PRIVATE TYPE t_col_def RECORD
                  col_name STRING,
                  sql_type STRING,
                  dsp_name STRING,
                  inp_type SMALLINT,
                  pck_list SMALLINT
        END RECORD
PRIVATE TYPE t_rel_def RECORD
                  ref_table SMALLINT,
                  sql_cond STRING
        END RECORD
PRIVATE TYPE t_tab_def RECORD
                  tab_name STRING,
                  dsp_name STRING,
                  rel t_rel_def,
                  cols DYNAMIC ARRAY OF t_col_def
        END RECORD
PRIVATE TYPE t_pkl_def RECORD
                  name STRING,
                  items DYNAMIC ARRAY OF RECORD
                          vkey STRING,
                          vlabel STRING
                        END RECORD
        END RECORD

PRIVATE DEFINE qds DYNAMIC ARRAY OF RECORD
                  name STRING,
                  select_list STRING,
                  tabs DYNAMIC ARRAY OF t_tab_def,
                  pkls DYNAMIC ARRAY OF t_pkl_def
        END RECORD


PRIVATE DEFINE ta DYNAMIC ARRAY OF RECORD
                    name STRING,
                    label STRING
           END RECORD
PRIVATE DEFINE ca DYNAMIC ARRAY OF RECORD
                    name STRING,
                    label STRING
           END RECORD
PRIVATE DEFINE pl DYNAMIC ARRAY OF RECORD
                    vkey STRING,
                    vlabel STRING
           END RECORD

PRIVATE DEFINE params RECORD
                    mode CHAR(1),
                    case CHAR(1)
           END RECORD

PRIVATE DEFINE date_format STRING

#---

PUBLIC FUNCTION fglquerydlg_init()
    IF NOT initialized THEN
       LET initialized=TRUE
       CALL add_presentation_styles()
       LET date_format = "MDY(%2,%3,%1)"
    END IF
END FUNCTION

PUBLIC FUNCTION fglquerydlg_fini()
    LET initialized=FALSE
END FUNCTION

PUBLIC FUNCTION fglquerydlg_new(n)
    DEFINE n STRING
    DEFINE x SMALLINT
    LET x = qds.getLength()+1
    LET qds[x].name = n
    RETURN x
END FUNCTION

PUBLIC FUNCTION fglquerydlg_free(x)
    DEFINE x SMALLINT
    IF x>0 AND x<=qds.getLength() THEN
       CALL qds.deleteElement(x)
    END IF
END FUNCTION

PUBLIC FUNCTION fglquerydlg_set_select_list(qx, sl)
    DEFINE qx SMALLINT, sl STRING
    LET qds[qx].select_list = sl
END FUNCTION

PUBLIC FUNCTION fglquerydlg_add_table(qx, tn, dn)
    DEFINE qx SMALLINT, tn, dn STRING
    DEFINE x SMALLINT
    LET x = qds[qx].tabs.getLength()+1
    LET qds[qx].tabs[x].tab_name = tn
    LET qds[qx].tabs[x].dsp_name = dn
    RETURN x
END FUNCTION

PUBLIC FUNCTION fglquerydlg_add_column(qx, tx, cn, ct, it, lx, dn)
    DEFINE qx, tx, lx SMALLINT,
           cn, ct, dn STRING,
           it SMALLINT
    DEFINE x SMALLINT
    LET x = qds[qx].tabs[tx].cols.getLength()+1
    LET qds[qx].tabs[tx].cols[x].col_name = cn
    LET qds[qx].tabs[tx].cols[x].sql_type = ct
    LET qds[qx].tabs[tx].cols[x].dsp_name = NVL(dn,cn)
    LET qds[qx].tabs[tx].cols[x].inp_type = it
    LET qds[qx].tabs[tx].cols[x].pck_list = lx
END FUNCTION

PUBLIC FUNCTION fglquerydlg_set_relation(qx, tx, rtn, sql)
    DEFINE qx, tx SMALLINT,
           rtn STRING,
           sql STRING
    LET qds[qx].tabs[tx].rel.ref_table = tabs_lookup(qx, rtn)
    LET qds[qx].tabs[tx].rel.sql_cond = sql
END FUNCTION

PUBLIC FUNCTION fglquerydlg_add_pick_list(qx, nm)
    DEFINE qx SMALLINT, nm STRING
    DEFINE x SMALLINT
    LET x = qds[qx].pkls.getLength()+1
    LET qds[qx].pkls[x].name = nm
    RETURN x
END FUNCTION
     
PUBLIC FUNCTION fglquerydlg_add_pick_list_item(qx, lx, vk, vl)
    DEFINE qx, lx SMALLINT,
           vk, vl STRING
    DEFINE x SMALLINT
    LET x = qds[qx].pkls[lx].items.getLength()+1
    LET qds[qx].pkls[lx].items[x].vkey = vk
    LET qds[qx].pkls[lx].items[x].vlabel = vl
END FUNCTION

PRIVATE FUNCTION pick_list_lookup(qx, lx, vl)
    DEFINE qx, lx SMALLINT,
           vl STRING
    DEFINE x SMALLINT,
           vk STRING
    FOR x=1 TO qds[qx].pkls[lx].items.getLength()
        IF qds[qx].pkls[lx].items[x].vlabel == vl THEN
           RETURN qds[qx].pkls[lx].items[x].vkey
        END IF
    END FOR
    RETURN NULL
END FUNCTION

PRIVATE FUNCTION setup_mode(d,m)
    DEFINE d ui.Dialog, m CHAR(1)
    DEFINE f ui.Form,
           x SMALLINT
    LET f = d.getForm()
    CALL f.setFieldHidden("val_oper",  (m == MODE_BAS) )
    CALL f.setFieldHidden("val2_label",(m == MODE_BAS) )
    IF m==MODE_BAS THEN
       FOR x=1 TO crit.getLength()
           LET crit[x].val_oper = NULL
           LET crit[x].value2 = NULL
           LET crit[x].val2_type = NULL
           LET crit[x].val2_label = NULL
       END FOR
    END IF
END FUNCTION

PRIVATE FUNCTION set_crit_defaults(qx,f,x)
    DEFINE qx SMALLINT,
           f ui.Form,
           x SMALLINT
    LET crit[x].column = get_tab_col_name(qx,1,1)
    LET crit[x].col_label = get_tab_col_label(qx,1,1)
    LET crit[x].comp_oper = OPER_EQ
    LET crit[x].line_oper = OPER_AND
END FUNCTION

PUBLIC FUNCTION fglquerydlg_execute(qx,new)
    DEFINE qx SMALLINT,
           new BOOLEAN
    DEFINE r SMALLINT

    IF new THEN
       CALL crit.clear()
    END IF

    LET params.mode = MODE_BAS
    LET params.case = "S"

    OPEN WINDOW w_fglquerydlg WITH FORM "fglquerydlg"

    DIALOG ATTRIBUTES(UNBUFFERED)

      INPUT BY NAME params.* ATTRIBUTES(WITHOUT DEFAULTS)
        ON CHANGE mode
           CALL setup_mode(DIALOG,params.mode)
      END INPUT

      INPUT ARRAY crit FROM sr.* ATTRIBUTES(WITHOUT DEFAULTS, AUTO APPEND = FALSE)

        BEFORE INSERT
           CALL set_crit_defaults( qx, DIALOG.getForm(), arr_curr() )

        ON ACTION select_column
           CALL action_select_column(DIALOG, qx, arr_curr() )

        ON CHANGE comp_oper
           CALL setup_comp_oper(DIALOG, qx, arr_curr())

        ON ACTION select_value
           CALL action_select_value(DIALOG, qx, arr_curr())
        AFTER FIELD val_label
           IF NOT after_field_val_label(DIALOG, qx, arr_curr()) THEN
              NEXT FIELD val_label
           END IF

        ON CHANGE val_oper
           CALL setup_val_oper(qx,arr_curr())

        ON ACTION select_value_2
           CALL action_select_value2(DIALOG, qx, arr_curr())
        AFTER FIELD val2_label
           IF NOT after_field_val2_label(DIALOG, qx, arr_curr()) THEN
              NEXT FIELD val2_label
           END IF

      END INPUT

      BEFORE DIALOG
         CALL setup_mode(DIALOG,params.mode)
         IF new OR crit.getLength()==0 THEN
            CALL set_crit_defaults(qx,DIALOG.getForm(),1) -- Adds a first line
         END IF

      ON ACTION preview
         LET r = DIALOG.validate("sr.*")
         IF NOT after_field_val_label(DIALOG, qx, arr_curr()) THEN
            NEXT FIELD val_label
         END IF
         IF NOT after_field_val2_label(DIALOG, qx, arr_curr()) THEN
            NEXT FIELD val2_label
         END IF
         CALL preview_result_set(qx)

      ON ACTION accept
         IF INFIELD(val_label) THEN
            IF NOT after_field_val_label(DIALOG, qx, arr_curr()) THEN
               NEXT FIELD val_label
            END IF
         END IF
         IF INFIELD(val2_label) THEN
            IF NOT after_field_val2_label(DIALOG, qx, arr_curr()) THEN
               NEXT FIELD val2_label
            END IF
         END IF
         LET r = 0
         EXIT DIALOG

      ON ACTION cancel
         LET r = 1
         EXIT DIALOG

    END DIALOG

    CLOSE WINDOW w_fglquerydlg

    RETURN r

END FUNCTION

PRIVATE FUNCTION after_field_val_label(dlg, qx, x)
    DEFINE dlg ui.Dialog,
           qx SMALLINT,
           x SMALLINT
    DEFINE tmp1 STRING
    CALL check_value_input(qx, x, crit[x].val_label)
              RETURNING crit[x].value,
                        crit[x].val_type,
                        tmp1
    IF crit[x].val_type==-1 THEN
       RETURN FALSE
    ELSE
       LET crit[x].val_label = tmp1
       RETURN TRUE
    END IF
END FUNCTION

PRIVATE FUNCTION after_field_val2_label(dlg, qx, x)
    DEFINE dlg ui.Dialog,
           qx SMALLINT,
           x SMALLINT
    DEFINE tmp1 STRING
    CALL check_value_input(qx, x, crit[x].val2_label)
              RETURNING crit[x].value2,
                        crit[x].val2_type,
                        tmp1
    IF crit[x].val2_type==-1 THEN
       RETURN FALSE
    ELSE
       LET crit[x].val2_label = tmp1
       RETURN TRUE
    END IF
END FUNCTION

PRIVATE FUNCTION action_select_value(dlg,qx,x)
    DEFINE dlg ui.Dialog,
           qx SMALLINT,
           x SMALLINT
    DEFINE r SMALLINT,
           tmp1, tmp2 STRING
    CALL enter_value( qx, crit[x].column, crit[x].value, crit[x].val_label )
         RETURNING r, tmp1, tmp2
    IF r != VALUE_IGN THEN
       CALL dlg.setFieldTouched("sr.val_label",TRUE)
       IF r==VALUE_COL AND crit[x].val_type!=r THEN
          LET crit[x].comp_oper = OPER_EQ
          LET crit[x].val_oper = NULL
          LET crit[x].value = NULL
          LET crit[x].val2_label = NULL
          LET crit[x].val2_type = NULL
       END IF
       LET crit[x].value = tmp1
       LET crit[x].val_label = tmp2
       LET crit[x].val_type = r
    END IF
END FUNCTION

PRIVATE FUNCTION action_select_value2(dlg,qx,x)
    DEFINE dlg ui.Dialog,
           qx SMALLINT,
           x SMALLINT
    DEFINE r SMALLINT,
           tmp1, tmp2 STRING
    CALL enter_value( qx, crit[x].column, crit[x].value2, crit[x].val2_label )
         RETURNING r, tmp1, tmp2
    IF r != VALUE_IGN THEN
       CALL dlg.setFieldTouched("sr.val2_label",TRUE)
       LET crit[x].value2 = tmp1
       LET crit[x].val2_label = tmp2
       LET crit[x].val2_type = r
    END IF
END FUNCTION

PRIVATE FUNCTION equivalent_sql_types(t1,t2)
   DEFINE t1, t2 STRING
   CASE
     WHEN t1 MATCHES "*CHAR*"
       RETURN (t2 MATCHES "*CHAR*")
     WHEN t1 == "TINYINT"
       OR t1 == "SMALLINT"
       OR t1 == "INTEGER"
       OR t1 == "BIGINT"
       RETURN ( t2 == "TINYINT"
             OR t2 == "SMALLINT"
             OR t2 == "INTEGER"
             OR t2 == "BIGINT" )
     WHEN t1 MATCHES "DECIMAL*"
       OR t1 == "SMALLFLOAT"
       OR t1 == "FLOAT"
       RETURN ( t2 MATCHES "DECIMAL*"
             OR t2 == "SMALLFLOAT"
             OR t2 == "FLOAT" )
     OTHERWISE
       RETURN (t1 == t2)
   END CASE
END FUNCTION

PRIVATE FUNCTION action_select_column(dlg,qx,x)
    DEFINE dlg ui.Dialog,
           qx SMALLINT,
           x SMALLINT
    DEFINE r SMALLINT,
           tx, cx SMALLINT,
           ctp STRING,
           tmp1, tmp2 STRING
    LET ctp = get_column_sql_type( qx, crit[x].column )
    CALL enter_column( qx, crit[x].column, crit[x].col_label )
         RETURNING r, tmp1, tmp2
    IF r!=VALUE_COL THEN
       RETURN
    END IF
    CALL dlg.setFieldTouched("sr.col_label",TRUE)
    LET crit[x].column = tmp1
    LET crit[x].col_label = tmp2
    CALL cols_lookup(qx, crit[x].column) RETURNING tx, cx
    IF crit[x].comp_oper IS NULL THEN
       LET crit[x].comp_oper = OPER_EQ
    ELSE
       IF NOT oper_data_match( crit[x].comp_oper, qds[qx].tabs[tx].cols[cx].sql_type ) THEN
          LET crit[x].comp_oper = OPER_EQ
       END IF
    END IF
    IF qds[qx].tabs[tx].cols[cx].inp_type==FGLQD_IT_PICK_LIST
    OR NOT equivalent_sql_types(ctp, qds[qx].tabs[tx].cols[cx].sql_type)
    THEN
       LET crit[x].comp_oper = OPER_EQ
       LET crit[x].value = NULL
       LET crit[x].val_label = NULL
       LET crit[x].val_type = VALUE_PKL
    END IF
    LET crit[x].val_oper = NULL
    LET crit[x].value2 = NULL
    LET crit[x].val2_label = NULL
    LET crit[x].val2_type = NULL
END FUNCTION

PRIVATE FUNCTION setup_comp_oper(dlg,qx,x)
    DEFINE dlg ui.Dialog,
           qx SMALLINT,
           x SMALLINT
    DEFINE tx, cx SMALLINT
    CALL cols_lookup(qx, crit[x].column) RETURNING tx, cx
    IF NOT oper_data_match( crit[x].comp_oper, qds[qx].tabs[tx].cols[cx].sql_type ) THEN
       CALL mbox_ok("This operator cannot be used with current column type")
       LET crit[x].comp_oper = OPER_EQ
    END IF
    CASE
      WHEN crit[x].comp_oper == OPER_EQ
        OR crit[x].comp_oper == OPER_NE
        OR crit[x].comp_oper == OPER_GT
        OR crit[x].comp_oper == OPER_GE
        OR crit[x].comp_oper == OPER_LT
        OR crit[x].comp_oper == OPER_LE
           LET crit[x].val_oper = NULL
           LET crit[x].value2 = NULL
           LET crit[x].val2_label = NULL
           LET crit[x].val2_type = NULL
      WHEN crit[x].comp_oper == OPER_NN
        OR crit[x].comp_oper == OPER_NL
           LET crit[x].value = NULL
           LET crit[x].val_label = NULL
           LET crit[x].val_type = NULL
           LET crit[x].val_oper = NULL
           LET crit[x].value2 = NULL
           LET crit[x].val2_label = NULL
           LET crit[x].val2_type = NULL
      WHEN crit[x].comp_oper == OPER_BW
        OR crit[x].comp_oper == OPER_EW
        OR crit[x].comp_oper == OPER_CT
           LET crit[x].val_oper = NULL
           LET crit[x].value2 = NULL
           LET crit[x].val2_label = NULL
           LET crit[x].val2_type = NULL
           IF crit[x].val_type != VALUE_VAL THEN
              LET crit[x].value = NULL
              LET crit[x].val_label = NULL
              LET crit[x].val_type = VALUE_VAL
           END IF
      WHEN crit[x].comp_oper == OPER_RG
        LET params.mode = MODE_ADV
        CALL setup_mode(dlg,params.mode)
        LET crit[x].val_oper = OPER_RTO
    END CASE
END FUNCTION

PRIVATE FUNCTION setup_val_oper(qx,x)
    DEFINE qx, x SMALLINT
    DEFINE ctp STRING,
           ok BOOLEAN
    LET ctp = get_column_sql_type(qx,crit[x].column)
    LET ok = TRUE
    CASE
      WHEN ctp MATCHES "*CHAR*"
       IF crit[x].val_oper!=OPER_CAT THEN
          LET ok = FALSE
       END IF
      WHEN ctp MATCHES "DATE*"
       IF crit[x].val_oper IS NOT NULL THEN
          LET ok = FALSE
       END IF
      OTHERWISE
       IF crit[x].val_oper==OPER_CAT THEN
          LET ok = FALSE
       END IF
    END CASE
    IF NOT ok THEN
       CALL mbox_ok("This operation cannot be used with the current column type")
       LET crit[x].val_oper = NULL
       RETURN 
    END IF
    IF crit[x].val_oper == OPER_RTO THEN
       LET crit[x].comp_oper = OPER_RG
    ELSE
       IF crit[x].val_oper IS NOT NULL THEN
          IF crit[x].comp_oper == OPER_BW
          OR crit[x].comp_oper == OPER_EW
          OR crit[x].comp_oper == OPER_CT
          OR crit[x].comp_oper == OPER_NL
          OR crit[x].comp_oper == OPER_NN
          THEN
              LET crit[x].comp_oper = OPER_EQ
          END IF
       ELSE
          LET crit[x].value2 = NULL
          LET crit[x].val2_type = NULL
          LET crit[x].val2_label = NULL
       END IF
    END IF
END FUNCTION

PRIVATE FUNCTION check_value_input(qx, x, label)
    DEFINE qx, x SMALLINT,
           label STRING
    DEFINE tx, cx SMALLINT,
           value STRING,
           ctp STRING,
           val_type SMALLINT,
           v_date DATE,
           v_decimal DECIMAL,
           v_bigint BIGINT
    CALL cols_lookup(qx, crit[x].column) RETURNING tx, cx
    IF tx<=0 OR cx<=0 THEN
       RETURN NULL, 0, NULL
    END IF
    LET ctp = qds[qx].tabs[tx].cols[cx].sql_type
    IF qds[qx].tabs[tx].cols[cx].inp_type==FGLQD_IT_PICK_LIST THEN
       IF LENGTH(label)>0 THEN
          LET value = pick_list_lookup(qx, qds[qx].tabs[tx].cols[cx].pck_list, label)
          IF value IS NULL THEN
             CALL mbox_ok("The entered value does not match a pick list element")
             RETURN NULL, -1, NULL
          END IF
       END IF
       LET val_type = VALUE_PKL
    ELSE
       CALL cols_lookup_label(qx, label) RETURNING tx, cx
       IF tx>0 AND cx>0 THEN -- User entered [column name]
          LET value = get_tab_col_name(qx, tx, cx)
          LET val_type = VALUE_COL
          IF crit[x].comp_oper == OPER_BW
          OR crit[x].comp_oper == OPER_EW
          OR crit[x].comp_oper == OPER_CT
          OR crit[x].comp_oper == OPER_NL
          OR crit[x].comp_oper == OPER_NN
          THEN
             LET crit[x].comp_oper = OPER_EQ
          END IF
       ELSE
          LET val_type = VALUE_VAL
          IF label IS NULL THEN
             RETURN NULL, val_type, NULL
          END IF
          CASE
            WHEN ctp=="DATE"
              LET v_date = label
              IF v_date IS NULL THEN
                 CALL mbox_ok("The entered value is not a date")
                 RETURN NULL, -1, NULL
              END IF
              LET value = v_date
            WHEN ctp=="TINYINT"
              OR ctp=="SMALLINT"
              OR ctp=="INTEGER"
              OR ctp=="BIGINT"
              LET v_bigint = label
              IF v_bigint IS NULL THEN
                 CALL mbox_ok("The entered value is not an integer")
                 RETURN NULL, -1, NULL
              END IF
              LET value = v_bigint
            WHEN ctp=="SMALLFLOAT"
              OR ctp=="FLOAT"
              OR ctp MATCHES "DECIMAL*"
              LET v_decimal = label
              IF v_decimal IS NULL THEN
                 CALL mbox_ok("The entered value is not a number")
                 RETURN NULL, -1, NULL
              END IF
              LET value = v_decimal
            OTHERWISE
              LET value = label
          END CASE
          LET label = value
       END IF
    END IF
    RETURN value, val_type, label
END FUNCTION

PRIVATE FUNCTION get_case_sens(exp)
    DEFINE exp STRING
    IF params.case=="S" THEN
       RETURN exp
    ELSE
       RETURN SFMT("UPPER(%1)",exp)
    END IF
END FUNCTION

PRIVATE FUNCTION get_scalar_string(str)
    DEFINE str STRING
    DEFINE buf base.StringBuffer
    LET buf = base.StringBuffer.create()
    CALL buf.append(str)
    CALL buf.replace("'","''",0)
    CALL buf.replace("\\","\\\\",0)
    RETURN "'"||buf.toString()||"'"
END FUNCTION

-- %1 = year, %2 = month, %3 = day
PUBLIC FUNCTION fglquerydlg_set_date_format(fmt)
    DEFINE fmt STRING
    LET date_format = fmt
END FUNCTION

PRIVATE FUNCTION get_scalar_date(val)
    DEFINE val STRING
    DEFINE dv DATE
    LET dv = val
    IF dv IS NULL THEN
       RETURN NULL
    ELSE
       RETURN SFMT(date_format, YEAR(dv), MONTH(dv), DAY(dv) )
    END IF
END FUNCTION

PRIVATE FUNCTION get_scalar_datetime(ctp,val)
-- TODO: ISO to target DB datetime format
    DEFINE ctp, val STRING
    RETURN get_scalar_string(val)
END FUNCTION

PRIVATE FUNCTION build_scalar_value(ctp, vtp, val)
    DEFINE ctp STRING,
           vtp SMALLINT,
           val STRING
    CASE vtp
      WHEN VALUE_PKL RETURN val
      WHEN VALUE_COL RETURN val
      WHEN VALUE_VAL
        CASE
          WHEN ctp MATCHES "*CHAR*"    RETURN get_case_sens(get_scalar_string(val))
          WHEN ctp == "DATE"           RETURN get_scalar_date(val)
          WHEN ctp MATCHES "DATETIME*" RETURN get_scalar_datetime(ctp,val)
          OTHERWISE                    RETURN val
        END CASE
      OTHERWISE RETURN val
    END CASE 
END FUNCTION

PRIVATE FUNCTION build_value(ctp, vtp, val)
    DEFINE ctp STRING,
           vtp SMALLINT,
           val STRING
    IF vtp==VALUE_PKL THEN
       RETURN val
    ELSE
       RETURN build_scalar_value(ctp,vtp,val)
    END IF 
END FUNCTION

PRIVATE FUNCTION build_val_op_val(ctp, vtp, val, vop, vtp2, val2)
    DEFINE ctp STRING,
           vtp SMALLINT, val STRING,
           vop STRING,
           vtp2 SMALLINT, val2 STRING
    IF vop IS NULL OR LENGTH(val2)==0 THEN
       RETURN build_value(ctp,vtp,val)
    ELSE
       RETURN SFMT("(%1 %2 %3)", build_value(ctp,vtp,val),
                                 vop,
                                 build_value(ctp,vtp2,val2)
                  )
    END IF 
END FUNCTION


PRIVATE FUNCTION get_sql_expression(ctp,cn,oc,vtp,val,vop,vtp2,val2)
    DEFINE ctp STRING,
           cn STRING,
           oc STRING,
           vtp SMALLINT,
           val STRING,
           vop STRING,
           vtp2 SMALLINT,
           val2 STRING
    CASE oc
      WHEN OPER_NL RETURN " IS NULL"
      WHEN OPER_NN RETURN " IS NOT NULL"
      WHEN OPER_EQ RETURN SFMT(" = %1",  build_val_op_val(ctp,vtp,val,vop,vtp2,val2))
      WHEN OPER_NE RETURN SFMT(" <> %1", build_val_op_val(ctp,vtp,val,vop,vtp2,val2))
      WHEN OPER_LT RETURN SFMT(" < %1",  build_val_op_val(ctp,vtp,val,vop,vtp2,val2))
      WHEN OPER_LE RETURN SFMT(" <= %1", build_val_op_val(ctp,vtp,val,vop,vtp2,val2))
      WHEN OPER_GT RETURN SFMT(" > %1",  build_val_op_val(ctp,vtp,val,vop,vtp2,val2))
      WHEN OPER_GE RETURN SFMT(" >= %1", build_val_op_val(ctp,vtp,val,vop,vtp2,val2))
      WHEN OPER_BW RETURN " LIKE "||get_case_sens(get_scalar_string(     val||"%"))
      WHEN OPER_EW RETURN " LIKE "||get_case_sens(get_scalar_string("%"||val     ))
      WHEN OPER_CT RETURN " LIKE "||get_case_sens(get_scalar_string("%"||val||"%"))
      WHEN OPER_RG IF LENGTH(val2)==0 THEN
                      RETURN SFMT(" >= %1", build_scalar_value(ctp,vtp,val))
                   ELSE
                      RETURN SFMT(" BETWEEN %1 AND %2",
                                  build_scalar_value(ctp,vtp,val),
                                  build_scalar_value(ctp,vtp2,val2))
                   END IF
      OTHERWISE    RETURN NULL -- cannot happen
    END CASE
END FUNCTION

PRIVATE FUNCTION get_column_operand(ctp,col)
    DEFINE ctp, col STRING
    CASE
      WHEN ctp MATCHES "*CHAR*" RETURN get_case_sens(col)
      OTHERWISE                 RETURN col
    END CASE
END FUNCTION

PRIVATE FUNCTION build_line_condition(qx,tx,cx,x)
    DEFINE qx,tx,cx,x SMALLINT
    DEFINE cond STRING
    IF crit[x].value IS NULL THEN
       RETURN NULL
    END IF
    LET cond = get_column_operand(
                  qds[qx].tabs[tx].cols[cx].sql_type,
                  crit[x].column
               )
            || get_sql_expression(
                  qds[qx].tabs[tx].cols[cx].sql_type,
                  crit[x].column,
                  crit[x].comp_oper,
                  crit[x].val_type, crit[x].value,
                  crit[x].val_oper,
                  crit[x].val2_type, crit[x].value2)
    RETURN cond
END FUNCTION

PRIVATE FUNCTION add_table_to_use_list(qx,tx,txmap)
    DEFINE qx SMALLINT,
           tx SMALLINT,
           txmap DYNAMIC ARRAY OF BOOLEAN
    LET txmap[tx] = TRUE
    IF qds[qx].tabs[tx].rel.ref_table>0 THEN
       LET txmap[qds[qx].tabs[tx].rel.ref_table] = TRUE
    END IF
END FUNCTION

PRIVATE FUNCTION build_used_tabs_list(qx,txmap)
    DEFINE qx SMALLINT,
           txmap DYNAMIC ARRAY OF BOOLEAN
    DEFINE tx, cx, x SMALLINT
    CALL txmap.clear()
    FOR x=1 TO crit.getLength()
        CALL cols_lookup(qx, crit[x].column) RETURNING tx, cx
        CALL add_table_to_use_list(qx,tx,txmap)
        IF crit[x].val_type==VALUE_COL THEN
           CALL cols_lookup(qx, crit[x].value) RETURNING tx, cx
           CALL add_table_to_use_list(qx,tx,txmap)
        END IF
        IF crit[x].val2_type==VALUE_COL THEN
           CALL cols_lookup(qx, crit[x].value2) RETURNING tx, cx
           CALL add_table_to_use_list(qx,tx,txmap)
        END IF
    END FOR
END FUNCTION

PUBLIC FUNCTION fglquerydlg_get_from_part(qx)
    DEFINE qx SMALLINT
    DEFINE fp base.StringBuffer,
           txmap DYNAMIC ARRAY OF BOOLEAN,
           tx SMALLINT
    LET fp = base.StringBuffer.create()
    CALL build_used_tabs_list(qx,txmap)
    FOR tx=1 TO qds[qx].tabs.getLength()
        IF txmap[tx] THEN
           IF fp.getLength()>0 THEN
              CALL fp.append(",")
           END IF
           CALL fp.append(qds[qx].tabs[tx].tab_name)
        END IF
    END FOR
    RETURN fp.toString()
END FUNCTION

PUBLIC FUNCTION fglquerydlg_get_where_part(qx)
    DEFINE qx SMALLINT
    DEFINE wp, twp base.StringBuffer,
           txmap DYNAMIC ARRAY OF BOOLEAN,
           tx, cx, x, z SMALLINT,
           cond, rels STRING,
           cf BOOLEAN
    LET wp = base.StringBuffer.create()
    CALL build_used_tabs_list(qx,txmap)
    FOR tx=1 TO qds[qx].tabs.getLength()
        IF NOT txmap[tx] THEN CONTINUE FOR END IF
        LET cf = FALSE
        LET twp = base.StringBuffer.create()
        FOR x=1 TO crit.getLength()
            CALL cols_lookup(qx,crit[x].column) RETURNING z,cx
            IF z != tx THEN CONTINUE FOR END IF
            LET cond = build_line_condition(qx,tx,cx,x)
            IF cond IS NOT NULL THEN
               LET cf = TRUE
               IF twp.getLength()>0 THEN
                  IF crit[x-1].line_oper==OPER_AND THEN
                     CALL twp.append(" AND ")
                  ELSE
                     CALL twp.append(" OR ")
                  END IF
               END IF
               CALL twp.append( SFMT("(%1)", cond ) )
            END IF
        END FOR
        IF cf THEN
           IF wp.getLength()>0 THEN
              CALL wp.append("\n AND \n")
           END IF
           CALL wp.append("( ")
           CALL wp.append(twp.toString())
           IF qds[qx].tabs[tx].rel.ref_table>0 THEN
              CALL wp.append(" AND ( ")
              CALL wp.append(qds[qx].tabs[tx].rel.sql_cond)
              CALL wp.append(" )")
           END IF
           CALL wp.append(" )")
        END IF
    END FOR
    IF wp.getLength()==0 THEN
       RETURN "(1=1)"
    ELSE
       RETURN wp.toString()
    END IF
END FUNCTION

PRIVATE FUNCTION cols_lookup(qx,tcn)
    DEFINE qx SMALLINT, tcn STRING
    DEFINE tx, cx SMALLINT
    FOR tx=1 TO qds[qx].tabs.getLength()
        FOR cx=1 TO qds[qx].tabs[tx].cols.getLength()
            IF get_tab_col_name(qx,tx,cx)==tcn THEN
               RETURN tx, cx
            END IF
        END FOR
    END FOR
    RETURN 0, 0
END FUNCTION

PRIVATE FUNCTION cols_lookup_label(qx,tcl)
    DEFINE qx SMALLINT, tcl STRING
    DEFINE tx, cx SMALLINT
    FOR tx=1 TO qds[qx].tabs.getLength()
        FOR cx=1 TO qds[qx].tabs[tx].cols.getLength()
            IF get_tab_col_label(qx,tx,cx)==tcl THEN
               RETURN tx, cx
            END IF
        END FOR
    END FOR
    RETURN 0, 0
END FUNCTION

PRIVATE FUNCTION oper_data_match(op,tp)
    DEFINE op, tp STRING
    CASE
      WHEN op==OPER_BW
        OR op==OPER_EW
        OR op==OPER_CT
           RETURN (tp MATCHES "*CHAR*")
      OTHERWISE
        RETURN TRUE
    END CASE
END FUNCTION

PRIVATE FUNCTION tabs_lookup(qx,tn)
    DEFINE qx SMALLINT,
           tn STRING
    DEFINE x SMALLINT
    FOR x=1 TO qds[qx].tabs.getLength()
        IF qds[qx].tabs[x].tab_name==tn THEN
           RETURN x
        END IF
    END FOR
    RETURN 0
END FUNCTION

PRIVATE FUNCTION get_tab_col_name(qx,tx,cx)
    DEFINE qx,tx,cx SMALLINT
    IF tx<1 OR tx>qds[qx].tabs.getLength() THEN RETURN NULL END IF
    IF cx<1 OR cx>qds[qx].tabs[tx].cols.getLength() THEN RETURN NULL END IF
    RETURN SFMT('%1.%2', qds[qx].tabs[tx].tab_name, qds[qx].tabs[tx].cols[cx].col_name)
END FUNCTION

PRIVATE FUNCTION get_tab_col_label(qx,tx,cx)
    DEFINE qx,tx,cx SMALLINT
    DEFINE z SMALLINT
    IF tx<1 OR tx>qds[qx].tabs.getLength() THEN RETURN NULL END IF
    IF cx<1 OR cx>qds[qx].tabs[tx].cols.getLength() THEN RETURN NULL END IF
    RETURN SFMT("[%1: %2]", qds[qx].tabs[tx].dsp_name, qds[qx].tabs[tx].cols[cx].dsp_name)
END FUNCTION

PRIVATE FUNCTION fill_ta(qx)
    DEFINE qx SMALLINT
    DEFINE tx SMALLINT
    CALL ta.clear()
    FOR tx=1 TO qds[qx].tabs.getLength()
        LET ta[tx].name = qds[qx].tabs[tx].tab_name
        LET ta[tx].label = qds[qx].tabs[tx].dsp_name
    END FOR
END FUNCTION

PRIVATE FUNCTION fill_pl(qx,tcn)
    DEFINE qx SMALLINT,
           tcn STRING
    DEFINE tx, cx, lx, x SMALLINT
    CALL cols_lookup(qx, tcn) RETURNING tx, cx
    IF tx>0 AND cx>0 THEN
       LET lx = qds[qx].tabs[tx].cols[cx].pck_list
       CALL pl.clear()
       FOR x=1 TO qds[qx].pkls[lx].items.getLength()
           LET pl[x].vkey   = qds[qx].pkls[lx].items[x].vkey
           LET pl[x].vlabel = qds[qx].pkls[lx].items[x].vlabel
       END FOR
    END IF
END FUNCTION

PRIVATE FUNCTION fill_ca(qx,tx)
    DEFINE qx, tx STRING
    DEFINE cx SMALLINT
    CALL ca.clear()
    FOR cx=1 TO qds[qx].tabs[tx].cols.getLength()
        LET ca[cx].name = get_tab_col_name(qx,tx,cx)
        LET ca[cx].label = qds[qx].tabs[tx].cols[cx].dsp_name
    END FOR
END FUNCTION

PRIVATE FUNCTION get_column_input_type(qx,tcn)
    DEFINE qx SMALLINT,
           tcn STRING
    DEFINE tx, cx SMALLINT
    CALL cols_lookup(qx, tcn) RETURNING tx, cx
    IF tx>0 AND cx>0 THEN
       RETURN qds[qx].tabs[tx].cols[cx].inp_type
    ELSE
       RETURN 0
    END IF
END FUNCTION

PRIVATE FUNCTION get_column_sql_type(qx,tcn)
    DEFINE qx SMALLINT,
           tcn STRING
    DEFINE tx, cx SMALLINT
    CALL cols_lookup(qx, tcn) RETURNING tx, cx
    IF tx>0 AND cx>0 THEN
       RETURN qds[qx].tabs[tx].cols[cx].sql_type
    ELSE
       RETURN NULL
    END IF
END FUNCTION

PRIVATE FUNCTION enter_val_col_base(mode,qx,tcn,cc,cl)
    DEFINE mode CHAR(1),
           qx SMALLINT,
           tcn,cc,cl STRING
    DEFINE tx,cx,vx SMALLINT,
           f ui.Form,
           it SMALLINT,
           dt STRING,
           r SMALLINT,
           rec RECORD
                   cst_ed STRING,
                   cst_da DATE,
                   cst_dt DATETIME YEAR TO SECOND
               END RECORD

    LET it = get_column_input_type(qx,tcn)
    LET dt = get_column_sql_type(qx,tcn)

    IF mode=="C" THEN
       CALL fill_ta(qx)
    ELSE
       IF it==FGLQD_IT_PICK_LIST THEN
          CALL fill_pl(qx,tcn)
       ELSE
          CALL fill_ta(qx)
          LET rec.cst_ed = cl
          LET rec.cst_da = cl -- Can fail, ignore.
          LET rec.cst_dt = cl -- Can fail, ignore.
       END IF
    END IF

    OPEN WINDOW w_fglquerydlg_value WITH FORM "fglquerydlg_value"

    DIALOG ATTRIBUTES(UNBUFFERED) 

        INPUT BY NAME rec.* ATTRIBUTES(WITHOUT DEFAULTS)
        END INPUT

        DISPLAY ARRAY ta TO sr_tabs.* ATTRIBUTES(DOUBLECLICK=_none_)
            BEFORE ROW
               CALL fill_ca(qx,arr_curr())
               CALL DIALOG.setCurrentRow("sr_cols",1)
        END DISPLAY
        DISPLAY ARRAY ca TO sr_cols.*
        END DISPLAY

        DISPLAY ARRAY pl TO sr_vals.*
        END DISPLAY

        BEFORE DIALOG
           LET f = DIALOG.getForm()
           CALL f.setElementHidden("page1", NOT (mode=="V" AND it!=FGLQD_IT_PICK_LIST) )
           CALL f.setElementHidden("page2", NOT (mode=="C" OR it!=FGLQD_IT_PICK_LIST) )
           CALL f.setFieldHidden("cst_ed", 1)
           CALL f.setFieldHidden("cst_da", 1)
           CALL f.setFieldHidden("cst_dt", 1)
           IF mode="V" AND it!=FGLQD_IT_PICK_LIST THEN
              CASE
                WHEN dt == "DATE"            CALL f.setFieldHidden("cst_da", 0)
                WHEN dt MATCHES "DATETIME*"  CALL f.setFieldHidden("cst_dt", 0)  
                OTHERWISE                    CALL f.setFieldHidden("cst_ed", 0)    
              END CASE
           END IF
           CALL f.setElementHidden("page3", NOT (mode=="V" AND it==FGLQD_IT_PICK_LIST) )

        ON ACTION accept
           CASE DIALOG.getCurrentItem()
             WHEN "sr_cols"
               LET tx = DIALOG.getCurrentRow("sr_tabs")
               LET cx = DIALOG.getCurrentRow("sr_cols")
               LET cc = ca[cx].name
               LET cl = get_tab_col_label(qx,tx,cx)
               LET r = VALUE_COL
             WHEN "sr_vals"
               LET vx = DIALOG.getCurrentRow("sr_vals")
               LET cc = pl[vx].vkey
               LET cl = pl[vx].vlabel
               LET r = VALUE_PKL
             OTHERWISE
               CASE
                 WHEN dt == "DATE"             LET cc = rec.cst_da
                 WHEN dt MATCHES "DATETIME*"   LET cc = rec.cst_dt
                 OTHERWISE                     LET cc = rec.cst_ed
               END CASE
               LET cl = cc
               LET r = VALUE_VAL
           END CASE
           EXIT DIALOG

        ON ACTION cancel
           LET r = VALUE_IGN
           EXIT DIALOG

    END DIALOG

    CLOSE WINDOW w_fglquerydlg_value

    RETURN r, cc, cl

END FUNCTION

PRIVATE FUNCTION enter_value(qx,tcn,cc,cl)
    DEFINE qx SMALLINT,
           tcn,cc,cl STRING
    DEFINE r SMALLINT
    CALL enter_val_col_base("V",qx,tcn,cc,cl) RETURNING r, cc, cl
    RETURN r, cc, cl
END FUNCTION

PRIVATE FUNCTION enter_column(qx,cc,cl)
    DEFINE qx SMALLINT,
           cc,cl STRING
    DEFINE r SMALLINT
    CALL enter_val_col_base("C",qx,cc,cc,cl) RETURNING r, cc, cl
    RETURN r, cc, cl
END FUNCTION

PRIVATE FUNCTION fields_from_columns(qx, cursor, maxcols, fields)
    DEFINE qx SMALLINT,
           maxcols SMALLINT,
           cursor base.SqlHandle,
           fields DYNAMIC ARRAY OF RECORD
               name STRING,
               type STRING,
               label STRING
           END RECORD
    DEFINE x SMALLINT,
           tcn STRING,
           tx, cx SMALLINT
    CALL fields.clear()
    FOR x=1 TO maxcols
        LET fields[x].name = SFMT("formonly.col%1",x)
        IF x <= cursor.getResultCount() THEN
           LET fields[x].label = SFMT("Column %1",x)
           LET fields[x].type = cursor.getResultType(x)
           LET tcn = cursor.getResultName(x)
           CALL cols_lookup(qx,tcn) RETURNING tx, cx
           IF tx==0 THEN
              LET tcn = qds[qx].tabs[1].tab_name || "." || tcn
              CALL cols_lookup(qx,tcn) RETURNING tx, cx
           END IF
           IF tx>0 AND cx>0 THEN
              LET fields[x].label = qds[qx].tabs[tx].cols[cx].dsp_name
           END IF
        ELSE
           LET fields[x].type = "CHAR(1)"
        END IF
    END FOR
END FUNCTION

PRIVATE FUNCTION set_preview_column_titles(frm,maxcols,fields)
    DEFINE frm ui.Form,
           maxcols SMALLINT,
           fields DYNAMIC ARRAY OF RECORD
               name STRING,
               type STRING,
               label STRING
           END RECORD,
           x SMALLINT,
           pn, cn, cc om.DomNode
    LET pn = frm.getNode()
    FOR x=1 TO maxcols
        LET cn = get_aui_node(pn, "TableColumn", fields[x].name)
        IF cn IS NOT NULL THEN
           LET cc = cn.getFirstChild()
           IF LENGTH(fields[x].label)>0 THEN
              CALL cn.setAttribute("text",fields[x].label)
              CALL cc.setAttribute("hidden",0)
           ELSE
              CALL cn.setAttribute("text",NULL)
              CALL cc.setAttribute("hidden",1)
           END IF
        END IF
    END FOR
END FUNCTION

PRIVATE FUNCTION preview_result_set(qx)
    DEFINE qx SMALLINT
    DEFINE stmt STRING,
           cursor base.SqlHandle,
           fields DYNAMIC ARRAY OF RECORD
               name STRING,
               type STRING,
               label STRING
           END RECORD,
           rsdlg ui.Dialog,
           x SMALLINT,
           f ui.Form

    LET cursor = base.SqlHandle.create()
    LET stmt = "SELECT ", NVL(qds[qx].select_list, "*"),
               " FROM ", fglquerydlg_get_from_part(qx),
               " WHERE ", fglquerydlg_get_where_part(qx)
display stmt
    TRY
        CALL cursor.prepare(stmt)
        CALL cursor.open()
    CATCH
        CALL mbox_ok(SFMT("Could not execute SQL statement:\n%1: %2",SQLCA.SQLCODE,SQLERRMESSAGE))
        RETURN
    END TRY
    CALL fields_from_columns(qx, cursor, PREVIEW_MAXCOLS, fields)
    LET rsdlg = ui.Dialog.createDisplayArrayTo(fields,"pv")
    CALL rsdlg.deleteAllRows("pv")
    WHILE TRUE
        TRY
            CALL cursor.fetch() 
        CATCH
            CALL mbox_ok(SFMT("Could not fetch data row:\n%1: %2",SQLCA.SQLCODE,SQLERRMESSAGE))
            RETURN
        END TRY
        IF SQLCA.SQLCODE==NOTFOUND THEN
           EXIT WHILE
        END IF
        CALL rsdlg.appendRow("pv")
        CALL rsdlg.setCurrentRow("pv",rsdlg.getArrayLength("pv"))
        FOR x=1 TO cursor.getResultCount()
            CALL rsdlg.setFieldValue(fields[x].name,cursor.getResultValue(x))
        END FOR
    END WHILE
    CALL cursor.close() 
    IF rsdlg.getArrayLength("pv")==0 THEN
       CALL mbox_ok("No rows found.")
       RETURN
    END IF
    CALL rsdlg.setCurrentRow("pv",1)
    CALL rsdlg.addTrigger("ON ACTION pvstop")
    CALL set_preview_column_titles(rsdlg.getForm(), PREVIEW_MAXCOLS, fields)
    WHILE TRUE
        CASE rsdlg.nextEvent()
          WHEN "ON ACTION pvstop" EXIT WHILE
        END CASE
    END WHILE
END FUNCTION

FUNCTION fglquerydlg_cmb_comp_oper_init(cmb)
    DEFINE cmb ui.ComboBox
    CALL cmb.addItem(OPER_EQ, "equals")
    CALL cmb.addItem(OPER_NE, "not equals")
    CALL cmb.addItem(OPER_GT, "greater than")
    CALL cmb.addItem(OPER_GE, "greater or = ")
    CALL cmb.addItem(OPER_LT, "lower than")
    CALL cmb.addItem(OPER_LE, "lower or =")
    CALL cmb.addItem(OPER_BW, "begins with")
    CALL cmb.addItem(OPER_EW, "ends with")
    CALL cmb.addItem(OPER_CT, "contains")
    --CALL cmb.addItem(OPER_OO, "is one of")
    CALL cmb.addItem(OPER_RG, "in range")
    CALL cmb.addItem(OPER_NL, "is null")
    CALL cmb.addItem(OPER_NN, "is not null")
END FUNCTION

FUNCTION fglquerydlg_cmb_val_oper_init(cmb)
    DEFINE cmb ui.ComboBox
    CALL cmb.addItem(OPER_ADD, " + ")
    CALL cmb.addItem(OPER_SUB, " - ")
    CALL cmb.addItem(OPER_MUL, " x ")
    CALL cmb.addItem(OPER_DIV, " / ")
    CALL cmb.addItem(OPER_CAT, " || ")
    CALL cmb.addItem(OPER_RTO, " to ")
END FUNCTION

FUNCTION fglquerydlg_cmb_line_oper_init(cmb)
    DEFINE cmb ui.ComboBox
    CALL cmb.addItem(OPER_AND, "and")
    CALL cmb.addItem(OPER_OR , "or")
END FUNCTION

PRIVATE FUNCTION mbox_ok(msg)
    DEFINE msg STRING
    MENU "Query Dialog" ATTRIBUTES(STYLE="dialog", COMMENT=msg)
        COMMAND "Ok" EXIT MENU
    END MENU
END FUNCTION

PRIVATE FUNCTION get_aui_node(p, tagname, name)
    DEFINE p om.DomNode,
           tagname STRING,
           name STRING
    DEFINE nl om.NodeList
    IF name IS NOT NULL THEN
       LET nl = p.selectByPath(SFMT("//%1[@name=\"%2\"]",tagname,name))
    ELSE
       LET nl = p.selectByPath(SFMT("//%1",tagname))
    END IF
    IF nl.getLength() == 1 THEN
       RETURN nl.item(1)
    ELSE
       RETURN NULL
    END IF
END FUNCTION

PRIVATE FUNCTION add_style(pn, name)
    DEFINE pn om.DomNode,
           name STRING
    DEFINE nn om.DomNode
    LET nn = get_aui_node(pn, "Style", name)
    IF nn IS NOT NULL THEN RETURN NULL END IF
    LET nn = pn.createChild("Style")
    CALL nn.setAttribute("name", name)
    RETURN nn
END FUNCTION

PRIVATE FUNCTION set_style_attribute(pn, name, value)
    DEFINE pn om.DomNode,
           name STRING,
           value STRING
    DEFINE sa om.DomNode
    LET sa = get_aui_node(pn, "StyleAttribute", name)
    IF sa IS NULL THEN
       LET sa = pn.createChild("StyleAttribute")
       CALL sa.setAttribute("name", name)
    END IF
    CALL sa.setAttribute("value", value)
END FUNCTION

PUBLIC FUNCTION add_presentation_styles()
    DEFINE rn om.DomNode,
           sl om.DomNode,
           nn om.DomNode
    LET rn = ui.Interface.getRootNode()
    LET sl = get_aui_node(rn, "StyleList", NULL)
    --
    LET nn = add_style(sl, "Window.fglquerydlg")
    IF nn IS NOT NULL THEN
       CALL set_style_attribute(nn, "windowType", "modal" )
       CALL set_style_attribute(nn, "sizable", "yes" )
       CALL set_style_attribute(nn, "actionPanelPosition", "none" )
       CALL set_style_attribute(nn, "ringMenuPosition", "none" )
       CALL set_style_attribute(nn, "statusBarType", "none" )
       CALL set_style_attribute(nn, "errorMessagePosition", "popup" )
    END IF
    LET nn = add_style(sl, "ButtonEdit.fglquerydlg_column")
    IF nn IS NOT NULL THEN
       CALL set_style_attribute(nn, "backgroundColor", "lightYellow" )
    END IF
    LET nn = add_style(sl, "ButtonEdit.fglquerydlg_value")
    IF nn IS NOT NULL THEN
       CALL set_style_attribute(nn, "backgroundColor", "lightCyan" )
    END IF
END FUNCTION
