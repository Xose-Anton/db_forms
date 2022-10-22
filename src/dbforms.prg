#include "hwgui.ch"
#include "hwg_extctrl.ch"
#include "hbclass.ch"

FUNCTION pFrmDRun()
    LOCAL cFile, oDesig, cFrm
    cFile := "Frm_prueba.frm"
    cFrm := IIF(file(cFile), hb_MemoRead(cFile), "")
    oDesig := FrmDesig():New(cFrm)
    oDesig:Run()
/*    
    oDesig:Design()
    IF oDesig:lSal
        cFrm := oDesig:GetFrm()
        hb_MemoWrit(cFile,cFrm)
    ENDIF 
*/
    oDesig := NIL
RETURN NIL 
//--------------------------------------------------------------------------------
FUNCTION pFrmDesig()
    LOCAL cFile, oDesig, cFrm
    cFile := "Frm_prueba.frm"
    cFrm := IIF(file(cFile), hb_MemoRead(cFile), "")
    oDesig := FrmDesig():New(cFrm)
    oDesig:Design()
    IF oDesig:lSal
        cFrm := oDesig:GetFrm()
        hb_MemoWrit(cFile,cFrm)
    ENDIF 
    oDesig := NIL
RETURN NIL 
//--------------------------------------------------------------------------------
CLASS FrmDesig
DATA oFrmMain
DATA oFrmNew 
DATA oPnlWnd
DATA hComponents INIT { => }
DATA cForm
DATA hForm 
DATA lSal       INIT .F.
DATA lRun       INIT .F.
DATA consola   //TEMPORAL AL ELIMINAR

METHOD New(cFrm)
METHOD Design()     INLINE ::ActWin()       // Modo diseño
METHOD ActWin()                             // Activa las ventanas de diseño
METHOD Run()
METHOD InitComps()  // INICIALIZA LOS COMPONENTES DISPONIBLES
METHOD GetFrm()
METHOD SetcForm()
METHOD SetoForm()
METHOD RefreshForm()    
METHOD ObjectToHash(obj)
METHOD AddNewCtrl(oCtrl)
METHOD PosNewCtrl(oCtrl)   
METHOD CountCtrl(cTip) 
METHOD OnCtrlEvent(msg, wParam, lParam)
METHOD CalRedimCtrl(oCtrl, x, y, lcurs)
METHOD OnFocus(oCtrl, handle)
METHOD OnGetFocus(oCtrl, a,b)
METHOD OnLostFocus(oCtrl, a,b)
METHOD CargaCtrl(oCtrl)

END CLASS 
//--------------------------------------------------------------------
METHOD Run()           CLASS FrmDesig
    ::lRun := .T.
    INIT WINDOW ::oFrmNew CHILD APPNAME "XaFDesig" TITLE "New win";
        AT 300,100 SIZE 300,300 ;
        ICON HIcon():AddResource( "ICOJS" );
        STYLE WS_CAPTION+WS_SYSMENU+WS_MINIMIZEBOX+WS_MAXIMIZEBOX+WS_SIZEBOX;
        ON EXIT {| | ::lSal:=.T. }

    ::InitComps()
    ::SetoForm()

    ACTIVATE WINDOW ::oFrmNew


    do while !::lSal .AND. Len(::oFrmNew:oParent:aWindows)>0
        hwg_doEvents()
    end do

RETURN NIL 
//--------------------------------------------------------------------
METHOD InitComps()           CLASS FrmDesig
//    LOCAL hC, nPos  
    ::hComponents["HSTATIC"] := {"BITMAP"=>"DSG_LABEL", "OBJ"=>NIL , "TOOLTIP"=>"Etiquta de texto", "WIDTH"=>18, "HEIGHT"=>18, ;
        "ETIQUETA"=>"Label", "WINCLASS"=>"STATIC", "CREATE"=>"HStatic()", "PNEW"=>{::oFrmMain}, "DW"=>56, "DH"=>22, "HBCLASS"=>"HSTATIC"}

    ::hComponents["HEDIT"] := {"BITMAP"=>"DSG_EDIT", "OBJ"=> NIL, "TOOLTIP"=>"Campo de edición", "WIDTH"=>18, "HEIGHT"=>18, ; 
        "ETIQUETA"=>"Edit", "WINCLASS"=>"EDIT", "CREATE"=>"HEdit()", "PNEW"=>{::oFrmMain}, "DW"=>56, "DH"=>22, "HBCLASS"=>"HEDIT" }

    ::hComponents["HOWNBUTTON"] := {"BITMAP"=>"DSG_BOTON", "OBJ"=> NIL, "TOOLTIP"=>"Boton configurable", "WIDTH"=>18, "HEIGHT"=>18, ; 
        "ETIQUETA"=>"OwButton", "WINCLASS"=>"OWNBTN", "CREATE"=>"HOwnButton()", "PNEW"=>{::oFrmMain}, "DW"=>56, "DH"=>22, "HBCLASS"=>"HOWNBUTTON" }

    ::hComponents["HBUTTON"] := {"BITMAP"=>"DSG_BOTON", "OBJ"=> NIL, "TOOLTIP"=>"Boton windows", "WIDTH"=>18, "HEIGHT"=>18, ; 
        "ETIQUETA"=>"Button", "WINCLASS"=>"BUTTON", "CREATE"=>"HButton()", "PNEW"=>{::oFrmMain}, "DW"=>56, "DH"=>22, "HBCLASS"=>"HBUTTON" }
        
    ::hComponents["HBROWSE"] := {"BITMAP"=>"DSG_BROWSE", "OBJ"=> NIL, "TOOLTIP"=>"Grid compuesto", "WIDTH"=>18, "HEIGHT"=>18, ; 
        "ETIQUETA"=>"Browse", "WINCLASS"=>"BROWSE", "CREATE"=>"HBrowse()", "PNEW"=>{BRW_ARRAY, ::oFrmMain}, "DW"=>100, "DH"=>100, "HBCLASS"=>"HBROWSE" }
       

RETURN NIL     
//--------------------------------------------------------------------    
//--------------------------------------------------------------------
METHOD New(cFrm) CLASS FrmDesig
    ::cForm := cFrm
    ::hForm :=  hb_jsonDecode(cFrm)
    //::ActWin()
RETURN Self 
//--------------------------------------------------------------------
METHOD GetFrm() CLASS FrmDesig
    //::cForm := hb_jsonEncode()// "FRM Generado"
RETURN ::cForm
//--------------------------------------------------------------------    
METHOD CargaCtrl(oCtrl) CLASS FrmDesig
    LOCAL PnlCtrl, oC, hC, pNew, oNC, i, key, value  
    IF !hb_HHasKey(::hComponents, hb_HKeyAt(oCtrl,1))
        RETURN NIL
    ENDIF 
    hC := ::hComponents[hb_HKeyAt(oCtrl,1)]
    pNew := AClone( hC["PNEW"] )
    oC := hb_HValueAt(oCtrl,1)
    IF !::lRun
        PnlCtrl := HPanel():New(::oFrmNew,,,oC["__pAt"][1],oC["__pAt"][2],oC["NWIDTH"],oC["NHEIGHT"],NIL,NIL,NIL,NIL)
        PnlCtrl:bSize := NIL
    ENDIF 
    FOR i := 1 TO Len(pNew)
        IF pNew[i]:ClassName()=="HCHILDWINDOW"
            pNew[i] := PnlCtrl
        ENDIF 
    NEXT     
    oNC := &(hC["CREATE"]):New(hb_ArrayToParams(pNew))
    FOR i := 1 TO Len(oC)
        key := hb_HKeyAt(oC,i) ;  value := hb_HValueAt(oC,i )
        IF !__objHasData(oNC,key)
            __objAddData( oNC, key )    
        ENDIF
        oNC:&(key) := value 
    NEXT 
    oNC:Move(oC["__pAt"][1],oC["__pAt"][2],oNC:nWidth,oNC:nHeight)
    IF !::lRun
        oNC:Move(0,0,oNC:nWidth,oNC:nHeight)
    ENDIF 
    IF ::lRun 
        oNC:Enable()
    ENDIF
    IF !::lRun
        PnlCtrl:bOther := { | oCtrl, msg, wParam, lParam | ::OnCtrlEvent(oCtrl, msg, wParam, lParam)  }
        __objAddData( PnlCtrl, "UserData" )
        PnlCtrl:UserData := { => }
        PnlCtrl:UserData["Drag"] := .F.
        PnlCtrl:UserData["Size"] := .F.
        PnlCtrl:UserData["Selected"] := .F.
        PnlCtrl:UserData["DesingTime"] := .T.
        PnlCtrl:bGetFocus  := {|oCtrl, a,b| ::OnGetFocus(oCtrl, a,b)}
        PnlCtrl:bLostFocus := {|oCtrl, a,b| ::OnLostFocus(oCtrl, a,b)}
    ENDIF 
RETURN NIL 
//--------------------------------------------------------------------    
METHOD SetoForm() CLASS FrmDesig
    LOCAL i, key, Value, z, oNCtrl 
    IF ValType(::hForm) =="H" .AND. Len(::hForm)>0
        IF hb_HHasKey(::hForm,"NLEFT") .AND. hb_HHasKey(::hForm,"NTOP") .AND. hb_HHasKey(::hForm,"NWIDTH") .AND. hb_HHasKey(::hForm,"NHEIGHT")
            ::oFrmNew:Move(::hForm["NLEFT"],::hForm["NTOP"],::hForm["NWIDTH"],::hForm["NHEIGHT"])
        ENDIF
        FOR i := 1 TO Len(::hForm)
            key :=hb_HKeyAt(::hForm,i)
            Value :=hb_HValueAt(::hForm, i)
            //__objSendMsg(::oFrmNew, key, value )//hb_HKeyAt(::hForm,i), hb_HValueAt(::hForm, i) )
            IF key == "ACONTROLS"
                FOR z := 1 TO Len(Value)
                    ::CargaCtrl(Value[z])
                NEXT
            ELSE
                ::oFrmNew:&(key):=value 
            ENDIF
        NEXT 
    ELSE 
        ::oFrmNew:Move(10,10,300,300)
    ENDIF
    ::RefreshForm()
RETURN NIL    
//--------------------------------------------------------------------
METHOD SetcForm() CLASS FrmDesig
    LOCAL hFrmP
    hFrmP := ::ObjectToHash(::oFrmNew)
    ::cForm := hb_jsonEncode(hFrmP, .T.)
    ::lSal := .T.
RETURN NIL 
//--------------------------------------------------------------------    
METHOD RefreshForm()    CLASS FrmDesig
    ::oFrmNew:Move( ::oFrmNew:nLeft, ::oFrmNew:nTop, ::oFrmNew:nWidth, ::oFrmNew:nHeight)
    ::oFrmNew:SetTitle(::oFrmNew:TITLE)
RETURN NIL
//--------------------------------------------------------------------    
METHOD ObjectToHash(obj)    CLASS FrmDesig
    LOCAL valtmp, ppts, i, z, oHash := { => }, aCoors,aPt, aExcl 
    aCoors := hwg_Getwindowrect(::oFrmNew:handle )
    aPt := hwg_ScreenToClient(::oPnlWnd:handle, aCoors[1], aCoors[2])
    ::oFrmNew:nLeft := aPt[1]
    ::oFrmNew:nTop := aPt[2]  
    IF ValType(obj)=="O" .AND. obj:ClassName()=="HPANEL" .AND. __objHasData(obj,"UserData") .AND. hb_HHasKey(obj:UserData,"DesingTime")
        IF obj:UserData["DesingTime"] .AND. Len(obj:aControls)>0
            IF obj:UserData["Selected"]
                ::OnLostFocus(obj)
            ENDIF 
            oHash[obj:aControls[1]:ClassName()] := ::ObjectToHash(obj:aControls[1])
            oHash[obj:aControls[1]:ClassName()]["__pAt"] :={obj:nLeft,obj:nTop}
            RETURN oHash
        ENDIF
    ENDIF     
    aExcl := {"AICONS","HANDLE","OPARENT","ODEFAULTPARENT","AWINDOWS","NOLDX","NOLDY","LRESIZEX","LRESIZEY","NSIZE",;
        "ROWPOSOLD","ABRUSHES", ,"BRUSH"}
    ppts := __objGetValueList(obj, aExcl)
    //tmp := hb_Serialize(obj)
    FOR i:=1 TO Len(ppts)        
        valtmp := ppts[i][2]
        IF ValType(valtmp)=="A"
            oHash[ppts[i][1]] := {}
            FOR z :=1 TO Len(valtmp)
                AAdd(oHash[ppts[i][1]], IIF(ValType(valtmp[z])=="O", ::ObjectToHash(valtmp[z]), valtmp[z] ) )
            NEXT 
        ELSE 
            oHash[ppts[i][1]] := IIF(ValType(ppts[i][2])=="O", ::ObjectToHash(ppts[i][2]), ppts[i][2])
        ENDIF
    NEXT

RETURN oHash 
//-------------------------------------------------------------------- 
METHOD PosNewCtrl(oCtrl)      CLASS FrmDesig
    LOCAL aPos:={}, x:=0,y:=0, i 
    FOR I :=1 TO Len(::oFrmNew:aControls)
        y := Max(::oFrmNew:aControls[i]:nTop, y )
    NEXT 
    FOR I :=1 TO Len(::oFrmNew:aControls)
        IF ::oFrmNew:aControls[i]:nTop > y-1
            x := Max(::oFrmNew:aControls[i]:nLeft+::oFrmNew:aControls[i]:nWidth+1, x) 
        ENDIF  
    NEXT 
    IF 31 + x + oCtrl:nWidth > ::oFrmNew:nWidth
        x := 0
        y := y + oCtrl:nHeight
    ENDIF 
    AAdd(aPos,x)
    AAdd(aPos,y)
RETURN aPos
//-------------------------------------------------------------------- 
METHOD CountCtrl(cTip)      CLASS FrmDesig
    LOCAL i,z:=1
    FOR i :=1 TO Len(::oFrmNew:aControls)
        z := z + IIF(::oFrmNew:aControls[i]:aControls[1]:winclass==cTip, 1, 0)
    NEXT 
RETURN z 
//-------------------------------------------------------------------- 
METHOD CalRedimCtrl(oCtrl, x, y, lcurs)      CLASS FrmDesig
    LOCAL redim :=0, aPt , cursor, nLf, nTp, nWi,nHe, bord:=4
    aPt := hwg_ClientToScreen(oCtrl:handle,x,y)
    aPt := hwg_ScreenToClient(oCtrl:oParent:handle, aPt[1], aPt[2] )
    x := aPt[1]  ; y := aPt[2]
    nLf := oCtrl:nLeft ;         nTp := oCtrl:nTop
    nWi := oCtrl:nWidth ;        nHe := oCtrl:nHeight
    IF Abs(nLf+nWi-x) < bord .AND. Abs(nTp+nHe-y) < bord //Bottom Rigth
        cursor := IDC_SIZENWSE 
        redim :=1
    ELSEIF Abs(nLf-x) < bord .AND. Abs(nTp+nHe-y) < bord //Bottom Left
        cursor := IDC_SIZENESW 
        redim :=2
    ELSEIF Abs(nLf+nWi-x) < bord .AND. Abs(nTp-y) < bord //Top Rigth
        cursor := IDC_SIZENESW 
        redim :=3
    ELSEIF Abs(nLf-x) < bord .AND. Abs(nTp-y) < bord //Top Left    
        cursor := IDC_SIZENWSE 
        redim :=4
    ELSEIF Abs(nLf-x) < bord                         // Left    
        cursor := IDC_SIZEWE
        redim :=5
    ELSEIF Abs(nTp-y) < bord                         // TOP
        cursor := IDC_SIZENS
        redim :=6
    ELSEIF Abs(nLf+nWi-x) < bord                    // right
        cursor := IDC_SIZEWE
        redim :=7
    ELSEIF Abs(nTp+nHe-y) < bord                    //Bottom 
        cursor := IDC_SIZENS
        redim :=8
    ELSE 
        hwg_SetCursor(hwg_Loadcursor( IDC_HAND ))
    ENDIF 

    IF lcurs .AND. redim > 0
        hwg_SetCursor(hwg_Loadcursor( cursor ))
    ENDIF        
RETURN redim
//-------------------------------------------------------------------- 
METHOD OnCtrlEvent(oCtrl, msg, wParam, lParam)      CLASS FrmDesig
    LOCAL h, w, sRect, aPt, redim := 0 , x, y, nalt   

    IF msg==WM_CAPTURECHANGED
        ::OnFocus(oCtrl, lParam)
    ENDIF

    nalt := IIF( msg == WM_MOUSEMOVE, hwg_PtrToULong(wParam),-1)
    IF msg == WM_MOUSEMOVE .AND. !oCtrl:UserData["Drag"] .AND. !oCtrl:UserData["Size"]
        redim := ::CalRedimCtrl(oCtrl, hwg_Loword( lParam ), hwg_Hiword( lParam ), .T.)    
    ELSEIF msg == WM_LBUTTONDOWN
        redim := ::CalRedimCtrl(oCtrl, hwg_Loword( lParam ), hwg_Hiword( lParam ), .F.)
        IF redim==0 //hwg_Hiword( lParam ) < 19 .AND. redim==0
             oCtrl:UserData["Drag"] := .T.
             hwg_Setcapture(oCtrl:handle)
        ENDIF
        IF redim > 0
            oCtrl:UserData["Size"] := .T.
            oCtrl:UserData["Redim"] := redim
            hwg_Setcapture(oCtrl:handle)
        ENDIF 
    ELSEIF msg == WM_LBUTTONUP
        oCtrl:UserData["Drag"] := .F.  
        oCtrl:UserData["Size"] := .F.
        hwg_ReleaseCapture()   
    ELSEIF msg == WM_MOUSEMOVE .AND. oCtrl:UserData["Drag"]
        IF nalt==0
            oCtrl:UserData["Drag"] := .F.
            hwg_ReleaseCapture()   
            RETURN -1
        ENDIF
        hwg_SetCursor(hwg_Loadcursor( IDC_HAND ))
        sRect := hwg_Getwindowrect(oCtrl:handle )
        w := sRect[3]-sRect[1]
        h := sRect[4]-sRect[2] 
        aPt := hwg_ClientToScreen(oCtrl:handle,hwg_Loword( lParam ),hwg_Hiword( lParam ))
        aPt := hwg_ScreenToClient(oCtrl:oParent:handle, aPt[1], aPt[2])
        IF aPt[1] > oCtrl:oParent:nWidth .OR. apt[2] > oCtrl:oParent:nHeight .OR. aPt[1] < 0 .OR. aPt[2] < 0 
              RETURN -1
        ENDIF
        //hwg_MoveWindow(oCtrl:handle, aPt[1]-(w/2), aPt[2]-(h/5), w, h , .T.)
        oCtrl:Move(aPt[1]-(w/2), aPt[2]-(h/5), w, h)
    ELSEIF msg == WM_MOUSEMOVE .AND. oCtrl:UserData["Size"]
        IF nalt==0
            oCtrl:UserData["Size"] := .F.
            hwg_ReleaseCapture()   
            RETURN -1
        ENDIF 
        redim := oCtrl:UserData["Redim"]
        x := xaw_Loword( hwg_PtrToULong(lParam) )
        y := xaw_Hiword( hwg_PtrToULong(lParam) )
        ::CalRedimCtrl(oCtrl, x, y, .T.)
        aPt := hwg_ClientToScreen(oCtrl:handle,x,y)
        aPt := hwg_ScreenToClient(oCtrl:oParent:handle, aPt[1], aPt[2])
        IF redim==1             //Bottom Rigth
            oCtrl:Move( NIL, NIL, aPt[1] - oCtrl:nLeft, aPt[2]- oCtrl:nTop)
        ELSEIF redim == 2      //Bottom Left
            //w := oCtrl:nWidth + oCtrl:nLeft - aPt[1] ; h := aPt[2]- oCtrl:nTop
            oCtrl:Move( aPt[1] ,NIL, oCtrl:nWidth + oCtrl:nLeft - aPt[1], aPt[2]- oCtrl:nTop)
        ELSEIF redim == 3      //Top Rigth
           // w := aPt[1] - oCtrl:nLeft  ; h := aPt[2] - oCtrl:nTop + oCtrl:nHeight
            oCtrl:Move( NIL ,aPt[2], aPt[1] - oCtrl:nLeft , oCtrl:nTop - aPt[2]  + oCtrl:nHeight)
        ELSEIF redim == 4       //Top Left    
            oCtrl:Move( aPt[1] ,aPt[2], oCtrl:nLeft - aPt[1] +oCtrl:nWidth , oCtrl:nTop - aPt[2]  + oCtrl:nHeight)
        ELSEIF redim == 5       // Left    
            oCtrl:Move( aPt[1] ,NIL, oCtrl:nLeft - aPt[1] +oCtrl:nWidth , NIL)
        ELSEIF redim == 6        // TOP
            oCtrl:Move( NIL ,aPt[2], NIL , oCtrl:nTop - aPt[2]  + oCtrl:nHeight)
        ELSEIF redim == 7        // right
            oCtrl:Move( NIL ,NIL, aPt[1] - oCtrl:nLeft , NIL)   
        ELSEIF redim == 8        //Bottom 
            oCtrl:Move( NIL ,NIL, NIL , aPt[2] - oCtrl:nTop  )
        ENDIF
        IF oCtrl:UserData["Selected"]
            oCtrl:aControls[1]:Move(NIL, NIL, oCtrl:nWidth-1, oCtrl:nHeight-1)
        ELSE 
            oCtrl:aControls[1]:Move(NIL, NIL, oCtrl:nWidth, oCtrl:nHeight)
        ENDIF

    ELSE 
       // ::consola:Value := ::consola:Value +","+ str(msg)
    ENDIF 
RETURN -1
//--------------------------------------------------------------------
METHOD OnFocus(oCtrl, handle)      CLASS FrmDesig
    LOCAL  i, oCl 
    FOR i :=1 TO Len(oCtrl:oParent:aControls)
        oCl := oCtrl:oParent:aControls[i]
        IF __objHasData(oCl, "UserData") .AND. hb_HHasKey(oCl:UserData, "Selected")
            IF oCl:UserData["Selected"] .AND. !Empty( oCl:bLostFocus)
                Eval(oCl:bLostFocus,oCl)
            ENDIF
            oCl:UserData["Selected"] := .F.
        ENDIF
    NEXT 
    IF __objHasData(oCtrl, "UserData") .AND. hb_HHasKey(oCtrl:UserData, "Selected")
        IF !Empty( oCtrl:bGetFocus)
            Eval(oCtrl:bGetFocus,oCtrl)
        ENDIF
        oCtrl:UserData["Selected"] := .T.
    ENDIF
RETURN -1
//-------------------------------------------------------------------- 
METHOD OnGetFocus(oCtrl, a,b)      CLASS FrmDesig
    oCtrl:BackColor(hwg_ColorC2N("0000FF"))
    oCtrl:Move(oCtrl:nLeft-1,oCtrl:nTop-1,oCtrl:nWidth+2,oCtrl:nHeight+2)
    oCtrl:aControls[1]:Move(1,1)
RETURN NIL
//--------------------------------------------------------------------     
METHOD OnLostFocus(oCtrl, a,b)      CLASS FrmDesig
    oCtrl:Move(oCtrl:nLeft+1,oCtrl:nTop+1,oCtrl:nWidth-2,oCtrl:nHeight-2)
    oCtrl:aControls[1]:Move(0,0,oCtrl:nWidth,oCtrl:nHeight)
RETURN NIL
//-------------------------------------------------------------------- 
METHOD AddNewCtrl(oCtrl)      CLASS FrmDesig
    LOCAL i, z, aCoors, aPtD, aPtO, aPtN, NewCtrl, cName, hC, create, PnlCtrl 
    FOR i:=1 TO Len(::hComponents)
        //::hComponents[oCtrl:ObjName]["OBJ"]:Disable()
        hb_HValueAt(::hComponents, i)["OBJ"]:Disable()
    NEXT 
    aCoors := hwg_Getwindowrect(::oFrmNew:handle )
    aPtD := hwg_ScreenToClient(::oFrmMain:handle, aCoors[1], aCoors[2])
    aCoors := hwg_Getwindowrect(oCtrl:handle )
    aPtO := hwg_ScreenToClient(::oFrmMain:handle, aCoors[1], aCoors[2])
    hC := ::hComponents[oCtrl:ObjName]
    cName := hC["ETIQUETA"] + AllTrim(str(::CountCtrl(hC["WINCLASS"])))
    //create := StrTran(hC["CREATE"],"<oWndParent>","::oFrmMain")
    NewCtrl := &(hC["CREATE"]):New(hb_ArrayToParams(hC["PNEW"]))
    NewCtrl:Disable()
    NewCtrl:Move(aPtO[2],aPtO[1], hC["DW"],hC["DH"])
    hwg_SetCtrlName(NewCtrl,cName)
    //NewCtrl:bOther := {|NewCtrl,msg, wParam, lParam|::OnCtrlEvent(NewCtrl, msg, wParam, lParam)}

    IF __objHasData(NewCtrl,"TITLE") ; NewCtrl:Title := cName; NewCtrl:SetText(cName);  ENDIF
    IF __objHasData(NewCtrl,"CAPTION") ; NewCtrl:Caption := cName; ENDIF
    IF __objHasData(NewCtrl,"VALUE") ; NewCtrl:Value := cName; ENDIF

    __objAddData( NewCtrl, "UserData" )
    NewCtrl:UserData := { => }
//    NewCtrl:UserData["Drag"] := .F.

    aPtN := ::PosNewCtrl(oCtrl)
    aPtD[1]:=aPtD[1]+8  + aPtN[1]
    aPtD[2]:=aPtD[2]+31 + aPtN[2]

    z:=aPtO[2]
    FOR i:= aPtO[1] TO aPtD[1] 
        z := IIF( z < aPtD[2], z+1 ,z)
        NewCtrl:Move(i,z)
        hwg_doEvents()
    NEXT 
    FOR z:=z TO aPtD[2]
        NewCtrl:Move(NIL, z)
        hwg_doEvents()
    NEXT 
    aCoors := hwg_Getwindowrect(NewCtrl:handle )
    aPtD := hwg_ScreenToClient(::oFrmNew:handle, aCoors[1], aCoors[2])
                                    //WS_BORDER+WS_THICKFRAME
    PnlCtrl := HPanel():New(::oFrmNew,,,aPtD[1],aPtD[2],NewCtrl:nWidth,NewCtrl:nHeight)
    PnlCtrl:bOther := { | oCtrl, msg, wParam, lParam | ::OnCtrlEvent(oCtrl, msg, wParam, lParam)  }
    __objAddData( PnlCtrl, "UserData" )
    PnlCtrl:UserData := { => }
    PnlCtrl:UserData["Drag"] := .F.
    PnlCtrl:UserData["Size"] := .F.
    PnlCtrl:UserData["Selected"] := .F.
    PnlCtrl:UserData["DesingTime"] := .T.
    PnlCtrl:bGetFocus  := {|oCtrl, a,b| ::OnGetFocus(oCtrl, a,b)}
    PnlCtrl:bLostFocus := {|oCtrl, a,b| ::OnLostFocus(oCtrl, a,b)}
    PnlCtrl:AddControl(NewCtrl)
    NewCtrl:oParent := PnlCtrl
    Hwg_SetParent(NewCtrl:handle, PnlCtrl:handle)
    NewCtrl:Move(0, 0)
    hwg_doEvents()
//    ::oFrmNew:AddControl(NewCtrl)
//    NewCtrl:oParent := ::oFrmNew
//    Hwg_SetParent(NewCtrl:handle, ::oFrmNew:handle)
//    NewCtrl:Move(aPtD[1], aPtD[2])
    FOR i:=1 TO Len(::hComponents)
        //::hComponents[oCtrl:ObjName]["OBJ"]:Enable()
        hb_HValueAt(::hComponents, i)["OBJ"]:Enable()
    NEXT 
RETURN NIL
//-------------------------------------------------------------------- 
METHOD ActWin() CLASS FrmDesig
    LOCAL oStatusBar, aStatus1, aStatus2, oToolbar, oToolbPnl, oPnlProp, oBrwProp , oSplitV, i, hC, nPos
    Local aSample := { {"Alex",17,1200}, {"Victor",42,1600}, {"John",31,1000} }
    local ocont 
    //LOCAL oWndProp
    INIT WINDOW ::oFrmMain CHILD APPNAME "XaFDesig" TITLE "XaApp Designer";
        AT 582,184 SIZE 782,722 ;
        ICON HIcon():AddResource( "ICOJS" );
        STYLE WS_CAPTION+WS_SYSMENU+WS_MINIMIZEBOX+WS_MAXIMIZEBOX+WS_SIZEBOX; //; //STYLE WS_SYSMENU+WS_SIZEBOX+WS_VISIBLE;
        ON SIZE {|ctrl,a,b,c,d| OnFrmMainMove(ctrl,a,b,c,d)};
        ON EXIT {| | ::SetcForm() }

    MENU OF ::oFrmMain
        MENU TITLE "&File"
            MENUITEM "&Message" ACTION hwg_Msginfo( "Just a test", "Message" )
            SEPARATOR
            MENUITEM "&Exit" ACTION ::oFrmMain:Close()
        ENDMENU
        MENU TITLE "&Help"
            MENUITEM "&About" ACTION hwg_Msginfo( "Just a test", "Message" )
        ENDMENU
    ENDMENU

    ADD STATUS oStatusBar TO ::oFrmMain  ; 
    PARTS aStatus1, aStatus2

    @ 0,0 REBAR oToolbar OF ::oFrmMain SIZE 500,26 ;  // BUTTONWIDTH 30
    STYLE  CCS_NOPARENTALIGN+WS_BORDER +RBS_BANDBORDERS + RBS_DBLCLKTOGGLE + RBS_REGISTERDROP
     
    oToolbPnl:=HPanel():New( oToolbar,,,0,0,24,24,,,{|ctrl,ptr| PanelPaint(ctrl,ptr)},, )
    ADDBAND oToolbPnl:handle to oToolbar 

    ::InitComps()
    nPos := 2
    FOR i:=1 TO Len(::hComponents)
        hC := ::hComponents[hb_HKeyAt(::hComponents, i )]
        @ nPos,2 OWNERBUTTON hC["OBJ"] OF oToolbPnl  SIZE hC["WIDTH"], hC["HEIGHT"]  ;
            BITMAP hC["BITMAP"] FROM RESOURCE TRANSPARENT FLAT;
            ON CLICK {| oCtrl | ::AddNewCtrl(oCtrl) }   TOOLTIP hC["TOOLTIP"]
        hC["OBJ"]:ObjName := hb_HKeyAt(::hComponents, i )
        nPos := nPos + hC["WIDTH"]+3
    NEXT

    //PANEL IZQUIERDO
    //STYLE  WS_BORDER ;
    @ 1,oToolbar:nHeight PANEL oPnlProp  OF ::oFrmMain SIZE ::oFrmMain:nWidth/10*3,::oFrmMain:nHeight/10*6 ;         
        on size ANCHOR_TOPABS + ANCHOR_LEFTABS + ANCHOR_BOTTOMABS;
        ON PAINT {|ctrl,ptr| PanelPaint(ctrl,ptr)} ;
        BACKCOLOR ::oFrmMain:bcolor 

/*    INIT DIALOG oWndProp  TITLE "Propiedades";
            AT 1,oToolbar:nHeight SIZE ::oFrmMain:nWidth/10*3,::oFrmMain:nHeight/10*6 ;           // ICON HIcon():AddResource( "ICOJS" ) ;
            STYLE WND_NOTITLE //+WND_NOSYSMENU+WND_NOSIZEBOX    // WS_EX_TOOLWINDOW-(12582912 + 524288 + 262144 + 131072)-WS_OVERLAPPEDWINDOW //WND_NOTITLE+WS_VSCROLL+WS_HSCROLL //WS_CAPTION  //+WS_SYSMENU+WS_MINIMIZEBOX+WS_MAXIMIZEBOX+WS_SIZEBOX  

    oWndProp:oParent := oPnlProp 
    Hwg_SetParent(oWndProp:handle, oPnlProp:handle)
*/
    @ 4,1  SAY "Propiedades" OF oPnlProp SIZE oPnlProp:nWidth-8, 20 ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS
    @ 4,20 BROWSE oBrwProp ARRAY OF oPnlProp SIZE oPnlProp:nWidth-8,oPnlProp:nHeight-30  ;
            ON SIZE ANCHOR_TOPABS + ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS;
            NOBORDER     
    //oBrwProp:active:=.F.
    hwg_CreateArList( oBrwProp, aSample )
    oBrwProp:aColumns[1]:heading := "Name"
    oBrwProp:aColumns[2]:heading := "Age"
    //PANEL derecho
    @ oPnlProp:nWidth+8,oToolbar:nHeight PANEL ::oPnlWnd  OF ::oFrmMain SIZE ::oFrmMain:nWidth-oPnlProp:nWidth-26,oPnlProp:nHeight ; 
        STYLE  0; // WS_VSCROLL + WS_HSCROLL ; //WS_BORDER + 
        on size ANCHOR_TOPABS + ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS;
        ON PAINT {|ctrl,ptr| PanelPaint(ctrl,ptr)}     
     //SPLITTER   
    @ oPnlProp:nWidth+3,oToolbar:nHeight SPLITTER oSplitV OF ::oFrmMain SIZE 4,oPnlProp:nHeight DIVIDE {oPnlProp} FROM {::oPnlWnd}; //LIMITS 400,400
        ON SIZE {|oSplitV| onSplitVSize(oSplitV) }
  
    /// TEMPORAL CONSOLA
    //@ 0, oPnlProp:nHeight + oToolbar:nHeight +10 EDITBOX ::consola CAPTION ""  OF ::oFrmMain  SIZE ::oFrmMain:nWidth -8, ::oFrmMain:nHeight - (oPnlProp:nHeight + oToolbar:nHeight +10) ;
    @  20, oPnlProp:nHeight + oToolbar:nHeight +30 CONTAINER ocont  OF ::oFrmMain  SIZE ::oFrmMain:nWidth -158, ::oFrmMain:nHeight - (oPnlProp:nHeight + oToolbar:nHeight +140) ;
        STYLE 3 ;
    on size ANCHOR_TOPREL + ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
        BACKSTYLE 1


    ocont:bpaint:= {|ctrl,ptr| ContPaint(ctrl,ptr)} 
    
//---------------------------------------------------------------------------------------
    
    INIT WINDOW ::oFrmNew CHILD APPNAME "XaFDesig" TITLE "New win";
        AT 10,10 SIZE 300,300 ;
        ICON HIcon():AddResource( "ICOJS" );
        STYLE WS_CAPTION+WS_SYSMENU+WS_MINIMIZEBOX+WS_MAXIMIZEBOX+WS_SIZEBOX
        
    
    ::oFrmNew:oParent := ::oPnlWnd
    Hwg_SetParent(::oFrmNew:handle, ::oPnlWnd:handle)

    ACTIVATE WINDOW ::oFrmMain
 //   ACTIVATE WINDOW oWndProp
    ACTIVATE WINDOW ::oFrmNew

    //::oFrmNew:Move(0,0,300,300)
    
    hwg_doEvents()
    hwg_doEvents()
    //::oFrmNew:Move(10,10,300,300)
    ::SetoForm()
    
    do while !::lSal .AND. Len(::oFrmMain:oParent:aWindows)>0
        hwg_doEvents()
    end do

RETURN NIL
//--------------------------------------------------------------
STATIC FUNCTION OnFrmMainMove(ctrl,nWidth,nHeight)
    LOCAL i, oTool 
    oTool := ctrl:FindControl("oToolbar")
    IF !Empty(oTool)
        oTool:Move(NIL,NIL,nWidth,NIL)
    ENDIF
    //IF ctrl::ClassName()=="HCHILDWINDOW"
    //    FOR i:=1 TO Len(ctrl:aControls)

        //ctrl:FindControl("oToolbar"):Move(NIL,NIL,nWidth,NIL)
    //ENDIF 
RETURN NIL
//-----------------------------------------------------------
STATIC FUNCTION onSplitVSize(ctrl)
    ctrl:Move(NIL,NIL,NIL,ctrl:oParent:FindControl("oPnlProp"):nHeight)
RETURN NIL
//-----------------------------------------------------------
STATIC FUNCTION ContPaint(ctrl,ptr)
    LOCAL drawInfo, hDC, oPen
    LOCAL x1, y1, x2, y2, TRANSPARENT := 1
 
    drawInfo := hwg_Getdrawiteminfo( ptr )
    hDC := drawInfo[ 3 ]
    x1  := drawInfo[ 4 ]
    y1  := drawInfo[ 5 ]
    x2  := drawInfo[ 6 ]
    y2  := drawInfo[ 7 ]
 
    hwg_Selectobject( hDC, ctrl:oPen:handle )
 
    IF ctrl:ncStyle != NIL
       hwg_Setbkmode( hDC, ctrl:backStyle )
       IF ! ctrl:lnoBorder
          IF ctrl:ncStyle == 0      // RAISED
             hwg_Drawedge( hDC, x1, y1, x2, y2,BDR_RAISED,BF_LEFT+BF_TOP+BF_RIGHT+BF_BOTTOM)  // raised  forte      8
          ELSEIF ctrl:ncStyle == 1  // sunken
             hwg_Drawedge( hDC, x1, y1, x2, y2,BDR_SUNKEN,BF_LEFT+BF_TOP+BF_RIGHT+BF_BOTTOM ) // sunken mais forte
          ELSEIF ctrl:ncStyle == 2  // FRAME
             hwg_Drawedge( hDC, x1, y1, x2, y2,BDR_RAISED+BDR_RAISEDOUTER,BF_LEFT+BF_TOP+BF_RIGHT+BF_BOTTOM) // FRAME
          ELSEIF ctrl:ncStyle == 3  // REDONDEADO   
             oPen := HPen():Add( BS_TRANSPARENT, 1, hwg_Getsyscolor( COLOR_3DSHADOW ) )
             hwg_Selectobject( hDC, oPen:handle )
             hwg_RoundRect( hDC, x1, y1, x2, y2, 5  )

             oPen := HPen():Add( BS_TRANSPARENT, 1, hwg_Getsyscolor( COLOR_3DFACE ) )
             hwg_Selectobject( hDC, oPen:handle )
             hwg_RoundRect( hDC, x1+1, y1+1, x2-1, y2-1, 4  )
             hDC := drawInfo[ 3 ]
          ELSE                   // FLAT
             hwg_Drawedge( hDC, x1, y1, x2, y2,BDR_SUNKENINNER,BF_TOP)
             hwg_Drawedge( hDC, x1, y1, x2, y2,BDR_RAISEDOUTER,BF_BOTTOM)
             hwg_Drawedge( hDC, x1, y2, x2, y1,BDR_SUNKENINNER,BF_LEFT)
             hwg_Drawedge( hDC, x1, y2, x2, y1,BDR_RAISEDOUTER,BF_RIGHT)
          ENDIF
       ELSE
          hwg_Drawedge( hDC, x1, y1, x2, y2,0,0)
       ENDIF
       IF ctrl:backStyle != TRANSPARENT
          IF ctrl:Brush != NIL
             hwg_Fillrect( hDC, x1 + 2, y1 + 2, x2 - 2, y2 - 2 , ctrl:brush:handle )
          ENDIF
       ELSE
          hwg_Fillrect( hDC, x1 + 2, y1 + 2, x2 - 2, y2 - 2 , hwg_Getstockobject( 5 ) )
       ENDIF
       //hwg_Setbkmode( hDC, 0 )
    ENDIF
 
RETURN 1
//-----------------------------------------------------------
STATIC FUNCTION PanelPaint(ctrl,ptr)
    LOCAL pps, hDC, hDCl, aCoors, block, oPenLight, oPenGray

    pps    := hwg_Definepaintstru()
    hDC    := hwg_Beginpaint( ctrl:handle, pps )
    hDCl    := hwg_Beginpaint( ctrl:handle, pps )
    aCoors := hwg_Getclientrect( ctrl:handle )
 /*
    IF !Empty( block := hwg_getPaintCB( ctrl:aPaintCB, PAINT_BACK ) )
       Eval( block, Self, hDC, aCoors[1], aCoors[2], aCoors[3], aCoors[4] )
    ELSEIF ctrl:oStyle == Nil
       oPenLight := HPen():Add( BS_SOLID, 1, hwg_Getsyscolor( COLOR_3DHILIGHT ) )
       hwg_Selectobject( hDC, oPenLight:handle )
       //hwg_Drawline( hDC, 5, 1, aCoors[3] - 5, 1 )
       oPenGray := HPen():Add( BS_SOLID, 1, hwg_Getsyscolor( COLOR_3DSHADOW ) )
       hwg_Selectobject( hDC, oPenGray:handle )
       //hwg_Drawline( hDC, 5, 0, aCoors[3] - 5, 0 )
    ELSE
       ctrl:oStyle:Draw( hDC, 0, 0, aCoors[3], aCoors[4] )
    ENDIF
*/
    oPenGray := HPen():Add( BS_TRANSPARENT, 1, hwg_Getsyscolor( COLOR_3DSHADOW ) )
    hwg_Selectobject( hDCl, oPenGray:handle )
    hwg_RoundRect( hDCl, 0, 0, aCoors[3] , aCoors[4], 5  )
    oPenGray := HPen():Add( BS_TRANSPARENT, 1, hwg_Getsyscolor( COLOR_3DFACE ) )
    hwg_Selectobject( hDCl, oPenGray:handle )
    hwg_RoundRect( hDCl, 1, 1, aCoors[3] , aCoors[4], 4  )
    hwg_Fillrect( hDC, 2, 2, aCoors[4]  - 2, aCoors[4]  - 2 , hwg_Getstockobject( 5 ) )

    ctrl:DrawItems( hDC, aCoors )

    oPenGray:Release()
 /*
    IF !Empty( oPenGray )
       oPenGray:Release()
       oPenLight:Release()
    ENDIF
*/
    hwg_Endpaint( ctrl:handle, pps )
    
RETURN NIL 
//-----------------------------------------------------------


// *****************************************************************


#pragma begindump
#include "hbapi.h"
#include "hwingui.h"
HB_FUNC( HWG_SETPARENT ){
  HB_RETHANDLE( SetParent(( HWND ) HB_PARHANDLE( 1 ), ( HWND ) HB_PARHANDLE( 2 )) ); 
}
//----------------------------------------------------------------------------------------------
HB_FUNC( XAW_LOWORD )
{
   hb_retni( ( int ) (short) LOWORD( ( DWORD ) hb_parnl( 1 ) ) );
}
//----------------------------------------------------------------------------------------------
HB_FUNC( XAW_HIWORD )
{
   hb_retni( ( int ) (short) HIWORD( ( DWORD ) hb_parnl( 1 ) ) );
}
//----------------------------------------------------------------------------------------------
/*
HB_FUNC( HWG_PTRTOLONG){
   hb_retnl(PtrToLong(( HWND ) HB_PARHANDLE( 1 )));
}
*/
//PtrToLong
#pragma enddump
