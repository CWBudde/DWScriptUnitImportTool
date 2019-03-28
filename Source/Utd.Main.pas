unit Utd.Main;

interface

uses
  (* Delphi *)
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Actions, System.Generics.Collections, System.Contnrs, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList,
  Vcl.StdActns, Vcl.Menus, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ImgList,

  (* SynEdit *)
  SynHighlighterDfm, SynEditHighlighter, SynHighlighterDWS, SynEdit,
  SynHighlighterPas,

  (* DelphiAst *)
  DelphiAst, DelphiAST.Classes, DelphiAST.Consts,

  dwsComp, SynEditMiscClasses, SynEditSearch, SynEditCodeFolding;

type
  TCustomDataModule = class(TDataModule);

  TFormUnitToDfm = class(TForm)
    ActionConvert: TAction;
    ActionFileExit: TFileExit;
    ActionFileOpen: TFileOpen;
    ActionFileSaveAs: TFileSaveAs;
    ActionList: TActionList;
    ActionSearchFind: TSearchFind;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItemConvert: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemSearch: TMenuItem;
    N1: TMenuItem;
    PageControl: TPageControl;
    StatusBar: TStatusBar;
    SynDfmSyn: TSynDfmSyn;
    SynEditDfm: TSynEdit;
    SynEditPas: TSynEdit;
    SynEditSearch: TSynEditSearch;
    SynEditUnit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    TabSheetDfm: TTabSheet;
    TabSheetPascal: TTabSheet;
    TabSheetUnit: TTabSheet;
    ToolBar: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButtonConvert: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSaveAs: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionConvertExecute(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure SynEditUnitStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FIniFileName: TFileName;
    FPascalFileName: TFileName;
    FDataModule: TCustomDataModule;
    FdwsUnit: TdwsUnit;
    FCurrentSynEdit: TSynEdit;
    procedure VisitNode(Node: TSyntaxNode; Level: Integer);
    procedure AddConstant(const Node: TSyntaxNode);
    procedure AddEnum(const Node: TSyntaxNode);
    procedure AddClass(const Node: TSyntaxNode);
    procedure AddSynonym(const Node: TSyntaxNode);
    procedure AddInterface(const Node: TSyntaxNode);
    procedure AddFunction(const Node: TSyntaxNode);
    procedure SetPascalFileName(const Value: TFileName);
    procedure GenerateDfm;
    procedure GeneratePas;
  public
    procedure LoadFromFile(FileName: TFileName);

    property PascalFileName: TFileName read FPascalFileName write SetPascalFileName;
  end;

var
  FormUnitToDfm: TFormUnitToDfm;

implementation

uses
  Inifiles, dwsSymbols, dwsXPlatform;

{$R *.dfm}

function NormalizeDataType(DataType: String): String;
begin
  if ASCIISameText(DataType, 'bool') or ASCIISameText(DataType, 'boolean') then
    Result := 'Boolean'
  else
  if ASCIISameText(DataType, 'Byte') or ASCIISameText(DataType, 'Word') or ASCIISameText(DataType, 'DWord') or ASCIISameText(DataType, 'Int64') then
    Result := 'Integer'
  else
  if ASCIISameText(DataType, 'PWideChar') or ASCIISameText(DataType, 'PAnsiChar') or ASCIISameText(DataType, 'String') then
    Result := 'String'
  else
    Result := DataType;
end;

{ TFormUnitToDfm }

procedure TFormUnitToDfm.FormCreate(Sender: TObject);
begin
  FDataModule := TCustomDataModule.CreateNew(nil);
  FDataModule.Name := 'CustomDataModule';
  FDataModule.DesignSize := Point(150, 215);

  FdwsUnit := TdwsUnit.Create(FDataModule);
  FdwsUnit.Name := 'dwsUnit1';
  FdwsUnit.StaticSymbols := False;
  FCurrentSynEdit := SynEditUnit;

  FIniFileName := ChangeFileExt(ParamStr(0), '.ini');

  with TIniFile.Create(FIniFileName) do
  try
    FPascalFileName := ReadString('Recent', 'PascalFile', '');
  finally
    Free;
  end;

  if FileExists(FPascalFileName) then
  begin
    LoadFromFile(FPascalFileName);

    ActionConvert.Execute;
  end;
end;

procedure TFormUnitToDfm.FormDestroy(Sender: TObject);
begin
  with TIniFile.Create(FIniFileName) do
  try
    if FPascalFileName <> '' then
      WriteString('Recent', 'PascalFile', FPascalFileName);
  finally
    Free;
  end;

  FdwsUnit.Free;
  FDataModule.Free;
end;

procedure TFormUnitToDfm.LoadFromFile(FileName: TFileName);
begin
  SynEditUnit.Text := LoadTextFromFile(FileName);
  FdwsUnit.UnitName := ChangeFileExt(ExtractFileName(FPascalFileName), '');
end;

procedure TFormUnitToDfm.PageControlChange(Sender: TObject);
begin
  case PageControl.TabIndex of
    0:
      FCurrentSynEdit := SynEditUnit;
    1:
      FCurrentSynEdit := SynEditDfm;
    2:
      FCurrentSynEdit := SynEditPas;
  end;
end;

procedure TFormUnitToDfm.SetPascalFileName(const Value: TFileName);
begin
  if FPascalFileName <> Value then
  begin
    FPascalFileName := Value;
    ActionFileSaveAs.Dialog.FileName := FPascalFileName + '.dfm';
  end;
end;

procedure TFormUnitToDfm.SynEditUnitStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if [scCaretX, scCaretY] * Changes <> [] then
    StatusBar.Panels[0].Text :=
      'X: ' + IntToStr(FCurrentSynEdit.CaretX) + ' ' +
      'Y: ' + IntToStr(FCurrentSynEdit.CaretY);
end;

procedure TFormUnitToDfm.ActionFileOpenAccept(Sender: TObject);
begin
  LoadFromFile(ActionFileOpen.Dialog.FileName);
  FPascalFileName := ActionFileOpen.Dialog.FileName;
end;

procedure TFormUnitToDfm.ActionFileSaveAsAccept(Sender: TObject);
begin
  SaveTextToUTF8File(ActionFileSaveAs.Dialog.FileName, SynEditDfm.Text);
  if SynEditPas.Text <> '' then
    SaveTextToUTF8File(ChangeFileExt(ActionFileSaveAs.Dialog.FileName, '.pas'), SynEditPas.Text);
end;

procedure TFormUnitToDfm.AddConstant(const Node: TSyntaxNode);

  function GetConstantValue(Node: TSyntaxNode; var Value: Variant; var DataType: string): Boolean;
  var
    RefIndex: Integer;
    ValuedNode: TValuedSyntaxNode;
    TypeString: string;
    ReferenceName: string;
    CurrentValue: Variant;
    CurrentDataType: string;
    Index: Integer;
    Symbol: TdwsSymbol;
  begin
    Result := False;
    if Node.Typ = ntLiteral then
    begin
      Assert(Node is TValuedSyntaxNode);
      ValuedNode := TValuedSyntaxNode(Node);
      if Node.HasAttribute(anType) then
      begin
        TypeString := Node.GetAttribute(anType);
        if TypeString = 'numeric' then
        begin
          Value := Int64(StrToInt(ValuedNode.Value));
          if (DataType = '') then
            DataType := 'Integer';
          Exit(True);
        end;
        if TypeString = 'string' then
        begin
          Value := ValuedNode.Value;
          if (DataType = '') then
            DataType := 'String';
          Exit(True);
        end;
      end;
    end else
    if Node.Typ = ntIdentifier then
    begin
      Assert(Node.HasAttribute(anName));
      ReferenceName := Node.GetAttribute(anName);
      RefIndex := FdwsUnit.Constants.IndexOf(ReferenceName);
      if RefIndex >= 0 then
      begin
        Symbol := FdwsUnit.Constants.Symbols[ReferenceName];
        Value := TdwsConstant(Symbol).Value;
        DataType := TdwsConstant(Symbol).DataType;
        Exit(True);
      end;
    end
    else
    if Node.Typ = ntAdd then
    begin
      if GetConstantValue(Node.ChildNodes[0], Value, DataType) then
        for Index := 1 to Length(Node.ChildNodes) - 1 do
          if GetConstantValue(Node.ChildNodes[Index], CurrentValue, CurrentDataType) then
          begin
            Assert(CurrentDataType = DataType);
            Value := Value + CurrentValue;
          end;
    end
(*
    else
      raise Exception.CreateFmt('Not implemented yet (Name: %s; Type: $s)',
        [Name, SyntaxNodeNames[ExpressionNode.Typ]]);
*)
  end;

var
  ValuedNode: TValuedSyntaxNode;
  Name: string;
  Index: Integer;
  CurrentValue: Variant;
  CurrentDataType: string;
  Constant: TdwsConstant;
  ExpressionNode: TSyntaxNode;
begin
  Assert(Node.Typ = ntConstant);
  Assert(Node.ChildNodes[0].Typ = ntName);
  Assert(Node.ChildNodes[0] is TValuedSyntaxNode);
  ValuedNode := TValuedSyntaxNode(Node.ChildNodes[0]);
  Name := ValuedNode.Value;

  Index := 1;

  // eventually read type information
  if Node.ChildNodes[Index].Typ = ntType then
  begin
    if Node.ChildNodes[Index].HasAttribute(anName) then
      CurrentDataType := Node.ChildNodes[Index].GetAttribute(anName);
    Inc(Index);
  end;

  Assert(Node.ChildNodes[Index].Typ = ntValue);
  Assert(Length(Node.ChildNodes[Index].ChildNodes) >= 1);

  if Node.ChildNodes[Index].ChildNodes[0].Typ = ntField then
  begin
    // todo, as in Winapi.Windows.pas, line 16126
    // const WIN_TRUST_SUBJTYPE_RAW_FILEEX: TGUID = (
    //   D1:$6f458110; D2:$c2f1; D3:$11cf; D4:($8a, $69, $0, $aa, 0, $6c, $37, 6));

    Exit;
  end;

  Assert(Node.ChildNodes[Index].ChildNodes[0].Typ = ntExpression);
  Assert(Length(Node.ChildNodes[Index].ChildNodes) >= 1);

  ExpressionNode := Node.ChildNodes[Index].ChildNodes[0].ChildNodes[0];
  if GetConstantValue(ExpressionNode, CurrentValue, CurrentDataType) then
  begin
    Constant := FdwsUnit.Constants.Add;
    Constant.Name := Name;
    Constant.Value := CurrentValue;
    Constant.DataType := NormalizeDataType(CurrentDataType);
  end;
end;

procedure TFormUnitToDfm.AddEnum(const Node: TSyntaxNode);
var
  Name: string;
  TypeNode: TSyntaxNode;
  Index: Integer;
  Enum: TdwsEnumeration;
  Element: TdwsElement;
  TypeString, NameString: string;
  ValuedNode: TValuedSyntaxNode;
begin
  if Node.HasAttribute(anName) then
    Name := Node.GetAttribute(anName);
  TypeNode := Node.ChildNodes[0];
  Assert(TypeNode.Typ = ntType);

  Enum := FdwsUnit.Enumerations.Add;
  Enum.Name := Name;

  Index := 0;
  while Index < Length(TypeNode.ChildNodes) do
  begin
    Assert(TypeNode.ChildNodes[Index].Typ = ntIdentifier);
    Assert(TypeNode.ChildNodes[Index].HasAttribute(anName));
    Name := TypeNode.ChildNodes[Index].GetAttribute(anName);
    Element := Enum.Elements.Add;
    Element.Name := Name;
    Inc(Index);
    if (Index < Length(TypeNode.ChildNodes)) and (TypeNode.ChildNodes[Index].Typ = ntExpression) then
    begin
      if TypeNode.ChildNodes[Index].ChildNodes[0] is TValuedSyntaxNode then
      begin
        ValuedNode := TValuedSyntaxNode(TypeNode.ChildNodes[Index].ChildNodes[0]);
        if TypeNode.ChildNodes[Index].ChildNodes[0].HasAttribute(anType) then
        begin
          TypeString := TypeNode.ChildNodes[Index].ChildNodes[0].GetAttribute(anType);
          if TypeString = 'numeric' then
          begin
            Element.UserDefValue := Int64(StrToInt(ValuedNode.Value));
            Element.IsUserDef := True;
          end;
        end;
        Inc(Index);
      end
      else
      begin
        if TypeNode.ChildNodes[Index].ChildNodes[0].HasAttribute(anName) then
        begin
          NameString := TypeNode.ChildNodes[Index].ChildNodes[0].GetAttribute(anName);
//          Element.UserDefValue := NameString; // TODO
          Element.IsUserDef := True;
        end;
        Inc(Index);
      end;
    end;
  end;
end;

procedure TFormUnitToDfm.AddFunction(const Node: TSyntaxNode);
var
  Func: TdwsFunction;
  Param: TdwsParameter;
  ChildNode, ParamChildNode: TSyntaxNode;
  Name, ParamName, KindName: String;
  Index, ParamsIndex, ParamIndex: Integer;
begin
  Assert(Node.Typ = ntMethod);

  if Node.HasAttribute(anName) then
    Name := Node.GetAttribute(anName);

  Func := FdwsUnit.Functions.Add;
  Func.Name := Name;

  for Index := 0 to Length(Node.ChildNodes) - 1 do
  begin
    if Node.ChildNodes[Index].Typ = ntParameters then
    begin
      for ParamsIndex := 0 to Length(Node.ChildNodes[Index].ChildNodes) - 1 do
      begin
        ChildNode := Node.ChildNodes[Index].ChildNodes[ParamsIndex];
        if not (ChildNode.Typ = ntParameter) then
          Continue;

        Param := Func.Parameters.Add;

        if ChildNode.HasAttribute(anKind) then
        begin
          KindName := ChildNode.GetAttribute(anKind);
          if KindName = 'const' then
          begin
            Param.IsVarParam := True;
            Param.IsVarParam := False;
          end
          else
          if KindName = 'var' then
          begin
            Param.IsVarParam := True;
            Param.IsWritable := True;
          end;
        end;

        if ChildNode.ChildNodes[0] is TValuedSyntaxNode then
          Param.Name := TValuedSyntaxNode(ChildNode.ChildNodes[0]).Value;

        if ChildNode.ChildNodes[1].HasAttribute(anName) then
          Param.DataType := NormalizeDataType(ChildNode.ChildNodes[1].GetAttribute(anName));
      end;
    end
    else
    if Node.ChildNodes[Index].Typ = ntReturnType then
    begin
      ChildNode := Node.ChildNodes[Index];
      if ChildNode.ChildNodes[0].HasAttribute(anName) then
        Func.ResultType := ChildNode.ChildNodes[0].GetAttribute(anName);
    end
    else
      raise Exception.Create('Unexpected type: ' + SyntaxNodeNames[Node.ChildNodes[Index].Typ]);
  end;
end;

procedure TFormUnitToDfm.AddClass(const Node: TSyntaxNode);
var
  Cls: TdwsClass;

  procedure ScanClassMembers(const Node: TSyntaxNode; Visibility: TdwsVisibility);
  var
    ChildNode: TSyntaxNode;
    Method: TdwsMethod;
    Prop: TdwsProperty;
    MethodChildNode: TSyntaxNode;
    ParamChildNode: TSyntaxNode;
    Param: TdwsParameter;
    ValueNode: TValuedSyntaxNode;
    MethodBinding: string;
  begin
    for ChildNode in Node.ChildNodes do
      case ChildNode.Typ of
        ntMethod:
          begin
            Method := Cls.Methods.Add;
            Method.Visibility := Visibility;
            if ChildNode.HasAttribute(anName) then
              Method.Name := ChildNode.GetAttribute(anName);
            if ChildNode.HasAttribute(anMethodBinding) then
            begin
              MethodBinding := ChildNode.GetAttribute(anMethodBinding);
              if MethodBinding = 'virtual' then
                Method.Attributes := Method.Attributes + [maVirtual]
              else
              if MethodBinding = 'override' then
                Method.Attributes := Method.Attributes + [maOverride];
            end;

            for MethodChildNode in ChildNode.ChildNodes do
            begin
              case MethodChildNode.Typ of
                ntReturnType:
                  begin
                    Assert(MethodChildNode.ChildNodes[0].Typ = ntType);
                    if MethodChildNode.ChildNodes[0].HasAttribute(anName) then
                      Method.ResultType := MethodChildNode.ChildNodes[0].GetAttribute(anName);
                  end;
                ntParameters:
                  for ParamChildNode in MethodChildNode.ChildNodes do
                  begin
                    Assert(ParamChildNode.Typ = ntParameter);
                    if ParamChildNode.ChildNodes[0] is TValuedSyntaxNode then
                    begin
                      Param := Method.Parameters.Add;
                      ValueNode := TValuedSyntaxNode(ParamChildNode.ChildNodes[0]);
                      Param.Name := ValueNode.Value;
                      if ParamChildNode.ChildNodes[1].HasAttribute(anName) then
                        Param.DataType := NormalizeDataType(ParamChildNode.ChildNodes[1].GetAttribute(anName));
                    end;
                  end;
              end;
            end;
          end;
        ntProperty:
          begin
            Prop := Cls.Properties.Add;
            Prop.Visibility := Visibility;
            if ChildNode.HasAttribute(anName) then
              Prop.Name := ChildNode.GetAttribute(anName);
            if Length(ChildNode.ChildNodes) > 0 then
              if ChildNode.ChildNodes[0].HasAttribute(anName) then
                Prop.DataType := NormalizeDataType(ChildNode.ChildNodes[0].GetAttribute(anName))
          end;
      end;
  end;

var
  Name: string;
  TypeNode: TSyntaxNode;
  ChildNode: TSyntaxNode;
begin
  if Node.HasAttribute(anName) then
    Name := Node.GetAttribute(anName);
  TypeNode := Node.ChildNodes[0];
//  Assert(TypeNode.Typ = ntClass);

  Cls := FdwsUnit.Classes.Add;
  Cls.Name := Name;

  for ChildNode in TypeNode.ChildNodes do
  begin
    case ChildNode.Typ of
      ntProtected:
        ScanClassMembers(ChildNode, cvProtected);
      ntPublic:
        ScanClassMembers(ChildNode, cvPublic);
      ntPublished:
        ScanClassMembers(ChildNode, cvPublished);
    end;
  end;
end;

procedure TFormUnitToDfm.AddInterface(const Node: TSyntaxNode);
var
  Cls: TdwsClass;

  procedure ScanClassMembers(const Node: TSyntaxNode; Visibility: TdwsVisibility);
  var
    ChildNode: TSyntaxNode;
    Method: TdwsMethod;
    Prop: TdwsProperty;
    MethodChildNode: TSyntaxNode;
    ParamChildNode: TSyntaxNode;
    Param: TdwsParameter;
    ValueNode: TValuedSyntaxNode;
    MethodBinding: string;
  begin
    for ChildNode in Node.ChildNodes do
      case ChildNode.Typ of
        ntMethod:
          begin
            Method := Cls.Methods.Add;
            Method.Visibility := Visibility;
            if ChildNode.HasAttribute(anName) then
              Method.Name := ChildNode.GetAttribute(anName);
            if ChildNode.HasAttribute(anMethodBinding) then
            begin
              MethodBinding := ChildNode.GetAttribute(anMethodBinding);
              if MethodBinding = 'virtual' then
                Method.Attributes := Method.Attributes + [maVirtual]
              else
              if MethodBinding = 'override' then
                Method.Attributes := Method.Attributes + [maOverride];
            end;

            for MethodChildNode in ChildNode.ChildNodes do
            begin
              case MethodChildNode.Typ of
                ntReturnType:
                  begin
                    Assert(MethodChildNode.ChildNodes[0].Typ = ntType);
                    if MethodChildNode.ChildNodes[0].HasAttribute(anName) then
                      Method.ResultType := MethodChildNode.ChildNodes[0].GetAttribute(anName);
                  end;
                ntParameters:
                  for ParamChildNode in MethodChildNode.ChildNodes do
                  begin
                    Assert(ParamChildNode.Typ = ntParameter);
                    if ParamChildNode.ChildNodes[0] is TValuedSyntaxNode then
                    begin
                      Param := Method.Parameters.Add;
                      ValueNode := TValuedSyntaxNode(ParamChildNode.ChildNodes[0]);
                      Param.Name := ValueNode.Value;
                      if ParamChildNode.ChildNodes[1].HasAttribute(anName) then
                        Param.DataType := NormalizeDataType(ParamChildNode.ChildNodes[1].GetAttribute(anName));
                    end;
                  end;
              end;
            end;
          end;
        ntProperty:
          begin
            Prop := Cls.Properties.Add;
            Prop.Visibility := Visibility;
            if ChildNode.HasAttribute(anName) then
              Prop.Name := ChildNode.GetAttribute(anName);
          end;
      end;
  end;

var
  Name: string;
  TypeNode: TSyntaxNode;
begin
  if Node.HasAttribute(anName) then
    Name := Node.GetAttribute(anName);
  TypeNode := Node.ChildNodes[0];

  Cls := FdwsUnit.Classes.Add;
  Cls.Name := Name;

  ScanClassMembers(TypeNode, cvPublic);
end;

procedure TFormUnitToDfm.AddSynonym(const Node: TSyntaxNode);
var
  Name: string;
  TypeNode: TSyntaxNode;
  Synonym: TdwsSynonym;
  DataType: string;
begin
  if Node.HasAttribute(anName) then
    Name := Node.GetAttribute(anName);
  TypeNode := Node.ChildNodes[0];

  if TypeNode.HasAttribute(anName) then
  begin
    DataType := NormalizeDataType(TypeNode.GetAttribute(anName));
    if DataType = 'Integer' then
    begin
      Synonym := FdwsUnit.Synonyms.Add;
      Synonym.Name := Name;
      Synonym.DataType := 'Integer';
    end
    else
    if (DataType = 'AnsiChar') or (DataType = 'String') then
    begin
      Synonym := FdwsUnit.Synonyms.Add;
      Synonym.Name := Name;
      Synonym.DataType := 'String';
    end;
  end;
end;

procedure TFormUnitToDfm.VisitNode(Node: TSyntaxNode; Level: Integer);
var
  NodeInfo: string;
  TypeName, KindName: string;
  Index: Integer;
  TypeNode: TSyntaxNode;
begin
  // build node information
  NodeInfo := '(' + IntToStr(Node.Line) + ', ' + IntToStr(Node.Col) + ')';
  if Node.HasAttribute(anName) then
    NodeInfo := Node.GetAttribute(anName) + ' ' + NodeInfo;
  if Node.HasAttribute(anType) then
    NodeInfo := Node.GetAttribute(anType) + ' ' + NodeInfo;

  NodeInfo := SyntaxNodeNames[Node.Typ] + ': ' + NodeInfo;

  // indention
  NodeInfo := StringOfChar(' ', 2 * Level) + NodeInfo;

  if Node.Typ = ntConstants then
  begin
    for Index := 0 to Length(Node.ChildNodes) - 1 do
      if Node.ChildNodes[Index].Typ = ntConstant then
        AddConstant(Node.ChildNodes[Index])
  end else
  if Node.Typ = ntTypedecl then
  begin
    if Length(Node.ChildNodes) > 0 then
    begin
      TypeNode := Node.ChildNodes[0];
      Assert(TypeNode.Typ = ntType);
      if TypeNode.HasAttribute(anName) then
      begin
        TypeName := TypeNode.GetAttribute(anName);
        if TypeName = 'enum' then
          AddEnum(Node)
        else
          AddSynonym(Node);
      end else
      if TypeNode.HasAttribute(anType) then
      begin
        TypeName := TypeNode.GetAttribute(anType);
        if TypeName = 'class' then
          AddClass(Node)
        else
        if TypeName = 'interface' then
          AddInterface(Node)
      end;
    end;
  end
  else
  if Node.Typ = ntMethod then
  begin
    if Node.HasAttribute(anKind) then
    begin
      KindName := Node.GetAttribute(anKind);
      if KindName = 'function' then
        AddFunction(Node);
    end;
  end;
  begin
    for Index := 0 to Length(Node.ChildNodes) - 1 do
      VisitNode(Node.ChildNodes[Index], Level + 1);
  end;
end;

procedure TFormUnitToDfm.GenerateDfm;
var
  StringStream: TStringStream;
  MemoryStream: TMemoryStream;
begin
  SynEditDfm.Clear;
  SynEditDfm.BeginUpdate;
  try
    MemoryStream := TMemoryStream.Create;
    try
      MemoryStream.WriteComponent(FDataModule);
      MemoryStream.Seek(0, soFromBeginning);

      StringStream := TStringStream.Create;
      try
        ObjectBinaryToText(MemoryStream, StringStream);
        SynEditDfm.Text := StringStream.DataString;
      finally
        StringStream.Free;
      end;
    finally
      MemoryStream.Free
    end;
  finally
    SynEditDfm.EndUpdate;
  end;
end;

procedure TFormUnitToDfm.GeneratePas;
var
  StringStream: TStringStream;
  MemoryStream: TMemoryStream;
begin
  SynEditPas.Clear;
  SynEditPas.BeginUpdate;
  try
    SynEditPas.Lines.Add('unit Convert;');
    SynEditPas.Lines.Add('');
    SynEditPas.Lines.Add('interface');
    SynEditPas.Lines.Add('');
    SynEditPas.Lines.Add('uses');
    SynEditPas.Lines.Add('  System.SysUtils, System.Classes, dwsComp;');
    SynEditPas.Lines.Add('');
    SynEditPas.Lines.Add('type');
    SynEditPas.Lines.Add('  TCustomDataModule = class(TDataModule)');
    SynEditPas.Lines.Add('    dwsUnit1: TdwsUnit;');
    SynEditPas.Lines.Add('  end;');
    SynEditPas.Lines.Add('');
    SynEditPas.Lines.Add('implementation');
    SynEditPas.Lines.Add('');
    SynEditPas.Lines.Add('{$R *.dfm}');
    SynEditPas.Lines.Add('');
    SynEditPas.Lines.Add('end.');
  finally
    SynEditPas.EndUpdate;
  end;
end;

procedure TFormUnitToDfm.ActionConvertExecute(Sender: TObject);
var
  Node: TSyntaxNode;
begin
  FdwsUnit.Constants.Clear;
  FdwsUnit.Classes.Clear;
  FdwsUnit.Enumerations.Clear;
  FdwsUnit.Synonyms.Clear;

  Node := TPasSyntaxTreeBuilder.Run(FPascalFileName, True);
  VisitNode(Node, 0);

  GenerateDfm;
  GeneratePas;

  PageControl.ActivePage := TabSheetDfm;
end;

end.
