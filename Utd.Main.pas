unit Utd.Main;

interface

uses
  (* Delphi *)
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  System.Actions, System.Generics.Collections, System.Contnrs, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ActnList, Vcl.StdActns, Vcl.Menus,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.ImgList,

  (* SynEdit *)
  SynHighlighterDfm, SynEditHighlighter, SynHighlighterDWS, SynEdit,
  SynHighlighterPas,

  (* DelphiAst *)
  DelphiAst, DelphiAST.Classes, DelphiAST.Consts,

  dwsComp, SynEditMiscClasses, SynEditSearch;

type
  TFormUnitToDfm = class(TForm)
    ActionConvert: TAction;
    ActionFileExit: TFileExit;
    ActionFileOpen: TFileOpen;
    ActionFileSaveAs: TFileSaveAs;
    ActionList: TActionList;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MenuItemConvert: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileExit: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileSaveAs: TMenuItem;
    N1: TMenuItem;
    PageControl: TPageControl;
    StatusBar: TStatusBar;
    SynDfmSyn: TSynDfmSyn;
    SynEditDfm: TSynEdit;
    SynEditUnit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    TabSheetDfm: TTabSheet;
    TabSheetUnit: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButtonConvert: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSaveAs: TToolButton;
    SynEditSearch: TSynEditSearch;
    ActionSearchFind: TSearchFind;
    MenuItemSearch: TMenuItem;
    Find1: TMenuItem;
    procedure ActionConvertExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
  private
    FIniFileName: TFileName;
    FPascalFileName: TFileName;
    FDataModule: TDataModule;
    FdwsUnit: TdwsUnit;
    procedure VisualizeNode(Node: TSyntaxNode; Level: Integer);
    procedure AddConstant(const Node: TSyntaxNode);
    procedure AddEnum(const Node: TSyntaxNode);
    procedure AddClass(const Node: TSyntaxNode);
    procedure AddSynonym(const Node: TSyntaxNode);
    procedure AddInterface(const Node: TSyntaxNode);
    procedure SetPascalFileName(const Value: TFileName);
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

{ TFormUnitToDfm }

procedure TFormUnitToDfm.FormCreate(Sender: TObject);
begin
  FDataModule := TDataModule.Create(nil);
  FdwsUnit := TdwsUnit.Create(FDataModule);
  FdwsUnit.Name := 'dwsUnit';
  FdwsUnit.StaticSymbols := False;

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

procedure TFormUnitToDfm.SetPascalFileName(const Value: TFileName);
begin
  if FPascalFileName <> Value then
  begin
    FPascalFileName := Value;
    ActionFileSaveAs.Dialog.FileName := FPascalFileName + '.dfm';
  end;
end;

procedure TFormUnitToDfm.ActionFileOpenAccept(Sender: TObject);
begin
  LoadFromFile(ActionFileOpen.Dialog.FileName);
  FPascalFileName := ActionFileOpen.Dialog.FileName;
end;

procedure TFormUnitToDfm.ActionFileSaveAsAccept(Sender: TObject);
begin
  SaveTextToUTF8File(ActionFileSaveAs.Dialog.FileName, SynEditDfm.Text);
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
          DataType := 'Integer';
          Exit(True);
        end;
        if TypeString = 'string' then
        begin
          Value := ValuedNode.Value;
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

  Assert(Node.ChildNodes[1].Typ = ntValue);
  Assert(Length(Node.ChildNodes[1].ChildNodes) >= 1);
  Assert(Node.ChildNodes[1].ChildNodes[0].Typ = ntExpression);
  Assert(Length(Node.ChildNodes[1].ChildNodes) >= 1);

  ExpressionNode := Node.ChildNodes[1].ChildNodes[0].ChildNodes[0];
  if GetConstantValue(ExpressionNode, CurrentValue, CurrentDataType) then
  begin
    Constant := FdwsUnit.Constants.Add;
    Constant.Name := Name;
    Constant.Value := CurrentValue;
    Constant.DataType := CurrentDataType;
  end;
end;

procedure TFormUnitToDfm.AddEnum(const Node: TSyntaxNode);
var
  Name: string;
  TypeNode: TSyntaxNode;
  Index: Integer;
  Enum: TdwsEnumeration;
  Element: TdwsElement;
  TypeString: string;
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
      Assert(TypeNode.ChildNodes[Index].ChildNodes[0] is TValuedSyntaxNode);
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
    end;
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
                        Param.DataType := ParamChildNode.ChildNodes[1].GetAttribute(anName);
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
                Prop.DataType := ChildNode.ChildNodes[0].GetAttribute(anName)
          end;
      end;
  end;

var
  Name: string;
  TypeNode: TSyntaxNode;
  ChildNode: TSyntaxNode;
  Element: TdwsElement;
  TypeString: string;
  ValuedNode: TValuedSyntaxNode;
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
                        Param.DataType := ParamChildNode.ChildNodes[1].GetAttribute(anName);
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
  ChildNode: TSyntaxNode;
  Element: TdwsElement;
  TypeString: string;
  ValuedNode: TValuedSyntaxNode;
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
    DataType := TypeNode.GetAttribute(anName);
    if (DataType = 'Byte') or (DataType = 'Word') or (DataType = 'Integer') or (DataType = 'UInt64') then
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

procedure TFormUnitToDfm.VisualizeNode(Node: TSyntaxNode; Level: Integer);
var
  NodeInfo: string;
  TypeName: string;
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
  begin

    for Index := 0 to Length(Node.ChildNodes) - 1 do
      VisualizeNode(Node.ChildNodes[Index], Level + 1);
  end;
end;

procedure TFormUnitToDfm.ActionConvertExecute(Sender: TObject);
var
  Node: TSyntaxNode;
  StringStream: TStringStream;
  MemoryStream: TMemoryStream;
begin
  FdwsUnit.Constants.Clear;
  FdwsUnit.Classes.Clear;
  FdwsUnit.Enumerations.Clear;
  FdwsUnit.Synonyms.Clear;

  SynEditDfm.Clear;
  SynEditDfm.BeginUpdate;
  try
    Node := TPasSyntaxTreeBuilder.Run(FPascalFileName, True);
    VisualizeNode(Node, 0);
    PageControl.ActivePage := TabSheetDfm;

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

end.
