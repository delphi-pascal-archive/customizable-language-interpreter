unit UValues;

interface

uses types,SysUtils;

type
 TTypeValue=(TypeNone,TypeNumber,TypeString,TypeBoolean,TypeArray);
type
 TValue=class
 private
  Ftypevalue:TTypeValue;
  FPosition:tpoint;
  FNumber:extended;
  FBoolean:boolean;
  FString:string;
  FCount:integer;
  FArray:array of TValue;
  FFormatSettings:TFormatSettings;
  function GetNumberValue:extended;
  procedure SetNumberValue(v:extended);
  function GetStringValue:string;
  procedure SetStringValue(v:string);
  function GetBooleanValue:boolean;
  procedure SetBooleanValue(v:boolean);
  function GetArrayValue(index:integer):tvalue;
  procedure SetArrayValue(index:integer;value:tvalue);
 public
  constructor create;overload;
  constructor create(position:tpoint);overload;
  constructor create(position:tpoint;Value:extended); overload;
  constructor create(position:tpoint;Value:string); overload;
  constructor create(position:tpoint;Value:boolean); overload;
  constructor create(position:tpoint;Value:TValue); overload;
  constructor create(Value:extended); overload;
  constructor create(Value:string); overload;
  constructor create(Value:boolean); overload;
  constructor create(Value:TValue); overload;

  destructor Destroy; override;

  procedure Add(value:TValue);
  procedure Clear;
  procedure ClearArray;
  function ToString:string;
  function ToNumber:extended;
  function ToBoolean:Boolean;
  procedure ToArray;
  property ArrayLenght:integer read FCount;
  property TypeValue:TTypeValue read Ftypevalue;
  property position:tpoint read FPosition;
  property NumberValue:extended read GetNumberValue write SetNumberValue;
  property StringValue:string read GetStringValue write SetStringValue;
  property BooleanValue:boolean read GetBooleanValue write SetBooleanValue;
  property ArrayValue[index:integer]:tvalue read GetArrayValue write SetArrayValue;
  function IsString:Boolean;
  function IsNumber:Boolean;
  function IsBoolean:Boolean;
  function IsArray:Boolean;
  function Assign(value:tvalue):tvalue;
 end;


implementation

// TValue TValue TValue TValue TValue TValue TValue TValue TValue TValue TValue
//==============================================================================

constructor TValue.create(position:tpoint);
begin
 FPosition:=position;
 GetLocaleFormatSettings(0,FFormatSettings);
 FFormatSettings.DecimalSeparator:='.';
 Ftypevalue:=TypeNone;
 FNumber:=0;
 FBoolean:=false;
 Fstring:='';
 FCount:=0;
 FArray:=nil;
end;

constructor TValue.create;
begin
 create(point(-1,-1));
end;


constructor TValue.create(position:tpoint;Value:extended);
begin
 Create(position);
 Ftypevalue:=TypeNumber;
 FNumber:=Value;
end;

constructor TValue.create(position:tpoint;Value:string);
begin
 Create(position);
 Ftypevalue:=TypeString;
 Fstring:=Value;
end;


constructor TValue.create(position:tpoint;Value:boolean);
begin
 Create(position);
 Ftypevalue:=TypeBoolean;
 FBoolean:=Value;
end;

constructor TValue.create(position:tpoint;Value:TValue);
var
 i:integer;
begin
 Create(position);
 if value=nil then exit;
 Ftypevalue:=Value.Ftypevalue;
 FNumber:=Value.FNumber;
 FBoolean:=Value.FBoolean;
 Fstring:=Value.FString;
 for i:=0 to Value.FCount-1 do Add(Value.FArray[i]);
end;

constructor TValue.create(Value:extended);
begin
 create(point(-1,-1),value);
end;

constructor TValue.create(Value:string);
begin
 create(point(-1,-1),value);
end;

constructor TValue.create(Value:boolean);
begin
 create(point(-1,-1),value);
end;

constructor TValue.create(Value:TValue);
begin
 create(point(-1,-1),value);
end;

procedure TValue.ClearArray;
var
 i:integer;
begin
 for i:=0 to Fcount-1 do FArray[i].Free;
 Fcount:=0;
 Setlength(FArray,0);
end;

procedure TValue.Clear;
begin
 ClearArray;
 FTypeValue:=TypeNone;
end;

function TValue.ToString:string;
begin
 FString:=GetStringValue;
 FTypevalue:=TypeString;
 ClearArray;
 result:=FString;
end;

function TValue.ToNumber:extended;
begin
 FNumber:=GetNumberValue;
 FTypevalue:=TypeNumber;
 ClearArray;
 result:=FNumber;
end;

function TValue.ToBoolean:Boolean;
begin
 FBoolean:=GetBooleanValue;
 FTypevalue:=TypeBoolean;
 ClearArray;
 result:=FBoolean;
end;

procedure TValue.ToArray;
var
 v:tvalue;
begin
 // si c'est une valeur vide, on transforme en tableau vide
 if FTypeValue=TypeNone then FTypeValue:=TypeArray;

 if FTypeValue<>TypeArray then
  begin
   v:=tvalue.create(self);
   inc(FCount);
   setlength(FArray,FCount);
   FArray[FCount-1]:=v;
   FTypeValue:=TypeArray;
  end;
end;

destructor TValue.Destroy;
var
 i:integer;
begin
 for i:=0 to Fcount-1 do FArray[i].Free;
 inherited Destroy;
end;

function TValue.Assign(value:tvalue):tvalue;
var
 i:integer;
begin
 Ftypevalue:=Value.Ftypevalue;
 FNumber:=Value.FNumber;
 FBoolean:=Value.FBoolean;
 Fstring:=Value.FString;
 ClearArray;
 for i:=0 to Value.FCount-1 do Add(Value.FArray[i]);
 result:=self;
end;

function TValue.IsString:Boolean;
begin
 result:=Ftypevalue = TypeString;
end;

function TValue.IsNumber:Boolean;
begin
 result:=Ftypevalue = TypeNumber;
end;

function TValue.IsBoolean:Boolean;
begin
 result:=Ftypevalue = TypeBoolean;
end;

function TValue.IsArray:Boolean;
begin
 result:=Ftypevalue = TypeArray;
end;

procedure TValue.add(Value:TValue);
var
 i:integer;
 v:tvalue;
begin
 if FTypeValue<>TypeArray then
  begin
   v:=tvalue.create(self);
   inc(FCount);
   setlength(FArray,FCount);
   FArray[FCount-1]:=v;
   FTypeValue:=TypeArray;
  end;
 inc(FCount);
 setlength(FArray,FCount);
 FArray[FCount-1]:=tvalue.create(Value);
end;

function TValue.GetNumberValue:extended;
begin
 case FTypevalue of
  TypeNone:result:=0;
  TypeNumber:result:=FNumber;
  TypeString:if not trystrtofloat(FString,result,FFormatSettings) then
                Raise Exception.CreateFmt('impossible de convertir "%s" en nombre (%d,%d)', [FString,Fposition.X,Fposition.Y]);
  TypeBoolean:if FBoolean then result:=1 else result:=0;
  TypeArray:result:=Fcount;
 end;
end;

procedure TValue.SetNumberValue(v:extended);
begin
 Ftypevalue:=TypeNumber;
 FNumber:=v;
 ClearArray;
end;

function TValue.GetStringValue:string;
var
 i:integer;
begin
 case FTypevalue of
  TypeNone:result:='';
  TypeNumber:result:=floattostr(FNumber,FFormatSettings);
  TypeString:result:=FString;
  TypeBoolean:if FBoolean then result:='TRUE' else result:='FALSE';
  TypeArray:
   begin
    result:='[';
    for i:=0 to Fcount-1 do
     begin
      result:=result+FArray[i].GetStringValue;
      if i<>Fcount-1 then result:=result+',';
     end;
    result:=result+']';
   end;
 end;
end;

procedure TValue.SetStringValue(v:string);
begin
 Ftypevalue:=TypeString;
 FString:=v;
 ClearArray;
end;

function TValue.GetBooleanValue:boolean;
begin
 case FTypevalue of
  TypeNone:result:=false;
  TypeNumber:result:=FNumber<>0;
  TypeString:result:=FString<>'';
  TypeBoolean:result:=FBoolean;
  TypeArray:result:=Fcount<>0;
 end;
end;

procedure TValue.SetBooleanValue(v:boolean);
begin
 Ftypevalue:=TypeBoolean;
 FBoolean:=v;
 ClearArray;
end;

function TValue.GetArrayValue(index:integer):tvalue;
begin
 if (index<0) or (index>fcount-1) then
  Raise Exception.CreateFmt('indice en dehors des limites (%d,%d)', [Fposition.X,Fposition.Y]);
 result:=Farray[index];
end;

procedure TValue.SetArrayValue(index:integer;value:tvalue);
begin
 //
end;

end.
