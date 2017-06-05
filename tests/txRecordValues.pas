unit txRecordValues;

interface

  procedure BuildRecordValueTests;

implementation


uses
  Mahogany,
{$IFDEF USE_DLL}
  txImports,
{$ENDIF}
{$IFNDEF USE_DLL}
  xeElements, xeRecordValues,
{$ENDIF}
  txMeta, txElements;

procedure BuildRecordValueTests;
var
  xt2, block, subBlock, childGroup, persistentGroup, refr, armo, rec,
  element, keyword: Cardinal;
  len: Integer;
begin
  Describe('Record Values', procedure
    begin
      BeforeAll(procedure
        begin
          GetElement(0, 'xtest-2.esp', @xt2);
          GetElement(xt2, 'ARMO', @armo);
          GetElement(armo, '00012E46', @rec);
          GetElement(rec, 'DNAM', @element);
          GetElement(rec, 'KWDA\[1]', @keyword);
          GetElement(xt2, '00027D1C\Child Group', @childGroup);
          GetElement(xt2, 'CELL\[0]', @block);
          GetElement(block, '[0]', @subBlock);
          GetElement(childGroup, '[0]', @persistentGroup);
          GetElement(xt2, '000170F0', @refr);
        end);

      Describe('EditorID', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(EditorID(xt2, @len));
            end);
          It('Should fail if a group is passed', procedure
            begin
              ExpectFailure(EditorID(block, @len));
              ExpectFailure(EditorID(subBlock, @len));
              ExpectFailure(EditorID(childGroup, @len));
              ExpectFailure(EditorID(persistentGroup, @len));
              ExpectFailure(EditorID(armo, @len));
            end);
          It('Should fail if an element is passed', procedure
            begin
              ExpectFailure(EditorID(element, @len));
              ExpectFailure(EditorID(keyword, @len));
            end);
          It('Should return EditorID if a record is passed', procedure
            begin
              ExpectSuccess(EditorID(rec, @len));
              ExpectEqual(grs(len), 'ArmorIronGauntlets');
              ExpectSuccess(EditorID(refr, @len));
              ExpectEqual(grs(len), 'DA09PedestalEmptyRef');
            end);
        end);

      Describe('FullName', procedure
        begin
          It('Should fail if a file is passed', procedure
            begin
              ExpectFailure(FullName(xt2, @len));
            end);
          It('Should fail if a group is passed', procedure
            begin
              ExpectFailure(FullName(block, @len));
              ExpectFailure(FullName(subBlock, @len));
              ExpectFailure(FullName(childGroup, @len));
              ExpectFailure(FullName(persistentGroup, @len));
              ExpectFailure(FullName(armo, @len));
            end);
          It('Should fail if an element is passed', procedure
            begin
              ExpectFailure(FullName(element, @len));
              ExpectFailure(FullName(keyword, @len));
            end);
          It('Should fail if a record with no full name is passed', procedure
            begin
              ExpectFailure(FullName(refr, @len));
            end);
          It('Should return Full Name if a record is passed', procedure
            begin
              ExpectSuccess(FullName(rec, @len));
              ExpectEqual(grs(len), 'Iron Gauntlets', '');
            end);
        end);
    end);
end;

end.
