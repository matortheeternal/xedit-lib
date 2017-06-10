unit txFiles;

interface

uses
  SysUtils;

  // PUBLIC TESTING INTERFACE
  procedure BuildFileHandlingTests;

implementation

uses
  Mahogany,
  txMeta,
{$IFDEF USE_DLL}
  txImports;
{$ENDIF}
{$IFNDEF USE_DLL}
  xeFiles;
{$ENDIF}

procedure BuildFileHandlingTests;
var
  h: Cardinal;
begin
  Describe('File Handling Functions', procedure
    begin
      Describe('FileByName', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              ExpectSuccess(FileByName('Skyrim.esm', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return false if a matching file is not loaded', procedure
            begin
              ExpectFailure(FileByName('NonExistingFile.esp', @h));
            end);
        end);

      Describe('FileByIndex', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              ExpectSuccess(FileByIndex(1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return false if index is out of bounds', procedure
            begin
              ExpectFailure(FileByIndex(999, @h));
            end);
        end);

      Describe('FileByLoadOrder', procedure
        begin
          It('Should return a handle if the index is in bounds', procedure
            begin
              ExpectSuccess(FileByLoadOrder(1, @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return false if index is out of bounds', procedure
            begin
              ExpectFailure(FileByLoadOrder(999, @h));
            end);
        end);

      Describe('FileByAuthor', procedure
        begin
          It('Should return a handle if a matching file is loaded', procedure
            begin
              ExpectSuccess(FileByAuthor('mcarofano', @h));
              Expect(h > 0, 'Handle should be greater than 0');
            end);

          It('Should return return false if a matching file is not loaded', procedure
            begin
              ExpectFailure(FileByAuthor('U. N. Owen', @h));
            end);
        end);

      Describe('SaveFile', procedure
        begin
          It('Should return true if it succeeded in saving the file', procedure
            begin
              ExpectSuccess(SaveFile(3));
            end);

          It('Should return false if the index is out of bounds', procedure
            begin
              ExpectFailure(SaveFile(999));
            end);
        end);

      {$IF false}
      Describe('AddFile', procedure
        begin
          It('Should return true if it succeeds', procedure
            begin
              ExpectSuccess(AddFile('abc.esp', @h));
            end);

          It('Should return false if the file already exists', procedure
            begin
              ExpectFailure(AddFile('Dawnguard.esm', @h));
            end);

          {$IF false}
          It('Should return false if the load order is already full', procedure
            begin
              AddFile('0.esp', @h);
              AddFile('1.esp', @h);
              AddFile('2.esp', @h);
              AddFile('3.esp', @h);
              AddFile('4.esp', @h);
              AddFile('5.esp', @h);
              AddFile('6.esp', @h);
              AddFile('7.esp', @h);
              AddFile('8.esp', @h);
              AddFile('9.esp', @h);
              AddFile('10.esp', @h);
              AddFile('11.esp', @h);
              AddFile('12.esp', @h);
              AddFile('13.esp', @h);
              AddFile('14.esp', @h);
              AddFile('15.esp', @h);
              AddFile('16.esp', @h);
              AddFile('17.esp', @h);
              AddFile('18.esp', @h);
              AddFile('19.esp', @h);
              AddFile('20.esp', @h);
              AddFile('21.esp', @h);
              AddFile('22.esp', @h);
              AddFile('23.esp', @h);
              AddFile('24.esp', @h);
              AddFile('25.esp', @h);
              AddFile('26.esp', @h);
              AddFile('27.esp', @h);
              AddFile('28.esp', @h);
              AddFile('29.esp', @h);
              AddFile('30.esp', @h);
              AddFile('31.esp', @h);
              AddFile('32.esp', @h);
              AddFile('33.esp', @h);
              AddFile('34.esp', @h);
              AddFile('35.esp', @h);
              AddFile('36.esp', @h);
              AddFile('37.esp', @h);
              AddFile('38.esp', @h);
              AddFile('39.esp', @h);
              AddFile('40.esp', @h);
              AddFile('41.esp', @h);
              AddFile('42.esp', @h);
              AddFile('43.esp', @h);
              AddFile('44.esp', @h);
              AddFile('45.esp', @h);
              AddFile('46.esp', @h);
              AddFile('47.esp', @h);
              AddFile('48.esp', @h);
              AddFile('49.esp', @h);
              AddFile('50.esp', @h);
              AddFile('51.esp', @h);
              AddFile('52.esp', @h);
              AddFile('53.esp', @h);
              AddFile('54.esp', @h);
              AddFile('55.esp', @h);
              AddFile('56.esp', @h);
              AddFile('57.esp', @h);
              AddFile('58.esp', @h);
              AddFile('59.esp', @h);
              AddFile('60.esp', @h);
              AddFile('61.esp', @h);
              AddFile('62.esp', @h);
              AddFile('63.esp', @h);
              AddFile('64.esp', @h);
              AddFile('65.esp', @h);
              AddFile('66.esp', @h);
              AddFile('67.esp', @h);
              AddFile('68.esp', @h);
              AddFile('69.esp', @h);
              AddFile('70.esp', @h);
              AddFile('71.esp', @h);
              AddFile('72.esp', @h);
              AddFile('73.esp', @h);
              AddFile('74.esp', @h);
              AddFile('75.esp', @h);
              AddFile('76.esp', @h);
              AddFile('77.esp', @h);
              AddFile('78.esp', @h);
              AddFile('79.esp', @h);
              AddFile('80.esp', @h);
              AddFile('81.esp', @h);
              AddFile('82.esp', @h);
              AddFile('83.esp', @h);
              AddFile('84.esp', @h);
              AddFile('85.esp', @h);
              AddFile('86.esp', @h);
              AddFile('87.esp', @h);
              AddFile('88.esp', @h);
              AddFile('89.esp', @h);
              AddFile('90.esp', @h);
              AddFile('91.esp', @h);
              AddFile('92.esp', @h);
              AddFile('93.esp', @h);
              AddFile('94.esp', @h);
              AddFile('95.esp', @h);
              AddFile('96.esp', @h);
              AddFile('97.esp', @h);
              AddFile('98.esp', @h);
              AddFile('99.esp', @h);
              AddFile('100.esp', @h);
              AddFile('101.esp', @h);
              AddFile('102.esp', @h);
              AddFile('103.esp', @h);
              AddFile('104.esp', @h);
              AddFile('105.esp', @h);
              AddFile('106.esp', @h);
              AddFile('107.esp', @h);
              AddFile('108.esp', @h);
              AddFile('109.esp', @h);
              AddFile('110.esp', @h);
              AddFile('111.esp', @h);
              AddFile('112.esp', @h);
              AddFile('113.esp', @h);
              AddFile('114.esp', @h);
              AddFile('115.esp', @h);
              AddFile('116.esp', @h);
              AddFile('117.esp', @h);
              AddFile('118.esp', @h);
              AddFile('119.esp', @h);
              AddFile('120.esp', @h);
              AddFile('121.esp', @h);
              AddFile('122.esp', @h);
              AddFile('123.esp', @h);
              AddFile('124.esp', @h);
              AddFile('125.esp', @h);
              AddFile('126.esp', @h);
              AddFile('127.esp', @h);
              AddFile('128.esp', @h);
              AddFile('129.esp', @h);
              AddFile('130.esp', @h);
              AddFile('131.esp', @h);
              AddFile('132.esp', @h);
              AddFile('133.esp', @h);
              AddFile('134.esp', @h);
              AddFile('135.esp', @h);
              AddFile('136.esp', @h);
              AddFile('137.esp', @h);
              AddFile('138.esp', @h);
              AddFile('139.esp', @h);
              AddFile('140.esp', @h);
              AddFile('141.esp', @h);
              AddFile('142.esp', @h);
              AddFile('143.esp', @h);
              AddFile('144.esp', @h);
              AddFile('145.esp', @h);
              AddFile('146.esp', @h);
              AddFile('147.esp', @h);
              AddFile('148.esp', @h);
              AddFile('149.esp', @h);
              AddFile('150.esp', @h);
              AddFile('151.esp', @h);
              AddFile('152.esp', @h);
              AddFile('153.esp', @h);
              AddFile('154.esp', @h);
              AddFile('155.esp', @h);
              AddFile('156.esp', @h);
              AddFile('157.esp', @h);
              AddFile('158.esp', @h);
              AddFile('159.esp', @h);
              AddFile('160.esp', @h);
              AddFile('161.esp', @h);
              AddFile('162.esp', @h);
              AddFile('163.esp', @h);
              AddFile('164.esp', @h);
              AddFile('165.esp', @h);
              AddFile('166.esp', @h);
              AddFile('167.esp', @h);
              AddFile('168.esp', @h);
              AddFile('169.esp', @h);
              AddFile('170.esp', @h);
              AddFile('171.esp', @h);
              AddFile('172.esp', @h);
              AddFile('173.esp', @h);
              AddFile('174.esp', @h);
              AddFile('175.esp', @h);
              AddFile('176.esp', @h);
              AddFile('177.esp', @h);
              AddFile('178.esp', @h);
              AddFile('179.esp', @h);
              AddFile('180.esp', @h);
              AddFile('181.esp', @h);
              AddFile('182.esp', @h);
              AddFile('183.esp', @h);
              AddFile('184.esp', @h);
              AddFile('185.esp', @h);
              AddFile('186.esp', @h);
              AddFile('187.esp', @h);
              AddFile('188.esp', @h);
              AddFile('189.esp', @h);
              AddFile('190.esp', @h);
              AddFile('191.esp', @h);
              AddFile('192.esp', @h);
              AddFile('193.esp', @h);
              AddFile('194.esp', @h);
              AddFile('195.esp', @h);
              AddFile('196.esp', @h);
              AddFile('197.esp', @h);
              AddFile('198.esp', @h);
              AddFile('199.esp', @h);
              AddFile('200.esp', @h);
              AddFile('201.esp', @h);
              AddFile('202.esp', @h);
              AddFile('203.esp', @h);
              AddFile('204.esp', @h);
              AddFile('205.esp', @h);
              AddFile('206.esp', @h);
              AddFile('207.esp', @h);
              AddFile('208.esp', @h);
              AddFile('209.esp', @h);
              AddFile('210.esp', @h);
              AddFile('211.esp', @h);
              AddFile('212.esp', @h);
              AddFile('213.esp', @h);
              AddFile('214.esp', @h);
              AddFile('215.esp', @h);
              AddFile('216.esp', @h);
              AddFile('217.esp', @h);
              AddFile('218.esp', @h);
              AddFile('219.esp', @h);
              AddFile('220.esp', @h);
              AddFile('221.esp', @h);
              AddFile('222.esp', @h);
              AddFile('223.esp', @h);
              AddFile('224.esp', @h);
              AddFile('225.esp', @h);
              AddFile('226.esp', @h);
              AddFile('227.esp', @h);
              AddFile('228.esp', @h);
              AddFile('229.esp', @h);
              AddFile('230.esp', @h);
              AddFile('231.esp', @h);
              AddFile('232.esp', @h);
              AddFile('233.esp', @h);
              AddFile('234.esp', @h);
              AddFile('235.esp', @h);
              AddFile('236.esp', @h);
              AddFile('237.esp', @h);
              AddFile('238.esp', @h);
              AddFile('239.esp', @h);
              AddFile('240.esp', @h);
              AddFile('241.esp', @h);
              AddFile('242.esp', @h);
              AddFile('243.esp', @h);
              AddFile('244.esp', @h);
              AddFile('245.esp', @h);
              AddFile('246.esp', @h);
              ExpectFailure(AddFile('247.esp', @h));
            end);
          {$IFEND}
        end);
      {$IFEND}
    end);
end;

end.
