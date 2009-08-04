Para fazer:

.Visualizar relatorio em tempo de design
.Corrigir bug que impede visualizar componentes no linux ao recarregar o projeto (obs: no windows carrega perfeitamente)
.Corrigir bug do RLPreview
.Implementar giro nos componentes RLAngleLabel e RLBarcode

Historico

28/07/2009:

.O Draftfilter esta funcionando perfeitamente no modo padr�o do FortesReport

O modo padr�o � configurado automaticamente quando insere o componente Draftfilter no form, sendo o padr�o o seguinte:
WINDOWS
DeviceKind :=dkPrinterPort;
DevicePath :='prn';

Linux
DeviceKind :=dkProgram;
DevicePath :='lpr -P%p %f';

27/07/2009:
Unit RLPrinters
.Corre��es para as fun��es:

LoadPrintersList no linux
GetPrinterDevice no linux

23/07/2009:
No linux, nao precisa mais da biblioteca libborqt-6.9.0-qt2.3.so
.Draftfilter:
.Retirada todas as chamadas de scanline, antes dependente da dll, que teve mal funcionamento.
Obviamente, em alguns casos deve retornar algum valor estranho. Nos meus testes funcionou normalmente
.Eu consegui usar o DraftFilter de duas maneiras:
1- DeviceKind:=dkFilename;

linux-> DevicePath:='/dev/lp0';
windows-> DevicePath:='prn';
...funcionou perfeitamente
ou
2- DeviceKind:=dkFilename;
DevicePath:='/tmp/saida.txt';
Nesse caso gerou um arquivo txt, usando o comando
$ cat /tmp/saida.txt > /dev/lp0
type c:\saida.txt > prn
...funcionou perfeitamente



22/07/2009:
.Na vers�o para linux, n�o precisa mais da librlreportshared.so
.Corrigido bug do ShowProgress, que eu havia fixado em True.
.DraftFilter n�o funcionou, provavelmente problemas com a dll.

21/07/2009:

.Listagem simples com Header, ColumHeader,Detail, Summary e Footer funcionou perfeitamente.
.Relatorio com RLGroup funcionou perfeitamente
.Exporta para PDF, RichText, HTML e Excel sem problemas
.Preview funcionando perfeitamente.
.Print funcionou perfeitamente.

.DraftFilter ainda n�o foi testado. Assim que eu conseguir uma impressora matricial eu farei os testes.

.Os componentes RLBarcode e RLAngleLabel est�o sem a op��o de rotacionar, pois ainda n�o est� resolvido a quest�o do ScanLine, que n�o tem na classe TBitmap do freepascal.

.O componente RLPreview n�o funcionou, da erro e fecha o lazarus
.Testado em win32 e GTK2 (no meu ARCHLINUX)
.Na GTK2 apresenta problemas na ide. Ao recarregar um projeto com o componente RLReport, simplesmente fica impossivel de alterar, pois n�o � possivel discernir onde est�o os componentes.
.No windows funcionou beleza, sem problemas na ide.

Postem suas sugest�es, criticas e duvidas para que todos possamos tirar proveito desse componente, e deixa-lo o mais funcional possivel.

email para contato sobre o projeto:
dicas4lazarus@yahoo.com.br

TRLCustomControl.Notification -> for�a internalpaint
O evento paint do TRLCustomControl foi for�ado a executar em todo o processo.
O evento internalpaint do TRLLabel tb.

*19-07-2009
unit rlreport.pas

procedure TRLCustomSite.Loaded;

Adicionado o codigo:
  {$IFDEF FPC}
  PaintAsCustomSite;
  {$ENDIF}

para for�ar o desenho do rlreport no form.

*19-07-2009

Adicionado a DIRETIVA TRACEREPORT
  
{$IFDEF TRACEREPORT}

*19-07-2009

Ignorada a funcao RotatedBitMap em rlbarcode.pas e rlreport.pas

