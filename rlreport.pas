///3.24 TRLCustomSkipper.DataFirst loops

{$I RLReport.inc}
//{$DEFINE TRACECUSTOMSITE}
//{$DEFINE TRACECUSTOMLABEL}
{@unit RLReport - Implementação dos principais componentes e tipos do FortesReport.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009

29/04/10 - Luiz Américo
  * Remove código específico de Delphi/CLX. Usa tipos específicos da LCL
  * Corrige RLAngleLabel
}
unit RLReport;
{$MODE DELPHI}{$H+}
interface

uses
  Types, LCLType, LCLProc, DB, Classes, SysUtils, Math, Contnrs, TypInfo, SyncObjs,
{$ifdef USEVARIANTS}
  Variants,
{$endif}
{$ifdef USEMASKUTILS}
  MaskUtils,
{$endif}
  ExtCtrls, DBCtrls, Controls, Forms, Dialogs, StdCtrls, LMessages, Buttons, Graphics,
  RLMetaVCL, RLMetaFile, RLFeedBack, RLParser, RLFilters, RLConsts, RLUtils, RLPrintDialog, RLSaveDialog, RLPreviewForm,
  RLPreview, RLTypes, RLPrinters, RLSpoolFilter, RLPageSetupConfig;

const
  CommercialVersion=RLConsts.CommercialVersion;
  ReleaseVersion=RLConsts.ReleaseVersion;
  CommentVersion=RLConsts.CommentVersion;

type

  // CLASSES
  
  TRLCustomControl=class;             // tcontrol base para todos os outros
    TRLCustomDBControl=class;         // tcontrol base com acesso a banco de dados
    TRLCustomLabel=class;             // tcontrol base com texto de uma linha
      TRLLabel=class;                 // componente label
      TRLCustomDBText=class;          // label basico de campo de arquivo
        TRLDBText=class;              // label de campo de arquivo
        TRLCustomDBResult=class;      // label de operacoes com campos
          TRLDBResult=class;
      TRLCustomSystemInfo=class;      // label com informacoes de sistema (nr.pag,totais,data&hora)
        TRLSystemInfo=class;
    TRLCustomAngleLabel=class;
      TRLAngleLabel=class;
    TRLCustomMultiLine=class;         // tcontrol base com texto de varias linhas
      TRLCustomMemo=class;            // componente memo
        TRLMemo=class;
      TRLCustomDBMemo=class;          // memo de campo de arquivo
        TRLDBMemo=class;
    TRLCustomImage=class;             // componente imagem
      TRLImage=class;
      TRLCustomDBImage=class;         // imagem de campo de arquivo
        TRLDBImage=class;
    TRLCustomDraw=class;              // canvas de figuras
      TRLDraw=class;
    TRLCustomSite=class;              // controle base para todas as janelas de impressao
      TRLCustomPanel=class;
        TRLPanel=class;               // componente de design para fazer containers alinhados
      TRLCustomBandSet=class;
        TRLCustomBand=class;            // banda de impressao que pode ser: header, detail ou footer
          TRLBand=class;
          TRLCustomDetailGrid=class;    // banda de colunas de detalhe
            TRLDetailGrid=class;
        TRLCustomPager=class;           // painel paginador de tamanho variavel com header, detail e footer
          TRLCustomGroup=class;         // paginador com escopo de registros
            TRLGroup=class;
          TRLCustomSkipper=class;       // paginador controlado por base de dados
            TRLCustomSubDetail=class;   // controlador de base masterizada
              TRLSubDetail=class;
            TRLCustomReport=class;
              TRLReport=class;          // controlador principal

  TRLBorders=class;
  TRLMargins=class;
  TRLPageSetup=class;
  TRLRealBounds=class;
  TRLBackground=class;
  TRLDegradeEffect=class;
  TRLSortedBands=class;

  // CLASS TYPES

  TRLPagerClassType=class of TRLCustomPager;

  // PROPERTY TYPES

  {@type TRLDataFieldProperty - Tipo de propriedade para nome de campo de um dataset.
  Este tipo existe somente para identificar o editor de prop coerente para a prop datafield. :/}
  TRLDataFieldProperty=type string;

  {@type TRLDataFieldsProperty - Tipo de propriedade para lista de nomes de campos de um dataset.
  Este tipo existe somente para identificar o editor de prop coerente para a prop datafields
  Os nomes dos campos devem ser separados por ponto-e-vírgula (";"). :/}
  TRLDataFieldsProperty=type string;

  {@type TRLRecordRange - Especifica uma faixa de registros do dataset que o skipper deve processar. 
   Pode ser um dos seguintes valores:
   rrAllRecords - Processa desde o primeiro registro até o fim (default);
   rrCurrentOnly - Processa apenas o registro corrente e termina;
   rrUntilEof - Processa a partir do registro corrente até o fim;
   rrNextN - Processa a partir do registro corrente (inclusive) N registros. Informe N na prop RangeCount.
   @links TRLCustomSkipper.RecordRange, TRLCustomSkipper.RangeCount. :/}
  TRLRecordRange=(rrAllRecords,rrCurrentOnly,rrUntilEof,rrNextN);

  // EVENT TYPES

  {@type TRLRecordAction - Especifica uma ação a ser tomada pelo skipper para cada registro processado da dataset.
   Pode ser um dos seguintes valores:
   raUseIt - Utilizar o registro e processar o próximo;
   raIgnoreIt - Não utilizar o registro e processar o próximo;
   raUseAndRetain - Utilizar o registro e retê-lo para que seja utilizado uma outra vez;
   raBlankAndRetain - Imprimir detalhes em branco e adiar o uso do registro para a próxima iteração.
   @links TRLCustomSkipper.DataFirst, TRLCustomSkipper.DataNext, TRLCustomSkipper.RecordRange. :/}
  TRLRecordAction=(raUseIt,raIgnoreIt,raUseAndRetain,raBlankAndRetain);

  {@type TRLAfterPrintEvent - Evento disparado após a impressão de um componente.
   Implemente um evento TRLAfterPrintEvent para tomar atitudes logo após a impressão
   de um componente.
   @links TRLBeforePrintEvent, TRLBeforeTextEvent. :/}
  TRLAfterPrintEvent=procedure(Sender:TObject) of object;

  {@type TRLBeforePrintEvent - Evento disparado antes da impressão de um componente.
   Implemente um evento TRLBeforePrintEvent para tomar atitudes antes da impressão de um componente.
   É possível bloquear a impressão do componente através do parâmetro PrintIt, ou alterar as suas
   características, como dimensões, cor, bordas etc.
   @links TRLAfterPrintEvent, TRLBeforeTextEvent. :/}
  TRLBeforePrintEvent=procedure(Sender:TObject; var PrintIt:boolean) of object;

  {@type TRLBeforeTextEvent - Evento disparado antes da impressão de um componente do tipo caixa de texto.
   Implemente um evento TRLBeforePrintEvent para tomar atitudes antes da impressão de um componente.
   É possível bloquear a impressão do componente através do parâmetro PrintIt, ou alterar as suas
   características, como dimensões, cor, bordas etc.
   Para alterar o conteúdo de texto do componente utilize o parâmetro OutputText e não as props do componente.
   Nota: Este evento aparecerá como o nome BeforePrint nas caixas de texto.
   @links TRLAfterPrintEvent, TRLBeforePrintEvent, TRLCustomLabel. :/}
  TRLBeforeTextEvent=procedure(Sender:TObject; var OutputText:string; var PrintIt:boolean) of object;

  {@type TRLOnComputeEvent - Evento disparado pelo componente TRLDBResult ao considerar um valor para
   estatística nos acumuladores internos.
   Altere os parâmetro Value e OutputText para modificar o valor processado pelos acumuladores.
   Altere o parâmetro ComputeIt para indicar se o valor deve ser computado ou ignorado.
   @links TRLDBResult. :/}
  TRLOnComputeEvent=procedure(Sender:TObject; var Value:variant; var OutputText:string; var ComputeIt:boolean) of object;

  {@type TRLOnGetBreakEvent - Evento disparado pelo componente TRLGroup durante as verificações de quebra de
   sequência de registros manual.
   Altere o parâmetro BreakIt para indicar a quebra de sequência de registros num grupo.
   @links TRLGroup. :/}
  TRLOnGetBreakEvent=procedure(Sender:TObject; var BreakIt:boolean) of object;

  {@type TRLOnDataCountEvent - Evento disparado pelos componentes TRLCustomSkipper no início da impressão para
   calcular a quantidade de registros e fornecer feedback.
   Informe a quantidade total de registros do dataset alterando o parâmetro DataCount. Isto ajuda a projetar a
   expectativa de término, e também substitui a consulta à prop RecordCount do DataSet que estiver associado
   pela prop DataSource.
   Nota: A prop RecordCount só estará acessível em datasets bi-direcionais, como é o caso dos componentes do BDE
   e ClientDataSets.
   @links TRLCustomSkipper. :/}
  TRLOnDataCountEvent=procedure(Sender:TObject; var DataCount:integer) of object;

  {@type TRLOnNeedDataEvent - Evento disparado pelo TRLCustomSkipper para a produção de registros quando não
   há um dataset associado.
   Implemente este evento para fornecer registros de dados a um RLReport ou RLSubDetail quando não houver um
   dataset associado. Neste caso, não deverão ser utilizados componentes dataware como TRLDBText, e o conteúdo
   dos controles terá de ser feito nos eventos e com acesso direto as props Caption, Text etc.
   Quando não houver mais registros a processar, altere o valor do parâmetro MoreData para false.
   @links TRLCustomSkipper. :/}
  TRLOnNeedDataEvent=procedure(Sender:TObject; var MoreData:boolean) of object;

  {@type TRLOnDataRecordEvent - Evento disparado pelo TRLCustomSkipper durante o processamento de um registro.
   Este evento é disparado todas as vezes que um registro é processado, quer seja físico obtido de um DataSet ou
   virtual indicado no evento OnNeedData.
   O parâmetro RecNo representa o número sequencial do registro corrente, é o número de ordem.
   O parâmetro CopyNo é o número de cópia do registro, caso ele esteja sendo repetido. Um registro é repetido se
   a última ação foi indicada como raUseAndRetain ou raBlankAndRetain. O valor de CopyNo será incrementado tantas
   vezes quantas forem as repetições do registro.
   O parâmetro Eof indica ou atribui o fim dos dados. É semelhante ao MoreData do evento OnNeedData.
   O parâmetro RecordAction indica a ação a ser tomada para este registro.
   @links TRLCustomSkipper, TRLRecordAction. :/}
  TRLOnDataRecordEvent=procedure(Sender:TObject; RecNo:integer; CopyNo:integer; var Eof:boolean; var RecordAction:TRLRecordAction) of object;

  {@type TRLOnDrawEvent - Evento disparado pelo TRLCustomSite durante o desenho do fundo do componente.
   A impressão do site já está consolidada, o tamanho final está definido e o componente está pronto para ser impresso.
   Implemente este evento para desenhar qualquer coisa no fundo de um componente como marca d'água.
   O parâmetro Surface representa a superfície de desenho (o Canvas) do componente em questão.
   Rect é o retângulo da área cliente do componente, aonde é permitido desenhar.
   @links TRLCustomSite. :/}
  TRLOnDrawEvent=procedure(Sender:TObject; Surface:TRLGraphicSurface; Rect:TRect) of object;

  // ENUMERATED TYPES
                                                
  {@type TRLBandType - Especifica o tipo da band e indica o comportamento que a band deverá assumir durante a listagem.
   Pode ser um dos seguintes valores:
   btHeader - Cabeçalho. Impressa uma vez na primeira página e sempre que houver quebra de página ou de sequência
   de registros. É útil para exibir números de página, escopo do relatório ou informações sobre a sequência de registros
   atual, no caso dos grupos e subdetalhes;
   btTitle - Título. Impressa apenas na primeira página ou no ínicio de uma sequência de registros logo abaixo do
   header. É útil para mostrar uma descrição do relatório;
   btColumnHeader - Cabeçalho de colunas. Mesmo comportamento do header exceto por seu posicionamento após o title;
   btDetail - Detalhe. Imprime uma vez para cada registro de dados;
   btColumnFooter - Rodapé de colunas. Mesmo comportamento do rodapé exceto por seu posicionamento antes do summary;
   btSummary - Sumário. Imprime ao final do relatório ou da sequência de registros antes do footer. É útil para mostrar
   resumos, somatórios e informações estatísticas;
   btFooter - Rodapé. Imprime uma vez na última página e sempre após quebra de página ou de sequência de dados.
   @links TRLBand. :/}
  TRLBandType=(btHeader,btTitle,btColumnHeader,btDetail,btColumnFooter,btSummary,btFooter);
  
  {@type TRLCompletionType - Escpecifica o preenchimento de página quando não houver mais dados a imprimir e estiver
   sobrando espaço.
   Quando não houver mais registros a imprimir e ainda houver espaço, a página poderá ser completamente preenchida com
   bands de detalhe em branco de acordo com o indicado na prop Completion.
   Pode ser um dos seguintes valores:
   ctNone - Não completa;
   ctFullPage - Completa até o fim da página;
   ctMaxBands - Completa até o número máximo de bands do ParentPager;
   ctMinBands - Completa até o número mínimo de bands do ParentPager.
   @links TRLBand, MaxBands. :/}
  TRLCompletionType=(ctNone,ctFullPage,ctMaxBands,ctMinBands);

  {@type TRLImageArrange - Escpecifica como uma imagem será arranjada no fundo do
   componente.
   Pode ser um dos seguintes valores:
   baAligned - A imagem deve ser alinhada no fundo de acordo com a prop Align;
   baSidebySide - A imagem deve ser distribuída pelo fundo horizontalmente;
   baCenter - A imagem deve ser centralizada;
   baDistributed - A imagem deve ser distribuída lado-a-lado como numa parede de tijolos.
   @links TRLBackground. :/}
  TRLImageArrange=(baAligned,baSidebySide,baCenter,baDistributed);

  {@type TRLReportState - Escpecifica o estado atual do relatório em relação ao processamento
   de páginas.
   Pode ser um dos seguintes valores:
   rsAbout - O relatório ainda não foi preparado;
   rsInitiating - O relatório está iniciando o processo de preparação; 
   rsPreparing - O relatório está sendo escrito;
   rsClosing - O relatório está sendo finalizado pois não há mais páginas a preparar;
   rsReady - Foi preparado e está pronto para imprimir.
   @links TRLReport. :/}
  TRLReportState=(rsAbout,rsInitiating,rsPreparing,rsClosing,rsReady);

  {@type TRLDegradeDirection - Especifica a direção do efeito transição de cores (degradê) no fundo do componente.
   Pode ser um dos seguintes valores:
   ddNone - Nenhuma efeito;
   ddHorizontal - Efeito horizontal da esquerda para a direita, da cor Color para a cor OppositeColor;
   ddVertical - Efeito vertical de cima para baixo, da cor Color para a cor OppositeColor.
   @links TRLDegradeEffect. :/}
  TRLDegradeDirection=(ddNone,ddHorizontal,ddVertical);

  {@type TRLInfoType - Especifica o tipo de informação que deve ser exibida pelo
   componente TRLSystemInfo.
   Pode ser um dos seguintes valores:
   itCarbonCopy - Número da cópia da band. A cópia é indicada na prop CarbonCopies da band;
   itDate - Data da impressão (ver prop TRLReport.ReportDateTime);
   itDetailCount - Quantidade de detalhes impressos até o momento;
   itFullDate - Data e hora da impressão no formato LongDateFormat;
   itHour - Hora da impressão (ver prop TRLReport.ReportDateTime);
   itJunction - Flag de junção de páginas. Este flag geralmente é impresso no rodapé e
   indica que o relatório não terminou e continua nas próximas páginas;
   itLastPageNumber - Número da última página do relatório (ver Nota);
   itMend - Flag de junção de páginas. Este flag é geralmente impresso no header e indica
   que a página atual é a continuação de páginas anteriores;
   itNow - Data e hora da impressão no formato ShotDateFormat;
   itPageNumber - Número da página atual;
   itPagePreview - Número da página atual e total de páginas do relatório (ver Nota);
   itTitle - Título do relatório obtido na prop Title do TRLReport;
   itRecNo - Número sequencial do registro corrente;
   itCopyNo - Número sequencial da cópia do registro.
   itCompanyName - Nome da compania - Version info
   itFileDescription - Descrição do arquivo - Version info
   itFileVersion - Versão do Arquivo - Version info
   itInternalName - Nome interno - Version info
   itLegalCopyright - Copyright - Version info
   itLegalTrademarks - Trademarks - Version info
   itOriginalFilename - OriginalFilename - Version info
   itProductName - ProductName - Version info
   itProductVersion - ProductVersion - Version info
   itComments - Comments
   Nota: O número de última página só estará disponível quando o relatório for completamente
   processado antes de ser impresso. A prop BackgroundMode deve estar desligada.
   @links TRLSystemInfo, TRLReport, TRLReport.ReportDateTime, TRLCustomBand.CarbonCopies, TRLReport.BackgroundMode. :}
  TRLInfoType=(itCarbonCopy,itDate,itDetailCount,itFullDate,itHour,itJunction,itLastPageNumber,itMend,itNow,
               itPageNumber,itPagePreview,itTitle,itRecNo,itCopyNo, itCompanyName, itFileDescription, itFileVersion, itInternalName, itLegalCopyright, itLegalTrademarks, itOriginalFilename, itProductName, itProductVersion, itComments);
  {/@type}
  {/@TRLVersionType - Tipo de informação sobre as informações do arquivo
  vtCompanyName - Nome da compania
  vtFileDescription - Descrição do arquivo
  vtFileVersion - Versão do arquivo
  vtInternalName - Nome interno
  vtLegalCopyright - Registro da cópia
  vtLegalTrademarks - Trademarks
  vtOriginalFilename - Nome original do arquivo
  vtProductName - Nome do produto
  vtProductVersion - Versão do produto
  vtComments - Comentários
  vtemail - email}


  TRLVersionType=(vtCompanyName, vtFileDescription, vtFileVersion, vtInternalName, vtLegalCopyright, vtLegalTrademarks, vtOriginalFilename, vtProductName, vtProductVersion, vtComments, vtemail);
  {/@type}

  {@type TRLResultInfo - Especifica o tipo de informação estatística exibida pelo componente TRLDBResult.
   Pode ser um dos seguintes valores:
   riAverage - Média aritmética dos valores impressos;
   riCount - Número de ocorrências dos valores impressos;
   riFirst - Primeiro valor impresso;
   riLast - Último valor impresso;
   riMax - Maior dos valores impressos;
   riMin - Menor dos valores impressos;
   riSum - Somátório de todos os valores impressos;
   riFirstText - Primeiro texto impresso;
   riLastText - Último texto impresso;
   riSimple - Valor atual da expressão. Utilizado na resolução de fórmulas com funções built-in.
   @links TRLDBResult. :/}
  TRLResultInfo=(riAverage,riCount,riFirst,riLast,riMax,riMin,riSum,riFirstText,riLastText,riSimple);
  
  {@type TRLControlAlign - Especifica regras de posicionamento do componente dentro do componente parent.
   Pode ser um dos seguintes valores:
   faNone - Nenhum alinhamento. Nenhuma alteração automática no posicionamento ou dimensão do controle;
   faLeft - Alinhado à esquerda. A largura é mantida e a altura se ajusta ao máximo disponível no controle pai;
   faTop - Alinhado acima. A altura é mantida e a largura se ajusta ao máximo disponível no controle pai;
   faRight - Alinhado à direita. A largura é mantida e a altura se ajusta ao máximo disponível no controle pai;
   faBottom - Alinhado abaixo. A altura é mantida e a largura se ajusta ao máximo disponível no controle pai;
   faLeftMost - Alinhado à esquerda com prioridade. Mesmo que faLeft com prioridade sobre os alinhamentos verticais;
   faRightMost - Alinhado à direita com prioridade. Mesmo que faRight com prioridade sobre os alinhamentos verticais;
   faClient - Alinhado à área cliente. O controle se ajusta à área que sobrou no controle pai;
   faLeftTop - Alinhado à esquerda e acima. O controle mantém suas dimensões e suas coordenadas são (0,0);
   faRightTop - Alinhado à direita e acima. O controle mantém suas dimensões e suas coordenadas são (-Width,0);
   faLeftBottom - Alinhado à esquerda e abaixo. O controle mantém suas dimensões e suas coordenadas são (0,-Height);
   faRightBottom - Alinhado à direita e abaixo. O controle mantém suas dimensões e suas coordenadas são (-Width,-Height);
   faCenter - Alinhado ao centro. O controle mantém suas dimensões;
   faCenterLeft - Alinhado ao centro e à esquerda. O controle mantém suas dimensões;
   faCenterTop - Alinhado ao centro e acima. O controle mantém suas dimensões;
   faCenterRight - Alinhado ao centro e à direita. O controle mantém suas dimensões;
   faCenterBottom - Alinhado ao centro e abaixo. O controle mantém suas dimensões;
   faClientLeft - Alinhado ao centro e à esquerda. O controle mantém suas dimensões;
   faClientTop - Alinhado ao centro e acima. O controle mantém suas dimensões;
   faClientRight - Alinhado ao centro e à esquerda. O controle mantém suas dimensões;
   faClientBottom - Alinhado ao centro e abaixo. O controle mantém suas dimensões;
   faHeight - Alinhado pela altura. O controle mantém a sua largura e expande a sua altura de modo a se acomodar no controle pai;
   faWidth - Alinhado pela largura. O controle mantém a sua altura e expande a sua largura de modo a se acomodar no controle pai;
   faLeftOnly - Alinhado à esquerda somente. O controle tem sua coordenada esquerda igual a 0;
   faRightOnly - Alinhado à direita somente. O controle tem sua coordenada direita igual a 0;
   faTopOnly - Alinhado acima somente. O controle tem sua coordenada de topo igual a 0;
   faBottomOnly - Alinhado abaixo somente. O controle tem sua coordenada abaixo igual a -Height.
   @links TRLCustomControl. :}
  TRLControlAlign=(faNone,faLeft,faTop,faRight,faBottom,faLeftMost,faRightMost,faClient,
                   faLeftTop,faRightTop,faLeftBottom,faRightBottom,
                   faCenter,faCenterLeft,faCenterTop,faCenterRight,faCenterBottom,
                   faClientLeft,faClientTop,faClientRight,faClientBottom,
                   faHeight,faWidth,
                   faLeftOnly,faRightOnly,faTopOnly,faBottomOnly);
  {/@type}                 

/// revisar daqui

  {@type TRLTextAlignment - Especifica como o texto deve ser posicionado horizontalmente dentro do componente.
   Este tipo é uma extensão do tipo padrão TAlignment com opção para texto justificado.
   Pode ser um dos seguintes valores:
   taLeftJustify - Alinhado à esquerda (padrão);
   taRightJustify - Alinhado à direita;
   taCenter - Alinhado ao centro do componente;
   taJustify - As palavras são distribuídas de modo que o texto ocupe toda a largura da área cliente do componente.
   @links TRLCustomLabel, TRLCustomMemo. :/}
  TRLTextAlignment=(taLeftJustify,taRightJustify,taCenter,taJustify);

  {@type TRLTextLayout - Especifica como o texto deve ser posicionado verticalmente dentro do componente.
   Este tipo é uma extensão do tipo padrão TLayout com opção para texto justificado.
   Pode ser um dos seguintes valores:
   tlTop - Alinhado ao topo (padrão);
   tlCenter - Alinhado ao centro do controle;
   tlBottom - Alinhado ao fundo;
   tlJustify - As linhas são distribuidas de modo que o texto ocupe toda a altura da área cliente do componente.
   @links TRLCustomLabel, TRLCustomMemo. :/}
  TRLTextLayout=(tlTop,tlCenter,tlBottom,tlJustify);

  {@type TRLPageBreaking - Especifica como a quebra de página forçada será implementada pela band ou pager.
   Pode ser um dos seguintes valores:
   pbNone - Haverá apenas quebras de páginas naturais;
   pbBeforePrint - A quebra de página será verificada sempre antes da impressão do controle;
   pbAfterPrint - A quebra de página será verificada sempre após da impressão do controle.
   @links PageBreaking. :/}
  TRLPageBreaking=(pbNone,pbBeforePrint,pbAfterPrint);

  {@type TRLPrintBandResults - Representa o valor de resultado da impressão de bands como retorno do método PrintBands.
   Pode ser um dos seguintes valores:
   brNoBands - Nenhuma band foi impressa;
   brPrinted - Ao menos uma band foi impressa;
   brStackExit - Saída forçada por salto de página.
   @links TRLCustomPager.PrintBands. :/}
  TRLPrintBandResults=(brNoBands,brPrinted,brStackExit);

  {@type TRLHoldStyle - Especifica um estilo para a ancoragem relativa de componente para componente.
   Pode ser um dos seguintes valores:
   hsAsColumn - O componente copia a coordenada horizontal do Holder. Copia também a
   largura se a propriedade AutoSize do componente estiver False;
   hsHorizontally - O componente copia a coordenada horizontal do Holder;
   hsVertically - O componente copia a coordenada vertical do Holder;
   hsRelatively - O componente mantém a distância em relação ao Holder tanto na horizontal como na vertical;
   hsCopyWidth - O componente copia a largura do Holder;
   hsCopyHeight - O componente copia a altura do Holder;
   hsCopySize - O componente copia a largura e altura do Holder.
   @links TRLCustomControl.Holder, TRLCustomControl.SecondHolder. :/}
  TRLHoldStyle=(hsAsColumn,hsHorizontally,hsVertically,hsRelatively,hsCopyWidth,hsCopyHeight,hsCopySize);

  {@type TRLPrintQuality - Especifica graus de qualidade de impressão através da
   inclusão ou exclusão de características dos componentes, como traços, cores etc.
   Pode ser um dos seguintes valores:
   pqFullFeature - Todos os recursos gráficos serão reproduzidos;
   pqFixedOnly - Imprimir apenas bordas fixas. Também dispensa características gráficas especiais.
   @links TRLReport. :/}
  TRLPrintQuality=(pqFullFeature,pqFixedOnly);
                                            
  {@type TRLControlAnchorsType - Especifica como o componente será ancorado ao componente pai.
   Pode ser um dos seguintes valores:
   fkLeft - Ancorado à esquerda;
   fkTop - Ancorado ao topo;
   fkRight - Ancorado à direita;
   fkBottom - Ancorado à base.
   Nota: Quando dois lados opostos estão ancorados o componente é esticado conforme o
   componente pai é redimensionado, lembrando o alinhamento faClient. 
   @links TRLCustomControl. :/}
  TRLControlAnchorsType=(fkLeft,fkTop,fkRight,fkBottom);

  {@type TRLControlStateType - Descreve o estado atual de um componente do FortesReport.
   Um conjunto de estados pode indicar se um componente está sendo impresso, em pleno processo de alinhamento etc.
   Pode ser um dos seguintes valores:
   stPrinting - O componente está sendo impresso;
   stAligningControls - O componente está alinhando seus componentes filhos;
   stAdjustingHoldeds - O componente está ajustando os componentes dos quais ele é Holder;
   stAdjustingBounds - O componente está ajustando o seu tamanho de acordo com o seu conteúdo;
   stExpandingParent - O componente está ajustando o controle pai que é AutoSize e AutoExpand;
   stRestoringBounds - O componente está restaurando suas dimensões após a sua impressão;
   stMeasuringHeights - O componente está calculando as suas dimensões.
   @links TRLCustomControl.ControlState, TRLCustomControl.Holder, TRLCustomControl.AutoSize, TRLCustomControl.AutoExpand. :/}
  TRLControlStateType=(stPrinting,stAligningControls,stAdjustingHoldeds,stAdjustingBounds,stExpandingParent,stRestoringBounds,stMeasuringHeights);

  {@type TRLAutoSizeDirType - Especifica a direção para o redimensionamento automático.
   As props deste tipo são protegidas e inicializadas na criação pelo próprio componente. Este tipo determina a
   direção do redimensionamento automático para controles com prop AutoSize ou AutoExpand. Labels e caixas de
   texto de uma só linha são redimensionadas na horizontal. Memos e richtexts são redimensionados na vertical.
   Pode ser um dos seguintes valores:
   asWidthDir - Redimensionamento pela largura;
   asHeightDir - Redimensionamento pela altura.
   @links TRLCustomControl.AutoSize, TRLCustomControl.AutoExpand. :/}
  TRLAutoSizeDirType=(asWidthDir,asHeightDir);
  
  {@type TRLControlBehaviorType - Especifica o comportamento do controle sob circunstâncias diversas.
   Esta é uma prop de uso genérico e foi concebida para condensar diversas props do tipo boolean.
   Pode ser um dos seguintes valores:
   beSiteExpander - O componente deve expandir o componente pai sempre que houver redimensionamento. O componente
   pai deve ter a prop AutoExpand ligada.
   @links TRLCustomControl.AutoSize, TRLCustomControl.AutoExpand. :/}
  TRLControlBehaviorType=(beSiteExpander);

  {@type TRLFooterMeasuring - Especifica o momento em que uma band deve efetuar o cálculo de espaço.
   O cálculo para saber se bands do tipo btFooter ou btSummary cabem na página pode ser realizado em
   diversos momento. A escolha do momento ideal pode resultar num relatório de melhor qualidade.
   Pode ser um dos seguintes valores:
   fmNone - Nenhuma antecipação é feita. A margem inferior para rodapés é calculada no início do relatório
   e serve como referência a partir daí;
   fmAfterHeader - A margem inferior é recalculada em todas as páginas logo após a impressão dos cabeçalhos;
   fmBeforeDetail - A margem á calculada antes da impressão de cada band de detalhe.
   Nota: Quanto mais frequente o cálculo, mais preciso será o resultado, especialmente se houver muitas
   alterações de tamanho ou visibilidade nas bands de rodapé. Porém, isso também pode significar perda de
   performance.
   @links TRLCustomBand.BandType. :/}
  TRLFooterMeasuring=(fmNone,fmAfterHeader,fmBeforeDetail);

  // SETS

  {@type TRLControlAnchors - Especifica como o componente será ancorado ao componente pai.
   Este tipo é um conjunto dos lados do componente pai aos quais o componente em questão será ancorado.
   Nota: Quando dois lados opostos estão ancorados o componente é esticado conforme o
   componente pai é redimensionado, lembrando o alinhamento faClient. 
   @links TRLControlAnchorsType, TRLCustomControl.Anchors. :/}
  TRLControlAnchors=set of TRLControlAnchorsType;

/// revisar

  {@type TRLAllowedBands - Tipos de band inseridos.
   Determina que tipos de band inicialmente serão inseridos sobre o pager.
   Nota: Este recurso é mantido para fins de compatibilidade, pois o FortesReport permite mais de uma band
   do mesmo tipo por relatório.
   @links TRLBandType, TRLCustomPager.AllowedBands. :/}
  TRLAllowedBands=set of TRLBandType;

  {@type TRLControlState - Status do controle.
   Indica o estado atual do controle.
   @links TRLControlStateType. :/}
  TRLControlState=set of TRLControlStateType;
  
  {@type TRLControlBehavior - Comportamento do controle.
   Determina características de comportamento do controle.
   @links TRLControlBehaviorType, TRLCustomControl.Behavior. :/}
  TRLControlBehavior=set of TRLControlBehaviorType;

  {@type TRLAutoSizeDirSet - Direções de redimensionamento.
   Determina as direções do redimensionamento automático.
   @links TRLAutoSizeDirType, TRLCustomControl.AutoSizeDir, TRLCustomControl.ExpandParentSite. :/}
  TRLAutoSizeDirSet=set of TRLAutoSizeDirType;

  // OBJECT PROPERTIES

  {@type TRLBorderSides - Configuração rápida de bordas.
   Pode ser um dos seguintes valores:
   sdCustom - As bordas devem ser indicadas pela propriedade Borders;
   sdNone - O controle não deve exibir bordas;
   sdAll - Todas as bordas acionadas.
   @links TRLBorders. :/}
  TRLBorderSides=(sdCustom,sdNone,sdAll);

  {@class TRLBorders - Propriedades para as bordas de um TRLCustomControl.
   Determina que lados serão desenhados, a largura, estilo, cor e espessura das linhas. Possui propriedade ParentControl
   para determinar o controle onde se deve desenhar. Possui método AdjustParent que chama o AdjustBounds do ParentControl
   sempre que forem alteradas as propriedades que afetam o tamanho. Invoca o Invalidate do ParentControl sempre que houver
   alteração na cor e estilo. Este objeto não é responsável pelo Paint do ParentControl, este é que deve faze-lo no seu
   Paint de acordo com as propriedades de borda.
   @links TRLCustomControl.Borders. }
  TRLBorders=class(TPersistent)
  private

    // variables

    fParentControl:TRLCustomControl;
    fDrawLeft     :boolean;
    fDrawTop      :boolean;
    fDrawRight    :boolean;
    fDrawBottom   :boolean;
    fWidth        :integer;
    fColor        :TColor;
    fStyle        :TBrushStyle;
    fSides        :TRLBorderSides;
    fFixedLeft    :boolean;
    fFixedTop     :boolean;
    fFixedRight   :boolean;
    fFixedBottom  :boolean;

    // assign methods

    procedure   SetSides(const aValue:TRLBorderSides);
    procedure   SetDrawLeft(const aValue:boolean);
    procedure   SetDrawTop(const aValue:boolean);
    procedure   SetDrawRight(const aValue:boolean);
    procedure   SetDrawBottom(const aValue:boolean);
    procedure   SetWidth(const aValue:integer);
    procedure   SetColor(const aValue:TColor);
    procedure   SetStyle(const aValue:TBrushStyle);
    procedure   SetParentControl(const aValue:TRLCustomControl);
    procedure   SetFixedLeft(const aValue:boolean);
    procedure   SetFixedTop(const aValue:boolean);
    procedure   SetFixedRight(const aValue:boolean);
    procedure   SetFixedBottom(const aValue:boolean);

    // custom methods

    procedure   AdjustParent;
    procedure   CheckSides;
    function    IsCustom:boolean;

  public

    // constructors & destructors
    
    constructor Create(aOwner:TRLCustomControl);

    // custom methods
    
    {@method PaintTo - Desenha as bordas em um canvas delimitado por um retângulo. :}
    procedure   PaintTo(aCanvas:TCanvas; aRect:TRect); overload;
    procedure   PaintTo(aSurface:TRLGraphicSurface; aRect:TRect); overload;
    {/@method}

    {@method CanDrawLeft - Indica se é pérmitido desenhar a borda esquerda. :/}
    function    CanDrawLeft:boolean;
    
    {@method CanDrawTop - Indica se é pérmitido desenhar a borda superior. :/}
    function    CanDrawTop:boolean;
    
    {@method CanDrawRight - Indica se é pérmitido desenhar a borda direita. :/}
    function    CanDrawRight:boolean;
    
    {@method CanDrawBottom - Indica se é pérmitido desenhar a borda inferior. :/}
    function    CanDrawBottom:boolean;

    // custom properties
    
    {@prop ParentControl - Controle sobre o qual as bordas serão desenhadas.
     @links TRLCustomControl. :/}
    property    ParentControl:TRLCustomControl read fParentControl write SetParentControl;

  published
  
    // custom properties
    
    {@prop Sides - Configuração instantânea das bordas.
     @links TRLBorderSides. :/}
    property    Sides        :TRLBorderSides   read fSides         write SetSides       default sdNone;

    {@prop DrawLeft - Desenhar borda esquerda. :/}
    property    DrawLeft     :boolean          read fDrawLeft      write SetDrawLeft    stored IsCustom;

    {@prop DrawTop - Desenhar borda superior. :/}
    property    DrawTop      :boolean          read fDrawTop       write SetDrawTop     stored IsCustom;

    {@prop DrawRight - Desenhar borda direita. :/}
    property    DrawRight    :boolean          read fDrawRight     write SetDrawRight   stored IsCustom;

    {@prop DrawBottom - Desenhar borda inferior. :/}
    property    DrawBottom   :boolean          read fDrawBottom    write SetDrawBottom  stored IsCustom;

    {@prop Width - Largura da borda. :/}
    property    Width        :integer          read fWidth         write SetWidth       default 1;
    
    {@prop Color - Cor da borda. :/}
    property    Color        :TColor           read fColor         write SetColor       default clBlack;
    
    {@prop Style - Estilo da borda. :/}
    property    Style        :TBrushStyle      read fStyle         write SetStyle       default bsSolid;
    
    {@prop FixedLeft - Desenhar borda esquerda fixa. :/}
    property    FixedLeft    :boolean          read fFixedLeft     write SetFixedLeft   default False;

    {@prop FixedTop - Desenhar borda superior fixa. :/}
    property    FixedTop     :boolean          read fFixedTop      write SetFixedTop    default False;
    
    {@prop FixedRight - Desenhar borda direita fixa. :/}
    property    FixedRight   :boolean          read fFixedRight    write SetFixedRight  default False;
    
    {@prop FixedBottom - Desenhar borda inferior fixa. :/}
    property    FixedBottom  :boolean          read fFixedBottom   write SetFixedBottom default False;
  end;
  {/@class}
  

  {@class TRLMargins - Propriedades para as margens internas de alinhamento de um CustomPanel.
   Determina largura das margens: superior, inferior e laterais em MM. Possui método AdjustParent que chama o AdjustBounds
   do ParentControl sempre que forem alteradas as propriedades que afetam o tamanho.
   @links TRLCustomSite.Margins, TRLCustomSite.InsideMargins. }
  TRLMargins=class(TPersistent)
  private

    // variables

    fParentControl      :TRLCustomControl;
    fLeftMargin         :double;
    fTopMargin          :double;
    fRightMargin        :double;
    fBottomMargin       :double;
    fDefaultLeftMargin  :double;
    fDefaultTopMargin   :double;
    fDefaultRightMargin :double;
    fDefaultBottomMargin:double;

    // assign methods

    procedure   SetLeftMargin(const aValue:double);
    procedure   SetRightMargin(const aValue:double);
    procedure   SetTopMargin(const aValue:double);
    procedure   SetBottomMargin(const aValue:double);
    //
    procedure   ReadLeftMargin(Reader:TReader);
    procedure   WriteLeftMargin(Writer:TWriter);
    procedure   ReadTopMargin(Reader:TReader);
    procedure   WriteTopMargin(Writer:TWriter);
    procedure   ReadRightMargin(Reader:TReader);
    procedure   WriteRightMargin(Writer:TWriter);
    procedure   ReadBottomMargin(Reader:TReader);
    procedure   WriteBottomMargin(Writer:TWriter);

    // custom methods

    procedure   AdjustParent;

  protected

    // override

    procedure   DefineProperties(Filer:TFiler); override;

    // custom methods

    procedure   SetDefaults(aLeft,aTop,aRight,aBottom:double);

  public

    // constructors & destructors

    constructor Create(aOwner:TRLCustomControl);

    // custom properties

    {@prop ParentControl - Referência ao controle.
     @links TRLCustomControl. :/}
    property    ParentControl:TRLCustomControl read fParentControl write fParentControl;

    // override

    procedure   Assign(Source:TPersistent); override;

  published

    // custom properties

    {@prop LeftMargin - Margem esquerda em milímetros. :/}
    property    LeftMargin   :double        read fLeftMargin   write SetLeftMargin stored False;

    {@prop TopMargin - Margem superior em milímetros. :/}
    property    TopMargin    :double        read fTopMargin    write SetTopMargin stored False;

    {@prop RightMargin - Margem direita em milímetros. :/}
    property    RightMargin  :double        read fRightMargin  write SetRightMargin stored False;

    {@prop BottomMargin - Margem inferior em milímetros. :/}
    property    BottomMargin :double        read fBottomMargin write SetBottomMargin stored False;
  end;
  {/@class}
  

  {@class TRLPageSetup - Propriedades para configuração de página.
                         Determina a largura e altura do papel em MM, o tipo de papel utilizado e a orientação.
   @links TRLCustomReport.PageSetup. }
  TRLPageSetup=class(TPersistent)
  private

    // variables

    fParentReport  :TRLCustomReport;
    fPaperHeight   :double;
    fPaperWidth    :double;
    fPaperSize     :TRLPaperSize;
    fOrientation   :TRLPageOrientation;
    fForceEmulation:boolean;

    // assign methods

    function    GetOrientedWidth:double;
    function    GetOrientedHeight:double;
    procedure   SetOrientedHeight(const aValue:double);
    procedure   SetOrientedWidth(const aValue:double);
    procedure   SetPaperSize(const aValue:TRLPaperSize);
    procedure   SetPaperHeight(const aValue:double);
    procedure   SetPaperWidth(const aValue:double);
    procedure   SetOrientation(const aValue:TRLPageOrientation);

    // custom methods

    procedure   AdjustParent;
    function    IsCustomPaperSize:boolean;

  public

    // constructors & destructors

    constructor Create(aOwner:TRLCustomReport);

    // custom methods

    {@method Assign - Inicializa propriedades a partir de um outro objeto. :/}
    procedure   Assign(Source:TRLPageSetup); reintroduce;
    
    {@prop ParentReport - Referência ao objeto relatório.
     @links TRLCustomReport. :/}
    property    ParentReport  :TRLCustomReport    read fParentReport     write fParentReport;

    {@prop OrientedWidth - Largura orientada do papel em milímetros. :/}
    property    OrientedWidth :double             read GetOrientedWidth  write SetOrientedWidth;

    {@prop OrientedHeight - Altura orientada do papel em milímetros. :/}
    property    OrientedHeight:double             read GetOrientedHeight write SetOrientedHeight;
    
  published

    // custom properties
    
    {@prop PaperSize - Tamanho do papel.
     @links TRLPaperSize. :/}
    property    PaperSize     :TRLPaperSize       read fPaperSize        write SetPaperSize    default fpA4;
    
    {@prop Orientation - Orientação do papel.
     @links TRLPageOrientation. :/}
    property    Orientation   :TRLPageOrientation read fOrientation      write SetOrientation  default poPortrait;
    
    {@prop PaperWidth - Largura do papel em milímetros. :/}
    property    PaperWidth    :double             read fPaperWidth       write SetPaperWidth   stored IsCustomPaperSize;
    
    {@prop PaperHeight - Altura do papel em milímetros. :/}
    property    PaperHeight   :double             read fPaperHeight      write SetPaperHeight  stored IsCustomPaperSize;
    
    {@prop ForceEmulation - Emulação forçada. :/}
    property    ForceEmulation:boolean            read fForceEmulation   write fForceEmulation default False;
  end;
  {/@class}
  

  {@type TRLRealBoundsUnit - Unidades de medida para o dimensionamento real de um controle.
                             Nota: Esta funcionalizade não está implementada. :/}
  TRLRealBoundsUnit=(buNone,buMilimeters,buInches);

  {@class TRLRealBounds - Configuração do tamanho real de um controle em milímetros ou polegadas.
                          Nota: Esta funcionalizade não está implementada.
   @links TRLCustomControl.RealBounds. }
  TRLRealBounds=class(TPersistent)
  private

    // variables

    fParentControl:TRLCustomControl;
    fUsedUnit     :TRLRealBoundsUnit;
    fLeft         :double;
    fTop          :double;
    fHeight       :double;
    fWidth        :double;

    // assign methods

    procedure   SetLeft(const aValue:double);
    procedure   SetTop(const aValue:double);
    procedure   SetHeight(const aValue:double);
    procedure   SetWidth(const aValue:double);
    procedure   SetUsedUnit(const aValue:TRLRealBoundsUnit);

    // custom methods

    procedure   AdjustParent;
    
  public

    // constructors & destructors
    
    constructor Create(aOwner:TRLCustomControl);

    // custom methods

    {@prop ParentReport - Referência ao controle.
     @links TRLCustomControl. :/}
    property    ParentControl:TRLCustomControl  read fParentControl write fParentControl;

  published

    // custom properties
    
    {@prop UsedUnit - Unidade de medida utilizada.
     @links TRLRealBoundsUnit. :/}
    property    UsedUnit     :TRLRealBoundsUnit read fUsedUnit      write SetUsedUnit default buNone;
    
    {@prop Left - Coordenada esquerda em milímetros. :/}
    property    Left         :double            read fLeft          write SetLeft;
         
    {@prop Top - Coordenada superior em milímetros. :/}
    property    Top          :double            read fTop           write SetTop;
          
    {@prop Width - Largura em milímetros. :/}
    property    Width        :double            read fWidth         write SetWidth;
        
    {@prop Height - Altura em milímetros. :/}
    property    Height       :double            read fHeight        write SetHeight;
  end;
  {/@class}
  

  {@class TRLBackground - Propriedades para uma figura a ser desenhada no fundo de um site.
                          Determina o posicionamento ou forma de distribuicao, e o tamanho da figura no parentsite.
   @links TRLCustomSite.Background, TRLDegradeEffect. }
  TRLBackground=class(TPersistent)
  private

    // variables

    fParentSite :TRLCustomSite;
    fAlign      :TRLControlAlign;
    fArrange    :TRLImageArrange;
    fAutoSize   :boolean;
    fHeight     :integer;
    fPicture    :TPicture;
    fStretch    :boolean;
    fWidth      :integer;

    // assign methods

    procedure   SetAlign(const aValue:TRLControlAlign);
    procedure   SetArrange(const aValue:TRLImageArrange);
    procedure   SetAutoSize(const aValue:boolean);
    procedure   SetHeight(const aValue:integer);
    procedure   SetPicture(const aValue:TPicture);
    procedure   SetStretch(const aValue:boolean);
    procedure   SetWidth(const aValue:integer);

  public

    // constructors & destructors

    constructor Create(aOwner:TRLCustomSite);
    destructor  Destroy; override;

    // custom methods
    
    {@method PaintTo - Desenha em outra superfície. :}
    procedure   PaintTo(aCanvas:TCanvas; aRect:TRect); overload;
    procedure   PaintTo(aSurface:TRLGraphicSurface; aRect:TRect); overload;
    {/@method}

    {@method AdjustSize - Ajusta tamanho de acordo com a imagem. :/}
    procedure   AdjustSize;

    // custom properties
    
    {@prop ParentSite - Referência ao site sobre o qual o fundo será desenhado.
     @links TRLCustomSite. :/}
    property    ParentSite :TRLCustomSite   read fParentSite write fParentSite;
    
  published

    // custom properties
    
    {@prop Align - Alinhamento da imagem.
     @links TRLControlAlign. :/}
    property    Align      :TRLControlAlign read fAlign      write SetAlign    default faClient;
    
    {@prop Arrange - Arranjo da imagem.
     @links TRLImageArrange. :/}
    property    Arrange    :TRLImageArrange read fArrange    write SetArrange  default baAligned;

    {@prop AutoSize - Redimensionamento automático da imagem. :/}
    property    AutoSize   :boolean         read fAutoSize   write SetAutoSize default True;

    {@prop Height - Altura da imagem. :/}
    property    Height     :integer         read fHeight     write SetHeight   default 40;
    
    {@prop Stretch - Esticamento da imagem. :/}
    property    Stretch    :boolean         read fStretch    write SetStretch  default False;
    
    {@prop Width - Largura da imagem. :/}
    property    Width      :integer         read fWidth      write SetWidth    default 40;
    
    {@prop Picture - Imagem de fundo. :/}
    property    Picture    :TPicture        read fPicture    write SetPicture;
  end;
  {/@class}
  

  {@class TRLDegradeEffect - Efeito de transição de cores no fundo de um site.
                             Determina as cores origem e destino e a direção do efeito.
   @links TRLCustomSite.Degrade, TRLBackground. }
  TRLDegradeEffect=class(TPersistent)
  private

    // variables

    fParentSite   :TRLCustomSite;
    fOppositeColor:TColor;
    fDirection    :TRLDegradeDirection;
    fGranularity  :integer;

    // assign methods

    procedure   SetDirection(const aValue:TRLDegradeDirection);
    procedure   SetOppositeColor(const aValue:TColor);
    procedure   SetGranularity(const aValue:integer);

    // custom methods
                        
    procedure   PaintTo(aCanvas:TCanvas; aRect:TRect; aColor:TColor); overload;
    procedure   PaintTo(aSurface:TRLGraphicSurface; aRect:TRect; aColor:TColor); overload;
    
  public

    // constructors & destructors

    constructor Create(aOwner:TRLCustomSite);

    {@prop ParentSite - Referência ao site sobre o qual o efeito será desenhado.
     @links TRLCustomSite. :/}
    property    ParentSite   :TRLCustomSite       read fParentSite;

  published

    // custom properties

    {@prop Direction - Direção do efeito.
     @links TRLDegradeDirection. :/}
    property    Direction    :TRLDegradeDirection read fDirection     write SetDirection     default ddNone;
    
    {@prop OppositeColor - Cor oposta. :/}
    property    OppositeColor:TColor              read fOppositeColor write SetOppositeColor default clBlack;
    
    {@prop Granularity - Distância entre os tons do efeito. :/}
    property    Granularity  :integer             read fGranularity   write SetGranularity   default 1;
  end;
  {/@class}
  

  {@type TRLSortedBandTypes - Tipos das bands sortedadas.
   @links TRLSortedBands. :}
  TRLSortedBandTypes=array[btHeader..btFooter] of record
    List   :TList;
    Printed:boolean;
  end;
  {/@type}

  {@class TRLSortedBands - Propriedades para atribuição de Bands a CustomSkippers.
                           Determina as Bands incluidas pelos seus tipos bem como controla os tipos de Bands já
                           impressos no ParentSkipper.
   @links TRLCustomPager.SortedBands, TRLSortedBandTypes. }
  TRLSortedBands=class(TPersistent)
  private

    // variables

    fTypes:TRLSortedBandTypes;

    // assign methods

    function    GetList(aType:TRLBandType):TList;
    function    GetPrinted(aType:TRLBandType):boolean;
    procedure   SetPrinted(aType:TRLBandType; aValue:boolean);
    
  public

    // constructors & destructors
    
    constructor Create;
    destructor  Destroy; override;

    // custom methods

    {@method Add - Adiciona banda ou controle semelhante.
     @links TRLCustomSite. :/}
    procedure   Add(aBand:TRLCustomSite);
    
    {@method Clear - Limpa a lista. :/}
    procedure   Clear;
    
    {@method ResetPage - Reseta os flags de impresso para bands não title. :/}
    procedure   ResetPage;
    
    {@method ResetAll - Reseta os flags de impresso para todas as bands. :/}
    procedure   ResetAll;
    
    // custom properties
    
    {@prop List - Referência para lista de bands do tipo informado.
     @links TRLBandType. :/}
    property    List[aType:TRLBandType]:TList      read GetList;
    
    {@prop Printed - Flag de impresso para bands do tipo informado.
     @links TRLBandType. :/}
    property    Printed[aType:TRLBandType]:boolean read GetPrinted write SetPrinted;
  end;
  {/@class}
  

  { TRLPreviewOptions }

  {@type TRLPreviewOptionsDefaults - Uso dos defaults no preview padrão.
   Pode ser um dos seguintes valores:
   pdUseDefaults - Utilizar as mesmas opções deixadas pelo último preview;
   pdIgnoreDefaults - Utilizar as opções definidas na prop PreviewOptions.
   @links TRLPreviewOptions. :/}
  TRLPreviewOptionsDefaults=(pdUseDefaults,pdIgnoreDefaults);

  {@class TRLPreviewOptions - Opções do form de preview padrão para um componente TRLReport em particular.
   @links TRLCustomReport.PreviewOptions, TRLPreviewOptionsDefaults. }
  TRLPreviewOptions=class(TPersistent)
  private

    // variables

    fParentReport:TRLCustomReport;
    fDefaults    :TRLPreviewOptionsDefaults;
    fShowModal   :boolean;
    fFormStyle   :TFormStyle;
    fPosition    :TPosition;
    fWindowState :TWindowState;
    fBorderIcons :TBorderIcons;
    fHelpFile    :string;
    fHelpContext :integer;
    fCaption     :TCaption;

    // assign methods

    function    IsCaption:boolean;
    
  public

    // constructors & destructors
    
    constructor Create(aOwner:TRLCustomReport);

    {@method Assign - Inicializa propriedades a partir de um outro objeto. :/}
    procedure   Assign(Source:TRLPreviewOptions); reintroduce;

    // custom properties
    
    {@prop ParentReport - Referência ao report.
     @links TRLCustomReport. :/}
    property    ParentReport:TRLCustomReport read fParentReport write fParentReport;
    
  published

    // custom properties
    
    {@prop WindowState - Indica o estado inicial da janela de preview. :/}
    property    WindowState:TWindowState              read fWindowState write fWindowState default wsMaximized;
    
    {@prop Position - Indica a posição da janela de preview. :/}
    property    Position   :TPosition                 read fPosition    write fPosition    default poScreenCenter;
    
    {@prop FormStyle - Indica o estilo da janela de preview. :/}
    property    FormStyle  :TFormStyle                read fFormStyle   write fFormStyle   default fsNormal;
    
    {@prop ShowModal - Indica se a janela de preview será modal. :/}
    property    ShowModal  :boolean                   read fShowModal   write fShowModal   default False;
    
    {@prop BorderIcons - Seleciona os botões da janela de preview. :/}
    property    BorderIcons:TBorderIcons              read fBorderIcons write fBorderIcons default [biSystemMenu,biMinimize,biMaximize];
    
    {@prop HelpFile - Nome do arquivo de help para a janela preview, se houver. :/}
    property    HelpFile   :string                    read fHelpFile    write fHelpFile;
    
    {@prop HelpContext - Contexto de help para a janela preview, se houver. :/}
    property    HelpContext:integer                   read fHelpContext write fHelpContext default 0;
    
    {@prop Caption - Título da janela de preview. :/}
    property    Caption    :TCaption                  read fCaption     write fCaption     stored  IsCaption;
    
    {@prop Defaults - Indica como estas configurações serão utilizadas pelo form de preview.
     @links TRLPreviewOptionsDefaults. :/}
    property    Defaults   :TRLPreviewOptionsDefaults read fDefaults    write fDefaults    default pdUseDefaults;
  end;
  {/@class}
  

  // CUSTOM Components

  { TRLCustomControl }

  {@class TRLCustomControl - Super classe da qual derivam todos os controles do FortesReport.
   @ancestor TCustomControl. }
  TRLCustomControl=class(TCustomControl)
  private

    // variables

    fPreparingCaption  :TCaption;

    // property variables
    
    fAfterPrint        :TRLAfterPrintEvent;
    fAlign             :TRLControlAlign;
    fAlignment         :TRLTextAlignment;
    fAutoSize          :boolean;
    fAutoSizeDir       :TRLAutoSizeDirSet;
    fAutoExpand        :boolean;
    fAutoTrunc         :boolean;
    fAnchors           :TRLControlAnchors;
    fBorders           :TRLBorders;
    fHolder            :TRLCustomControl;
    fHoldStyle         :TRLHoldStyle;
    fHolderOffset      :TPoint;
    fSecondHolder      :TRLCustomControl;
    fSecondHoldStyle   :TRLHoldStyle;
    fSecondHolderOffset:TPoint;
    fHoldeds           :TList;
    fPeekBoundsRect    :TRect;
    fRealBounds        :TRLRealBounds;
    fCaption           :TCaption;
    fLayout            :TRLTextLayout;
    fControlState      :TRLControlState;
    fBehavior          :TRLControlBehavior;
    fTransparent       :boolean;
    fOldBoundsRect     :TRect;
    fFixedSize         :TPoint;
    fSizeFixed         :boolean;
    fFriendlyName      :string;
    fCouldPrint        :boolean;
    fDefaultBehavior   :TRLControlBehavior;
    fOnMeasureHeight   :TNotifyEvent;
    parentHwnd         :HWND;

    // internal state variables
    fLocker:TCriticalSection;

    // assign methods

    procedure   SetAlign(const aValue:TRLControlAlign);
    procedure   SetAnchors(const aValue:TRLControlAnchors);
    procedure   SetAutoExpand(const aValue:boolean);
    procedure   SetHolder(const aValue:TRLCustomControl);
    procedure   SetHoldStyle(const aValue:TRLHoldStyle); 
    procedure   SetSecondHolder(const aValue:TRLCustomControl);
    procedure   SetSecondHoldStyle(const aValue:TRLHoldStyle);
    procedure   SetTransparent(const aValue:boolean);
    procedure   SetCaption(const aValue:TCaption); 
    procedure   SetAlignment(const aValue:TRLTextAlignment);
    procedure   SetAutoTrunc(const aValue:boolean);
    procedure   SetLayout(const aValue:TRLTextLayout);
    procedure   SetBorders(const aValue:TRLBorders);
    procedure   SetRealBounds(const aValue:TRLRealBounds);
    procedure   SetClientHeight(const Value:integer);
    procedure   SetClientWidth(const Value:integer);
    procedure   SetClientSize(const Value:TPoint);
    procedure   SetFriendlyName(const Value:string);
    function    GetDefaultCaption:TCaption;

    // custom methods

    function    IsFriendlyName:boolean;
    function    IsCaption:boolean;
    function    IsBehavior:boolean;

    procedure   SafeSetBoundsMethod;
    procedure   SafeSetBounds(ALeft,ATop,AWidth,AHeight:Integer);

  protected

    // property variables

    fBeforeText        :TRLBeforeTextEvent;
    fBeforePrint       :TRLBeforePrintEvent;

    {@method PrintAsCustomControl - Imprimir como TRLCustomControl. :/}
    procedure   PrintAsCustomControl;

    {@method PaintAsCustomControl - Desenha como TRLCustomControl. :/}
    procedure   PaintAsCustomControl;

    // override & reintroduce

    procedure   Notification(aComponent: TComponent; Operation: TOperation); override;
    procedure   RequestAlign; override;
    function    GetClientRect:TRect; override;
    procedure   SetName(const Value:TComponentName); override;
    procedure   WMMOUSEMOVE (var Msg: TLMessage); message LM_MOUSEMOVE;
    procedure   SetParent(aParent:TWinControl); override;
    procedure   InternalPaintFinish; dynamic;
    procedure   InternalPaint; dynamic;
    procedure   Paint; override;

    {@method SetAutoSize - SetAutoSize estendido. :/}
    procedure   SetAutoSize(const aValue:boolean); reintroduce;

    // assign methods
    
    {@method GetCaption - Retorna o caption dependendo do estado do relatório. :/}
    function    GetCaption:TCaption;
                                                  
    {@method GetMasterReport - Retorna referência ao relatório principal da cadeia após busca recursiva através das props Parent e PriorReport.
     Se não encontrar, retorna nil.
     @links TRLCustomReport, TRLCustomReport.NextReport, TRLCustomReport.PriorReport. :/}
    function    GetMasterReport:TRLCustomReport;

    {@method GetClientHeight - Retorna a altura da área cliente.
     @links ClientRect. :/}
    function    GetClientHeight:integer;

    {@method GetClientWidth - Retorna a largura da área cliente.
     @links ClientRect. :/}
    function    GetClientWidth:integer;

    // static methods
    
    {@method AdjustToParentFrame - Ajusta as dimensões do controle pai, se este for um TFrame. :/}
    procedure   AdjustToParentFrame(var aLeft,aTop,aWidth,aHeight:integer);

    {@method AdjustToFixedSize - Ajusta coordenadas de acordo com as dimensões estabelecidas no método CalcSize.
     @links CalcSize. :/}
    procedure   AdjustToFixedSize(var aLeft,aTop,aWidth,aHeight:integer);

    {@method AdjustToHolder - Ajusta coordenadas de acordo com o holder.
     @links TRLCustomControl, Holder, SecondHolder. :/}
    procedure   AdjustToHolder(aHolder:TRLCustomControl; var aLeft,aTop,aWidth,aHeight:integer);

    {@method CanSetWidth - Indica se é possível para o usuário determinar uma largura aleatória para o controle.
     Em determinadas circunstâncias dependendo do alinhamento, autosize ou holder, não é possível modificar as dimensões.
     @links AutoSize, Align, Holder. :/}
    function    CanSetWidth:boolean;
    
    {@method CanSetHeight - Indica se é possível para o usuário determinar uma altura aleatória para o controle.
     Em determinadas circunstâncias dependendo do alinhamento, autosize ou holder, não é possível modificar as dimensões.
     @links AutoSize, Align, Holder. :/}
    function    CanSetHeight:boolean;

    {@method ExpandParentSite - Ajusta as dimensões do controle pai. :/}
    procedure   ExpandParentSite;

    {@method AdjustAlignment - Ajusta as dimensões do controle respeitando o seu alinhamento. :/}
    procedure   AdjustAlignment(var aRect:TRect);

    {@method DoAfterPrint - Invoca o evento AfterPrint.
                            Não utilize o método diretamente. Ele invoca o evento AfterPrint do controle após a sua impressão. :/}
    procedure   DoAfterPrint;

    {@method DoBeforePrint - Invoca o evento BeforePrint. Não utilize o método diretamente. Ele invoca
    o evento BeforePrint do controle antes da sua impressão. :/}
    procedure   DoBeforePrint;

    {@method DoBeforeText - Invoca o evento BeforePrint.
                            Não utilize o método diretamente. Ele invoca o evento BeforePrint do controle antes da sua impressão. :/}
    procedure   DoBeforeText(var aText:string; var aPrintIt:boolean);

    {@method DoOnMeasureHeight - Invoca o evento OnMeasureHeight.
                                 Não utilize o método diretamente. Ele invoca o evento OnMeasureHeight do controle na horas das medições de página. :/}
    procedure   DoOnMeasureHeight;

    {@method GetMadeCaption - Produz e retorna o Caption.
     @links MakeCaption. :/}
    function    GetMadeCaption:string;

    {@method MakeCaption - Produz o Caption.
     @links GetMadeCaption. :/}
    procedure   MakeCaption;

    {@method AdjustBounds - Ajusta coordenadas e tamanho. :/}
    procedure   AdjustBounds;

    {@method RealignHoldeds - Ajusta cotroles "agarrados".
     @links Hold, Holdeds. :/}
    procedure   RealignHoldeds;

    {@method Hold - Agarra controle. :/}
    procedure   Hold(aControl:TRLCustomControl; aPlace:integer);

    {@method Unhold - Libera controle agarrado. :/}
    procedure   Unhold(aControl:TRLCustomControl);
    procedure   CheckParent(var aControl:TWinControl);

    {@method IsBallast - O controle está sendo impresso como um lastro.
                         Quando o parentpager está imprimindo bands em branco para preencher o espaço da página, ou
                         quando o parentskipper foi instruído a saltar um registro, o controle é dito lastro. :/}
    function    IsBallast:boolean;

    // dynamic methods
    
    {@method CanPrint - Intervenção antes da impressão.
     Não utilize CanPrint diretamente. Este método é disparado automaticamente pelo painel sempre antes de
     sua impressão. Este método invoca o evento BeforePrint, dentro do qual se pode mudar características
     do painel como: tamanho, cor, etc., além de decidir se ele será impresso ou não.
     Nota: Paineis não visíveis ou desabilitados não dispararão este método.
     O tamanho do painel será restaurado automaticamente após a sua impressão. :/}
    function    CanPrint:boolean; dynamic;

    {@method CalcSize - Cacula o tamanho do controle. :/}
    procedure   CalcSize(var aSize:TPoint); dynamic;

    {@method DrawBounds - Desenha bordas.
     Não utilize este método diretamente. Ele é disparado automaticamente para que sejam impressas as bordas
     ao redor do panel. :/}
    procedure   DrawBounds; dynamic;
    
    {@method CalcWastedPixels - Margens dispensadas do controle. :/}
    function    CalcWastedPixels:TRect; dynamic;

    {@method CalcPrintClientRect - Retângulo com coordenadas relativas à linha corrente da página. :/}
    function    CalcPrintClientRect:TRect; dynamic;

    {@method CalcPrintBoundsRect - Retângulo com coordenadas relativas ao parentreport. :/}
    function    CalcPrintBoundsRect:TRect; dynamic;
    
    {@method CalcPrintSizeRect - Cacula o tamanho do controle para fins de impressão. :/}
    function    CalcPrintSizeRect:TRect; dynamic;

    {@method CalcSizeRect - Cacula o tamanho do controle. :/}
    function    CalcSizeRect:TRect; dynamic;
    
    {@method SetClientRect - Estabelece as dimensões do controle descontando margens etc. :/}
    procedure   SetClientRect(const aValue:TRect); virtual;
    
    {@method InternalMakeCaption - Produz Caption. :/}
    function    InternalMakeCaption:string; dynamic;

    {@method Initialize - Inicializa os acumuladores internos.
     Inicializa os acumuladores internos do controle em questão e de seus controles filhos.
     Estes acumuladores podem ser contadores de registros, totalizadores de campos numéricos
     e informações de estatística. :/}
    procedure   Initialize; dynamic;

    {@method ComputeDetail - Computar novo Detail.
     Não utilize este método diretamente. Ele é invocado sempre que uma band de detalhe é impressa para
     que controles de contabilidade e estatística possam computar seus valores. O controle repassa a chamada
     para seus controles filhos em cascata. :/}
    procedure   ComputeDetail(aCaller:TObject); dynamic;
    
    {@method InternalPrint - Processa o controle para impressão. :/}
    procedure   InternalPrint; dynamic;
    
    {@method RealignControls - Realinha os controles dentro deste de acordo com suas props. :/}
    procedure   RealignControls; dynamic;

    {@method InternalMeasureHeight - Mede a altura da band de acordo com seu conteúdo, tentando predizer as
     quebras de folha que virão. :/}
    procedure   InternalMeasureHeight; dynamic;

    {@method GetAttribute - Devolve o valor do controle como um variant.
     Este valor é arbitrário e depende da classe que implementa o método. :/}
    function    GetAttribute(const aName:string):variant; virtual;

    {@method SetAttribute - Modifica o valor do controle. :/}
    function    SetAttribute(const aName:string; aValue:variant):boolean; virtual;

    procedure   CMColorChanged(var Message:TLMessage); message CM_COLORCHANGED;
    procedure   CMFontChanged(var Message:TLMessage); message CM_FONTCHANGED;

    {@method PrepareStatics - Prepara os controles filhos do painel antes de imprimí-los.
     Esta operação consiste em invocar os eventos BeforePrint de cada controle, dando oportunidade para o
     redimensionamento antes de renderizar todos os controles. :/}
    procedure   PrepareStatics;

    {@method PrintStatics - Desenha os controles filhos do painel sobre a sua superfície. :/}
    procedure   PrintStatics;

    {@method PrintNonStatics - Força a impressão de controles não estáticos como subdetalhes e grupos. :/}
    procedure   PrintNonStatics;

    property    CouldPrint:boolean read fCouldPrint write fCouldPrint;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // override methods

    procedure   SetBounds(aLeft,aTop,aWidth,aHeight:integer); override;

    // static methods

    function    IsPreparing:boolean;

    {@method IsMeasurable - Indica se o controle pode sofrer predicção sobre a sua altura. :/}
    function    IsMeasurable:boolean;
    
    {@method MeasureHeight - Mede a altura do controle. :/}
    procedure   MeasureHeight;
    
    {@method PushBoundsRect - Guarda as dimensões do controle. :/}
    procedure   PushBoundsRect;
    
    {@method PopBoundsRect - Restaura as dimensões do controle. :/}
    procedure   PopBoundsRect;
    
    {@method Print - Gera imagem do controle para impressão.
     Gera imagem do controle junto com seus controles filhos e dispara os eventos BeforePrint e AfterPrint. :/}
    procedure   Print;
    //
    {@method FindParentSite - Referência ao site pai. Retorna referência ao site pai após busca dinâmica pela prop Parent.
     @links TRLCustomSite. :/}
    function    FindParentSite   :TRLCustomSite;
    
    {@method FindParentBand - Referência à band pai. Retorna referência à band pai após busca dinâmica pela prop Parent.
     @links TRLCustomBand. :/}
    function    FindParentBand   :TRLCustomBand;

    {@method FindParentGroup - Referência ao grupo pai. Retorna referência ao grupo pai após busca dinâmica pela prop Parent.
     @links TRLCustomGroup. :/}
    function    FindParentGroup  :TRLCustomGroup;

    {@method FindParentPager - Referência ao pager pai. Retorna referência ao parentpager pai após busca dinâmica pela prop Parent.
     @links TRLCustomPager. :/}
    function    FindParentPager  :TRLCustomPager;

    {@method FindParentSkipper - Referência à skipper pai. Retorna referência ao skipper pai após busca dinâmica pela prop Parent.
     @links TRLCustomSkipper. :/}
    function    FindParentSkipper:TRLCustomSkipper;
    
    {@method FindParentReport - Referência ao relatório pai. Retorna referência ao relatório pai após busca dinâmica pela prop Parent.
     @links TRLCustomReport. :/}
    function    FindParentReport :TRLCustomReport;
    //
    {@method RequestParentPager - Referência ao pager pai. Gera exceção se não encontrar.
     @links TRLCustomPager. :/}
    function    RequestParentPager  :TRLCustomPager;

    {@method RequestParentSkipper - Referência à skipper pai. Gera exceção se não encontrar.
     @links TRLCustomSkipper. :/}
    function    RequestParentSkipper:TRLCustomSkipper;
    
    {@method RequestParentSurface - Referência à skipper pai. Gera exceção se não encontrar.
     @links TRLGraphicSurface. :/}
    function    RequestParentSurface:TRLGraphicSurface;

    {@method RequestParentReport - Referência ao report pai. Gera exceção se não encontrar.
     @links TRLCustomReport. :/}
    function    RequestParentReport :TRLCustomReport;

    {@method Realign - Força o realinhamento do controle dentro de seu control pai. :/}
    procedure   Realign; reintroduce;

    // dynamic methods
    
    {@method FindParentSurface - Superfície de desenho pai.
     Referência à superfície de desenho do painel pai.
     @links TRLGraphicSurface. :/}
    function    FindParentSurface:TRLGraphicSurface; dynamic;
    
    // custom properties
    
    {@prop Anchors - Ancoramento estendido. Propriedade estendida de ancoragem de controles.
     @links TRLControlAnchors. :/}
    property    Anchors           :TRLControlAnchors  read fAnchors            write SetAnchors          default [];
    
    {@prop Align - Alinhamento estendido. Propriedade estendida de alinhamento de controles.
     @links TRLControlAlign. :/}
    property    Align             :TRLControlAlign    read fAlign              write SetAlign            default faNone;

    {@prop Alignment - Especifica como o texto deve ser alinhado dentro de um controle.
     @links TRLTextAlignment. :/}
    property    Alignment         :TRLTextAlignment   read fAlignment          write SetAlignment        default taLeftJustify;

    {@prop AutoSize - Redimensionamento automático. Determina se o controle irá se
     redimensionar automaticamente de acordo com o seu conteúdo. :/}
    property    AutoSize          :boolean            read fAutoSize           write SetAutoSize         default False;

    {@prop AutoSizeDir - Determina em que direções o controle poderá efetuar o redimensionamento automático.
     @links TRLAutoSizeDirSet. :/}
    property    AutoSizeDir       :TRLAutoSizeDirSet  read fAutoSizeDir        write fAutoSizeDir        default [];
    
    {@prop AutoExpand - Determina se o controle fará a expansão de acordo com o seu conteúdo. :/}
    property    AutoExpand        :boolean            read fAutoExpand         write SetAutoExpand       default False;
    
    {@prop AutoTrunc - Determina se o tamanho do controle depende do conteúdo impresso. :/}
    property    AutoTrunc         :boolean            read fAutoTrunc          write SetAutoTrunc        default False;
    
    {@prop Behavior - Comportamento do controle. Utilize Behavior para definir o comportamento do controle sob diversos aspectos.
     @links TRLControlBehavior. :/}
    property    Behavior          :TRLControlBehavior read fBehavior           write fBehavior           stored IsBehavior;
    
    {@prop Caption - Texto a imprimir. :/}
    property    Caption           :TCaption           read GetCaption          write SetCaption          stored  IsCaption;
    
    {@prop FriendlyName - Nome amigável para uso com o ExpressionParser e interface com o usuário final. :/}
    property    FriendlyName      :string             read fFriendlyName       write SetFriendlyName     stored IsFriendlyName;
    
    {@prop HoldStyle - Estilo de ancoragem. Define as regras de ancoragem entre dois controles.
     @links TRLHoldStyle. :/}
    property    HoldStyle         :TRLHoldStyle       read fHoldStyle          write SetHoldStyle        default hsAsColumn;

    {@prop HolderOffset - Distância do ancoradouro. :/}
    property    HolderOffset      :TPoint             read fHolderOffset       write fHolderOffset;

    {@prop Layout - Layout do texto. Define o posicionamento vertical do texto no controle.
     @links TRLTextLayout. :/}
    property    Layout            :TRLTextLayout      read fLayout             write SetLayout           default tlTop;

    {@prop SecondHoldStyle - Estilo de ancoragem ao segundo ancoradouro.
     @links TRLHoldStyle. :/}
    property    SecondHoldStyle   :TRLHoldStyle       read fSecondHoldStyle    write SetSecondHoldStyle  default hsAsColumn;

    {@prop SecondHolderOffset - Distância ao segundo ancoradouro. :/}
    property    SecondHolderOffset:TPoint             read fSecondHolderOffset write fSecondHolderOffset;
    
    {@prop Transparent - Transparência do controle em tempo de impressão.
     Utilize Transparent quando for necessário imprimir apenas o conteúdo do painel.
     Um painel normalmente sobrepõe qualquer imagem ou efeito que estiver por trás dele.
     Quando o painel é transparente não possui uma cor de preenchimento, preservando a
     imagem ou efeitos desenhados no painel pai. :/} 
    property    Transparent       :boolean            read fTransparent        write SetTransparent      default True;
    
    // internal custom properties
    
    {@prop ControlState - Estado do controle dentre as diversas atividades.
     @links TRLControlState. :/}
    property    ControlState      :TRLControlState    read fControlState       write fControlState;

    {@prop OldBoundsRect - Contém as últimas dimensões do controle antes da última alteração. :/}
    property    OldBoundsRect     :TRect              read fOldBoundsRect      write fOldBoundsRect;

    {@prop PeekBoundsRect - Contém as dimensões originais do controle salvas antes da sua impressão. :/}
    property    PeekBoundsRect    :TRect              read fPeekBoundsRect     write fPeekBoundsRect;

    // indirections

    {@prop ClientHeight - Determina ou indica a altura da área cliente. :/}
    property    ClientHeight      :integer            read GetClientHeight     write SetClientHeight;

    {@prop ClientRect - Retângulo da área cliente.
     Retorna retângulo contendo as coordenadas da área cliente do controle.
     A área cliente corresponde ao retângulo Rect(0,0,Width,Height), deduzido das bordas. :/}
    property    ClientRect        :TRect              read GetClientRect       write SetClientRect;

    {@prop ClientWidth - Determina ou indica a largura da área cliente. :/}
    property    ClientWidth       :integer            read GetClientWidth      write SetClientWidth;

    // links

    {@prop Holder - Controle referência para ancoragem.
     O mecanismo por trás da prop holder é um dos recursos mais interessantes do FortesReport. Esta prop
     aponta para um controle que servirá como âncora, como referência de posicionamento.
     É possível informar para um RLDBText de uma band detalhe que sua posição horizontal deve se
     mantêr sempre igual ao RLLabel correspondente no cabeçalho, indicando RLDBText.Holder:=RLLabel.
     Deste modo, ao mover o label do cabeçalho, em tempo de design ou impressão, o RLDBText será
     movido junto com ele.
     Há várias opções de ancoragem e também há a possibilidade de um controle possuir dois
     holders: um para referência horizontal e outro para vertical, por exemplo.
     @links TRLCustomControl, HoldStyle, SecondHolder. :/}
    property    Holder            :TRLCustomControl   read fHolder             write SetHolder;

    {@prop SecondHolder - Segundo controle referência de ancoragem. Define um outro controle para referência de ancoragem.
     @links TRLCustomControl, SecondHoldStyle, Holder. :/}
    property    SecondHolder      :TRLCustomControl   read fSecondHolder       write SetSecondHolder;
    
    // agregates
    
    {@prop Borders - Bordas ao redor do controle.
     Utilize Borders para exibir bordas ao redor do painel. As bordas serão exibidas entre as margens
     exteriores e interiores do painel. É possível informar que lados serão exibidos, a largura das linhas,
     o nível de qualidade, a cor e etc.
     @links TRLBorders. :/}
    property    Borders           :TRLBorders         read fBorders            write SetBorders;

    {@prop RealBounds - Configuração do tamanho real de um controle em milímetros ou polegadas.
     Nota: Esta funcionalizade não está implementada.
     @links TRLRealBounds. :/}
    property    RealBounds        :TRLRealBounds      read fRealBounds         write SetRealBounds;

    // readonly

    {@prop Holdeds - Lista de controles "agarrados". Contém a lista dos controles que orientam suas posições relativamente às coordenadas deste. :/}
    property    Holdeds           :TList              read fHoldeds;

    {@prop MasterReport - Relatório mestre.
     Retorna referência ao componente TRLReport do relatório mestre ao qual o painel pertence.
     A pesquisa é feita dinamicamente a cada chamada e utiliza a propriedade Parent.
     Nota: O FortesReport permite a composição de relatórios através de concatenação.
     Esta propriedade deve retornar uma referência ao primeiro relatório da composição, do
     qual se pode extrair informações comuns a todos os relatórios, como: número de páginas,
     tamanho do papel, etc.
     @links TRLCustomReport. :/}
    property    MasterReport      :TRLCustomReport    read GetMasterReport;

    // events
    
    {@event AfterPrint - Após a impressão. Ocorre exatamente após o controle ter sua imagem impressa no relatório.
     @links TRLAfterPrintEvent. :/}
    property    AfterPrint        :TRLAfterPrintEvent read fAfterPrint         write fAfterPrint;

    {@event OnMeasureHeight - Na hora de medir a altura. :/}
    property    OnMeasureHeight   :TNotifyEvent       read fOnMeasureHeight    write fOnMeasureHeight;

    // standard properties

    {@prop ParentColor - Herança de cor. Define se o controle deve herdar a cor do controle pai. :/}
    property    ParentColor default True;

    {@prop ParentFont - Herança de fonte. Define se o controle deve herdar a fonte do controle pai. :/}
    property    ParentFont  default True;

    {@prop Visible - Determina se o controle será visível em tempo de impressão.
     Com esta propriedade configurada para False o controle será ignorado em tempo de impressão e nenhum evento
     ligado a ele será disparado. :/}
    property    Visible;

    {@prop Color - Cor do controle. Define a cor de fundo do controle. :/}
    property    Color;

    {@prop Font - Fonte do controle. Define a fonte do controle. :/}
    property    Font;
  end;
  {/@class}
  

  { TRLCustomDBControl }

  {@class TRLCustomDBControl - Classe base da qual se pode derivar controles de impressão dataware.
   @ancestor TRLCustomControl. }
  TRLCustomDBControl=class(TRLCustomControl)
  private

    // variables

    fDataField :TRLDataFieldProperty;
    fDataSource:TDataSource;

    // assign methods

    function    GetField:TField;
    function    GetDataSet:TDataSet;
    procedure   SetDataField(const aValue:TRLDataFieldProperty);
    procedure   SetDataSource(const aValue:TDataSource);
    
  protected

    // override methods

    function    InternalMakeCaption:string; override;
    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
    
  public

    // constructors & destructors
    
    constructor Create(aOwner:TComponent); override;

    // custom properties
    
    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property    DataField :TRLDataFieldProperty read fDataField  write SetDataField;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property    DataSource:TDataSource          read fDataSource write SetDataSource;

    // readonly
    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property    Field     :TField               read GetField;
    
    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property    DataSet   :TDataSet             read GetDataSet;
  end;
  {/@class}
  

  { TRLCustomLabel }

  {@class TRLCustomLabel - Classe base da qual derivam todas as caixas de texto.
   Utilize descendentes do TRLCustomLabel para imprimir textos estáticos ou dinâmicos sobre o relatório.
   @ancestor TRLCustomControl. }
  TRLCustomLabel=class(TRLCustomControl)
  protected

    // override methods

    procedure   CalcSize(var aSize:TPoint); override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties
    
    {@prop AutoSize - Redimensionamento automático.
     Determina se a label irá se redimensionar automaticamente de acordo com o tamanho do seu Caption. :/}
    property    AutoSize default True;

    {@prop Caption - Texto a ser impresso no corpo do label. :/}
    property    Caption;

    // events

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para alterar o texto ou
     suspender a sua impressão.
     @links TRLBeforeTextEvent. :/}
    property    BeforePrint:TRLBeforeTextEvent read fBeforeText write fBeforeText;
  end;
  {/@class}
  

  { TRLCustomAngleLabel }

  {@class TRLCustomAngleLabel - Caixa de texto com rotação por ângulo.
   @ancestor TRLCustomControl. }
  TRLCustomAngleLabel=class(TRLCustomControl)
  private

    // variables

    fAngle       :double;
    fAngleBorders:boolean;

    // assign methods

    procedure   SetAngle(const aValue:double);
    procedure   SetAngleBorders(const aValue:boolean);
    function    IsAngle:boolean;
    
  protected

    // override methods

    procedure   CalcSize(var aSize:TPoint); override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;


  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties

    {@prop Angle - Ângulo de inclinação.
     Determina o ângulo de inclinação no desenho do texto. :/}
    property    Angle       :double             read fAngle        write SetAngle        stored IsAngle;

    {@prop AngleBorders - Funcionalidade não implementada. :/}
    property    AngleBorders:boolean            read fAngleBorders write SetAngleBorders default False;

    // events

    {@event BeforePrint - Antes da impressão.
     Ocorre antes da impressão do controle para alterar o texto ou suspender a sua impressão.
     @links TRLBeforeTextEvent. :/}
    property    BeforePrint :TRLBeforeTextEvent read fBeforeText   write fBeforeText;

    {@prop AutoSize - Redimensionamento automático.
     Determina se a label irá se redimensionar automaticamente de acordo com o tamanho do seu Caption. :/}
    property    AutoSize default True;
  end;
  {/@class}


  { TRLCustomDBText }

  {@class TRLCustomDBText - Classe base da qual podem derivar caixas de texto dataware.
   @ancestor TRLCustomLabel. }
  TRLCustomDBText=class(TRLCustomLabel)
  private

    // variables

    fText       :TCaption;
    fDataField  :TRLDataFieldProperty;
    fDataFormula:string;
    fDataSource :TDataSource;
    fDisplayMask:string;

    // assign methods

    function    GetField:TField;
    function    GetFieldLabel:string;
    function    GetDataSet:TDataSet;
    procedure   SetDataField(const aValue:TRLDataFieldProperty);
    procedure   SetDataFormula(const aValue:string);
    procedure   SetDataSource(const aValue:TDataSource);

    // custom methods

    function    ApplyMask(const aValue:variant):string;

  protected

    // override & reintroduce

    function    InternalMakeCaption:string; override;
    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
    procedure   SetText(const aValue:TCaption); reintroduce;

    // dynamic

    function    GetFieldText:string; dynamic;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties

    {@prop Text - Texto auxiliar. Este texto deverá ser impresso junto com o conteúdo do campo. :/}
    property    Text       :TCaption             read fText        write SetText;

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property    DataField  :TRLDataFieldProperty read fDataField   write SetDataField;

    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. :/}
    property    DataFormula:string               read fDataFormula write SetDataFormula;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property    DataSource :TDataSource          read fDataSource  write SetDataSource;

    {@prop DisplayMask - Mascara de formatação. :/}
    property    DisplayMask:string               read fDisplayMask write fDisplayMask;

    // readonly

    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property    Field      :TField               read GetField;

    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property    DataSet    :TDataSet             read GetDataSet;

  end;
  {/@class}


  { TRLCustomDBResult }

  TRLDBResultBuiltIn=class
  public
    Id   :integer;
    Count:integer;
    Max  :variant;
    Min  :variant;
    Sum  :double;
    First:variant;
    Last :variant;
  end;

  {@class TRLCustomDBResult - Caixa de texto para resultado de cáculos matemáticos com campos de um dataset.
   @ancestor TRLCustomDBText. }
  TRLCustomDBResult=class(TRLCustomDBText)
  private

    // variables

    fCount          :integer;
    fMax            :variant;
    fMin            :variant;
    fSum            :double;
    fInfo           :TRLResultInfo;
    fFirst          :variant;
    fLast           :variant;
    fFirstText      :string;
    fLastText       :string;
    fSimple         :variant;
    fOnCompute      :TRLOnComputeEvent;
    fNullValue      :variant;
    fResetAfterPrint:boolean;
    fMustResetValue :boolean;
    fBuiltInRegs    :TObjectList;
    fComputeNulls   :boolean;

    // assign methods

    procedure   SetInfo(const aValue:TRLResultInfo);
    function    GetValue:variant; 
    function    GetNullValue:variant; 

    // builtin methods

    function    BuiltIn(aId:integer; aCanCreate:boolean=True):TRLDBResultBuiltIn;
    function    BuiltInCount(aId:integer):variant;
    function    BuiltInSum(aId:integer; aValue:variant):variant;
    function    BuiltInMin(aId:integer; aValue:variant):variant;
    function    BuiltInMax(aId:integer; aValue:variant):variant;
    function    BuiltInAvg(aId:integer; aValue:variant):variant;
    function    BuiltInFirst(aId:integer; aValue:variant):variant;
    function    BuiltInLast(aId:integer; aValue:variant):variant;
    
    procedure   Evaluate(var FieldText: string; var FieldValue: variant);

  protected

    // override methods

    function    GetFieldText:string; override;
    procedure   Initialize; override;
    procedure   ComputeDetail(aCaller:TObject); override;
    procedure   InternalPrint; override;
    function    GetAttribute(const aName:string):variant; override;

    {@method Resolve - Avalia uma função built-in. :/}
    function    Resolve(Sender:TObject; const Identifier:string; Params:variant):variant;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // properties

    {@prop Info - Tipo de informação.
     @links TRLResultInfo. :/}
    property    Info           :TRLResultInfo     read fInfo            write SetInfo default riSimple;

    {@prop ResetAfterPrint - Zerar os acumuladores após a impressão. :/}
    property    ResetAfterPrint:boolean           read fResetAfterPrint write fResetAfterPrint default False;

    // readonly

    {@prop Value - Valor parcial. :/}
    property    Value:variant read GetValue;

    {@prop ComputeNulls - Indica se campos com valor nulo serão computados. :/}
    property    ComputeNulls:boolean read fComputeNulls write fComputeNulls default True;

    // events

    {@event OnCompute - Ocorre durante os cálculos estatísticos para validação do valor a ser computado.
     @links TRLOnComputeEvent. :/}
    property    OnCompute:TRLOnComputeEvent read fOnCompute write fOnCompute;

  end;
  {/@class}
  

  { TRLCustomSystemInfo }

  {@class TRLCustomSystemInfo - Caixa de texto com informações de sistema.
   @ancestor TRLCustomLabel. }
  TRLCustomSystemInfo=class(TRLCustomLabel)
  private

    // variables

    fInfoType:TRLInfoType;
    fText    :TCaption;

    // assign methods

    procedure   SetInfoType(const aValue:TRLInfoType);

  protected

    // override & reintroduce

    function    InternalMakeCaption:string; override;
    procedure   SetText(const aValue:TCaption); reintroduce;
    
  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties

    {@prop Info - Tipo de informação.
     @links TRLInfoType. :/}
    property    Info:TRLInfoType read fInfoType write SetInfoType default itDate;

    {@prop Text - Texto auxiliar. :/}
    property    Text:TCaption    read fText     write SetText;
  end;
  {/@class}
  

  { TRLCustomMultiLine }

  {@class TRLCustomMultiLine - Classe base para controles multilinhas.
   @ancestor TRLCustomControl. }
  TRLCustomMultiLine=class(TRLCustomControl)
  private

    // variables

    fWordWrap      :boolean;
    fIntegralHeight:boolean;

    // assign methods

    procedure   SetWordWrap(const aValue:boolean);

  protected

    // override methods

    procedure   CalcSize(var aSize:TPoint); override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties

    {@prop WordWrap - Quebra automática de linha.
     Determina se quebras automáticas de linha deverão ser inseridas de modo a encaixar o texto de acordo com a
     largura do controle. :/}
    property    WordWrap      :boolean            read fWordWrap       write SetWordWrap     default True;

    {@prop IntegralHeight - Altura integral das linhas.
     Determina se as linhas que excederem a área cliente do controle serão exibidas. :/}
    property    IntegralHeight:boolean            read fIntegralHeight write fIntegralHeight default False;

    // events

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para alterar o texto ou suspender
     a sua impressão.
     @links TRLBeforeTextEvent. :/}
    property    BeforePrint   :TRLBeforeTextEvent read fBeforeText     write fBeforeText;

    {@prop AutoSize - Redimensionamento automático. Determina se o memo irá se redimensionar automaticamente de
     acordo com o tamanho do seu texto. :/}
    property    AutoSize default True;

  end;
  {/@class}
  

  { TRLCustomMemo }

  {@class TRLCustomMemo - Classe base para caixa de texto multilinhas.
   @ancestor TRLCustomMultiLine. }
  TRLCustomMemo=class(TRLCustomMultiLine)
  private

    // variables

    fLines:TStrings;

    // assign methods

    procedure   SetLines(const aValue:TStrings);

    // event handlers

    procedure   TreatOnChange(Sender: TObject);

  protected

    // override methods

    function    InternalMakeCaption:string; override;
    
  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // custom properties
    
    {@prop Lines - Lista contendo as linhas de texto do memo. :/}
    property    Lines:TStrings read fLines write SetLines;
  end;
  {/@class}
  

  { TRLCustomDBMemo }

  {@class TRLCustomDBMemo - Classe base para caixa de texto multilinhas ligado a campo de dataset.
   @ancestor TRLCustomMultiLine. }
  TRLCustomDBMemo=class(TRLCustomMultiLine)
  private

    // variables

    fDataField  :TRLDataFieldProperty;
    fDataFormula:string;
    fDataSource :TDataSource;

    // assign methods

    function    GetField:TField;
    function    GetFieldLabel:string;
    function    GetDataSet:TDataSet;
    procedure   SetDataField(const aValue:TRLDataFieldProperty);
    procedure   SetDataFormula(const aValue:string);
    procedure   SetDataSource(const aValue:TDataSource);

  protected

    // override methods

    function    InternalMakeCaption:string; override;
    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
    function    GetFieldText:string;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties
    
    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property    Field      :TField               read GetField;
    
    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property    DataSet    :TDataSet             read GetDataSet;
    
    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property    DataField  :TRLDataFieldProperty read fDataField   write SetDataField;

    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. :/}
    property    DataFormula:string               read fDataFormula write SetDataFormula;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property    DataSource :TDataSource          read fDataSource  write SetDataSource;
  end;
  {/@class}
  

  { TRLCustomImage }

  {@class TRLCustomImage - Classe base para caixa de imagem.
   @ancestor TRLCustomControl. }
  TRLCustomImage=class(TRLCustomControl)
  private

    // variables

    fPicture:TPicture;
    fStretch:boolean;
    fCenter :boolean;
    fScaled :boolean;

    // assign methods

    procedure   SetCenter(const aValue:boolean);
    procedure   SetPicture(const aValue:TPicture);
    procedure   SetStretch(const aValue:boolean);
    procedure   SetScaled(const aValue:boolean);

    // custom methods

    procedure   PictureChanged(Sender:TObject);

  protected

    // event handlers

    procedure   CalcSize(var aSize:TPoint); override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // custom properties
    
    {@prop Center - Centralização da imagem.
     Determina se a imagem deve ser posicionada ao centro da área cliente. :/}
    property    Center     :boolean             read fCenter      write SetCenter  default False;

    {@prop Stretch - Esticamento da imagem.
     Indica se a imagem deve ser esticada de modo a preencher totalmente a área cliente do controle. :/}
    property    Stretch    :boolean             read fStretch     write SetStretch default False;

    {@prop Scaled - Esticamento proporcional.
     Indica se a imagem deve ser esticada de modo a preencher área cliente do controle mantendo a mesma proporção
     de altura e largura. :/}
    property    Scaled     :boolean             read fScaled      write SetScaled  default False;

    // objects

    {@prop Picture - Representa a imagem que aparece no fundo do controle. :/}
    property    Picture    :TPicture            read fPicture     write SetPicture;

    // events

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para modificar a imagem ou
     suspender a sua impressão.
     @links TRLBeforePrintEvent. :/}
    property    BeforePrint:TRLBeforePrintEvent read fBeforePrint write fBeforePrint;
  end;
  {/@class}
  

  { TRLCustomDBImage }

  {@class TRLCustomDBImage - Classe base para caixa de imagem ligada a campo de dataset.
   @ancestor TRLCustomImage. }
  TRLCustomDBImage=class(TRLCustomImage)
  private

    // variables

    fDataField :TRLDataFieldProperty;
    fDataSource:TDataSource;

    // assign methods

    function    GetField:TField;
    function    GetDataSet:TDataSet;
    procedure   SetDataField(const aValue:TRLDataFieldProperty);
    procedure   SetDataSource(const aValue:TDataSource);

    // custom methods

    procedure   LoadPicture;

  protected

    // override methods

    procedure   Notification(aComponent:TComponent; Operation:TOperation); override;
    procedure   InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom properties

    {@prop Field - Referência para o objeto TField determinado pelas props DataField e DataSource. :/}
    property    Field     :TField               read GetField;

    {@prop DataSet - Referência para o objeto TDataSet determinado pela prop DataSource. :/}
    property    DataSet   :TDataSet             read GetDataSet;

    {@prop DataField - Nome do campo associado.
     @links TRLDataFieldProperty. :/}
    property    DataField :TRLDataFieldProperty read fDataField  write SetDataField;

    {@prop DataSource - Referência ao DataSource que controle utiliza para se conectar ao DataSet. :/}
    property    DataSource:TDataSource          read fDataSource write SetDataSource;
  end;
  {/@class}
  

  { TRLCustomDraw }

  {@type TRLDrawKind - Tipo de figura geométrica para o componente TRLDraw.
   Pode ser um dos seguintes valores:
   dkRectangle - Desenha um retângulo ou um quadrado;
   dkLine - Desenha uma linha reta;
   dkTriangle - Desenha um triângulo;
   dkElipse - Desenha uma elipse ou um círculo;
   dkArrow - Desenha uma seta simples;
   dkCustom - Desenha um polígono cujos pontos são definidos na prop DrawData.
   @links TRLDraw, TRLCustomDraw.DrawData. :/}
  TRLDrawKind=(dkRectangle,dkLine,dkTriangle,dkElipse,dkArrow,dkCustom);

  {@type TRLDrawOptions - Opções para desenho de figuras do RLDraw.
   Pode ser um conjunto dos seguintes valores:
   doKeepAspectRatio - A relação entre largura e altura da figura deve ser mantida;
   doKeepSize - O tamanho da figura será o original (mesmo do ângulo zero) para qualquer ângulo escolhido;
   doKeepVisible - A figura terá um tamanho que permita que ela seja vista inteira em qualquer ângulo escolhido.
   @links TRLDraw, TRLCustomDraw.DrawData. :}
  TRLDrawOption=(doKeepAspectRatio,doKeepSize,doKeepVisible);
  TRLDrawOptions=set of TRLDrawOption;
  {/@type}

  {@class TRLCustomDraw - Classe base para caixa de desenho de figuras geométricas.
   @ancestor TRLCustomControl. }
  TRLCustomDraw=class(TRLCustomControl)
  private

    // variables

    fAngle     :double;
    fBrush     :TBrush;
    fDrawKind  :TRLDrawKind;
    fPen       :TPen;
    fDrawData  :TStrings;
    fCenter    :boolean;
    fDrawWidth :integer;
    fDrawHeight:integer;
    fOptions   :TRLDrawOptions;

    // assign methods

    procedure   SetAngle(const aValue:double);
    procedure   SetBrush(const aValue:TBrush);
    procedure   SetDrawKind(const aValue:TRLDrawKind);
    procedure   SetPen(const aValue:TPen);
    procedure   SetDrawData(const Value:TStrings);
    procedure   SetCenter(const Value: boolean);
    procedure   SetDrawHeight(const Value: integer);
    procedure   SetDrawWidth(const Value: integer);
    procedure   SetOptions(const Value: TRLDrawOptions);
    //
    procedure   ReadKind(Reader: TReader);

    // event handlers

    procedure   ChangeResponse(Sender:TObject);

    // custom methods

    function    IsAngle:boolean;
    function    IsDrawData:Boolean;
    function    IsDrawSize: Boolean;
    procedure   ProducePoints(var aDest:TPointArray);
    procedure   ScaleToFit(var aPoints:TPointArray; const aRect:TRect);

  protected

    // override methods

    procedure   DefineProperties(Filer:TFiler); override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;

    

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // custom properties
    
    {@prop Angle - Ângulo de rotação da figura. :/}
    property    Angle      :double              read fAngle       write SetAngle stored IsAngle;

    {@prop DrawKind - Tipo de figura geométrica.
     @links TRLDrawKind. :/}
    property    DrawKind   :TRLDrawKind         read fDrawKind    write SetDrawKind default dkRectangle;

    // agregates

    {@prop Brush - Cor e padrão de preenchimento da figura. :/}
    property    Brush      :TBrush              read fBrush       write SetBrush;

    {@prop Pen - Cor e estilo dos traçoes usados no desenho da figura. :/}
    property    Pen        :TPen                read fPen         write SetPen;

    {@prop DrawData - Lista de coordenadas para desenho do polígono.
     A lista é uma sequência de números inteiros separados por espaços. Cada par de números
     representa a coordenada absoluta de um ponto do polígono. Todos os pontos serão
     ligados. O primeiro e o último fecharão o polígono. O polígono será desenhado e preenchido
     de acordo com as props Pen e Brush. :/}
    property    DrawData:TStrings read fDrawData write SetDrawData stored IsDrawData;

    {@prop Center - A figura deve ser centralizada na área cliente. :/}
    property    Center:boolean read fCenter write SetCenter default True;

    {@prop DrawWidth - Largura da figura em pixels. Quando não informada, fica valendo a largura do componente. :/}
    property    DrawWidth :integer read fDrawWidth write SetDrawWidth stored IsDrawSize;

    {@prop DrawHeight - Altura da figura em pixels. Quando não informada, fica valendo a altura do componente. :/}
    property    DrawHeight:integer read fDrawHeight write SetDrawHeight stored IsDrawSize;

    {@prop Options - Determina várias opções de desenho da figura. @links TRLDrawOptions. :/}
    property    Options:TRLDrawOptions read fOptions write SetOptions default [];

    // events

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para modificar a imagem
                          ou suspender a sua impressão.
     @links TRLBeforePrintEvent. :/}
    property    BeforePrint:TRLBeforePrintEvent read fBeforePrint write fBeforePrint;
  end;
  {/@class}
  

  { TRLCustomSite }

  {@class TRLCustomSite - Classe base da qual derivam todos os paineis de impressão como: TRLBand, TRLPanel,
   TRLGroup e o próprio TRLReport. Derive a partir do TRLCustomSite para criar qualquer painel customizado.
   Nota: Descendentes do TRLCustomSite podem conter controles e outros paineis.
   @links TRLPanel, TRLBand, TRLGroup, TRLReport.
   @ancestor TRLCustomControl. }
  TRLCustomSite=class(TRLCustomControl)
  private

    // variables

    fBackground      :TRLBackground;
    fDegrade         :TRLDegradeEffect;
    fInsideMargins   :TRLMargins;
    fMargins         :TRLMargins;
    fSurface         :TRLGraphicSurface;
    fPrintPosition   :TPoint;
    fPrintSize       :TPoint;

    // events

    fOnDraw       :TRLOnDrawEvent;

    // assign methods

    procedure   SetBackground(const aValue:TRLBackground);
    procedure   SetDegrade(const aValue:TRLDegradeEffect);
    procedure   SetInsideMargins(const aValue:TRLMargins);
    procedure   SetMargins(const aValue:TRLMargins);
    //
    procedure   DrawFrame(Rect:TRect; aColor:TColor; aRound:boolean);
    procedure   DrawTracks;
    procedure   DrawUnusedRect(Rect:TRect);
    procedure   InvalidateAll;
    procedure   Signup(const aSignature:string);
    // calc
    function    CalcClientPixels:TRect;
    function    CalcBordersPixels:TRect;
    function    CalcBordersRect:TRect;
    function    CalcMarginalRect:TRect;
    function    CalcPrintBordersRect:TRect;
    function    CalcPrintClientPixels:TRect;
    function    CalcPrintMarginalRect:TRect;
    function    CalcPrintWastedPixels:TRect;
    function    CalcPrintWastedPixelsSum:TRect;
    function    CalcGlobalPrintPosition:TPoint;
    function    CalcPrintBordersPixels:TRect;
    function    CalcPrintMarginalPixels:TRect;
  protected

    // override & reintroduce

    procedure   Loaded; override;
    procedure   CalcSize(var aSize:TPoint); override;

    {@method GetClientRect - Margens externas do painel.
     Retorna retângulo contendo as coordenadas da área cliente do painel.
     A área cliente corresponde ao retângulo (0,0,Width,Height), deduzido
     das margens externas, internas e das bordas. :/}
    function    GetClientRect:TRect; override;

    function    CanPrint:boolean; override;
    function    CalcWastedPixels:TRect; override;
    function    CalcPrintClientRect:TRect; override;
    function    CalcPrintSizeRect:TRect; override;
    function    CalcPrintBoundsRect:TRect; override;
    procedure   SetClientRect(const aValue:TRect); override;
    procedure   DrawBounds; override;
    procedure   InternalPrint; override;
    procedure   InternalPaint; override;
    procedure   InternalPaintFinish; override;
    procedure   RealignControls; override;
    procedure   InternalMeasureHeight; override;

    {@method AlignControls - Alinha os controles filhos. Não utilize este método diretamente.
     Ele provoca o alinhamento os controles filhos do panel segundo a propriedade estendida Align de cada controle
     através do método AlignControls e prossegue recursivamente. :}
    procedure   AlignControls(aRect:TRect); reintroduce; overload;
    procedure   AlignControls(aControl:TControl; var Rect:TRect); overload; override;
    {/@method}

    {@method DoOnDraw - Invoca o evento OnDraw. Não utilize este método diretamente.
     Ele é invocado durante a impressão do panel para permitir que um desenho qualquer seja feito em sua superfície. :/}
    procedure   DoOnDraw(aSurface:TRLGraphicSurface; aRect:TRect);

    // dynamic methods

    {@method SurfaceOpening - Uma nova superfície de impressão está sendo aberta.
     Local ideal para inicializações relativas à página ou sequência de dados. :/}
    procedure   SurfaceOpening; dynamic;

    {@method SurfaceBeginDraw - Os controles estão sendo desenhados na nova superfície de desenho. :/}
    procedure   SurfaceBeginDraw; dynamic;
    
    {@method SurfaceOpened - A superfície de impressão foi aberta e os controles estáticos já foram desenhados. :/}
    procedure   SurfaceOpened; dynamic;
    
    {@method WriteSurface - A superfície de impressão está pronta para a rotina de trabalho, se houver. :/}
    procedure   WriteSurface; dynamic;
    
    {@method SurfaceEndDraw - Os controles estáticos que dependem do tamanho do site e os de finalização estão sendo desenhados. :/}
    procedure   SurfaceEndDraw; dynamic;
    
    {@method SurfaceClosed - A superfície já foi fechada e agora deverá ser acumulada na superfície do controle pai. :/}
    procedure   SurfaceClosed; dynamic;
    
    {@method TruncateSurface - O desenho da superfície já foi terminado e sua altura definitiva deve ser determinada. :/}
    procedure   TruncateSurface; dynamic;

    {@method MarkPrintPosition - Primeira marcação da linha/coluna e dimensões de impressão. :/}
    procedure   MarkPrintPosition; dynamic;
    
    {@method ThrowSurface - Procede a transferência e posicionamento da superfície de impressão sobre a superfície do controle pai. :/}
    procedure   ThrowSurface; dynamic;
    
    {@method PrepareBackgroundSurface - Prepara a superfície de desenho do controle pai antes da relocação.
     Neste momento o controle está ciente do sua posição e tamanho finais e deve providenciar a preparação da
     superfície do controle pai.
     @links TRLGraphicSurface. :/}
    procedure   PrepareBackgroundSurface(aBackgroundSurface:TRLGraphicSurface; const aRect:TRect); dynamic;

    procedure   DrawClient; dynamic;
    
    {@method DrawBackground - Desenha imagem de fundo.
     Não utilize este método diretamente. Ele desenha a imagem definida em Background no fundo do painel. :/}
    procedure   DrawBackground(const aRect:TRect); dynamic;
    
    function    CalcEffectiveRect:TRect; dynamic;
    function    CalcMarginalPixels:TRect; dynamic;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // override methods
    
    procedure   SetBounds(aLeft,aTop,aWidth,aHeight:integer); override;
    procedure   PaintAsCustomSite;
    procedure   Initialize; override;
    procedure   ComputeDetail(aCaller:TObject); override;

    // static methods
    
    {@method OpenSurface - Cria uma nova superfície de desenho e inicializa-a.
     Não utilize este método diretamente. Esté método é invocado pelo o método Print. :/}
    procedure   OpenSurface;

    {@method CloseSurface - Fecha superfície de desenho e envia-a para o panel pai.
     Não utilize este método diretamente. Esté método é invocado após o método Print. Ele fecha a superfície de
     desenho e a repassa para o panel pai para ser devidamente posicionada. :/}
    procedure   CloseSurface;

    // agregates
    
    {@prop Background - Imagem para o fundo do painel.
     Utilize Background para colocar uma imagem no fundo do painel. A imagem deve ser um bitmap ou icone e pode ser
     disposta de várias formas de acordo com a propriedade Arrange.
     @links TRLBackground. :/}
    property    Background     :TRLBackground       read fBackground      write SetBackground;

    {@prop Degrade - Efeito de transição de cores no fundo do painel.
     Utilize Degrade para produzir o efeito de transição de cores no fundo do painel. Pode-se configurar as cores
     origem e destino, bem como a direção e a qualidade do efeito.
     @links TRLDegradeEffect. :/}
    property    Degrade        :TRLDegradeEffect    read fDegrade         write SetDegrade;

    {@prop InsideMargins - Margens internas do painel.
     Utilize InsideMargins quando for necessário posicionar os controles dentro do painel com um afastamento lateral
     dentro do retângulo definido por Margins e Borders.
     @links TRLMargins. :/}
    property    InsideMargins  :TRLMargins          read fInsideMargins   write SetInsideMargins;

    {@prop Margins - Margens externas do painel.
     Utilize Margins quando for necessário posicionar os controles dentro do painel com um afastamento lateral ou para
     reduzir o retângulo das bordas.
     @links TRLMargins. :/}
    property    Margins        :TRLMargins          read fMargins         write SetMargins;

    // events

    {@event OnDraw - Na hora de desenhar o fundo do site.
     @links TRLOnDrawEvent. :/}
    property    OnDraw         :TRLOnDrawEvent      read fOnDraw          write fOnDraw;

    {@event BeforePrint - Antes da impressão. Ocorre antes da impressão do controle para modificá-lo ou suspender
     sua impressão.
     @links TRLBeforePrintEvent. :/}
    property    BeforePrint    :TRLBeforePrintEvent read fBeforePrint     write fBeforePrint;

    // readonly

    {@prop Surface - Superfície de desenho.
     @links TRLGraphicSurface. :/}
    property    Surface        :TRLGraphicSurface   read fSurface;

    // standard properties
    
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEndDrag;
  end;
  {/@class}
  

  { TRLCustomPanel }

  {@class TRLCustomPanel - Classe base para containers de controles.
   Utilize um TRLCustomPanel como container para controles ou outros paineis.
   @ancestor TRLCustomSite. }
  TRLCustomPanel=class(TRLCustomSite)
  protected

    // override methods

    procedure   DrawBounds; override;
    
  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
  end;
  {/@class}
  

  { TRLCustomBandSet }

  {@class TRLCustomBandSet - Classe base para criação de bands.
   @ancestor TRLCustomSite. }
  TRLCustomBandSet=class(TRLCustomSite)
  private

    // variables

    fBandSets:TList;

    // custom methods

    function    FindParentBandSet:TRLCustomBandSet;

  protected

    // override methods

    procedure   SurfaceOpened; override;
    procedure   SurfaceClosed; override;
    procedure   SurfaceBeginDraw; override;

    // custom methods

    procedure   AddBandSet(aBandSet:TRLCustomBandSet);
    function    CountBandSet(aBandSet:TRLCustomBandSet):integer;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // custom methods
    
    function    IsFirstBandSet:boolean;
    function    BandSetCount:integer;
  end;
  {/@class}

  {@type TRLBandOption - Opção para formatação e comportamento de uma band.
   Pode ser um dos seguintes valores:
   boOptimisticPageBreak - Quebra de página otimista. O cálculo de espaço para
   forçar a quebra de página é feito somente após a renderização da band. Assim,
   o usuário pode modificar a altura da band e interferir na decisão da quebra.
   @links TRLBand.Options. :/}
  TRLBandOption=(boOptimisticPageBreak);

  {@type TRLBandOptions - Conjunto de opções para formatação e comportamento de uma band.
   @links TRLBandOption. :/}
  TRLBandOptions=set of TRLBandOption;

  { TRLCustomBand }

  {@class TRLCustomBand - Classe base da qual derivam as bandas de impressão.
   Derive a partir da TRLCustomBand para criar bandas de impressão de dados.
   As bandas de impressão formam a base do algorítmo de paginação do FortesReport.
   @links TRLBand, TRLDetailGrid.
   @ancestor TRLCustomBandSet. }
  TRLCustomBand=class(TRLCustomBandSet)
  private

    // variables

    fBandType      :TRLBandType;
    fComputable    :boolean;
    fPageBreaking  :TRLPageBreaking;
    fCompletion    :TRLCompletionType;
    fAlignToBottom :boolean;
    fCarbonCopies  :integer;
    fCarbonIndex   :integer;
    fGroupIndex    :integer;
    fIntegralHeight:boolean;
    fOptions       :TRLBandOptions;

    // assign methods

    procedure   SetBandType(const aValue:TRLBandType);
    procedure   SetCarbonCopies(const aValue:integer);
    procedure   SetGroupIndex(const aValue:integer);
    procedure   AdjustCarbonGroup;
    procedure   AdjustFromCarbonGroup;

    procedure   NotifyDataBandPrinted;
    function    GetCompleting:boolean;
    procedure   CheckPageBreak;

  protected

    // override methods
    procedure   SurfaceClosed; override;

    {@method ThrowSurface - Procede a transferência e posicionamento da superfície de impressão sobre a superfície
     do controle pai.
     Determina a posição e as dimensões de impressão antes da relocação para o controle pai. :/}
    procedure   ThrowSurface; override;

    {@method VerticalExceeded - O limite vertical foi excedido e uma atitude deve ser tomada.
     No caso das bands simples, a impressão para para uma nova página. :/}
    procedure   VerticalExceeded; dynamic;

    procedure   MarkPrintPosition; override;
    procedure   InternalPrint; override;

    // dynamic methods

    {@method HeightFits - A band cabe na página atual. Se não couber, aAvailableHeight representará o espaço disponível
     em pixels. :/}
    function    HeightFits(aHeight:integer; var aAvailable:integer):boolean; dynamic;

    {@method SkipToNextPosition - Move o cursor do parentpager para a posição da próxima band. :/}
    procedure   SkipToNextPosition(aWidth,aHeight:integer); dynamic;

    function    GetBandTypeName:string; dynamic;

    {@method IsDataBand - Indica se a band é uma band de dados.
     Se o tipo da band é btDetail ou btSummary e ela não está sendo impressa como um lastro, então ela é uma band
     de dados.
     @links IsBallast, BandType. :/}
    function    IsDataBand:boolean;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // override methods

    procedure   InternalPaintFinish; override;

    // internal custom properties
    
    {@prop Completing - Indica se a band está sendo impressa após o fim dos dados para completar o espaço que sobrou. :/}
    property    Completing    :boolean           read GetCompleting;

    {@prop CarbonIndex - Número da cópia da band. :/}
    property    CarbonIndex   :integer           read fCarbonIndex    write fCarbonIndex;

    // publishable
    
    {@prop AlignToBottom - Alinhado a parte inferior da página.
     Força a band a ser impressa na parte inferior da página como se fosse um btFooter. :/}
    property    AlignToBottom :boolean           read fAlignToBottom  write fAlignToBottom  default False;

    {@prop BandType - Define o comportamento da banda.
     Utilize a propriedade BandType para definir o comportamento da banda em relação aos dados impressos.
     @links TRLBandType. :/}
    property    BandType      :TRLBandType       read fBandType       write SetBandType     default btDetail;

    {@prop CarbonCopies - Número de cópias da band. :/}
    property    CarbonCopies  :integer           read fCarbonCopies   write SetCarbonCopies default 1;

    {@prop Completion - Tipo de preenchimento de página.
     @links TRLCompletionType. :/}
    property    Completion    :TRLCompletionType read fCompletion     write fCompletion     default ctNone;

    {@prop Computable - Indica se a band é válida para estatísticas. :/}
    property    Computable    :boolean           read fComputable     write fComputable     default True;

    {@prop GroupIndex - Agrupamento de bands. :/}
    property    GroupIndex    :integer           read fGroupIndex     write SetGroupIndex   default 0;

    {@prop PageBreaking - Quebra de página.
     @links TRLPageBreaking. :/}
    property    PageBreaking  :TRLPageBreaking   read fPageBreaking   write fPageBreaking   default pbNone;

    {@prop IntegralHeight - Determina se a band poderá ser exibida parcialmente.
     Se a band com o seu conteúdo não couber na página, a band poderá ser dividida em partes por página. :/}
    property    IntegralHeight:boolean           read fIntegralHeight write fIntegralHeight default True;

    {@prop Options - Opções diversas de formatação e comportamento da band.
     @links TRLBandOptions. :/}
    property    Options       :TRLBandOptions    read fOptions        write fOptions        default [];

    // standard
    
    {@prop AutoExpand - Expansão automática de acordo com crescimento do conteúdo. :/}
    property    AutoExpand default True;
  end;
  {/@class}
  

  { TRLCustomDetailGrid }

  {@type TRLDetailGridOrganization - Organização para impressão das bandas.
   Pode ser um dos seguintes valores:
   goInRows - Todas as bandas de uma linha são impressas antes de passar para a linha seguinte (padrão);
   goInColumns - As bandas são impressas verticalmente em coluna até o fim da página e então a impressão passa para
   o topo da próxima coluna.
   @links TRLDetailGrid. :/}
  TRLDetailGridOrganization=(goInRows,goInColumns);

  {@class TRLCustomDetailGrid - Classe base para bandas de detalhe multi-colunas.
   Banda de tipo fixado em btDetail. Ideal para a impressão de etiquetas e relatórios em colunas.
   @ancestor TRLCustomBand. }
  TRLCustomDetailGrid=class(TRLCustomBand)
  private

    // variables

    fColIndex   :integer;
    fColCount   :integer;
    fColSpacing :double;
    fColWidth   :double;
    fRowIndex   :integer;
    fTopRow     :integer;
    fBottomRow  :integer;
    fOrganization:TRLDetailGridOrganization;

    // assign methods

    procedure   SetColCount(const aValue:integer);
    procedure   SetColSpacing(const aValue:double);
    procedure   SetColWidth(const aValue:double);
    function    GetClientCellRect(aColIndex,aRowIndex:integer):TRect;

    //

    function    IsManyCols:boolean;

  protected

    // override methods

    function    GetBandTypeName:string; override;
    function    CalcEffectiveRect:TRect; override;
    procedure   MarkPrintPosition; override;
    procedure   SurfaceOpening; override;
    procedure   SurfaceClosed; override;

    {@method VerticalExceeded - O limite vertical foi excedido e uma atitude deve ser tomada.
     No caso do detailgrid, se a orientação for colbycol, então a impressão deve passar para uma nova coluna. :/}
    procedure   VerticalExceeded; override;

    {@method HeightFits - A band cabe na página atual.
     Se não couber, aAvailableHeight representará o espaço disponível em pixels. :/}
    function    HeightFits(aHeight:integer; var aAvailable:integer):boolean; override;

    {@method SkipToNextPosition - Move o cursor do parentpager para a posição da próxima band na coluna à direita
     ou abaixo. :/}
    procedure   SkipToNextPosition(aWidth,aHeight:integer); override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // override methods

    procedure   DrawClient; override;

    // custom methods

    procedure   Initialize; override;

    // internal custom properties

    {@prop ColIndex - Índice da coluna imprimindo. :/}
    property    ColIndex  :integer read fColIndex;

    {@prop ColCount - Total de colunas da grid. :/}
    property    ColCount  :integer read fColCount   write SetColCount default 1;

    {@prop ColSpacing - Espaço entre as colunas em milímetros. :/}
    property    ColSpacing:double  read fColSpacing write SetColSpacing stored IsManyCols;

    {@prop ColWidth - Largura das colunas em milímetros. :/}
    property    ColWidth  :double  read fColWidth   write SetColWidth stored IsManyCols;

    {@prop RowIndex - Índice da linha imprimindo. :/}
    property    RowIndex  :integer read fRowIndex;

    {@prop Organization - Determina a direção para a impressão das bandas.
     @links TRLDetailGridOrganization. :/}
    property    Organization:TRLDetailGridOrganization read fOrganization write fOrganization default goInRows;
  end;
  {/@class}
  

  { TRLCustomPager }

  {@type TRLPagerStatusType - Estado do pager.
   Indica os estados que o pager pode assumir.
   Pode ser um dos seguintes valores:
   psCompleting - Está completando a página com bands em branco. :/}
  TRLPagerStatusType=(psCompleting);
  
  {@type TRLPagerStatus - Conjunto de estados do pager.
   Indica os trabalhos que o pager está executando. :/}
  TRLPagerStatus=set of TRLPagerStatusType;

  {@class TRLCustomPager - Classe base para paginadores.
   Derive a partir da TRLCustomPager para criar controles de quebra de página.
   Os paginadores são containers para as bandas de impressão e controlam a quantidade de bandas que podem ser
   impressas por página.
   @links TRLReport, TRLSubDetail, TRLGroup.
   @ancestor TRLCustomBandSet. }
  TRLCustomPager=class(TRLCustomBandSet)
  private

    // variables

    fAllowedBands     :TRLAllowedBands;
    fDetailCount      :integer;
    fSortedBands      :TRLSortedBands;
    fMaxBands         :integer;
    fMinBands         :integer;
    fRelativePagerRow :integer;
    fDetailsInSurface :integer;
    fNewPageNeeded    :boolean;
    fPageBreaking     :TRLPageBreaking;
    fJumpPending      :boolean;
    fJumpLength       :integer;
    fNewPageCaller    :TObject;
    fForceMinBands    :boolean;
    fFooterMeasuring  :TRLFooterMeasuring;
    fDataBandPrinted  :boolean;
    fPagerStatus      :TRLPagerStatus;

    // assign methods
    
    function    GetSummaryHeight:integer;
    function    GetSummaryHeightSum:integer;
    function    GetFooterHeight:integer;
    function    GetFooterHeightSum:integer;
    function    GetColumnFooterHeight:integer;
    function    GetColumnFooterHeightSum:integer;
    function    GetWastedBottomSum:integer;
    function    GetNewPageNeeded:boolean;
    procedure   SetAllowedBands(const aValue:TRLAllowedBands);

    // custom methods

    function    CreateChild(aType:TRLBandType):TRLCustomBand;
    function    FindChild(aType:TRLBandType):TRLCustomBand;
    procedure   KillChild(aType:TRLBandType);
    procedure   SortBands;
    function    IsSatisfied:boolean;
    procedure   InitializePageInfo;

  protected

    // override methods
    
    procedure   SurfaceOpening; override;
    procedure   TruncateSurface; override;
    procedure   SurfaceClosed; override;
    procedure   MarkPrintPosition; override;
    procedure   SurfaceBeginDraw; override;
    procedure   SurfaceEndDraw; override;
    procedure   Notification(aComponent: TComponent; Operation:TOperation); override;

    // dynamic

    procedure   InternalBeginDoc; dynamic;
    procedure   InternalEndDoc; dynamic;

    // static

    procedure   InternalNewPage(aCaller:TObject; aMoveOnly:boolean=False);

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // override methods

    procedure   Initialize; override;
    procedure   ComputeDetail(aCaller:TObject); override;

    // custom methods

    function    PrintBands(aType:TRLBandType):TRLPrintBandResults;
    procedure   PrintBand(aBand:TRLCustomBand);
    procedure   PrintDetails;
    procedure   PrintHeaders;
    procedure   PrintFooters(aSummarize:boolean=False);
    procedure   PrintCompletion;
    procedure   PrintSite(aSite: TRLCustomSite);
    procedure   PrintPagers(aClass:TRLPagerClassType);
    procedure   PrintEmptySkippers;
    procedure   MeasureFooters;
    function    GetRelativeFooterRow:integer;
    function    GoFooterRow:boolean;
    function    GetRelativeSummaryRow:integer;
    function    GoSummaryRow:boolean;
    function    GetRelativeColumnFooterRow:integer;
    function    GoColumnFooterRow:boolean;
    procedure   InvalidatePage;
    procedure   BeginDoc;
    procedure   EndDoc;
    procedure   NewPage;

    // custom properties
    
    {@prop MaxBands - Número máximo de bands para o pager. :/}
    property    MaxBands         :integer             read fMaxBands          write fMaxBands        default 0;
    
    {@prop MinBands - Número mínimo de bands para o pager. :/}
    property    MinBands         :integer             read fMinBands          write fMinBands        default 0;
    
    {@prop PageBreaking - Quebra de página do pager.
     @links TRLPageBreaking. :/}
    property    PageBreaking     :TRLPageBreaking     read fPageBreaking      write fPageBreaking    default pbNone;

    {@prop AllowedBands - Tipos de bands inseridas.
     @links TRLAllowedBands. :/}
    property    AllowedBands     :TRLAllowedBands     read fAllowedBands      write SetAllowedBands  default [];

    {@prop ForceMinBands - Forçar a quantidade mínima de bands. :/}
    property    ForceMinBands    :boolean             read fForceMinBands     write fForceMinBands   default False;

    {@prop FooterMeasuring - Antecipação do cálculo da altura dos rodapés.
     @links TRLFooterMeasuring. :/}
    property    FooterMeasuring  :TRLFooterMeasuring  read fFooterMeasuring   write fFooterMeasuring default fmNone;
    
    // internal custom properties
    
    {@prop RelativePagerRow - Número da linha atual relativa ao pager. :/}
    property    RelativePagerRow :integer             read fRelativePagerRow  write fRelativePagerRow;
    
    {@prop DetailsInSurface - Quantidade de detalhes impressos na página atual. :/}
    property    DetailsInSurface :integer             read fDetailsInSurface  write fDetailsInSurface;
    
    {@prop NewPageNeeded - Indica a necessidade de salto de página. :/}
    property    NewPageNeeded    :boolean             read GetNewPageNeeded   write fNewPageNeeded;
    
    {@prop DataBandPrinted - Indica se alguma band de dados já foi impressa na página atual. :/}
    property    DataBandPrinted  :boolean             read fDataBandPrinted   write fDataBandPrinted;

    // readonly
    
    {@prop DetailCount - Número de bands de detalhe impressas desde o início da impressão. :/}
    property    DetailCount      :integer             read fDetailCount;

    // colections

    {@prop SortedBands - Lista de bands agrupadas pelo tipo.
     @links TRLSortedBands. :/}
    property    SortedBands      :TRLSortedBands      read fSortedBands;

    {@prop PagerStatus - Estado do pager.
     Indica se o pager está completando a página com bands vazias após o término dos dados.
     @links TRLPagerStatus. :/}
    property    PagerStatus      :TRLPagerStatus      read fPagerStatus;
  end;
  {/@class}
  

  { TRLCustomGroup }

  {@class TRLCustomGroup - Classe base para sequências de registros de dados.
   Utilize descendentes do TRLCustomGroup para imprimir sequências de registros de dados.
   @ancestor TRLCustomPager. }
  TRLCustomGroup=class(TRLCustomPager)
  private

    // variables

    fOnGetBreak :TRLOnGetBreakEvent;
    fDataFields :TRLDataFieldsProperty;
    fDataFormula:string;
    fLastKey    :string;
    fBroken     :boolean;

    // assign methods

    function    GetKey:string;
    function    CheckBreak:boolean;
    procedure   SetDataFields(const Value: TRLDataFieldsProperty);
    procedure   SetDataFormula(const Value: string);

  protected

    // override methods

    procedure   InternalPrint; override;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // override methods

    procedure   ComputeDetail(aCaller:TObject); override;
    procedure   InternalPaintFinish; override;

    // custom properties
    
    {@prop DataFields - Campo ou conjunto de campos que determinam a quebra de sequência de registros.
     Informe os campos determinantes da quebra de sequência de registros. Os campos devem ser separados por
     ponto-e-vírgula ";". A quebra automatica é detectada através da comparação no conteúdo dos campos do último
     registro impresso com o atual.
     @links TRLDataFieldsProperty. :/}
    property    DataFields:TRLDataFieldsProperty read fDataFields  write SetDataFields;

    {@prop DataFormula - Expressão matemática envolvendo campos, valores e literais. @links DataFields. :/}
    property    DataFormula:string               read fDataFormula write SetDataFormula;

    {@prop Enabled - Quebra de registros habilitada.
     Quando setada para False, esta propriedade desativa as quebras de sequência do grupo, porém sem interferir
     nos controles e grupos internos, que são impressos normalmente. :/}
    property    Enabled;

    // events

    {@event OnGetBreak - Evento que determina da quebra de sequência de registros.
     Informe na implementação do evento OnGetBreak quando a quebra de sequência deverá ser efetuada. Sender é uma
     referência ao componente de grupo que originou a chamada. O parâmetro BreakIt deverá ser setado para True para
     que a quebra aconteça.
     Nota: Este evento é chamado a partir do segundo registro da sequência a ser impresso.
     @links TRLOnGetBreakEvent. :/}
    property    OnGetBreak:TRLOnGetBreakEvent    read fOnGetBreak  write fOnGetBreak;

  end;
  {/@class}
  

  { TRLCustomSkipper }

  {@class TRLCustomSkipper - Classe base para pager com fontes de dados.
   Derive a partir da TRLCustomSkipper para criar fontes de dados para as bandas.
   As fontes de dados, além de acumularem a função de paginadores, controlam a sequência de dados, automaticamente
   quando a fonte é uma DataSource, ou através de eventos de interação.
   @links TRLReport, TRLSubDetail.
   @ancestor TRLCustomPager. }
  TRLCustomSkipper=class(TRLCustomPager)
  private

    // variables

    fRecordAction:TRLRecordAction;
    fDataSource  :TDataSource;
    fOnDataCount :TRLOnDataCountEvent;
    fOnDataRecord:TRLOnDataRecordEvent;
    fOnNeedData  :TRLOnNeedDataEvent;
    fDataEof     :boolean;
    fRecNo       :integer;
    fCopyNo      :integer;
    fRecordMoved :boolean;
    fRecordRange :TRLRecordRange;
    fRangeCount  :integer;
    fPrintEmpty  :boolean;

    //

    function    IsNextNRecordRange:boolean;

    // assign methods

    procedure   SetDataSource(const aValue:TDataSource);

  protected

    // override methods

    procedure   Notification(aComponent: TComponent; Operation:TOperation); override;
    procedure   InternalPrint; override;
    
    procedure   PrintAnything;

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // custom methods

    function    DataCount:integer; dynamic;
    procedure   DataFirst; dynamic;
    procedure   DataNext; dynamic;

    // custom properties
    
    {@prop DataSource - Referência ao DataSource de onde os registros serão obtidos. :/}
    property    DataSource  :TDataSource          read fDataSource   write SetDataSource;

    {@prop RecordRange - Indica a faixa de registros a processar. @links TRLRecordRange, RangeCount. :/}
    property    RecordRange :TRLRecordRange       read fRecordRange  write fRecordRange default rrAllRecords;

    {@prop RangeCount - Indica a quantidade de registros a processar a partir do atual se a prop RecordRange for rrNextN. @links RecordRange. :/}
    property    RangeCount  :integer              read fRangeCount   write fRangeCount stored IsNextNRecordRange;

    // internal custom properties

    {@prop RecordMoved - Indice se o registro foi movido por algum processo subsequente. :/}
    property    RecordMoved :boolean              read fRecordMoved  write fRecordMoved;

    // events

    {@event OnDataCount - Ao solicitar a quantidade de registros.
     @links TRLOnDataCountEvent. :/}
    property    OnDataCount :TRLOnDataCountEvent  read fOnDataCount  write fOnDataCount;

    {@event OnDataRecord - Ao selecionar um registro a imprimir.
     @links TRLOnDataRecordEvent. :/}
    property    OnDataRecord:TRLOnDataRecordEvent read fOnDataRecord write fOnDataRecord;

    {@event OnNeedData - Ao solicitar novos registros.
     @links TRLOnNeedDataEvent. :/}
    property    OnNeedData  :TRLOnNeedDataEvent   read fOnNeedData   write fOnNeedData;

    // readonly
    
    {@prop DataEof - Indica o final dos dados de entrada. :/}
    property    DataEof     :boolean              read fDataEof;
    
    {@prop RecordAction - Ação tomada para último registro.
     @links TRLRecordAction. :/}
    property    RecordAction:TRLRecordAction      read fRecordAction;
    
    {@prop RecNo - Número do registro atual. :/}
    property    RecNo       :integer              read fRecNo;
    
    {@prop CopyNo - Número da cópia da band atual. :/}
    property    CopyNo      :integer              read fCopyNo;

    {@prop PrintEmpty - Indica se o relatório deve ser gerado e impresso mesmo que não haja registros a imprimir. :/}
    property    PrintEmpty:boolean read fPrintEmpty write fPrintEmpty default False;
  end;
  {/@class}
  

  { TRLCustomSubDetail }

  {@class TRLCustomSubDetail - Mini relatório para relacionamentos tipo master/detail.
   Utilize os descendentes do TRLCustomSubDetail para imprimir registros ou sequências de dados relacionadas com
   os registros da fontes de dados principal. O controle de sub-detalhe é especialmente útil quando se quer listar
   registros de uma base que possui registros filhos ou relacionados (Master/Detail), aonde um TRLReport responderia
   pelos registros principais e o TRLSubDetail pelos registros filhos.
   @links TRLSubDetail.
   @ancestor TRLCustomSkipper. }
  TRLCustomSubDetail=class(TRLCustomSkipper)
  private

    // variables

    fPositioning:TRLBandType;

    // assign methods

    procedure   SetPositioning(const Value:TRLBandType);

  public

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;

    // override methods

    procedure   InternalPaintFinish; override;

    // custom properties

    {@prop Positioning - Posicionamento do subdetail. Equivalente à prop BandType da TRLBand.
     @links TRLBandType. :/}
    property    Positioning:TRLBandType read fPositioning write SetPositioning default btDetail;
  end;
  {/@class}

  { TRLCustomReport }

  {@class TRLCustomReport - Componente principal na confecção de relatórios.
   Utilize os descendentes do TRLCustomReport como ponto de partida na confecção de qualquer relatório com o
   FortesReport. Um componente TRLCustomReport pode listar registros de uma fonte de dados, solicitar os dados
   através de eventos em tempo de execução ou apenas imprimir páginas confeccionadas com os componentes da biblioteca.
   @links TRLReport.
   @ancestor TRLCustomSkipper. }
  TRLCustomReport=class(TRLCustomSkipper)
  private

    // variables

    fParseInvoker     :TObject;

    // property variables

    fOnPageEnding     :TNotifyEvent;
    fOnPageStarting   :TNotifyEvent;
    fCanceled         :boolean;
    fPages            :TRLGraphicStorage;
    fPageSurface      :TRLGraphicSurface;
    fNextReport       :TRLCustomReport;
    fPriorReport      :TRLCustomReport;
    fFirstPageNumber  :integer;
    fPageIndex        :integer;
    fPageSetup        :TRLPageSetup;
    fPrintDialog      :boolean;
    fPrinterMetrics   :TRLPrinterMetrics;
    fReportState      :TRLReportState;
    fShowDesigners    :boolean;
    fShowTracks       :boolean;
    fShowExplosion    :boolean;
    fTitle            :string;
    fReportDateTime   :TDateTime;
    fDefaultFilter    :TRLCustomPrintFilter;
    fExpressionParser :TRLExpressionParser;
    fInternalParser   :TRLExpressionParser;
    fShowProgress     :boolean;
    fPrintQuality     :TRLPrintQuality;
    fOnFilterText     :TRLBeforeTextEvent;
    fAdjustableMargins:boolean;
    fPreviewOptions   :TRLPreviewOptions;
    fForcePrepare     :boolean;
    fBackgroundMode   :boolean;
    fProgressPhase    :string;
    fProgressMax      :integer;

    // assign methods

    function    GetCurrentPage:TRLGraphicSurface;
    function    GetPageByNumber(n:integer):TRLGraphicSurface;
    function    GetPageNumber:integer;
    function    GetLastPageNumber:integer;
    procedure   SetPriorReport(const aValue:TRLCustomReport);
    procedure   SetNextReport(const aValue:TRLCustomReport);
    procedure   SetPageIndex(const aValue:integer);
    procedure   SetPageNumber(const aValue:integer);
    procedure   SetShowDesigners(const aValue:boolean);
    procedure   SetShowTracks(const aValue:boolean);
    procedure   SetShowExplosion(const aValue:boolean);
    procedure   SetPrintQuality(const aValue:TRLPrintQuality);
    procedure   SetDefaultFilter(const aValue:TRLCustomPrintFilter);
    procedure   SetExpressionParser(const aValue:TRLExpressionParser);
    procedure   SetAdjustableMargins(const aValue:boolean);
    procedure   SetPageSetup(const Value: TRLPageSetup);
    procedure   SetPreviewOptions(const Value: TRLPreviewOptions);

    // custom events

    procedure   ParserResource(Sender:TObject; const aIdentifier:string; aParams:variant; var aResult:variant);
    procedure   ParserTokener(Sender:TObject; var aToken:string; var aKind:TRLParserTokenKind);
    procedure   ParserFindAgregate(Sender:TObject; aOwner:TPersistent; const aName:string; var aAgregate:TPersistent);
    procedure   ParserGetAttribute(Sender:TObject; aOwner:TPersistent; const aName:string; var aValue:variant);
    procedure   ParserSetAttribute(Sender:TObject; aOwner:TPersistent; const aName:string; const aValue:variant; var aHandled:boolean);

    // custom methods

    function    GetOrientedUnprintablePixels:TRect;
    function    GetOrientedUnprintableRect:TRect;

    procedure   ProgressCreate;
    procedure   ProgressDestroy;
    procedure   ProgressPhase;
    procedure   ProgressSetMax;
    procedure   ProgressStepIt;

    procedure   DoPageStarting;
    procedure   DoPageEnding;
    procedure   DoFilterText(var aText:string; var aPrintIt:boolean);
    procedure   UpdateMacros;
    procedure   InitPrintParams;
    procedure   PrepareNeeded;

  protected

    // override methods

    function    CalcSizeRect:TRect; override;
    procedure   SurfaceOpening; override;
    procedure   SurfaceBeginDraw; override;
    procedure   SurfaceEndDraw; override;
    procedure   SurfaceClosed; override;
    procedure   Notification(aComponent: TComponent; Operation: TOperation); override;
    procedure   DrawBackground(const aRect:TRect); override;
    procedure   CalcSize(var aSize:TPoint); override;
    function    CalcMarginalPixels:TRect; override;
    procedure   InternalPrint; override;
    procedure   InternalEndDoc; override;

    // custom methods

    procedure   AfterLoad;
    procedure   ReloadPrinter;
    function    ParserNeeded:TRLExpressionParser;
    function    CanShowProgress:boolean;
    procedure   InternalPrepare;
  public

    // variables

    ProgressForm :TfrmRLFeedBack;
    PreviewClosed:boolean;

    // constructors & destructors

    constructor Create(aOwner:TComponent); override;
    destructor  Destroy; override;

    // override methods

    procedure   DataFirst; override;
    procedure   DataNext; override;
    procedure   InternalPaintFinish; override;
    function    FindParentSurface:TRLGraphicSurface; override;

    // custom methods

    procedure   Clear;
    function    ShowPrintDialog:boolean;

    {@method Prepare - Prepara as páginas para impressão. Representa o processo de confecção do
    relatório, chamadas a eventos beforeprint, processamentos de datasources, etc.
    @links BackgroundMode. :/}
    function Prepare:boolean;

    procedure   Preview(Dest:TRLPreview=nil);
    procedure   PreviewModal;
    procedure   ClosePreview;

    procedure   SaveToStream(aStream:TStream);
    procedure   LoadFromStream(aStream:TStream);
    procedure   LoadFromFile(const aFileName:string);
    procedure   SaveToFile(const aFileName:string);
    procedure   Cancel;
    function    Parse(Sender:TObject; const aExpression:string):variant;

    // custom properties

    {@prop AdjustableMargins - Determina se as margens poderão ser aumentadas de acordo com a área não imprimível da
     impressora. :/}
    property    AdjustableMargins :boolean              read fAdjustableMargins   write SetAdjustableMargins default False;

    {@prop FirstPageNumber - Númeração para a primeira página. :/}
    property    FirstPageNumber   :integer              read fFirstPageNumber     write fFirstPageNumber     default 1;

    {@prop ForcePrepare - Indica que o relatório deve ser sempre preparado antes de imprimir ou visualizar, mesmo que isso já
    tenha sido feito na mesma seção, isto é, na mesma instância do form de design.
    @links Prepare. :/}
    property    ForcePrepare      :boolean              read fForcePrepare        write fForcePrepare        default True;

    {@prop PrintDialog - Indica se um diálogo de seleção será exibido antes da impressão. :/}
    property    PrintDialog       :boolean              read fPrintDialog         write fPrintDialog         default True;

    {@prop ShowDesigners - Exibir régua e delineadores dos controles em tempo de design. :/}
    property    ShowDesigners     :boolean              read fShowDesigners       write SetShowDesigners     default True;

    {@prop ShowTracks - Exibir régua em tempo de design. :/}
    property    ShowTracks        :boolean              read fShowTracks          write SetShowTracks        default True;

    {@prop ShowExplosion - Não implementada. :/}
    property    ShowExplosion     :boolean              read fShowExplosion       write SetShowExplosion     default False;

    {@prop Title - Título do relatório.
     Pode ser recuperado pelo componente TRLSystemInfo. :/}
    property    Title             :string               read fTitle               write fTitle;

    {@prop ShowProgress - Exibir barra de progresso. :/}
    property    ShowProgress      :boolean              read fShowProgress        write fShowProgress        default True;

    {@prop PrintQuality - Qualidade de impressão.
     @links TRLPrintQuality. :/}
    property    PrintQuality      :TRLPrintQuality      read fPrintQuality        write SetPrintQuality      default pqFullFeature;

    {@prop ReportDateTime - Data e hora de impressão do relatório. :/}
    property    ReportDateTime    :TDateTime            read fReportDateTime      write fReportDateTime;

    // external

    {@prop DefaultFilter - Filtro padrão de impressão.
     @links TRLCustomPrintFilter. :/}
    property    DefaultFilter     :TRLCustomPrintFilter read fDefaultFilter       write SetDefaultFilter;

    {@prop ExpressionParser - Referência para um objeto avaliador de expressões matemáticas.
     @links TRLExpressionParser. :/}
    property    ExpressionParser  :TRLExpressionParser  read fExpressionParser    write SetExpressionParser;

    {@prop PriorReport - Relatório anterior da composição.
     @links TRLCustomReport. :/}
    property    PriorReport       :TRLCustomReport      read fPriorReport         write SetPriorReport;

    {@prop NextReport - Relatório seguinte da composição.
     @links TRLCustomReport. :/}
    property    NextReport        :TRLCustomReport      read fNextReport          write SetNextReport;

    // internal custom properties

    {@prop PageIndex - Índice da página atual. :/}
    property    PageIndex         :integer              read fPageIndex           write SetPageIndex;

    {@prop PageNumber - Número da página atual (FirstPageNumber+PageIndex). :/}
    property    PageNumber        :integer              read GetPageNumber        write SetPageNumber;

    {@prop ReportState - Estado da preparação do relatório.
     @links TRLReportState. :/}
    property    ReportState       :TRLReportState       read fReportState;

    // readonly

    {@prop Canceled - Indica se o relatório foi cancelado durante a preparação. :/}
    property    Canceled          :boolean              read fCanceled;

    {@prop LastPageNumber - Número da última página. :/}
    property    LastPageNumber    :integer              read GetLastPageNumber;

    {@prop PageByNumber - Referência ao objeto página pelo número.
     @links TRLGraphicSurface. :/}
    property    PageByNumber[n:integer]:TRLGraphicSurface read GetPageByNumber;

    // agregates

    {@prop PrinterMetrics - Dimensões do papel na impressora.
     @links TRLPrinterMetrics. :/}
    property    PrinterMetrics    :TRLPrinterMetrics    read fPrinterMetrics;

    {@prop Pages - Lista de páginas preparadas.
     @links TRLGraphicStorage. :/}
    property    Pages             :TRLGraphicStorage    read fPages;

    {@prop CurrentPage - Referência à página atual.
     @links TRLGraphicSurface. :/}
    property    CurrentPage       :TRLGraphicSurface    read GetCurrentPage;

    {@prop PageSetup - Configuração do papel.
     @links TRLPageSetup. :/}
    property    PageSetup         :TRLPageSetup         read fPageSetup           write SetPageSetup;

    {@prop PreviewOptions - Opções de pré-visualização.
     @links TRLPreviewOptions. :/}
    property    PreviewOptions    :TRLPreviewOptions    read fPreviewOptions      write SetPreviewOptions;

    {@prop BackgroundMode - Configura o modo de impressão imediata com preparação de páginas em segundo plano.
    Com esta opção ligada o relatório poderá começar ser impresso ou visualizado mesmo antes de ter sido completado.
    Nota: Para permitir a impressão das páginas tão logo sejam preparadas, o FortesReport implementou
    o esquema de multi-threads, isto é, a rotina de preparação dos dados roda numa linha de processamento
    diferente da rotina de impressão ou pré-visualização. Isto dá uma maior clareza e alta performance
    aos processos, os quais se intercalam naturalmente. Neste processo, a tela de pré-visualização exibe
    apenas as páginas que já tiverem sido preparadas.
    Como consequência deste processo, é importante ressaltar que informações que não estiverem disponíveis
    no momento da impressão da página não serão inseridas, como por exemplo o número da página final. Dessa
    maneira, informações como PagePreview e LastPageNumber do RLSystemInfo não serão compatíveis com o modo
    de impressão imediata. O usuário deve estar atento também a informações que ele mesmo disponibilizou para
    as páginas na prop Macros do RLReport.Pages.
    Também em qualquer implementação de eventos dos componentes, deve-se tomar cuidado ao fazer chamadas não
    threadsafe, como: exibição de diálogos ou manipulação de controles VCL/CLX. Nestes casos, e para relatórios
    que rodem em background, é necessário sincronizar a chamada a métodos com através do procedimento
    RLUtils.ThreadSafeCall().
    Nota: Por motivo de compatibilidade com versões anteriores à 3.24, esta opção é inicializada com False.
    Nota: Valores definidos somente no final do relatório podem comprometer a impressão imediata (ex: LastPageNumber).
    @links Prepare, ThreadSafeCall. :/}
    property    BackgroundMode   :boolean              read fBackgroundMode     write fBackgroundMode default False;

    // events

    {@event OnPageEnding - Ao terminar uma página. :/}
    property    OnPageEnding      :TNotifyEvent         read fOnPageEnding        write fOnPageEnding;

    {@event OnPageStarting - No início de cada página. :/}
    property    OnPageStarting    :TNotifyEvent         read fOnPageStarting      write fOnPageStarting;

    {@event OnFilterText - Ao imprimir qualquer texto.
     Captura de textos antes do envio para a impressora.
     @links TRLBeforeTextEvent. :/}
    property    OnFilterText      :TRLBeforeTextEvent   read fOnFilterText        write fOnFilterText;

    // standard
    
    property    ParentFont  default False;
    property    ParentColor default False;
    property    Color       default clWhite;
  end;
  {/@class}
  

  // FINAL COMPONENTS

  { TRLLabel }

  {@class TRLLabel - Caixa de texto padrão.
   Utilize o TRLLabel para imprimir textos estáticos sobre o relatório.
   @icon TRLLabel.jpg
   @ancestor TRLCustomLabel.
   @pub }
  TRLLabel=class(TRLCustomLabel)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Caption = ancestor /}
    property    Caption;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}
  

  { TRLAngleLabel }

  {@class TRLAngleLabel - Caixa de texto de com rotação por ângulo.
   @icon TRLAngleLabel.jpg
   @ancestor TRLCustomAngleLabel.
   @pub }
  TRLAngleLabel=class(TRLCustomAngleLabel)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop Angle = ancestor /}
    property    Angle;
    {@prop AngleBorders = ancestor /}
    property    AngleBorders;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Caption = ancestor /}
    property    Caption;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}


  { TRLDBText }

  {@class TRLDBText - Caixa de texto ligada a campo de dataset.
   @icon TRLDBText.jpg
   @ancestor TRLCustomDBText.
   @pub }
  TRLDBText=class(TRLCustomDBText)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataFormula = ancestor /}
    property    DataFormula;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop DisplayMask = ancestor /}
    property    DisplayMask;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Text = ancestor /}
    property    Text;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;

    Protected
  end;
  {/@class}


  { TRLDBResult }

  {@class TRLDBResult - Caixa de texto de resultado de operações matemáticas ou estatíticas com campos de dataset.
   @icon TRLDBResult.jpg
   @ancestor TRLCustomDBResult.
   @pub }
  TRLDBResult=class(TRLCustomDBResult)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop ComputeNulls = ancestor /}
    property    ComputeNulls;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataFormula = ancestor /}
    property    DataFormula;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop DisplayMask = ancestor /}
    property    DisplayMask;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop Info = ancestor /}
    property    Info;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop ResetAfterPrint = ancestor /}
    property    ResetAfterPrint;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Text = ancestor /}
    property    Text;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnCompute = ancestor /}
    property    OnCompute;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;

  end;
  {/@class}


  { TRLSystemInfo }

  {@class TRLSystemInfo - Caixa de texto de com informações do sistema.
   @icon TRLSystemInfo.jpg
   @ancestor TRLCustomSystemInfo.
   @pub }
  TRLSystemInfo=class(TRLCustomSystemInfo)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop Info = ancestor /}
    property    Info;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Text = ancestor /}
    property    Text;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}


  { TRLMemo }

  {@class TRLMemo - Caixa de texto multilinhas.
   @icon TRLMemo.jpg
   @ancestor TRLCustomMemo.
   @pub }
  TRLMemo=class(TRLCustomMemo)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop IntegralHeight = ancestor /}
    property    IntegralHeight;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop Lines = ancestor /}
    property    Lines;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;
    {@prop WordWrap = ancestor /}
    property    WordWrap;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}


  { TRLDBMemo }

  {@class TRLDBMemo - Caixa de texto multilinhas ligada a campo de dataset.
   @icon TRLDBMemo.jpg
   @ancestor TRLCustomDBMemo.
   @pub }
  TRLDBMemo=class(TRLCustomDBMemo)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataFormula = ancestor /}
    property    DataFormula;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop IntegralHeight = ancestor /}
    property    IntegralHeight;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;
    {@prop WordWrap = ancestor /}
    property    WordWrap;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;

  end;
  {/@class}


  { TRLImage }

  {@class TRLImage - Caixa de imagem.
   @icon TRLImage.jpg
   @ancestor TRLCustomImage.
   @pub }
  TRLImage=class(TRLCustomImage)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Center = ancestor /}
    property    Center;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop Picture = ancestor /}
    property    Picture;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Scaled = ancestor /}
    property    Scaled;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Stretch = ancestor /}
    property    Stretch;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;

  end;
  {/@class}


  { TRLDBImage }

  {@class TRLDBImage - Caixa de imagem ligada a campo de dataset.
   @icon TRLDBImage.jpg
   @ancestor TRLCustomDBImage.
   @pub }
  TRLDBImage=class(TRLCustomDBImage)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Center = ancestor /}
    property    Center;
    {@prop DataField = ancestor /}
    property    DataField;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Scaled = ancestor /}
    property    Scaled;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Stretch = ancestor /}
    property    Stretch;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;

  end;
  {/@class}


  { TRLDraw }

  {@class TRLDraw - Caixa de desenho para figuras geométricas.
   As figuras podem ser de um tipo pré-determinado ou customizado pelo usuário.
   @icon TRLDraw.jpg
   @ancestor TRLCustomDraw.
   @pub }
  TRLDraw=class(TRLCustomDraw)
  published

    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop Angle = ancestor /}
    property    Angle;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Brush = ancestor /}
    property    Brush;
    {@prop Center = ancestor /}
    property    Center;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DrawData = ancestor /}
    property    DrawData;
    {@prop DrawHeight = ancestor /}
    property    DrawHeight;
    {@prop DrawKind = ancestor /}
    property    DrawKind;
    {@prop DrawWidth = ancestor /}
    property    DrawWidth;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop Options = ancestor /}
    property    Options;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop Pen = ancestor /}
    property    Pen;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}


  { TRLPanel }

  {@class TRLPanel - Container para controles.
                     Utilize o TRLPanel como container para controles ou outros paineis.
   @icon TRLPanel.jpg
   @ancestor TRLCustomPanel.
   @pub }
  TRLPanel=class(TRLCustomPanel)
  published
    // properties

    {@prop Align = ancestor /}
    property    Align;
    {@prop Alignment = ancestor /}
    property    Alignment;
    {@prop Anchors = ancestor /}
    property    Anchors;
    {@prop AutoExpand = ancestor /}
    property    AutoExpand;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Background = ancestor /}
    property    Background;
    {@prop Behavior = ancestor /}
    property    Behavior;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Degrade = ancestor /}
    property    Degrade;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop Holder = ancestor /}
    property    Holder;
    {@prop HoldStyle = ancestor /}
    property    HoldStyle;
    {@prop InsideMargins = ancestor /}
    property    InsideMargins;
    {@prop Layout = ancestor /}
    property    Layout;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop SecondHolder = ancestor /}
    property    SecondHolder;
    {@prop SecondHoldStyle = ancestor /}
    property    SecondHoldStyle;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnDraw = ancestor /}
    property    OnDraw;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
    protected
    {@event OnMouseMove = ancestor /}
    property    OnMouseMove;///
    procedure WMMouseMove (var Msg: TLMessage); message LM_KEYDOWN;
  end;
  {/@class}


  { TRLBand }

  {@class TRLBand - Banda de impressão.
   Utilize a banda de impressão para representar registros de dados ou quebras de sequências de dados. Ela deve ser
   colocada dentro de um Report, Group ou SubDetail.
   O comportamento da banda é controlado através da propriedade BandType.
   @icon TRLBand.jpg
   @ancestor TRLCustomBand.
   @pub }
  TRLBand=class(TRLCustomBand)
  published

    // properties

    {@prop AlignToBottom = ancestor /}
    property    AlignToBottom;
    {@prop AutoExpand = ancestor /}
    property    AutoExpand;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Background = ancestor /}
    property    Background;
    {@prop BandType = ancestor /}
    property    BandType;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop CarbonCopies = ancestor /}
    property    CarbonCopies;
    {@prop Color = ancestor /}
    property    Color;
    {@prop Completion = ancestor /}
    property    Completion;
    {@prop Computable = ancestor /}
    property    Computable;
    {@prop Degrade = ancestor /}
    property    Degrade;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop GroupIndex = ancestor /}
    property    GroupIndex;
    {@prop InsideMargins = ancestor /}
    property    InsideMargins;
    {@prop IntegralHeight = ancestor /}
    property    IntegralHeight;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop Options = ancestor /}
    property    Options;
    {@prop PageBreaking = ancestor /}
    property    PageBreaking;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnMeasureHeight = ancestor /}
    property    OnMeasureHeight;
  end;
  {/@class}


  { TRLDetailGrid }

  {@class TRLDetailGrid - Banda de detalhe multi-colunas.
   Banda de tipo fixo btDetail. Ideal para a impressão de etiquetas e relatórios em colunas.
   @icon TRLDetailGrid.jpg
   @ancestor TRLCustomDetailGrid.
   @pub }
  TRLDetailGrid=class(TRLCustomDetailGrid)
  published

    // properties

    {@prop AutoExpand = ancestor /}
    property    AutoExpand;
    {@prop AutoSize = ancestor /}
    property    AutoSize;
    {@prop Background = ancestor /}
    property    Background;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop ColCount = ancestor /}
    property    ColCount;
    {@prop Color = ancestor /}
    property    Color;
    {@prop ColSpacing = ancestor /}
    property    ColSpacing;
    {@prop ColWidth = ancestor /}
    property    ColWidth;
    {@prop Completion = ancestor /}
    property    Completion;
    {@prop Computable = ancestor /}
    property    Computable;
    {@prop Degrade = ancestor /}
    property    Degrade;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop GroupIndex = ancestor /}
    property    GroupIndex;
    {@prop InsideMargins = ancestor /}
    property    InsideMargins;
    {@prop IntegralHeight = ancestor /}
    property    IntegralHeight;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop Organization = ancestor /}
    property    Organization;
    {@prop PageBreaking = ancestor /}
    property    PageBreaking;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
  end;
  {/@class}


  { TRLGroup }

  {@class TRLGroup - Sequência de registros de dados.
   Insira bands sobre um componente de grupo para imprimir sequências de registros de dados.
   A quebra de sequência dos registros será detectada automaticamente se for indicado um campo ou conjunto de campos
   através da propriedade DataFields, ou ainda pela expressão contida em DataFormula. A quebra também poderá ser feita
   interativamente durante as chamadas ao evento OnGetBreak. Um componente de grupo deve conter pelo menos uma band de
   detalhe para imprimir os registros da sequência. Adicionalmente, podem ser inseridos quaisquer outros tipos de band
   como, por exemplo: btSummary para mostrar somatórios e estatísticas ao final da sequência, ou btHeader para mostrar
   cabeçalhos. Grupos podem ser inseridos recursivamente dentro de outros grupos formando uma cadeia de sequências
   hierárquicas. Subdetalhes também podem ser inseridos dentro de grupos e vice-versa. Um grupo pode ser desativado
   sem no entanto influenciar na impressão dos seus controles através da propriedade Enabled.
   @links TRLSubDetail.
   @icon TRLGroup.jpg
   @ancestor TRLCustomGroup.
   @pub }
  TRLGroup=class(TRLCustomGroup)
  published

    // properties

    {@prop AllowedBands = ancestor /}
    property    AllowedBands;
    {@prop Background = ancestor /}
    property    Background;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DataFields = ancestor /}
    property    DataFields;
    {@prop DataFormula = ancestor /}
    property    DataFormula;
    {@prop Degrade = ancestor /}
    property    Degrade;
    {@prop Enabled = ancestor /}
    property    Enabled;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FooterMeasuring = ancestor /}
    property    FooterMeasuring;
    {@prop ForceMinBands = ancestor /}
    property    ForceMinBands;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop InsideMargins = ancestor /}
    property    InsideMargins;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop MaxBands = ancestor /}
    property    MaxBands;
    {@prop MinBands = ancestor /}
    property    MinBands;
    {@prop PageBreaking = ancestor /}
    property    PageBreaking;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnGetBreak = ancestor /}
    property    OnGetBreak;
  end;
  {/@class}
  

  { TRLSubDetail }

  {@class TRLSubDetail - Sub-relatório.
   Utilize o TRLSubDetail para imprimir registros ou sequências de dados relacionadas com os registros da fontes de
   dados principal. O controle de sub-detalhe é especialmente útil quando se quer listar registros de uma base que
   possui registros filhos ou relacionados (Master/Detail), aonde um TRLReport responderia pelos registros principais
   e o TRLSubDetail pelos registros filhos.
   @links TRLGroup.
   @icon TRLSubDetail.jpg
   @ancestor TRLCustomSubDetail.
   @pub }
  TRLSubDetail=class(TRLCustomSubDetail)
  published

    // properties

    {@prop AllowedBands = ancestor /}
    property    AllowedBands;
    {@prop Background = ancestor /}
    property    Background;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop Degrade = ancestor /}
    property    Degrade;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FooterMeasuring = ancestor /}
    property    FooterMeasuring;
    {@prop ForceMinBands = ancestor /}
    property    ForceMinBands;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop InsideMargins = ancestor /}
    property    InsideMargins;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop MaxBands = ancestor /}
    property    MaxBands;
    {@prop MinBands = ancestor /}
    property    MinBands;
    {@prop PageBreaking = ancestor /}
    property    PageBreaking;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop Positioning = ancestor /}
    property    Positioning;
    {@prop PrintEmpty = ancestor /}
    property    PrintEmpty;
    {@prop RangeCount = ancestor /}
    property    RangeCount;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop RecordRange = ancestor /}
    property    RecordRange;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnDataCount = ancestor /}
    property    OnDataCount;
    {@event OnDataRecord = ancestor /}
    property    OnDataRecord;
    {@event OnNeedData = ancestor /}
    property    OnNeedData;
  end;
  {/@class}


  { TRLReport }

  {@class TRLReport - Componente principal na construção de relatórios.
   Utilize o TRLReport como ponto de partida na confecção de qualquer relatório com o FortesReport. Um componente
   TRLReport pode listar registros de uma fonte de dados, solicitar os dados através de eventos em tempo de execução
   ou apenas imprimir páginas confeccionadas com os componentes da biblioteca.
   @icon TRLReport.jpg
   @ancestor TRLCustomReport.
   @pub }
  TRLReport=class(TRLCustomReport)
  published

    // properties

    {@prop AllowedBands = ancestor /}
    property    AllowedBands;
    {@prop AdjustableMargins = ancestor /}
    property    AdjustableMargins;
    {@prop Background = ancestor /}
    property    Background;
    {@prop BackgroundMode = ancestor /}
    property    BackgroundMode;
    {@prop Borders = ancestor /}
    property    Borders;
    {@prop Color = ancestor /}
    property    Color;
    {@prop DataSource = ancestor /}
    property    DataSource;
    {@prop DefaultFilter = ancestor /}
    property    DefaultFilter;
    {@prop Degrade = ancestor /}
    property    Degrade;
    {@prop FirstPageNumber = ancestor /}
    property    FirstPageNumber;
    {@prop Font = ancestor /}
    property    Font;
    {@prop FooterMeasuring = ancestor /}
    property    FooterMeasuring;
    {@prop ForceMinBands = ancestor /}
    property    ForceMinBands;
    {@prop ForcePrepare = ancestor /}
    property    ForcePrepare;
    {@prop FriendlyName = ancestor /}
    property    FriendlyName;
    {@prop InsideMargins = ancestor /}
    property    InsideMargins;
    {@prop Margins = ancestor /}
    property    Margins;
    {@prop MaxBands = ancestor /}
    property    MaxBands;
    {@prop MinBands = ancestor /}
    property    MinBands;
    {@prop NextReport = ancestor /}
    property    NextReport;
    {@prop PageSetup = ancestor /}
    property    PageSetup;
    {@prop ParentColor = ancestor /}
    property    ParentColor;
    {@prop ParentFont = ancestor /}
    property    ParentFont;
    {@prop PreviewOptions = ancestor /}
    property    PreviewOptions;
    {@prop PrintDialog = ancestor /}
    property    PrintDialog;
    {@prop PrintEmpty = ancestor /}
    property    PrintEmpty default True;
    {@prop PrintQuality = ancestor /}
    property    PrintQuality;
    {@prop RangeCount = ancestor /}
    property    RangeCount;
    {@prop RealBounds = ancestor /}
    property    RealBounds;
    {@prop RecordRange = ancestor /}
    property    RecordRange;
    {@prop ShowDesigners = ancestor /}
    property    ShowDesigners;
    {@prop ShowExplosion = ancestor /}
    property    ShowExplosion;
    {@prop ShowProgress = ancestor /}
    property    ShowProgress;
    {@prop ShowTracks = ancestor /}
    property    ShowTracks;
    {@prop Title = ancestor /}
    property    Title;
    {@prop ExpressionParser = ancestor /}
    property    ExpressionParser;
    {@prop Transparent = ancestor /}
    property    Transparent;
    {@prop Visible = ancestor /}
    property    Visible;

    // events

    {@event AfterPrint = ancestor /}
    property    AfterPrint;
    {@event BeforePrint = ancestor /}
    property    BeforePrint;
    {@event OnDataCount = ancestor /}
    property    OnDataCount;
    {@event OnDataRecord = ancestor /}
    property    OnDataRecord;
    {@event OnFilterText = ancestor /}
    property    OnFilterText;
    {@event OnNeedData = ancestor /}
    property    OnNeedData;
    {@event OnPageEnding = ancestor /}
    property    OnPageEnding;
    {@event OnPageStarting = ancestor /}
    property    OnPageStarting;
  end;
  {/@class}
  

{@proc LoadReportDialog - Executa um diálogo para carregar e pré-visualizar um relatório salvo em disco.
 Exibe um diálogo para carga de relatório salvo em disco e em seguida chama o preview padrão. :/}
procedure LoadReportDialog;

{@proc LoadReportFromFile - Carrega e pré-visualiza relatório a partir de um arquivo. :/}
procedure LoadReportFromFile(const aFileName:string);

{/@unit}

implementation

uses
  LCLIntf;

const
  faSlaveLeftSet  =[faLeft,faTop,faBottom,faLeftMost,faClient,faLeftTop,faLeftBottom,faCenterLeft,faClientLeft,faClientTop,faClientBottom,faWidth,faLeftOnly];
  faSlaveTopSet   =[faLeft,faTop,faRight,faLeftMost,faRightMost,faClient,faLeftTop,faRightTop,faCenterTop,faClientLeft,faClientTop,faClientRight,faHeight,faTopOnly];
  faSlaveRightSet =[faTop,faRight,faBottom,faRightMost,faClient,faRightTop,faRightBottom,faCenterRight,faClientTop,faClientRight,faClientBottom,faWidth,faRightOnly];
  faSlaveBottomSet=[faLeft,faRight,faBottom,faLeftMost,faRightMost,faClient,faLeftBottom,faRightBottom,faCenterBottom,faClientLeft,faClientRight,faClientBottom,faHeight,faBottomOnly];
  faLeftSet       =[faLeft,faLeftMost,faLeftTop,faLeftBottom,faCenterLeft,faClientLeft,faLeftOnly];
  faTopSet        =[faTop,faLeftTop,faRightTop,faCenterTop,faClientTop,faTopOnly];
  faRightSet      =[faRight,faRightMost,faRightTop,faRightBottom,faCenterRight,faClientRight,faRightOnly];
  faBottomSet     =[faBottom,faLeftBottom,faRightBottom,faCenterBottom,faClientBottom,faBottomOnly];
  //
  faSlaveWidthSet =faSlaveLeftSet*faSlaveRightSet;
  faSlaveHeightSet=faSlaveTopSet*faSlaveBottomSet;
  faFreeLeftSet   =[Low(TRLControlAlign)..High(TRLControlAlign)]-faSlaveLeftSet;
  faFreeTopSet    =[Low(TRLControlAlign)..High(TRLControlAlign)]-faSlaveTopSet;
  faFreeRightSet  =[Low(TRLControlAlign)..High(TRLControlAlign)]-faSlaveRightSet;
  faFreeBottomSet =[Low(TRLControlAlign)..High(TRLControlAlign)]-faSlaveBottomSet;
  faFreeWidthSet  =[Low(TRLControlAlign)..High(TRLControlAlign)]-faSlaveWidthSet;
  faFreeHeightSet =[Low(TRLControlAlign)..High(TRLControlAlign)]-faSlaveHeightSet;

const
  BandTypeNames:array[TRLBandType] of string=('Header','Title','ColumnHeader','Detail','ColumnFooter','Summary','Footer');
  InfoTypeNames:array[TRLInfoType] of string=('CarbonCopy','Date','DetailCount','FullDate','Hour','Junction','LastPageNumber','Mend','Now','PageNumber','PagePreview','Title','RecNo','CopyNo', 'CompanyName', 'FileDescription', 'FileVersion', 'InternalName', 'LegalCopyright', 'LegalTrademarks', 'OriginalFilename', 'ProductName', 'ProductVersion', 'Comments');
  faFromAlign  :array[TAlign] of TRLControlAlign=(
  faNone,faTop,faBottom,faLeft,faRight,faClient
{$ifndef DELPHI5}
,faNone
{$endif}
);
  fkFromAnchor :array[TAnchorKind] of TRLControlAnchorsType=(fkLeft,fkTop,fkRight,fkBottom);

procedure LoadReportFromFile(const aFileName:string);
var
  form      :TForm;
  report    :TRLCustomReport;
  savecursor:TCursor;
begin
  if not FileExists(aFileName) then
    raise Exception.Create(LS_FileNotFoundStr+' "'+aFileName+'"');
  //
  savecursor   :=Screen.Cursor;
  Screen.Cursor:=crHourGlass;
  try
    form:=TForm.Create(nil);
    try
      report:=TRLCustomReport.Create(form);
      report.LoadFromFile(aFileName);
      Screen.Cursor:=savecursor;
      report.Preview;
    finally
      FreeObj(form);
    end;
  except
    Screen.Cursor:=savecursor;
    raise;
  end;
end;

procedure LoadReportDialog;
var
  dialog:TOpenDialog;
begin
  dialog:=TOpenDialog.Create(nil);
  try
    dialog.DefaultExt :=FormatFileExt(RLFILEEXT);
    dialog.Filter     :=AddFileFilter(emptystr,CS_ProductTitleStr,RLFILEEXT);
    dialog.FilterIndex:=1;
    dialog.Title      :=LS_LoadReportStr;
    if dialog.Execute then
      LoadReportFromFile(dialog.FileName);
  finally
    FreeObj(dialog);
  end;
end;

// controle dentro de frame
function ControlWithin(aControl:TControl):TControl;
begin
  if (aControl is TCustomFrame) and (TCustomFrame(aControl).ControlCount>0) then
    Result:=ControlWithin(TCustomFrame(aControl).Controls[0])
  else
    Result:=aControl;
end;

function IsStaticCustomControl(aControl:TControl):boolean;
begin
  Result:=((aControl is TRLCustomControl) and not (aControl is TRLCustomSite)) or (aControl is TRLCustomPanel);
end;

function IsTransparent(aControl:TRLCustomControl):boolean;
begin
  Result:=aControl.Transparent;
end;

// alinhamento de controle
function GetControlAlignOf(aControl:TControl):TRLControlAlign;
begin
  aControl:=ControlWithin(aControl);
  if aControl is TRLCustomControl then
    Result:=TRLCustomControl(aControl).Align
  else if aControl is TControl then
    Result:=faFromAlign[TControl(aControl).Align]
  else
    Result:=faNone;
end;

function GetControlAnchorsOf(aControl:TControl):TRLControlAnchors;
var
  i:TAnchorKind;
begin
  if aControl is TRLCustomControl then
    Result:=TRLCustomControl(aControl).Anchors
  else if aControl is TControl then
  begin
    Result:=[];
    for i:=Low(TAnchorKind) to High(TAnchorKind) do
      if i in TControl(aControl).Anchors then
        Result:=Result+[fkFromAnchor[i]];
  end
  else
    Result:=[];
end;

function GetScreenLeft(aControl:TControl):integer; overload;
begin
  Result:=aControl.Left;
  if aControl.Parent<>nil then
    Inc(Result,GetScreenLeft(aControl.Parent));
end;
function GetScreenLeft(aControl:TControl; aLeft:integer):integer; overload;
begin
  Result:=aLeft;
  if aControl.Parent<>nil then
    Inc(Result,GetScreenLeft(aControl.Parent));
end;

procedure SetScreenLeft(aControl:TControl; aLeft:integer); overload;
begin
  aControl.Left:=aControl.Left+aLeft-GetScreenLeft(aControl);
end;
procedure SetScreenLeft(aControl:TControl; aLeft:integer; var aResult:integer); overload;
begin
  aResult:=aControl.Left+aLeft-GetScreenLeft(aControl);
end;

function GetScreenTop(aControl:TControl):integer; overload;
begin
  Result:=aControl.Top;
  if aControl.Parent<>nil then
    Inc(Result,GetScreenTop(aControl.Parent));
end;
function GetScreenTop(aControl:TControl; aTop:integer):integer; overload;
begin
  Result:=aTop;
  if aControl.Parent<>nil then
    Inc(Result,GetScreenTop(aControl.Parent));
end;

procedure SetScreenTop(aControl:TControl; aTop:integer); overload;
begin
  aControl.Top:=aControl.Top+aTop-GetScreenTop(aControl);
end;
procedure SetScreenTop(aControl:TControl; aTop:integer; var aResult:integer); overload;
begin
  aResult:=aControl.Top+aTop-GetScreenTop(aControl);
end;

function GetScreenPos(aControl:TControl):TPoint; overload;
begin
  Result:=Point(aControl.Left,aControl.Top);
  if aControl.Parent<>nil then
    with GetScreenPos(aControl.Parent) do
    begin
      Inc(Result.x,x);
      Inc(Result.y,y);
    end;
end;
function GetScreenPos(aControl:TControl; aPos:TPoint):TPoint; overload;
begin
  Result:=aPos;
  if aControl.Parent<>nil then
    with GetScreenPos(aControl.Parent) do
    begin
      Inc(Result.x,x);
      Inc(Result.y,y);
    end;
end;

procedure SetScreenPos(aControl:TControl; aPos:TPoint); overload;
var
  p:TPoint;
begin
  p:=GetScreenPos(aControl);
  with aControl do
    SetBounds(Left+aPos.x-p.x,Top+aPos.y-p.y,Width,Height);
end;
procedure SetScreenPos(aControl:TControl; aPos:TPoint; var aResult:TPoint); overload;
var
  p:TPoint;
begin
  p:=GetScreenPos(aControl);
  aResult.X:=aControl.Left+aPos.x-p.x;
  aResult.Y:=aControl.Top +aPos.y-p.y;
end;

function ReportOrNIL(aSource:TObject):TRLCustomReport;
begin
  if Assigned(aSource) and (aSource is TRLCustomReport) then
    Result:=TRLCustomReport(aSource)
  else
    Result:=nil;
end;

{ TRLBorders }

constructor TRLBorders.Create(aOwner:TRLCustomControl);
begin
  // variables
  fParentControl:=aOwner;
  fSides        :=sdNone;
  //fDrawLeft     :=False;
  //fDrawTop      :=False;
  //fDrawRight    :=False;
  //fDrawBottom   :=False;
  fWidth        :=1;
  fColor        :=clBlack;
  fStyle        :=bsSolid;
  //fFixedLeft    :=False;
  //fFixedTop     :=False;
  //fFixedRight   :=False;
  //fFixedBottom  :=False;
  //
  inherited Create;
end;

procedure TRLBorders.AdjustParent;
begin
  with ParentControl do
  begin
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRLBorders.PaintTo(aCanvas:TCanvas; aRect:TRect);
var
  w:integer;
begin
  if Width>0 then
  begin
    w:=Self.Width;
    aCanvas.Pen.Color  :=Self.Color;
    aCanvas.Pen.Style  :=psSolid;
    aCanvas.Pen.Mode   :=pmCopy;
    aCanvas.Pen.Width  :=1;
    aCanvas.Brush.Color:=Self.Color;
    aCanvas.Brush.Style:=bsSolid;
    if CanDrawLeft then
      aCanvas.Rectangle(aRect.Left,aRect.Top,aRect.Left+w,aRect.Bottom);
    if CanDrawTop then
      aCanvas.Rectangle(aRect.Left,aRect.Top,aRect.Right,aRect.Top+w);
    if CanDrawRight then
      aCanvas.Rectangle(aRect.Right-w,aRect.Top,aRect.Right,aRect.Bottom);
    if CanDrawBottom then
      aCanvas.Rectangle(aRect.Left,aRect.Bottom-w,aRect.Right,aRect.Bottom);
  end;
end;

procedure TRLBorders.PaintTo(aSurface:TRLGraphicSurface; aRect:TRect);
var
  w:integer;
begin
  if Width>0 then
  begin
    w:=Self.Width;
    aSurface.Pen.Color  :=Self.Color;
    aSurface.Pen.Style  :=psSolid;
    aSurface.Pen.Mode   :=pmCopy;
    aSurface.Pen.Width  :=1;
    aSurface.Brush.Color:=Self.Color;
    aSurface.Brush.Style:=bsSolid;
    if CanDrawLeft then
      aSurface.Rectangle(aRect.Left,aRect.Top,aRect.Left+w,aRect.Bottom);
    if CanDrawTop then
      aSurface.Rectangle(aRect.Left,aRect.Top,aRect.Right,aRect.Top+w);
    if CanDrawRight then
      aSurface.Rectangle(aRect.Right-w,aRect.Top,aRect.Right,aRect.Bottom);
    if CanDrawBottom then
      aSurface.Rectangle(aRect.Left,aRect.Bottom-w,aRect.Right,aRect.Bottom);
  end;
end;

procedure TRLBorders.CheckSides;
begin
  if fDrawLeft and fDrawTop and fDrawRight and fDrawBottom and (fSides=sdAll) then
  else if not fDrawLeft and not fDrawTop and not fDrawRight and not fDrawBottom and (fSides=sdNone) then
  else
    fSides:=sdCustom;
end;

procedure TRLBorders.SetDrawLeft(const aValue:boolean);
begin
  if aValue=fDrawLeft then
    Exit;
  fDrawLeft:=aValue;
  if not fDrawLeft then
    fFixedLeft:=False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetDrawTop(const aValue:boolean);
begin
  if aValue=fDrawTop then
    Exit;
  fDrawTop:=aValue;
  if not fDrawTop then
    fFixedTop:=False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetDrawRight(const aValue:boolean);
begin
  if aValue=fDrawRight then
    Exit;
  fDrawRight:=aValue;
  if not fDrawRight then
    fFixedRight:=False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetDrawBottom(const aValue:boolean);
begin
  if aValue=fDrawBottom then
    Exit;
  fDrawBottom:=aValue;
  if not fDrawBottom then
    fFixedBottom:=False;
  CheckSides;
  AdjustParent;
end;

procedure TRLBorders.SetWidth(const aValue:integer);
begin
  if aValue=fWidth then
    Exit;
  if aValue<0 then
    Exit;
  fWidth:=aValue;
  AdjustParent;
end;

procedure TRLBorders.SetColor(const aValue:TColor);
begin
  if aValue=fColor then
    Exit;
  fColor:=aValue;
  ParentControl.Invalidate;
end;

procedure TRLBorders.SetStyle(const aValue:TBrushStyle);
begin
  if aValue=fStyle then
    Exit;
  fStyle:=aValue;
  ParentControl.Invalidate;
end;

procedure TRLBorders.SetParentControl(const aValue:TRLCustomControl);
begin
  if aValue=fParentControl then
    Exit;
  fParentControl:=aValue;
  AdjustParent;
end;

procedure TRLBorders.SetSides(const aValue:TRLBorderSides);
begin
  if aValue=fSides then
    Exit;
  fSides:=aValue;
  case fSides of
    sdNone: begin
              fDrawLeft   :=False;
              fDrawTop    :=False;
              fDrawRight  :=False;
              fDrawBottom :=False;
              fFixedLeft  :=False;
              fFixedTop   :=False;
              fFixedRight :=False;
              fFixedBottom:=False;
            end;
    sdAll : begin
              fDrawLeft  :=True;
              fDrawTop   :=True;
              fDrawRight :=True;
              fDrawBottom:=True;
            end;
  else
    Exit;
  end;
  AdjustParent;
end;

procedure TRLBorders.SetFixedLeft(const aValue:boolean);
begin
  if aValue=fFixedLeft then
    Exit;
  fFixedLeft:=aValue;
  if aValue and not DrawLeft then
    DrawLeft:=True;
end;

procedure TRLBorders.SetFixedTop(const aValue:boolean);
begin
  if aValue=fFixedTop then
    Exit;
  fFixedTop:=aValue;
  if aValue and not DrawTop then
    DrawTop:=True;
end;

procedure TRLBorders.SetFixedRight(const aValue:boolean);
begin
  if aValue=fFixedRight then
    Exit;
  fFixedRight:=aValue;
  if aValue and not DrawRight then
    DrawRight:=True;
end;

procedure TRLBorders.SetFixedBottom(const aValue:boolean);
begin
  if aValue=fFixedBottom then
    Exit;
  fFixedBottom:=aValue;
  if aValue and not fDrawBottom then
    DrawBottom:=True;
end;

function TRLBorders.CanDrawLeft:boolean;
begin
  Result:=DrawLeft and (FixedLeft or ((ParentControl.MasterReport<>nil) and (ParentControl.MasterReport.PrintQuality=pqFullFeature)));
end;

function TRLBorders.CanDrawTop:boolean;
begin
  Result:=DrawTop and (FixedTop or ((ParentControl.MasterReport<>nil) and (ParentControl.MasterReport.PrintQuality=pqFullFeature)));
end;

function TRLBorders.CanDrawRight:boolean;
begin
  Result:=DrawRight and (FixedRight or ((ParentControl.MasterReport<>nil) and (ParentControl.MasterReport.PrintQuality=pqFullFeature)));
end;

function TRLBorders.CanDrawBottom:boolean;
begin
  Result:=DrawBottom and (FixedBottom or ((ParentControl.MasterReport<>nil) and (ParentControl.MasterReport.PrintQuality=pqFullFeature)));
end;

function TRLBorders.IsCustom: boolean;
begin
  Result:=(fSides=sdCustom);
end;

{ TRLMargins }

constructor TRLMargins.Create(aOwner:TRLCustomControl);
begin
  // variables
  fParentControl      :=aOwner;
  fLeftMargin         :=0;
  fTopMargin          :=0;
  fRightMargin        :=0;
  fBottomMargin       :=0;
  fDefaultLeftMargin  :=0;
  fDefaultTopMargin   :=0;
  fDefaultRightMargin :=0;
  fDefaultBottomMargin:=0;
  //
  inherited Create;
end;

procedure TRLMargins.ReadLeftMargin(Reader:TReader);
begin
  fLeftMargin:=Reader.ReadFloat;
end;

procedure TRLMargins.WriteLeftMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fLeftMargin);
end;

procedure TRLMargins.ReadTopMargin(Reader:TReader);
begin
  fTopMargin:=Reader.ReadFloat;
end;

procedure TRLMargins.WriteTopMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fTopMargin);
end;

procedure TRLMargins.ReadRightMargin(Reader:TReader);
begin
  fRightMargin:=Reader.ReadFloat;
end;

procedure TRLMargins.WriteRightMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fRightMargin);
end;

procedure TRLMargins.ReadBottomMargin(Reader:TReader);
begin
  fBottomMargin:=Reader.ReadFloat;
end;

procedure TRLMargins.WriteBottomMargin(Writer:TWriter);
begin
  Writer.WriteFloat(fBottomMargin);
end;

procedure TRLMargins.DefineProperties(Filer:TFiler);
begin
  Filer.DefineProperty('LeftMargin'  ,ReadLeftMargin  ,WriteLeftMargin  ,fLeftMargin<>fDefaultLeftMargin);
  Filer.DefineProperty('TopMargin'   ,ReadTopMargin   ,WriteTopMargin   ,fTopMargin<>fDefaultTopMargin);
  Filer.DefineProperty('RightMargin' ,ReadRightMargin ,WriteRightMargin ,fRightMargin<>fDefaultRightMargin);
  Filer.DefineProperty('BottomMargin',ReadBottomMargin,WriteBottomMargin,fBottomMargin<>fDefaultBottomMargin);
end;

procedure TRLMargins.SetDefaults(aLeft,aTop,aRight,aBottom:double);
begin
  fDefaultLeftMargin  :=aLeft;
  fDefaultTopMargin   :=aTop;
  fDefaultRightMargin :=aRight;
  fDefaultBottomMargin:=aBottom;
  fLeftMargin         :=aLeft;
  fTopMargin          :=aTop;
  fRightMargin        :=aRight;
  fBottomMargin       :=aBottom;
end;

procedure TRLMargins.AdjustParent;
begin
  with ParentControl do
  begin
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRLMargins.SetLeftMargin(const aValue:double);
begin
  if aValue=fLeftMargin then
    Exit;
  fLeftMargin:=aValue;
  AdjustParent;
end;

procedure TRLMargins.SetRightMargin(const aValue:double);
begin
  if aValue=fRightMargin then
    Exit;
  fRightMargin:=aValue;
  AdjustParent;
end;

procedure TRLMargins.SetTopMargin(const aValue:double);
begin
  if aValue=fTopMargin then
    Exit;
  fTopMargin:=aValue;
  AdjustParent;
end;

procedure TRLMargins.SetBottomMargin(const aValue:double);
begin
  if aValue=fBottomMargin then
    Exit;
  fBottomMargin:=aValue;
  AdjustParent;
end;

procedure TRLMargins.Assign(Source:TPersistent);
begin
  if Source is TRLMargins then
    with TRLMargins(Source) do
    begin
      Self.LeftMargin   :=LeftMargin;
      Self.TopMargin    :=TopMargin;
      Self.RightMargin  :=RightMargin;
      Self.BottomMargin :=BottomMargin;
    end
  else
    inherited;
end;

{ TRLPageSetup }

constructor TRLPageSetup.Create(aOwner:TRLCustomReport);
begin
  // variables
  fParentReport  :=ReportOrNIL(aOwner);
  fPaperSize     :=fpA4;
  fPaperWidth    :=PaperInfo[fPaperSize].Width;
  fPaperHeight   :=PaperInfo[fPaperSize].Height;
  fOrientation   :=poPortrait;
  //fForceEmulation:=False;
  //
  inherited Create;
end;

procedure TRLPageSetup.AdjustParent;
begin
  with ParentReport do
  begin
    ReloadPrinter;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TRLPageSetup.SetPaperSize(const aValue:TRLPaperSize);
begin
  if aValue=fPaperSize then
    Exit;
  fPaperSize:=aValue;
  if fPaperSize<>fpCustom then
  begin
    fPaperWidth :=PaperInfo[fPaperSize].Width;
    fPaperHeight:=PaperInfo[fPaperSize].Height;
  end;
  AdjustParent;
end;

procedure TRLPageSetup.SetPaperHeight(const aValue:double);
begin
  if aValue=fPaperHeight then
    Exit;
  if (fPaperSize<>fpCustom) or (aValue=0) then
    Exit;
  fPaperHeight:=aValue;
  AdjustParent;
end;

procedure TRLPageSetup.SetPaperWidth(const aValue:double);
begin
  if aValue=fPaperWidth then
    Exit;
  if (fPaperSize<>fpCustom) or (aValue=0) then
    Exit;
  fPaperWidth:=aValue;
  AdjustParent;
end;

procedure TRLPageSetup.SetOrientation(const aValue:TRLPageOrientation);
begin
  if aValue=fOrientation then
    Exit;
  fOrientation:=aValue;
  AdjustParent;
end;

function TRLPageSetup.GetOrientedWidth:double;
begin
  if fOrientation=poPortrait then
    Result:=fPaperWidth
  else
    Result:=fPaperHeight;
end;

function TRLPageSetup.GetOrientedHeight:double;
begin
  if fOrientation=poPortrait then
    Result:=fPaperHeight
  else
    Result:=fPaperWidth;
end;    

procedure TRLPageSetup.SetOrientedHeight(const aValue:double);
begin
  if fOrientation=poPortrait then
    fPaperHeight:=aValue
  else
    fPaperWidth:=aValue;
end;

procedure TRLPageSetup.SetOrientedWidth(const aValue:double);
begin
  if fOrientation=poPortrait then
    fPaperWidth:=aValue
  else
    fPaperHeight:=aValue;
end;

function TRLPageSetup.IsCustomPaperSize: boolean;
begin
  Result:=(fPaperSize=fpCustom);
end;

procedure TRLPageSetup.Assign(Source: TRLPageSetup);
begin
  PaperSize     :=Source.PaperSize;
  Orientation   :=Source.Orientation;
  PaperWidth    :=Source.PaperWidth;
  PaperHeight   :=Source.PaperHeight;
  ForceEmulation:=Source.ForceEmulation;
end;

{ TRLBackground }

constructor TRLBackground.Create(aOwner:TRLCustomSite);
begin
  // variables
  fParentSite :=aOwner;
  fAlign      :=faClient;
  fArrange    :=baAligned;
  fAutoSize   :=True;
  fHeight     :=40;
  //fStretch    :=False;
  fWidth      :=40;
  // objects
  fPicture    :=TPicture.Create;
  //
  inherited Create;
end;

destructor TRLBackground.Destroy;
begin
  FreeObj(fPicture);
  //
  inherited;
end;

procedure TRLBackground.AdjustSize;
begin
  if (fPicture.Graphic<>nil) and not fPicture.Graphic.Empty then
  begin
    fWidth :=fPicture.Width;
    fHeight:=fPicture.Height;
  end;
end;

procedure TRLBackground.PaintTo(aCanvas:TCanvas; aRect:TRect);
var
  x,y,d:integer;
  r:TRect;
  b:TBitmap;
begin
  if (fPicture.Graphic=nil) or fPicture.Graphic.Empty then
    Exit;
  case fArrange of
    baAligned    : begin
                     case fAlign of
                       faNone        : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Left+fWidth,aRect.Top+fHeight);
                       faLeft,
                       faLeftMost    : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Left+fWidth,aRect.Bottom);
                       faTop         : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Right,aRect.Top+fHeight);
                       faRight,
                       faRightMost   : r:=Classes.Rect(aRect.Right-fWidth,aRect.Top,aRect.Right,aRect.Bottom);
                       faBottom      : r:=Classes.Rect(aRect.Left,aRect.Bottom-fHeight,aRect.Right,aRect.Bottom);
                       faClient      : r:=aRect;
                       faLeftTop     : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Left+fWidth,aRect.Top+fHeight);
                       faRightTop    : r:=Classes.Rect(aRect.Right-fWidth,aRect.Top,aRect.Right,aRect.Top+fHeight);
                       faLeftBottom  : r:=Classes.Rect(aRect.Left,aRect.Bottom-fHeight,aRect.Left+fWidth,aRect.Bottom);
                       faRightBottom : r:=Classes.Rect(aRect.Right-fWidth,aRect.Bottom-fHeight,aRect.Right,aRect.Bottom);
                       faCenter      : begin
                                         x:=(aRect.Left+aRect.Right -fWidth ) div 2;
                                         y:=(aRect.Top +aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(x,y,x+fWidth,y+fHeight);
                                       end;
                       faCenterLeft,
                       faClientLeft  : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Left,y,aRect.Left+fWidth,y+fHeight);
                                       end;
                       faCenterTop,
                       faClientTop   : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Top,x+fWidth,aRect.Top+fHeight);
                                       end;
                       faCenterRight,
                       faClientRight : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Right-fWidth,y,aRect.Right,y+fHeight);
                                       end;
                       faCenterBottom,
                       faClientBottom: begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Bottom-fHeight,x+fWidth,aRect.Bottom);
                                       end;
                       faHeight      : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Top,x+fWidth,aRect.Bottom);
                                       end;
                       faWidth       : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Left,y,aRect.Right,y+fHeight);
                                       end;
                       faLeftOnly    : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Left,y,aRect.Left+fWidth,y+fHeight);
                                       end;
                       faRightOnly   : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Right-fWidth,y,aRect.Right,y+fHeight);
                                       end;
                       faTopOnly     : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Top,x+fWidth,aRect.Top+fHeight);
                                       end;
                       faBottomOnly  : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Bottom-fHeight,x+fWidth,aRect.Bottom);
                                       end;
                     end;
                     if fStretch then
                     else
                     begin
                       r.Right :=r.Left+fPicture.Width;
                       r.Bottom:=r.Top +fPicture.Height;
                     end;
                     aCanvas.StretchDraw(r,fPicture.Graphic);
                   end;
    baCenter     : begin
                     r.Left:=(aRect.Left+aRect.Right -fWidth ) div 2;
                     r.Top :=(aRect.Top +aRect.Bottom-fHeight) div 2;
                     if fStretch then
                     begin
                       r.Right :=r.Left+fWidth;
                       r.Bottom:=r.Top +fHeight;
                     end
                     else
                     begin
                       r.Right :=r.Left+fPicture.Width;
                       r.Bottom:=r.Top +fPicture.Height;
                     end;
                     aCanvas.StretchDraw(r,fPicture.Graphic);
                   end;
    baSidebySide,
    baDistributed: begin
                     b:=AuxBitmapNeeded;
                     b.Width :=RectWidth(aRect);
                     b.Height:=RectHeight(aRect);
                     d:=0;
                     y:=0;
                     repeat
                       x:=-d*fWidth div 2;
                       repeat
                         r.Left:=x;
                         r.Top :=y;
                         if fStretch then
                         begin
                           r.Right :=r.Left+fWidth;
                           r.Bottom:=r.Top +fHeight;
                         end
                         else
                         begin
                           r.Right :=r.Left+fPicture.Width;
                           r.Bottom:=r.Top +fPicture.Height;
                         end;
                         b.Canvas.StretchDraw(r,fPicture.Graphic);
                         Inc(x,fWidth);
                       until x>b.Width;
                       if fArrange=baDistributed then
                         d:=1-d;
                       Inc(y,fHeight);
                     until y>b.Height;
                     aCanvas.StretchDraw(aRect,b);
                   end;
  end;
end;

procedure TRLBackground.PaintTo(aSurface:TRLGraphicSurface; aRect:TRect); 
var
  x,y,d:integer;
  r:TRect;
  b:TBitmap;
begin
  if (fPicture.Graphic=nil) or fPicture.Graphic.Empty then
    Exit;
  case fArrange of
    baAligned    : begin
                     case fAlign of
                       faNone        : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Left+fWidth,aRect.Top+fHeight);
                       faLeft,
                       faLeftMost    : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Left+fWidth,aRect.Bottom);
                       faTop         : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Right,aRect.Top+fHeight);
                       faRight,
                       faRightMost   : r:=Classes.Rect(aRect.Right-fWidth,aRect.Top,aRect.Right,aRect.Bottom);
                       faBottom      : r:=Classes.Rect(aRect.Left,aRect.Bottom-fHeight,aRect.Right,aRect.Bottom);
                       faClient      : r:=aRect;
                       faLeftTop     : r:=Classes.Rect(aRect.Left,aRect.Top,aRect.Left+fWidth,aRect.Top+fHeight);
                       faRightTop    : r:=Classes.Rect(aRect.Right-fWidth,aRect.Top,aRect.Right,aRect.Top+fHeight);
                       faLeftBottom  : r:=Classes.Rect(aRect.Left,aRect.Bottom-fHeight,aRect.Left+fWidth,aRect.Bottom);
                       faRightBottom : r:=Classes.Rect(aRect.Right-fWidth,aRect.Bottom-fHeight,aRect.Right,aRect.Bottom);
                       faCenter      : begin
                                         x:=(aRect.Left+aRect.Right -fWidth ) div 2;
                                         y:=(aRect.Top +aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(x,y,x+fWidth,y+fHeight);
                                       end;
                       faCenterLeft,
                       faClientLeft  : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Left,y,aRect.Left+fWidth,y+fHeight);
                                       end;
                       faCenterTop,
                       faClientTop   : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Top,x+fWidth,aRect.Top+fHeight);
                                       end;
                       faCenterRight,
                       faClientRight : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Right-fWidth,y,aRect.Right,y+fHeight);
                                       end;
                       faCenterBottom,
                       faClientBottom: begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Bottom-fHeight,x+fWidth,aRect.Bottom);
                                       end;
                       faHeight      : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Top,x+fWidth,aRect.Bottom);
                                       end;
                       faWidth       : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Left,y,aRect.Right,y+fHeight);
                                       end;
                       faLeftOnly    : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Left,y,aRect.Left+fWidth,y+fHeight);
                                       end;
                       faRightOnly   : begin
                                         y:=(aRect.Top+aRect.Bottom-fHeight) div 2;
                                         r:=Classes.Rect(aRect.Right-fWidth,y,aRect.Right,y+fHeight);
                                       end;
                       faTopOnly     : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Top,x+fWidth,aRect.Top+fHeight);
                                       end;
                       faBottomOnly  : begin
                                         x:=(aRect.Left+aRect.Right-fWidth) div 2;
                                         r:=Classes.Rect(x,aRect.Bottom-fHeight,x+fWidth,aRect.Bottom);
                                       end;
                     end;
                     if fStretch then
                     else
                     begin
                       r.Right :=r.Left+fPicture.Width;
                       r.Bottom:=r.Top +fPicture.Height;
                     end;
                     aSurface.StretchDraw(r,fPicture.Graphic);
                   end;
    baCenter     : begin
                     r.Left:=(aRect.Left+aRect.Right -fWidth ) div 2;
                     r.Top :=(aRect.Top +aRect.Bottom-fHeight) div 2;
                     if fStretch then
                     begin
                       r.Right :=r.Left+fWidth;
                       r.Bottom:=r.Top +fHeight;
                     end
                     else
                     begin
                       r.Right :=r.Left+fPicture.Width;
                       r.Bottom:=r.Top +fPicture.Height;
                     end;
                     aSurface.StretchDraw(r,fPicture.Graphic);
                   end;
    baSidebySide,
    baDistributed: begin
                     b:=AuxBitmapNeeded;
                     b.Width :=RectWidth(aRect);
                     b.Height:=RectHeight(aRect);
                     d:=0;
                     y:=0;
                     repeat
                       x:=-d*fWidth div 2;
                       repeat
                         r.Left:=x;
                         r.Top :=y;
                         if fStretch then
                         begin
                           r.Right :=r.Left+fWidth;
                           r.Bottom:=r.Top +fHeight;
                         end
                         else
                         begin
                           r.Right :=r.Left+fPicture.Width;
                           r.Bottom:=r.Top +fPicture.Height;
                         end;
                         b.Canvas.StretchDraw(r,fPicture.Graphic);
                         Inc(x,fWidth);
                       until x>b.Width;
                       if fArrange=baDistributed then
                         d:=1-d;
                       Inc(y,fHeight);
                     until y>b.Height;
                     aSurface.StretchDraw(aRect,b);
                   end;
  end;
end;

procedure TRLBackground.SetAlign(const aValue:TRLControlAlign);
begin
  if aValue=fAlign then
    Exit;
  fAlign:=aValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetArrange(const aValue:TRLImageArrange);
begin
  if aValue=fArrange then
    Exit;
  fArrange:=aValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetAutoSize(const aValue:boolean);
begin
  if aValue=fAutoSize then
    Exit;
  fAutoSize:=aValue;
  if aValue then
    AdjustSize;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetHeight(const aValue:integer);
begin
  if aValue=fHeight then
    Exit;
  fHeight:=aValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetPicture(const aValue:TPicture);
begin
  fPicture.Assign(aValue);
  if fAutoSize then
    AdjustSize;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetStretch(const aValue:boolean);
begin
  if aValue=fStretch then
    Exit;
  fStretch:=aValue;
  ParentSite.Invalidate;
end;

procedure TRLBackground.SetWidth(const aValue:integer);
begin
  if aValue=fWidth then
    Exit;
  fWidth:=aValue;
  ParentSite.Invalidate;
end;

{ TRLDegradeEffect }

constructor TRLDegradeEffect.Create(aOwner:TRLCustomSite);
begin
  // variables
  fParentSite   :=aOwner;
  fDirection    :=ddNone;
  fOppositeColor:=clBlack;
  fGranularity  :=1;
  //
  inherited Create;
end;

procedure TRLDegradeEffect.PaintTo(aCanvas:TCanvas; aRect:TRect; aColor:TColor);
type
  TRGBInfo=record
    red,green,blue,pallete:byte;
  end;
var
  i,barcount,barwidth,totalwidth:integer;
  p:double;
  r:TRect;
  cl1,cl2:TRGBInfo;
function RGBInfo(Color:TColor):TRGBInfo;
var
  l:integer;
begin
  l:=ColorToRGB(Color);
  Move(l,Result,4);
end;
begin
  cl1:=RGBInfo(fParentSite.Color);
  cl2:=RGBInfo(fOppositeColor);
  if fDirection=ddVertical then
    totalwidth:=RectHeight(aRect)
  else
    totalwidth:=RectWidth(aRect);
  barwidth:=fGranularity; 
  barcount:=(totalwidth+barwidth-1) div barwidth;
  //
  for i:=0 to barcount-1 do
  begin
    p:=i/barcount;
    aCanvas.Brush.Style:=bsSolid;
    aCanvas.Brush.Color:=Round(p*cl2.Blue )*65536+(cl1.Blue -Round(p*cl1.Blue ))*65536+
                         Round(p*cl2.Green)*256  +(cl1.Green-Round(p*cl1.Green))*256  +
                         Round(p*cl2.Red  )*1    +(cl1.Red  -Round(p*cl1.Red  ))*1;
    if fDirection=ddVertical then
    begin
      r.Left  :=aRect.Left;
      r.Top   :=aRect.Top+i*barwidth;
      r.Right :=aRect.Right;
      r.Bottom:=r.Top+barwidth;
      if r.Bottom>aRect.Bottom then
        r.Bottom:=aRect.Bottom;
    end
    else
    begin
      r.Left  :=aRect.Left+i*barwidth;
      r.Top   :=aRect.Top;
      r.Right :=r.Left+barwidth;
      r.Bottom:=aRect.Bottom;
      if r.Right>aRect.Right then
        r.Right:=aRect.Right;
    end;
    aCanvas.FillRect(r);
  end;
end;

procedure TRLDegradeEffect.PaintTo(aSurface:TRLGraphicSurface; aRect:TRect; aColor:TColor);
type
  TRGBInfo=record
    red,green,blue,pallete:byte;
  end;
var
  i,barcount,barwidth,totalwidth:integer;
  p:double;
  r:TRect;
  cl1,cl2:TRGBInfo;
function RGBInfo(Color:TColor):TRGBInfo;
var
  l:integer;
begin
  l:=ColorToRGB(Color);
  Move(l,Result,4);
end;
begin
  cl1:=RGBInfo(fParentSite.Color);
  cl2:=RGBInfo(fOppositeColor);
  if fDirection=ddVertical then
    totalwidth:=RectHeight(aRect)
  else
    totalwidth:=RectWidth(aRect);
  barwidth:=fGranularity; 
  barcount:=(totalwidth+barwidth-1) div barwidth;
  //
  for i:=0 to barcount-1 do
  begin
    p:=i/barcount;
    aSurface.Brush.Style:=bsSolid;
    aSurface.Brush.Color:=Round(p*cl2.Blue )*65536+(cl1.Blue -Round(p*cl1.Blue ))*65536+
                          Round(p*cl2.Green)*256  +(cl1.Green-Round(p*cl1.Green))*256  +
                          Round(p*cl2.Red  )*1    +(cl1.Red  -Round(p*cl1.Red  ))*1;
    if fDirection=ddVertical then
    begin
      r.Left  :=aRect.Left;
      r.Top   :=aRect.Top+i*barwidth;
      r.Right :=aRect.Right;
      r.Bottom:=r.Top+barwidth;
      if r.Bottom>aRect.Bottom then
        r.Bottom:=aRect.Bottom;
    end
    else
    begin
      r.Left  :=aRect.Left+i*barwidth;
      r.Top   :=aRect.Top;
      r.Right :=r.Left+barwidth;
      r.Bottom:=aRect.Bottom;
      if r.Right>aRect.Right then
        r.Right:=aRect.Right;
    end;
    aSurface.FillRect(r);
  end;
end;

procedure TRLDegradeEffect.SetDirection(const aValue:TRLDegradeDirection);
begin
  if aValue=fDirection then
    Exit;
  fDirection:=aValue;
  fParentSite.Invalidate;
end;

procedure TRLDegradeEffect.SetGranularity(const aValue:integer);
begin
  if aValue=fGranularity then
    Exit;
  fGranularity:=aValue;
  fParentSite.Invalidate;
end;

procedure TRLDegradeEffect.SetOppositeColor(const aValue:TColor);
begin
  if aValue=fOppositeColor then
    Exit;
  fOppositeColor:=aValue;
  fParentSite.Invalidate;
end;

{ TRLSortedBands }

constructor TRLSortedBands.Create;
var
  i:TRLBandType;
begin
  for i:=Low(TRLBandType) to High(TRLBandType) do
    with fTypes[i] do
    begin
      List   :=TList.Create;
      Printed:=False;
    end;
  //
  inherited Create;
end;

destructor TRLSortedBands.Destroy;
var
  i:TRLBandType;
begin
  for i:=Low(TRLBandType) to High(TRLBandType) do
    with fTypes[i] do
      FreeObj(List);
  //    
  inherited;
end;

procedure TRLSortedBands.Add(aBand:TRLCustomSite);
var
  i:integer;
  t:TRLBandType;
begin
{$ifdef KYLIX1}
  t:=btDetail;
{$endif}
  if aBand is TRLCustomBand then
    t:=TRLCustomBand(aBand).BandType
  else if aBand is TRLCustomSubDetail then
    t:=TRLCustomSubDetail(aBand).Positioning
  else
    Exit;
  with fTypes[t].List do
  begin
    i:=0;
    while (i<=Count-1) and (aBand.Top>=TRLCustomSite(Items[i]).Top) do
      Inc(i);
    if i>Count-1 then
      Add(aBand)
    else
      Insert(i,aBand);
  end;
end;

procedure TRLSortedBands.Clear;
var
  i:TRLBandType;
begin
  for i:=Low(TRLBandType) to High(TRLBandType) do
    fTypes[i].List.Clear;
end;

procedure TRLSortedBands.ResetAll;
var
  i:TRLBandType;
begin
  for i:=Low(TRLBandType) to High(TRLBandType) do
    fTypes[i].Printed:=False;
end;

procedure TRLSortedBands.ResetPage;
var
  i:TRLBandType;
begin
  for i:=Low(TRLBandType) to High(TRLBandType) do
    if i in [btTitle] then
    else
      fTypes[i].Printed:=False;
end;

function TRLSortedBands.GetList(aType:TRLBandType):TList;
begin
  Result:=fTypes[aType].List;
end;

function TRLSortedBands.GetPrinted(aType:TRLBandType):boolean;
begin
  Result:=fTypes[aType].Printed;
end;

procedure TRLSortedBands.SetPrinted(aType:TRLBandType; aValue:boolean);
begin
  fTypes[aType].Printed:=aValue;
end;

{ TRLRealBounds }

constructor TRLRealBounds.Create(aOwner:TRLCustomControl);
begin
  fParentControl:=aOwner;
  fUsedUnit     :=buNone;
  //fLeft         :=0;
  //fTop          :=0;
  //fWidth        :=0;
  //fHeight       :=0;
  //
  inherited Create;
end;

procedure TRLRealBounds.AdjustParent;
begin
end;

procedure TRLRealBounds.SetUsedUnit(const aValue:TRLRealBoundsUnit);
begin
  fUsedUnit:=aValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetWidth(const aValue:double);
begin
  fWidth:=aValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetHeight(const aValue:double);
begin
  fHeight:=aValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetLeft(const aValue:double);
begin
  fLeft:=aValue;
  AdjustParent;
end;

procedure TRLRealBounds.SetTop(const aValue:double);
begin
  fTop:=aValue;
  AdjustParent;
end;

{ TRLCustomControl }

constructor TRLCustomControl.Create(aOwner:TComponent);
begin
  // initialization
  //fAfterPrint     :=nil;
  fAlign          :=faNone;
  //fAnchors        :=[];
  //fBeforePrint    :=nil;
  //fBeforeText     :=nil;
  //fHolder         :=nil;
  fHoldStyle      :=hsAsColumn;
  //fSecondHolder   :=nil;
  fSecondHoldStyle:=hsAsColumn;
  //fRealBounds     :=nil;
  fAlignment      :=taLeftJustify;
  //fAutoSize       :=False;
  //fAutoSizeDir    :=[];
  //fAutoExpand     :=False;
  //fAutoTrunc      :=False;
  fLayout         :=tlTop;
  //fControlState   :=[];
  //fBehavior       :=[];
  fDefaultBehavior:=fBehavior;
  fTransparent    :=True;
  //fSizeFixed      :=False;
  fFriendlyName   :=emptystr;
  //fOnMeasureHeight:=nil;
  //fLocker         :=nil;
  // objects
  fBorders        :=TRLBorders.Create(Self);
  fHoldeds        :=TList.Create;
  fRealBounds     :=TRLRealBounds.Create(Self);
  fLocker         :=TCriticalSection.Create;
  //
  inherited Create(aOwner);
  // customization
  ControlStyle    :=ControlStyle+[csOpaque,csReplicatable];
  //
  MakeCaption;
  parentHwnd := (AOwner as TWinControl).Handle;
end;

destructor TRLCustomControl.Destroy;
begin
  FreeObj(fLocker);
  FreeObj(fRealBounds);
  FreeObj(fHoldeds);
  FreeObj(fBorders);
  //
  inherited;
end;

procedure TRLCustomControl.ComputeDetail(aCaller:TObject);
begin
end;

procedure TRLCustomControl.Initialize;
begin
end;

// invoca evento apos a impressao
procedure TRLCustomControl.DoAfterPrint;
begin
  if Assigned(fAfterPrint) then
    fAfterPrint(Self);
end;

procedure TRLCustomControl.DrawBounds;
var
  r:TRect;
begin
  r:=CalcSizeRect;
  with Canvas do
  begin
    Pen.Color:=clBlack;
    Pen.Style:=psSolid;
    Pen.Mode :=pmCopy;
    MoveTo(r.Left   ,r.Top+5   ); LineTo(r.Left   ,r.Top     ); LineTo(r.Left +5,r.Top     );
    MoveTo(r.Right-5,r.Top     ); LineTo(r.Right-1,r.Top     ); LineTo(r.Right-1,r.Top+5   );
    MoveTo(r.Right-1,r.Bottom-5); LineTo(r.Right-1,r.Bottom-1); LineTo(r.Right-5,r.Bottom-1);
    MoveTo(r.Left +5,r.Bottom-1); LineTo(r.Left   ,r.Bottom-1); LineTo(r.Left   ,r.Bottom-5);
  end;
end;

procedure TRLCustomControl.Notification(aComponent:TComponent; Operation:TOperation);
var
  i:integer;
begin
  inherited;
  //
  if Operation=opRemove then
  begin
    if aComponent=fHolder then
      fHolder:=nil;
    if aComponent=fSecondHolder then
      fSecondHolder:=nil;
    if fHoldeds<>nil then
    begin
      i:=fHoldeds.IndexOf(aComponent);
      if i<>-1 then
        fHoldeds.Delete(i);
    end;
  end;  
end;

// anula alinhamento natural do delphi
procedure TRLCustomControl.RequestAlign;
begin
end;

procedure TRLCustomControl.PaintAsCustomControl;
var
  r:TRect;
begin
  r:=CalcSizeRect;
  with Canvas do
  begin
    Brush.Color:=Self.Color;
    Brush.Style:=bsSolid;
    FillRect(r);
  end;
  Borders.PaintTo(Canvas,r);
end;

procedure TRLCustomControl.InternalPaintFinish;
var
  p:TRLCustomReport;
begin
  {$IFDEF TRACECUSTOMSITE}
  DebugLn('TRLCustomControl.InternalPaintFinish;');
  {$ENDIF}

  p:=FindParentReport;
  if not Assigned(p) or p.ShowDesigners then
    DrawBounds;
end;

procedure TRLCustomControl.InternalPaint;
begin
  PaintAsCustomControl;
end;

procedure TRLCustomControl.Paint;
var
  r:TRLCustomReport;
begin
  r:=FindParentReport;
  if Assigned(r) and (r.ReportState in [rsPreparing,rsClosing]) then // 3.20
  else
  begin
    InternalPaint;
    InternalPaintFinish;
  end;  
end;

function TRLCustomControl.IsFriendlyName:boolean;
begin
  Result:=(fFriendlyName<>emptystr) and (fFriendlyName<>Name);
end;

function TRLCustomControl.IsCaption:boolean;
begin
  Result:=(fCaption<>emptystr) and (fCaption<>GetDefaultCaption);
end;

function TRLCustomControl.IsBehavior:boolean;
begin
  Result:=(fBehavior<>fDefaultBehavior);
end;

procedure TRLCustomControl.PrintAsCustomControl;
var
  r:TRect;
  s:TRLGraphicSurface;
begin
  r:=CalcPrintBoundsRect;
  s:=RequestParentSurface;
  s.GeneratorId:=Integer(Self);
  NewGroupId;
  if not IsTransparent(Self) then
  begin
    s.Brush.Color:=Self.Color;
    s.Brush.Style:=bsSolid;
    s.FillRect(r);
  end;
  Borders.PaintTo(s,r);
end;

procedure TRLCustomControl.InternalPrint;
begin
  PrintAsCustomControl;
end;

procedure TRLCustomControl.Print;
begin
  Include(fControlState,stPrinting);
  try
    InternalPrint;
  finally
    Exclude(fControlState,stPrinting);
  end;
end;

procedure TRLCustomControl.CalcSize(var aSize:TPoint);
begin
  aSize:=Point(Width,Height);
end;

procedure TRLCustomControl.AdjustAlignment(var aRect:TRect);
var
  newwidth,newheight:integer;
begin
  newwidth:=RectWidth(aRect);
  case Alignment of
    taLeftJustify : aRect.Right:=Left+newwidth;
    taCenter      : begin
                      if Odd(newwidth) then
                        Inc(newwidth);
                      aRect.Left :=Left+(Width-newwidth) div 2;
                      aRect.Right:=aRect.Left+newwidth;
                    end;
    taRightJustify: begin
                      aRect.Right:=Left+Width;
                      aRect.Left :=aRect.Right-newwidth;
                    end;
    taJustify     : aRect.Right:=Left+newwidth;
  end;
  newheight:=RectHeight(aRect);
  case Layout of
    tlTop   : aRect.Bottom:=Top+newheight;
    tlCenter: begin
                if Odd(newheight) then
                  Inc(newheight);
                aRect.Top   :=Top+(Height-newheight) div 2;
                aRect.Bottom:=aRect.Top+newheight;
              end;
    tlBottom: begin
                aRect.Bottom:=Top+Height;
                aRect.Top   :=aRect.Bottom-newheight;
              end;
  end;
end;

var
  SafeSetBoundsParams:record
    ALeft,ATop,AWidth,AHeight:Integer;
  end;

procedure TRLCustomControl.SafeSetBoundsMethod;
begin
  with SafeSetBoundsParams do
    inherited SetBounds(aLeft,aTop,aWidth,aHeight);
end;

procedure TRLCustomControl.SafeSetBounds(ALeft,ATop,AWidth,AHeight:Integer);
begin
  fLocker.Enter;
  try
    SafeSetBoundsParams.ALeft  :=ALeft;
    SafeSetBoundsParams.ATop   :=ATop;
    SafeSetBoundsParams.AWidth :=AWidth;
    SafeSetBoundsParams.AHeight:=AHeight;
    ThreadSafeCall(SafeSetBoundsMethod);
  finally
    fLocker.Leave;
  end;
end;

procedure TRLCustomControl.AdjustBounds;
var
  p:TPoint;
  r:TRect;
begin
  if stAdjustingBounds in fControlState then
    Exit;
  Include(fControlState,stAdjustingBounds);
  try
    CalcSize(p);
    if csDesigning in ComponentState then
    begin
      if p.X=0 then
        p.X:=1;
      if p.Y=0 then
        p.Y:=1;
    end;
    r.Left  :=Left;
    r.Top   :=Top;
    r.Right :=r.Left+p.X;
    r.Bottom:=r.Top +p.Y;
    AdjustAlignment(r);
    BoundsRect:=r;
  finally
    Exclude(fControlState,stAdjustingBounds);
  end;
end;

procedure TRLCustomControl.CMColorChanged(var Message: TLMessage);
begin
  if not (csLoading in ComponentState) and (Color<>clWhite) then
    fTransparent:=False;
  //  
  inherited;
end;
procedure TRLCustomControl.CMFontChanged(var Message:TLMessage);
begin
  AdjustBounds;
  Invalidate;
  //
  inherited;
end;

type TFriendControl=class(TControl) end;

function GetControlImage(aControl:TControl; var aBitmap:TBitmap):boolean;
{$ifdef CLX}
var c:TFriendControl;
{$ifndef CPP}
var h:QPaintDeviceH;
{$endif}
{$endif}
begin
  Result:=False;
  aBitmap:=NewBitmap(aControl.Width,aControl.Height);
  try
{$ifdef CLX}
    c:=TFriendControl(aControl);
{$ifndef CPP}
    h:=c.GetPaintDevice;
    QPainter_redirect(h,aBitmap.Handle);
{$endif}
    c.Repaint;
{$ifndef CPP}
    QPainter_redirect(h,nil);
{$endif}
{$else}
    if aControl is TWinControl then
      TWinControl(aControl).PaintTo(aBitmap.Canvas.Handle,0,0)
    else if aControl is TControl then
      aControl.Perform(LM_PAINT,aBitmap.Canvas.Handle,0)
    else
      SysUtils.Abort;  
{$endif}
    Result:=True;
  except
    aBitmap.Free;
    aBitmap:=nil;
  end;
end;

procedure GetControlsByPrintOrder(aParent:TWinControl; aList:TList);
var
  i,j:integer;
  c:TControl;
  function IsPriorPrintOrder(aCtrl,aRef:TControl):boolean;
  begin
    Result:=(aCtrl.ComponentIndex<aRef.ComponentIndex);
  end;
begin
  for i:=0 to aParent.ControlCount-1 do
  begin
    c:=aParent.Controls[i];
    j:=0;
    while (j<aList.Count) and IsPriorPrintOrder(TControl(aList[j]),c) do
      Inc(j);
    aList.Insert(j,c);
  end;
end;

procedure PrepareStaticsAllFrom(aParent:TWinControl);
var
  i:integer;
  c:TControl;
  l:TList;
begin
  l:=TList.Create;
  try
    // monta lista sorteada por creation order
    GetControlsByPrintOrder(aParent,l);
    for i:=0 to l.Count-1 do
    begin
      c:=TControl(l[i]);
      // control ou panel não site
      if IsStaticCustomControl(c) then
        with TRLCustomControl(c) do
          if CanPrint then
          begin
            AdjustBounds;
            PrepareStatics;
          end
          else
      else if c is TCustomFrame then
        if TCustomFrame(c).Visible then
          PrepareStaticsAllFrom(TCustomFrame(c));
    end;
  finally
    l.free;
  end;
end;

procedure PrintNonNative(aParent:TWinControl; aControl:TControl);
var
  site:TWinControl;
  offs:TPoint;
  bmp :TBitmap;
begin
  // procura o parentsite para pegar o surface de desenho e as coordenadas relativas do controle aControl
  site:=aParent;
  offs:=Point(aControl.Left,aControl.Top);
  while (site<>nil) and not (site is TRLCustomSite) do
  begin
    Inc(offs.X,site.Left);
    Inc(offs.Y,site.Top);
    site:=site.Parent;
  end;
  //
  if site<>nil then
    if GetControlImage(aControl,bmp) then
      try
        TRLCustomSite(site).Surface.Draw(offs.X,offs.Y,bmp);
      finally
        bmp.Free;
      end;
end;

procedure PrintStaticsAllFrom(aParent:TWinControl);
var
  i:integer;
  c:TControl;
  l:TList;
begin
  l:=TList.Create;
  try
    // monta lista sorteada por creation order
    GetControlsByPrintOrder(aParent,l);
    for i:=0 to l.Count-1 do
    begin
      c:=TControl(l[i]);
      // control ou panel não site
      if IsStaticCustomControl(c) then
        with TRLCustomControl(c) do
          if CouldPrint then
          begin
            Print;
            DoAfterPrint;
          end
          else
      else if c is TCustomFrame then
        if TCustomFrame(c).Visible then
          PrintStaticsAllFrom(TCustomFrame(c))
        else
      else if not (c is TRLCustomControl) then
        if c.Visible then
          PrintNonNative(aParent,c);
    end;
  finally
    l.free;
  end;
end;

procedure PrintNonStaticsAllFrom(aParent:TWinControl);
var
  i:integer;
  c:TControl;
  l:TList;
begin
  l:=TList.Create;
  try
    // monta lista sorteada por creation order
    GetControlsByPrintOrder(aParent,l);
    for i:=0 to l.Count-1 do
    begin
      c:=TControl(l[i]);
      // control ou panel não site
      if c is TCustomFrame then
        if TCustomFrame(c).Visible then
          PrintNonStaticsAllFrom(TCustomFrame(c))
        else
      else if not IsStaticCustomControl(c) then
        with TRLCustomControl(c) do
          if CanPrint then
          begin
            Print;
            DoAfterPrint;
          end;
    end;
  finally
    l.free;
  end;
end;

procedure TRLCustomControl.PrepareStatics;
begin
  if Enabled then
    PrepareStaticsAllFrom(Self);
end;

procedure TRLCustomControl.PrintStatics;
begin
  if Enabled then
    PrintStaticsAllFrom(Self);
end;

procedure TRLCustomControl.PrintNonStatics;
begin
  if Enabled then
    PrintNonStaticsAllFrom(Self);
end;

function TRLCustomControl.IsBallast:boolean;
var
  p:TRLCustomPager;
  s:TRLCustomSkipper;
begin
  s:=FindParentSkipper;
  p:=FindParentPager;
  if (p<>nil) and (psCompleting in p.PagerStatus) then
    Result:=True
  else if (s<>nil) and (s.RecordAction=raBlankAndRetain) then
    Result:=True
  else
    Result:=False;
end;

procedure TRLCustomControl.MakeCaption;
begin
  Caption:=GetMadeCaption;
end;

function TRLCustomControl.GetMadeCaption:string;
begin
  if IsBallast then
    Result:=emptystr
  else
    Result:=InternalMakeCaption;
end;

function TRLCustomControl.InternalMakeCaption:string;
begin
  Result:=GetCaption;
end;

function TRLCustomControl.GetDefaultCaption:TCaption;
begin
  if fFriendlyName<>emptystr then
    Result:=fFriendlyName
  else
    Result:=Name;
end;

// relatorio anterior
function TRLCustomControl.FindParentReport:TRLCustomReport;
var
  w:TControl;
begin
  w:=Self;
  while (w<>nil) and not (w is TRLCustomReport) do
    w:=w.Parent;
  Result:=TRLCustomReport(w);
end;

// relatorio mestre, o primeiro da lista
function TRLCustomControl.GetMasterReport:TRLCustomReport;
begin
  Result:=FindParentReport;
  if Result<>nil then
    if Result.PriorReport<>nil then
      Result:=Result.PriorReport.MasterReport;
end;

// site anterior
function TRLCustomControl.FindParentSite:TRLCustomSite;
var
  w:TControl;
begin
  w:=Parent;
  while (w<>nil) and not (w is TRLCustomSite) do
    w:=w.Parent;
  Result:=TRLCustomSite(w);
end;

// band anterior 
function TRLCustomControl.FindParentBand:TRLCustomBand;
var
  w:TControl;
begin
  w:=Parent;
  while (w<>nil) and not (w is TRLCustomBand) do
    w:=w.Parent;
  Result:=TRLCustomBand(w);
end;

function TRLCustomControl.FindParentGroup:TRLCustomGroup;
var
  w:TControl;
begin
  Result:=nil;
  w:=Parent;
  while (w<>nil) and not (w is TRLCustomGroup) do
    if w is TRLCustomPager then
      Exit
    else
      w:=w.Parent;
  Result:=TRLCustomGroup(w);
end;

// controlador de registros atual ou anterior (DlReport ou subdetail)
function TRLCustomControl.FindParentSkipper:TRLCustomSkipper;
var
  w:TControl;
begin
  w:=Self;
  while (w<>nil) and not (w is TRLCustomSkipper) do
    w:=w.Parent;
  Result:=TRLCustomSkipper(w);
end;

function TRLCustomControl.FindParentPager:TRLCustomPager;
var
  w:TControl;
begin
  w:=Parent;
  while (w<>nil) and not (w is TRLCustomPager) do
    w:=w.Parent;
  Result:=TRLCustomPager(w);
end;

function TRLCustomControl.FindParentSurface:TRLGraphicSurface;
var
  p:TRLCustomSite;
begin
  p:=FindParentSite;
  if Assigned(p) then
    Result:=p.Surface
  else
    Result:=nil;
end;

// pager atual ou anterior (DlReport, subdetail ou group)
function TRLCustomControl.RequestParentPager:TRLCustomPager;
begin
  Result:=FindParentPager;
  if Result=nil then
    raise Exception.Create(LS_NotFoundStr+': '+Name+'.ParentPager');
end;

function TRLCustomControl.RequestParentSkipper:TRLCustomSkipper;
begin
  Result:=FindParentSkipper;
  if Result=nil then
    raise Exception.Create(LS_NotFoundStr+': '+Name+'.ParentSkipper');
end;

function TRLCustomControl.RequestParentReport:TRLCustomReport;
begin
  Result:=FindParentReport;
  if Result=nil then
    raise Exception.Create(LS_NotFoundStr+': '+Name+'.ParentReport');
end;

function TRLCustomControl.RequestParentSurface:TRLGraphicSurface;
begin
  Result:=FindParentSurface;
  if Result=nil then
    raise Exception.Create(LS_NotFoundStr+': '+Name+'.ParentSurface');
end;

procedure TRLCustomControl.DoBeforePrint;
begin
  if Assigned(fBeforePrint) then
    fBeforePrint(Self,fCouldPrint);
end;

procedure TRLCustomControl.DoBeforeText(var aText:string; var aPrintIt:boolean);
begin
  if Assigned(fBeforeText) then
    fBeforeText(Self,aText,aPrintIt);
end;

procedure TRLCustomControl.DoOnMeasureHeight;
begin
  if Assigned(fOnMeasureHeight) then
    fOnMeasureHeight(Self);
end;

function TRLCustomControl.CanPrint:boolean;
var
  s:string;
begin
  fCouldPrint:=Visible;
  if fCouldPrint then
  begin
    DoBeforePrint;
    if fCouldPrint then
    begin
      s:=GetMadeCaption;
      DoBeforeText(s,fCouldPrint);
      if fCouldPrint then
      begin
        FindParentReport.DoFilterText(s,fCouldPrint);
        if fCouldPrint then
          Caption:=s;
      end;
    end;
  end;
  Result:=fCouldPrint;
end;

function TRLCustomControl.CalcSizeRect:TRect;
begin
  Result:=BoundsRect;
  MoveRect(Result,0,0);
end;

function TRLCustomControl.GetClientRect:TRect;
var
  w:integer;
begin
  Result:=CalcSizeRect;
  w:=fBorders.Width;
  if w>0 then
  begin
    Inc(w);
    if fBorders.CanDrawLeft then
      Inc(Result.Left,w);
    if fBorders.CanDrawTop then
      Inc(Result.Top,w);
    if fBorders.CanDrawRight then
      Dec(Result.Right,w);
    if fBorders.CanDrawBottom then
      Dec(Result.Bottom,w);
  end;
end;

function TRLCustomControl.CalcPrintBoundsRect:TRect;
var
  p:TControl;
begin
  Result:=BoundsRect;
  //
  p:=Parent;
  while (p<>nil) and not (p is TRLCustomSite) do
  begin
    OffsetRect(Result,p.Left,p.Top);
    p:=p.Parent;
  end;
end;

function TRLCustomControl.CalcPrintSizeRect:TRect;
begin
  Result:=BoundsRect;
  MoveRect(Result,0,0);
end;

function TRLCustomControl.CalcPrintClientRect:TRect;
var
  w,h:integer;
begin
  Result:=CalcPrintBoundsRect;
  if fBorders.Width>0 then
  begin
    w:=fBorders.Width;
    h:=fBorders.Width;
    if fBorders.CanDrawLeft then
      Inc(Result.Left,w);
    if fBorders.CanDrawTop then
      Inc(Result.Top,h);
    if fBorders.CanDrawRight then
      Dec(Result.Right,w);
    if fBorders.CanDrawBottom then
      Dec(Result.Bottom,h);
  end;
end;

function TRLCustomControl.CalcWastedPixels:TRect;
begin
  Result:=Rect(0,0,0,0);
  if fBorders.Width>0 then
  begin
    if fBorders.CanDrawLeft then
      Inc(Result.Left  ,fBorders.Width);
    if fBorders.CanDrawTop then
      Inc(Result.Top   ,fBorders.Width);
    if fBorders.CanDrawRight then
      Inc(Result.Right ,fBorders.Width);
    if fBorders.CanDrawBottom then
      Inc(Result.Bottom,fBorders.Width);
  end;
end;

procedure TRLCustomControl.SetClientRect(const aValue:TRect);
var
  rWasted:TRect;
  newRect:TRect;
begin
  rWasted:=CalcWastedPixels;
  newRect:=aValue;
  Dec(newRect.Left  ,rWasted.Left);
  Dec(newRect.Top   ,rWasted.Top);
  Inc(newRect.Right ,rWasted.Right);
  Inc(newRect.Bottom,rWasted.Bottom);
  BoundsRect:=newRect;
end;

procedure TRLCustomControl.SetAlign(const aValue:TRLControlAlign);
var
  old:TRLControlAlign;
begin
  old:=fAlign;
  if aValue=old then
    Exit;
  if aValue<>faNone then
    fAnchors:=[];
  fAlign:=aValue;
  //
  if ((old in faFreeHeightSet) and (aValue in faFreeWidthSet)) or
     ((old in faFreeWidthSet) and (aValue in faFreeHeightSet)) then
  begin
    fAlign:=faNone;
    AdjustBounds;
    fAlign:=aValue;
  end;
  Realign;
end;

procedure TRLCustomControl.SetAnchors(const aValue:TRLControlAnchors);
begin
  if aValue<>[] then
    fAlign:=faNone;
  fAnchors:=aValue;
end;

procedure TRLCustomControl.Hold(aControl:TRLCustomControl; aPlace:integer);
var
  m,n,p:TPoint;
begin
  if aControl=Self then
    Exit;
  if fHoldeds.IndexOf(aControl)=-1 then
    fHoldeds.Add(aControl);
  // guarda posição relativa
  m  :=GetScreenPos(Self);
  n  :=GetScreenPos(aControl);
  p.x:=n.x-m.x;
  p.y:=n.y-m.y;
  case aPlace of
    0: aControl.HolderOffset:=p;
    1: aControl.SecondHolderOffset:=p;
  end;  
  // ajusta posição do controle amarrado
  aControl.AdjustBounds;
end;

procedure TRLCustomControl.Unhold(aControl:TRLCustomControl);
var
  i:integer;
begin
  i:=fHoldeds.IndexOf(aControl);
  if i<>-1 then
    fHoldeds.Delete(i);
end;

procedure TRLCustomControl.SetHolder(const aValue:TRLCustomControl);
var
  old:TRLCustomControl;
begin
  old:=fHolder;
  if aValue=old then
    Exit;
  fHolder:=aValue;
  if old<>nil then
    old.Unhold(Self);
  if aValue<>nil then
    aValue.Hold(Self,0);
end;

procedure TRLCustomControl.SetHoldStyle(const aValue:TRLHoldStyle);
begin
  if aValue=fHoldStyle then
    Exit;
  fHoldStyle:=aValue;
  if fHolder<>nil then
  begin
    fHolder.Unhold(Self);
    fHolder.Hold(Self,0);
  end;
end;

procedure TRLCustomControl.SetSecondHolder(const aValue:TRLCustomControl);
var
  old:TRLCustomControl;
begin
  old:=fSecondHolder;
  if aValue=old then
    Exit;
  fSecondHolder:=aValue;
  if old<>nil then
    old.Unhold(Self);
  if aValue<>nil then
    aValue.Hold(Self,1);
end;

procedure TRLCustomControl.SetSecondHoldStyle(const aValue:TRLHoldStyle);
begin
  if aValue=fSecondHoldStyle then
    Exit;
  fSecondHoldStyle:=aValue;
  if fSecondHolder<>nil then
  begin
    fSecondHolder.Unhold(Self);
    fSecondHolder.Hold(Self,1);
  end;
end;

// ajusta a altura do controle pai para comportar este controle
procedure TRLCustomControl.ExpandParentSite;
var
  w,h:integer;
  s:TRLCustomSite;
begin
  if csLoading in ComponentState then
    Exit;
  if stRestoringBounds in fControlState then
    Exit;
  if stExpandingParent in fControlState then
    Exit;
  Include(fControlState,stExpandingParent);
  try
    w:=(BoundsRect.Right -BoundsRect.Left)-(OldBoundsRect.Right -OldBoundsRect.Left);
    h:=(BoundsRect.Bottom-BoundsRect.Top )-(OldBoundsRect.Bottom-OldBoundsRect.Top);
    // detecta controle escravo de largura
    if Align in faFreeWidthSet then
    begin
      // procura o site pai não escravo de largura
      s:=FindParentSite;
      while (s<>nil) and not (s.Align in faFreeWidthSet) do
        s:=s.FindParentSite;
      if (s<>nil) and (asWidthDir in s.AutoSizeDir) then
        if s.AutoSize then
          s.AdjustBounds
        else if s.AutoExpand and (beSiteExpander in Behavior) then
          s.Width:=s.Width+w;
    end;
    // detecta controle escravo de altura
    if Align in faFreeHeightSet then
    begin
      // procura o site pai não escravo de altura
      s:=FindParentSite;
      while (s<>nil) and not (s.Align in faFreeHeightSet) do
        s:=s.FindParentSite;
      if (s<>nil) and (asHeightDir in s.AutoSizeDir) then
        if s.AutoSize then
          s.AdjustBounds
        else if s.AutoExpand and (beSiteExpander in Behavior) then
          s.Height:=s.Height+h;
    end;
  finally
    Exclude(fControlState,stExpandingParent);
  end;
end;

procedure TRLCustomControl.Realign;
var
  p:TRLCustomSite;
begin
  if csLoading in ComponentState then
    Exit;
  p:=FindParentSite;
  if p<>nil then
    p.RealignControls;
end;

procedure TRLCustomControl.RealignControls;
begin
end;

function TRLCustomControl.CanSetWidth:boolean;
begin
  Result:=(Align in faFreeWidthSet) and not ((asWidthDir in AutoSizeDir) and AutoSize);
end;

function TRLCustomControl.CanSetHeight:boolean;
begin
  Result:=(Align in faFreeHeightSet) and not ((asHeightDir in AutoSizeDir) and AutoSize);
end;

procedure TRLCustomControl.RealignHoldeds;
var
  i:integer;
begin
  if stAdjustingHoldeds in fControlState then
    Exit;
  Include(fControlState, stAdjustingHoldeds);
  try
    for i:=0 to fHoldeds.Count-1 do
      TRLCustomControl(fHoldeds[i]).AdjustBounds;
  finally
    Exclude(fControlState, stAdjustingHoldeds);
  end;
end;

//
procedure TRLCustomControl.AdjustToFixedSize(var aLeft,aTop,aWidth,aHeight:integer);
begin
  if fSizeFixed then
  begin
    aWidth :=fFixedSize.X;
    aHeight:=fFixedSize.Y;
  end;
end;

// adequa o frame que contém o control
procedure TRLCustomControl.AdjustToParentFrame(var aLeft,aTop,aWidth,aHeight:integer);
var
  r:TRect;
begin
  if (Parent<>nil) and (Parent is TCustomFrame) then
  begin
    r.Left  :=Parent.Left+aLeft;
    r.Top   :=Parent.Top +aTop;
    r.Right :=r.Left+aWidth;
    r.Bottom:=r.Top +aHeight;
    aLeft   :=0;
    aTop    :=0;
    Parent.BoundsRect:=r;
  end;
end;

//
procedure TRLCustomControl.AdjustToHolder(aHolder:TRLCustomControl; var aLeft,aTop,aWidth,aHeight:integer);
var
  m,p,r,holdofs:TPoint;
  ox,oy:integer;
  holdst:TRLHoldStyle;
begin
{$ifdef KYLIX1}
  holdofs:=Point(0,0);
  holdst :=hsAsColumn;
{$endif}
  if aHolder=nil then
    Exit
  else if aHolder=fHolder then
  begin
    holdofs:=HolderOffset;
    holdst :=HoldStyle;
  end
  else if aHolder=fSecondHolder then
  begin
    holdofs:=SecondHolderOffset;
    holdst :=SecondHoldStyle;
  end
  else
    Exit;
  //
  case Alignment of
    taLeftJustify : ox:=0;
    taCenter      : ox:=(aHolder.Width-aWidth) div 2;
    taRightJustify: ox:=aHolder.Width-aWidth;
    taJustify     : ox:=0;
  else
    ox:=0;
  end;
  //
  case Layout of
    tlTop    : oy:=0;
    tlCenter : oy:=(aHolder.Height-aHeight) div 2;
    tlBottom : oy:=aHolder.Height-aHeight;
    tlJustify: oy:=0;
  else
    oy:=0;
  end;
  //
  case holdst of
    hsAsColumn    : begin
                      SetScreenLeft(Self,GetScreenLeft(aHolder)+ox,aLeft);
                      if CanSetWidth then
                        aWidth:=aHolder.Width;
                    end;
    hsHorizontally: SetScreenLeft(Self,GetScreenLeft(aHolder)+holdofs.X+ox,aLeft);
    hsVertically  : SetScreenTop(Self,GetScreenTop(aHolder)+holdofs.Y+oy,aTop);
    hsRelatively  : begin
                      m:=GetScreenPos(aHolder);
                      p:=Point(m.X+holdofs.X+ox,m.Y+holdofs.Y+oy);
                      SetScreenPos(Self,p,r);
                      aLeft:=r.X;
                      aTop :=r.Y;
                    end;
    hsCopyWidth   : if CanSetWidth then
                      aWidth:=aHolder.Width;
    hsCopyHeight  : if CanSetHeight then
                      aHeight:=aHolder.Height;
    hsCopySize    : begin
                      if CanSetWidth then
                        aWidth:=aHolder.Width;
                      if CanSetHeight then
                        aHeight:=aHolder.Height;
                    end;
  end;
end;

procedure TRLCustomControl.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
var
  SavedBoundsRect:TRect;
begin
  SavedBoundsRect:=BoundsRect;
  OldBoundsRect  :=SavedBoundsRect;
  //
  AdjustToFixedSize(aLeft,aTop,aWidth,aHeight);
  AdjustToParentFrame(aLeft,aTop,aWidth,aHeight);
  AdjustToHolder(fHolder,aLeft,aTop,aWidth,aHeight);
  AdjustToHolder(fSecondHolder,aLeft,aTop,aWidth,aHeight);
  // se as coordenadas mudaram em relação ao controle pai...
  if (OldBoundsRect.Left<>aLeft) or
     (OldBoundsRect.Top<>aTop) or
     (RectWidth(OldBoundsRect)<>aWidth) or
     (RectHeight(OldBoundsRect)<>aHeight) then
  begin
    SafeSetBounds(aLeft,aTop,aWidth,aHeight);
    OldBoundsRect:=SavedBoundsRect;
    ExpandParentSite;
    Realign;
  end;
  // incondicionalmente realinha os controles internos e "agarrados"
  RealignControls;
  RealignHoldeds;
end;

procedure TRLCustomControl.SetTransparent(const aValue:boolean);
begin
  if aValue=fTransparent then
    Exit;
  if aValue then
    if Self is TRLCustomReport then
      Color:=clWhite
    else
      ParentColor:=True;
  fTransparent:=aValue;
end;

procedure TRLCustomControl.SetAlignment(const aValue:TRLTextAlignment);
begin
  if aValue=fAlignment then
    Exit;
  fAlignment:=aValue;
  Invalidate;
end;

procedure TRLCustomControl.SetAutoSize(const aValue:boolean);
begin
  if aValue=fAutoSize then
    Exit;
  fAutoSize:=aValue;
  if aValue then
    AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetAutoExpand(const aValue:boolean);
begin
  if aValue=fAutoExpand then
    Exit;
  fAutoExpand:=aValue;
  if aValue then
    AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetAutoTrunc(const aValue:boolean);
begin
  if aValue=fAutoTrunc then
    Exit;
  fAutoTrunc:=aValue;
  if aValue then
    AdjustBounds;
end;

function TRLCustomControl.GetCaption:TCaption;
begin
  if IsPreparing then
    Result:=fPreparingCaption
  else if (fCaption=emptystr) and (csDesigning in ComponentState) then
    Result:=GetDefaultCaption
  else
    Result:=fCaption;
end;

procedure TRLCustomControl.SetCaption(const aValue:TCaption);
begin
  // fPreparingCaption é o caption para efeitos de impressão, e é descartado quando o relatório termina
  // fCaption contém o texto oficial do caption, que deve ser gravado do dfm
  fPreparingCaption:=aValue;
  if IsPreparing then
  else if (aValue=GetDefaultCaption) and (csDesigning in ComponentState) then
    fCaption:=emptystr
  else
    fCaption:=aValue;
  //
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetLayout(const aValue:TRLTextLayout);
begin
  if aValue=fLayout then
    Exit;
  fLayout:=aValue;
  Invalidate;
end;

procedure PushBoundsAllFrom(aParent:TWinControl);
var
  i:integer;
  c:TControl;
begin
  for i:=0 to aParent.ControlCount-1 do
  begin
    c:=aParent.Controls[i];
    if c is TRLCustomControl then
    begin
      TRLCustomControl(c).PushBoundsRect;
      if c is TRLCustomSite then
        PushBoundsAllFrom(TRLCustomSite(c));
    end
    else if c is TCustomFrame then
      PushBoundsAllFrom(TCustomFrame(c));
  end;
end;

procedure PopBoundsAllFrom(aParent:TWinControl);
var
  i:integer;
  c:TControl;
begin
  for i:=0 to aParent.ControlCount-1 do
  begin
    c:=aParent.Controls[i];
    if c is TRLCustomControl then
    begin
      TRLCustomControl(c).PopBoundsRect;
      if c is TRLCustomSite then
        PopBoundsAllFrom(TRLCustomSite(c));
    end
    else if c is TCustomFrame then
      PopBoundsAllFrom(TCustomFrame(c));
  end;
end;

procedure TRLCustomControl.PushBoundsRect;
begin
  fPeekBoundsRect:=BoundsRect;
end;

procedure TRLCustomControl.PopBoundsRect;
begin
  Include(fControlState,stRestoringBounds);
  try
    BoundsRect:=fPeekBoundsRect;
  finally
    Exclude(fControlState,stRestoringBounds);
  end;
end;

procedure TRLCustomControl.SetBorders(const aValue:TRLBorders);
begin
  fBorders:=aValue;
  fBorders.ParentControl:=Self;
  Invalidate;
end;

procedure TRLCustomControl.SetRealBounds(const aValue:TRLRealBounds);
begin
  fRealBounds:=aValue;
end;

function TRLCustomControl.IsPreparing:boolean;
var
  r:TRLCustomReport;
begin
  r     :=FindParentReport;
  Result:=Assigned(r) and (r.ReportState in [rsPreparing,rsClosing]);
end;

procedure TRLCustomControl.CheckParent(var aControl:TWinControl);
begin
  // uma band não pode conter outras
  if (Self is TRLCustomBand) and (aControl is TRLCustomBand) then
    aControl:=aControl.Parent;
  // um panel não pode conter bands ou paginadores
  if (Self is TRLCustomBand) or (Self is TRLCustomPager) then
    while (aControl<>nil) and (aControl is TRLCustomPanel) do
      aControl:=aControl.Parent;
end;

procedure TRLCustomControl.SetName(const Value:TComponentName);
begin
  inherited SetName(Value);
  //
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomControl.SetFriendlyName(const Value:string);
var
  i:integer;
begin
  if Value=fFriendlyName then
    Exit;
  //
  if (Value=emptystr) or (Value=Name) then
    fFriendlyName:=emptystr
  else if not IsValidIdent(Value) then
    if csLoading in ComponentState then
      fFriendlyName:=emptystr
    else
      raise Exception.Create(LS_InvalidNameStr+' "'+Value+'"')
  else
  begin
    for i:=0 to Owner.ComponentCount-1 do
      if Owner.Components[i] is TRLCustomControl then
        if SameText(Value,TRLCustomControl(Owner.Components[i]).FriendlyName) then
          raise Exception.Create(LS_DuplicateNameStr+' "'+Value+'"');
    fFriendlyName:=Value;
  end;
  //
  AdjustBounds;
  Invalidate;
end;

function TRLCustomControl.IsMeasurable:boolean;
begin
  Result:=AutoSize or AutoExpand or Assigned(fOnMeasureHeight);
end;

// try to Measure controls height before it be printed
procedure TRLCustomControl.MeasureHeight;
begin
  if not Visible then
    Exit;
  if not IsMeasurable then
    Exit;
  if csLoading in ComponentState then
    Exit;
  if stMeasuringHeights in fControlState then
    Exit;
  Include(fControlState,stMeasuringHeights);
  try
    InternalMeasureHeight;
    DoOnMeasureHeight;
  finally
    Exclude(fControlState,stMeasuringHeights);
  end;
  AdjustBounds;
end;

procedure TRLCustomControl.InternalMeasureHeight;
begin
  Caption:=GetMadeCaption;
end;

function TRLCustomControl.GetAttribute(const aName:string):variant;
begin
  Result:=Caption;
end;

Procedure TRLCustomControl.WMMOUSEMOVE(var Msg: TLMessage);
begin
  inherited;
  ReleaseCapture;
  TWinControl(self).perform(LM_SYSCOMMAND, $f012, 0);
end;

function TRLCustomControl.SetAttribute(const aName:string; aValue:variant):boolean;
begin
  Result:=False;
end;


procedure TRLCustomControl.SetParent(aParent:TWinControl);
var
  p:TWinControl;
begin
  p:=aParent;
  if p<>nil then
    CheckParent(p);
  //  
  inherited SetParent(p);
  //
  if p<>nil then
  begin
    AdjustBounds;
    Realign;
  end;  
end;

function TRLCustomControl.GetClientHeight:integer;
begin
  Result:=RectHeight(ClientRect);
end;

function TRLCustomControl.GetClientWidth:integer;
begin
  Result:=RectWidth(ClientRect);
end;

procedure TRLCustomControl.SetClientSize(const Value:TPoint);
var
  r:TRect;
begin
  r:=GetClientRect;
  SetBounds(Left,Top,Width-RectWidth(r)+Value.X,Height-RectHeight(r)+Value.Y);
end;

procedure TRLCustomControl.SetClientHeight(const Value:integer);
begin
  SetClientSize(Point(Width,Value));
end;

procedure TRLCustomControl.SetClientWidth(const Value:integer);
begin
  SetClientSize(Point(Value,Height));
end;

{ TRLCustomDBControl }

constructor TRLCustomDBControl.Create(aOwner:TComponent);
begin
  fDataField :=emptystr;
  fDataSource:=nil;
  //
  inherited Create(aOwner);
  // customization
  Width :=65;
  Height:=17;
end;

procedure TRLCustomDBControl.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
      fDataSource:=nil;
end;

procedure TRLCustomDBControl.SetDataSource(const aValue:TDataSource);
begin
  if aValue=fDataSource then
    Exit;
  fDataSource:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
  Invalidate;
end;

procedure TRLCustomDBControl.SetDataField(const aValue:TRLDataFieldProperty);
begin
  if aValue=fDataField then
    Exit;
  fDataField:=aValue;
  MakeCaption;
end;

function TRLCustomDBControl.GetDataSet:TDataSet;
begin
  if Assigned(fDataSource) then
    Result:=fDataSource.DataSet
  else
    Result:=nil;
end;

function TRLCustomDBControl.GetField:TField;
begin
  if (DataSet<>nil) and (fDataField<>emptystr) then
  begin
    Result:=DataSet.FindField(fDataField);
    if Result=nil then
      raise Exception.Create(LS_NotFoundStr+': '+Name+'.DataField "'+fDataField+'"');
  end
  else
    Result:=nil;
end;

function TRLCustomDBControl.InternalMakeCaption:string;
var
  f:TField;
begin
  if not IsPreparing then
    if fFriendlyName<>emptystr then
      Result:=fFriendlyName
    else if fDataField<>emptystr then
      Result:=fDataField
    else
      Result:=Name
  else
  begin
    f:=GetField;
    if (f<>nil) and f.DataSet.Active and not f.DataSet.Eof then
      Result:=SmartGetFieldDisplayText(f)
    else
      Result:=emptystr;
  end;    
end;

{ TRLCustomLabel }

constructor TRLCustomLabel.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  {$IFDEF TRACECUSTOMLABEL}
  Debugln('TRLCustomLabel.Create');
  {$ENDIF}

  // customization
  Width      :=65;
  Height     :=17;
  AutoSizeDir:=[asWidthDir];
  AutoSize   :=True;

end;

procedure TRLCustomLabel.InternalPaint;
var
  s:string;
  r:TRect;
begin
  PaintAsCustomControl;
  s:=Caption;
  if (s=emptystr) and not IsPreparing then
    s:=Name;
  r:=GetClientRect;
  with Canvas do
  begin
    Font       :=Self.Font;
    Brush.Color:=Self.Color;
    Brush.Style:=bsSolid;
  end;
  CanvasTextRectEx(Canvas,r,r.Left,r.Top,s,ToMetaTextAlignment(TAlignment(Alignment)),ToMetaTextLayout(TTextLayout(Layout)),MetaTextFlagAutoSize or MetaTextFlagIntegralHeight);
end;

procedure TRLCustomLabel.InternalPrint;
var
  w,o,h,t,l:integer;
  r:TRect;
  s:string;
  f:TRLMetaTextFlags;
begin
  inherited;
  {$IFDEF TRACECUSTOMLABEL}
  DebugLn('TRLCustomLabel.InternalPrint;');
  {$ENDIF}
  //
  r:=CalcPrintClientRect;
  with RequestParentSurface do
  begin
    GeneratorId:=Integer(Self);
    NewGroupId;
    Font:=Self.Font;
    s   :=Caption; 
    o   :=TextWidth(' ') div 2;
    w   :=TextWidth(s+' ');
    h   :=TextHeight(s+' ');
    case Alignment of
      taCenter      : l:=(r.Left+r.Right-w) div 2+o;
      taRightJustify: l:=r.Right-w+o;
    else
      l:=r.Left+o;
    end;
    case Layout of
      tlCenter: t:=(r.Top+r.Bottom-h) div 2;
      tlBottom: t:=r.Bottom-h;
    else
      t:=r.Top;
    end;
    Brush.Style:=bsClear;
    f:=MetaTextFlagIntegralHeight;
    if AutoSize then
      f:=f or MetaTextFlagAutoSize;
    TextRectEx(r,l,t,s,ToMetaTextAlignment(TAlignment(Alignment)),ToMetaTextLayout(TTextLayout(Layout)),f);
  end;
end;

procedure TRLCustomLabel.CalcSize(var aSize:TPoint);
var
  w:integer;
  c:string;
begin
  {$IFDEF TRACECUSTOMLABEL}
  Debugln('TRLCustomLabel.CalcSize');
  {$ENDIF}

  aSize:=Point(Width,Height);
  if not AutoSize then
    Exit;
  // texto a utilizar para o cálculo
  c:=Caption;
  if (c=emptystr) and not IsPreparing then
    c:=Name;
  // dimensões do texto
  aSize.X:=0;
  aSize.Y:=0;
  with TextBounds(c+' ',Self.Font,0) do
  begin
    Inc(aSize.X,X);
    Inc(aSize.Y,Y);
  end;
  // adicional das bordas
  w:=fBorders.Width;
  if w>0 then
  begin
    Inc(w);
    if fBorders.CanDrawLeft then
      Inc(aSize.X,w);
    if fBorders.CanDrawTop then
      Inc(aSize.Y,w);
    if fBorders.CanDrawRight then
      Inc(aSize.X,w);
    if fBorders.CanDrawBottom then
      Inc(aSize.Y,w);
  end;
end;

{ TRLCustomAngleLabel }

constructor TRLCustomAngleLabel.Create(aOwner:TComponent);
begin
  //fAngle       :=0;
  fAngleBorders:=False;
  //
  inherited Create(aOwner);
  // customization
  Width      :=65;
  Height     :=17;
  AutoSizeDir:=[asWidthDir];
  AutoSize   :=True;
end;

procedure TRLCustomAngleLabel.InternalPaint;
var
  w,o,h,t,l:integer;
  s:string;
  r:TRect;
  m,a:TBitmap;
begin
  PaintAsCustomControl;
  s:=Caption;
  if (s=emptystr) and not IsPreparing then
    s:=Name;
  r:=GetClientRect;
  with Canvas do
  begin
    Font:=Self.Font;
    o:=0;
    w:=TextWidth(s+' ');
    h:=TextHeight(s+' ');
    //
    m:=NewBitmap(w,h);
    try
      m.Transparent       :=Self.Transparent;
      m.TransparentColor  :=Self.Color;
      m.TransparentMode   :=tmFixed;
      m.Canvas.Font       :=Self.Font;
      m.Canvas.Brush.Color:=Self.Color;
      m.Canvas.Brush.Style:=bsSolid;
      m.Canvas.Pen.Style  :=psClear;
      m.Canvas.Rectangle(0,0,m.Width+1,m.Height+1);
      m.Canvas.TextOut(1,-1,s);
      //
      a:=RotatedBitmap(m,fAngle);
      try
        case Alignment of
          taCenter      : l:=(r.Left+r.Right-a.Width) div 2+o;
          taRightJustify: l:=r.Right-a.Width+o;
        else
          l:=r.Left+o;
        end;
        case Layout of
          tlCenter      : t:=(r.Top+r.Bottom-a.Height) div 2;
          tlBottom      : t:=r.Bottom-a.Height;
        else
          t:=r.Top;
        end;
        Draw(l,t,a);
      finally
        a.free;
      end;
    finally
      m.free;
    end;
  end;  
end;

procedure TRLCustomAngleLabel.InternalPrint;
var
  w,o,h,t,l:integer;
  s:string;
  r:TRect;
  m,a:TBitmap;
begin
  inherited;
  //
  s:=Caption;
  r:=CalcPrintClientRect;
  with RequestParentSurface do
  begin
    GeneratorId:=Integer(Self);
    NewGroupId;
    Font:=Self.Font;
    o:=0;
    w:=TextWidth(Caption+' ');
    h:=TextHeight(Caption+' ');
    //
    m:=NewBitmap(w,h);
    try
      m.Transparent       :=Self.Transparent;
      m.TransparentColor  :=Self.Color;
      m.TransparentMode   :=tmFixed;
      m.Canvas.Font       :=Self.Font;
      m.Canvas.Brush.Color:=Self.Color;
      m.Canvas.Brush.Style:=bsSolid;
      m.Canvas.Pen.Style  :=psClear;
      m.Canvas.Rectangle(0,0,m.Width+1,m.Height+1);
      m.Canvas.TextOut(1,-1,s);
      //
      a:=RotatedBitmap(m,fAngle);
      try
        case Alignment of
          taCenter      : l:=(r.Left+r.Right-a.Width) div 2+o;
          taRightJustify: l:=r.Right-a.Width+o;
        else
          l:=r.Left+o;
        end;
        case Layout of
          tlCenter      : t:=(r.Top+r.Bottom-a.Height) div 2;
          tlBottom      : t:=r.Bottom-a.Height;
        else
          t:=r.Top;
        end;
        Draw(l,t,a);
      finally
        a.free;
      end;
    finally
      m.free;
    end;
  end;
end;

procedure TRLCustomAngleLabel.CalcSize(var aSize:TPoint);
var
  w:integer;
  c:string;
begin
  aSize:=Point(Width,Height);
  if not AutoSize then
    Exit;
  // texto a utilizar para o cálculo
  c:=Caption;
  if (c=emptystr) and not IsPreparing then
    c:=Name;
  // dimensões do texto
  aSize.X:=0;
  aSize.Y:=0;
  with TextBounds(c+' ',Self.Font,fAngle) do
  begin
    Inc(aSize.X,X);
    Inc(aSize.Y,Y);
  end;
  // adicional das bordas
  w:=fBorders.Width;
  if w>0 then
  begin
    Inc(w);
    if fBorders.CanDrawLeft then
      Inc(aSize.X,w);
    if fBorders.CanDrawTop then
      Inc(aSize.Y,w);
    if fBorders.CanDrawRight then
      Inc(aSize.X,w);
    if fBorders.CanDrawBottom then
      Inc(aSize.Y,w);
  end;
end;

procedure TRLCustomAngleLabel.SetAngle(const aValue:double);
begin
  if aValue=fAngle then
    Exit;
  fAngle:=aValue;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomAngleLabel.SetAngleBorders(const aValue:boolean);
begin
  if aValue=fAngleBorders then
    Exit;
  fAngleBorders:=aValue;
  Invalidate;
end;

function TRLCustomAngleLabel.IsAngle:boolean;
begin
  Result:=(abs(fAngle-Round(fAngle))<1/10);
end;

{ TRLCustomDBText }

constructor TRLCustomDBText.Create(aOwner:TComponent);
begin
  fText       :=emptystr;
  fDataField  :=emptystr;
  fDataFormula:=emptystr;
  //fDataSource :=nil;
  fDisplayMask:=emptystr;
  //
  inherited Create(aOwner);
end;

procedure TRLCustomDBText.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
      fDataSource:=nil;
end;

procedure TRLCustomDBText.SetDataSource(const aValue:TDataSource);
begin
  if aValue=fDataSource then
    Exit;
  fDataSource:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
  MakeCaption;
end;

procedure TRLCustomDBText.SetDataField(const aValue:TRLDataFieldProperty);
begin
  if aValue=fDataField then
    Exit;
  if aValue<>emptystr then
    fDataFormula:=emptystr;
  fDataField:=aValue;
  MakeCaption;
end;

procedure TRLCustomDBText.SetDataFormula(const aValue:string);
begin
  if aValue=fDataFormula then
    Exit;
  if aValue<>emptystr then
    fDataField:=emptystr;
  fDataFormula:=aValue;
  MakeCaption;
end;

function TRLCustomDBText.GetDataSet:TDataSet;
begin
  if Assigned(fDataSource) then
    Result:=fDataSource.DataSet
  else
    Result:=nil;
end;

function TRLCustomDBText.GetField:TField;
begin
  if (DataSet<>nil) and (fDataField<>emptystr) then
  begin
    Result:=DataSet.FindField(fDataField);
    if Result=nil then
      raise Exception.Create(LS_NotFoundStr+': '+Name+'.DataField "'+fDataField+'"');
  end
  else
    Result:=nil;
end;

function TRLCustomDBText.ApplyMask(const aValue:variant):string;
var
  m:string;
  p:TFloatFormat;
  c:integer;
  v:double;
begin
  if VarIsNull(aValue) or VarIsEmpty(aValue) then
    Result:=emptystr
  else if fDisplayMask=emptystr then
    if Field<>nil then
      if Field is TNumericField then
        with TNumericField(Field) do
        begin
          if EditFormat=emptystr then
            m:=DisplayFormat
          else
            m:=EditFormat;
          v:=aValue;
          if m<>emptystr then
            Result:=FormatFloat(m,v)
          else
          begin
            if (Field is TFloatField) and TFloatField(Field).Currency then
            begin
              p:=ffCurrency;
              c:=CurrencyDecimals;
            end
            else
            begin
              p:=ffGeneral;
              c:=0;
            end;
            if Field is TFloatField then
              Result:=FloatToStrF(v,p,TFloatField(Field).Precision,c)
            else
              Result:=FloatToStrF(v,p,0,c);
          end;
        end
      else
        Result:=aValue
    else
      Result:=aValue
  else if VarType(aValue) in [varSmallint,varInteger,varSingle,varDouble,varCurrency] then
    Result:=FormatFloat(fDisplayMask,aValue)
  else
    Result:=FormatMaskText(fDisplayMask,aValue);
end;

function TRLCustomDBText.GetFieldText:string;
var
  d:TDataSet;
  f:TField;
  p:TRLCustomReport;
begin
  p:=FindParentReport;
  if not IsPreparing then
    if fFriendlyName<>emptystr then
      Result:=fFriendlyName
    else if fDataField<>emptystr then
      Result:=GetFieldLabel
    else if fDataFormula<>emptystr then
      Result:=fDataFormula
    else
      Result:=Name
  else
  begin
    d:=GetDataSet;
    f:=GetField;
    if Assigned(d) and d.Active and not d.Eof then
      if f<>nil then
        Result:=SmartGetFieldDisplayText(f)
      else if fDataFormula<>emptystr then
        Result:=ApplyMask(p.Parse(Self,fDataFormula))
      else
        Result:=emptystr
    else
      Result:=emptystr;
  end;
end;

function TRLCustomDBText.InternalMakeCaption:string;
var
  i:integer;
begin
  Result:=GetFieldText;
  if fText<>emptystr then
  begin
    i:=Pos('#',fText);
    if i>0 then
      Result:=Copy(fText,1,i-1)+Result+Copy(fText,i+1,Length(fText))
    else
      Result:=fText+Result;
  end;
end;

procedure TRLCustomDBText.SetText(const aValue:TCaption);
begin
  if aValue=fText then
    Exit;
  fText:=aValue;
  MakeCaption;
end;

function TRLCustomDBText.GetFieldLabel: string;
var
  f:TField;
begin
  if (DataSet<>nil) and (fDataField<>emptystr) then
    f:=DataSet.FindField(fDataField)
  else
    f:=nil;
  if f<>nil then
    Result:=f.DisplayLabel
  else
    Result:=fDataField;
end;

function IsTextField(aField:TField):boolean;
begin
  if aField<>nil then
    Result:=(aField is TStringField) or (aField is TBinaryField) or (aField is TBlobField)
  else
    Result:=False;
end;

function GetFieldNullValue(aField:TField):variant;
begin
  if aField<>nil then
    if aField is TNumericField then
      Result:=0
    else if aField is TBooleanField then
      Result:=False
    else if aField is TStringField then
      Result:=emptystr
    else if aField is TDateTimeField then
      Result:=0
    else if aField is TBinaryField then
      Result:=emptystr
    else if aField is TBlobField then
      Result:=emptystr
    //else if aField is TObjectField then
    //  Result:=0
    else if aField is TVariantField then
      Result:=0
    //else if aField is TInterfaceField then
    //  Result:=emptystr
    else
      Result:=Null
  else
    Result:=Null;
end;

{ TRLCustomDBResult }

constructor TRLCustomDBResult.Create(aOwner:TComponent);
begin
  fInfo           :=riSimple;
  //fResetAfterPrint:=False;
  //fMustResetValue :=False;
  //fBuiltInRegs    :=nil;
  fComputeNulls   :=True;
  //
  fBuiltInRegs:=TObjectList.Create;
  //
  inherited Create(aOwner);
  //
  Initialize;
end;

destructor TRLCustomDBResult.Destroy;
begin
  inherited;
  //
  FreeObj(fBuiltInRegs);
end;

procedure TRLCustomDBResult.InternalPrint;
begin
  inherited;
  //
  if fResetAfterPrint then
    fMustResetValue:=True;
end;

procedure TRLCustomDBResult.Evaluate(var FieldText:string; var FieldValue:variant);
var
  DatasetRef:TDataSet;
  FieldRef  :TField;
begin
  DatasetRef:=DataSet;
  FieldRef  :=Field;
  FieldValue:=Null;
  FieldText :=emptystr;
  if fDataFormula<>emptystr then
  begin
    FieldValue:=RequestParentReport.Parse(Self,fDataFormula);
    if (VarIsNull(FieldValue) or VarIsEmpty(FieldValue)) and fComputeNulls then
      FieldValue:=GetNullValue;
    FieldText:=VarToStr(FieldValue);
  end
  else if Assigned(DatasetRef) and DatasetRef.Active and not DatasetRef.Eof and (FieldRef<>nil) then
  begin
    FieldValue:=FieldRef.Value;
    FieldText :=SmartGetFieldDisplayText(FieldRef);
  end;
end;

procedure TRLCustomDBResult.Initialize;
  procedure InitializeBuiltIn;
  var
    i:integer;
  begin
    for i:=0 to fBuiltInRegs.Count-1 do
      with TRLDBResultBuiltIn(fBuiltInRegs) do
      begin
        Count:=0;
        Sum  :=0;
      end;
  end;
begin
  fNullValue:=GetNullValue;
  fCount    :=0;
  fSum      :=0;
  fMax      :=fNullValue;
  fMin      :=fNullValue;
  fLast     :=fNullValue;
  fFirst    :=fNullValue;
  fLastText :=emptystr;
  fFirstText:=emptystr;
  fSimple   :=fNullValue;
  fBuiltInRegs.Clear;
  //
  Evaluate(fFirstText,fFirst);
  InitializeBuiltIn;
  fMin      :=fFirst;
  fMax      :=fFirst;
  fLast     :=fFirst;
  fLastText :=fFirstText;
  fSimple   :=fFirst;
end;

procedure TRLCustomDBResult.ComputeDetail(aCaller:TObject);
var
  fieldvalue:variant;
  fieldtext :string;
  computeit :boolean;
begin
  inherited;
  //
  if fMustResetValue then
  begin
    Initialize;
    fMustResetValue:=False;
  end;
  //
  Evaluate(fieldtext,fieldvalue);
  //
  computeit:=True;
  if Assigned(fOnCompute) then
    fOnCompute(Self,fieldvalue,fieldtext,computeit);
  if not computeit then
    Exit;
  //
  if VarIsNull(fieldvalue) or VarIsEmpty(fieldvalue) then
    if fComputeNulls then
      fieldvalue:=GetNullValue
    else
      Exit;
  fSimple:=fieldvalue;
  Inc(fCount);
  if fCount=1 then
  begin
    fFirst    :=fieldvalue;
    fFirstText:=fieldtext;
  end;
  fLast    :=fieldvalue;
  fLastText:=fieldtext;
  if VarIsNumeric(fieldvalue) then
    fSum:=fSum+fieldvalue;
  if (fCount=1) or (fieldvalue>fMax) then
    fMax:=fieldvalue;
  if (fCount=1) or (fieldvalue<fMin) then
    fMin:=fieldvalue;
end;

function TRLCustomDBResult.GetFieldText:string;
var
  n:string;
begin
  if not IsPreparing then
  begin
    if fFriendlyName<>emptystr then
      n:=fFriendlyName
    else if fDataField<>emptystr then
      n:=GetFieldLabel
    else if fDataFormula<>emptystr then
      n:=fDataFormula
    else
      n:=emptystr;
    case fInfo of
      riAverage  : Result:='(Average '+n+')';
      riCount    : Result:='(Count)';
      riFirst    : Result:='(First '+n+')';
      riLast     : Result:='(Last '+n+')';
      riMax      : Result:='(Max '+n+')';
      riMin      : Result:='(Min '+n+')';
      riSum      : Result:='(Sum '+n+')';
      riFirstText: Result:='(FirstText '+n+')';
      riLastText : Result:='(LastText '+n+')';
      riSimple   : Result:='(Simple '+n+')';
    else
      Result:='(???)';  
    end
  end
  else if VarIsNull(Self.Value) or VarIsEmpty(Self.Value) then
    Result:=emptystr
  else
    case fInfo of
      riCount   : Result:=Self.Value;
      riFirstText,
      riLastText: Result:=Self.Value;
    else
      Result:=ApplyMask(Self.Value);
    end;
end;

function TRLCustomDBResult.GetValue:variant;
begin
  case fInfo of
    riAverage  : if fCount=0 then
                   Result:=0
                 else
                   Result:=fSum/fCount;
    riCount    : Result:=fCount;
    riMax      : Result:=fMax;
    riMin      : Result:=fMin;
    riSum      : Result:=fSum;
    riFirst    : Result:=fFirst;
    riLast     : Result:=fLast;
    riFirstText: Result:=fFirstText;
    riLastText : Result:=fLastText;
    riSimple   : Result:=fSimple;
  else
    Result:=GetNullValue;
  end;
end;

function TRLCustomDBResult.GetNullValue:variant;
begin
  case fInfo of
    riAverage  : Result:=0;
    riCount    : Result:=0;
    riSum      : Result:=0;
    riFirstText: Result:=emptystr;
    riLastText : Result:=emptystr;
  else
    Result:=GetFieldNullValue(Field);
  end;
end; 

function TRLCustomDBResult.GetAttribute(const aName:string):variant; 
begin
  Result:=GetValue;
end;

function TRLCustomDBResult.Resolve(Sender:TObject; const Identifier:string; Params:variant):variant;
var
  id:integer;
begin
  id:=TRLExpressionParser(Sender).IdentifierId;
  if SameText(Identifier,'count') then
    Result:=BuiltInCount(id)
  else if SameText(Identifier,'sum') then
    Result:=BuiltInSum(id,Params[0])
  else if SameText(Identifier,'min') then
    Result:=BuiltInMin(id,Params[0])
  else if SameText(Identifier,'max') then
    Result:=BuiltInMax(id,Params[0])
  else if SameText(Identifier,'avg') then
    Result:=BuiltInAvg(id,Params[0])
  else if SameText(Identifier,'first') then
    Result:=BuiltInFirst(id,Params[0])
  else if SameText(Identifier,'last') then
    Result:=BuiltInLast(id,Params[0])
  else
    Result:=Unassigned;
end;

procedure TRLCustomDBResult.SetInfo(const aValue:TRLResultInfo);
begin
  if aValue=fInfo then
    Exit;
  fInfo:=aValue;
  MakeCaption;
end;

function TRLCustomDBResult.BuiltIn(aId:integer; aCanCreate:boolean=True):TRLDBResultBuiltIn;
var
  i:integer;
begin
  Result:=nil;
  for i:=0 to fBuiltInRegs.Count-1 do
    if TRLDBResultBuiltIn(fBuiltInRegs[i]).Id=aId then
    begin
      Result:=TRLDBResultBuiltIn(fBuiltInRegs[i]);
      Break;
    end;
  if (Result=nil) and aCanCreate then
  begin
    Result:=TRLDBResultBuiltIn.Create;
    Result.Id   :=aId;
    Result.Count:=0;
    Result.Max  :=Null;
    Result.Min  :=Null;
    Result.Sum  :=0;
    Result.First:=Null;
    Result.Last :=Null;
    fBuiltInRegs.Add(Result);
  end;
end;

function TRLCustomDBResult.BuiltInCount(aId:integer):variant;
begin
  with BuiltIn(aId) do
  begin
    Inc(Count);
    Result:=Count;
  end;
end;

function TRLCustomDBResult.BuiltInSum(aId:integer; aValue:variant):variant;
begin
  with BuiltIn(aId) do
  begin
    Sum:=Sum+aValue;
    Result:=Sum;
  end;
end;

function TRLCustomDBResult.BuiltInMin(aId:integer; aValue:variant):variant;
begin
  with BuiltIn(aId) do
  begin
    if VarIsNull(Min) or VarIsEmpty(Min) or (aValue<Min) then
      Min:=aValue;
    Result:=Min;
  end;
end;

function TRLCustomDBResult.BuiltInMax(aId:integer; aValue:variant):variant;
begin
  with BuiltIn(aId) do
  begin
    if VarIsNull(Max) or VarIsEmpty(Max) or (aValue<Max) then
      Max:=aValue;
    Result:=Max;
  end;
end;

function TRLCustomDBResult.BuiltInAvg(aId:integer; aValue:variant):variant;
begin
  with BuiltIn(aId) do
  begin
    Inc(Count);
    Sum:=Sum+aValue;
    Result:=Sum/Count;
  end;
end;

function TRLCustomDBResult.BuiltInFirst(aId:integer; aValue:variant):variant;
begin
  with BuiltIn(aId) do
  begin
    Inc(Count);
    if Count=1 then
      First:=aValue;
    Result:=First;
  end;
end;

function TRLCustomDBResult.BuiltInLast(aId:integer; aValue:variant):variant;
begin
  with BuiltIn(aId) do
  begin
    Result:=Last;
    Last  :=aValue;
  end;
end;

{ RunMemo }

function CanvasTextWidth(Canvas:TObject; const Text:string):integer;
begin
  if Canvas is TRLGraphicSurface then
    Result:=(Canvas as TRLGraphicSurface).TextWidth(Text)
  else if Canvas is TCanvas then
    Result:=(Canvas as TCanvas).TextWidth(Text)
  else
    Result:=0;
end;

function CanvasTextHeight(Canvas:TObject; const Text:string):integer;
begin
  if Canvas is TRLGraphicSurface then
    Result:=(Canvas as TRLGraphicSurface).TextHeight(Text)
  else if Canvas is TCanvas then
    Result:=(Canvas as TCanvas).TextHeight(Text)
  else
    Result:=0;
end;

function NextLine(const Buffer:string; var Pos:integer; var Wrapped:boolean; var LineWidth:integer; Canvas:TObject; MaxWidth:integer):string;
var
  SpaceWidth,TabWidth,Pos0,PosAux:integer;
  HasText:boolean;
begin
  SpaceWidth:=CanvasTextWidth(Canvas,' ');
  TabWidth  :=SpaceWidth*8;
  Wrapped   :=False;
  LineWidth :=0;
  HasText   :=False;
  //
  Pos0:=Pos;
  while True do
    if Pos<=Length(Buffer) then
      if Buffer[Pos] in [#9,#32] then
      begin
        if (LineWidth>MaxWidth) and HasText then
        begin
          Wrapped:=True;
          Result :=Copy(Buffer,Pos0,Pos-Pos0);
          while (Pos<=Length(Buffer)) and (Buffer[Pos] in [#9,#32]) do
            Inc(Pos);
          Break;
        end;
        if Buffer[Pos]=#9 then
          Inc(LineWidth,TabWidth)
        else
          Inc(LineWidth,SpaceWidth);
        Inc(Pos);
      end
      else if Buffer[Pos] in [#13] then
      begin
        Result:=Copy(Buffer,Pos0,Pos-Pos0);
        Inc(Pos);
        if (Pos<=Length(Buffer)) and (Buffer[Pos] in [#10]) then
          Inc(Pos);
        Break;
      end
      else
      begin
        PosAux:=Pos;
        while (Pos<=Length(Buffer)) and not (Buffer[Pos] in [#9,#32,#13]) do
        begin
          Inc(LineWidth,CanvasTextWidth(Canvas,Buffer[Pos]));
          Inc(Pos);
        end;
        if (LineWidth>MaxWidth) and HasText then
        begin
          Pos    :=PosAux;
          Wrapped:=True;
          Result :=Copy(Buffer,Pos0,Pos-Pos0);
          while (Pos<=Length(Buffer)) and (Buffer[Pos] in [#9,#32]) do
            Inc(Pos);
          Break;
        end;
        HasText:=True;
      end
    else
    begin
      Result:=Copy(Buffer,Pos0,Pos-Pos0);
      Break;
    end;
end;

procedure CanvasTextRect(Canvas:TObject; const ARect:TRect; const Text:string; Alignment:TRLTextAlignment=taLeftJustify);
var
  X,Y:integer;
begin
  if Canvas is TRLGraphicSurface then
  begin
    case Alignment of
      taCenter      : X:=(ARect.Left+ARect.Right-(Canvas as TRLGraphicSurface).TextWidth(Text)) div 2;
      taRightJustify: X:=ARect.Right-(Canvas as TRLGraphicSurface).TextWidth(Text);
      taJustify     : X:=ARect.Left;
    else // taLeftJustify
      X:=ARect.Left;
    end;
    Y:=ARect.Top;
    (Canvas as TRLGraphicSurface).TextRectEx(ARect,X,Y,Text,ToMetaTextAlignment(TAlignment(Alignment)),ToMetaTextLayout(TTextLayout(tlTop)),MetaTextFlagIntegralHeight or MetaTextFlagAutoSize);
  end
  else if Canvas is TCanvas then
    CanvasTextRectEx(Canvas as TCanvas,ARect,ARect.Left,ARect.Top,Text,ToMetaTextAlignment(TAlignment(Alignment)),ToMetaTextLayout(TTextLayout(tlTop)),MetaTextFlagIntegralHeight);
end;

procedure RunMemo(const Buffer:string; Canvas:TObject; Alignment:TRLTextAlignment; const ARect:TRect; MaxWidth,MaxHeight:integer; var Size:TPoint);
var
  LineHeight:integer;
  LineWidth :integer;
  LineOffset:integer;
  Pos       :integer;
  Wrapped   :boolean;
  TextLn    :string;
  Aux       :TRect;
  LineAlign :TRLTextAlignment;
begin
  Size.X     :=0;
  LineHeight :=CanvasTextHeight(Canvas,'A');
  LineOffset :=0;
  Pos        :=1;
  while Pos<=Length(Buffer) do
  begin
    TextLn:=NextLine(Buffer,Pos,Wrapped,LineWidth,Canvas,MaxWidth);
    if LineWidth>Size.X then
      Size.X:=LineWidth;
    Aux:=Rect(ARect.Left+0,ARect.Top+LineOffset,ARect.Left+MaxWidth,ARect.Top+LineOffset+LineHeight);
    if Aux.Bottom>ARect.Bottom then
      Aux.Bottom:=ARect.Bottom;
    LineAlign:=Alignment;
    if (LineAlign=taJustify) and not Wrapped then
      LineAlign:=taLeftJustify;
    CanvasTextRect(Canvas,Aux,TextLn,LineAlign);
    Inc(LineOffset,LineHeight);
    if LineOffset>MaxHeight then
      Break;
  end;
  Size.Y:=LineOffset;
end;

function MemoSize(const Buffer:string; Font:TFont; MaxWidth:integer):TPoint;
var
  b:TBitmap;
begin
  b:=AuxBitmapNeeded;
  b.Canvas.Font.Assign(Font);
  RunMemo(Buffer,b.Canvas,taLeftJustify,Rect(0,0,MaxWidth,MaxInt),MaxWidth,MaxInt,Result);
  FreeAndNil(b);
end;

procedure MemoDraw(const Buffer:string; Canvas:TObject; Alignment:TRLTextAlignment; const ARect:TRect; WordWrap:boolean);
var
  FooSize  :TPoint;
  MaxWidth :integer;
  MaxHeight:integer;
begin
  if WordWrap then
    MaxWidth:=ARect.Right-ARect.Left
  else
    MaxWidth:=MaxInt;
  MaxHeight:=ARect.Bottom-ARect.Top;  
  RunMemo(Buffer,Canvas,Alignment,ARect,MaxWidth,MaxHeight,FooSize);
end;

{ TRLCustomMultiLine }

constructor TRLCustomMultiLine.Create(aOwner:TComponent);
begin
  fWordWrap      :=True;
  fIntegralHeight:=False;
  inherited Create(aOwner);
  // customization
  Width      :=185;
  Height     :=89;
  Behavior   :=[beSiteExpander];
  fDefaultBehavior:=Behavior;
  AutoSizeDir:=[asHeightDir];
  AutoSize   :=True;
end;

procedure TRLCustomMultiLine.InternalPaint;
var
  r:TRect;
  s:string;
begin
  PaintAsCustomControl;
  s:=Caption;
  if (s=emptystr) and not IsPreparing then
    s:=Name;
  r:=GetClientRect;
  with Canvas do
  begin
    Font:=Self.Font;
    if IsTransparent(Self) then
      Brush.Style:=bsClear
    else
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=Self.Color;
    end;
    MemoDraw(s,Self.Canvas,Self.Alignment,r,Self.WordWrap);
  end;
end;

procedure TRLCustomMultiLine.InternalPrint;
var
  r:TRect;
begin
  inherited;
  //
  r:=CalcPrintClientRect;
  with RequestParentSurface do
  begin
    GeneratorId:=Integer(Self);
    NewGroupId;
    Font:=Self.Font;
    if IsTransparent(Self) then
      Brush.Style:=bsClear
    else
    begin
      Brush.Style:=bsSolid;
      Brush.Color:=Self.Color;
    end;
    MemoDraw(Caption,Self.RequestParentSurface,Self.Alignment,r,Self.WordWrap);
  end;
end;

procedure TRLCustomMultiLine.CalcSize(var aSize:TPoint);
var
  w:integer;
  aux:TPoint;
  c:string;
begin
  aSize:=Point(Width,Height);
  if not AutoSize then
    Exit;
  // texto a utilizar para o cálculo
  c:=Caption;
  if (c=emptystr) and not IsPreparing then
    c:=Name;
  // dimensões do texto
  aSize.Y:=0;
  aux:=MemoSize(c,Self.Font,aSize.X);
  if aux.Y=0 then
    Inc(aSize.Y,TextBounds(' ',Self.Font,0).Y)
  else  
    Inc(aSize.Y,aux.Y);
  // adicional das bordas
  w:=fBorders.Width;
  if w>0 then
  begin
    Inc(w);
    if fBorders.CanDrawTop then
      Inc(aSize.Y,w);
    if fBorders.CanDrawBottom then
      Inc(aSize.Y,w);
  end;
end;

procedure TRLCustomMultiLine.SetWordWrap(const aValue:boolean);
begin
  if aValue=fWordWrap then
    Exit;
  fWordWrap:=aValue;
  MakeCaption;
end;

{ TRLCustomMemo }

constructor TRLCustomMemo.Create(aOwner:TComponent);
begin
  fLines:=TStringList.Create;
  TStringList(fLines).OnChange:=TreatOnChange;
  //
  inherited Create(aOwner);
end;

destructor TRLCustomMemo.Destroy;
begin
  FreeObj(fLines);
  //
  inherited;
end;

function TRLCustomMemo.InternalMakeCaption:string;
begin
  Result:=fLines.Text; 
  if not IsPreparing and (Trim(Result)=emptystr) then
    Result:=GetDefaultCaption;
end;

procedure TRLCustomMemo.SetLines(const aValue:TStrings);
begin
  fLines.Assign(aValue);
end;

procedure TRLCustomMemo.TreatOnChange(Sender:TObject);
begin
  MakeCaption;
end;

{ TRLCustomDBMemo }

constructor TRLCustomDBMemo.Create(aOwner:TComponent);
begin
  fDataField :=emptystr;
  fDataSource:=nil;
  //
  inherited Create(aOwner);
end;

procedure TRLCustomDBMemo.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
      fDataSource:=nil;
end;

procedure TRLCustomDBMemo.SetDataSource(const aValue:TDataSource);
begin
  if aValue=fDataSource then
    Exit;
  fDataSource:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
  MakeCaption;
end;

procedure TRLCustomDBMemo.SetDataField(const aValue:TRLDataFieldProperty);
begin
  if aValue=fDataField then
    Exit;
  if aValue<>emptystr then
    fDataFormula:=emptystr;
  fDataField:=aValue;
  MakeCaption;
end;

procedure TRLCustomDBMemo.SetDataFormula(const aValue:string);
begin
  if aValue=fDataFormula then
    Exit;
  if aValue<>emptystr then
    fDataField:=emptystr;
  fDataFormula:=aValue;
  MakeCaption;
end;

function TRLCustomDBMemo.GetDataSet:TDataSet;
begin
  if Assigned(fDataSource) then
    Result:=fDataSource.DataSet
  else
    Result:=nil;
end;

function TRLCustomDBMemo.GetField:TField;
begin
  if (DataSet<>nil) and (fDataField<>emptystr) then
  begin
    Result:=DataSet.FindField(fDataField);
    if Result=nil then
      raise Exception.Create(LS_NotFoundStr+': '+Name+'.DataField "'+fDataField+'"');
  end
  else
    Result:=nil;
end;

function TRLCustomDBMemo.GetFieldText:string;
var
  d:TDataSet;
  f:TField;
  p:TRLCustomReport;
begin
  p:=FindParentReport;
  if not IsPreparing then
    if fFriendlyName<>emptystr then
      Result:=fFriendlyName
    else if fDataField<>emptystr then
      Result:=GetFieldLabel
    else if fDataFormula<>emptystr then
      Result:=fDataFormula
    else
      Result:=Name
  else
  begin
    d:=GetDataSet;
    f:=GetField;
    if Assigned(d) and d.Active and not d.Eof then
      if f<>nil then
        Result:=SmartGetFieldDisplayText(f)
      else if fDataFormula<>emptystr then
        Result:=p.Parse(Self,fDataFormula)
      else
        Result:=emptystr
    else
      Result:=emptystr;
  end;
end;

function TRLCustomDBMemo.InternalMakeCaption:string;
begin
  Result:=GetFieldText;
end;

function TRLCustomDBMemo.GetFieldLabel: string;
var
  f:TField;
begin
  if (DataSet<>nil) and (fDataField<>emptystr) then
    f:=DataSet.FindField(fDataField)
  else
    f:=nil;
  if f<>nil then
    Result:=f.DisplayLabel
  else
    Result:=fDataField;
end;

{ TRLCustomImage }

constructor TRLCustomImage.Create(aOwner:TComponent);
begin
  // variables
  //fStretch:=False;
  //fCenter :=False;
  //fScaled :=False;
  // objects
  fPicture         :=TPicture.Create;
  fPicture.OnChange:=PictureChanged;
  //
  inherited Create(aOwner);
  // customization
  Height     :=105;
  Width      :=105;
  AutoSizeDir:=[asWidthDir,asHeightDir];
end;

destructor TRLCustomImage.Destroy;
begin
  FreeObj(fPicture);
  //
  inherited;
end;

procedure TRLCustomImage.CalcSize(var aSize:TPoint);
var
  w:integer;
begin
  aSize:=Point(Width,Height);
  if (fPicture=nil) or not AutoSize then
    Exit;
  // pega size da imagem
  aSize.X:=fPicture.Width;
  aSize.Y:=fPicture.Height;
  // adicional das bordas
  w:=fBorders.Width;
  if w>0 then
  begin
    Inc(w);
    if fBorders.CanDrawLeft then
      Inc(aSize.X,w);
    if fBorders.CanDrawTop then
      Inc(aSize.Y,w);
    if fBorders.CanDrawRight then
      Inc(aSize.X,w);
    if fBorders.CanDrawBottom then
      Inc(aSize.Y,w);
  end;
end;

procedure TRLCustomImage.InternalPaint;
var
  r:TRect;
  b:TBitmap;
begin
  PaintAsCustomControl;
  r:=GetClientRect;
  if (fPicture<>nil) and (fPicture.Graphic<>nil) then
    if fScaled then
    begin
      r:=ScaleRect(Rect(0,0,fPicture.Graphic.Width,fPicture.Graphic.Height),r,fCenter);
      Canvas.StretchDraw(r,fPicture.Graphic);
    end
    else if fStretch then
      Canvas.StretchDraw(r,fPicture.Graphic)
    else
    begin
      b:=ClipGraphic(fPicture.Graphic,r,fCenter);
      try
        Canvas.StretchDraw(r,b);
      finally
        b.free;
      end;
    end;
end;

procedure TRLCustomImage.PictureChanged(Sender:TObject);
begin
  if AutoSize and (fPicture.Width>0) and (fPicture.Height>0) then
    BoundsRect:=Rect(Left,Top,Left+fPicture.Width,Top+fPicture.Height);
end;

procedure TRLCustomImage.InternalPrint;
var
  r:TRect;
begin
  inherited;
  //
  r:=CalcPrintClientRect;
  if (fPicture<>nil) and (fPicture.Graphic<>nil) then
    with RequestParentSurface do
    begin
      GeneratorId:=Integer(Self);
      NewGroupId;
      if fScaled then
        ScaleDraw(r,fPicture.Graphic,fCenter)
      else if fStretch then
        StretchDraw(r,fPicture.Graphic)
      else
        ClipDraw(r,fPicture.Graphic,fCenter);
    end;
end;

procedure TRLCustomImage.SetCenter(const aValue:boolean);
begin
  if aValue=fCenter then
    Exit;
  fCenter:=aValue;
  Invalidate;
end;

procedure TRLCustomImage.SetPicture(const aValue:TPicture);
begin
  fPicture.Assign(aValue);
  Invalidate;
end;

procedure TRLCustomImage.SetStretch(const aValue:boolean);
begin
  if aValue=fStretch then
    Exit;
  fStretch:=aValue;
  if fStretch then
    fScaled:=False;
  Invalidate;
end;

procedure TRLCustomImage.SetScaled(const aValue:boolean);
begin
  if aValue=fScaled then
    Exit;
  fScaled:=aValue;
  if fScaled then
    fStretch:=False;
  Invalidate;
end;

{ TRLCustomDBImage }

constructor TRLCustomDBImage.Create(aOwner:TComponent);
begin
  fDataField :=emptystr;
  //fDataSource:=nil;
  //
  inherited Create(aOwner);
end;

procedure TRLCustomDBImage.LoadPicture;
var
  f:TField;
begin
  fPicture.Graphic:=nil;
  f:=GetField;
  if (f<>nil) and f.DataSet.Active and not f.DataSet.Eof then
    fPicture.Assign(f);
end;

procedure TRLCustomDBImage.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
    begin
      LoadPicture;
      Invalidate;
      fDataSource:=nil;
    end;
end;

procedure TRLCustomDBImage.SetDataSource(const aValue:TDataSource);
begin
  if aValue=fDataSource then
    Exit;
  fDataSource:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
  LoadPicture;
  Invalidate;
end;

procedure TRLCustomDBImage.SetDataField(const aValue:TRLDataFieldProperty);
begin
  if aValue=fDataField then
    Exit;
  fDataField:=aValue;
  LoadPicture;
  AdjustBounds;
  Invalidate;
end;

function TRLCustomDBImage.GetDataSet:TDataSet;
begin
  if Assigned(fDataSource) then
    Result:=fDataSource.DataSet
  else
    Result:=nil;
end;

function TRLCustomDBImage.GetField:TField;
begin
  if (DataSet<>nil) and (fDataField<>emptystr) then
  begin
    Result:=DataSet.FindField(fDataField);
    if Result=nil then
      raise Exception.Create(LS_NotFoundStr+': '+Name+'.DataField "'+fDataField+'"');
  end
  else
    Result:=nil;
end;

procedure TRLCustomDBImage.InternalPrint;
begin
  LoadPicture;
  //
  inherited;
end;

{ TRLCustomSystemInfo }

constructor TRLCustomSystemInfo.Create(aOwner:TComponent);
begin
  fInfoType:=itDate;
  fText    :=emptystr;
  //
  inherited Create(aOwner);
end;


function TRLCustomSystemInfo.InternalMakeCaption:string;
const
  CLEARCONST='^CLEAR';
var
  i,p:integer;
  s,v:string;
  r  :TRLCustomReport;
  function JunctionStr:string;
  var
    s,t1,t2:string;
    i:integer;
    r:TRLReportState;
    p:TRLCustomReport;
  begin
    // a propriedade TEXT pode vir na seguinte forma: "Esta página # o relatório\|continua;encerra";
    t1:=emptystr;
    t2:=emptystr;
    s:=fText;
    i:=Pos('|',s);
    if i>0 then
    begin
      // elimina os textos antes da barra e após a segunda barra, se houver
      Delete(s,1,i);
      i:=Pos('|',s);
      if i>0 then
        s:=Copy(s,1,i-1);
      // primeiro e segundo parâmetros
      i:=Pos(';',s);
      if i=0 then
        i:=Length(s)+1;
      t1:=Copy(s,1,i-1);
      t2:=Copy(s,i+1,MaxInt);
    end;
    if t1=emptystr then
      t1:=LS_PageBreakStr;
    if t2=emptystr then
      t2:=LS_ReportEndStr;
    //
    p:=MasterReport;
    if Assigned(p) then
      r:=p.ReportState
    else
      r:=rsAbout;
    if r=rsClosing then
      Result:=t2
    else
      Result:=t1;
  end;
  function MendStr:string;
  var
    s,t1,t2:string;
    i,q:integer;
  begin
    // a propriedade TEXT pode vir na seguinte forma: "Esta página é a # o relatório\|continuação;";
    t1:=emptystr;
    t2:=emptystr;
    s:=fText;
    i:=Pos('|',s);
    if i>0 then
    begin
      // elimina os textos antes da barra e após a segunda barra, se houver
      Delete(s,1,i);
      i:=Pos('|',s);
      if i>0 then
        s:=Copy(s,1,i-1);
      // primeiro e segundo parâmetros
      i:=Pos(';',s);
      if i=0 then
        i:=Length(s)+1;
      t1:=Copy(s,1,i-1);
      t2:=Copy(s,i+1,MaxInt);
    end;
    if t1=emptystr then
      t1:=LS_PageMendStr;
    if t2=emptystr then
      t2:=CLEARCONST;  
    //
    q:=RequestParentPager.DetailCount;
    if q>0 then
      Result:=t1
    else
      Result:=t2;
  end;
  function PagePreviewStr:string;
  begin
    Result:=IntToStr(MasterReport.PageNumber)+#9+'{LastPageNumber}';
  end;

  {Retorna as informação do arquivo}
Function Versioninfo(FileInfo: TRLVersionType): String;
Const
Sfi = '\\StringFileInfo\\04';
var
 VerSize : DWORD;
 sid: String;
 Zero    : THandle;
 PBlock  : Pointer;
 PS      : Pointer;
 Size    : UINT;
begin
  //todo: ver a funo deste metodo e implementar multi plataforma em outra unit
  {
  //Laguage = locale id 
  sid:= Format('%.x', [SysLocale.PriLangID]);
  // Reserva o tamanho para alocação de memoria
  VerSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Zero);
  if VerSize = 0 then
       Exit;

  // Aloca memória
  GetMem(PBlock, VerSize);
  // Verifica a versão do resorce
  GetFileVersionInfo(PChar(ParamStr(0)), 0, VerSize, PBlock);
  // Predefinição do tamanho na memoria
  GetMem(PS, 256);

  Case FileInfo of
       vtCompanyName: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\CompanyName'),PS, Size);
       vtFileDescription: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\FileDescription'),PS, Size);
       vtFileVersion: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\FileVersion'),PS, Size);
       vtInternalName: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\InternalName'),PS, Size);
       vtLegalCopyright: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\LegalCopyright'),PS, Size);
       vtLegalTrademarks: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\LegalTrademarks'),PS, Size);
       vtOriginalFilename: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\OriginalFilename'),PS, Size);
       vtProductName: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\ProductName'),PS, Size);
       vtProductVersion: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\ProductVersion'),PS, Size);
       vtComments: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\Comments'),PS, Size);
       vtemail: VerQueryValue(PBlock, pchar(Sfi + sid + '04E4\\email'),PS, Size);
      end;

  Result:= StrPas(PS);
  Freemem(PBlock);
  }
end;

begin
  r:=FindParentReport;
  //
  if not IsPreparing then
    if fFriendlyName<>emptystr then
      s:=fFriendlyName
    else if fInfoType=itPagePreview then
      s:='('+InfoTypeNames[itPageNumber]+')'+#9+'('+InfoTypeNames[itLastPageNumber]+')'
    else
      s:='('+InfoTypeNames[fInfoType]+')'
  else
    case fInfoType of
      itCarbonCopy      : s:=IntToStr(FindParentBand.CarbonIndex+1);
      itDate            : s:=DateToStr(MasterReport.ReportDateTime);
      itDetailCount     : s:=IntToStr(FindParentPager.DetailCount);
      itFullDate        : s:=FormatDateTime(LongDateFormat,MasterReport.ReportDateTime);
      itHour            : s:=TimeToStr(MasterReport.ReportDateTime);
      itJunction        : s:=JunctionStr;
      itLastPageNumber  : s:='{LastPageNumber}';
      itMend            : s:=MendStr;
      itNow             : s:=DateTimeToStr(MasterReport.ReportDateTime);
      itPageNumber      : s:=IntToStr(MasterReport.PageNumber);
      itPagePreview     : s:=PagePreviewStr;
      itTitle           : s:=r.Title;
      itRecNo           : s:=IntToStr(FindParentSkipper.RecNo);
      itCopyNo          : s:=IntToStr(FindParentSkipper.CopyNo);
      itCompanyName     : s:= Versioninfo(vtCompanyName);
      itFileDescription : s:= Versioninfo(vtFileDescription);
      itFileVersion     : s:= Versioninfo(vtFileVersion);
      itInternalName    : s:= Versioninfo(vtInternalName);
      itLegalCopyright  : s:= Versioninfo(vtLegalCopyright);
      itLegalTrademarks : s:= Versioninfo(vtLegalTrademarks);
      itOriginalFilename: s:= Versioninfo(vtOriginalFilename);
      itProductName     : s:= Versioninfo(vtProductName);
      itProductVersion  : s:= Versioninfo(vtProductVersion);
      itComments        : s:= Versioninfo(vtComments);
    end;
    
  // brecha para eliminar o texto
  if Pos(CLEARCONST,s)>0 then
    Result:=emptystr
  else
  begin
    // elimina opções embutidas em TEXT
    Result:=fText;
    i:=Pos('|',Result);
    if i>0 then
      Result:=Copy(Result,1,i-1);
    // substitui parâmetros em TEXT
    repeat
      // próximo parâmetro de S
      p:=Pos(#9,s);
      if p=0 then
        p:=Length(s)+1;
      v:=Copy(s,1,p-1);
      Delete(s,1,p);
      // próximo lugar em Result
      i:=Pos('#',Result);
      if i=0 then
        Result:=Result+v
      else
      begin
        Delete(Result,i,1);
        Insert(v,Result,i);
      end;
    until s=emptystr;
  end;
end;

procedure TRLCustomSystemInfo.SetInfoType(const aValue:TRLInfoType);
begin
  if aValue=fInfoType then
    Exit;
  fInfoType:=aValue;
  MakeCaption;
end;

procedure TRLCustomSystemInfo.SetText(const aValue:TCaption);
begin
  if aValue=fText then
    Exit;
  fText:=aValue;
  MakeCaption;
end;

{ TRLCustomDraw }

constructor TRLCustomDraw.Create(aOwner:TComponent);
begin
  // initialization
  //fAngle     :=0;
  fDrawKind  :=dkRectangle;
  fCenter    :=True;
  //fDrawData  :=nil;
  //fDrawWidth :=0;
  //fDrawHeight:=0;
  //fOptions   :=[];
  // objects
  fBrush         :=TBrush.Create;
  fBrush.OnChange:=ChangeResponse;
  fPen           :=TPen.Create;
  fPen.OnChange  :=ChangeResponse;
  fDrawData      :=TStringList.Create;
  //
  inherited Create(aOwner);
  // customization
  Width :=48;
  Height:=48;
end;

destructor TRLCustomDraw.Destroy;
begin
  FreeObj(fDrawData);
  FreeObj(fBrush);
  FreeObj(fPen);
  //
  inherited;
end;

procedure PointArray(aPoints:array of TPoint; var aDest:TPointArray);
var
  i:integer;
begin
  SetLength(aDest,High(aPoints)+1);
  for i:=0 to High(aPoints) do
    aDest[i]:=aPoints[i]; 
end;

procedure RectToPoints(const aRect:TRect; var aPoints:TPointArray);
begin
  SetLength(aPoints,4);
  aPoints[0].x:=aRect.Left;
  aPoints[0].y:=aRect.Top;
  aPoints[1].x:=aRect.Right;
  aPoints[1].y:=aRect.Top;
  aPoints[2].x:=aRect.Right;
  aPoints[2].y:=aRect.Bottom;
  aPoints[3].x:=aRect.Left;
  aPoints[3].y:=aRect.Bottom;
end;

procedure ProduceRectanglePoints(var aDest:TPointArray);
begin
  PointArray([Point(0,0),Point(1,0),Point(1,1),Point(0,1)],aDest);
end;

procedure ProduceElipsePoints(var aDest:TPointArray);
const
  MaxPoints=36;
  Axis     =1000;        
var
  d,s,c:double;
  i    :integer;
begin
  SetLength(aDest,MaxPoints);
  i:=0;
  while i<MaxPoints do
  begin
    d:=2*Pi*i/MaxPoints;
    s:=Sin(d);
    c:=Cos(d);
    aDest[i]:=Point(Round(Axis+c*Axis),Round(Axis+s*Axis));
    Inc(i);
  end;
end;

procedure ProduceLinePoints(var aDest:TPointArray);
begin
  PointArray([Point(0,0),Point(1,0)],aDest);
end;

procedure ProduceTriaglePoints(var aDest:TPointArray);
begin
  PointArray([Point(0,87),Point(50,0),Point(100,87)],aDest);
end;

procedure ProduceArrowPoints(var aDest:TPointArray);
begin
  PointArray([Point(0,2),Point(5,2),Point(5,0),Point(8,3),Point(5,6),Point(5,4),Point(0,4)],aDest);
end;

function PointsToStr(const aPoints:TPointArray):string;
var
  len,i:integer;
begin
  Result:=emptystr;
  len:=High(aPoints)+1;
  for i:=0 to len-1 do
  begin
    if i>0 then
      Result:=Result+#13;
    Result:=Result+IntToStr(aPoints[i].x)+' '+IntToStr(aPoints[i].y);
  end;
end;

procedure ProducePolygonPoints(var aDest:TPointArray; const aPoints:string);
  function NextInt(var i,n:integer):boolean;
  const
    SpaceSet=[#32,#9,#13,#10,#26];
    NumSet  =['0'..'9'];
  var
    m:integer;
  begin
    Result:=False;
    while (i<=Length(aPoints)) and (aPoints[i] in SpaceSet) do
      Inc(i);
    m:=i;
    while (i<=Length(aPoints)) and (aPoints[i] in NumSet) do
      Inc(i);
    if i-m>0 then
    begin
      n     :=StrToIntDef(Copy(aPoints,m,i-m),0);
      Result:=True;
    end;
  end;
var
  i,q:integer;
  p:TPoint;
begin
  // conta os pontos
  q:=0;
  i:=1;
  while NextInt(i,p.x) do
    if NextInt(i,p.y) then
      Inc(q);
  // popula
  SetLength(aDest,q);
  q:=0;
  i:=1;
  while NextInt(i,p.x) do
    if NextInt(i,p.y) then
    begin
      aDest[q]:=p;
      Inc(q);
    end;
end;

procedure TRLCustomDraw.ProducePoints(var aDest:TPointArray);
begin
  case fDrawKind of
    dkRectangle: ProduceRectanglePoints(aDest);
    dkTriangle : ProduceTriaglePoints(aDest);
    dkLine     : ProduceLinePoints(aDest);
    dkElipse   : ProduceElipsePoints(aDest);
    dkArrow    : ProduceArrowPoints(aDest);
    dkCustom   : ProducePolygonPoints(aDest,fDrawData.Text);
  else
    SetLength(aDest,0);
  end;
end;

procedure TRLCustomDraw.ScaleToFit(var aPoints:TPointArray; const aRect:TRect);
var
  r:TRect;
  n:integer;
begin
  r:=aRect;
  if fDrawWidth<>0 then
    r.Right:=r.Left+fDrawWidth;
  if fDrawHeight<>0 then
    r.Bottom:=r.Top+fDrawHeight;
  if doKeepVisible in fOptions then
  begin
    n       :=Min(r.Right-r.Left,r.Bottom-r.Top);
    r.Right :=r.Left+n;
    r.Bottom:=r.Top +n;
  end;
  if doKeepAspectRatio in fOptions then
    ScalePoints(aPoints,r)
  else
    StretchPoints(aPoints,r);
end;

procedure TRLCustomDraw.InternalPaint;
var
  r:TRect;
  p:TPointArray;
begin
  PaintAsCustomControl;
  r:=GetClientRect;
  with Canvas do
  begin
    Brush:=Self.Brush;
    Pen  :=Self.Pen;
    //
    Dec(r.Right);
    Dec(r.Bottom);
    if Pen.Width>1 then
    begin
      Inc(r.Left,Pen.Width div 2);
      Inc(r.Top,Pen.Width div 2);
      Dec(r.Right,(Pen.Width-1) div 2);
      Dec(r.Bottom,(Pen.Width-1) div 2);
    end;
    //
    ProducePoints(p);
    ScaleToFit(p,r);
    RotatePoints(p,fAngle);
    if doKeepSize in fOptions then
    else
      ScaleToFit(p,r);
    if fCenter then
      CenterPoints(p,r);
    Polygon(p);
  end;
end;

procedure TRLCustomDraw.InternalPrint;
var
  r:TRect;
  p:TPointArray;
begin
  inherited;
  //
  r:=CalcPrintClientRect;
  with RequestParentSurface do
  begin
    GeneratorId:=Integer(Self);
    NewGroupId;
    Brush:=Self.Brush;
    Pen  :=Self.Pen;
    //
    Dec(r.Right);
    Dec(r.Bottom);
    if Pen.Width>1 then
    begin
      Inc(r.Left,Pen.Width div 2);
      Inc(r.Top,Pen.Width div 2);
      Dec(r.Right,(Pen.Width-1) div 2);
      Dec(r.Bottom,(Pen.Width-1) div 2);
    end;
    //
    ProducePoints(p);
    ScaleToFit(p,r);
    RotatePoints(p,fAngle);
    if doKeepSize in fOptions then
    else
      ScaleToFit(p,r);
    if fCenter then
      CenterPoints(p,r);
    Polygon(p);
  end;
end;

procedure TRLCustomDraw.ChangeResponse(Sender:TObject);
begin
  Invalidate;
end;

procedure TRLCustomDraw.SetAngle(const aValue:double);
begin
  if aValue=fAngle then
    Exit;
  fAngle:=aValue;
  Invalidate;
end;

procedure TRLCustomDraw.SetBrush(const aValue:TBrush);
begin
  fBrush.Assign(aValue);
  Invalidate;
end;

procedure TRLCustomDraw.SetDrawKind(const aValue:TRLDrawKind);
var
  p:TPointArray;
begin
  if aValue=fDrawKind then
    Exit;
  fDrawKind:=aValue;
  if fDrawKind<>dkCustom then
  begin
    ProducePoints(p);
    fDrawData.Text:=PointsToStr(p);
  end;
  Invalidate;
end;

procedure TRLCustomDraw.SetPen(const aValue:TPen);
begin
  fPen.Assign(aValue);
  Invalidate;
end;

function TRLCustomDraw.IsAngle: boolean;
begin
  Result:=(Abs(fAngle-Round(fAngle))<1/10);
end;

function TRLCustomDraw.IsDrawData: Boolean;
begin
  Result:=(fDrawKind in [dkCustom]);
end;

procedure TRLCustomDraw.SetDrawData(const Value:TStrings);
begin
  if Value.Text=fDrawData.Text then
    Exit;
  fDrawData.Assign(Value);
  fDrawKind:=dkCustom;
  Invalidate;
end;

procedure TRLCustomDraw.SetCenter(const Value: boolean);
begin
  if fCenter=Value then
    Exit;
  fCenter:=Value;
  Invalidate;
end;

procedure TRLCustomDraw.ReadKind(Reader:TReader);
var
  kindname:string;
begin
  kindname:=Reader.ReadIdent;
  if SameText(kindname,'dkRectangle') then
  begin
    fDrawKind:=dkRectangle;
    fOptions :=[doKeepSize];
  end  
  else if SameText(kindname,'dkLine') then
  begin
    fDrawKind     :=dkCustom;
    fDrawData.Text:='0 0 1 1';
    fOptions      :=[doKeepSize];
  end
  else if SameText(kindname,'dkTriangle') then
  begin
    fDrawKind:=dkTriangle;
    fOptions :=[doKeepSize];
  end
  else if SameText(kindname,'dkElipse') then
  begin
    fDrawKind:=dkElipse;
    fOptions :=[doKeepSize];
  end
  else if SameText(kindname,'dkArrow') then
  begin
    fDrawKind:=dkArrow;
    fOptions :=[doKeepSize];
  end
  else if SameText(kindname,'dkCircle') then
  begin
    fDrawKind:=dkElipse;
    fOptions :=[doKeepAspectRatio];
  end
  else if SameText(kindname,'dkHorzLine') then
  begin
    fDrawKind:=dkLine;
    fOptions :=[doKeepSize];
  end
  else if SameText(kindname,'dkVertLine') then
  begin
    fDrawKind:=dkLine;
    fAngle   :=fAngle+90;
    fOptions :=[doKeepSize,doKeepVisible];
  end
  else if SameText(kindname,'dkReverseLine') then
  begin
    fDrawKind     :=dkCustom;
    fDrawData.Text:='1 0 0 1';
    fOptions      :=[doKeepSize];
  end;
end;

procedure TRLCustomDraw.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Kind',ReadKind,nil,False);
end;

procedure TRLCustomDraw.SetDrawHeight(const Value: integer);
begin
  if Value=fDrawHeight then
    Exit;
  fDrawHeight:=Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomDraw.SetDrawWidth(const Value: integer);
begin
  if Value=fDrawWidth then
    Exit;
  fDrawWidth:=Value;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomDraw.SetOptions(const Value: TRLDrawOptions);
begin
  if Value=fOptions then
    Exit;
  fOptions:=Value;
  AdjustBounds;
  Invalidate;
end;

function TRLCustomDraw.IsDrawSize: Boolean;
begin
  Result:=(fDrawWidth<>0) or (fDrawHeight<>0);
end;

{ TRLCustomSite }

constructor TRLCustomSite.Create(aOwner:TComponent);
begin
  {$IFDEF TRACECUSTOMSITE}
  Debugln('TRLCustomSite.Create');
  {$ENDIF}

  // initialization
  //fOnDraw          :=nil;
  // objects
  fBackground      :=TRLBackground.Create(Self);
  fDegrade         :=TRLDegradeEffect.Create(Self);
  fInsideMargins   :=TRLMargins.Create(Self);
  fMargins         :=TRLMargins.Create(Self);
  fSurface         :=TRLGraphicSurface.Create;
  //
  inherited Create(aOwner);
  // customization
  ControlStyle  :=ControlStyle+[csAcceptsControls,csCaptureMouse,csClickEvents,csOpaque,csDoubleClicks,csReplicatable];
end;

destructor TRLCustomSite.Destroy;
begin
  {$IFDEF TRACECUSTOMSITE}
  Debugln('TRLCustomSite.Destroy;');
  {$ENDIF}

  FreeObj(fSurface);
  FreeObj(fMargins);
  FreeObj(fInsideMargins);
  FreeObj(fDegrade);
  FreeObj(fBackground);
  //
  inherited;
end;

// anula alinhamento natural do delphi
procedure TRLCustomSite.AlignControls(aControl:TControl; var Rect:TRect);
begin
  {$IFDEF TRACECUSTOMSITE}
  Debugln('TRLCustomSite.AlignControls');
  {$ENDIF}
end;

// novo alinhamento de controles
procedure TRLCustomSite.AlignControls(aRect:TRect);
type
  TAlignControlArray=array[TRLControlAlign] of TList;
var
  alignarray:TAlignControlArray;
  align     :TRLControlAlign;
  control   :TControl;
  anchors   :TRLControlAnchors;
  alignrect :TRect;
  auxrect   :TRect;
  leftrect  :TRect;
  rightrect :TRect;
  l         :TList;
  i,j,w,h   :integer;
// retorna TRUE se os controles na ordem correta segundo o alinhamento e suas posições
function IsOrdered(aControl1,aControl2:TControl; aAlign:TRLControlAlign):boolean;
begin
  case aAlign of
    faTop         : Result:=(aControl1.Top<aControl2.Top);
    faBottom      : Result:=(aControl1.Top>aControl2.Top);
    faLeft        : Result:=(aControl1.Left<aControl2.Left);
    faLeftMost    : Result:=(aControl1.Left<aControl2.Left);
    faClient      : Result:=(aControl1.Left<aControl2.Left);
    faRight       : Result:=(aControl1.Left>aControl2.Left);
    faRightMost   : Result:=(aControl1.Left>aControl2.Left);
    faLeftTop     : Result:=(aControl1.Left<aControl2.Left) and (aControl1.Top<aControl2.Top);
    faRightTop    : Result:=(aControl1.Left>aControl2.Left) and (aControl1.Top<aControl2.Top);
    faLeftBottom  : Result:=(aControl1.Left<aControl2.Left) and (aControl1.Top>aControl2.Top);
    faRightBottom : Result:=(aControl1.Left>aControl2.Left) and (aControl1.Top>aControl2.Top);
    faCenter      : Result:=(aControl1.Left<aControl2.Left);
    faCenterLeft  : Result:=(aControl1.Top<aControl2.Top);
    faCenterTop   : Result:=(aControl1.Left<aControl2.Left);
    faCenterRight : Result:=(aControl1.Top<aControl2.Top);
    faCenterBottom: Result:=(aControl1.Left>aControl2.Left);
    faClientLeft  : Result:=(aControl1.Top<aControl2.Top);
    faClientTop   : Result:=(aControl1.Left<aControl2.Left);
    faClientRight : Result:=(aControl1.Top<aControl2.Top);
    faClientBottom: Result:=(aControl1.Left>aControl2.Left);
    faHeight      : Result:=(aControl1.Top<aControl2.Top);
    faWidth       : Result:=(aControl1.Left<aControl2.Left);
    faLeftOnly    : Result:=(aControl1.Left<aControl2.Left);
    faRightOnly   : Result:=(aControl1.Left>aControl2.Left);
    faTopOnly     : Result:=(aControl1.Top<aControl2.Top);
    faBottomOnly  : Result:=(aControl1.Top>aControl2.Top);
  else
    Result:=True;
  end
end;
// retorna nível de alinhamento (prioridade)
function AlignPriority(aControl:TControl):integer;
begin
  if IsStaticCustomControl(aControl) then
    Result:=0
  else if aControl is TRLCustomBand then
    case TRLCustomBand(aControl).BandType of
      btHeader      : Result:=10;
      btTitle       : Result:=20;
      btColumnHeader: Result:=30;
      btDetail      : Result:=40;
      btColumnFooter: Result:=50;
      btSummary     : Result:=60;
      btFooter      : Result:=70;
    else
      Result:=10;
    end
  else if aControl is TRLCustomSubDetail then
    case TRLCustomSubDetail(aControl).Positioning of
      btHeader      : Result:=11;
      btTitle       : Result:=21;
      btColumnHeader: Result:=31;
      btDetail      : Result:=41;
      btColumnFooter: Result:=51;
      btSummary     : Result:=61;
      btFooter      : Result:=71;

    else
      Result:=100;
    end
  else if aControl is TRLCustomGroup then
    Result:=32
  else
    Result:=100;
end;
// retorna ID do grupo de controles
function AlignGroup(aControl:TControl):integer;
begin
  if aControl is TRLCustomBand then
    Result:=TRLCustomBand(aControl).GroupIndex
  else
    Result:=0;
end;
// retorna TRUE se os controles na ordem correta segundo grupos, níveis e suas posições perante a um alinhamento
function IsPrior(aControl1,aControl2:TControl; aAlign:TRLControlAlign):boolean;
var
  prio1,prio2,group1,group2:integer;
  ctrl1,ctrl2:TControl;
begin
  ctrl1:=ControlWithin(aControl1);
  ctrl2:=ControlWithin(aControl2);
  prio1:=AlignPriority(ctrl1);
  prio2:=AlignPriority(ctrl2);
  if prio1=prio2 then
    if (ctrl1 is TRLCustomBand) and (ctrl2 is TRLCustomBand) then
    begin
      group1:=AlignGroup(ctrl1);
      group2:=AlignGroup(ctrl2);
      if group1=group2 then
        Result:=IsOrdered(aControl1,aControl2,aAlign)
      else
        Result:=(group1<group2);
    end
    else
      Result:=IsOrdered(aControl1,aControl2,aAlign)
  else
    Result:=(prio1<prio2);
end;
// adiciona controle numa lista na posição ideal para o alinhamento
procedure AddToList(aControl:TControl; var aArray:TAlignControlArray);
var
  a:TRLControlAlign;
  i:integer;
begin
  a:=GetControlAlignOf(aControl);
  if a=faNone then
    i:=aArray[a].Count
  else
  begin
    i:=0;
    while (i<=aArray[a].Count-1) and not IsPrior(aControl,TControl(aArray[a][i]),a) do
      Inc(i);
  end;
  if i=aArray[a].Count then
    aArray[a].Add(aControl)
  else
    aArray[a].Insert(i,aControl);
end;
procedure SetControlBoundsRect(aControl:TControl; aBoundsRect:TRect);
var
  ctrl:TControl;
begin
  ctrl:=ControlWithin(aControl);
  if ctrl<>aControl then
  begin
    OffsetRect(aBoundsRect,-aControl.Left,-aControl.Top);
    aControl:=ctrl;
  end;
  aControl.BoundsRect:=aBoundsRect;
end;
begin
  {$IFDEF TRACECUSTOMSITE}
  Debugln('TRLCustomSite.AlignControls');
  {$ENDIF}

  // limpa vetor de listas
  for align:=Low(TRLControlAlign) to High(TRLControlAlign) do
    alignarray[align]:=nil;
  try
    // criar listas de alinhamento
    for align:=Low(TRLControlAlign) to High(TRLControlAlign) do
      alignarray[align]:=TList.Create;
    // adiciona controles às listas de alinhamento
    for i:=0 to ControlCount-1 do
    begin
      control:=Controls[i];
      if not (csDesigning in ComponentState) and not control.Visible then
        Continue;
      AddToList(control,alignarray);
    end;

    // retângulo de alinhamento
    alignrect:=aRect;

    // alinhamentos de alta prioridade: leftmost, rightmost
    l:=alignarray[faLeftMost];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(alignrect.Left,alignrect.Top,alignrect.Left+control.Width,alignrect.Bottom));
      Inc(alignrect.Left,control.Width);
    end;
    l:=alignarray[faRightMost];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(alignrect.Right-control.Width,alignrect.Top,alignrect.Right,alignrect.Bottom));
      Dec(alignrect.Right,control.Width);
    end;

    // alinhamentos de média prioridade: top,bottom
    l:=alignarray[faTop];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(alignrect.Left,alignrect.Top,alignrect.Right,alignrect.Top+control.Height));
      Inc(alignrect.Top,control.Height);
    end;
    l:=alignarray[faBottom];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(alignrect.Left,alignrect.Bottom-control.Height,alignrect.Right,alignrect.Bottom));
      Dec(alignrect.Bottom,control.Height);
    end;

    // alinhamentos de baixa prioridade: left,right
    l:=alignarray[faLeft];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(alignrect.Left,alignrect.Top,alignrect.Left+control.Width,alignrect.Bottom));
      Inc(alignrect.Left,control.Width);
    end;
    l:=alignarray[faRight];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(alignrect.Right-control.Width,alignrect.Top,alignrect.Right,alignrect.Bottom));
      Dec(alignrect.Right,control.Width);
    end;

    // alinhamento pela sobra de espaço: client
    auxrect:=alignrect;
    l      :=alignarray[faClient];
    if l.Count>0 then
    begin
      w:=RectWidth(auxrect) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=RectWidth(auxrect);
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Top,auxrect.Left+w,auxrect.Bottom));
        Inc(auxrect.Left,control.Width);
      end;
    end;

    // outros alinhamentos que pegam a mesma sobra de espaço 
    leftrect :=alignrect;
    rightrect:=alignrect;

    // sobras ao topo
    auxrect:=alignrect;
    l      :=alignarray[faLeftTop];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Top,auxrect.Left+control.Width,auxrect.Top+control.Height));
      if control.BoundsRect.Bottom>leftrect.Top then
        leftrect.Top:=control.BoundsRect.Bottom;
      Inc(auxrect.Left,control.Width);
    end;
    l:=alignarray[faRightTop];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(auxrect.Right-control.Width,auxrect.Top,auxrect.Right,auxrect.Top+control.Height));
      if control.BoundsRect.Bottom>rightrect.Top then
        rightrect.Top:=control.BoundsRect.Bottom;
      Dec(auxrect.Right,control.Width);
    end;
    l:=alignarray[faClientTop];
    if l.Count>0 then
    begin
      h:=(auxrect.Right-auxrect.Left) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Right-auxrect.Left
        else
          w:=h;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Top,auxrect.Left+w,auxrect.Top+control.Height));
        Inc(auxrect.Left,control.Width);
      end;
    end;

    // sobras à base
    auxrect:=alignrect;
    l      :=alignarray[faLeftBottom];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Bottom-control.Height,auxrect.Left+control.Width,auxrect.Bottom));
      if control.BoundsRect.Top<leftrect.Bottom then
        leftrect.Bottom:=control.BoundsRect.Top;
      Inc(auxrect.Left,control.Width);
    end;
    l:=alignarray[faRightBottom];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(auxrect.Right-control.Width,auxrect.Bottom-control.Height,auxrect.Right,auxrect.Bottom));
      if control.BoundsRect.Top<rightrect.Bottom then
        rightrect.Bottom:=control.BoundsRect.Top;
      Dec(auxrect.Right,control.Width);
    end;
    l:=alignarray[faClientBottom];
    if l.Count>0 then
    begin
      h:=(auxrect.Right-auxrect.Left) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Right-auxrect.Left
        else
          w:=h;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Bottom-control.Height,auxrect.Left+w,auxrect.Bottom));
        Inc(auxrect.Left,control.Width);
      end;
    end;

    // sobras à esquerda
    auxrect:=leftrect;
    l      :=alignarray[faClientLeft];
    if l.Count>0 then
    begin
      h:=(auxrect.Bottom-auxrect.Top) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Bottom-auxrect.Top                                                                              
        else
          w:=h;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Top,auxrect.Left+control.Width,auxrect.Top+w));
        Inc(auxrect.Top,w);
      end;
    end;

    // sobras à direita
    auxrect:=rightrect;
    l      :=alignarray[faClientRight];
    if l.Count>0 then
    begin
      h:=(auxrect.Bottom-auxrect.Top) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Bottom-auxrect.Top
        else
          w:=h;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Right-control.Width,auxrect.Top,auxrect.Right,auxrect.Top+w));
        Inc(auxrect.Top,control.Height);
      end;
    end;

    // alinhamentos parciais
    auxrect:=alignrect;

    // somente à esquerda
    l:=alignarray[faLeftOnly];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(auxrect.Left,control.Top,auxrect.Left+control.Width,control.Top+control.Height));
    end;
    // somente à direita
    l:=alignarray[faRightOnly];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(auxrect.Right-control.Width,control.Top,auxrect.Right,control.Top+control.Height));
    end;
    // somente ao topo
    l:=alignarray[faTopOnly];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(control.Left,auxrect.Top,control.Left+control.Width,auxrect.Top+control.Height));
    end;
    // somente à base
    l:=alignarray[faBottomOnly];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(control.Left,auxrect.Bottom-control.Height,control.Left+control.Width,auxrect.Bottom));
    end;
    // somente à altura
    l:=alignarray[faHeight];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(control.Left,auxrect.Top,control.Left+control.Width,auxrect.Bottom));
    end;
    // somente à largura
    l:=alignarray[faWidth];
    for i:=0 to l.Count-1 do
    begin
      control:=TControl(l[i]);
      SetControlBoundsRect(control,Classes.Rect(auxrect.Left,control.Top,auxrect.Right,control.Top+control.Height));
    end;

    // alinhamentos aos centros
    
    // centro à esquerda
    auxrect:=alignrect;
    l      :=alignarray[faCenterLeft];
    if l.Count>0 then
    begin
      h:=(auxrect.Bottom-auxrect.Top) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Bottom-auxrect.Top
        else
          w:=h;
        j:=(w-control.Height) div 2;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Top+j,auxrect.Left+control.Width,auxrect.Top+j+control.Height));
        Inc(auxrect.Top,w);
      end;
    end;
    // centro ao topo
    auxrect:=alignrect;
    l      :=alignarray[faCenterTop];
    if l.Count>0 then
    begin
      h:=(auxrect.Right-auxrect.Left) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Right-auxrect.Left
        else
          w:=h;
        j:=(w-control.Width) div 2;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left+j,auxrect.Top,auxrect.Left+j+control.Width,auxrect.Top+control.Height));
        Inc(auxrect.Left,w);
      end;
    end;
    // centro à direita
    auxrect:=alignrect;
    l      :=alignarray[faCenterRight];
    if l.Count>0 then
    begin
      h:=(auxrect.Bottom-auxrect.Top) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Bottom-auxrect.Top
        else
          w:=h;
        j:=(w-control.Height) div 2;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Right-control.Width,auxrect.Top+j,auxrect.Right,auxrect.Top+j+control.Height));
        Inc(auxrect.Top,w);
      end;
    end;
    // centro à base
    auxrect:=alignrect;
    l      :=alignarray[faCenterBottom];
    if l.Count>0 then
    begin
      h:=(auxrect.Right-auxrect.Left) div l.Count;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        if i=l.Count-1 then
          w:=auxrect.Right-auxrect.Left
        else
          w:=h;
        j:=(w-control.Width) div 2;
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left+j,auxrect.Bottom-control.Height,auxrect.Left+j+control.Width,auxrect.Bottom));
        Inc(auxrect.Left,w);
      end;
    end;
    // centro
    auxrect:=alignrect;
    l      :=alignarray[faCenter];
    if l.Count>0 then
    begin
      h:=0;
      w:=0;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        Inc(h,control.Height);
        Inc(w,control.Width);
      end;
      auxrect.Top :=(auxrect.Top +auxrect.Bottom-h) div 2;
      auxrect.Left:=(auxrect.Left+auxrect.Right -w) div 2;
      for i:=0 to l.Count-1 do
      begin
        control:=TControl(l[i]);
        SetControlBoundsRect(control,Classes.Rect(auxrect.Left,auxrect.Top,auxrect.Left+control.Width,auxrect.Top+control.Height));
        Inc(auxrect.Left,control.Width);
      end;
    end;

    // ajusta controles ancorados
    w:=RectWidth(OldBoundsRect);
    h:=RectHeight(OldBoundsRect);
    l:=alignarray[faNone];
    for i:=0 to l.Count-1 do
    begin
      control  :=TControl(l[i]);
      alignrect:=control.BoundsRect;
      anchors  :=GetControlAnchorsOf(control);
      if fkRight in anchors then
        if fkLeft in anchors then
          Inc(alignrect.Right,Width-w)
        else
          OffsetRect(alignrect,Width-w,0)
      else if (fkLeft in anchors) and (anchors*[fkTop,fkBottom]=[]) then
        OffsetRect(alignrect,0,Round(alignrect.Top*Height/h));
      if fkBottom in anchors then
        if fkTop in anchors then
          Inc(alignrect.Bottom,Height-h)
        else
          OffsetRect(alignrect,0,Height-h)
      else if (fkTop in anchors) and (anchors*[fkLeft,fkRight]=[]) then
        OffsetRect(alignrect,Round(alignrect.Left*Width/w),0);
      SetControlBoundsRect(control,alignrect);
    end;

  finally
    for align:=Low(TRLControlAlign) to High(TRLControlAlign) do
      FreeObj(alignarray[align]);
  end;
end;

// alinha os controles do panel e dos panels parentizados
procedure TRLCustomSite.RealignControls;
var
  i:integer;
  c:TControl;
begin
  {$IFDEF TRACECUSTOMSITE}
  DebugLn('TRLCustomSite.RealignControls;');
  {$ENDIF}
  if csLoading in ComponentState then
    Exit;
  if stAligningControls in fControlState then
    Exit;
  Include(fControlState,stAligningControls);
  try
    AlignControls(ClientRect);
    //
    for i:=0 to ControlCount-1 do
    begin
      c:=ControlWithin(Controls[i]);
      if c is TRLCustomSite then
        TRLCustomSite(c).RealignControls;
    end;
  finally
    Exclude(fControlState,stAligningControls);
  end;
end;

procedure TRLCustomSite.DrawClient;
begin
  {$IFDEF TRACECUSTOMSITE}
  DebugLn('TRLCustomSite.DrawClient;');
  {$ENDIF}

  DrawFrame(GetClientRect,clGray,True);
end;

// desenha frames delimitadores
procedure TRLCustomSite.DrawBounds;
begin
  {$IFDEF TRACECUSTOMSITE}
  Debugln('TRLCustomSite.DrawBounds;');
  {$ENDIF}
  DrawFrame(CalcSizeRect,clBlue,False);
end;

// desenha uma frame colorida e com cantos arredondados 
procedure TRLCustomSite.DrawFrame(Rect:TRect; aColor:TColor; aRound:boolean);
var
  curv:integer;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.DrawFrame;');
  {$ENDIF}

  with Canvas do
  begin
    Pen.Color  :=aColor;
    Pen.Style  :=psDot;
    Pen.Mode   :=pmCopy;
    Brush.Style:=bsClear;
    if aRound then
    begin
{$ifdef VCL}
      curv:=6;
{$else}
      curv:=2;
{$endif}
      RoundRect(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom,curv,curv);
    end
    else
      Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
  end;
end;

// desenha regua 
procedure TRLCustomSite.DrawTracks;
const
  clCm  =$00DFDFDF;
  clHalf=$00F1F1F1;
var
  x,y:integer;
  cm,f:double;
  bCm,num:boolean;
  r:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.DrawTracks');
  {$ENDIF}

  num:=True; //Self is TRLCustomReport;
  r  :=CalcSizeRect;
  f  :=ScreenPPI/(InchAsMM/10);
  with Canvas do
  begin
    if num then
    begin
      Font.Name :='Small Fonts';
      Font.Size :=6;
      Font.Style:=[];
      Font.Color:=clBlack;
    end;
    Pen.Color  :=clAqua;
    Pen.Mode   :=pmCopy;
    Brush.Style:=bsClear;
    bCm:=False;
    cm :=1/2;
    repeat
      y:=r.Top+Round(cm*f);
      if y>r.Bottom then
        Break;
      if bCm then
      begin
        if num then
          TextOut(r.Left+1,y+1,IntToStr(Round(cm)));
        Pen.Style:=psSolid;
      end
      else
        Pen.Style:=psDot;
      MoveTo(r.Left,y); LineTo(r.Right,y);
      cm :=cm+1/2;
      bCm:=not bCm;
    until False;
    bCm:=False;
    cm :=1/2;
    repeat
      x:=r.Left+Round(cm*f);
      if x>r.Right then
        Break;
      if bCm then
      begin
        if num then
          TextOut(x+1,r.Top+1,IntToStr(Round(cm)));
        Pen.Style:=psSolid;
      end
      else
        Pen.Style:=psDot;
      MoveTo(x,r.Top); LineTo(x,r.Bottom);
      cm :=cm+1/2;
      bCm:=not bCm;
    until False;
  end;
end;

// preenche regiao nao utilizada com barras 
procedure TRLCustomSite.DrawUnusedRect(Rect:TRect);
const
  clDarkness=$00F4F4F4;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.DrawUnusedRect');
  {$ENDIF}

  with Canvas do
  begin
    Pen.Color  :=clDarkness;
    Pen.Style  :=psSolid;
    Pen.Mode   :=pmCopy;
    Brush.Color:=clDarkness;
    Brush.Style:=bsSolid;
    Rectangle(Rect.Left,Rect.Top,Rect.Right,Rect.Bottom);
  end;
end;

// zera contadores 
procedure InitializeAllFrom(aParent:TWinControl);
var
  i:integer;
begin
  for i:=0 to aParent.ControlCount-1 do
    if aParent.Controls[i] is TRLCustomControl then
      TRLCustomControl(aParent.Controls[i]).Initialize
    else if aParent.Controls[i] is TCustomFrame then
      InitializeAllFrom(TCustomFrame(aParent.Controls[i]));
end;

procedure TRLCustomSite.Initialize;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.Initialize');
  {$ENDIF}
  InitializeAllFrom(Self);
end;

// incrementa contadores 
procedure ComputeDetailAllFrom(aParent:TWinControl; aCaller:TObject);
var
  i:integer;
begin
  for i:=0 to aParent.ControlCount-1 do
    if aParent.Controls[i]<>aCaller then
      if IsStaticCustomControl(aParent.Controls[i]) or (aParent.Controls[i] is TRLCustomBand) then
        TRLCustomControl(aParent.Controls[i]).ComputeDetail(aCaller)
      else if aParent.Controls[i] is TCustomFrame then
        ComputeDetailAllFrom(TCustomFrame(aParent.Controls[i]),aCaller);
end;

procedure TRLCustomSite.ComputeDetail(aCaller:TObject);
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.ComputeDetail');
  {$ENDIF}
  ComputeDetailAllFrom(Self,aCaller);
end;

// forca o redesenho do panel e dos panels filhos
procedure InvalidateAllFrom(aParent:TWinControl);
var
  i:integer;
begin
  aParent.Invalidate;
  for i:=0 to aParent.ControlCount-1 do
    if aParent.Controls[i] is TRLCustomSite then
      TRLCustomSite(aParent.Controls[i]).InvalidateAll
    else if aParent.Controls[i] is TRLCustomControl then
      TRLCustomControl(aParent.Controls[i]).Invalidate
    else if aParent.Controls[i] is TCustomFrame then
      InvalidateAllFrom(TCustomFrame(aParent.Controls[i]));
end;

procedure TRLCustomSite.InvalidateAll;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.InvalidateAll');
  {$ENDIF}
  InvalidateAllFrom(Self);
end;

// invoca evento durante a impressão 
procedure TRLCustomSite.DoOnDraw(aSurface:TRLGraphicSurface; aRect:TRect);
var
  r:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.DoOnDraw');
  {$ENDIF}
  if Assigned(fOnDraw) then
  begin
    r:=GetClientRect;
    OffsetRect(r,aRect.Left,aRect.Top);
    fOnDraw(Self,aSurface,r);
  end;
end;

procedure TRLCustomSite.OpenSurface;
var
  s:TRLCustomSite;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.OpenSurface');
  {$ENDIF}
  if Surface.Opened then
    Exit;
  // por precaução, abre o canvas do controle pai antes
  s:=FindParentSite;
  if (s<>nil) and not s.Surface.Opened then
    s.OpenSurface;
  //
  SurfaceOpening;
  MarkPrintPosition;
  Surface.Open;
  Surface.Clear;
  Surface.Margins:=ClientRect;
  if Enabled then
    SurfaceBeginDraw;
  SurfaceOpened;
end;

procedure TRLCustomSite.CloseSurface;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CloseSurface');
  {$ENDIF}
  if not Surface.Opened then
    Exit;
  //
  if Enabled then
    SurfaceEndDraw;
  TruncateSurface;
  ThrowSurface;
  Surface.Close;
  SurfaceClosed;
end;

procedure TRLCustomSite.ThrowSurface;
var
  destsurface:TRLGraphicSurface;
  destrect   :TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.ThrowSurface');
  {$ENDIF}
  destsurface:=RequestParentSurface;
  destrect:=CalcPrintBoundsRect;
  //
  if Enabled then
    PrepareBackgroundSurface(destsurface,destrect);
  destsurface.Draw(destrect.Left,destrect.Top,Surface);
end;

procedure TRLCustomSite.PrepareBackgroundSurface(aBackgroundSurface:TRLGraphicSurface; const aRect:TRect);
var
  m:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.PrepareBackgroundSurface');
  {$ENDIF}
  aBackgroundSurface.GeneratorId:=Integer(Self);
  NewGroupId;
  if (Degrade.Direction<>ddNone) and (Degrade.OppositeColor<>Color) then
    Degrade.PaintTo(aBackgroundSurface,aRect,Color)
  else if not IsTransparent(Self) then
  begin
    aBackgroundSurface.Brush.Color:=Color;
    aBackgroundSurface.Brush.Style:=bsSolid;
    aBackgroundSurface.FillRect(aRect);
  end;
  Background.PaintTo(aBackgroundSurface,aRect);
  m:=CalcPrintMarginalRect;
  OffsetRect(m,aRect.Left,aRect.Top);
  DoOnDraw(aBackgroundSurface,m);
  Borders.PaintTo(aBackgroundSurface,m);
end;

procedure TRLCustomSite.SurfaceOpening;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.SurfaceOpening');
  {$ENDIF}
end;

procedure TRLCustomSite.SurfaceBeginDraw;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.SurfaceBeginDraw');
  {$ENDIF}
  PrepareStatics;
  PrintStatics;
end;

procedure TRLCustomSite.SurfaceOpened;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.SurfaceOpened');
  {$ENDIF}
end;

procedure TRLCustomSite.WriteSurface;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.WriteSurface');
  {$ENDIF}
end;

procedure TRLCustomSite.SurfaceEndDraw;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.SurfaceEndDraw');
  {$ENDIF}
end;

procedure TRLCustomSite.SurfaceClosed;
begin
  {$IFDEF TRACECUSTOMSITE}
   debugln('chamou TRLCustomSite.SurfaceClosed');
  {$ENDIF}
end;

procedure TRLCustomSite.TruncateSurface;
begin
  {$IFDEF TRACECUSTOMSITE}
   debugln('chamou TRLCustomSite.TruncateSurface;');
  {$ENDIF}
end;

procedure TRLCustomSite.MarkPrintPosition;
var
  p:TWinControl;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.MarkPrintPosition');
  {$ENDIF}
  fPrintPosition.x:=Left;
  fPrintPosition.y:=Top;
  fPrintSize.x    :=Width;
  fPrintSize.y    :=Height;
  //
  p:=Parent;
  while (p<>nil) and not (p is TRLCustomSite) do
  begin
    Inc(fPrintPosition.x,p.Left);
    Inc(fPrintPosition.y,p.Top);
    p:=p.Parent;
  end;
end;

procedure TRLCustomSite.DrawBackground(const aRect:TRect);
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.DrawBackground');
  {$ENDIF}
  Background.PaintTo(Canvas,aRect);
end;

function TRLCustomSite.CalcEffectiveRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CalcEffectiveRect');
  {$ENDIF}
  Result:=CalcSizeRect;
end;

procedure TRLCustomSite.Signup(const aSignature:string);
var
  w,h:integer;
  t:TRect;
  s:string;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.Signup');
  {$ENDIF}
  with Canvas do
  begin
    Font.Name  :='Small Fonts';
    Font.Size  :=6;
    Font.Style :=[];
    s:=' '+aSignature+' ';
    w:=TextWidth(s);
    h:=TextHeight(s);
    t.Left     :=1;
    t.Top      :=1;
    t.Right    :=t.Left+w;
    t.Bottom   :=t.Top+h;
    Brush.Color:=RGB(220,220,255);
    Brush.Style:=bsSolid;
    FillRect(t);
    Brush.Style:=bsClear;
    Font.Color :=clWhite;
    TextRect(t,t.Left+1,t.Top+1,s);
    Font.Color :=clBlue;
    TextRect(t,t.Left,t.Top,s);
  end;
end;

procedure TRLCustomSite.PaintAsCustomSite;
var
  z,s,e,r:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.PaintAsCustomSite');
  {$ENDIF}
  z:=CalcSizeRect;
  s:=z;
  e:=CalcEffectiveRect;
  // pinta fundo
  with Canvas do
  begin
    Brush.Color:=Self.Color;
    Brush.Style:=bsSolid;
    FillRect(z);
  end;
  // preenche espaços não client
  if e.Top>s.Top then
  begin
    r       :=s;
    r.Bottom:=e.Top;
    DrawUnusedRect(r);
    s.Top   :=e.Top;
  end;
  if e.Bottom<s.Bottom then
  begin
    r       :=s;
    r.Top   :=e.Bottom;
    DrawUnusedRect(r);
    s.Bottom:=e.Bottom;
  end;
  if e.Left>s.Left then
  begin
    r      :=s;
    r.Right:=e.Left;
    DrawUnusedRect(r);
    s.Left :=e.Left;
  end;
  if e.Right<s.Right then
  begin
    r      :=s;
    r.Left :=e.Right;
    DrawUnusedRect(r);
    s.Right:=e.Right;
  end;
  //
  if (Degrade.Direction<>ddNone) and (Degrade.OppositeColor<>Color) then
    Degrade.PaintTo(Canvas,e,Color);
  DrawBackground(e);
  Borders.PaintTo(Canvas,CalcMarginalRect);
end;

// desenha o panel em tela
procedure TRLCustomSite.InternalPaint;
begin
  {$IFDEF TRACECUSTOMSITE}
  writeln('chamou TRLCustomSite.InternalPaint');
    {$ENDIF}
  PaintAsCustomSite;
end;

procedure TRLCustomSite.InternalPaintFinish;
var
  p:TRLCustomReport;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.InternalPaintFinish');
  {$ENDIF}
  p:=FindParentReport;
  if not Assigned(p) or (p.ShowDesigners and p.ShowTracks) then
    DrawTracks;
  if not Assigned(p) or p.ShowDesigners then
  begin
    DrawClient;
    DrawBounds;
  end;
end;

procedure TRLCustomSite.InternalPrint;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.InternalPrint');
  {$ENDIF}
  OpenSurface;
  WriteSurface;
  CloseSurface;
end;

procedure TRLCustomSite.CalcSize(var aSize:TPoint);
var
  i,totalwidth,totalheight,maxright,maxbottom:integer;
  control  :TControl;
  ctrlalign:TRLControlAlign;
  clirect  :TRect;
  ctrlsize :TPoint;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CalcSize');
  {$ENDIF}
  aSize:=Point(Width,Height);
  if not AutoSize then
    Exit;
  // totaliza tamanho dos controles
  totalwidth :=0;
  totalheight:=0;
  maxright   :=0;
  maxbottom  :=0;
  for i:=0 to ControlCount-1 do
  begin
    control  :=Controls[i];
    ctrlalign:=GetControlAlignOf(control);
    ctrlsize :=Point(control.Width,control.Height);
    if ctrlalign=faNone then
    begin
      maxright :=Max(maxright ,control.Left+ctrlsize.X);
      maxbottom:=Max(maxbottom,control.Top+ctrlsize.Y);
    end
    else
    begin
      if ctrlalign in faFreeWidthSet then
        Inc(totalwidth,ctrlsize.X);
      if ctrlalign in faFreeHeightSet then
        Inc(totalheight,ctrlsize.Y);
    end;    
  end;
  //
  clirect:=GetClientRect;
  Dec(maxright,clirect.Left);
  Dec(maxbottom,clirect.Top);
  totalwidth :=Max(totalwidth ,maxright);
  totalheight:=Max(totalheight,maxbottom);
  //
  if (Align in faSlaveWidthSet) or (totalwidth=0) then
    totalwidth:=RectWidth(clirect);
  if (Align in faSlaveHeightSet) or (totalheight=0) then
    totalheight:=RectHeight(clirect);
  // incremento das bordas, margens e etc.
  aSize.X:=(Width-RectWidth(clirect))+totalwidth;
  aSize.Y:=(Height-RectHeight(clirect))+totalheight;
end;

// margens externas em pixels
function TRLCustomSite.CalcMarginalPixels:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CalcMarginalPixels');
  {$ENDIF}
  Result.Left  :=Round(ScreenPPI*fMargins.LeftMargin  /InchAsMM);
  Result.Top   :=Round(ScreenPPI*fMargins.TopMargin   /InchAsMM);
  Result.Right :=Round(ScreenPPI*fMargins.RightMargin /InchAsMM);
  Result.Bottom:=Round(ScreenPPI*fMargins.BottomMargin/InchAsMM);
end;

// retangulo interno as margens
function TRLCustomSite.CalcMarginalRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CalcMarginalRect');
  {$ENDIF}
  Result:=ReduceRect(CalcEffectiveRect,CalcMarginalPixels);
end;

function TRLCustomSite.CalcBordersPixels:TRect;
var
  w,h:integer;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CalcBordersPixels');
  {$ENDIF}
  Result:=Rect(0,0,0,0);
  if fBorders.Width>0 then
  begin
    w:=fBorders.Width;
    h:=fBorders.Width;
    if fBorders.CanDrawLeft then
      Inc(Result.Left,w);
    if fBorders.CanDrawTop then
      Inc(Result.Top,h);
    if fBorders.CanDrawRight then
      Inc(Result.Right,w);
    if fBorders.CanDrawBottom then
      Inc(Result.Bottom,h);
  end;
end;

// retangulo interno as bordas 
function TRLCustomSite.CalcBordersRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CalcBordersRect');
  {$ENDIF}
  Result:=ReduceRect(CalcMarginalRect,CalcBordersPixels);
end;

function TRLCustomSite.CalcClientPixels:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.CalcClientPixels');
  {$ENDIF}
  Result.Left  :=Round(ScreenPPI*fInsideMargins.LeftMargin  /InchAsMM);
  Result.Top   :=Round(ScreenPPI*fInsideMargins.TopMargin   /InchAsMM);
  Result.Right :=Round(ScreenPPI*fInsideMargins.RightMargin /InchAsMM);
  Result.Bottom:=Round(ScreenPPI*fInsideMargins.BottomMargin/InchAsMM);
end;

// retangulo livre de bordas e margens para desenho interno ou posicionamento de controls
function TRLCustomSite.GetClientRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.GetClientRect');
  {$ENDIF}
  Result:=ReduceRect(CalcBordersRect,CalcClientPixels);
end;

function TRLCustomSite.CalcPrintBoundsRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintBoundsRect');
  {$ENDIF}

  Result:=Rect(fPrintPosition.X,fPrintPosition.Y,fPrintPosition.X+fPrintSize.X,fPrintPosition.Y+fPrintSize.Y);
end;

function TRLCustomSite.CalcPrintSizeRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintSizeRect');
  {$ENDIF}

  Result:=CalcPrintBoundsRect;
  MoveRect(Result,0,0);
end;

function TRLCustomSite.CalcPrintWastedPixels:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintWastedPixels');
  {$ENDIF}

  Result:=DiffRect(CalcPrintSizeRect,CalcPrintClientRect);
end;

function TRLCustomSite.CalcPrintWastedPixelsSum:TRect;
var
  p:TRLCustomPager;
  w:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintWastedPixelsSum');
  {$ENDIF}

  Result:=CalcPrintWastedPixels;
  p:=FindParentPager;
  if p<>nil then
  begin
    w:=p.CalcPrintWastedPixelsSum;
    Inc(Result.Left  ,w.Left);
    Inc(Result.Top   ,w.Top);
    Inc(Result.Right ,w.Right);
    Inc(Result.Bottom,w.Bottom);
  end;
end;

// espacos perdidos em pixels de tela 
function TRLCustomSite.CalcWastedPixels:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcWastedPixels');
  {$ENDIF}

  Result:=DiffRect(CalcSizeRect,GetClientRect);
end;

function TRLCustomSite.CanPrint:boolean;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CanPrint');
  {$ENDIF}

  fCouldPrint:=Visible and not (stPrinting in fControlState);
  if fCouldPrint then
    DoBeforePrint;
  Result:=fCouldPrint;
end;

function TRLCustomSite.CalcPrintMarginalPixels:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintMarginalPixels');
  {$ENDIF}

  Result:=CalcMarginalPixels;
end;

function TRLCustomSite.CalcPrintMarginalRect:TRect;
var
  m:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintMarginalRect');
  {$ENDIF}

  Result:=CalcPrintSizeRect;
  m     :=CalcPrintMarginalPixels;
  Inc(Result.Left  ,m.Left  );
  Inc(Result.Top   ,m.Top   );
  Dec(Result.Right ,m.Right );
  Dec(Result.Bottom,m.Bottom);
end;

function TRLCustomSite.CalcPrintBordersPixels:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintBordersPixels');
  {$ENDIF}

  Result:=CalcBordersPixels;
end;

function TRLCustomSite.CalcPrintBordersRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintBordersRect');
  {$ENDIF}

  Result:=ReduceRect(CalcPrintMarginalRect,CalcPrintBordersPixels);
end;

function TRLCustomSite.CalcPrintClientPixels:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintClientPixels');
  {$ENDIF}

  Result:=CalcClientPixels;
end;

function TRLCustomSite.CalcPrintClientRect:TRect;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcPrintClientRect');
  {$ENDIF}

  Result:=ReduceRect(CalcPrintBordersRect,CalcPrintClientPixels);
end;

function TRLCustomSite.CalcGlobalPrintPosition:TPoint;
var
  p:TRLCustomSite;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.CalcGlobalPrintPosition');
  {$ENDIF}

  Result:=fPrintPosition;
  p:=FindParentSite;
  if p<>nil then
    with p.CalcGlobalPrintPosition do
    begin
      Inc(Result.x,x);
      Inc(Result.y,y);
    end;
end;

procedure TRLCustomSite.SetClientRect(const aValue:TRect);
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.SetClientRect');
  {$ENDIF}

  BoundsRect:=IncreaseRect(aValue,CalcWastedPixels);
end;

procedure TRLCustomSite.SetBounds(aLeft,aTop,aWidth,aHeight:integer);
begin
  inherited SetBounds(aLeft,aTop,aWidth,aHeight);
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.SetBounds');
  {$ENDIF}
  //
  fPrintSize.X:=Width;
  fPrintSize.Y:=Height;
end;

procedure TRLCustomSite.SetBackground(const aValue:TRLBackground);
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.SetBackground');
  {$ENDIF}
  fBackground:=aValue;
  fBackground.ParentSite:=Self;
  Invalidate;
end;

procedure TRLCustomSite.SetDegrade(const aValue:TRLDegradeEffect);
begin
 {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSite.SetDegrade');
 {$ENDIF}

  fDegrade:=aValue;
  Invalidate;
end;

procedure TRLCustomSite.SetInsideMargins(const aValue:TRLMargins);
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.SetInsideMargins');
  {$ENDIF}
  fInsideMargins.Assign(aValue);
  Invalidate;
end;

procedure TRLCustomSite.SetMargins(const aValue:TRLMargins);
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.SetMargins');
  {$ENDIF}
  fMargins.Assign(aValue);
  Invalidate;
end;

procedure TRLCustomSite.Loaded;
begin
  inherited;
  //
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.Loaded');
  {$ENDIF}
  AdjustBounds;
  AlignControls(ClientRect);
end;

procedure TRLCustomSite.InternalMeasureHeight;
var
  c:TControl;
  i:integer;
begin
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomSite.InternalMeasureHeight');
  {$ENDIF}
  for i:=0 to ControlCount-1 do
  begin
    c:=ControlWithin(Controls[i]);
    if c is TRLCustomControl then
      TRLCustomControl(c).MeasureHeight;
  end;
  //
  inherited;
end;

{ TRLCustomPanel }

constructor TRLCustomPanel.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  // customization
  {$IFDEF TRACECUSTOMSITE}
  debugln('chamou TRLCustomPanel.Create');
  {$ENDIF}
  Width      :=64;
  Height     :=32;
  AutoSizeDir:=[asWidthDir,asHeightDir];
  ReleaseCapture;

end;

procedure TRLCustomPanel.DrawBounds;
var
  r:TRect;
begin
  r:=CalcSizeRect;
  with Canvas do
  begin
    Pen.Color:=clBlack;
    Pen.Style:=psSolid;
    Pen.Mode :=pmCopy;
    MoveTo(r.Left   ,r.Top+5   ); LineTo(r.Left   ,r.Top     ); LineTo(r.Left +5,r.Top     );
    MoveTo(r.Right-5,r.Top     ); LineTo(r.Right-1,r.Top     ); LineTo(r.Right-1,r.Top+5   );
    MoveTo(r.Right-1,r.Bottom-5); LineTo(r.Right-1,r.Bottom-1); LineTo(r.Right-5,r.Bottom-1);
    MoveTo(r.Left +5,r.Bottom-1); LineTo(r.Left   ,r.Bottom-1); LineTo(r.Left   ,r.Bottom-5);
  end;
end;

{ TRLCustomBandSet }

constructor TRLCustomBandSet.Create(aOwner: TComponent);
begin
  fBandSets:=nil;
  //
  fBandSets:=TList.Create;
  //
  inherited;
end;

destructor TRLCustomBandSet.Destroy;
begin
  FreeObj(fBandSets);
  //
  inherited;
end;

procedure TRLCustomBandSet.SurfaceOpened;
var
  p:TRLCustomBandSet;
begin
  inherited;
  //
  fBandSets.Clear;
  p:=FindParentBandSet;
  if Assigned(p) then
    p.AddBandSet(Self);
end;

procedure TRLCustomBandSet.SurfaceClosed;
begin
  inherited;
  //
  fBandSets.Clear;
end;

procedure TRLCustomBandSet.SurfaceBeginDraw;
begin
  inherited;
  //
  if (Self is TRLBand) or (Self is TRLDetailGrid) then
    PrintNonStatics;
end;

function TRLCustomBandSet.FindParentBandSet:TRLCustomBandSet;
var
  w:TControl;
begin
  w:=Parent;
  while (w<>nil) and not (w is TRLCustomBandSet) do
    w:=w.Parent;
  Result:=TRLCustomBandSet(w);
end;

function TRLCustomBandSet.CountBandSet(aBandSet: TRLCustomBandSet): integer;
var
  i:integer;
begin
  Result:=0;
  for i:=0 to fBandSets.Count-1 do
    if fBandSets[i]=aBandSet then
      Inc(Result);
end;

procedure TRLCustomBandSet.AddBandSet(aBandSet: TRLCustomBandSet);
begin
  fBandSets.Add(aBandSet);
end;

function TRLCustomBandSet.BandSetCount: integer;
var
  p:TRLCustomBandSet;
begin
  p:=FindParentBandSet;
  if Assigned(p) then
    Result:=p.CountBandSet(Self)
  else
    Result:=0;
end;

function TRLCustomBandSet.IsFirstBandSet: boolean;
begin
  Result:=(BandSetCount=0);
  {mudança aqui = 1}
end;

{ TRLCustomBand }

constructor TRLCustomBand.Create(aOwner:TComponent);
begin
  // initialization
  fBandType      :=btDetail;
  fComputable    :=True;
  fPageBreaking  :=pbNone;
  fCompletion    :=ctNone;
  fAlignToBottom :=False;
  fCarbonCopies  :=1;
  fCarbonIndex   :=0;
  fGroupIndex    :=0;
  fIntegralHeight:=True;
  // objects
  inherited Create(aOwner);
  // customization
  fAlign     :=faTop;
  fAutoExpand:=True;
  AutoSizeDir:=[asHeightDir];
  //
  Height:=16;
  Width :=185;
end;

function TRLCustomBand.HeightFits(aHeight:integer; var aAvailable:integer):boolean;
var
  pager:TRLCustomPager;
  footr:integer;
  pgrow:integer;
begin
  pager     :=RequestParentPager;
  // excedeu a última linha para bands de dados?
  footr     :=pager.GetRelativeFooterRow;
  pgrow     :=pager.RelativePagerRow;
  aAvailable:=footr-pgrow; /// consider columnfooterrow?
  Result    :=(aHeight<=aAvailable);
end;

procedure TRLCustomBand.ThrowSurface;
var
  destsurface:TRLGraphicSurface;
  destrect   :TRect;
  srcrect    :TRect;
  fullrect   :TRect;
  report     :TRLCustomReport;
  pager      :TRLCustomPager;
  vertspace  :integer;
  totalcut   :integer;
  cutheight  :integer;
  cutwidth   :integer;
  freerow    :integer;
  function CounterExceeds:boolean;
  begin
    Result:=(pager.MaxBands>0) and (pager.DetailsInSurface+1>pager.MaxBands) and not Completing;
  end;
begin
  report     :=RequestParentReport;
  pager      :=RequestParentPager;
  destsurface:=RequestParentSurface;
  // checa se é preciso saltar a página antes de imprimir esta band
  if (IsDataBand and report.NewPageNeeded) or                     // se o último controle impresso recomendou que o salto fosse feito na próxima band de dados
     ((PageBreaking=pbBeforePrint) and report.DataBandPrinted) or // se a quebra deve ser feita antes desta band e já foi impresso algum detalhe
     ((BandType=btDetail) and CounterExceeds) then                // se esta band excede o máximo previsto
    pager.InternalNewPage(Self,not pager.IsSatisfied);
  // bands alinhadas ao rodapé da página (footers são sempre alinhados)            
  if AlignToBottom or (BandType in [btFooter]) then
    case BandType of
      btFooter      : pager.GoFooterRow;
      btSummary     : if not pager.GoSummaryRow then
                      begin
                        pager.InternalNewPage(Self,not pager.IsSatisfied);
                        pager.GoSummaryRow;
                      end;
      btColumnFooter: if not pager.GoColumnFooterRow then
                      begin
                        pager.InternalNewPage(Self,not pager.IsSatisfied);
                        pager.GoColumnFooterRow;
                      end;
    end;
  MarkPrintPosition;
  //
  totalcut :=0;
  cutheight:=fPrintSize.Y;
  cutwidth :=fPrintSize.X;
  while totalcut<fPrintSize.Y do
  begin
    cutheight:=fPrintSize.Y-totalcut;
    // se a band tem obrigatoriamente que ser impressa nesta página...
    if BandType in [btFooter,btColumnFooter] then
    // se a band (ou pedaço) couber na página...
    else if HeightFits(cutheight,vertspace) then
    // se não puder dividir a band ou o pedaço que couber for menor que o tamanho mínimo...
    else if IntegralHeight or (vertspace<Constraints.MinHeight) then
      VerticalExceeded
    else if not IntegralHeight then
      if Surface.FindFreeRow(totalcut+vertspace,freerow) and (freerow>=Constraints.MinHeight) and (freerow>totalcut) then
        cutheight:=freerow-totalcut
      else
        VerticalExceeded
    else
      cutheight:=vertspace;
    // tamanho da band descontando o pedaço já impresso
    srcrect :=Rect(0,totalcut,fPrintSize.X,totalcut+cutheight);
    destrect:=srcrect;
    MoveRect(destrect,fPrintPosition.X,fPrintPosition.Y);
    //
    if Enabled then
    begin
      destsurface.SetClipRect(destrect);
      try
        fullrect:=Rect(fPrintPosition.X,fPrintPosition.Y-totalcut,fPrintPosition.X+fPrintSize.X,fPrintPosition.Y-totalcut+fPrintSize.Y);
        PrepareBackgroundSurface(destsurface,fullrect);
      finally
        destsurface.ResetClipRect;
      end;
    end;
    destsurface.CopyRect(destrect,Surface,srcrect);
    //
    Inc(totalcut,RectHeight(srcrect));
    if totalcut<fPrintSize.Y then
      VerticalExceeded;
  end;
  //
  SkipToNextPosition(cutwidth,cutheight);
end;

procedure TRLCustomBand.VerticalExceeded;
begin
  // move para a próxima página
  RequestParentPager.InternalNewPage(Self,False);
  MarkPrintPosition;
end;

procedure TRLCustomBand.SkipToNextPosition(aWidth,aHeight:integer);
begin
  with RequestParentPager do
    RelativePagerRow:=RelativePagerRow+aHeight;
end;

procedure TRLCustomBand.CheckPageBreak;
var
  vertspace:integer;
begin
  // se a band tem obrigatoriamente que ser impressa nesta página...
  if BandType in [btFooter,btColumnFooter] then
  // se a band couber na página...
  else if HeightFits(fPrintSize.Y,vertspace) then
  // se não puder dividir a band ou o pedaço que couber for menor que o tamanho mínimo...
  else if IntegralHeight or (vertspace<Constraints.MinHeight) then
    VerticalExceeded;
end;

procedure TRLCustomBand.SurfaceClosed;
begin
  inherited;
  //
  if (BandType=btDetail) and Computable then
    with RequestParentPager do
      DetailsInSurface:=DetailsInSurface+1;
  if PageBreaking=pbAfterPrint then
    RequestParentReport.InvalidatePage;
end;

function TRLCustomBand.GetBandTypeName:string;
begin
  Result:=BandTypeNames[fBandType];
end;

procedure TRLCustomBand.InternalPaintFinish;
var
  p:TRLCustomReport;
begin
  inherited;
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomBand.InternalPaintFinish;');
  {$ENDIF}

  //
  p:=FindParentReport;
  if not Assigned(p) or p.ShowDesigners then
    Signup(GetBandTypeName+' '+Name);
end;

procedure TRLCustomBand.NotifyDataBandPrinted;
var
  p:TRLCustomPager;
begin
  p:=FindParentPager;
  while p<>nil do
  begin
    p.DataBandPrinted:=True;
    p:=p.FindParentPager;
  end;
end;

function TRLCustomBand.IsDataBand:boolean;
begin
  Result:=(BandType in [btDetail,btSummary]) and not IsBallast;
end;

procedure TRLCustomBand.InternalPrint;
var
  compute:boolean;
  dataprt:boolean;
begin
//  RequestParentPager.PrintHeaders;
  /// revisado no band
  // se for detail computável deve computar o registro
  compute:=(BandType=btDetail) and Computable and not IsBallast;
  // se for band de dados, deve setar o flag de dados impressos
  dataprt:=compute or (BandType=btSummary);
  // computa o registro nos controles da própria band
  if compute then
    Self.ComputeDetail(Self);
  //
  inherited;
  // computa o registro para o pager
  if compute then
    RequestParentPager.ComputeDetail(Self);
  // seta flag de dados impressos
  if dataprt then
    NotifyDataBandPrinted;
end;

procedure TRLCustomBand.MarkPrintPosition;
begin
  fPrintPosition.x:=Left;
  fPrintPosition.y:=RequestParentPager.RelativePagerRow;
  fPrintSize.x    :=Width;
  fPrintSize.y    :=Height;
end;

procedure TRLCustomBand.SetBandType(const aValue:TRLBandType);
begin
  if aValue=fBandType then
    Exit;
  fBandType:=aValue;
  //
  Realign;
  Invalidate;
end;

procedure TRLCustomBand.AdjustCarbonGroup;
var
  p:TRLCustomSite;
  b:TControl;
  i:integer;
begin
  if fGroupIndex>0 then
  begin
    p:=FindParentSite;
    if p=nil then
      Exit;
    for i:=0 to p.ControlCount-1 do
    begin
      b:=p.Controls[i];
      if (b is TRLCustomBand) and not (b=Self) and (TRLCustomBand(b).GroupIndex=fGroupIndex) then
        TRLCustomBand(b).fCarbonCopies:=fCarbonCopies;
    end;
  end;
end;

procedure TRLCustomBand.AdjustFromCarbonGroup;
var
  p:TRLCustomSite;
  b:TControl;
  i:integer;
begin
  if fGroupIndex>0 then
  begin
    p:=FindParentSite;
    if p=nil then
      Exit;
    for i:=0 to p.ControlCount-1 do
    begin
      b:=p.Controls[i];
      if (b is TRLCustomBand) and not (b=Self) and (TRLCustomBand(b).GroupIndex=fGroupIndex) then
      begin
        fCarbonCopies:=TRLCustomBand(b).CarbonCopies;
        Break;
      end;
    end;
  end;
end;

procedure TRLCustomBand.SetCarbonCopies(const aValue:integer);
begin
  if aValue=fCarbonCopies then
    Exit;
  if aValue<1 then
    fCarbonCopies:=1
  else
    fCarbonCopies:=aValue;
  AdjustCarbonGroup;
end;

procedure TRLCustomBand.SetGroupIndex(const aValue:integer);
begin
  if aValue=fGroupIndex then
    Exit;
  if aValue<0 then
    fGroupIndex:=0
  else
    fGroupIndex:=aValue;
  AdjustFromCarbonGroup;
end;

function TRLCustomBand.GetCompleting:boolean;
var
  pager:TRLCustomPager;
begin
  pager :=FindParentPager;
  Result:=(pager<>nil) and (psCompleting in pager.PagerStatus);
end;

{ TRLCustomDetailGrid }

constructor TRLCustomDetailGrid.Create(aOwner:TComponent);
begin
  fBandType    :=btDetail;
  fColIndex    :=0;
  fColCount    :=1;
  fColSpacing  :=0;
  fColWidth    :=0;
  fRowIndex    :=0;
  fOrganization:=goInRows;
  //
  inherited Create(aOwner);
end;

procedure TRLCustomDetailGrid.Initialize;
begin
  inherited;
  //
  fColIndex:=0;
  fRowIndex:=0;
end;

function TRLCustomDetailGrid.GetClientCellRect(aColIndex,aRowIndex:integer):TRect;
var
  w,ws,h:integer;
  r:TRect;
begin
  r:=CalcSizeRect;
  //
  ws:=Round(fColSpacing*ScreenPPI/InchAsMM);
  if fColCount>0 then
    if fColWidth>0 then
      w:=Round(fColWidth*ScreenPPI/InchAsMM)
    else
      w:=Round((RectWidth(r)-(fColCount-1)*ws)/fColCount)
  else
    w:=r.Right-r.Left;
  //
  h:=r.Bottom-r.Top;
  //
  Result.Left  :=aColIndex*(w+ws);
  Result.Top   :=aRowIndex*h;
  Result.Right :=Result.Left+w;
  Result.Bottom:=Result.Top+h;
end;

procedure TRLCustomDetailGrid.DrawClient;
  procedure DrawBall(aRect:TRect; const aCaption:string);
  var
    tw,th,d:integer;
    t      :TRect;
  begin
    with Canvas do
    begin
      Pen.Style  :=psSolid;
      Pen.Color  :=clBlue;
      Pen.Mode   :=pmCopy;
      Brush.Style:=bsClear;
      with Font do
      begin
        Name :='Small Fonts';
        Size :=6;
        Style:=[];
        Color:=clBlue;
      end;
      tw:=TextWidth(' '+aCaption+' ');
      th:=TextHeight(aCaption);
      if tw>th then
        d:=tw
      else
        d:=th;
      // text rect
      t:=aRect;
      t.Right:=t.Left  +d+2;
      t.Top  :=t.Bottom-d-2;
      TextOut((t.Left+t.Right-tw) div 2,(t.Top+t.Bottom-th) div 2,aCaption);
      Ellipse(t.Left,t.Top,t.Right,t.Bottom);
    end;
  end;
var
  x:integer;
  r:TRect;
begin
  inherited;
  //
  x:=0;
  repeat
    r:=GetClientCellRect(x,0);
    DrawFrame(r,clGray,True);
    DrawBall(r,IntToStr(x+1));
    Inc(x);
  until not (x<fColCount);
end;

function TRLCustomDetailGrid.CalcEffectiveRect:TRect;
begin
  Result:=CalcSizeRect;
  with GetClientCellRect(0,0) do
  begin
    Result.Right :=Result.Left+(Right-Left);
    Result.Bottom:=Result.Top +(Bottom-Top);
  end;  
end;

procedure TRLCustomDetailGrid.VerticalExceeded;
begin
  // se a organização é em colunas, passa para a próxima coluna
  if fOrganization=goInColumns then
  begin
    Inc(fColIndex);
    if fColIndex>fColCount-1 then
    begin
      with RequestParentPager do
      begin
        InternalNewPage(Self,False);
        fTopRow   :=RelativePagerRow;
        fBottomRow:=fTopRow;
      end;
      fColIndex:=0;
    end;
  end
  else
    with RequestParentPager do
    begin
      InternalNewPage(Self,False);
      fTopRow   :=RelativePagerRow;
      fBottomRow:=fTopRow;
    end;
  fRowIndex:=0;
  //
  MarkPrintPosition;
end;

procedure TRLCustomDetailGrid.SkipToNextPosition(aWidth,aHeight:integer);
begin
  case fOrganization of
    goInRows   : begin
                   Inc(fColIndex);
                   if fColIndex>fColCount-1 then
                   begin
                     Inc(fRowIndex);
                     fColIndex:=0;
                   end;
                 end;
    goInColumns: Inc(fRowIndex);
  end;
end;

procedure TRLCustomDetailGrid.SurfaceOpening;
begin
  if (fColIndex=0) and (fRowIndex=0) then
  begin
    fTopRow   :=RequestParentPager.RelativePagerRow;
    fBottomRow:=fTopRow;
  end;
end;

procedure TRLCustomDetailGrid.SurfaceClosed;
begin
  inherited;
  //
  RequestParentPager.RelativePagerRow:=fBottomRow;
end;

procedure TRLCustomDetailGrid.MarkPrintPosition;
var
  cellrect:TRect;
  avail   :integer;
begin
  cellrect        :=GetClientCellRect(fColIndex,fRowIndex);
  fPrintPosition.x:=Left+cellrect.Left;
  fPrintPosition.y:=fTopRow+cellrect.Top;
  fPrintSize.x    :=RectWidth(cellrect);
  fPrintSize.y    :=RectHeight(cellrect);
  //
  if HeightFits(fPrintSize.y,avail) then
    fBottomRow:=Max(fBottomRow,fTopRow+cellrect.Bottom)
  else if not IntegralHeight then  
    fBottomRow:=Max(fBottomRow,fTopRow+cellrect.Top+avail);
end;

function TRLCustomDetailGrid.HeightFits(aHeight:integer; var aAvailable:integer):boolean;
var
  pagerrow:integer;
  pager   :TRLCustomPager;
begin
  pager     :=RequestParentPager;
  // excedeu a última linha para bands de dados?
  pagerrow  :=fTopRow+GetClientCellRect(fColIndex,fRowIndex).Top;
  aAvailable:=pager.GetRelativeFooterRow-pagerrow; /// consider columnfooterrow?
  Result    :=(aHeight<=aAvailable);
end;

function TRLCustomDetailGrid.GetBandTypeName:string;
begin
  Result:='DetailGrid';
end;

procedure TRLCustomDetailGrid.SetColCount(const aValue:integer);
begin
  if aValue=fColCount then
    Exit;
  if aValue<1 then
    fColCount:=1
  else
    fColCount:=aValue;
  RealignControls;
  Invalidate;
end;

procedure TRLCustomDetailGrid.SetColSpacing(const aValue:double);
begin
  if aValue=fColSpacing then
    Exit;
  fColSpacing:=aValue;
  RealignControls;
  Invalidate;
end;

procedure TRLCustomDetailGrid.SetColWidth(const aValue:double);
begin
  if aValue=fColWidth then
    Exit;
  fColWidth:=aValue;
  RealignControls;
  Invalidate;
end;

function TRLCustomDetailGrid.IsManyCols:boolean;
begin
  Result:=(fColCount>1);
end;

{ TRLCustomPager }

constructor TRLCustomPager.Create(aOwner:TComponent);
begin
  // initialization
  //fAllowedBands     :=[];
  //fDetailCount      :=0;
  //fMaxBands         :=0;
  //fMinBands         :=0;
  //fRelativePagerRow :=0;
  //fDetailsInSurface :=0;
  //fNewPageNeeded    :=False;
  fPageBreaking     :=pbNone;
  //fJumpPending      :=False;
  //fJumpLength       :=0;
  //fNewPageCaller    :=nil;
  //fForceMinBands    :=False;
  fFooterMeasuring  :=fmNone;
  //fDataBandPrinted  :=False;
  //fPagerStatus      :=[];
  // objects
  fSortedBands:=TRLSortedBands.Create;
  //
  inherited Create(aOwner);
  // customization
  fAlign     :=faTop;
  AutoSizeDir:=[asHeightDir];
end;

destructor TRLCustomPager.Destroy;
begin
  FreeObj(fSortedBands);
  //
  inherited;
end;

function TRLCustomPager.CreateChild(aType:TRLBandType):TRLCustomBand;
begin
  Result:=FindChild(aType);
  if Result<>nil then
    Exit;
  Result:=TRLBand.Create(Owner);
  with Result do
  begin
    Parent:=Self;
    if Self is TRLCustomReport then
      Align:=faTop
    else
      case aType of
        btHeader,
        btTitle,
        btColumnHeader: Align:=faTop;
        btDetail      : Align:=faClient;
        btSummary,
        btColumnFooter,
        btFooter      : Align:=faBottom;
      end;
    BandType:=aType;
    Height  :=20;
    Name    :=NewComponentName(Result);
  end;
end;

function TRLCustomPager.FindChild(aType:TRLBandType):TRLCustomBand;
var
  i:integer;
begin
  Result:=nil;
  for i:=0 to ControlCount-1 do
    if (Controls[i] is TRLCustomBand) and (TRLCustomBand(Controls[i]).BandType=aType) then
    begin
      Result:=TRLCustomBand(Controls[i]);
      Break;
    end;
end;

procedure TRLCustomPager.KillChild(aType:TRLBandType);
var
  b:TRLCustomBand;
begin
  b:=FindChild(aType);
  FreeObj(b);
end;

procedure TRLCustomPager.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if (aComponent is TRLCustomBand) and (TRLCustomBand(aComponent).Parent=Self) then
    case Operation of
      opInsert: Include(fAllowedBands,TRLCustomBand(aComponent).BandType);
      opRemove: Exclude(fAllowedBands,TRLCustomBand(aComponent).BandType);
    end;
end;

procedure TRLCustomPager.InternalNewPage(aCaller:TObject; aMoveOnly:boolean=False);
var
  savedcaller:TObject;
begin
  savedcaller   :=fNewPageCaller;
  fNewPageCaller:=aCaller;
  try
    // MoveOnly=True significa que o pager não vai se dividir entre a página atual e a próxima
    if aMoveOnly then
    else
      CloseSurface;
    if (Parent<>nil) and (Parent is TRLCustomPager) then
      TRLCustomPager(Parent).InternalNewPage(aCaller);
    if aMoveOnly then
      MarkPrintPosition
    else
      OpenSurface;
  finally
    fNewPageCaller:=savedcaller;
  end;
end;

procedure TRLCustomPager.SurfaceOpening;
begin
  inherited;
  //
  PushBoundsRect;
  InitializePageInfo;
  SortedBands.ResetPage;
end;

procedure TRLCustomPager.SurfaceBeginDraw;
begin
  inherited;
  //
  PrintHeaders;
end;

procedure TRLCustomPager.SurfaceEndDraw;
begin
  PrintFooters;
  //
  inherited;
end;

procedure TRLCustomPager.TruncateSurface;
begin
  Inc(fRelativePagerRow,CalcPrintWastedPixels.Bottom);
  if AutoTrunc then
    fPrintSize.y:=fRelativePagerRow;
end;

procedure TRLCustomPager.SurfaceClosed;
var
  p:TRLCustomPager;
begin
  inherited;
  //
  p:=FindParentPager;
  if p<>nil then
    p.RelativePagerRow:=p.RelativePagerRow+RectHeight(CalcPrintBoundsRect);
  //
  PopBoundsRect;
end;

function TRLCustomPager.PrintBands(aType:TRLBandType):TRLPrintBandResults;
var
  i,icc,qcc,savei,savedgroup:integer;
  e:TRLCustomSite;
  l:TList;
begin
  l:=SortedBands.List[aType];
  if l.Count>0 then
  begin
    i:=0;
    while i<l.Count do
    begin
      if l.Items[i]=fNewPageCaller then
      begin
        Result:=brStackExit;
        Exit;
      end;
      e:=TRLCustomSite(l.Items[i]);

      if e is TRLCustomBand then
      begin
        savedgroup:=TRLCustomBand(l.Items[i]).GroupIndex;
        savei     :=i;
        qcc       :=TRLCustomBand(l.Items[i]).CarbonCopies;
        icc       :=0;
        while icc<qcc do
        begin
          i:=savei;
          while (i<l.Count) and (TRLCustomBand(l.Items[i]).GroupIndex=savedgroup) do
          begin
            TRLCustomBand(l.Items[i]).CarbonIndex:=icc;
            PrintBand(TRLCustomBand(l.Items[i]));
            Inc(i);
            if savedgroup=0 then
              Break;
          end;
          Inc(icc);
        end;
      end
      else if e is TRLCustomSubDetail then
      begin
        PrintSite(e);
        Inc(i);
      end;
    end;
    SortedBands.Printed[aType]:=True;
    Result:=brPrinted;
  end
  else
    Result:=brNoBands;
end;

procedure TRLCustomPager.PrintBand(aBand:TRLCustomBand);
begin
  with aBand do
  begin
    // save all bounds
    PushBoundsRect;
    PushBoundsAllFrom(aBand);
    //
    if Visible then
    begin
      if not (boOptimisticPageBreak in Options) then
        CheckPageBreak;
      if CanPrint then
      begin
        AdjustBounds;
        Print;
        DoAfterPrint;
      end;
    end;
    // restore all bounds
    PopBoundsAllFrom(aBand);
    PopBoundsRect;
  end;
end;

procedure TRLCustomPager.PrintDetails;
var
  r:TRLPrintBandResults;
begin
  if fFooterMeasuring=fmBeforeDetail then
     MeasureFooters;

  PrintPagers(TRLCustomGroup);
  r:=PrintBands(btDetail);
  if r=brStackExit then
     Exit;
end;

procedure TRLCustomPager.PrintSite(aSite:TRLCustomSite);
begin
  with aSite do
    if CanPrint then
    begin
      AdjustBounds;
      Print;
      DoAfterPrint;
    end;
end;

procedure TRLCustomPager.PrintPagers(aClass:TRLPagerClassType);
var
  i:integer;
begin
  for i:=0 to ControlCount-1 do
    if Controls[i] is aClass then
      PrintSite(TRLCustomSite(Controls[i]));
end;

procedure TRLCustomPager.PrintEmptySkippers;
var
  i:integer;
begin
  for i:=0 to ControlCount-1 do
    if Controls[i] is TRLCustomSkipper then
      if TRLCustomSkipper(Controls[i]).PrintEmpty then
        PrintSite(TRLCustomSkipper(Controls[i]));
end;

procedure TRLCustomPager.PrintHeaders;
begin
  if not Enabled then
    Exit;
  // a ordem correta é Header, Title e ColumnHeader
  if not SortedBands.Printed[btHeader] then
    if PrintBands(btHeader)=brStackExit then
      Exit;
  if not SortedBands.Printed[btTitle] then
    if PrintBands(btTitle)=brStackExit then
      Exit;
  if not SortedBands.Printed[btColumnHeader] then
    if PrintBands(btColumnHeader)=brStackExit then
      Exit;
  if fFooterMeasuring=fmAfterHeader then
    MeasureFooters;
end;


{function TRLCustomPager.StillHeaderHeights:integer;
begin
  Result:=0;
  if not Enabled then
    Exit;
  if not SortedBands.Printed[btHeader] then
    if PrintBands(btHeader)=brStackExit then
      Exit;
  if not SortedBands.Printed[btTitle] then
    if PrintBands(btTitle)=brStackExit then
      Exit;
  if not SortedBands.Printed[btColumnHeader] then
    if PrintBands(btColumnHeader)=brStackExit then
      Exit;
  if fFooterMeasuring=fmAfterHeader then
    MeasureFooters;
end;}
///

procedure TRLCustomPager.PrintFooters(aSummarize:boolean=False);
begin
  if not Enabled then
    Exit;
  // a ordem correta é ColumnFooter, Summary e Footer
  if not SortedBands.Printed[btColumnFooter] then
    if PrintBands(btColumnFooter)=brStackExit then
      Exit;
  if aSummarize then
    if not SortedBands.Printed[btSummary] then
      if PrintBands(btSummary)=brStackExit then
        Exit;
  if not SortedBands.Printed[btFooter] then
    if PrintBands(btFooter)=brStackExit then
      Exit;
end;

procedure TRLCustomPager.PrintCompletion;
var
  i,iHeight,iLast:integer;
  l:TList;
  b:TRLCustomBand;
begin
  Include(fPagerStatus,psCompleting);
  try
    l:=SortedBands.List[btDetail];
    if l.Count=0 then
      Exit;
    // encontra a band que será utilizada para completar a página
    b:=nil;
    for i:=0 to l.Count-1 do
      if TObject(l.Items[i]) is TRLCustomBand then
        with TRLCustomBand(l.Items[i]) do
          if (Completion<>ctNone) and Computable then
          begin
            b:=TRLCustomBand(l.Items[i]);
            Break;
          end;
    if b=nil then
      Exit;
    case b.Completion of
      ctMinBands: while fDetailsInSurface<fMinBands do
                    PrintBand(b);
      ctMaxBands: while fDetailsInSurface<fMaxBands do
                    PrintBand(b);
      ctFullPage: begin
                    iHeight:=RectHeight(b.CalcPrintBoundsRect);
                    iLast  :=GetRelativeColumnFooterRow;
                    while not (fRelativePagerRow+iHeight>=iLast) do
                      if (fMaxBands>0) and not (fDetailsInSurface<fMaxBands) then
                        Break
                      else
                        PrintBand(b);
                  end;
    end;
  finally
    Exclude(fPagerStatus,psCompleting);
  end;
end;

procedure TRLCustomPager.SortBands;
var
  i:integer;
  c:TControl;
begin
  SortedBands.Clear;
  for i:=0 to ControlCount-1 do
  begin
    c:=ControlWithin(Controls[i]);
    if c is TRLCustomBand then
      SortedBands.Add(TRLCustomBand(c))
    else
    begin
      if c is TRLCustomSubDetail then
        SortedBands.Add(TRLCustomSubDetail(c));
      if c is TRLCustomPager then
        TRLCustomPager(c).SortBands;
    end;
  end;
end;

function TRLCustomPager.GoFooterRow:boolean;
var
  r:integer;
begin
  if not AutoTrunc then
  begin
    r     :=GetRelativeFooterRow;
    Result:=(fRelativePagerRow<r);
    if Result then
      fRelativePagerRow:=r;
  end
  else
    Result:=False;
end;

function TRLCustomPager.GoSummaryRow:boolean;
var
  r:integer;
begin
  r:=GetRelativeSummaryRow;
  Result:=fRelativePagerRow<r;
  if Result then
    fRelativePagerRow:=r;
end;

function TRLCustomPager.GoColumnFooterRow:boolean;
var
  r:integer;
begin
  r:=GetRelativeColumnFooterRow;
  Result:=fRelativePagerRow<r;
  if Result then
    fRelativePagerRow:=r;
end;

procedure TRLCustomPager.MeasureFooters;
var
  l:TList;
  i:integer;
begin
  l:=SortedBands.List[btFooter];
  for i:=0 to l.Count-1 do
    if TObject(l.Items[i]) is TRLCustomBand then
      TRLCustomBand(l.Items[i]).MeasureHeight;
  if (Parent<>nil) and (Parent is TRLCustomPager) then
    TRLCustomPager(Parent).MeasureFooters;
end;

function TRLCustomPager.GetFooterHeight:integer;
var
  l:TList;
  i:integer;
begin
  Result:=0;
  l:=SortedBands.List[btFooter];
  for i:=0 to l.Count-1 do
    if TObject(l.Items[i]) is TRLCustomBand then
      with TRLCustomBand(l.Items[i]) do
        if Visible then
          Inc(Result,Height);
end;

function TRLCustomPager.GetFooterHeightSum:integer;
begin
  Result:=GetFooterHeight;
  if (Parent<>nil) and (Parent is TRLCustomPager) then
    Inc(Result,TRLCustomPager(Parent).GetFooterHeightSum);
end;

function TRLCustomPager.GetColumnFooterHeight:integer;
var
  i:integer;
  l:TList;
begin
  Result:=0;
  l:=SortedBands.List[btColumnFooter];
  for i:=0 to l.Count-1 do
    if TObject(l.Items[i]) is TRLCustomBand then
      with TRLCustomBand(l.Items[i]) do
        if AlignToBottom and Visible then
          Inc(Result,Height);
end;

function TRLCustomPager.GetColumnFooterHeightSum:integer;
begin
  Result:=GetColumnFooterHeight;
  if (Parent<>nil) and (Parent is TRLCustomPager) then
    Inc(Result,TRLCustomPager(Parent).GetColumnFooterHeightSum);
end;

function TRLCustomPager.GetSummaryHeightSum:integer;
begin
  Result:=GetSummaryHeight;
  if (Parent<>nil) and (Parent is TRLCustomPager) then
    Inc(Result,TRLCustomPager(Parent).GetSummaryHeightSum);
end;

function TRLCustomPager.GetSummaryHeight:integer;
var
  i:integer;
  l:TList;
begin
  Result:=0;
  l:=SortedBands.List[btSummary];
  for i:=0 to l.Count-1 do
    if TObject(l.Items[i]) is TRLCustomBand then
      with TRLCustomBand(l.Items[i]) do
        if AlignToBottom and Visible then
          Inc(Result,Height);
end;

function TRLCustomPager.GetWastedBottomSum:integer;
begin
  Result:=CalcWastedPixels.Bottom;
  if (Parent<>nil) and (Parent is TRLCustomPager) then
    Inc(Result,TRLCustomPager(Parent).GetWastedBottomSum);
end;

function TRLCustomPager.GetRelativeFooterRow:integer;
var
  report:TRLCustomReport;
  printr:TRect;
  globxy:TPoint;
  wasted:integer;
  footer:integer;
begin
  report:=RequestParentReport;
  printr:=report.CalcPrintBoundsRect;
  globxy:=CalcGlobalPrintPosition;
  wasted:=GetWastedBottomSum;
  footer:=GetFooterHeightSum;
  Result:=printr.Bottom-globxy.y-wasted-footer;
end;

function TRLCustomPager.GetRelativeSummaryRow:integer;
begin
  Result:=GetRelativeFooterRow-GetSummaryHeightSum;
end;

function TRLCustomPager.GetRelativeColumnFooterRow:integer;
begin
  Result:=GetRelativeSummaryRow-GetColumnFooterHeightSum;
end;

procedure TRLCustomPager.MarkPrintPosition;
var
  p:TRLCustomPager;
begin
  p:=FindParentPager;
  if Assigned(p) then
  begin
    fPrintPosition.x:=Left;
    fPrintPosition.y:=p.RelativePagerRow;
    fPrintSize.x    :=Width;
    if AutoTrunc then
      fPrintSize.y:=(RequestParentReport.CalcPrintBoundsRect.Bottom-CalcGlobalPrintPosition.y)-p.CalcPrintWastedPixelsSum.Bottom-p.GetFooterHeightSum
    else
      fPrintSize.y:=Height;
  end
  else
  begin
    fPrintPosition.x:=0;
    fPrintPosition.y:=0;
    fPrintSize.x    :=Width;
    fPrintSize.y    :=Height;
  end;
end;

procedure TRLCustomPager.SetAllowedBands(const aValue:TRLAllowedBands);
var
  i:TRLBandType;
begin
  if aValue=fAllowedBands then
    Exit;
  fAllowedBands:=aValue;
  if csLoading in ComponentState then
    Exit;
  //
  if csDesigning in ComponentState then
    for i:=Low(TRLBandType) to High(TRLBandType) do
      if (i in aValue) xor (i in fAllowedBands) then
        if i in aValue then
          CreateChild(i)
        else
          KillChild(i);
end;

procedure TRLCustomPager.Initialize;
begin
  inherited;
  //
  fDetailCount:=0;
  InitializePageInfo;
//  PrintHeaders;///Adicionado aqui novo controle
end;

procedure TRLCustomPager.InitializePageInfo;
begin
  fDataBandPrinted :=False;
  fDetailsInSurface:=0;
  fNewPageNeeded   :=False;
  fJumpPending     :=False;
  fJumpLength      :=0;
  fRelativePagerRow:=CalcPrintWastedPixels.Top;
end;

procedure TRLCustomPager.ComputeDetail(aCaller:TObject);
begin
  inherited;
  //
  Inc(fDetailCount);
end;

procedure TRLCustomPager.InternalBeginDoc;
begin
  SortedBands.ResetAll;
  Initialize;
end;

procedure TRLCustomPager.InvalidatePage;
begin
  fNewPageNeeded:=True;
end;

function TRLCustomPager.GetNewPageNeeded:boolean;
begin
  Result        :=fNewPageNeeded;
  fNewPageNeeded:=False;
end;

procedure TRLCustomPager.InternalEndDoc;
begin
  PrintCompletion;
  PrintFooters(True);
  CloseSurface;
end;

procedure TRLCustomPager.BeginDoc;
begin
  InternalBeginDoc;
  MarkPrintPosition;
end;

procedure TRLCustomPager.EndDoc;
begin
  InternalEndDoc;
end;

procedure TRLCustomPager.NewPage;
begin
  InternalNewPage(nil);
end;

function TRLCustomPager.IsSatisfied: boolean;
begin
  Result:=True;
  if ForceMinBands and (MinBands>0) and (DetailsInSurface<MinBands) then
    Result:=False;
  if not DataBandPrinted then
    Result:=False;
end;

{ TRLCustomGroup }

constructor TRLCustomGroup.Create(aOwner:TComponent);
begin
  // initialization
  //fOnGetBreak :=nil;
  //fBroken     :=False;
  fDataFields :=emptystr;
  fDataFormula:=emptystr;

  // objects
  inherited Create(aOwner);
  // customization
  fAutoTrunc :=True;
  //
  Height     :=64;
end;

procedure TRLCustomGroup.InternalPaintFinish;
var
  p:TRLCustomReport;
begin
  inherited;
  //
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomGroup.InternalPaintFinish;');
  {$ENDIF}

  p:=FindParentReport;
  if not Assigned(p) or p.ShowDesigners then
    Signup('Group '+Name);
end;

//inicializa a confecção de bands
procedure TRLCustomGroup.InternalPrint;
var
  b:boolean;
  r:TRLCustomReport;
  p:TRLCustomPager;
  s:TRLCustomSkipper;
begin
  r:=RequestParentReport;
  p:=RequestParentPager;
  s:=RequestParentSkipper;
  //
  if (PageBreaking=pbBeforePrint) and Assigned(r) and r.DataBandPrinted then
  begin
    if Assigned(p) then
      p.InternalNewPage(Self,not p.IsSatisfied);
     MarkPrintPosition;
  end;
  //
  BeginDoc;
  //
  b:=True; // flag de primeira quebra
  fLastKey:=GetKey;
  while not s.DataEof and not r.Canceled do
  begin
    fBroken:=False;
    if b then
      b:=False
    else if CheckBreak then
      Break;
    s.RecordMoved:=False;
    PrintDetails;
    if fBroken then
      Break;
    if not s.RecordMoved then
      s.DataNext;
  end;
  //
  EndDoc;
  //
  if PageBreaking=pbAfterPrint then
    r.InvalidatePage;
end;

function TRLCustomGroup.GetKey:string;
var
  i:integer;
  n,k:string;
  f:tfield;
  s:TRLCustomSkipper;
begin
  s:=RequestParentSkipper;
  if Assigned(s.DataSource) and s.DataSource.DataSet.Active then
    if fDataFormula<>emptystr then
      Result:=FindParentReport.Parse(Self,fDataFormula)
    else if fDataFields<>emptystr then
    begin
      Result:=emptystr;
      n:=fDataFields;
      repeat
        i:=Pos(';',n);
        if i=0 then
          i:=Length(n)+1;
        k:=Copy(n,1,i-1);
        if k<>emptystr then
        begin
          f:=s.DataSource.DataSet.FindField(k);
          if f=nil then
            raise Exception.Create(LS_NotFoundStr+': '+Name+'.DataField "'+k+'"');
          Result:=Result+f.AsString;
        end;
        Delete(n,1,i);
      until n=emptystr;
    end
    else
      Result:=emptystr
  else
    Result:=emptystr;
end;

function TRLCustomGroup.CheckBreak:boolean;
var
  key:string;
  grp:TRLCustomGroup;
begin
  Result:=False;
  if Enabled then
    if Assigned(fOnGetBreak) then
      fOnGetBreak(Self,Result)
    else
    begin
      key     :=GetKey;
      Result  :=key<>fLastKey;
      fLastKey:=key;
    end
  else
  begin
    grp:=FindParentGroup;
    if grp<>nil then
      Result:=grp.CheckBreak;
  end;
  fBroken:=Result;
end;

procedure TRLCustomGroup.ComputeDetail(aCaller:TObject);
var
  p:TRLCustomPager;
begin
  inherited;
  //
  p:=FindParentPager;
  if p<>nil then
    p.ComputeDetail(aCaller);
end;

procedure TRLCustomGroup.SetDataFields(const Value: TRLDataFieldsProperty);
begin
  fDataFields:=Value;
  if fDataFields<>emptystr then
    fDataFormula:=emptystr;
end;

procedure TRLCustomGroup.SetDataFormula(const Value: string);
begin
  fDataFormula:=Value;
  if fDataFormula<>emptystr then
    fDataFields:=emptystr;
end;

{ TRLCustomSkipper }

constructor TRLCustomSkipper.Create(aOwner:TComponent);
begin
  // initialization
  fRecordAction:=raUseIt;
  //fDataSource  :=nil;
  //fOnNeedData  :=nil;
  //fOnDataCount :=nil;
  //fOnDataRecord:=nil;
  //fRecordMoved :=False;
  fDataEof     :=True;
  //fRecNo       :=0;
  //fCopyNo      :=0;
  fRecordRange :=rrAllRecords;
  //fRangeCount  :=0;
  //fPrintEmpty  :=False;

  // objects
  inherited Create(aOwner);
  // customization
end;

function TRLCustomSkipper.DataCount:integer;
begin
  if Assigned(fOnDataCount) then
    fOnDataCount(Self,Result)
  else if Assigned(DataSource) and Assigned(DataSource.DataSet) and DataSource.DataSet.Active then
    Result:=DataSource.DataSet.RecordCount
  else
    Result:=0;
end;

procedure TRLCustomSkipper.DataFirst;
var
  keepon:boolean;
  loops :Integer;
begin
  fRecNo :=1;
  fCopyNo:=1;
  loops  :=0;
  repeat
    Inc(loops);
    if Assigned(DataSource) then
      if Assigned(DataSource.DataSet) and DataSource.DataSet.Active then
      begin
        if loops=1 then
          if fRecordRange=rrAllRecords then
            DataSource.DataSet.First
          else
        else
          DataSource.DataSet.Next;
        keepon:=not DataSource.DataSet.Eof;
      end
      else
        keepon:=False
    else if Assigned(fOnNeedData) then
    begin
      keepon:=False;
      fOnNeedData(Self,keepon);
    end
    else
      keepon:=False;
    if (fRecordRange=rrNextN) and (fRecNo>fRangeCount) then
      keepon:=False;
    //
    fDataEof     :=not keepon;
    fRecordAction:=raUseIt;
    if Assigned(fOnDataRecord) then
      fOnDataRecord(Self,fRecNo,fCopyNo,fDataEof,fRecordAction);
    if fDataEof then
      Break;
    if fRecordAction in [raIgnoreIt] then
    else
      Break;
  until False;
  fRecordMoved:=False;
end;

procedure TRLCustomSkipper.DataNext;
var
  keepon:boolean;
begin
  if fRecordAction in [raUseAndRetain] then
    Inc(fCopyNo)
  else
    fCopyNo:=1;
  if fRecordAction in [raUseIt] then
    Inc(fRecNo);
  repeat
    if fRecordAction in [raUseIt,raIgnoreIt] then
    begin
      if Assigned(DataSource) then
        if Assigned(DataSource.DataSet) and DataSource.DataSet.Active then
        begin
          DataSource.DataSet.Next;
          keepon:=not DataSource.DataSet.Eof;
        end
        else
          keepon:=False
      else if Assigned(fOnNeedData) then
      begin
        keepon:=False;
        fOnNeedData(Self,keepon);
      end
      else
        keepon:=False;
    end
    else
      keepon:=True;
    if fRecordRange=rrCurrentOnly then
      keepon:=False
    else if (fRecordRange=rrNextN) and (fRecNo>fRangeCount) then
      keepon:=False;
    //
    fDataEof     :=not keepon;
    fRecordAction:=raUseIt;
    if Assigned(fOnDataRecord) then
      fOnDataRecord(Self,fRecNo,fCopyNo,fDataEof,fRecordAction);
  until fDataEof or not (fRecordAction in [raIgnoreIt]);
  fRecordMoved:=True;
end;

procedure TRLCustomSkipper.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
      fDataSource:=nil
end;

procedure TRLCustomSkipper.InternalPrint;
var
  r:TRLCustomReport;
  p:TRLCustomPager;
begin
  r:=RequestParentReport;
  p:=RequestParentPager;
  //
  if (PageBreaking=pbBeforePrint) and Assigned(r) and r.DataBandPrinted then
  begin
    if Assigned(p) then
      p.InternalNewPage(Self,not p.IsSatisfied);
    MarkPrintPosition;
  end;
  //
  BeginDoc;
  //  verifica a posicao de dados a serem impressos
  DataFirst;
  while not DataEof and not r.Canceled do
  begin
    fRecordMoved:=False;
    PrintDetails;
    if not fRecordMoved then
      DataNext;
  end;
  PrintAnything;
  //
  EndDoc;
  //
  if PageBreaking=pbAfterPrint then
    r.InvalidatePage;
end;

procedure TRLCustomSkipper.PrintAnything;
begin
  if fPrintEmpty and (fDetailCount=0) then
  begin
    OpenSurface;
    PrintEmptySkippers;
  end;
end;

procedure TRLCustomSkipper.SetDataSource(const aValue:TDataSource);
begin
  if aValue=fDataSource then
    Exit;
  fDataSource:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
end;

function TRLCustomSkipper.IsNextNRecordRange:boolean;
begin
  Result:=(RecordRange=rrNextN);
end;

{ TRLCustomSubDetail }

constructor TRLCustomSubDetail.Create(aOwner:TComponent);
begin
  // initialization
  // objects
  inherited Create(aOwner);
  // customization
  fPositioning:=btDetail;
  fAutoTrunc  :=True;
  //
  Height    :=64;
end;

procedure TRLCustomSubDetail.InternalPaintFinish;
var
  p:TRLCustomReport;
begin
  inherited;
  //
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomSubDetail.InternalPaintFinish');
  {$ENDIF}

  p:=FindParentReport;
  if not Assigned(p) or p.ShowDesigners then
    Signup('SubDetail '+Name);
end;

procedure TRLCustomSubDetail.SetPositioning(const Value: TRLBandType);
begin
  if Value=fPositioning then
    Exit;
  fPositioning:=Value;
  //
  Realign;
  Invalidate;
end;

{TRLCustomReport}

constructor TRLCustomReport.Create(aOwner:TComponent);
begin
  // initialization
  fShowProgress       :=True;
  //fDefaultFilter      :=nil;
  //fExpressionParser   :=nil;
  //fInternalParser     :=nil;
  //fOnPageEnding       :=nil;
  //fOnPageStarting     :=nil;
  //fCanceled           :=False;
  //fNextReport         :=nil;
  //fPriorReport        :=nil;
  fFirstPageNumber    :=1;
  //fPageIndex          :=0;
  fPrintDialog        :=True;
  fReportState        :=rsAbout;
  fShowDesigners      :=True;
  fShowTracks         :=True;
  //fShowExplosion      :=False;
  fPrintQuality       :=pqFullFeature;
  //fPageSurface        :=nil;
  //fOnFilterText       :=nil;
  //ProgressForm        :=nil;
  //fParseInvoker       :=nil;
  //fAdjustableMargins  :=False;
  //fPreviewOptions     :=nil;
  fForcePrepare       :=True;
  //fBackgroundMode     :=False;
  //
  FillChar(fPrinterMetrics,SizeOf(fPrinterMetrics),0);
  // objects
  fPages:=TRLGraphicStorage.Create(nil);
  fPages.Link(Self);
  fPageSetup:=TRLPageSetup.Create(Self);
  fPreviewOptions:=TRLPreviewOptions.Create(Self);
  //
  inherited Create(aOwner);
  // customization
  fMargins.SetDefaults(10,10,10,10);
  fPrintEmpty:=True;
  //
  ParentFont :=False;
  Font.Name  :='Arial';
  Font.Size  :=10;
  Font.Style :=[];
  Font.Color :=clBlack;
  ParentColor:=False;
  Color      :=clWhite;
  //
  ReloadPrinter;
end;

destructor TRLCustomReport.Destroy;
begin
  if Assigned(fPages) then
    fPages.Unlink(Self);
  FreeObj(fPageSurface);
  FreeObj(fPageSetup);
  FreeObj(fPreviewOptions);
  FreeObj(fInternalParser);
  //
  inherited;
end;

procedure TRLCustomReport.ReloadPrinter;
begin
  RLPrinter.LoadMetrics(fPrinterMetrics);
end;

procedure TRLCustomReport.CalcSize(var aSize:TPoint);
begin
  aSize     :=Point(Round(ScreenPPI*fPageSetup.OrientedWidth/InchAsMM),
                    Round(ScreenPPI*fPageSetup.OrientedHeight/InchAsMM));
  fFixedSize:=aSize;
  fSizeFixed:=True;
end;

procedure TRLCustomReport.ProgressCreate;
var
  q:integer;
  r:TRLCustomReport;
  m:TRLCustomReport;
begin
  m:=MasterReport;
  if m=Self then
  begin
    q:=1;
    r:=Self;
    while r.NextReport<>nil do
    begin
      /// todo: if enabled
      Inc(q);
      r:=r.NextReport;
    end;
    m.ProgressForm:=TfrmRLFeedBack.Create(LS_PreparingReportStr,q);
    m.ProgressForm.Show;
    m.ProgressForm.SetFocus;
  end
  else if m.ProgressForm<>nil then
    m.ProgressForm.NextBar;
end;

procedure TRLCustomReport.ProgressSetMax;
begin
  with MasterReport do
    if Assigned(ProgressForm) then
      ProgressForm.SetMax(Self.fProgressMax);
end;

procedure TRLCustomReport.ProgressPhase;
begin
  with MasterReport do
    if Assigned(ProgressForm) then
      ProgressForm.SetPhase(Self.fProgressPhase);
end;

procedure TRLCustomReport.ProgressStepIt;
begin
  with MasterReport do
    if Assigned(ProgressForm) then
    begin
      ProgressForm.StepIt;
      if ProgressForm.Canceled then
        SysUtils.Abort;
    end;
end;

procedure TRLCustomReport.ProgressDestroy;
begin
  FreeObj(ProgressForm);
end;

procedure TRLCustomReport.Cancel;
begin
  fCanceled:=True;
end;

procedure TRLCustomReport.Clear;
begin
  FreeObj(fPageSurface);
  //
  fPages.Clear;
  fReportState:=rsAbout;
  fCanceled   :=False;
  fPageIndex  :=0; // new 3.18
end;

procedure TRLCustomReport.SurfaceOpening;
begin
  inherited;
  //
  // Inc(MasterReport.fPageIndex); 3.18
end;

procedure TRLCustomReport.SurfaceBeginDraw;
begin
  inherited;
  //
  DoPageStarting;
end;

procedure TRLCustomReport.SurfaceEndDraw;
begin
  DoPageEnding;
  //
  inherited;
end;

procedure TRLCustomReport.SurfaceClosed;
begin
  inherited;
  //
  MasterReport.Pages.Add(fPageSurface);
  fPageSurface:=nil;
  Inc(MasterReport.fPageIndex); // new 3.18
end;

procedure TRLCustomReport.DoPageStarting;
begin
  if Assigned(fOnPageStarting) then
    fOnPageStarting(Self);
end;

procedure TRLCustomReport.DoPageEnding;
begin
  if Assigned(fOnPageEnding) then
    fOnPageEnding(Self);
end;

procedure TRLCustomReport.Notification(aComponent:TComponent; Operation:TOperation);
begin
  inherited;
  //
  if Operation=opRemove then
    if aComponent=fDataSource then
      fDataSource:=nil
    else if aComponent=fNextReport then
      fNextReport:=nil
    else if aComponent=fPriorReport then
      fPriorReport:=nil
    else if aComponent=fDefaultFilter then
      fDefaultFilter:=nil
    else if aComponent=fExpressionParser then
      fExpressionParser:=nil;
end;

procedure TRLCustomReport.DataFirst;
begin
  fProgressMax:=DataCount;
  if CanShowProgress then
    ProgressSetMax;
  //
  inherited;
end;

procedure TRLCustomReport.DataNext;
begin
  fProgressPhase:=LS_PageStr+' '+IntToStr(MasterReport.PageNumber);
  if CanShowProgress then
    ProgressPhase;
  if CanShowProgress then
    ProgressStepIt;
  if MasterReport.Pages.Canceled then
    SysUtils.Abort;
  //
  inherited;
end;

function TRLCustomReport.GetOrientedUnprintablePixels:TRect;
var
  r:TRect;
begin
  if fPrinterMetrics.PPIX*fPrinterMetrics.PPIY=0 then
    Result:=Rect(0,0,0,0)
  else
  begin
    with fPrinterMetrics do
      r:=Rect(Round(MarginLeft  *ScreenPPI/PPIX),
              Round(MarginTop   *ScreenPPI/PPIY),
              Round(MarginRight *ScreenPPI/PPIX),
              Round(MarginBottom*ScreenPPI/PPIY));
    if fPageSetup.Orientation=poPortrait then
      Result:=r
    else
    begin
      // as landscape the margins are turned anti clockwise
      Result.Left  :=r.Top;
      Result.Top   :=r.Right;
      Result.Right :=r.Bottom;
      Result.Bottom:=r.Left;
    end;
  end;
end;

function TRLCustomReport.GetOrientedUnprintableRect:TRect;
var
  r:TRect;
begin
  r:=GetOrientedUnprintablePixels;
  Result.Left  :=r.Left;
  Result.Top   :=r.Top;
  Result.Right :=Round(fPageSetup.OrientedWidth *ScreenPPI/InchAsMM)-r.Right;
  Result.Bottom:=Round(fPageSetup.OrientedHeight*ScreenPPI/InchAsMM)-r.Bottom;
end;

// unprintable area
procedure TRLCustomReport.DrawBackground(const aRect:TRect);
var
  r:TRect;
begin
  inherited;
  //
  with Canvas do
  begin
    r:=GetOrientedUnprintableRect;
    DrawUnusedRect(Rect(0,0,r.Left,Height));              // left
    DrawUnusedRect(Rect(r.Left,0,Width,r.Top));           // top
    DrawUnusedRect(Rect(r.Right,r.Top,Width,Height));     // right
    DrawUnusedRect(Rect(r.Left,r.Bottom,r.Right,Height)); // bottom
  end;
end;

procedure TRLCustomReport.InternalPaintFinish;
begin
  inherited;
  {$IFDEF TRACECUSTOMSITE}
  debugln('TRLCustomReport.InternalPaintFinish');
  {$ENDIF}
  //
  if fShowDesigners then
    Signup('Report '+Name);
end;

function TRLCustomReport.Prepare:boolean;
begin
  Result:=False;
  try
    InternalPrepare;
    Result:=True;
  except
    on e:EAbort do
      ;
    on e:Exception do
      ShowMessage(e.Message);
  end;
end;

procedure TRLCustomReport.InternalPrepare;
begin
  MasterReport.Pages.BeginUpdate;
  try
    PushBoundsRect;
    try
      if CanShowProgress then
        ProgressCreate;
      try
        Clear;
        //
        fReportState   :=rsPreparing;
        fReportDateTime:=Now;
        //
        UpdateMacros;
        //
        fCouldPrint:=True;
        DoBeforePrint;
        if not fCouldPrint then
          SysUtils.Abort;
        //
        SortBands;
        ReloadPrinter;
        //
        BeginDoc;
        //
        DataFirst;
        while not DataEof and not MasterReport.Canceled do
        begin
          fRecordMoved:=False;
          PrintDetails;
          if not fRecordMoved then
            DataNext;
        end;
        fReportState:=rsClosing;
        PrintAnything;
        //
        EndDoc;
        // verifica cancelamento
        if MasterReport.Canceled then
        begin
          fReportState:=rsAbout;
          SysUtils.Abort;
        end;
        //
        DoAfterPrint;
        // prepara o próximo relatório
        /// todo: if enabled
        if fNextReport<>nil then
          fNextReport.InternalPrepare;
        //
        UpdateMacros;
      except
        fReportState:=rsAbout;
        raise;
      end;
    finally
      PopBoundsRect;
      if CanShowProgress then
        ProgressDestroy;
    end;
  finally
    MasterReport.Pages.EndUpdate;
  end;
end;

procedure TRLCustomReport.PreviewModal;
begin
//carregar informações para a variavel fDeviceMode
//RLPrinter.SetPaperSize(fPageSetup.PaperWidth,fPageSetup.PaperHeight,fPageSetup.Orientation=poLandscape,fPageSetup.ForceEmulation);

if Assigned(DefaultFilter) and (not Assigned(PrintParams.Filter)) then
  PrintParams.Filter:=DefaultFilter;

if Assigned(PageParams) then begin
   PageParams.PaperMargins:= Rect(Round(Margins.LeftMargin), Round(Margins.TopMargin), Round(Margins.RightMargin), Round(Margins.BottomMargin));
   PageParams.PaperSize:= fpagesetup.fPaperSize;
   PageParams.PageOrientation:= PageSetup.Orientation;
   end;

  PrepareNeeded;
  with PreviewOptions do
    if Defaults=pdUseDefaults then
      PreviewPagesWithOptions(Self.Pages,True,DefaultFormStyle,DefaultPosition,DefaultWindowState,DefaultBorderIcons,DefaultHelpFile,DefaultHelpContext,DefaultCaption)
    else
      PreviewPagesWithOptions(Self.Pages,True,FormStyle,Position,WindowState,BorderIcons,HelpFile,HelpContext,Caption);

if Assigned(PageParams) then begin
   Margins.LeftMargin:= PageParams.PaperMargins.Left;
   Margins.RightMargin:= PageParams.PaperMargins.Right;
   fpagesetup.fPaperSize:= PageParams.PaperSize;
   PageSetup.SetOrientation(PageParams.PageOrientation);
   SurfaceBeginDraw;
   end;
end;

procedure TRLCustomReport.Preview(Dest:TRLPreview=nil); /// inverter: report não precisa conhecer preview
begin
if Assigned(DefaultFilter) and (not Assigned(PrintParams.Filter)) then
   PrintParams.Filter:=DefaultFilter;

  PrepareNeeded;
  if Assigned(Dest) then
    Dest.Pages:=Self.Pages
  else
    with PreviewOptions do
      if Defaults=pdUseDefaults then
        PreviewPagesWithOptions(Self.Pages,DefaultShowModal,DefaultFormStyle,DefaultPosition,DefaultWindowState,DefaultBorderIcons,DefaultHelpFile,DefaultHelpContext,DefaultCaption)
      else
        PreviewPagesWithOptions(Self.Pages,ShowModal,FormStyle,Position,WindowState,BorderIcons,HelpFile,HelpContext,Caption);
end;

procedure TRLCustomReport.ClosePreview;
begin
  PreviewClosed:=True;
end;

procedure TRLCustomReport.InitPrintParams;
begin
  PrintParams.Clear;
  PrintParams.HelpContext:=Self.HelpContext;
  if PrintParams.HelpContext<>0 then
    PrintParams.Options:=PrintParams.Options+[rpoHelp];
  if Self.ReportState=rsReady then
    if (csDesigning in ComponentState) or ForcePrepare then
    else
    begin
      PrintParams.MaxPage:=Self.Pages.PageCount;
      PrintParams.ToPage :=PrintParams.MaxPage;
    end else PrintParams.MaxPage:= 9999;
  PrintParams.Orientation:=Self.PageSetup.Orientation;
end;

function TRLCustomReport.ShowPrintDialog:boolean;
begin
  with TRLPrintDialog.Create(nil) do
    try
      Result:=Execute;
    finally
      Free;
    end;
end;

procedure TRLCustomReport.InternalPrint;
var
  ChoosenFilter:TRLCustomPrintFilter;
begin
  RLPrinter.SetPaperSize(fPageSetup.PaperWidth,fPageSetup.PaperHeight,fPageSetup.Orientation=poLandscape,fPageSetup.ForceEmulation);
  if Assigned(DefaultFilter) and (not Assigned(PrintParams.Filter)) then
    PrintParams.Filter:=DefaultFilter;
  //
  InitPrintParams;

  if fPrintDialog then
    if ShowPrintDialog then
    else
    begin
      fCanceled:=True;
      SysUtils.Abort;
    end
  else
    PrintParams.Apply;
  //
  PrepareNeeded;
  // se a preparação é em background, a impressão não pode sê-lo, pois geraria problemas
  // com o "BIOS" (ex: o form não pode ser liberado antes de terminar a preparação)
  ChoosenFilter:=PrintParams.Filter;
  if ChoosenFilter=nil then
  begin
    ChoosenFilter:=SpoolFilter;
    ChoosenFilter.ShowProgress:=Self.ShowProgress;
  end;
  ChoosenFilter.Pages         :=Self.Pages;
  ChoosenFilter.FirstPage     :=PrintParams.FromPage;
  if PrintParams.ToPage < Self.Pages.PageCount then
     ChoosenFilter.LastPage      :=PrintParams.ToPage else
     ChoosenFilter.LastPage      := Self.Pages.PageCount;
  ChoosenFilter.Copies        :=PrintParams.Copies;
  ChoosenFilter.BackgroundMode:=PrintParams.BackgroundMode;
  ChoosenFilter.Run;
end;

procedure TRLCustomReport.UpdateMacros; /// liberar páginas esperando WaitFor?
begin
  // atualiza símbolos
  RLPrinter.PrintDocName(Title);
  Pages.FirstPageNumber:=FirstPageNumber;
  Pages.LastPageNumber :=LastPageNumber;
  Pages.Title          :=Title;
  if PageSetup.Orientation=poLandscape then
    Pages.Orientation:=MetaOrientationLandscape
  else
    Pages.Orientation:=MetaOrientationPortrait;
  Pages.PaperWidth     :=PageSetup.PaperWidth;
  Pages.PaperHeight    :=PageSetup.PaperHeight;
end;

procedure TRLCustomReport.InternalEndDoc;
begin
  inherited;
  //
  fReportState:=rsReady;
end;

function TRLCustomReport.FindParentSurface:TRLGraphicSurface;
begin
  if not Assigned(fPageSurface) then
  begin
    fPageSurface:=TRLGraphicSurface.Create;
    with fPageSurface do
    begin
      Width      :=Self.Width;
      Height     :=Self.Height;
      if Self.PageSetup.Orientation=poLandscape then
        Orientation:=MetaOrientationLandscape
      else
        Orientation:=MetaOrientationPortrait;
      PaperWidth :=PageSetup.PaperWidth;
      PaperHeight:=PageSetup.PaperHeight;
      Open;
    end;
  end;
  //
  Result:=fPageSurface;
end;

function TRLCustomReport.GetCurrentPage:TRLGraphicSurface;
begin
  Result:=GetPageByNumber(PageNumber);
end;

function TRLCustomReport.GetPageByNumber(n:integer):TRLGraphicSurface;
begin
  if (fReportState=rsReady) and (n>=fFirstPageNumber) and (n<=LastPageNumber) then
    Result:=fPages[n-fFirstPageNumber]
  else
    Result:=nil;
end;

function TRLCustomReport.CalcSizeRect:TRect;
begin
  Result.Left:=0;
  Result.Top :=0;
  CalcSize(Result.BottomRight);
end;

procedure TRLCustomReport.SetPriorReport(const aValue:TRLCustomReport);
var
  old:TRLCustomReport;
begin
  old:=fPriorReport;
  if (aValue=old) or (aValue=Self) then
    Exit;
  fPriorReport:=aValue;
  if old<>nil then
    old.NextReport:=nil;
  if aValue<>nil then
  begin
    aValue.NextReport:=Self;
    aValue.FreeNotification(Self);
  end;
end;

procedure TRLCustomReport.SetNextReport(const aValue:TRLCustomReport);
var
  old:TRLCustomReport;
begin
  old:=fNextReport;
  if (aValue=old) or (aValue=Self) then
    Exit;
  fNextReport:=aValue;
  if old<>nil then
    old.PriorReport:=nil;
  if aValue<>nil then
  begin
    aValue.PriorReport:=Self;
    aValue.FreeNotification(Self);
  end;
end;

function TRLCustomReport.GetPageNumber;
begin
  Result:=fFirstPageNumber+fPageIndex;
end;

function TRLCustomReport.GetLastPageNumber:integer;
begin
  Result:=fPages.PageCount+fFirstPageNumber-1;
end;

procedure TRLCustomReport.SetPageIndex(const aValue:integer);
begin
  if (fReportState=rsReady) and (aValue>=0) and (aValue<=fPages.PageCount-1) then
    fPageIndex:=aValue;
end;

procedure TRLCustomReport.SetPageNumber(const aValue:integer);
begin
  SetPageIndex(aValue-fFirstPageNumber);
end;

procedure TRLCustomReport.SetShowDesigners(const aValue:boolean);
begin
  if aValue=fShowDesigners then
    Exit;
  fShowDesigners:=aValue;
  InvalidateAll;
end;

procedure TRLCustomReport.SetShowTracks(const aValue:boolean);
begin
  if aValue=fShowTracks then
    Exit;
  fShowTracks:=aValue;
  InvalidateAll;
end;

procedure TRLCustomReport.SetShowExplosion(const aValue:boolean);
begin
  if aValue=fShowExplosion then
    Exit;
  fShowExplosion:=aValue;
  InvalidateAll;
end;

procedure TRLCustomReport.AfterLoad;
begin
  FirstPageNumber      :=Pages.FirstPageNumber;
  Title                :=Pages.Title;
  PageSetup.PaperSize  :=PaperSizeBySize(Pages.PaperWidth,Pages.PaperHeight);
  if Pages.Orientation=MetaOrientationLandscape then
    PageSetup.Orientation:=poLandscape
  else
    PageSetup.Orientation:=poPortrait;
  PageSetup.PaperWidth :=Pages.PaperWidth;
  PageSetup.PaperHeight:=Pages.PaperHeight;
  //
  fReportState:=rsReady;
end;

procedure TRLCustomReport.SaveToStream(aStream:TStream);
begin
  PrepareNeeded;
  Pages.SaveToStream(aStream);
end;

procedure TRLCustomReport.LoadFromStream(aStream:TStream);
begin
  Clear;
  Pages.LoadFromStream(aStream);
  AfterLoad;
end;

procedure TRLCustomReport.SaveToFile(const aFileName:string);
var
  ChoosenFilter:TRLCustomSaveFilter;
begin
  PrepareNeeded;
  ChoosenFilter:=SaveFilterByFileName(aFileName);
  if ChoosenFilter<>nil then
  begin
    ChoosenFilter.Pages    :=Self.Pages;
    ChoosenFilter.FileName :=aFileName;
    ChoosenFilter.FirstPage:=1;
    ChoosenFilter.LastPage :=9999999;
    ChoosenFilter.Copies   :=1;
    ChoosenFilter.Run;
  end
  else
    Pages.SaveToFile(aFileName);
end;

procedure TRLCustomReport.LoadFromFile(const aFileName:string);
begin
  Clear;
  Pages.LoadFromFile(aFileName);
  AfterLoad;
end;

procedure TRLCustomReport.SetPrintQuality(const aValue:TRLPrintQuality);
begin
  if aValue=fPrintQuality then
    Exit;
  fPrintQuality:=aValue;
  InvalidateAll;
end;

procedure TRLCustomReport.SetDefaultFilter(const aValue:TRLCustomPrintFilter);
begin
  fDefaultFilter:=aValue;
  if aValue<>nil then
    aValue.FreeNotification(Self);
end;

function GetDataSourceOf(aControl:TControl):TDataSource;
begin
  if aControl is TRLCustomDBControl then
    Result:=TRLCustomDBControl(aControl).DataSource
  else if aControl is TRLCustomDBText then
    Result:=TRLCustomDBText(aControl).DataSource
  else if aControl is TRLCustomDBMemo then
    Result:=TRLCustomDBMemo(aControl).DataSource
  else if aControl is TRLCustomSkipper then
    Result:=TRLCustomSkipper(aControl).DataSource
  else
    Result:=nil;
end;

procedure TRLCustomReport.ParserResource(Sender:TObject; const aIdentifier:string; aParams:variant; var aResult:variant);
var
  src:TDataSource;
  fld:TField;
  ctr:TWinControl;
begin
  aResult:=Unassigned;
  //
  if Assigned(fParseInvoker) and (fParseInvoker is TRLCustomDBResult) then
  begin
    aResult:=TRLCustomDBResult(fParseInvoker).Resolve(Sender,aIdentifier,aParams);
    if not VarIsEmpty(aResult) then
      Exit;
  end;
  //
  ctr:=TWinControl(fParseInvoker);
  while ctr<>nil do
  begin
    src:=GetDataSourceOf(ctr);
    if Assigned(src) and Assigned(src.DataSet) then
    begin
      fld:=src.DataSet.FindField(aIdentifier);
      if Assigned(fld) then
      begin
        // considerar valores nulos
        if fld.IsNull and Assigned(fParseInvoker) and (fParseInvoker is TRLCustomDBResult) and TRLCustomDBResult(fParseInvoker).ComputeNulls then
          aResult:=GetFieldNullValue(fld)
        else if IsTextField(fld) then
          aResult:=SmartGetFieldDisplayText(fld)
        else
          aResult:=fld.Value;
        Break;
      end;
    end;
    ctr:=ctr.Parent;
  end;
end;

procedure TRLCustomReport.ParserTokener(Sender:TObject; var aToken:string; var aKind:TRLParserTokenKind);
begin
end;

function MatchFriendlyName(aComp:TComponent; const aName:string):boolean;
begin
  Result:=SameText(aComp.Name,aName) or (aComp is TRLCustomControl) and SameText(TRLCustomControl(aComp).FriendlyName,aName);
end;

procedure TRLCustomReport.ParserFindAgregate(Sender:TObject; aOwner:TPersistent; const aName:string; var aAgregate:TPersistent);
var
  i:integer;
begin
  if aOwner is TComponent then
    with TComponent(aOwner) do
    begin
      i:=0;
      while (i<ComponentCount) and not MatchFriendlyName(Components[i],aName) do
        Inc(i);
      if i<ComponentCount then
        aAgregate:=Components[i];
    end;
end;

procedure TRLCustomReport.ParserGetAttribute(Sender:TObject; aOwner:TPersistent; const aName:string; var aValue:variant);
var
  fld:TField;
begin
  if (aOwner is TDataSource) and Assigned(TDataSource(aOwner).DataSet) then
    aOwner:=TDataSource(aOwner).DataSet;
  if aOwner is TDataSet then
  begin
    fld:=TDataSet(aOwner).FindField(aName);
    if Assigned(fld) then
      aValue:=fld.Value;
  end
  else if aOwner is TRLCustomControl then
    aValue:=TRLCustomControl(aOwner).GetAttribute(aName)
  else if aOwner is TFont then
    with TFont(aOwner) do
      aValue:=Name+' '+IntToStr(Size);
end;

procedure TRLCustomReport.ParserSetAttribute(Sender:TObject; aOwner:TPersistent; const aName:string; const aValue:variant; var aHandled:boolean);
begin
  if aOwner is TRLCustomControl then
    aHandled:=TRLCustomControl(aOwner).SetAttribute(aName,aValue);
end;

procedure TRLCustomReport.SetExpressionParser(const aValue:TRLExpressionParser);
var
  old:TRLExpressionParser;
begin
  old:=fExpressionParser;
  if aValue=old then
    Exit;
  if old<>nil then
  begin
    old.ResourceProc    :=nil;
    old.TokenProc       :=nil;
    old.FindAgregateProc:=nil;
    old.SetAttributeProc:=nil;
    old.GetAttributeProc:=nil;
  end;
  fExpressionParser:=aValue;
  if aValue<>nil then
  begin
    aValue.ResourceProc    :=Self.ParserResource;
    aValue.TokenProc       :=Self.ParserTokener;
    aValue.FindAgregateProc:=Self.ParserFindAgregate;
    aValue.SetAttributeProc:=Self.ParserSetAttribute;
    aValue.GetAttributeProc:=Self.ParserGetAttribute;
    aValue.FreeNotification(Self);
  end;
end;

procedure TRLCustomReport.DoFilterText(var aText:string; var aPrintIt:boolean);
begin
  if Assigned(fOnFilterText) then
    fOnFilterText(Self,aText,aPrintIt);
end;

function TRLCustomReport.Parse(Sender:TObject; const aExpression:string):variant;
begin
  fParseInvoker:=Sender;
  try
    Result:=ParserNeeded.Evaluate(aExpression);
  finally
    fParseInvoker:=nil;
  end;
end;

function TRLCustomReport.CalcMarginalPixels:TRect;
var
  u:TRect;
begin
  Result:=inherited CalcMarginalPixels;
  if fAdjustableMargins then
  begin
    u:=GetOrientedUnprintablePixels;
    Result.Left  :=Max(Result.Left  ,u.Left);
    Result.Top   :=Max(Result.Top   ,u.Top);
    Result.Right :=Max(Result.Right ,u.Right);
    Result.Bottom:=Max(Result.Bottom,u.Bottom);
  end;
end;

procedure TRLCustomReport.SetAdjustableMargins(const aValue:boolean);
begin
  if aValue=fAdjustableMargins then
    Exit;
  fAdjustableMargins:=aValue;
  AdjustBounds;
  Invalidate;
end;

procedure TRLCustomReport.SetPageSetup(const Value: TRLPageSetup);
begin
  fPageSetup.Assign(Value);
end;

procedure TRLCustomReport.SetPreviewOptions(const Value: TRLPreviewOptions);
begin
  fPreviewOptions.Assign(Value);
end;

function TRLCustomReport.ParserNeeded: TRLExpressionParser;
begin
  if Assigned(fExpressionParser) then
    Result:=fExpressionParser
  else
  begin
    if not Assigned(fInternalParser) then
    begin
      fInternalParser:=TRLExpressionParser.Create(nil);
      fInternalParser.ResourceProc    :=Self.ParserResource;
      fInternalParser.TokenProc       :=Self.ParserTokener;
      fInternalParser.FindAgregateProc:=Self.ParserFindAgregate;
      fInternalParser.SetAttributeProc:=Self.ParserSetAttribute;
      fInternalParser.GetAttributeProc:=Self.ParserGetAttribute;
      fInternalParser.NameSpace       :=Self.Owner;
    end;
    Result:=fInternalParser;
  end;
end;

procedure TRLCustomReport.PrepareNeeded;
begin
  if (csDesigning in ComponentState) or ForcePrepare or (ReportState=rsAbout) then
  begin
    Clear;
    fReportState:=rsInitiating;
    Pages.PrepareUpdate;
    if BackgroundMode then
      ThreadIt(InternalPrepare).Resume
    else
      InternalPrepare;
  end;
end;

function TRLCustomReport.CanShowProgress: boolean;
begin
  // o progresso da impressão não pode ser concorrente do progresso de preparação
  //todo: remover este código assim que consertar showprogress
  {$IFDEF LINUX}
  Result:=MasterReport.ShowProgress;
  {$ELSE}
  Result:=MasterReport.ShowProgress and IsMainThread;
  {$ENDIF}
end;

{ TRLPreviewOptions }

constructor TRLPreviewOptions.Create(aOwner: TRLCustomReport);
begin
  fParentReport:=aOwner;
  //fShowModal   :=False;
  fFormStyle   :=fsNormal;
  fPosition    :=poScreenCenter;
  fWindowState :=wsMaximized;
  fBorderIcons :=[biSystemMenu,biMinimize,biMaximize];
  fHelpFile    :=emptystr;
  //fHelpContext :=0;
  fCaption     :=LS_PreviewStr;
  //
  inherited Create;
end;

procedure TRLPreviewOptions.Assign(Source: TRLPreviewOptions);
begin
  WindowState:=Source.WindowState;
  Position   :=Source.Position;
  FormStyle  :=Source.FormStyle;
  ShowModal  :=Source.ShowModal;
  BorderIcons:=Source.BorderIcons;
  HelpFile   :=Source.HelpFile;
  HelpContext:=Source.HelpContext;
  Caption    :=Source.Caption;
  Defaults   :=Source.Defaults;
end;

function TRLPreviewOptions.IsCaption: boolean;
begin
  Result:=(fCaption<>LS_PreviewStr);
end;

{ TRLPanel }

procedure TRLPanel.WMMouseMove(var Msg: TLMessage);
begin
  inherited;
//  Perform(WM_SYSCOMMAND,$F003,0);   ///redimensionar em run time
end;

end.



