{@unit RLConsts - Variáveis de internacionalização e variáveis de configuração.
Portado para o Lazarus - Trabalho inicial de Isaac Trindade da Silva contato tioneobrasil@yahoo.com.br dicas4lazarus@yahoo.com.br
Lazarus Ported - initial work by Isaac 07/2009
}

unit RLConsts;

interface

uses
  SysUtils;

const  
  CommercialVersion=3;
  ReleaseVersion   =24;
  CommentVersion   ='(B14) ';

  LazVersion          =0;
  LazReleaseVersion   =3;
  LazCommentVersion   ='(Beta)';

const
  {@const ScreenPPI - Resolução do monitor em pixels por polegada.
   Representa a quantidade de pixels por polegada do vídeo. O valor real varia de monitor para monitor mas,
   para facilitar cálculos e tornar os projetos independentes do terminal, essa valor é assumido como sendo 96. :/}
  ScreenPPI=96;

  {@const InchAsMM - Fator de conversão de polegada para milímetros.
   Este fator é utilizado em diversos pontos para conversões de coordenadas. :/}
  InchAsMM=254/10;

  {@const MMAsPixels - Fator de conversão de milímetros para pixels de tela.
   @links ScreenPPI, InchAsMM. :/}
  MMAsPixels=ScreenPPI/InchAsMM;

const
  CS_CopyrightStr   ='Copyright © 1999-2008 Fortes Informática';
  CS_ProductTitleStr='FortesReport (Open Source)';
  CS_URLStr         ='http://www.fortesreport.com.br';
  CS_AuthorNameStr  ='Ronaldo Moreira, Eduardo Richeli';

var
  {@var LS_PrintingInProgressStr - Variável de internacionalização para "Imprimindo o relatório..." :/}
  LS_PrintingInProgressStr:string;
  {@var LS_FilterInProgressStr - Variável de internacionalização para "Salvando o relatório..." :/}
  LS_FilterInProgressStr  :string;
  {@var LS_PreparingReportStr - Variável de internacionalização para "Preparando o relatório..." :/}
  LS_PreparingReportStr   :string;
  {@var LS_PrinterNotFoundStr - Variável de internacionalização para "Nenhuma impressora encontrada" :/}
  LS_PrinterNotFoundStr   :string;
  {@var LS_NoPathToPrinterStr - Variável de internacionalização para "Caminho inválido para a impressora" :/}
  LS_NoPathToPrinterStr   :string;
  {@var LS_LoadDefaultConfigStr - Variável de internacionalização para "Será carregada a configuração padrão" :/}
  LS_LoadDefaultConfigStr :string;
  {@var LS_PrinterDriverErrorStr - Variável de internacionalização para "Erro no driver da impressora" :/}
  LS_PrinterDriverErrorStr:string;
  {@var LS_PageStr - Variável de internacionalização para "Página" :/}
  LS_PageStr              :string;
  {@var LS_PrepareErrorStr - Variável de internacionalização para "Erro durante a preparação do relatório" :/}
  LS_PrepareErrorStr      :string;
  {@var LS_PageBreakStr - Variável de internacionalização para "Continua..." :/}
  LS_PageBreakStr         :string;
  {@var LS_PageMendStr - Variável de internacionalização para "Continuação" :/}
  LS_PageMendStr          :string;
  {@var LS_ReportEndStr - Variável de internacionalização para "Fim" :/}
  LS_ReportEndStr         :string;
  {@var LS_FileNotFoundStr - Variável de internacionalização para "Arquivo não encontrado" :/}
  LS_FileNotFoundStr      :string;
  {@var LS_FileNameStr - Variável de internacionalização para "Nome do arquivo" :/}
  LS_FileNameStr          :string;
  {@var LS_AllFileTypesStr - Variável de internacionalização para "Todos os arquivos" :/}
  LS_AllFileTypesStr      :string;
  {@var LS_LoadReportStr - Variável de internacionalização para "Carregar relatório" :/}
  LS_LoadReportStr        :string;
  {@var LS_NotFoundStr - Variável de internacionalização para "Não encontrado" :/}
  LS_NotFoundStr          :string;
  {@var LS_WaitStr - Variável de internacionalização para "Aguarde..." :/}
  LS_WaitStr              :string;
  {@var LS_FinishedStr - Variável de internacionalização para "Concluído" :/}
  LS_FinishedStr          :string;
  {@var LS_CancelStr - Variável de internacionalização para "Cancelar" :/}
  LS_CancelStr            :string;
  {@var LS_CloseStr - Variável de internacionalização para "Fechar" :/}
  LS_CloseStr             :string;
  {@var LS_SaveStr - Variável de internacionalização para "Salvar" :/}
  LS_SaveStr              :string;
  {@var LS_SendStr - Variável de internacionalização para "Enviar" :/}
  LS_SendStr              :string;
  {@var LS_PrintStr - Variável de internacionalização para "Imprimir" :/}
  LS_PrintStr             :string;
  {@var LS_AboutTheStr - Variável de internacionalização para "Sobre o" :/}
  LS_AboutTheStr          :string;
  {@var LS_PreviewStr - Variável de internacionalização para "Pré-visualização" :/}
  LS_PreviewStr           :string;
  {@var LS_OfStr - Variável de internacionalização para "de" :/}
  LS_OfStr                :string;
  {@var LS_ZoomStr - Variável de internacionalização para "Zoom" :/}
  LS_ZoomStr              :string;
  {@var LS_FirstPageStr - Variável de internacionalização para "Primeira página" :/}
  LS_FirstPageStr         :string;
  {@var LS_PriorPageStr - Variável de internacionalização para "Página anterior" :/}
  LS_PriorPageStr         :string;
  {@var LS_NextPageStr - Variável de internacionalização para "Próxima página" :/}
  LS_NextPageStr          :string;
  {@var LS_LastPageStr - Variável de internacionalização para "Última página" :/}
  LS_LastPageStr          :string;
  {@var LS_EntirePageStr - Variável de internacionalização para "Página inteira" :/}
  LS_EntirePageStr        :string;
  {@var LS_EntireWidthStr - Variável de internacionalização para "Largura da página" :/}
  LS_EntireWidthStr       :string;
  {@var LS_MultiplePagesStr - Variável de internacionalização para "Várias páginas" :/}
  LS_MultiplePagesStr     :string;
  {@var LS_ConfigPrinterStr - Variável de internacionalização para "Configurar impressora" :/}
  LS_ConfigPrinterStr     :string;
  {@var LS_SaveToFileStr - Variável de internacionalização para "Salvar em disco" :/}
  LS_SaveToFileStr        :string;
  {@var LS_SendToStr - Variável de internacionalização para "Enviar para" :/}
  LS_SendToStr            :string;
  {@var LS_PrinterStr - Variável de internacionalização para "Impressora" :/}
  LS_PrinterStr           :string;
  {@var LS_NameStr - Variável de internacionalização para "Nome" :/}
  LS_NameStr              :string;
  {@var LS_PrintToFileStr - Variável de internacionalização para "Imprimir em arquivo" :/}
  LS_PrintToFileStr       :string;
  {@var LS_PrintInBackgroundStr - Variável de internacionalização para "Imprimir em segundo plano" :/}
  LS_PrintInBackgroundStr :string;
  {@var LS_OptionsStr - Variável de internacionalização para "Opções" de filtragem. :/}
  LS_OptionsStr           :string;
  {@var LS_SaveInBackground - Variável de internacionalização para "Salvar em segundo plano" :/}
  LS_SaveInBackground     :string;
  {@var LS_BackgroundSavingStr - Variável de internacionalização para "Salvar em segundo plano" :/}
  LS_BackgroundSavingStr  :string;
  {@var LS_PageRangeStr - Variável de internacionalização para "Intervalo de páginas" :/}
  LS_PageRangeStr         :string;
  {@var LS_CopyAsImageStr - Variável de internacionalização para "Copiar como imagem" :/}
  LS_CopyAsImageStr       :string;
  {@var LS_RangeFromStr - Variável de internacionalização para "de" :/}
  LS_RangeFromStr         :string;
  {@var LS_RangeToStr - Variável de internacionalização para "até" :/}
  LS_RangeToStr           :string;
  {@var LS_AllStr - Variável de internacionalização para "Tudo" :/}
  LS_AllStr               :string;
  {@var LS_PagesStr - Variável de internacionalização para "Páginas" :/}
  LS_PagesStr             :string;
  {@var LS_SelectionStr - Variável de internacionalização para "Seleção" :/}
  LS_SelectionStr         :string;
  {@var LS_CopiesStr - Variável de internacionalização para "Cópias" :/}
  LS_CopiesStr            :string;
  {@var LS_NumberOfCopiesStr - Variável de internacionalização para "Número de cópias" :/}
  LS_NumberOfCopiesStr    :string;
  {@var LS_OkStr - Variável de internacionalização para "Ok" :/}
  LS_OkStr                :string;
  {@var LS_DivideScreenStr - Variável de internacionalização para "Dividir a tela" :/}
  LS_DivideScreenStr      :string;
  {@var LS_InvalidNameStr - Variável de internacionalização para "Nome inválido" :/}
  LS_InvalidNameStr       :string;
  {@var LS_DuplicateNameStr - Variável de internacionalização para "Nome já utilizado" :/}
  LS_DuplicateNameStr     :string;
  {@var LS_UseFilterStr - Variável de internacionalização para "Usar Filtro" :/}
  LS_UseFilterStr         :string;
  {@var LS_WebPageStr - Variável de internacionalização para "Página da Web" :/}
  LS_WebPageStr           :string;
  {@var LS_RichFormatStr - Variável de internacionalização para "Formato RichText" :/}
  LS_RichFormatStr        :string;
  {@var LS_PDFFormatStr - Variável de internacionalização para "Documento PDF" :/}
  LS_PDFFormatStr         :string;
  {@var LS_XLSFormatStr - Variável de internacionalização para "Planilha Excel" :/}
  LS_XLSFormatStr         :string;
  {@var LS_AtStr - Variável de internacionalização para "em" :/}
  LS_AtStr                :string;
  {@var LS_FormStr - Variável de internacionalização para "Formulário" :/}
  LS_FormStr              :string;
  {@var LS_DefaultStr - Variável de internacionalização para "Padrão" :/}
  LS_DefaultStr           :string;
  {@var LS_ColsStr - Variável de internacionalização para "cols" :/}
  LS_ColsStr              :string;
  {@var LS_ZoomInStr - Variável de internacionalização para "Aumentar o zoom" :/}
  LS_ZoomInStr            :string;
  {@var LS_ZoomOutStr - Variável de internacionalização para "Diminuir o zoom" :/}
  LS_ZoomOutStr           :string;
  {@var LS_CopyStr - Variável de internacionalização para "Copiar" :/}
  LS_CopyStr              :string;
  {@var LS_EditStr - Variável de internacionalização para "Editar" :/}
  LS_EditStr              :string;
  {@var LS_FindCaptionStr - Variável de internacionalização para "Procurar" :/}
  LS_FindCaptionStr       :string;
  {@var LS_TextToFindStr - Variável de internacionalização para "Te&xto" :/}
  LS_TextToFindStr        :string;
  {@var LS_FindNextStr - &Variável de internacionalização para "Próxima" :/}
  LS_FindNextStr          :string;
  {@var LS_WholeWordsStr - Variável de internacionalização para "Palavras &inteiras" :/}
  LS_WholeWordsStr        :string;
  {@var LS_MatchCaseStr - Variável de internacionalização para "Diferenciar &maiúsculas de minúsculas" :/}
  LS_MatchCaseStr         :string;
  {@var LS_DirectionUpStr - Variável de internacionalização para "A&cima" :/}
  LS_DirectionUpStr       :string;
  {@var LS_DirectionDownStr - Variável de internacionalização para "A&baixo" :/}
  LS_DirectionDownStr     :string;
  {@var LS_DirectionCaptionStr - Variável de internacionalização para "Direção" :/}
  LS_DirectionCaptionStr  :string;
  {@var LS_ColumnsStr - Variável de internacionalização para "Colunas". :/}
  LS_ColumnsStr           :string;
  {@var Ls_GetOutlineTextMetrics - Variável de internacionalização para "GetOutlineTextMetrics erro". :/}
  Ls_GetOutlineTextMetrics: String; //3.24.5
  {@var Ls_Salvar_Como - Variável de internacionalização para "Salvar como". :/}
  Ls_Salvar_Como          : String; //3.24.5
  {@var Ls_Nome_Arquivo - Variável de internacionalização para "Nome do Arquivo". :/}
  Ls_Nome_Arquivo         : String; //3.24.5
  {@var Ls_Nome_Arquivo - Variável de internacionalização para "Propriedades". :/}
  Ls_Propriedades         : String; //3.24.5
  {@var Ls_Nome_Arquivo - Variável de internacionalização para "Progresso". :/}
  Ls_Progresso            : String; //3.24.6
  {@var Ls_Nome_Arquivo - Variável de internacionalização para "Aplicar". :/}
  Ls_Aplicar              : String; //3.24.5
  {@var Ls_File_corrupted - Variável de internacionalização para "Arquivo corrompido". :/}
  Ls_File_corrupted       : String; //3.24.12
  {@var Ls_File_corrupted - Variável de internacionalização para "Versão de Arquivo inválido". :/}
  Ls_File_version         : String;  //3.24.12
  {@var Ls_Page_setings - Variável de internacionalização para "Configuração da Página". :/}
  Ls_Page_settings         : String;  //3.24.12
  {@var Ls_Page_margins - Variável de internacionalização para "Margem da Página". :/}
  Ls_Page_margins         : String;  //3.24.12
  {@var Ls_Page_margins_top - Variável de internacionalização para "Margem Superior". :/}
  Ls_Page_margins_top     : String;  //3.24.12
  {@var Ls_Page_margins_bottom - Variável de internacionalização para "Margem Inferior". :/}
  Ls_Page_margins_bottom  : String;  //3.24.13
  {@var Ls_Page_margins_rigth - Variável de internacionalização para "Margem direita". :/}
  Ls_Page_margins_rigth   : String;  //3.24.13
  {@var Ls_Page_left_bottom - Variável de internacionalização para "Margem equerda". :/}
  Ls_Page_margins_left    : String;  //3.24.13
  {@var Ls_Page_margins_paper - Variável de internacionalização para "Margem do Papel". :/}
  Ls_Page_margins_paper   : String;  //3.24.13
  {@var Ls_Page_paper - Variável de internacionalização para "Papel". :/}
  Ls_Page_paper           : String;  //3.24.13
  {@var Ls_Paper_Size - Variável de internacionalização para "Tamanho do Papel". :/}
  Ls_Paper_Size           : String;  //3.24.13
  {@var Ls_Paper_Width - Variável de internacionalização para "Largura do Papel". :/}
  Ls_Paper_Size_Width     : String;  //3.24.13
  {@var Ls_Paper_Size_Heigth - Variável de internacionalização para "Altura do Papel". :/}
  Ls_Paper_Size_Heigth     : String;  //3.24.13
  {@var Ls_Paper_Orientation - Variável de internacionalização para "Orientação do Papel". :/}
  Ls_Paper_Orientation     : String;  //3.24.13
  {@var Ls_Paper_Orientation_Landscape - Variável de internacionalização para "Orientação da página em retrato". :/}
  Ls_Paper_Orientation_Landscape: String;  //3.24.13
  {@var Ls_Paper_Orientation_Portrait - Variável de internacionalização para "Orientação da página em paisagem". :/}
  Ls_Paper_Orientation_Portrait: String;  //3.24.13


const
  RLFILEEXT='.rpf';

procedure DetectLocale;
procedure LoadPortugueseStrings;
procedure LoadEnglishStrings;
procedure LoadFrenchStrings;
procedure LoadSpanishStrings;

{/@unit}

implementation

procedure LoadPortugueseStrings;
begin
  LS_PrintingInProgressStr:='Imprimindo o relatório...';
  LS_FilterInProgressStr  :='Salvando o relatório...';
  LS_PreparingReportStr   :='Preparando o relatório...';
  LS_PrinterNotFoundStr   :='Nenhuma impressora encontrada';
  LS_NoPathToPrinterStr   :='Caminho inválido para a impressora';
  LS_LoadDefaultConfigStr :='Será carregada a configuração padrão';
  LS_PrinterDriverErrorStr:='Erro no driver da impressora';
  LS_PageStr              :='Página';
  LS_PrepareErrorStr      :='Erro durante a preparação do relatório';
  LS_PageBreakStr         :='Continua...';
  LS_PageMendStr          :='Continuação';
  LS_ReportEndStr         :='Fim';
  LS_FileNotFoundStr      :='Arquivo não encontrado';
  LS_FileNameStr          :='Nome do arquivo';
  LS_AllFileTypesStr      :='Todos os arquivos';
  LS_LoadReportStr        :='Carregar relatório';
  LS_NotFoundStr          :='Não encontrado';
  LS_WaitStr              :='Aguarde...';
  LS_FinishedStr          :='Concluído';
  LS_CancelStr            :='Cancelar';
  LS_CloseStr             :='Fechar';
  LS_SaveStr              :='Salvar';
  LS_SendStr              :='Enviar';
  LS_PrintStr             :='Imprimir';
  LS_AboutTheStr          :='Sobre o';
  LS_PreviewStr           :='Pré-visualização';
  LS_OfStr                :='de';
  LS_ZoomStr              :='Zoom';
  LS_FirstPageStr         :='Primeira página';
  LS_PriorPageStr         :='Página anterior';
  LS_NextPageStr          :='Próxima página';
  LS_LastPageStr          :='Última página';
  LS_EntirePageStr        :='Página inteira';
  LS_EntireWidthStr       :='Largura da página';
  LS_MultiplePagesStr     :='Várias páginas';
  LS_ConfigPrinterStr     :='Configurar impressora';
  LS_SaveToFileStr        :='Salvar em disco';
  LS_SendToStr            :='Enviar para';
  LS_PrinterStr           :='Impressora';
  LS_NameStr              :='Nome';
  LS_PrintToFileStr       :='Imprimir em arquivo';
  LS_PrintInBackgroundStr :='Imprimir em segundo plano';
  LS_SaveInBackground     :='Salvar em segundo plano';
  LS_PageRangeStr         :='Intervalo de páginas';
  LS_RangeFromStr         :='de';
  LS_RangeToStr           :='até';
  LS_AllStr               :='Tudo';
  LS_PagesStr             :='Páginas';
  LS_SelectionStr         :='Seleção';
  LS_CopiesStr            :='Cópias';
  LS_NumberOfCopiesStr    :='Número de cópias';
  LS_OkStr                :='Ok';
  LS_DivideScreenStr      :='Dividir a tela';
  LS_InvalidNameStr       :='Nome inválido';
  LS_DuplicateNameStr     :='Nome já utilizado';
  LS_UseFilterStr         :='Usar Filtro';
  LS_WebPageStr           :='Página da Web';
  LS_RichFormatStr        :='Formato RichText';
  LS_PDFFormatStr         :='Documento PDF';
  LS_XLSFormatStr         :='Planilha Excel';
  LS_AtStr                :='em';
  LS_FormStr              :='Formulário';
  LS_DefaultStr           :='Padrão';
  LS_ColsStr              :='cols';
  LS_ZoomInStr            :='Aumentar o zoom';
  LS_ZoomOutStr           :='Diminuir o zoom';
  LS_CopyStr              :='Copiar';
  LS_EditStr              :='Editar';
  LS_FindCaptionStr       :='Procurar';
  LS_TextToFindStr        :='Te&xto';
  LS_FindNextStr          :='&Próxima';
  LS_WholeWordsStr        :='Palavras &inteiras';
  LS_MatchCaseStr         :='Diferenciar &maiúsculas de minúsculas';
  LS_DirectionUpStr       :='A&cima';
  LS_DirectionDownStr     :='A&baixo';
  LS_DirectionCaptionStr  :='Direção';
  Ls_GetOutlineTextMetrics:='Erro no calculo de  fontes'; 
  Ls_Salvar_Como          :='Salvar Como';
  Ls_Nome_Arquivo         :='Nome do Arquivo:';
  LS_OptionsStr           :='Opções';
  LS_ColumnsStr           :='Colunas';
  Ls_Propriedades         :='Propriedades';
  Ls_Progresso            :='Progresso';
  Ls_Aplicar              :='Aplicar';
  Ls_File_corrupted       :='Arquivo Corrompido';
  Ls_File_version         :='Versão de arquivo inválido!';
  Ls_Page_settings         :='Configurações da página';
  Ls_Page_margins         :='Margem';
  Ls_Page_margins_top     :='Superior';
  Ls_Page_margins_bottom  :='Inferior';
  Ls_Page_margins_paper   :='Margsss';
  Ls_Page_paper           :='Papel';
  Ls_Paper_Size           :='Tamanho do Papel';
  Ls_Paper_Size_Width     :='Largura';
  Ls_Paper_Size_Heigth    :='Altura';
  Ls_Page_margins_rigth   :='Direita';
  Ls_Page_margins_left    :='Esquerda';
  Ls_Paper_Orientation    :='Orientação';
  Ls_Paper_Orientation_Portrait:= 'Retrato';
  Ls_Paper_Orientation_Landscape:='Paisagem';
end;

procedure LoadEnglishStrings;
begin
  LS_PrintingInProgressStr:='Printing in progress...';
  LS_FilterInProgressStr  :='Saving report...';
  LS_PreparingReportStr   :='Preparing report...';
  LS_PrinterNotFoundStr   :='Printer not found';
  LS_NoPathToPrinterStr   :='Invalid printer path';
  LS_LoadDefaultConfigStr :='Load default configuration';
  LS_PrinterDriverErrorStr:='Printer driver error';
  LS_PageStr              :='Page';
  LS_PrepareErrorStr      :='Error while preparing report';
  LS_PageBreakStr         :='Continues...';
  LS_PageMendStr          :='Continuation';
  LS_ReportEndStr         :='End';
  LS_FileNotFoundStr      :='File not found';
  LS_FileNameStr          :='File Name';
  LS_AllFileTypesStr      :='All files';
  LS_LoadReportStr        :='Load report';
  LS_NotFoundStr          :='Not found';
  LS_WaitStr              :='Wait...';
  LS_FinishedStr          :='Finished';
  LS_CancelStr            :='Cancel';
  LS_CloseStr             :='Close';
  LS_SaveStr              :='Save';
  LS_SendStr              :='Send';
  LS_PrintStr             :='Print';
  LS_AboutTheStr          :='About';
  LS_PreviewStr           :='Preview';
  LS_OfStr                :='of';
  LS_ZoomStr              :='Zoom';
  LS_FirstPageStr         :='First page';
  LS_PriorPageStr         :='Prior page';
  LS_NextPageStr          :='Next page';
  LS_LastPageStr          :='Last page';
  LS_EntirePageStr        :='Entire page';
  LS_EntireWidthStr       :='Entire width';
  LS_MultiplePagesStr     :='Multiple pages';
  LS_ConfigPrinterStr     :='Configure printer';
  LS_SaveToFileStr        :='Save to file';
  LS_SendToStr            :='Send to';
  LS_PrinterStr           :='Printer';
  LS_NameStr              :='Name';
  LS_PrintToFileStr       :='Print to file';
  LS_PrintInBackgroundStr :='Print in background';
  LS_SaveInBackground     :='Save in background';
  LS_PageRangeStr         :='Page range';
  LS_RangeFromStr         :='from';
  LS_RangeToStr           :='to';
  LS_AllStr               :='All';
  LS_PagesStr             :='Pages';
  LS_SelectionStr         :='Selection';
  LS_CopiesStr            :='Copies';
  LS_NumberOfCopiesStr    :='Number of copies';
  LS_OkStr                :='Ok';
  LS_DivideScreenStr      :='Divide the screen';
  LS_InvalidNameStr       :='Invalid name';
  LS_DuplicateNameStr     :='Name already in use';
  LS_UseFilterStr         :='Use filter';
  LS_WebPageStr           :='Web page';
  LS_RichFormatStr        :='RichText Format';
  LS_PDFFormatStr         :='PDF Document';
  LS_XLSFormatStr         :='Excel spreadsheet';
  LS_AtStr                :='at';
  LS_FormStr              :='Form';
  LS_DefaultStr           :='Default';
  LS_ColsStr              :='cols';
  LS_ZoomInStr            :='Increase zoom';
  LS_ZoomOutStr           :='Decrease zoom';
  LS_CopyStr              :='Copy';
  LS_EditStr              :='Edit';
  LS_FindCaptionStr       :='Find';
  LS_TextToFindStr        :='Te&xt';
  LS_FindNextStr          :='Find &Next';
  LS_WholeWordsStr        :='&Whole words only';
  LS_MatchCaseStr         :='&Match Case';
  LS_DirectionUpStr       :='&Up';
  LS_DirectionDownStr     :='&Down';
  LS_DirectionCaptionStr  :='Direction';
  Ls_GetOutlineTextMetrics:='GetOutlineTextMetrics failed';
  Ls_Salvar_Como          :='Save as';
  Ls_Nome_Arquivo         :='File Name:';
  LS_OptionsStr           :='Options';
  LS_ColumnsStr           :='Columns';
  LS_Propriedades         :='Settings';
  Ls_Progresso            :='Progress';
  Ls_Aplicar              :='Apply';
  Ls_File_corrupted       :='File is corrupted!';
  Ls_File_version         :='Invalid file version!';
  Ls_Page_settings         :='Page Configuration';
  Ls_Page_margins         :='Margins';
  Ls_Page_margins_top     :='Top';
  Ls_Page_margins_bottom  :='Bottom';
  Ls_Page_paper           :='Paper';
  Ls_Paper_Size           :='Paper Size';
  Ls_Paper_Size_Width     :='Width';
  Ls_Paper_Size_Heigth    :='Height';
  Ls_Page_margins_rigth   :='Right';
  Ls_Page_margins_left    :='Left';
  Ls_Paper_Orientation    :='Orientation';
  Ls_Paper_Orientation_Portrait:= 'Portrait';
  Ls_Paper_Orientation_Landscape:= 'Landscape';
end;

procedure LoadFrenchStrings;
begin
  LS_PrintingInProgressStr:='Stampando in progresso...';
  LS_FilterInProgressStr  :='Rapporto che salva...';
  LS_PreparingReportStr   :='Rapporto che prepara...';
  LS_PrinterNotFoundStr   :='Stampante non fondato';
  LS_NoPathToPrinterStr   :='Percorso di stampante nullo';
  LS_LoadDefaultConfigStr :='Carico configurazione predefinita';
  LS_PrinterDriverErrorStr:='Errore di conducente di stampante';
  LS_PageStr              :='Pagina';
  LS_PrepareErrorStr      :='Errore mentre preparando rapporto';
  LS_PageBreakStr         :='Continua...';
  LS_PageMendStr          :='A suivre';
  LS_ReportEndStr         :='Fin';
  LS_FileNotFoundStr      :='Fichier non trouvé';
  LS_FileNameStr          :='Nom de Fichier';
  LS_AllFileTypesStr      :='Tous les fichiers';
  LS_LoadReportStr        :='Ouvrir rapport';
  LS_NotFoundStr          :='Non trouvé';
  LS_WaitStr              :='Patientez...';
  LS_FinishedStr          :='Fini';
  LS_CancelStr            :='Annulé';
  LS_CloseStr             :='Fermé';
  LS_SaveStr              :='Sauver';
  LS_SendStr              :='Envoyez';
  LS_PrintStr             :='Imprimer';
  LS_AboutTheStr          :='A propos de';
  LS_PreviewStr           :='Aperçu avant impression';
  LS_OfStr                :='de';
  LS_ZoomStr              :='Zoom';
  LS_FirstPageStr         :='Première page';
  LS_PriorPageStr         :='Page précédente';
  LS_NextPageStr          :='Page suivante';
  LS_LastPageStr          :='Dernière page';
  LS_EntirePageStr        :='Page entière';
  LS_EntireWidthStr       :='Pleine largeur';
  LS_MultiplePagesStr     :='Plusieurs pages';
  LS_ConfigPrinterStr     :='Configuration de l''imprimante';
  LS_SaveToFileStr        :='Enregistrer sous';
  LS_SendToStr            :='Envoyez à';
  LS_PrinterStr           :='Imprimante';
  LS_NameStr              :='Nom';
  LS_PrintToFileStr       :='Imprimer dans un fichier';
  LS_PrintInBackgroundStr :='Imprimer dans background';
  LS_SaveInBackground     :='Enregistrer dans background';
  LS_PageRangeStr         :='Intervalle de pages';
  LS_RangeFromStr         :='de';
  LS_RangeToStr           :='à';
  LS_AllStr               :='Tout';
  LS_PagesStr             :='Pages';
  LS_SelectionStr         :='Sélection';
  LS_CopiesStr            :='Copies';
  LS_NumberOfCopiesStr    :='Nombre de copies';
  LS_OkStr                :='Ok';
  LS_DivideScreenStr      :='Dividir a tela';
  LS_InvalidNameStr       :='Nom inadmissible';
  LS_DuplicateNameStr     :='Nom reproduit';
  LS_UseFilterStr         :='Use filter';
  LS_WebPageStr           :='Page de Web';
  LS_RichFormatStr        :='Formato RichText';
  LS_PDFFormatStr         :='Documento PDF';
  LS_XLSFormatStr         :='Excel tableur';
  LS_AtStr                :='à';
  LS_FormStr              :='Formulaire';
  LS_DefaultStr           :='Défaut';
  LS_ColsStr              :='cols';
  LS_ZoomInStr            :='Grandir zoom';
  LS_ZoomOutStr           :='Réduire zoom';
  LS_CopyStr              :='Copier';
  LS_EditStr              :='Edit';
  LS_FindCaptionStr       :='Trouvaille';
  LS_TextToFindStr        :='Te&xte';
  LS_FindNextStr          :='A&près';
  LS_WholeWordsStr        :='Mots &entiers seulement';
  LS_MatchCaseStr         :='Cas d''allu&mette';
  LS_DirectionUpStr       :='Le &Haut';
  LS_DirectionDownStr     :='Le &bas';
  LS_DirectionCaptionStr  :='Direction';
  Ls_GetOutlineTextMetrics:='error in invalid font calculation';
  Ls_Salvar_Como          :='Sauver as'; //3.24.5
  Ls_Nome_Arquivo         :='Nobre Fichier:';// 3.24b5
  LS_OptionsStr           :='Opciones';      //3.24b5
  LS_ColumnsStr           :='colonne';      //3.25b5
  Ls_Progresso            :='progrès';      //3.25b5
  Ls_Aplicar              :='appliquez';      //3.24.6
end;

procedure LoadSpanishStrings;
begin
  LS_PrintingInProgressStr:='Impresión en marcha...';
  LS_FilterInProgressStr  :='Guardando el informe...';
  LS_PreparingReportStr   :='Preparación del informe...';
  LS_PrinterNotFoundStr   :='Impresora no encontrada';
  LS_NoPathToPrinterStr   :='Caminho inválido para a impressora';
  LS_LoadDefaultConfigStr :='Cargar la configuración estándar';
  LS_PrinterDriverErrorStr:='Error en driver de la impresora';
  LS_PageStr              :='Página';
  LS_PrepareErrorStr      :='Un error ocurrió mientras se preparaba el informe';
  LS_PageBreakStr         :='Continúa...';
  LS_PageMendStr          :='Continuación';
  LS_ReportEndStr         :='Extremo';
  LS_FileNotFoundStr      :='Archivo no encontrado';
  LS_FileNameStr          :='Nombre del Archivo';
  LS_AllFileTypesStr      :='Todos los archivos';
  LS_LoadReportStr        :='Cargar el informe';
  LS_NotFoundStr          :='No encontrado';
  LS_WaitStr              :='Espera...';
  LS_FinishedStr          :='Finalizado';
  LS_CancelStr            :='Cancelar';
  LS_CloseStr             :='Cerrar';
  LS_SaveStr              :='Guardar';
  LS_SendStr              :='Enviar';
  LS_PrintStr             :='Imprimir';
  LS_AboutTheStr          :='Sobre';
  LS_PreviewStr           :='Ver';
  LS_OfStr                :='de';
  LS_ZoomStr              :='Zoom';
  LS_FirstPageStr         :='Primera página';
  LS_PriorPageStr         :='Página anterior';
  LS_NextPageStr          :='Página siguiente';
  LS_LastPageStr          :='Última página';
  LS_EntirePageStr        :='Página entera';
  LS_EntireWidthStr       :='Ancho completo';
  LS_MultiplePagesStr     :='Varias páginas';
  LS_ConfigPrinterStr     :='Configurar la impresora';
  LS_SaveToFileStr        :='Guardar en un archivo';
  LS_SendToStr            :='Envíar a';
  LS_PrinterStr           :='Impresora';
  LS_NameStr              :='Nombre';
  LS_PrintToFileStr       :='Imprimir a un archivo';
  LS_PrintInBackgroundStr :='Imprimir en background';
  LS_SaveInBackground     :='Guardar en background';
  LS_PageRangeStr         :='Intervalo de páginas';
  LS_RangeFromStr         :='de';
  LS_RangeToStr           :='a';
  LS_AllStr               :='Todas';
  LS_PagesStr             :='Páginas';
  LS_SelectionStr         :='Selección';
  LS_CopiesStr            :='Copias';
  LS_NumberOfCopiesStr    :='Número de copias';
  LS_OkStr                :='Ok';
  LS_DivideScreenStr      :='Dividir la pantalla';
  LS_InvalidNameStr       :='Nombre inválido';
  LS_DuplicateNameStr     :='Nombre ya en uso';
  LS_UseFilterStr         :='Usar Filtro';
  LS_WebPageStr           :='Página Web';
  LS_RichFormatStr        :='Formato RichText';
  LS_PDFFormatStr         :='Documento PDF';
  LS_XLSFormatStr         :='Planilha Excel';
  LS_AtStr                :='en';
  LS_FormStr              :='Formulario';
  LS_DefaultStr           :='Estándar';
  LS_ColsStr              :='cols';
  LS_ZoomInStr            :='Aumentar zoom';
  LS_ZoomOutStr           :='Disminuir zoom';
  LS_CopyStr              :='Copiar';
  LS_EditStr              :='Editar';
  LS_FindCaptionStr       :='Buscar';
  LS_TextToFindStr        :='Te&xto';
  LS_FindNextStr          :='&Siguiente';
  LS_WholeWordsStr        :='Palabras &completas sólamente';
  LS_MatchCaseStr         :='Diferenciar &mayúsculas y minúsculas';
  LS_DirectionUpStr       :='En&cima';
  LS_DirectionDownStr     :='&Abajo';
  LS_DirectionCaptionStr  :='Dirección';
  Ls_GetOutlineTextMetrics:='error en tamanho de font'; //3.24b5
  Ls_Salvar_Como          :='Guardar en';               //3.24.5
  Ls_Nome_Arquivo         :='Nombre Archivo:';          // 3.24b5
  LS_OptionsStr           :='Opziones';                 // 3.24b5
  LS_ColumnsStr           :='columna';                  // 3.25b5
  Ls_Progresso            :='Progressione';             // 3.25b5
  Ls_Aplicar              :='aplique';                  //3.24b6
end;

procedure LoadItalianStrings;
begin
  LS_PrintingInProgressStr:='Impression du rapport...';
  LS_FilterInProgressStr  :='Sauver le rapport...';
  LS_PreparingReportStr   :='Préparation du rapport...';
  LS_PrinterNotFoundStr   :='Imprimante non trouvée';
  LS_NoPathToPrinterStr   :='Invalid printer path';
  LS_LoadDefaultConfigStr :='Chargement de la configuration standard';
  LS_PrinterDriverErrorStr:='Erreur dans le driver d''impression';
  LS_PageStr              :='Page';
  LS_PrepareErrorStr      :='Erreur durant la prépartaion du rapport';
  LS_PageBreakStr         :='Continua...';
  LS_PageMendStr          :='Continuazione';
  LS_ReportEndStr         :='Fine';
  LS_FileNotFoundStr      :='Archivio non fondò';
  LS_FileNameStr          :='Nome di file';
  LS_AllFileTypesStr      :='Tutti archiviano';
  LS_LoadReportStr        :='Rapporto di carico';
  LS_NotFoundStr          :='Non fondato';
  LS_WaitStr              :='Attesa...';
  LS_FinishedStr          :='Finito';
  LS_CancelStr            :='Annulli';
  LS_CloseStr             :='Vicino';
  LS_SaveStr              :='Salvataggio';
  LS_SendStr              :='Spinta dellonda';
  LS_PrintStr             :='Stampa';
  LS_AboutTheStr          :='Circa';
  LS_PreviewStr           :='Anteprima';
  LS_OfStr                :='di';
  LS_ZoomStr              :='Zoom';
  LS_FirstPageStr         :='Prima la pagina';
  LS_PriorPageStr         :='Antecedente la pagina';
  LS_NextPageStr          :='Prossimo pagina';
  LS_LastPageStr          :='Ultima pagina';
  LS_EntirePageStr        :='Pagina intera';
  LS_EntireWidthStr       :='Ampiezza intera';
  LS_MultiplePagesStr     :='Pagine multiple';
  LS_ConfigPrinterStr     :='Configuri stampante';
  LS_SaveToFileStr        :='Salvi archiviare';
  LS_SendToStr            :='Spedisca a';
  LS_PrinterStr           :='Stampante';
  LS_NameStr              :='Nome';
  LS_PrintToFileStr       :='Stampi archiviare';
  LS_PrintInBackgroundStr :='Stampi in sfondo';
  LS_SaveInBackground     :='Salvi in sfondo';
  LS_PageRangeStr         :='Serie di pagina';
  LS_RangeFromStr         :='da';
  LS_RangeToStr           :='a';
  LS_AllStr               :='Tutti';
  LS_PagesStr             :='Pagine';
  LS_SelectionStr         :='Selezione';
  LS_CopiesStr            :='Copie';
  LS_NumberOfCopiesStr    :='Numero di copie';
  LS_OkStr                :='Ok';
  LS_DivideScreenStr      :='Divida lo schermo';
  LS_InvalidNameStr       :='Nome nullo';
  LS_DuplicateNameStr     :='Già chiami in uso';
  LS_UseFilterStr         :='Filtro di uso';
  LS_WebPageStr           :='Pagina di Web';
  LS_RichFormatStr        :='RichText Format';
  LS_PDFFormatStr         :='PDF Document';
  LS_XLSFormatStr         :='Eccella foglio di calcolo elettronico';
  LS_AtStr                :='a';
  LS_FormStr              :='Forma';
  LS_DefaultStr           :='Contumacia';
  LS_ColsStr              :='cols';
  LS_ZoomInStr            :='Zoom di aumento';
  LS_ZoomOutStr           :='Zoom di calo';
  LS_CopyStr              :='Copia';
  LS_EditStr              :='Compili';
  LS_FindCaptionStr       :='Trouvaille';
  LS_TextToFindStr        :='Testo';
  LS_FindNextStr          :='Trovi Prossimo';
  LS_WholeWordsStr        :='Parole intere solamente';
  LS_MatchCaseStr         :='Accoppi Caso';
  LS_DirectionUpStr       :='Su';
  LS_DirectionDownStr     :='In giù';
  LS_DirectionCaptionStr  :='Direzione';
  Ls_GetOutlineTextMetrics:='GetOutlineTextMetrics andò a vuoto';
  Ls_Salvar_Como          :='Applichi un unguento su come'; //3.24.5
  Ls_Nome_Arquivo         :='Nome di file:';// 3.24b5
  LS_OptionsStr           :='Opciones';      //3.24b5
  LS_ColumnsStr           :='Colonnas';      //3.25b5
  Ls_Progresso            :='Progresso';     //3.25b5
  Ls_Aplicar              :='Applichi';      //3.24.6
end;

procedure LoadSwedishStrings;
begin
  LS_PrintingInProgressStr:='Das Drucken im Gange...';
  LS_FilterInProgressStr  :='Das Sparen von Bericht  ...';
  LS_PreparingReportStr   :='Das Vorbereiten von Berich...';
  LS_PrinterNotFoundStr   :='Drucker fand nicht';
  LS_NoPathToPrinterStr   :='Ungültiger Druckerpfad';
  LS_LoadDefaultConfigStr :='Laden Sie Standardkonfiguration';
  LS_PrinterDriverErrorStr:='Druckerfahrer Fehler';
  LS_PageStr              :='Seite';
  LS_PrepareErrorStr      :='Fehler, während das Vorbereiten von Bericht';
  LS_PageBreakStr         :='Setzt fort...';
  LS_PageMendStr          :='Fortsetzung';
  LS_ReportEndStr         :='Ende';
  LS_FileNotFoundStr      :='File fand nicht';
  LS_FileNameStr          :='Akte Name';
  LS_AllFileTypesStr      :='Alles legt ab';
  LS_LoadReportStr        :='Lastbericht';
  LS_NotFoundStr          :='Finden Sie nicht';
  LS_WaitStr              :='Wartezeit...';
  LS_FinishedStr          :='Beendet';
  LS_CancelStr            :='Sagen Sie ab';
  LS_CloseStr             :='Ende';
  LS_SaveStr              :='Ballabwehr';
  LS_SendStr              :='Schicken Sie';
  LS_PrintStr             :='Druck';
  LS_AboutTheStr          :='Über';
  LS_PreviewStr           :='Vorschau';
  LS_OfStr                :='von';
  LS_ZoomStr              :='Gummilinse';
  LS_FirstPageStr         :='Erste Seite';
  LS_PriorPageStr         :='Vorausgehende Seite';
  LS_NextPageStr          :='Danach Seite';
  LS_LastPageStr          :='Letzte Seite';
  LS_EntirePageStr        :='Ganze Seite';
  LS_EntireWidthStr       :='Ganze Weite';
  LS_MultiplePagesStr     :='Mehrfache Seiten';
  LS_ConfigPrinterStr     :='Konfigurieren Sie Drucker';
  LS_SaveToFileStr        :='Ballabwehr, um abzulegen';
  LS_SendToStr            :='Schicken Sie dazu';
  LS_PrinterStr           :='Drucker';
  LS_NameStr              :='Name';
  LS_PrintToFileStr       :='Drucker, um abzulegen';
  LS_PrintInBackgroundStr :='Druck in Hintergrund ';
  LS_SaveInBackground     :='Ballabwehr in Hintergrund';
  LS_PageRangeStr         :='Rufen Sie Auswahl aus';
  LS_RangeFromStr         :='von';
  LS_RangeToStr           :='zu';
  LS_AllStr               :='Alles';
  LS_PagesStr             :='Seiten';
  LS_SelectionStr         :='Auswahl';
  LS_CopiesStr            :='Kopien';
  LS_NumberOfCopiesStr    :='Anzahl von Kopien';
  LS_OkStr                :='Ok';
  LS_DivideScreenStr      :='Teilen Sie den Bildschirm';
  LS_InvalidNameStr       :='Ungültiger Name';
  LS_DuplicateNameStr     :='Nennen Sie schon im Gebrauch';
  LS_UseFilterStr         :='Verwendungsfilter';
  LS_WebPageStr           :='Webseite';
  LS_RichFormatStr        :='RichText Format';
  LS_PDFFormatStr         :='PDF Document';
  LS_XLSFormatStr         :='Zeichnen Sie Tabelle aus';
  LS_AtStr                :='bei';
  LS_FormStr              :='Form';
  LS_DefaultStr           :='Versäumnis';
  LS_ColsStr              :='cols';
  LS_ZoomInStr            :='Zunahmegummilinse';
  LS_ZoomOutStr           :='Abnahmengummilinse';
  LS_CopyStr              :='Kopie';
  LS_EditStr              :='Bearbeiten Sie';
  LS_FindCaptionStr       :='Fund';
  LS_TextToFindStr        :='Te&xt';
  LS_FindNextStr          :='Finden Sie danach';
  LS_WholeWordsStr        :='&Ganze Wörter nur';
  LS_MatchCaseStr         :='&Passen Sie Fall zusammen';
  LS_DirectionUpStr       :='&Auf';
  LS_DirectionDownStr     :='&Entlang';
  LS_DirectionCaptionStr  :='Richtung';
  Ls_GetOutlineTextMetrics:='GetOutlineTextMetrics scheiterte';
  Ls_Salvar_Como          :='Salbe als';
  Ls_Nome_Arquivo         :='Akte Name:';
  LS_OptionsStr           :='Möglichkeiten';
  LS_ColumnsStr           :='Säule';
  Ls_Progresso            :='Fortschritt';
  Ls_Aplicar              :='Bewerben Sie sich';
end;

procedure LoadRussianStrings;
begin
  LS_PrintingInProgressStr:='Печать...';
  LS_FilterInProgressStr  :='Сохранение отчета...';
  LS_PreparingReportStr   :='Подготовка отчета...';
  LS_PrinterNotFoundStr   :='Принтер не найден';
  LS_NoPathToPrinterStr   :='�?еправильный путь принтера';
  LS_LoadDefaultConfigStr :='Загрузить конфигураци�? по умолчанию';
  LS_PrinterDriverErrorStr:='Ошибка драйвера принтера';
  LS_PageStr              :='Страница';
  LS_PrepareErrorStr      :='Ошибка при подготовке отчета';
  LS_PageBreakStr         :='Продолжает�?�?...';
  LS_PageMendStr          :='Продолжение';
  LS_ReportEndStr         :='Конец';
  LS_FileNotFoundStr      :='Файл не найден';
  LS_FileNameStr          :='Им�? файла';
  LS_AllFileTypesStr      :='В�?е файлы';
  LS_LoadReportStr        :='Загрузить отчет';
  LS_NotFoundStr          :='�?е найден';
  LS_WaitStr              :='Подождите...';
  LS_FinishedStr          :='Завершено';
  LS_CancelStr            :='Отмена';
  LS_CloseStr             :='Закрыть';
  LS_SaveStr              :='Сохранить';
  LS_SendStr              :='Отправить';
  LS_PrintStr             :='Печать';
  LS_AboutTheStr          :='О Программе';
  LS_PreviewStr           :='Предварительный про�?мотр';
  LS_OfStr                :='из';
  LS_ZoomStr              :='Маштаб';
  LS_FirstPageStr         :='Перва�? �?траница';
  LS_PriorPageStr         :='Предыдуща�? �?траница';
  LS_NextPageStr          :='Следующа�? �?траница';
  LS_LastPageStr          :='По�?ледн�?�? �?траница';
  LS_EntirePageStr        :='В�?�? �?траница';
  LS_EntireWidthStr       :='В�?�? ширина';
  LS_MultiplePagesStr     :='�?е�?колько �?траниц';
  LS_ConfigPrinterStr     :='�?а�?тройка принтера';
  LS_SaveToFileStr        :='Сохранить в файл';
  LS_SendToStr            :='Отправить в';
  LS_PrinterStr           :='Принтер';
  LS_NameStr              :='Им�?';
  LS_PrintToFileStr       :='Печать в файл';
  LS_PrintInBackgroundStr :='Печать в фоне';
  LS_SaveInBackground     :='Сохранение в фоне';
  LS_PageRangeStr         :='Диапазон печати';
  LS_RangeFromStr         :='�?';
  LS_RangeToStr           :='по';
  LS_AllStr               :='В�?е';
  LS_PagesStr             :='Страницы';
  LS_SelectionStr         :='Выделение';
  LS_CopiesStr            :='Копии';
  LS_NumberOfCopiesStr    :='Кол-во копий';
  LS_OkStr                :='Хорошо';
  LS_DivideScreenStr      :='Разделить �?кран';
  LS_InvalidNameStr       :='�?еправильное им�?';
  LS_DuplicateNameStr     :='Им�? уже и�?пользует�?�?';
  LS_UseFilterStr         :='Фильтр';
  LS_WebPageStr           :='Веб �?траница';
  LS_RichFormatStr        :='RichText Format';
  LS_PDFFormatStr         :='PDF Документ';
  LS_XLSFormatStr         :='Таблица Excel';
  LS_AtStr                :='в';
  LS_FormStr              :='Форма';
  LS_DefaultStr           :='По умолчанию';
  LS_ColsStr              :='Столб.';
  LS_ZoomInStr            :='Увеличить';
  LS_ZoomOutStr           :='Уменьшить';
  LS_CopyStr              :='Копировать';
  LS_EditStr              :='Редактировать';
  LS_FindCaptionStr       :='�?айти';
  LS_TextToFindStr        :='Те&к�?т';
  LS_FindNextStr          :='�?айти &дальше';
  LS_WholeWordsStr        :='&Только �?лово целиком';
  LS_MatchCaseStr         :='&Учитывать реги�?тр';
  LS_DirectionUpStr       :='&Вверх';
  LS_DirectionDownStr     :='В&низ';
  LS_DirectionCaptionStr  :='�?аправление';
  Ls_GetOutlineTextMetrics:='GetOutlineTextMetrics �?ломало�?ь';
  Ls_Salvar_Como          :='Сохранить как';
  Ls_Nome_Arquivo         :='Им�? файла:';
  LS_OptionsStr           :='�?а�?тройки';
  LS_ColumnsStr           :='Столбцы';
  LS_Propriedades         :='�?а�?тройки';
  Ls_Progresso            :='Прогре�?�?';
  Ls_Aplicar              :='Применить';
  Ls_File_corrupted       :='Файл поврежден!';
  Ls_File_version         :='�?еправильна�? вер�?и�? файла!';
  Ls_Page_settings        :='�?а�?тройка �?траницы';
  Ls_Page_margins         :='Пол�?';
  Ls_Page_margins_top     :='Верх';
  Ls_Page_margins_bottom  :='�?их';
  Ls_Page_paper           :='Бумага';
  Ls_Paper_Size           :='Размер бумаги';
  Ls_Paper_Size_Width     :='Ширина';
  Ls_Paper_Size_Heigth    :='Вы�?ота';
  Ls_Page_margins_rigth   :='Право';
  Ls_Page_margins_left    :='Лево';
  Ls_Paper_Orientation    :='Ориентаци�?';
  Ls_Paper_Orientation_Portrait:= 'Портретна�?';
  Ls_Paper_Orientation_Landscape:= 'Ландшафтна�?';
end;

procedure DetectLocale;
{$ifdef LINUX}
var
  dlct:string;
{$endif}
begin
{$ifdef LINUX}
  dlct:=AnsiUpperCase(Copy(GetEnvironmentVariable('LANG'),1,2));
  if dlct='PT' then
    LoadPortugueseStrings
  else if dlct='EN' then
    LoadEnglishStrings
  else if dlct='ES' then
    LoadSpanishStrings
  else if dlct='FR' then
    LoadFrenchStrings
  else if dlct='IT' then
    LoadItalianStrings
  else if dlct='SW' then
    LoadSwedishStrings
  else if dlct='RU' then
    LoadRussianStrings
  else
    LoadEnglishStrings;
{$else}
  case SysLocale.PriLangID of
    $16 {LANG_PORTUGUESE}: LoadPortugueseStrings;
    $09 {LANG_ENGLISH}   : LoadEnglishStrings;
    $0a {LANG_SPANISH}   : LoadSpanishStrings;
    $0c {LANG_FRENCH}    : LoadFrenchStrings;
    $10 {LANG_ITALIAN}   : LoadItalianStrings;
    $1d {LANG_SWEDISH}   : LoadSwedishStrings;
    $19 {LANG_RUSSIAN}   : LoadRussianStrings;
  else
    LoadEnglishStrings;
  end;
{$endif}
end;

initialization
  DetectLocale;

end.


