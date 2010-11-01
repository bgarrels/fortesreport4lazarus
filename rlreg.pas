{$I RLReport.inc}

unit RLReg;
{$MODE DELPHI}
interface

uses
  Classes, PropEdits, ComponentEditors, LCLType, LResources,
  RLDesign, RLReport, RLDraftFilter, RLRichFilter, RLHTMLFilter, RLPDFFilter, RLParser,
  RLPreview, RLMetaFile, RLBarcode, RLRichText, RLPreviewForm, RLXLSFilter,
  RLPrintDialog, RLSaveDialog;

procedure Register;

implementation

procedure Register;
begin
  // componentes
  RegisterComponents('Fortes Report',[TRLReport,
                                      TRLBand,
                                      TRLDetailGrid,
                                      TRLGroup,
                                      TRLSubDetail,
                                      TRLLabel,
                                      TRLAngleLabel,
                                      TRLDBText,
                                      TRLMemo,
                                      TRLDBMemo,
                                      TRLRichText,
                                      TRLDBRichText,
                                      TRLImage,
                                      TRLDBImage,
                                      TRLSystemInfo,
                                      TRLDraw,
                                      TRLPanel,
                                      TRLDBResult,
                                      TRLBarcode,
                                      TRLDBBarcode,
                                      TRLPreview,
                                      TRLExpressionParser,
                                      TRLDraftFilter,
                                      TRLRichFilter,
                                      TRLHTMLFilter,
                                      TRLPDFFilter,
                                      TRLXLSFilter,
                                      TRLPreviewSetup,
                                      TRLGraphicStorage,
                                      TRLPrintDialogSetup,
                                      TRLSaveDialogSetup]);
  // editores de componentes
  RegisterComponentEditor(TRLReport,TRLReportDesigner);
  // editores de propriedades
  RegisterPropertyEditor(TypeInfo(TRLDataFieldProperty) ,nil           ,'DataField' ,TRLDataFieldEditor);
  RegisterPropertyEditor(TypeInfo(TRLDataFieldsProperty),TRLCustomGroup,'DataFields',TRLDataFieldsEditor);
end;

initialization
  {$I fortes324forlaz.lrs}

end.

