{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fortes324forlaz; 

interface

uses
  RLReg, RLAbout, RLBarcode, RLConsts, RLDesign, RLDraftFilter, 
  RLDraftFilterDialog, RLFeedBack, RLFilters, RLFindDialog, RLHTMLFilter, 
  RLMetaFile, RLMetaVCL, RLPageSetupConfig, RLParser, RLPDFFilter, RLPreview, 
  RLPreviewForm, RLPrintDialog, RLPrinters, RLReport, RLRichFilter, 
  RLRichParsers, RLRichText, RLSaveDialog, RLSpoolFilter, RLTypes, RLUtils, 
  RLXLSFilter, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('RLReg', @RLReg.Register); 
end; 

initialization
  RegisterPackage('fortes324forlaz', @Register); 
end.
