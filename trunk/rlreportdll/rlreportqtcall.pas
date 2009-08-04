unit rlreportqtcall;

interface

implementation
{$IFDEF LINUX}
function QEvent_isQCustomEvent(Event: Pointer): Boolean; cdecl; external 'libborqt-6.9-qt2.3.so';
{$ENDIF}

Initialization
{$IFDEF LINUX}
if @QEvent_isQCustomEvent = nil then ;
{$ENDIF}

end.
 