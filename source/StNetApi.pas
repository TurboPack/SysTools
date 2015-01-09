// Upgraded to Delphi 2009: Sebastian Zierer

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StNetAPI.pas 4.04                           *}
{*********************************************************}
{* SysTools: Network API Defines                         *}
{*********************************************************}

{$I StDefine.inc}

{$H+} {Huge strings}

unit StNetAPI;

interface

uses
  Windows, StBase;

{-------------------- LMERR.H------------------------------------------------}
const
  NERR_SUCCESS      = 0; {Success}

  { NERR_BASE is the base of error codes from network utilities, }
  { chosen to avoid conflict with system and redirector error codes. }
  { 2100 is a value that has been assigned to us by system. }

  NERR_BASE      = 2100;

  { INTERNAL_ONLY }

  { ***********WARNING **************** }
  { *See the comment in lmcons.h for  * }
  { *info on the allocation of errors * }
  { *********************************** }

  { ***********WARNING **************** }
  { *The range 2750-2799 has been     * }
  { *allocated to the IBM LAN Server  * }
  { *********************************** }

  { ***********WARNING **************** }
  { *The range 2900-2999 has been     * }
  { *reserved for Microsoft OEMs      * }
  { *********************************** }

  { END_INTERNAL }

  { UNUSED BASE+0 }
  { UNUSED BASE+1 }

  NERR_NetNotStarted      = (NERR_BASE+2);  { The workstation driver is not installed. }
  NERR_UnknownServer      = (NERR_BASE+3);  { The server could not be located. }
  NERR_ShareMem           = (NERR_BASE+4);  { An internal error occurred.  The network cannot access a shared memory segment. }

  NERR_NoNetworkResource  = (NERR_BASE+5);  { A network resource shortage occurred. }
  NERR_RemoteOnly         = (NERR_BASE+6);  { This operation is not supported on workstations. }
  NERR_DevNotRedirected   = (NERR_BASE+7);  { The device is not connected. }

  { UNUSED BASE+8 }
  { UNUSED BASE+9 }
  { UNUSED BASE+10 }
  { UNUSED BASE+11 }
  { UNUSED BASE+12 }
  { UNUSED BASE+13 }

  NERR_ServerNotStarted   = (NERR_BASE+14); { The Server service is not started. }
  NERR_ItemNotFound       = (NERR_BASE+15); { The queue is empty. }
  NERR_UnknownDevDir      = (NERR_BASE+16); { The device or directory does not exist. }
  NERR_RedirectedPath     = (NERR_BASE+17); { The operation is invalid on a redirected resource. }
  NERR_DuplicateShare     = (NERR_BASE+18); { The name has already been shared. }
  NERR_NoRoom             = (NERR_BASE+19); { The server is currently out of the requested resource. }

  { UNUSED BASE+20 }

  NERR_TooManyItems       = (NERR_BASE+21); { Requested addition of items exceeds the maximum allowed. */ }
  NERR_InvalidMaxUsers    = (NERR_BASE+22); { The Peer service supports only two simultaneous users. */ }
  NERR_BufTooSmall        = (NERR_BASE+23); { The API return buffer is too small. }

  { UNUSED BASE+24 }
  { UNUSED BASE+25 }
  { UNUSED BASE+26 }

  NERR_RemoteErr          = (NERR_BASE+27); { A remote API error occurred. }

  { UNUSED BASE+28 }
  { UNUSED BASE+29 }
  { UNUSED BASE+30 }

  NERR_LanmanIniError     = (NERR_BASE+31); { An error occurred when opening or reading the configuration file. }

  { UNUSED BASE+32 }
  { UNUSED BASE+33 }
  { UNUSED BASE+34 }
  { UNUSED BASE+35 }

  NERR_NetworkError       = (NERR_BASE+36); { A general network error occurred. }
  NERR_WkstaInconsistentState = (NERR_BASE+37); { The Workstation service is in an inconsistent state. Restart the computer before restarting the Workstation service. }
  NERR_WkstaNotStarted    = (NERR_BASE+38); { The Workstation service has not been started. }
  NERR_BrowserNotStarted  = (NERR_BASE+39); { The requested information is not available. }
  NERR_InternalError      = (NERR_BASE+40); { An internal Windows NT error occurred. }
  NERR_BadTransactConfig  = (NERR_BASE+41); { The server is not configured for transactions. }
  NERR_InvalidAPI         = (NERR_BASE+42); { The requested API is not supported on the remote server. }
  NERR_BadEventName       = (NERR_BASE+43); { The event name is invalid. }
  NERR_DupNameReboot      = (NERR_BASE+44); { The computer name already exists on the network. Change it and restart the computer. }

  { Config API related (Error codes from BASE+45 to BASE+49) }

  { UNUSED BASE+45 }

  NERR_CfgCompNotFound    = (NERR_BASE+46); { The specified component could not be found in the configuration information. }
  NERR_CfgParamNotFound   = (NERR_BASE+47); { The specified parameter could not be found in the configuration information. }
  NERR_LineTooLong        = (NERR_BASE+49); { A line in the configuration file is too long. }

  { Spooler API related (Error codes from BASE+50 to BASE+79) }

  NERR_QNotFound          = (NERR_BASE+50); { The printer does not exist. }
  NERR_JobNotFound        = (NERR_BASE+51); { The print job does not exist. }
  NERR_DestNotFound       = (NERR_BASE+52); { The printer destination cannot be found. }
  NERR_DestExists         = (NERR_BASE+53); { The printer destination already exists. }
  NERR_QExists            = (NERR_BASE+54); { The printer queue already exists. }
  NERR_QNoRoom            = (NERR_BASE+55); { No more printers can be added. }
  NERR_JobNoRoom          = (NERR_BASE+56); { No more print jobs can be added. }
  NERR_DestNoRoom         = (NERR_BASE+57); { No more printer destinations can be added. }
  NERR_DestIdle           = (NERR_BASE+58); { This printer destination is idle and cannot accept control operations. }
  NERR_DestInvalidOp      = (NERR_BASE+59); { This printer destination request contains an invalid control function. }
  NERR_ProcNoRespond      = (NERR_BASE+60); { The print processor is not responding. }
  NERR_SpoolerNotLoaded   = (NERR_BASE+61); { The spooler is not running. }
  NERR_DestInvalidState   = (NERR_BASE+62); { This operation cannot be performed on the print destination in its current state. }
  NERR_QInvalidState      = (NERR_BASE+63); { This operation cannot be performed on the printer queue in its current state. }
  NERR_JobInvalidState    = (NERR_BASE+64); { This operation cannot be performed on the print job in its current state. }
  NERR_SpoolNoMemory      = (NERR_BASE+65); { A spooler memory allocation failure occurred. }
  NERR_DriverNotFound     = (NERR_BASE+66); { The device driver does not exist. }
  NERR_DataTypeInvalid    = (NERR_BASE+67); { The data type is not supported by the print processor. }
  NERR_ProcNotFound       = (NERR_BASE+68); { The print processor is not installed. }

  { Service API related (Error codes from BASE+80 to BASE+99) }

  NERR_ServiceTableLocked = (NERR_BASE+80); { The service database is locked. }
  NERR_ServiceTableFull   = (NERR_BASE+81); { The service table is full. }
  NERR_ServiceInstalled   = (NERR_BASE+82); { The requested service has already been started. }
  NERR_ServiceEntryLocked = (NERR_BASE+83); { The service does not respond to control actions. }
  NERR_ServiceNotInstalled= (NERR_BASE+84); { The service has not been started. }
  NERR_BadServiceName     = (NERR_BASE+85); { The service name is invalid. }
  NERR_ServiceCtlTimeout  = (NERR_BASE+86); { The service is not responding to the control function. }
  NERR_ServiceCtlBusy     = (NERR_BASE+87); { The service control is busy. }
  NERR_BadServiceProgName = (NERR_BASE+88); { The configuration file contains an invalid service program name. }
  NERR_ServiceNotCtrl     = (NERR_BASE+89); { The service could not be controlled in its present state. }
  NERR_ServiceKillProc    = (NERR_BASE+90); { The service ended abnormally. }
  NERR_ServiceCtlNotValid = (NERR_BASE+91); { The requested pause or stop is not valid for this service. }
  NERR_NotInDispatchTbl   = (NERR_BASE+92); { The service control dispatcher could not find the service name in the dispatch table. }
  NERR_BadControlRecv     = (NERR_BASE+93); { The service control dispatcher pipe read failed. }
  NERR_ServiceNotStarting = (NERR_BASE+94); { A thread for the new service could not be created. }

  { Wksta and Logon API related (Error codes from BASE+100 to BASE+118) }

  NERR_AlreadyLoggedOn    = (NERR_BASE+100); { This workstation is already logged on to the local-area network. }
  NERR_NotLoggedOn        = (NERR_BASE+101); { The workstation is not logged on to the local-area network. }
  NERR_BadUsername        = (NERR_BASE+102); { The user name or group name parameter is invalid. }
  NERR_BadPassword        = (NERR_BASE+103); { The password parameter is invalid. }
  NERR_UnableToAddName_W  = (NERR_BASE+104); { @W The logon processor did not add the message alias. }
  NERR_UnableToAddName_F  = (NERR_BASE+105); { The logon processor did not add the message alias. }
  NERR_UnableToDelName_W  = (NERR_BASE+106); { @W The logoff processor did not delete the message alias. }
  NERR_UnableToDelName_F  = (NERR_BASE+107); { The logoff processor did not delete the message alias. }

  { UNUSED BASE+108 }

  NERR_LogonsPaused       = (NERR_BASE+109); { Network logons are paused. }
  NERR_LogonServerConflict= (NERR_BASE+110); { A centralized logon-server conflict occurred. }
  NERR_LogonNoUserPath    = (NERR_BASE+111); { The server is configured without a valid user path. }
  NERR_LogonScriptError   = (NERR_BASE+112); { An error occurred while loading or running the logon script. }

  { UNUSED BASE+113 }

  NERR_StandaloneLogon    = (NERR_BASE+114); { The logon server was not specified.  Your computer will be logged on as STANDALONE. }
  NERR_LogonServerNotFound= (NERR_BASE+115); { The logon server could not be found. }
  NERR_LogonDomainExists  = (NERR_BASE+116); { There is already a logon domain for this computer. }
  NERR_NonValidatedLogon  = (NERR_BASE+117); { The logon server could not validate the logon. }

  { ACF API related (access, user, group) (Error codes from BASE+119 to BASE+149) }

  NERR_ACFNotFound        = (NERR_BASE+119); { The security database could not be found. }
  NERR_GroupNotFound      = (NERR_BASE+120); { The group name could not be found. }
  NERR_UserNotFound       = (NERR_BASE+121); { The user name could not be found. }
  NERR_ResourceNotFound   = (NERR_BASE+122); { The resource name could not be found. }
  NERR_GroupExists        = (NERR_BASE+123); { The group already exists. }
  NERR_UserExists         = (NERR_BASE+124); { The user account already exists. }
  NERR_ResourceExists     = (NERR_BASE+125); { The resource permission list already exists. }
  NERR_NotPrimary         = (NERR_BASE+126); { This operation is only allowed on the primary domain controller of the domain. }
  NERR_ACFNotLoaded       = (NERR_BASE+127); { The security database has not been started. }
  NERR_ACFNoRoom          = (NERR_BASE+128); { There are too many names in the user accounts database. }
  NERR_ACFFileIOFail      = (NERR_BASE+129); { A disk I/O failure occurred. }
  NERR_ACFTooManyLists    = (NERR_BASE+130); { The limit of 64 entries per resource was exceeded. }
  NERR_UserLogon          = (NERR_BASE+131); { Deleting a user with a session is not allowed. }
  NERR_ACFNoParent        = (NERR_BASE+132); { The parent directory could not be located. }
  NERR_CanNotGrowSegment  = (NERR_BASE+133); { Unable to add to the security database session cache segment. }
  NERR_SpeGroupOp         = (NERR_BASE+134); { This operation is not allowed on this special group. }
  NERR_NotInCache         = (NERR_BASE+135); { This user is not cached in user accounts database session cache. }
  NERR_UserInGroup        = (NERR_BASE+136); { The user already belongs to this group. }
  NERR_UserNotInGroup     = (NERR_BASE+137); { The user does not belong to this group. }
  NERR_AccountUndefined   = (NERR_BASE+138); { This user account is undefined. }
  NERR_AccountExpired     = (NERR_BASE+139); { This user account has expired. }
  NERR_InvalidWorkstation = (NERR_BASE+140); { The user is not allowed to log on from this workstation. }
  NERR_InvalidLogonHours  = (NERR_BASE+141); { The user is not allowed to log on at this time. }
  NERR_PasswordExpired    = (NERR_BASE+142); { The password of this user has expired. }
  NERR_PasswordCantChange = (NERR_BASE+143); { The password of this user cannot change. }
  NERR_PasswordHistConflict=(NERR_BASE+144); { This password cannot be used now. }
  NERR_PasswordTooShort   = (NERR_BASE+145); { The password is shorter than required. }
  NERR_PasswordTooRecent  = (NERR_BASE+146); { The password of this user is too recent to change. }
  NERR_InvalidDatabase    = (NERR_BASE+147); { The security database is corrupted. }
  NERR_DatabaseUpToDate   = (NERR_BASE+148); { No updates are necessary to this replicant network/local security database. }
  NERR_SyncRequired       = (NERR_BASE+149); { This replicant database is outdated; synchronization is required. }

  { Use API related (Error codes from BASE+150 to BASE+169) }

  NERR_UseNotFound        = (NERR_BASE+150); { The network connection could not be found. }
  NERR_BadAsgType         = (NERR_BASE+151); { This asg_type is invalid. }
  NERR_DeviceIsShared     = (NERR_BASE+152); { This device is currently being shared. }

  { Message Server related (Error codes BASE+170 to BASE+209) }

  NERR_NoComputerName     = (NERR_BASE+170); { The computer name could not be added as a message alias.  The name may already exist on the network. }
  NERR_MsgAlreadyStarted  = (NERR_BASE+171); { The Messenger service is already started. }
  NERR_MsgInitFailed      = (NERR_BASE+172); { The Messenger service failed to start. }
  NERR_NameNotFound       = (NERR_BASE+173); { The message alias could not be found on the network. }
  NERR_AlreadyForwarded   = (NERR_BASE+174); { This message alias has already been forwarded. }
  NERR_AddForwarded       = (NERR_BASE+175); { This message alias has been added but is still forwarded. }
  NERR_AlreadyExists      = (NERR_BASE+176); { This message alias already exists locally. }
  NERR_TooManyNames       = (NERR_BASE+177); { The maximum number of added message aliases has been exceeded. }
  NERR_DelComputerName    = (NERR_BASE+178); { The computer name could not be deleted. }
  NERR_LocalForward       = (NERR_BASE+179); { Messages cannot be forwarded back to the same workstation. }
  NERR_GrpMsgProcessor    = (NERR_BASE+180); { An error occurred in the domain message processor. }
  NERR_PausedRemote       = (NERR_BASE+181); { The message was sent, but the recipient has paused the Messenger service. }
  NERR_BadReceive         = (NERR_BASE+182); { The message was sent but not received. }
  NERR_NameInUse          = (NERR_BASE+183); { The message alias is currently in use. Try again later. }
  NERR_MsgNotStarted      = (NERR_BASE+184); { The Messenger service has not been started. }
  NERR_NotLocalName       = (NERR_BASE+185); { The name is not on the local computer. }
  NERR_NoForwardName      = (NERR_BASE+186); { The forwarded message alias could not be found on the network. }
  NERR_RemoteFull         = (NERR_BASE+187); { The message alias table on the remote station is full. }
  NERR_NameNotForwarded   = (NERR_BASE+188); { Messages for this alias are not currently being forwarded. }
  NERR_TruncatedBroadcast = (NERR_BASE+189); { The broadcast message was truncated. }
  NERR_InvalidDevice      = (NERR_BASE+194); { This is an invalid device name. }
  NERR_WriteFault         = (NERR_BASE+195); { A write fault occurred. }

  { UNUSED BASE+196 }

  NERR_DuplicateName      = (NERR_BASE+197); { A duplicate message alias exists on the network. }
  NERR_DeleteLater        = (NERR_BASE+198); { @W This message alias will be deleted later. }
  NERR_IncompleteDel      = (NERR_BASE+199); { The message alias was not successfully deleted from all networks. }
  NERR_MultipleNets       = (NERR_BASE+200); { This operation is not supported on computers with multiple networks. }

  { Server API related (Error codes BASE+210 to BASE+229) }

  NERR_NetNameNotFound    = (NERR_BASE+210); { This shared resource does not exist. }
  NERR_DeviceNotShared    = (NERR_BASE+211); { This device is not shared. }
  NERR_ClientNameNotFound = (NERR_BASE+212); { A session does not exist with that computer name. }
  NERR_FileIdNotFound     = (NERR_BASE+214); { There is not an open file with that identification number. }
  NERR_ExecFailure        = (NERR_BASE+215); { A failure occurred when executing a remote administration command. }
  NERR_TmpFile            = (NERR_BASE+216); { A failure occurred when opening a remote temporary file. }
  NERR_TooMuchData        = (NERR_BASE+217); { The data returned from a remote administration command has been truncated to 64K. }
  NERR_DeviceShareConflict= (NERR_BASE+218); { This device cannot be shared as both a spooled and a non-spooled resource. }
  NERR_BrowserTableIncomplete= (NERR_BASE+219); { The information in the list of servers may be incorrect. }
  NERR_NotLocalDomain     = (NERR_BASE+220); { The computer is not active in this domain. }

  { CharDev API related (Error codes BASE+230 to BASE+249) }

  { UNUSED BASE+230 }

  NERR_DevInvalidOpCode   = (NERR_BASE+231); { The operation is invalid for this device. }
  NERR_DevNotFound        = (NERR_BASE+232); { This device cannot be shared. }
  NERR_DevNotOpen         = (NERR_BASE+233); { This device was not open. }
  NERR_BadQueueDevString  = (NERR_BASE+234); { This device name list is invalid. }
  NERR_BadQueuePriority   = (NERR_BASE+235); { The queue priority is invalid. }
  NERR_NoCommDevs         = (NERR_BASE+237); { There are no shared communication devices. }
  NERR_QueueNotFound      = (NERR_BASE+238); { The queue you specified does not exist. }
  NERR_BadDevString       = (NERR_BASE+240); { This list of devices is invalid. }
  NERR_BadDev             = (NERR_BASE+241); { The requested device is invalid. }
  NERR_InUseBySpooler     = (NERR_BASE+242); { This device is already in use by the spooler. }
  NERR_CommDevInUse       = (NERR_BASE+243); { This device is already in use as a communication device. }

  { NetICanonicalize and NetIType and NetIMakeLMFileName }
  { NetIListCanon and NetINameCheck }
  { (Error codes BASE+250 to BASE+269) }

  NERR_InvalidComputer    = (NERR_BASE+251); { This computer name is invalid. }

  { UNUSED BASE+252 }
  { UNUSED BASE+253 }

  NERR_MaxLenExceeded     = (NERR_BASE+254); { The string and prefix specified are too long. }

  { UNUSED BASE+255 }

  NERR_BadComponent       = (NERR_BASE+256); { This path component is invalid. }
  NERR_CantType           = (NERR_BASE+257); { Could not determine the type of input. }

  { UNUSED BASE+258 }
  { UNUSED BASE+259 }

  NERR_TooManyEntries     = (NERR_BASE+262); { The buffer for types is not big enough. }

  { NetProfile (Error codes BASE+270 to BASE+276) }

  NERR_ProfileFileTooBig  = (NERR_BASE+270); { Profile files cannot exceed 64K. }
  NERR_ProfileOffset      = (NERR_BASE+271); { The start offset is out of range. }
  NERR_ProfileCleanup     = (NERR_BASE+272); { The system cannot delete current connections to network resources. }
  NERR_ProfileUnknownCmd  = (NERR_BASE+273); { The system was unable to parse the command line in this file. }
  NERR_ProfileLoadErr     = (NERR_BASE+274); { An error occurred while loading the profile file. }
  NERR_ProfileSaveErr     = (NERR_BASE+275); { @W Errors occurred while saving the profile file.  The profile was partially saved. }

  { NetAudit and NetErrorLog (Error codes BASE+277 to BASE+279) }

  NERR_LogOverflow        = (NERR_BASE+277); { Log file %1 is full. }
  NERR_LogFileChanged     = (NERR_BASE+278); { This log file has changed between reads. }
  NERR_LogFileCorrupt     = (NERR_BASE+279); { Log file %1 is corrupt. }

  { NetRemote (Error codes BASE+280 to BASE+299) }

  NERR_SourceIsDir        = (NERR_BASE+280); { The source path cannot be a directory. }
  NERR_BadSource          = (NERR_BASE+281); { The source path is illegal. }
  NERR_BadDest            = (NERR_BASE+282); { The destination path is illegal. }
  NERR_DifferentServers   = (NERR_BASE+283); { The source and destination paths are on different servers. }

  { UNUSED BASE+284 }

  NERR_RunSrvPaused       = (NERR_BASE+285); { The Run server you requested is paused. }

  { UNUSED BASE+286 }
  { UNUSED BASE+287 }
  { UNUSED BASE+288 }

  NERR_ErrCommRunSrv      = (NERR_BASE+289); { An error occurred when communicating with a Run server. }

  { UNUSED BASE+290 }

  NERR_ErrorExecingGhost  = (NERR_BASE+291); { An error occurred when starting a background process. }
  NERR_ShareNotFound      = (NERR_BASE+292); { The shared resource you are connected to could not be found. }

  { UNUSED BASE+293 }
  { UNUSED BASE+294 }

  { NetWksta.sys (redir) returned error codes. (NERR_BASE + (300-329)) }

  NERR_InvalidLana        = (NERR_BASE+300); { The LAN adapter number is invalid. }
  NERR_OpenFiles          = (NERR_BASE+301); { There are open files on the connection. }
  NERR_ActiveConns        = (NERR_BASE+302); { Active connections still exist. }
  NERR_BadPasswordCore    = (NERR_BASE+303); { This share name or password is invalid. }
  NERR_DevInUse           = (NERR_BASE+304); { The device is being accessed by an active process. }
  NERR_LocalDrive         = (NERR_BASE+305); { The drive letter is in use locally. }

  { Alert error codes. (NERR_BASE + (330-339)) }

  NERR_AlertExists        = (NERR_BASE+330); { The specified client is already registered for the specified event. }
  NERR_TooManyAlerts      = (NERR_BASE+331); { The alert table is full. }
  NERR_NoSuchAlert        = (NERR_BASE+332); { An invalid or nonexistent alert name was raised. }
  NERR_BadRecipient       = (NERR_BASE+333); { The alert recipient is invalid. }
  NERR_AcctLimitExceeded  = (NERR_BASE+334); { A user's session with this server has been deleted }
                                             { because the user's logon hours are no longer valid. }

  { Additional Error and Audit log codes. (NERR_BASE +(340-343)) }

  NERR_InvalidLogSeek     = (NERR_BASE+340); { The log file does not contain the requested record number. }

  { UNUSED BASE+341 }
  { UNUSED BASE+342 }
  { UNUSED BASE+343 }

  { Additional UAS and NETLOGON codes (NERR_BASE +(350-359)) }

  NERR_BadUasConfig       = (NERR_BASE+350); { The user accounts database is not configured correctly. }
  NERR_InvalidUASOp       = (NERR_BASE+351); { This operation is not permitted when the Netlogon service is running. }
  NERR_LastAdmin          = (NERR_BASE+352); { This operation is not allowed on the last administrative account. }
  NERR_DCNotFound         = (NERR_BASE+353); { Could not find domain controller for this domain. }
  NERR_LogonTrackingError = (NERR_BASE+354); { Could not set logon information for this user. }
  NERR_NetlogonNotStarted = (NERR_BASE+355); { The Netlogon service has not been started. }
  NERR_CanNotGrowUASFile  = (NERR_BASE+356); { Unable to add to the user accounts database. }
  NERR_TimeDiffAtDC       = (NERR_BASE+357); { This server's clock is not synchronized with the primary domain controller's clock. }
  NERR_PasswordMismatch   = (NERR_BASE+358); { A password mismatch has been detected. }

  { Server Integration error codes. (NERR_BASE +(360-369)) }

  NERR_NoSuchServer       = (NERR_BASE+360); { The server identification does not specify a valid server. }
  NERR_NoSuchSession      = (NERR_BASE+361); { The session identification does not specify a valid session. }
  NERR_NoSuchConnection   = (NERR_BASE+362); { The connection identification does not specify a valid connection. }
  NERR_TooManyServers     = (NERR_BASE+363); { There is no space for another entry in the table of available servers. }
  NERR_TooManySessions    = (NERR_BASE+364); { The server has reached the maximum number of sessions it supports. }
  NERR_TooManyConnections = (NERR_BASE+365); { The server has reached the maximum number of connections it supports. }
  NERR_TooManyFiles       = (NERR_BASE+366); { The server cannot open more files because it has reached its maximum number. }
  NERR_NoAlternateServers = (NERR_BASE+367); { There are no alternate servers registered on this server. }

  { UNUSED BASE+368 }
  { UNUSED BASE+369 }

  NERR_TryDownLevel       = (NERR_BASE+370); { Try down-level (remote admin protocol) version of API instead. }

  { UPS error codes. (NERR_BASE + (380-384)) }

  NERR_UPSDriverNotStarted= (NERR_BASE+380); { The UPS driver could not be accessed by the UPS service. }
  NERR_UPSInvalidConfig   = (NERR_BASE+381); { The UPS service is not configured correctly. }
  NERR_UPSInvalidCommPort = (NERR_BASE+382); { The UPS service could not access the specified Comm Port. }
  NERR_UPSSignalAsserted  = (NERR_BASE+383); { The UPS indicated a line fail or low battery situation. Service not started. }
  NERR_UPSShutdownFailed  = (NERR_BASE+384); { The UPS service failed to perform a system shut down. }

  { Remoteboot error codes. (NERR_BASE + (400-419)) }
  {   Error codes 400 - 405 are used by RPLBOOT.SYS. }
  {   Error codes 403, 407 - 416 are used by RPLLOADR.COM, }
  {   Error code 417 is the alerter message of REMOTEBOOT (RPLSERVR.EXE). }
  {   Error code 418 is for when REMOTEBOOT can't start }
  {   Error code 419 is for a disallowed 2nd rpl connection }

  NERR_BadDosRetCode      = (NERR_BASE+400); { The program below returned an MS-DOS error code: }
  NERR_ProgNeedsExtraMem  = (NERR_BASE+401); { The program below needs more memory: }
  NERR_BadDosFunction     = (NERR_BASE+402); { The program below called an unsupported MS-DOS function: }
  NERR_RemoteBootFailed   = (NERR_BASE+403); { The workstation failed to boot. }
  NERR_BadFileCheckSum    = (NERR_BASE+404); { The file below is corrupt. }
  NERR_NoRplBootSystem    = (NERR_BASE+405); { No loader is specified in the boot-block definition file. }
  NERR_RplLoadrNetBiosErr = (NERR_BASE+406); { NetBIOS returned an error: The NCB and SMB are dumped above. }
  NERR_RplLoadrDiskErr    = (NERR_BASE+407); { A disk I/O error occurred. }
  NERR_ImageParamErr      = (NERR_BASE+408); { Image parameter substitution failed. }
  NERR_TooManyImageParams = (NERR_BASE+409); { Too many image parameters cross disk sector boundaries. }
  NERR_NonDosFloppyUsed   = (NERR_BASE+410); { The image was not generated from an MS-DOS diskette formatted with /S. }
  NERR_RplBootRestart     = (NERR_BASE+411); { Remote boot will be restarted later. }
  NERR_RplSrvrCallFailed  = (NERR_BASE+412); { The call to the Remoteboot server failed. }
  NERR_CantConnectRplSrvr = (NERR_BASE+413); { Cannot connect to the Remoteboot server. }
  NERR_CantOpenImageFile  = (NERR_BASE+414); { Cannot open image file on the Remoteboot server. }
  NERR_CallingRplSrvr     = (NERR_BASE+415); { Connecting to the Remoteboot server... }
  NERR_StartingRplBoot    = (NERR_BASE+416); { Connecting to the Remoteboot server... }
  NERR_RplBootServiceTerm = (NERR_BASE+417); { Remote boot service was stopped; check the error log for the cause of the problem. }
  NERR_RplBootStartFailed = (NERR_BASE+418); { Remote boot startup failed; check the error log for the cause of the problem. }
  NERR_RPL_CONNECTED      = (NERR_BASE+419); { A second connection to a Remoteboot resource is not allowed. }

  { FTADMIN API error codes (NERR_BASE + (425-434)) }
  {   (Currently not used in NT) }

  { Browser service API error codes (NERR_BASE + (450-475)) }

  NERR_BrowserConfiguredToNotRun= (NERR_BASE+450); { * The browser service was configured with MaintainServerList=No. }

  { Additional Remoteboot error codes. (NERR_BASE + (510-550)) }

  NERR_RplNoAdaptersStarted      = (NERR_BASE+510); { Service failed to start since none of the network adapters started with this service. }
  NERR_RplBadRegistry            = (NERR_BASE+511); { Service failed to start due to bad startup information in the registry. }
  NERR_RplBadDatabase            = (NERR_BASE+512); { Service failed to start because its database is absent or corrupt. }
  NERR_RplRplfilesShare          = (NERR_BASE+513); { Service failed to start because RPLFILES share is absent. }
  NERR_RplNotRplServer           = (NERR_BASE+514); { Service failed to start because RPLUSER group is absent. }
  NERR_RplCannotEnum             = (NERR_BASE+515); { Cannot enumerate service records. }
  NERR_RplWkstaInfoCorrupted     = (NERR_BASE+516); { Workstation record information has been corrupted. }
  NERR_RplWkstaNotFound          = (NERR_BASE+517); { Workstation record was not found. }
  NERR_RplWkstaNameUnavailable   = (NERR_BASE+518); { Workstation name is in use by some other workstation. }
  NERR_RplProfileInfoCorrupted   = (NERR_BASE+519); { Profile record information has been corrupted. }
  NERR_RplProfileNotFound        = (NERR_BASE+520); { Profile record was not found. }
  NERR_RplProfileNameUnavailable = (NERR_BASE+521); { Profile name is in use by some other profile. }
  NERR_RplProfileNotEmpty        = (NERR_BASE+522); { There are workstations using this profile. }
  NERR_RplConfigInfoCorrupted    = (NERR_BASE+523); { Configuration record information has been corrupted. }
  NERR_RplConfigNotFound         = (NERR_BASE+524); { Configuration record was not found. }
  NERR_RplAdapterInfoCorrupted   = (NERR_BASE+525); { Adapter id record information has been corrupted. }
  NERR_RplInternal               = (NERR_BASE+526); { An internal service error has occured. }
  NERR_RplVendorInfoCorrupted    = (NERR_BASE+527); { Vendor id record information has been corrupted. }
  NERR_RplBootInfoCorrupted      = (NERR_BASE+528); { Boot block record information has been corrupted. }
  NERR_RplWkstaNeedsUserAcct     = (NERR_BASE+529); { The user account for this workstation record is missing. }
  NERR_RplNeedsRPLUSERAcct       = (NERR_BASE+530); { The RPLUSER local group could not be found. }
  NERR_RplBootNotFound           = (NERR_BASE+531); { Boot block record was not found. }
  NERR_RplIncompatibleProfile    = (NERR_BASE+532); { Chosen profile is incompatible with this workstation. }
  NERR_RplAdapterNameUnavailable = (NERR_BASE+533); { Chosen network adapter id is in use by some other workstation. }
  NERR_RplConfigNotEmpty         = (NERR_BASE+534); { There are profiles using this configuration. }
  NERR_RplBootInUse              = (NERR_BASE+535); { There are workstations, profiles or configurations using this boot block. }
  NERR_RplBackupDatabase         = (NERR_BASE+536); { Service failed to backup remoteboot database. }
  NERR_RplAdapterNotFound        = (NERR_BASE+537); { Adapter record was not found. }
  NERR_RplVendorNotFound         = (NERR_BASE+538); { Vendor record was not found. }
  NERR_RplVendorNameUnavailable  = (NERR_BASE+539); { Vendor name is in use by some other vendor record. }
  NERR_RplBootNameUnavailable    = (NERR_BASE+540); { (boot name, vendor id) is in use by some other boot block record. }
  NERR_RplConfigNameUnavailable  = (NERR_BASE+541); { Configuration name is in use by some other configuration. }

  { INTERNAL_ONLY }

  { Dfs API error codes. (NERR_BASE + (560-590)) }

  NERR_DfsInternalCorruption     = (NERR_BASE+560); { The internal database maintained by the Dfs service is corrupt }
  NERR_DfsVolumeDataCorrupt      = (NERR_BASE+561); { One of the records in the internal Dfs database is corrupt }
  NERR_DfsNoSuchVolume           = (NERR_BASE+562); { There is no volume whose entry path matches the input Entry Path }
  NERR_DfsVolumeAlreadyExists    = (NERR_BASE+563); { A volume with the given name already exists }
  NERR_DfsAlreadyShared          = (NERR_BASE+564); { The server share specified is already shared in the Dfs }
  NERR_DfsNoSuchShare            = (NERR_BASE+565); { The indicated server share does not support the indicated Dfs volume }
  NERR_DfsNotALeafVolume         = (NERR_BASE+566); { The operation is not valid on a non-leaf volume }
  NERR_DfsLeafVolume             = (NERR_BASE+567); { The operation is not valid on a leaf volume }
  NERR_DfsVolumeHasMultipleServers = (NERR_BASE+568); { The operation is ambiguous because the volume has multiple servers }
  NERR_DfsCantCreateJunctionPoint= (NERR_BASE+569); { Unable to create a junction point }
  NERR_DfsServerNotDfsAware      = (NERR_BASE+570); { The server is not Dfs Aware }
  NERR_DfsBadRenamePath          = (NERR_BASE+571); { The specified rename target path is invalid }
  NERR_DfsVolumeIsOffline        = (NERR_BASE+572); { The specified Dfs volume is offline }
  NERR_DfsNoSuchServer           = (NERR_BASE+573); { The specified server is not a server for this volume }
  NERR_DfsCyclicalName           = (NERR_BASE+574); { A cycle in the Dfs name was detected }
  NERR_DfsNotSupportedInServerDfs= (NERR_BASE+575); { The operation is not supported on a server-based Dfs }
  NERR_DfsInternalError          = (NERR_BASE+590); { Dfs internal error }

  { ***********WARNING **************** }
  { *The range 2750-2799 has been     * }
  { *allocated to the IBM LAN Server  * }
  { *********************************** }

  { ***********WARNING **************** }
  { *The range 2900-2999 has been     * }
  { *reserved for Microsoft OEMs      * }
  { *********************************** }

  { END_INTERNAL* }

  MAX_NERR                       = (NERR_BASE+899); { This is the last error in NERR range. }

  { WARNING:  Do not exceed MAX_NERR; values above this are used by }
  {           other error code ranges (errlog.h, service.h, apperr.h). }


{-------------------- LMCONS.H ----------------------------------------------}
const
  CNLEN       = 15;                  { Computer name length }
  LM20_CNLEN  = 15;                  { LM 2.0 Computer name length }
  DNLEN       = CNLEN;               { Maximum domain name length }
  LM20_DNLEN  = LM20_CNLEN;          { LM 2.0 Maximum domain name length }

  UNCLEN      = (CNLEN+2);           { UNC computer name length }
  LM20_UNCLEN = (LM20_CNLEN+2);      { LM 2.0 UNC computer name length }

  NNLEN       = 80;                  { Net name length (share name) }
  LM20_NNLEN  = 12;                  { LM 2.0 Net name length }

  RMLEN       = (UNCLEN+1+NNLEN);    { Max remote name length }
  LM20_RMLEN  = (LM20_UNCLEN+1+LM20_NNLEN); { LM 2.0 Max remote name length }

  SNLEN       = 80;                  { Service name length }
  LM20_SNLEN  = 15;                  { LM 2.0 Service name length }
  STXTLEN     = 256;                 { Service text length }
  LM20_STXTLEN= 63;                  { LM 2.0 Service text length }

  PATHLEN     = 256;                 { Max. path (not including drive name) }
  LM20_PATHLEN= 256;                 { LM 2.0 Max. path }

  DEVLEN      = 80;                  { Device name length }
  LM20_DEVLEN = 8;                   { LM 2.0 Device name length }

  EVLEN       = 16;                  { Event name length }


{ User, Group and Password lengths }


  UNLEN       = 256;                 { Maximum user name length }
  LM20_UNLEN  = 20;                  { LM 2.0 Maximum user name length }

  GNLEN       = UNLEN;               { Group name }
  LM20_GNLEN  = LM20_UNLEN;          { LM 2.0 Group name }

  PWLEN       = 256;                 { Maximum password length }
  LM20_PWLEN  = 14;                  { LM 2.0 Maximum password length }

  SHPWLEN     = 8;                   { Share password length (bytes) }

  CLTYPE_LEN  = 12;                  { Length of client type string }

  MAXCOMMENTSZ = 256;                { Multipurpose comment length }
  LM20_MAXCOMMENTSZ = 48;            { LM 2.0 Multipurpose comment length }

  QNLEN      =  NNLEN;               { Queue name maximum length }
  LM20_QNLEN  = LM20_NNLEN;          { LM 2.0 Queue name maximum length }

  { The ALERTSZ and MAXDEVENTRIES defines have not yet been NT'ized. }
  { Whoever ports these components should change these values appropriately. }

   ALERTSZ       = 128;              { size of alert string in server }
   MAXDEVENTRIES = SizeOf(Integer)*8;{ Max number of device entries }

   NETBIOS_NAME_LEN  = 16;           { NetBIOS net name (bytes) }

  { Value to be used with APIs which have a "preferred maximum length" }
  { parameter.  This value indicates that the API should just allocate }
  { "as much as it takes." }

  MAX_PREFERRED_LENGTH    = DWord(-1);

  { Constants used with encryption }

  CRYPT_KEY_LEN           = 7;
  CRYPT_TXT_LEN           = 8;
  ENCRYPTED_PWLEN         = 16;
  SESSION_PWLEN           = 24;
  SESSION_CRYPT_KLEN      = 21;

  {  Value to be used with SetInfo calls to allow setting of all }
  {  settable parameters (parmnum zero option) }
  PARMNUM_ALL             = 0;

  PARM_ERROR_UNKNOWN      = DWord(-1);
  PARM_ERROR_NONE         = 0;
  PARMNUM_BASE_INFOLEVEL  = 1000;

  { Message File Names }

  MESSAGE_FILENAME        = 'NETMSG';
  OS2MSG_FILENAME         = 'BASE';
  HELP_MSG_FILENAME       = 'NETH';

  {**INTERNAL_ONLY }

  { The backup message file named here is a duplicate of net.msg. It }
  { is not shipped with the product, but is used at buildtime to }
  { msgbind certain messages to netapi.dll and some of the services. }
  { This allows for OEMs to modify the message text in net.msg and }
  { have those changes show up.        Only in case there is an error in }
  { retrieving the messages from net.msg do we then get the bound }
  { messages out of bak.msg (really out of the message segment). }

  BACKUP_MSG_FILENAME     = 'BAK.MSG';

  {**END_INTERNAL }

  { Keywords used in Function Prototypes }

type
  NET_API_STATUS       =   DWord;
  API_RET_TYPE         =   NET_API_STATUS;      { Old value: do not use }

  { The platform ID indicates the levels to use for platform-specific }
  { information. }

const
  PLATFORM_ID_DOS = 300;
  PLATFORM_ID_OS2 = 400;
  PLATFORM_ID_NT  = 500;
  PLATFORM_ID_OSF = 600;
  PLATFORM_ID_VMS = 700;

  {      There message numbers assigned to different LANMAN components }
  {      are as defined below. }

  {      lmerr.h:        2100 - 2999     NERR_BASE }
  {      alertmsg.h:     3000 - 3049     ALERT_BASE }
  {      lmsvc.h:        3050 - 3099     SERVICE_BASE }
  {      lmerrlog.h:     3100 - 3299     ERRLOG_BASE }
  {      msgtext.h:      3300 - 3499     MTXT_BASE }
  {      apperr.h:       3500 - 3999     APPERR_BASE }
  {      apperrfs.h:     4000 - 4299     APPERRFS_BASE }
  {      apperr2.h:      4300 - 5299     APPERR2_BASE }
  {      ncberr.h:       5300 - 5499     NRCERR_BASE }
  {      alertmsg.h:     5500 - 5599     ALERT2_BASE }
  {      lmsvc.h:        5600 - 5699     SERVICE2_BASE }
  {      lmerrlog.h      5700 - 5799     ERRLOG2_BASE }

  MIN_LANMAN_MESSAGE_ID  = NERR_BASE;
  MAX_LANMAN_MESSAGE_ID  = 5799;

{-------------------- LMACCESS.H --------------------------------------------}
type
  TNetUserAdd                    = function(ServerName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetUserEnum                   = function(ServerName: LPCWSTR; Level, Filter: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetUserGetInfo                = function(ServerName, UserName: LPCWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetUserSetInfo                = function(ServerName, UserName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetUserDel                    = function(ServerName, UserName: LPCWSTR): NET_API_STATUS; stdcall;
  TNetUserGetGroups              = function(ServerName, UserName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries : DWord): NET_API_STATUS; stdcall;
  TNetUserSetGroups              = function(ServerName, UserName: LPCWSTR; Level: DWord; Buffer: Pointer; NumEntries: DWord): NET_API_STATUS; stdcall;
  TNetUserGetLocalGroups         = function(ServerName, UserName: LPCWSTR; Level: DWord; Flags: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries : DWord): NET_API_STATUS; stdcall;
  TNetUserModalsGet              = function(ServerName: LPCWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetUserModalsSet              = function(ServerName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetUserChangePassword         = function(DomainName, UserName, OldPassword, NewPassword: LPCWSTR): NET_API_STATUS; stdcall;

  TNetGroupAdd                   = function(ServerName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetGroupAddUser               = function(ServerName, GroupName, UserName: LPCWSTR): NET_API_STATUS; stdcall;
  TNetGroupEnum                  = function(ServerName: LPCWSTR; Level: DWord; Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetGroupGetInfo               = function(ServerName, GroupName: LPCWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetGroupSetInfo               = function(ServerName, GroupName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetGroupDel                   = function(ServerName, GroupName: LPCWSTR): NET_API_STATUS; stdcall;
  TNetGroupDelUser               = function(ServerName, GroupName, UserName: LPCWSTR): NET_API_STATUS; stdcall;
  TNetGroupGetUsers              = function(ServerName, GroupName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ResumeHandle: Pointer): NET_API_STATUS; stdcall;
  TNetGroupSetUsers              = function(ServerName, GroupName: LPCWSTR; Level: DWord; Buffer: Pointer; var TotalEntries : DWord): NET_API_STATUS; stdcall;

  TNetLocalGroupAdd              = function(ServerName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetLocalGroupAddMember        = function(ServerName, GroupName: LPCWSTR; MembersID: PSID): NET_API_STATUS; stdcall;
  TNetLocalGroupEnum             = function(ServerName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; var ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetLocalGroupGetInfo          = function(ServerName, GroupName: LPCWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetLocalGroupSetInfo          = function(ServerName, GroupName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetLocalGroupDel              = function(ServerName, GroupName: LPCWSTR): NET_API_STATUS; stdcall;
  TNetLocalGroupDelMember        = function(ServerName, GroupName: LPCWSTR; MembersID: PSID): NET_API_STATUS; stdcall;
  TNetLocalGroupGetMembers       = function(ServerName, GroupName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ResumeHandle: Pointer): NET_API_STATUS; stdcall;
  TNetLocalGroupSetMembers       = function(ServerName, GroupName: LPCWSTR; Level: DWord; Buffer: Pointer; var TotalEntries: DWord): NET_API_STATUS; stdcall;
  TNetLocalGroupAddMembers       = function(ServerName, GroupName: LPCWSTR; Level: DWord; Buffer: Pointer; TotalEntries: DWord): NET_API_STATUS; stdcall;
  TNetLocalGroupDelMembers       = function(ServerName, GroupName: LPCWSTR; Level: DWord; Buffer: Pointer; TotalEntries: DWord): NET_API_STATUS; stdcall;

  TNetQueryDisplayInformation    = function(ServerName: LPCWSTR; Level, Index, EntriesRequested, PrefMaxLen: DWord; var ReturnedCount: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetGetDisplayInformationIndex = function(ServerName: LPCWSTR; Level: DWord; Prefix: LPCWSTR; var Index: DWord): NET_API_STATUS; stdcall;

  TNetAccessAdd                  = function(ServerName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetAccessEnum                 = function(ServerName, BasePath: LPCWSTR; Recursive, Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetAccessGetInfo              = function(ServerName, Resource: LPCWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetAccessSetInfo              = function(ServerName, Resource: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetAccessDel                  = function(ServerName, Resource: LPCWSTR): NET_API_STATUS; stdcall;
  TNetAccessGetUserPerms         = function(ServerName, UGname, Resource: LPCWSTR; var Perms: DWord): NET_API_STATUS; stdcall;

  TNetGetDCName                  = function(ServerName, DomainName: LPCWSTR; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetGetAnyDCName               = function(ServerName, DomainName: LPCWSTR; var Buffer: PByte): NET_API_STATUS; stdcall;
  TI_NetLogonControl             = function(ServerName: LPCWSTR; FunctionCode, QueryLevel: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TI_NetLogonControl2            = function(ServerName: LPCWSTR; FunctionCode, QueryLevel: DWord; Data: Pointer; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetEnumerateTrustedDomains    = function(ServerName: LPCWSTR; var Buffer: Pointer):  LongInt; StdCall;

const
  USER_POSIX_ID_PARMNUM  = 1;
  GROUP_POSIX_ID_PARMNUM = 1;

type
  PUSER_INFO_0 = ^TUSER_INFO_0;
  TUSER_INFO_0 = record
    usri0_name            : LPWSTR;
  end;

  PUSER_INFO_1 = ^TUSER_INFO_1;
  TUSER_INFO_1 = record
    usri1_name            : LPWSTR;
    usri1_password        : LPWSTR{LPSTR};
    usri1_password_age    : DWord;
    usri1_priv            : DWord;
    usri1_home_dir        : LPWSTR;
    usri1_comment         : LPWSTR;
    usri1_flags           : DWord;
    usri1_script_path     : LPWSTR;
  end;

  PUSER_INFO_2 = ^TUSER_INFO_2;
  TUSER_INFO_2 = record
    usri2_name            : LPWSTR;
    usri2_password        : LPWSTR;
    usri2_password_age    : DWord;
    usri2_priv            : DWord;
    usri2_home_dir        : LPWSTR;
    usri2_comment         : LPWSTR;
    usri2_flags           : DWord;
    usri2_script_path     : LPWSTR;
    usri2_auth_flags      : DWord;
    usri2_full_name       : LPWSTR;
    usri2_usr_comment     : LPWSTR;
    usri2_parms           : LPWSTR;
    usri2_workstations    : LPWSTR;
    usri2_last_logon      : DWord;
    usri2_last_logoff     : DWord;
    usri2_acct_expires    : DWord;
    usri2_max_storage     : DWord;
    usri2_units_per_week  : DWord;
    usri2_logon_hours     : PByte;
    usri2_bad_pw_count    : DWord;
    usri2_num_logons      : DWord;
    usri2_logon_server    : LPWSTR;
    usri2_country_code    : DWord;
    usri2_code_page       : DWord;
  end;

  PUSER_INFO_3 = ^TUSER_INFO_3;
  TUSER_INFO_3 = record
    usri3_name            : LPWSTR;
    usri3_password        : LPWSTR;
    usri3_password_age    : DWord;
    usri3_priv            : DWord;
    usri3_home_dir        : LPWSTR;
    usri3_comment         : LPWSTR;
    usri3_flags           : DWord;
    usri3_script_path     : LPWSTR;
    usri3_auth_flags      : DWord;
    usri3_full_name       : LPWSTR;
    usri3_usr_comment     : LPWSTR;
    usri3_parms           : LPWSTR;
    usri3_workstations    : LPWSTR;
    usri3_last_logon      : DWord;
    usri3_last_logoff     : DWord;
    usri3_acct_expires    : DWord;
    usri3_max_storage     : DWord;
    usri3_units_per_week  : DWord;
    usri3_logon_hours     : Pointer;
    usri3_bad_pw_count    : DWord;
    usri3_num_logons      : DWord;
    usri3_logon_server    : LPWSTR;
    usri3_country_code    : DWord;
    usri3_code_page       : DWord;
    usri3_user_id         : DWord;
    usri3_primary_group_id: DWord;
    usri3_profile         : LPWSTR;
    usri3_home_dir_drive  : LPWSTR;
    usri3_password_expired: DWord;
  end;

  PUSER_INFO_10 = ^TUSER_INFO_10;
  TUSER_INFO_10 = record
    usri10_name           : LPWSTR;
    usri10_comment        : LPWSTR;
    usri10_usr_comment    : LPWSTR;
    usri10_full_name      : LPWSTR;
  end;

  PUSER_INFO_11 = ^TUSER_INFO_11;
  TUSER_INFO_11 = record
    usri11_name           : LPWSTR;
    usri11_comment        : LPWSTR;
    usri11_usr_comment    : LPWSTR;
    usri11_full_name      : LPWSTR;
    usri11_priv           : DWord;
    usri11_auth_flags     : DWord;
    usri11_password_age   : DWord;
    usri11_home_dir       : LPWSTR;
    usri11_parms          : LPWSTR;
    usri11_last_logon     : DWord;
    usri11_last_logoff    : DWord;
    usri11_bad_pw_count   : DWord;
    usri11_num_logons     : DWord;
    usri11_logon_server   : LPWSTR;
    usri11_country_code   : DWord;
    usri11_workstations   : LPWSTR;
    usri11_max_storage    : DWord;
    usri11_units_per_week : DWord;
    usri11_logon_hours    : PByte;
    usri11_code_page      : DWord;
  end;

  PUSER_INFO_20 = ^TUSER_INFO_20;
  TUSER_INFO_20 = record
    usri20_name           : LPWSTR;
    usri20_full_name      : LPWSTR;
    usri20_comment        : LPWSTR;
    usri20_flags          : DWord;
    usri20_user_id        : DWord;
  end;

  PUSER_INFO_21 = ^TUSER_INFO_21;
  TUSER_INFO_21 = record
    usri21_password       : array[0..ENCRYPTED_PWLEN-1] of byte;
  end;

  PUSER_INFO_22 = ^TUSER_INFO_22;
  TUSER_INFO_22 = record
    usri22_name           : LPWSTR;
    usri22_password       : array[0..ENCRYPTED_PWLEN-1] of byte;
    usri22_password_age   : DWord;
    usri22_priv           : DWord;
    usri22_home_dir       : LPWSTR;
    usri22_comment        : LPWSTR;
    usri22_flags          : DWord;
    usri22_script_path    : LPWSTR;
    usri22_auth_flags     : DWord;
    usri22_full_name      : LPWSTR;
    usri22_usr_comment    : LPWSTR;
    usri22_parms          : LPWSTR;
    usri22_workstations   : LPWSTR;
    usri22_last_logon     : DWord;
    usri22_last_logoff    : DWord;
    usri22_acct_expires   : DWord;
    usri22_max_storage    : DWord;
    usri22_units_per_week : DWord;
    usri22_logon_hours    : PByte;
    usri22_bad_pw_count   : DWord;
    usri22_num_logons     : DWord;
    usri22_logon_server   : LPWSTR;
    usri22_country_code   : DWord;
    usri22_code_page      : DWord;
  end;

  PUSER_INFO_1003 = ^TUSER_INFO_1003;
  TUSER_INFO_1003 = record
    usri1003_password: LPWSTR;
  end;

  PUSER_INFO_1005 = ^TUSER_INFO_1005;
  TUSER_INFO_1005 = record
    usri1005_priv : DWord;
  end;

  PUSER_INFO_1006 = ^TUSER_INFO_1006;
  TUSER_INFO_1006 = record
    usri1006_home_dir: LPWSTR;
  end;

  PUSER_INFO_1007 = ^TUSER_INFO_1007;
  TUSER_INFO_1007 = record
    usri1007_comment : LPWSTR;
  end;

  PUSER_INFO_1008 = ^TUSER_INFO_1008;
  TUSER_INFO_1008 = record
    usri1008_flags: DWord;
  end;

  PUSER_INFO_1009 = ^TUSER_INFO_1009;
  TUSER_INFO_1009 = record
    usri1009_script_path: LPWSTR;
  end;

  PUSER_INFO_1010 = ^TUSER_INFO_1010;
  TUSER_INFO_1010 = record
    usri1010_auth_flags: DWord;
  end;

  PUSER_INFO_1011 = ^TUSER_INFO_1011;
  TUSER_INFO_1011 = record
    usri1011_full_name : LPWSTR;
  end;

  PUSER_INFO_1012 = ^TUSER_INFO_1012;
  TUSER_INFO_1012 = record
    usri1012_usr_comment : LPWSTR;
  end;

  PUSER_INFO_1013 = ^TUSER_INFO_1013;
  TUSER_INFO_1013 = record
    usri1013_parms : LPWSTR;
  end;

  PUSER_INFO_1014 = ^TUSER_INFO_1014;
  TUSER_INFO_1014 = record
    usri1014_workstations : LPWSTR;
  end;

  PUSER_INFO_1017 = ^TUSER_INFO_1017;
  TUSER_INFO_1017 = record
    usri1017_acct_expires : DWord;
  end;

  PUSER_INFO_1018 = ^TUSER_INFO_1018;
  TUSER_INFO_1018 = record
    usri1018_max_storage: DWord;
  end;

  PUSER_INFO_1020 = ^TUSER_INFO_1020;
  TUSER_INFO_1020 = record
    usri1020_units_per_week : DWord;
    usri1020_logon_hours: PByte;
  end;

  PUSER_INFO_1023 = ^TUSER_INFO_1023;
  TUSER_INFO_1023 = record
    usri1023_logon_server : LPWSTR;
  end;

  PUSER_INFO_1024 = ^TUSER_INFO_1024;
  TUSER_INFO_1024 = record
    usri1024_country_code: DWord;
  end;

  PUSER_INFO_1025 = ^TUSER_INFO_1025;
  TUSER_INFO_1025 = record
    usri1025_code_page: DWord;
  end;

  PUSER_INFO_1051 = ^TUSER_INFO_1051;
  TUSER_INFO_1051 = record
    usri1051_primary_group_id: DWord;
  end;

  PUSER_INFO_1052 = ^TUSER_INFO_1052;
  TUSER_INFO_1052 = record
    usri1052_profile : LPWSTR;
  end;

  PUSER_INFO_1053 = ^TUSER_INFO_1053;
  TUSER_INFO_1053 = record
    usri1053_home_dir_drive : LPWSTR;
  end;

  PUSER_MODALS_INFO_0 = ^TUSER_MODALS_INFO_0;
  TUSER_MODALS_INFO_0 = record
    usrmod0_min_passwd_len    : DWord;
    usrmod0_max_passwd_age    : DWord;
    usrmod0_min_passwd_age    : DWord;
    usrmod0_force_logoff      : DWord;
    usrmod0_password_hist_len : DWord;
  end;

  PUSER_MODALS_INFO_1 = ^TUSER_MODALS_INFO_1;
  TUSER_MODALS_INFO_1 = record
    usrmod1_role    : DWord;
    usrmod1_primary : LPWSTR;
  end;

  PUSER_MODALS_INFO_2 = ^TUSER_MODALS_INFO_2;
  TUSER_MODALS_INFO_2 = record
    usrmod2_domain_name : LPWSTR;
    usrmod2_domain_id   : PSID;
  end;

  PUSER_MODALS_INFO_3 = ^TUSER_MODALS_INFO_3;
  TUSER_MODALS_INFO_3 = record
    usrmod3_lockout_duration           : DWord;
    usrmod3_lockout_observation_window : DWord;
    usrmod3_lockout_threshold          : DWord;
  end;

  PUSER_MODALS_INFO_1001 = ^TUSER_MODALS_INFO_1001;
  TUSER_MODALS_INFO_1001 = record
    usrmod1001_min_passwd_len : DWord;
  end;

  PUSER_MODALS_INFO_1002 = ^TUSER_MODALS_INFO_1002;
  TUSER_MODALS_INFO_1002 = record
    usrmod1002_max_passwd_age : DWord;
  end;

  PUSER_MODALS_INFO_1003 = ^TUSER_MODALS_INFO_1003;
  TUSER_MODALS_INFO_1003 = record
    usrmod1003_min_passwd_age : DWord;
  end;

  PUSER_MODALS_INFO_1004 = ^TUSER_MODALS_INFO_1004;
  TUSER_MODALS_INFO_1004 = record
    usrmod1004_force_logoff : DWord;
  end;

  PUSER_MODALS_INFO_1005 = ^TUSER_MODALS_INFO_1005;
  TUSER_MODALS_INFO_1005 = record
    usrmod1005_password_hist_len : DWord;
  end;

  PUSER_MODALS_INFO_1006 = ^TUSER_MODALS_INFO_1006;
  TUSER_MODALS_INFO_1006 = record
    usrmod1006_role : DWord;
  end;

  PUSER_MODALS_INFO_1007 = ^TUSER_MODALS_INFO_1007;
  TUSER_MODALS_INFO_1007 = record
    usrmod1007_primary : LPWSTR;
  end;

const
  { Bit masks for field usriX_flags of USER_INFO_X (X = 0/1). }
  UF_SCRIPT                      = $0001;
  UF_ACCOUNTDISABLE              = $0002;
  UF_HOMEDIR_REQUIRED            = $0008;
  UF_LOCKOUT                     = $0010;
  UF_PASSWD_NOTREQD              = $0020;
  UF_PASSWD_CANT_CHANGE          = $0040;

  { Account type bits as part of usri_flags. }
  UF_TEMP_DUPLICATE_ACCOUNT      = $0100;
  UF_NORMAL_ACCOUNT              = $0200;
  UF_INTERDOMAIN_TRUST_ACCOUNT   = $0800;
  UF_WORKSTATION_TRUST_ACCOUNT   = $1000;
  UF_SERVER_TRUST_ACCOUNT        = $2000;

  UF_MACHINE_ACCOUNT_MASK        = (UF_INTERDOMAIN_TRUST_ACCOUNT or
                                    UF_WORKSTATION_TRUST_ACCOUNT or
                                    UF_SERVER_TRUST_ACCOUNT);

  UF_ACCOUNT_TYPE_MASK           = (UF_TEMP_DUPLICATE_ACCOUNT or
                                    UF_NORMAL_ACCOUNT or
                                    UF_INTERDOMAIN_TRUST_ACCOUNT or
                                    UF_WORKSTATION_TRUST_ACCOUNT or
                                    UF_SERVER_TRUST_ACCOUNT);

  UF_DONT_EXPIRE_PASSWD          = $10000;
  UF_MNS_LOGON_ACCOUNT           = $20000;


  UF_SETTABLE_BITS               = (UF_SCRIPT or
                                    UF_ACCOUNTDISABLE or
                                    UF_LOCKOUT or
                                    UF_HOMEDIR_REQUIRED or
                                    UF_PASSWD_NOTREQD or
                                    UF_PASSWD_CANT_CHANGE or
                                    UF_ACCOUNT_TYPE_MASK or
                                    UF_DONT_EXPIRE_PASSWD or
                                    UF_MNS_LOGON_ACCOUNT);

  { bit masks for the NetUserEnum filter parameter. }
  FILTER_TEMP_DUPLICATE_ACCOUNT    = $0001;
  FILTER_NORMAL_ACCOUNT            = $0002;
  {FILTER_PROXY_ACCOUNT             = $0004;}
  FILTER_INTERDOMAIN_TRUST_ACCOUNT = $0008;
  FILTER_WORKSTATION_TRUST_ACCOUNT = $0010;
  FILTER_SERVER_TRUST_ACCOUNT      = $0020;

  { bit masks for the NetUserGetLocalGroups flags }
  LG_INCLUDE_INDIRECT              = $0001;

  { Bit masks for field usri2_auth_flags of USER_INFO_2. }
  AF_OP_PRINT             = $1;
  AF_OP_COMM              = $2;
  AF_OP_SERVER            = $4;
  AF_OP_ACCOUNTS          = $8;
  AF_SETTABLE_BITS        = (AF_OP_PRINT or AF_OP_COMM or
                             AF_OP_SERVER or AF_OP_ACCOUNTS);

  { UAS role manifests under NETLOGON }
  UAS_ROLE_STANDALONE    = 0;
  UAS_ROLE_MEMBER        = 1;
  UAS_ROLE_BACKUP        = 2;
  UAS_ROLE_PRIMARY       = 3;

  { Values for ParmError for NetUserSetInfo. }
  USER_NAME_PARMNUM              = 1;
  USER_PASSWORD_PARMNUM          = 3;
  USER_PASSWORD_AGE_PARMNUM      = 4;
  USER_PRIV_PARMNUM              = 5;
  USER_HOME_DIR_PARMNUM          = 6;
  USER_COMMENT_PARMNUM           = 7;
  USER_FLAGS_PARMNUM             = 8;
  USER_SCRIPT_PATH_PARMNUM       = 9;
  USER_AUTH_FLAGS_PARMNUM        = 10;
  USER_FULL_NAME_PARMNUM         = 11;
  USER_USR_COMMENT_PARMNUM       = 12;
  USER_PARMS_PARMNUM             = 13;
  USER_WORKSTATIONS_PARMNUM      = 14;
  USER_LAST_LOGON_PARMNUM        = 15;
  USER_LAST_LOGOFF_PARMNUM       = 16;
  USER_ACCT_EXPIRES_PARMNUM      = 17;
  USER_MAX_STORAGE_PARMNUM       = 18;
  USER_UNITS_PER_WEEK_PARMNUM    = 19;
  USER_LOGON_HOURS_PARMNUM       = 20;
  USER_PAD_PW_COUNT_PARMNUM      = 21;
  USER_NUM_LOGONS_PARMNUM        = 22;
  USER_LOGON_SERVER_PARMNUM      = 23;
  USER_COUNTRY_CODE_PARMNUM      = 24;
  USER_CODE_PAGE_PARMNUM         = 25;
  USER_PRIMARY_GROUP_PARMNUM     = 51;
  USER_PROFILE                   = 52;
  USER_PROFILE_PARMNUM           = 52;
  USER_HOME_DIR_DRIVE_PARMNUM    = 53;

  { the new infolevel counterparts of the old info level + parmnum }
  USER_NAME_INFOLEVEL             = (PARMNUM_BASE_INFOLEVEL + USER_NAME_PARMNUM);
  USER_PASSWORD_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_PARMNUM);
  USER_PASSWORD_AGE_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + USER_PASSWORD_AGE_PARMNUM);
  USER_PRIV_INFOLEVEL             = (PARMNUM_BASE_INFOLEVEL + USER_PRIV_PARMNUM);
  USER_HOME_DIR_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_PARMNUM);
  USER_COMMENT_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + USER_COMMENT_PARMNUM);
  USER_FLAGS_INFOLEVEL            = (PARMNUM_BASE_INFOLEVEL + USER_FLAGS_PARMNUM);
  USER_SCRIPT_PATH_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + USER_SCRIPT_PATH_PARMNUM);
  USER_AUTH_FLAGS_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + USER_AUTH_FLAGS_PARMNUM);
  USER_FULL_NAME_INFOLEVEL        = (PARMNUM_BASE_INFOLEVEL + USER_FULL_NAME_PARMNUM);
  USER_USR_COMMENT_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + USER_USR_COMMENT_PARMNUM);
  USER_PARMS_INFOLEVEL            = (PARMNUM_BASE_INFOLEVEL + USER_PARMS_PARMNUM);
  USER_WORKSTATIONS_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + USER_WORKSTATIONS_PARMNUM);
  USER_LAST_LOGON_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGON_PARMNUM);
  USER_LAST_LOGOFF_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + USER_LAST_LOGOFF_PARMNUM);
  USER_ACCT_EXPIRES_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + USER_ACCT_EXPIRES_PARMNUM);
  USER_MAX_STORAGE_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + USER_MAX_STORAGE_PARMNUM);
  USER_UNITS_PER_WEEK_INFOLEVEL   = (PARMNUM_BASE_INFOLEVEL + USER_UNITS_PER_WEEK_PARMNUM);
  USER_LOGON_HOURS_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + USER_LOGON_HOURS_PARMNUM);
  USER_PAD_PW_COUNT_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + USER_PAD_PW_COUNT_PARMNUM);
  USER_NUM_LOGONS_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + USER_NUM_LOGONS_PARMNUM);
  USER_LOGON_SERVER_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + USER_LOGON_SERVER_PARMNUM);
  USER_COUNTRY_CODE_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + USER_COUNTRY_CODE_PARMNUM);
  USER_CODE_PAGE_INFOLEVEL        = (PARMNUM_BASE_INFOLEVEL + USER_CODE_PAGE_PARMNUM);
  USER_PRIMARY_GROUP_INFOLEVEL    = (PARMNUM_BASE_INFOLEVEL + USER_PRIMARY_GROUP_PARMNUM);
  USER_POSIX_ID_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + USER_POSIX_ID_PARMNUM);
  USER_HOME_DIR_DRIVE_INFOLEVEL   = (PARMNUM_BASE_INFOLEVEL + USER_HOME_DIR_DRIVE_PARMNUM);

  { For SetInfo call (parmnum 0) when password change not required }
  NULL_USERSETINFO_PASSWD     = '              ';

  TIMEQ_FOREVER               = ULong(-1);
  USER_MAXSTORAGE_UNLIMITED   = ULong(-1);
  USER_NO_LOGOFF              = ULong(-1);
  UNITS_PER_DAY               = 24;
  UNITS_PER_WEEK              = UNITS_PER_DAY * 7;

  { Privilege levels (USER_INFO_X field usriX_priv (X = 0/1)). }
  USER_PRIV_MASK      = $3;
  USER_PRIV_GUEST     = 0;
  USER_PRIV_USER      = 1;
  USER_PRIV_ADMIN     = 2;

  { user modals related defaults }
  MAX_PASSWD_LEN    = PWLEN;
  DEF_MIN_PWLEN     = 6;
  DEF_PWUNIQUENESS  = 5;
  DEF_MAX_PWHIST    = 8;

  DEF_MAX_PWAGE       = TIMEQ_FOREVER;      { forever  }
  DEF_MIN_PWAGE       = ULong(0);           { 0 days   }
  DEF_FORCE_LOGOFF    = ULong($ffffffff);   { never    }
  DEF_MAX_BADPW       = 0;                  { no limit }
  ONE_DAY             = ULong(01*24*3600);  { 01 day   }

  { User Logon Validation (codes returned) }
  VALIDATED_LOGON        = 0;
  PASSWORD_EXPIRED       = 2;
  NON_VALIDATED_LOGON    = 3;

  VALID_LOGOFF           = 1;

  { parmnum manifests for user modals }
  MODALS_MIN_PASSWD_LEN_PARMNUM             = 1;
  MODALS_MAX_PASSWD_AGE_PARMNUM             = 2;
  MODALS_MIN_PASSWD_AGE_PARMNUM             = 3;
  MODALS_FORCE_LOGOFF_PARMNUM               = 4;
  MODALS_PASSWD_HIST_LEN_PARMNUM            = 5;
  MODALS_ROLE_PARMNUM                       = 6;
  MODALS_PRIMARY_PARMNUM                    = 7;
  MODALS_DOMAIN_NAME_PARMNUM                = 8;
  MODALS_DOMAIN_ID_PARMNUM                  = 9;
  MODALS_LOCKOUT_DURATION_PARMNUM           = 10;
  MODALS_LOCKOUT_OBSERVATION_WINDOW_PARMNUM = 11;
  MODALS_LOCKOUT_THRESHOLD_PARMNUM          = 12;

  { the new infolevel counterparts of the old info level + parmnum }
  MODALS_MIN_PASSWD_LEN_INFOLEVEL  = (PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_LEN_PARMNUM);
  MODALS_MAX_PASSWD_AGE_INFOLEVEL  = (PARMNUM_BASE_INFOLEVEL + MODALS_MAX_PASSWD_AGE_PARMNUM);
  MODALS_MIN_PASSWD_AGE_INFOLEVEL  = (PARMNUM_BASE_INFOLEVEL + MODALS_MIN_PASSWD_AGE_PARMNUM);
  MODALS_FORCE_LOGOFF_INFOLEVEL    = (PARMNUM_BASE_INFOLEVEL + MODALS_FORCE_LOGOFF_PARMNUM);
  MODALS_PASSWD_HIST_LEN_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + MODALS_PASSWD_HIST_LEN_PARMNUM);
  MODALS_ROLE_INFOLEVEL            = (PARMNUM_BASE_INFOLEVEL + MODALS_ROLE_PARMNUM);
  MODALS_PRIMARY_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + MODALS_PRIMARY_PARMNUM);
  MODALS_DOMAIN_NAME_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_NAME_PARMNUM);
  MODALS_DOMAIN_ID_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + MODALS_DOMAIN_ID_PARMNUM);

type
  PGROUP_INFO_0 = ^TGROUP_INFO_0;
  TGROUP_INFO_0 = record
    grpi0_name          : LPWSTR;
  end;

  PGROUP_INFO_1 = ^TGROUP_INFO_1;
  TGROUP_INFO_1 = record
    grpi1_name          : LPWSTR;
    grpi1_comment       : LPWSTR;
  end;

  PGROUP_INFO_2 = ^TGROUP_INFO_2;
  TGROUP_INFO_2 = record
    grpi2_name          : LPWSTR;
    grpi2_comment       : LPWSTR;
    grpi2_group_id      : DWord;
    grpi2_attributes    : DWord;
  end;

  PGROUP_INFO_1002 = ^TGROUP_INFO_1002;
  TGROUP_INFO_1002 = record
    grpi1002_comment    : LPWSTR;
  end;

  PGROUP_INFO_1005 = ^TGROUP_INFO_1005;
  TGROUP_INFO_1005 = record
    grpi1005_attributes : DWord;
  end;

  PGROUP_USERS_INFO_0 = ^TGROUP_USERS_INFO_0;
  TGROUP_USERS_INFO_0 = record
    grui0_name          : LPWSTR;
  end;

  PGROUP_USERS_INFO_1 = ^TGROUP_USERS_INFO_1;
  TGROUP_USERS_INFO_1 = record
    grui1_name          : LPWSTR;
    grui1_attributes    : DWord;
  end;

{ Special Values and Constants - Group }
const
  GROUPIDMASK                 = $8000;   { MSB set if uid refers to a group }

{ Predefined group for all normal users, administrators and guests }
{ LOCAL is a special group for pinball local security.             }
  GROUP_SPECIALGRP_USERS      = 'USERS';
  GROUP_SPECIALGRP_ADMINS     = 'ADMINS';
  GROUP_SPECIALGRP_GUESTS     = 'GUESTS';
  GROUP_SPECIALGRP_LOCAL      = 'LOCAL';

{ parmnum manifests for SetInfo calls (only comment is settable) }
  GROUP_ALL_PARMNUM           = 0;
  GROUP_NAME_PARMNUM          = 1;
  GROUP_COMMENT_PARMNUM       = 2;
  GROUP_ATTRIBUTES_PARMNUM    = 3;

{ the new infolevel counterparts of the old info level + parmnum }
  GROUP_ALL_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + GROUP_ALL_PARMNUM);
  GROUP_NAME_INFOLEVEL        = (PARMNUM_BASE_INFOLEVEL + GROUP_NAME_PARMNUM);
  GROUP_COMMENT_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + GROUP_COMMENT_PARMNUM);
  GROUP_ATTRIBUTES_INFOLEVEL  = (PARMNUM_BASE_INFOLEVEL + GROUP_ATTRIBUTES_PARMNUM);
  GROUP_POSIX_ID_INFOLEVEL    = (PARMNUM_BASE_INFOLEVEL + GROUP_POSIX_ID_PARMNUM);

type
  PLOCALGROUP_INFO_0 =  ^TLOCALGROUP_INFO_0;
  TLOCALGROUP_INFO_0 = record
    lgrpi0_name          : LPWSTR;
  end;

  PLOCALGROUP_INFO_1 = ^TLOCALGROUP_INFO_1;
  TLOCALGROUP_INFO_1 = record
    lgrpi1_name          : LPWSTR;
    lgrpi1_comment       : LPWSTR;
  end;

  PLOCALGROUP_INFO_1002 = ^TLOCALGROUP_INFO_1002;
  TLOCALGROUP_INFO_1002 = record
    lgrpi1002_comment    : LPWSTR;
  end;

  PLOCALGROUP_MEMBERS_INFO_0 = ^TLOCALGROUP_MEMBERS_INFO_0;
  TLOCALGROUP_MEMBERS_INFO_0 = record
    lgrmi0_sid           : PSID;
  end;

  PLOCALGROUP_MEMBERS_INFO_1 = ^TLOCALGROUP_MEMBERS_INFO_1;
  TLOCALGROUP_MEMBERS_INFO_1 = record
    lgrmi1_sid           : PSID;
    lgrmi1_sidusage      : SID_NAME_USE;
    lgrmi1_name          : LPWSTR;
  end;

  PLOCALGROUP_MEMBERS_INFO_2 = ^TLOCALGROUP_MEMBERS_INFO_2;
  TLOCALGROUP_MEMBERS_INFO_2 = record
    lgrmi2_sid           : PSID;
    lgrmi2_sidusage      : SID_NAME_USE;
    lgrmi2_domainandname : LPWSTR;
  end;

  PLOCALGROUP_MEMBERS_INFO_3 = ^TLOCALGROUP_MEMBERS_INFO_3;
  TLOCALGROUP_MEMBERS_INFO_3 = record
    lgrmi3_domainandname : LPWSTR;
  end;

  PLOCALGROUP_USERS_INFO_0 = ^TLOCALGROUP_USERS_INFO_0;
  TLOCALGROUP_USERS_INFO_0 = record
    lgrui0_name          : LPWSTR;
  end;

const
  LOCALGROUP_NAME_PARMNUM         = 1;
  LOCALGROUP_COMMENT_PARMNUM      = 2;

{ QueryDisplayInformation levels }
type
  PNET_DISPLAY_USER = ^TNET_DISPLAY_USER;
  TNET_DISPLAY_USER = record
    usri1_name       : LPWSTR;
    usri1_comment    : LPWSTR;
    usri1_flags      : DWord;
    usri1_full_name  : LPWSTR;
    usri1_user_id    : DWord;
    usri1_next_index : DWord;
  end;

  PNET_DISPLAY_MACHINE = ^TNET_DISPLAY_MACHINE;
  TNET_DISPLAY_MACHINE = record
    usri2_name       : LPWSTR;
    usri2_comment    : LPWSTR;
    usri2_flags      : DWord;
    usri2_user_id    : DWord;
    usri2_next_index : DWord;
  end;

  PNET_DISPLAY_GROUP = ^TNET_DISPLAY_GROUP;
  TNET_DISPLAY_GROUP = record
    grpi3_name       : LPWSTR;
    grpi3_comment    : LPWSTR;
    grpi3_group_id   : DWord;
    grpi3_attributes : DWord;
    grpi3_next_index : DWord;
  end;

{ Data Structures - Access }
  PACCESS_INFO_0 = ^TACCESS_INFO_0;
  TACCESS_INFO_0 = record
    acc0_resource_name : LPWSTR;
  end;

  PACCESS_INFO_1 = ^TACCESS_INFO_1;
  TACCESS_INFO_1 = record
    acc1_resource_name : LPWSTR;
    acc1_attr          : DWord;
    acc1_count         : DWord;
  end;

  PACCESS_INFO_1002 = ^TACCESS_INFO_1002;
  TACCESS_INFO_1002 = record
    acc1002_attr       : DWord;
  end;

  PACCESS_LIST = ^TACCESS_LIST;
  TACCESS_LIST = record
    acl_ugname         : LPWSTR;
    acl_access         : DWord;
  end;

{ Special Values and Constants - Access }

{ Maximum number of permission entries for each resource. }
const
  MAXPERMENTRIES      = 64;


{  Bit values for the access permissions.  ACCESS_ALL is a handy
  way to specify maximum permissions.  These are used in
  acl_access field of access_list structures.
}
  ACCESS_NONE         = 0;

  ACCESS_READ         = $01;
  ACCESS_WRITE        = $02;
  ACCESS_CREATE       = $04;
  ACCESS_EXEC         = $08;
  ACCESS_DELETE       = $10;
  ACCESS_ATRIB        = $20;
  ACCESS_PERM         = $40;

  ACCESS_ALL          = (ACCESS_READ or
                         ACCESS_WRITE or
                         ACCESS_CREATE or
                         ACCESS_EXEC or
                         ACCESS_DELETE or
                         ACCESS_ATRIB or
                         ACCESS_PERM);


  ACCESS_GROUP       = $8000;

{ Bit values for the acc1_attr field of the ACCESS_INFO_1 structure. }
  ACCESS_AUDIT        = $1;

  ACCESS_SUCCESS_OPEN        = $10;
  ACCESS_SUCCESS_WRITE       = $20;
  ACCESS_SUCCESS_DELETE      = $40;
  ACCESS_SUCCESS_ACL         = $80;
  ACCESS_SUCCESS_MASK        = $F0;

  ACCESS_FAIL_OPEN           = $100;
  ACCESS_FAIL_WRITE          = $200;
  ACCESS_FAIL_DELETE         = $400;
  ACCESS_FAIL_ACL            = $800;
  ACCESS_FAIL_MASK           = $F00;

  ACCESS_FAIL_SHIFT          = 4;

{ Parmnum value for NetAccessSetInfo. }
  ACCESS_RESOURCE_NAME_PARMNUM   = 1;
  ACCESS_ATTR_PARMNUM            = 2;
  ACCESS_COUNT_PARMNUM           = 3;
  ACCESS_ACCESS_LIST_PARMNUM     = 4;

{ the new infolevel counterparts of the old info level + parmnum }
  ACCESS_RESOURCE_NAME_INFOLEVEL  = (PARMNUM_BASE_INFOLEVEL + ACCESS_RESOURCE_NAME_PARMNUM);
  ACCESS_ATTR_INFOLEVEL           = (PARMNUM_BASE_INFOLEVEL + ACCESS_ATTR_PARMNUM);
  ACCESS_COUNT_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + ACCESS_COUNT_PARMNUM);
  ACCESS_ACCESS_LIST_INFOLEVEL    = (PARMNUM_BASE_INFOLEVEL + ACCESS_ACCESS_LIST_PARMNUM);

{
 ACCESS_LETTERS defines a letter for each bit position in
 the acl_access field of struct access_list.  Note that some
 bits have a corresponding letter of ' ' (space).
}
  ACCESS_LETTERS     = 'RWCXDAP         ';

{ FunctionCode values for I_NetLogonControl. }
const
  NETLOGON_CONTROL_QUERY            = 1;  { No-op: just query }
  NETLOGON_CONTROL_REPLICATE        = 2;  { Force replicate on BDC }
  NETLOGON_CONTROL_SYNCHRONIZE      = 3;  { Force synchronize on BDC }
  NETLOGON_CONTROL_PDC_REPLICATE    = 4;  { Force PDC to broadcast change }
  NETLOGON_CONTROL_REDISCOVER       = 5;  { Force to re-discover trusted domain DCs }
  NETLOGON_CONTROL_TC_QUERY         = 6;  { Query status of specified trusted channel status }
  NETLOGON_CONTROL_TRANSPORT_NOTIFY = 7;  { Notify netlogon that a new transport has come online }
  NETLOGON_CONTROL_FIND_USER        = 8;  { Find named user in trusted domain }

{ Debug function codes }
  NETLOGON_CONTROL_UNLOAD_NETLOGON_DLL=$FFFB;
  NETLOGON_CONTROL_BACKUP_CHANGE_LOG = $FFFC;
  NETLOGON_CONTROL_TRUNCATE_LOG      = $FFFD;
  NETLOGON_CONTROL_SET_DBFLAG        = $FFFE;
  NETLOGON_CONTROL_BREAKPOINT        = $FFFF;

{ Query level 1 for I_NetLogonControl }
type
  PNETLOGON_INFO_1 = ^TNETLOGON_INFO_1;
  TNETLOGON_INFO_1 = record
    netlog1_flags                 : DWord;
    netlog1_pdc_connection_status : NET_API_STATUS;
  end;

  PNETLOGON_INFO_2 = ^TNETLOGON_INFO_2;
  TNETLOGON_INFO_2 =  record
    netlog2_flags                 : DWord;
    netlog2_pdc_connection_status : NET_API_STATUS;
    {$ifdef MIDL_PASS}
    [string] wchar_t * netlog2_trusted_dc_name;
    {$else}
    netlog2_trusted_dc_name       : LPWSTR;
    {$endif}
    netlog2_tc_connection_status  : NET_API_STATUS;
  end;

  PNETLOGON_INFO_3 = ^TNETLOGON_INFO_3;
  TNETLOGON_INFO_3 = record
    netlog3_flags          : DWord;
    netlog3_logon_attempts : DWord;
    netlog3_reserved1      : DWord;
    netlog3_reserved2      : DWord;
    netlog3_reserved3      : DWord;
    netlog3_reserved4      : DWord;
    netlog3_reserved5      : DWord;
  end;

  PNETLOGON_INFO_4 = ^TNETLOGON_INFO_4;
  TNETLOGON_INFO_4 =  record
    {$IFDEF MIDL_PASS}
    [string] wchar_t * netlog4_trusted_dc_name;
    [string] wchar_t * netlog4_trusted_domain_name;
    {$ELSE}
    netlog4_trusted_dc_name     : LPWSTR;
    netlog4_trusted_domain_name : LPWSTR;
    {$ENDIF}
  end;

{ Values of netlog1_flags }
const
  NETLOGON_REPLICATION_NEEDED      = $01;  { Database is out of date }
  NETLOGON_REPLICATION_IN_PROGRESS = $02;  { Replication is happening now }
  NETLOGON_FULL_SYNC_REPLICATION   = $04;  { full sync replication required/progress }
  NETLOGON_REDO_NEEDED             = $08;  { Redo of previous replication needed }

{-------------------- LMALERT.H ---------------------------------------------}
type
  TNetAlertRaise                 = function(AlertEventName: LPCWSTR; Buffer: Pointer; BufferSize: DWord): NET_API_STATUS; stdcall;
  TNetAlertRaiseEx               = function(AlertEventName: LPCWSTR; VariableInfo: Pointer; VariableInfoSize: DWord; ServiceName: LPCWSTR): NET_API_STATUS; stdcall;

type
  PSTD_ALERT = ^TSTD_ALERT;
  TSTD_ALERT = record
    alrt_timestamp    : DWord;
    alrt_eventname    : array[0..EVLEN] of char;
    alrt_servicename  : array[0..SNLEN] of char;
  end;

  PADMIN_OTHER_INFO = ^TADMIN_OTHER_INFO;
  TADMIN_OTHER_INFO = record
    alrtad_errcode    : DWord;
    alrtad_numstrings : DWord;
  end;

  PERRLOG_OTHER_INFO = ^TERRLOG_OTHER_INFO;
  TERRLOG_OTHER_INFO = record
    alrter_errcode    : DWord;
    alrter_offset     : DWord;
  end;

  PPRINT_OTHER_INFO = ^TPRINT_OTHER_INFO;
  TPRINT_OTHER_INFO = record
    alrtpr_jobid      : DWord;
    alrtpr_status     : DWord;
    alrtpr_submitted  : DWord;
    alrtpr_size       : DWord;
  end;

  PUSER_OTHER_INFO = ^TUSER_OTHER_INFO;
  TUSER_OTHER_INFO = record
    alrtus_errcode    : DWord;
    alrtus_numstrings : DWord;
  end;

{ Name of mailslot to send alert notifications }
const
  ALERTER_MAILSLOT              = '\\.\MAILSLOT\Alerter';

(*
 The following macro gives a pointer to the other_info data.
 It takes an alert structure and returns a pointer to structure
 beyond the standard portion.

 ALERT_OTHER_INFO(x)    ((LPBYTE)(x) + sizeof(STD_ALERT))

 The following macro gives a pointer to the variable-length data.
 It takes a pointer to one of the other-info structs and returns a
 pointer to the variable data portion.

  ALERT_VAR_DATA(p)      ((LPBYTE)(p) + sizeof(*p))
*)

{ Names of standard Microsoft-defined alert events. }
  ALERT_PRINT_EVENT             = 'PRINTING';
  ALERT_MESSAGE_EVENT           = 'MESSAGE';
  ALERT_ERRORLOG_EVENT          = 'ERRORLOG';
  ALERT_ADMIN_EVENT             = 'ADMIN';
  ALERT_USER_EVENT              = 'USER';

{ Bitmap masks for prjob_status field of PRINTJOB. }
{ 2-7 bits also used in device status }
  PRJOB_QSTATUS                 = $3;   { Bits 0,1 }
  PRJOB_DEVSTATUS               = $1fc; { 2-8 bits }
  PRJOB_COMPLETE                = $4;   { Bit 2    }
  PRJOB_INTERV                  = $8;   { Bit 3    }
  PRJOB_ERROR                   = $10;  { Bit 4    }
  PRJOB_DESTOFFLINE             = $20;  { Bit 5    }
  PRJOB_DESTPAUSED              = $40;  { Bit 6    }
  PRJOB_NOTIFY                  = $80;  { Bit 7    }
  PRJOB_DESTNOPAPER             = $100; { Bit 8    }
  PRJOB_DELETED                 = $8000;{ Bit 15   }

{ Values of PRJOB_QSTATUS bits in prjob_status field of PRINTJOB. }
  PRJOB_QS_QUEUED               = 0;
  PRJOB_QS_PAUSED               = 1;
  PRJOB_QS_SPOOLING             = 2;
  PRJOB_QS_PRINTING             = 3;

{-------------------- LMSHARE.H ---------------------------------------------}
type
  TNetShareAdd                   = function(ServerName: LPWSTR; Level: DWORD; Buffer: Pointer; var ParmErr: DWORD): NET_API_STATUS; stdcall;
  TNetShareEnum                  = function(ServerName: LPWSTR; Level: DWORD; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; var ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetShareEnumSticky            = function(ServerName: LPWSTR; Level: DWORD; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetShareGetInfo               = function(ServerName, NetName: LPWSTR; Level: DWORD; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetShareSetInfo               = function(ServerName, NetName: LPWSTR; Level: DWORD; var Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetShareDel                   = function(ServerName, NetName: LPWSTR; Reserved: DWord): NET_API_STATUS; stdcall;
  TNetShareDelSticky             = function(ServerName, NetName: LPWSTR; Reserved: DWord): NET_API_STATUS; stdcall;
  TNetShareCheck                 = function(ServerName, Device: LPWSTR; var ShareType: DWord): NET_API_STATUS; stdcall;

  TNetSessionEnum                = function(ServerName, UncClientName, UserName: LPWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetSessionDel                 = function(ServerName, UncClientName, UserName: LPWSTR): NET_API_STATUS; stdcall;
  TNetSessionGetInfo             = function(ServerName, UncClientName, UserName: LPWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;

  TNetConnectionEnum             = function(ServerName, Qualifier: LPWSTR; Level: DWord; var Buffer : Pointer; PrefMaxLen : DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;

  TNetFileClose                  = function(ServerName: LPWSTR; FileID: DWord): NET_API_STATUS; stdcall;
  TNetFileEnum                   = function(ServerName, BasePath, UserName: LPWSTR; Level: DWord; var Buffer: Pointer; PrefMexLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetFileGetInfo                = function(ServerName: LPWSTR; FileID, Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;

type
  PShare_Info_0 = ^TShare_Info_0;
  TShare_Info_0  = record
    shi0_netname: LPWSTR;
  end;

  PShare_Info_1 = ^TShare_Info_1;
  TShare_Info_1  = record
    shi1_netname: LPWSTR;
    shi1_type: DWord;
    shi1_remark: LPWSTR;
  end;

  PShare_Info_2 = ^TShare_Info_2;
  TShare_Info_2  = record
    shi2_netname: LPWSTR;
    shi2_type: DWord;
    shi2_remark: LPWSTR;
    shi2_permissions: DWord;
    shi2_max_uses: DWord;
    shi2_current_uses: DWord;
    shi2_path: LPWSTR;
    shi2_passwd: LPWSTR;
  end;

  PShare_Info_501 = ^TShare_Info_501;
  TShare_Info_501  = record
    shi501_netname: LPWSTR;
    shi501_type: DWord;
    shi501_remark: LPWSTR;
    shi501_flags: DWord;
  end;

  PShare_Info_502 = ^TShare_Info_502;
  TShare_Info_502  = record
    shi502_netname: LPWSTR;
    shi502_type: DWord;
    shi502_remark: LPWSTR;
    shi502_permissions: DWord;
    shi502_max_uses: DWord;
    shi502_current_uses: DWord;
    shi502_path: LPWSTR;
    shi502_passwd: LPWSTR;
    shi502_reserved: DWord;
    shi502_security_descriptor: PSECURITY_DESCRIPTOR;
  end;

  PShare_Info_1004 = ^TShare_Info_1004;
  TShare_Info_1004  = record
    shi1004_remark: LPWSTR;
  end;

  PShare_Info_1005 = ^TShare_Info_1005;
  TShare_Info_1005  = record
    shi1005_flags: DWord;
  end;

  PShare_Info_1006 = ^TShare_Info_1006;
  TShare_Info_1006  = record
    shi1006_max_uses: DWord;
  end;

  PShare_Info_1501 = ^TShare_Info_1501;
  TShare_Info_1501  = record
    shi1501_reserved: DWord;
    shi1501_security_descriptor: PSECURITY_DESCRIPTOR;
  end;

{ Special Values and Constants - Share }

{ Values for parm_err parameter. }
const
  SHARE_NETNAME_PARMNUM        = 1;
  SHARE_TYPE_PARMNUM           = 3;
  SHARE_REMARK_PARMNUM         = 4;
  SHARE_PERMISSIONS_PARMNUM    = 5;
  SHARE_MAX_USES_PARMNUM       = 6;
  SHARE_CURRENT_USES_PARMNUM   = 7;
  SHARE_PATH_PARMNUM           = 8;
  SHARE_PASSWD_PARMNUM         = 9;
  SHARE_FILE_SD_PARMNUM        = 501;

{ Single-field infolevels for NetShareSetInfo. }
  SHARE_REMARK_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + SHARE_REMARK_PARMNUM);
  SHARE_MAX_USES_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + SHARE_MAX_USES_PARMNUM);
  SHARE_FILE_SD_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + SHARE_FILE_SD_PARMNUM);

  SHI1_NUM_ELEMENTS            = 4;
  SHI2_NUM_ELEMENTS            = 10;

{ Share types (shi1_type and shi2_type fields). }
  STYPE_DISKTREE               = 0;
  STYPE_PRINTQ                 = 1;
  STYPE_DEVICE                 = 2;
  STYPE_IPC                    = 3;

  STYPE_SPECIAL                = $80000000;

  SHI_USES_UNLIMITED           = DWord(-1);

{ Flags values for the 501 and 1005 levels }
  SHI1005_FLAGS_DFS            = $01;    { Share is in the DFS }
  SHI1005_FLAGS_DFS_ROOT       = $02;    { Share is root of DFS}

  CSC_MASK                     = $30;    { Used to mask off the following states}

  CSC_CACHE_MANUAL_REINT       = $00;    { No automatic file by file reintegration}
  CSC_CACHE_AUTO_REINT         = $10;    { File by file reintegration is OK}
  CSC_CACHE_VDO                = $20;    { no need to flow opens}
  CSC_CACHE_NONE               = $30;    { no CSC for this share}

{ The subset of 1005 infolevel flags that can be set via the API }
  SHI1005_VALID_FLAGS_SET      = CSC_MASK;

type
  PSession_Info_0 = ^TSession_Info_0;
  TSession_Info_0 = record
    sesi0_cname: LPWSTR;              { client name (no backslashes) }
  end;

  PSession_Info_1 = ^TSession_Info_1;
  TSession_Info_1 = record
    sesi1_cname: LPWSTR;              { client name (no backslashes) }
    sesi1_username: LPWSTR;
    sesi1_num_opens: DWord;
    sesi1_time: DWord;
    sesi1_idle_time: DWord;
    sesi1_user_flags: DWord;
  end;

  PSession_Info_2 = ^TSession_Info_2;
  TSession_Info_2 = record
    sesi2_cname: LPWSTR;              { client name (no backslashes) }
    sesi2_username: LPWSTR;
    sesi2_num_opens: DWord;
    sesi2_time: DWord;
    sesi2_idle_time: DWord;
    sesi2_user_flags: DWord;
    sesi2_cltype_name: LPWSTR;
  end;

  PSession_Info_10 = ^TSession_Info_10;
  TSession_Info_10 = record
    sesi10_cname: LPWSTR;             { client name (no backslashes) }
    sesi10_username: LPWSTR;
    sesi10_time: DWord;
    sesi10_idle_time: DWord;
  end;

  PSession_Info_502 = ^TSession_Info_502;
  TSession_Info_502 = record
    sesi502_cname: LPWSTR;             { client name (no backslashes) }
    sesi502_username: LPWSTR;
    sesi502_num_opens: DWord;
    sesi502_time: DWord;
    sesi502_idle_time: DWord;
    sesi502_user_flags: DWord;
    sesi502_cltype_name: LPWSTR;
    sesi502_transport: LPWSTR;
  end;

{ Special Values and Constants - Session }

{ Bits defined in sesi1_user_flags. }
const
  SESS_GUEST          = $00000001;  { session is logged on as a guest }
  SESS_NOENCRYPTION   = $00000002;  { session is not using encryption }

  SESI1_NUM_ELEMENTS  = 8;
  SESI2_NUM_ELEMENTS  = 9;

type
  PConnection_Info_0 = ^TConnection_Info_0;
  TConnection_Info_0 = record
    coni0_id: DWord;
  end;

  PConnection_Info_1 = ^TConnection_Info_1;
  TConnection_Info_1 = record
    coni1_id: DWord;
    coni1_type: DWord;
    coni1_num_opens: DWord;
    coni1_num_users: DWord;
    coni1_time: DWord;
    coni1_username: LPWSTR;
    coni1_netname: LPWSTR;
  end;

{ File APIs are available at information levels 2 & 3 only. Levels 0 & }
{ 1 are not supported. }
type
  PFile_Info_2 = ^TFile_Info_2;
  TFile_Info_2 = record
    fi2_id: DWord;
  end;

  PFile_Info_3 = ^TFile_Info_3;
  TFile_Info_3 = record
    fi3_id: DWord;
    fi3_permissions: DWord;
    fi3_num_locks: DWord;
    fi3_pathname: LPWSTR;
    fi3_username: LPWSTR;
  end;

{ Special Values and Constants - File }

{ bit values for permissions }
const
  PERM_FILE_READ      = $1; { user has read access  }
  PERM_FILE_WRITE     = $2; { user has write access }
  PERM_FILE_CREATE    = $4; { user has create access }

{-------------------- LMMSG.H -----------------------------------------------}
type
  TNetMessageNameAdd             = function(ServerName, MsgName: LPCWSTR): NET_API_STATUS; stdcall;
  TNetMessageNameEnum            = function(ServerName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ResumeHandle: PDWord): NET_API_STATUS; stdcall;
  TNetMessageNameGetInfo         = function(ServerName, MsgName: LPCWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetMessageNameDel             = function(ServerName, MsgName: LPCWSTR): NET_API_STATUS; stdcall;
  TNetMessageBufferSend          = function(ServerName, MsgName, FromName: LPCWSTR; Buffer: Pointer; BufferLen: DWord): NET_API_STATUS; stdcall;

type
  PMSG_INFO_0 = ^TMSG_INFO_0;
  TMSG_INFO_0 = record
    msgi0_name         : LPWSTR;
  end;

  PMSG_INFO_1 = ^TMSG_INFO_1;
  TMSG_INFO_1 = record
    msgi1_name         : LPWSTR;
    msgi1_forward_flag : DWord;
    msgi1_forward      : LPWSTR;
  end;

{ Special Values and Constants }

{ Values for msgi1_forward_flag. }
const
  MSGNAME_NOT_FORWARDED  = 0;      { Name not forwarded }
  MSGNAME_FORWARDED_TO   = $04;    { Name forward to remote station }
  MSGNAME_FORWARDED_FROM = $10;    { Name forwarded from remote station }

{-------------------- LMREMUTL.H --------------------------------------------}
type
  TNetRemoteTOD                  = function(UncServerName: LPCWSTR; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetRemoteComputerSupports     = function(UncServerName: LPCWSTR; OptionsWanted: DWord; var OptionsSupported: DWord): NET_API_STATUS; stdcall;

  T_TIME_OF_DAY_INFO = record
    tod_elapsedt : DWord;
    tod_msecs : DWord;
    tod_hours : DWord;
    tod_mins : DWord;
    tod_secs : DWord;
    tod_hunds : DWord;
    tod_timezone : LongInt;
    tod_tinterval : DWord;
    tod_day : DWord;
    tod_month : DWord;
    tod_year : DWord;
    tod_weekday : DWord;
  end;

{ Special Values and Constants }

const
{ Mask bits for use with NetRemoteComputerSupports: }
  SUPPORTS_REMOTE_ADMIN_PROTOCOL  = $00000002;
  SUPPORTS_RPC                    = $00000004;
  SUPPORTS_SAM_PROTOCOL           = $00000008;
  SUPPORTS_UNICODE                = $00000010;
  SUPPORTS_LOCAL                  = $00000020;
  SUPPORTS_ANY                    = $FFFFFFFF;


{-------------------- LMSERVER.H --------------------------------------------}
type
  TNetServerEnum                 = function(ServerName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ServerType: DWord; Domain: LPCWSTR; var ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetServerEnumEx               = function(ServerName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ServerType: DWord; Domain, FirstNameToReturn : LPWSTR): NET_API_STATUS; stdcall;
  TNetServerGetInfo              = function(ServerName: LPCWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetServerSetInfo              = function(ServerName: LPCWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetServerDiskEnum             = function(ServerName: LPCWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetServerComputerNameAdd      = function(ServerName, EmulatedDomainName, EmulatedServerName: LPWSTR): NET_API_STATUS; stdcall;
  TNetServerComputerNameDel      = function(ServerName, EmulatedServerName: LPWSTR): NET_API_STATUS; stdcall;
  TNetServerTransportAdd         = function(ServerName: LPWSTR; Level: DWord; Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetServerTransportAddEx       = function(ServerName: LPWSTR; Level: DWord; Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetServerTransportDel         = function(ServerName: LPWSTR; Level: DWord; Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetServerTransportEnum        = function(ServerName: LPWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;

type
  TSERVER_INFO_100 = record
    sv100_platform_id                 : DWord;
    sv100_name                        : LPWSTR;
  end;

  TSERVER_INFO_101 = record
    sv101_platform_id                 : DWord;
    sv101_name                        : LPWSTR;
    sv101_version_major               : DWord;
    sv101_version_minor               : DWord;
    sv101_type                        : DWord;
    sv101_comment                     : LPWSTR;
  end;

  TSERVER_INFO_102 = record
    sv102_platform_id                 : DWord;
    sv102_name                        : LPWSTR;
    sv102_version_major               : DWord;
    sv102_version_minor               : DWord;
    sv102_type                        : DWord;
    sv102_comment                     : LPWSTR;
    sv102_users                       : DWord;
    sv102_disc                        : LongInt;
    sv102_hidden                      : Bool;
    sv102_announce                    : DWord;
    sv102_anndelta                    : DWord;
    sv102_licenses                    : DWord;
    sv102_userpath                    : LPWSTR;
  end;

  TSERVER_INFO_402 = record
    sv402_ulist_mtime                 : DWord;
    sv402_glist_mtime                 : DWord;
    sv402_alist_mtime                 : DWord;
    sv402_alerts                      : LPWSTR;
    sv402_security                    : DWord;
    sv402_numadmin                    : DWord;
    sv402_lanmask                     : DWord;
    sv402_guestacct                   : LPWSTR;
    sv402_chdevs                      : DWord;
    sv402_chdevq                      : DWord;
    sv402_chdevjobs                   : DWord;
    sv402_connections                 : DWord;
    sv402_shares                      : DWord;
    sv402_openfiles                   : DWord;
    sv402_sessopens                   : DWord;
    sv402_sessvcs                     : DWord;
    sv402_sessreqs                    : DWord;
    sv402_opensearch                  : DWord;
    sv402_activelocks                 : DWord;
    sv402_numreqbuf                   : DWord;
    sv402_sizreqbuf                   : DWord;
    sv402_numbigbuf                   : DWord;
    sv402_numfiletasks                : DWord;
    sv402_alertsched                  : DWord;
    sv402_erroralert                  : DWord;
    sv402_logonalert                  : DWord;
    sv402_accessalert                 : DWord;
    sv402_diskalert                   : DWord;
    sv402_netioalert                  : DWord;
    sv402_maxauditsz                  : DWord;
    sv402_srvheuristics               : LPWSTR;
 end;

 TSERVER_INFO_403 = record
    sv403_ulist_mtime                 : DWord;
    sv403_glist_mtime                 : DWord;
    sv403_alist_mtime                 : DWord;
    sv403_alerts                      : LPWSTR;
    sv403_security                    : DWord;
    sv403_numadmin                    : DWord;
    sv403_lanmask                     : DWord;
    sv403_guestacct                   : LPWSTR;
    sv403_chdevs                      : DWord;
    sv403_chdevq                      : DWord;
    sv403_chdevjobs                   : DWord;
    sv403_connections                 : DWord;
    sv403_shares                      : DWord;
    sv403_openfiles                   : DWord;
    sv403_sessopens                   : DWord;
    sv403_sessvcs                     : DWord;
    sv403_sessreqs                    : DWord;
    sv403_opensearch                  : DWord;
    sv403_activelocks                 : DWord;
    sv403_numreqbuf                   : DWord;
    sv403_sizreqbuf                   : DWord;
    sv403_numbigbuf                   : DWord;
    sv403_numfiletasks                : DWord;
    sv403_alertsched                  : DWord;
    sv403_erroralert                  : DWord;
    sv403_logonalert                  : DWord;
    sv403_accessalert                 : DWord;
    sv403_diskalert                   : DWord;
    sv403_netioalert                  : DWord;
    sv403_maxauditsz                  : DWord;
    sv403_srvheuristics               : LPWSTR;
    sv403_auditedevents               : DWord;
    sv403_autoprofile                 : DWord;
    sv403_autopath                    : LPWSTR;
  end;

  TSERVER_INFO_502 = record
    sv502_sessopens                   : DWord;
    sv502_sessvcs                     : DWord;
    sv502_opensearch                  : DWord;
    sv502_sizreqbuf                   : DWord;
    sv502_initworkitems               : DWord;
    sv502_maxworkitems                : DWord;
    sv502_rawworkitems                : DWord;
    sv502_irpstacksize                : DWord;
    sv502_maxrawbuflen                : DWord;
    sv502_sessusers                   : DWord;
    sv502_sessconns                   : DWord;
    sv502_maxpagedmemoryusage         : DWord;
    sv502_maxnonpagedmemoryusage      : DWord;
    sv502_enablesoftcompat            : Bool;
    sv502_enableforcedlogoff          : Bool;
    sv502_timesource                  : Bool;
    sv502_acceptdownlevelapis         : Bool;
    sv502_lmannounce                  : Bool;
  end;

  TSERVER_INFO_503 = record
    sv503_sessopens                   : DWord;
    sv503_sessvcs                     : DWord;
    sv503_opensearch                  : DWord;
    sv503_sizreqbuf                   : DWord;
    sv503_initworkitems               : DWord;
    sv503_maxworkitems                : DWord;
    sv503_rawworkitems                : DWord;
    sv503_irpstacksize                : DWord;
    sv503_maxrawbuflen                : DWord;
    sv503_sessusers                   : DWord;
    sv503_sessconns                   : DWord;
    sv503_maxpagedmemoryusage         : DWord;
    sv503_maxnonpagedmemoryusage      : DWord;
    sv503_enablesoftcompat            : Bool;
    sv503_enableforcedlogoff          : Bool;
    sv503_timesource                  : Bool;
    sv503_acceptdownlevelapis         : Bool;
    sv503_lmannounce                  : Bool;
    sv503_domain                      : LPWSTR;
    sv503_maxcopyreadlen              : DWord;
    sv503_maxcopywritelen             : DWord;
    sv503_minkeepsearch               : DWord;
    sv503_maxkeepsearch               : DWord;
    sv503_minkeepcomplsearch          : DWord;
    sv503_maxkeepcomplsearch          : DWord;
    sv503_threadcountadd              : DWord;
    sv503_numblockthreads             : DWord;
    sv503_scavtimeout                 : DWord;
    sv503_minrcvqueue                 : DWord;
    sv503_minfreeworkitems            : DWord;
    sv503_xactmemsize                 : DWord;
    sv503_threadpriority              : DWord;
    sv503_maxmpxct                    : DWord;
    sv503_oplockbreakwait             : DWord;
    sv503_oplockbreakresponsewait     : DWord;
    sv503_enableoplocks               : Bool;
    sv503_enableoplockforceclose      : Bool;
    sv503_enablefcbopens              : Bool;
    sv503_enableraw                   : Bool;
    sv503_enablesharednetdrives       : Bool;
    sv503_minfreeconnections          : DWord;
    sv503_maxfreeconnections          : DWord;
  end;

  TSERVER_INFO_599 = record
    sv599_sessopens                   : DWord;
    sv599_sessvcs                     : DWord;
    sv599_opensearch                  : DWord;
    sv599_sizreqbuf                   : DWord;
    sv599_initworkitems               : DWord;
    sv599_maxworkitems                : DWord;
    sv599_rawworkitems                : DWord;
    sv599_irpstacksize                : DWord;
    sv599_maxrawbuflen                : DWord;
    sv599_sessusers                   : DWord;
    sv599_sessconns                   : DWord;
    sv599_maxpagedmemoryusage         : DWord;
    sv599_maxnonpagedmemoryusage      : DWord;
    sv599_enablesoftcompat            : Bool;
    sv599_enableforcedlogoff          : Bool;
    sv599_timesource                  : Bool;
    sv599_acceptdownlevelapis         : Bool;
    sv599_lmannounce                  : Bool;
    sv599_domain                      : LPWSTR;
    sv599_maxcopyreadlen              : DWord;
    sv599_maxcopywritelen             : DWord;
    sv599_minkeepsearch               : DWord;
    sv599_maxkeepsearch               : DWord;
    sv599_minkeepcomplsearch          : DWord;
    sv599_maxkeepcomplsearch          : DWord;
    sv599_threadcountadd              : DWord;
    sv599_numblockthreads             : DWord;
    sv599_scavtimeout                 : DWord;
    sv599_minrcvqueue                 : DWord;
    sv599_minfreeworkitems            : DWord;
    sv599_xactmemsize                 : DWord;
    sv599_threadpriority              : DWord;
    sv599_maxmpxct                    : DWord;
    sv599_oplockbreakwait             : DWord;
    sv599_oplockbreakresponsewait     : DWord;
    sv599_enableoplocks               : Bool;
    sv599_enableoplockforceclose      : Bool;
    sv599_enablefcbopens              : Bool;
    sv599_enableraw                   : Bool;
    sv599_enablesharednetdrives       : Bool;
    sv599_minfreeconnections          : DWord;
    sv599_maxfreeconnections          : DWord;
    sv599_initsesstable               : DWord;
    sv599_initconntable               : DWord;
    sv599_initfiletable               : DWord;
    sv599_initsearchtable             : DWord;
    sv599_alertschedule               : DWord;
    sv599_errorthreshold              : DWord;
    sv599_networkerrorthreshold       : DWord;
    sv599_diskspacethreshold          : DWord;
    sv599_reserved                    : DWord;
    sv599_maxlinkdelay                : DWord;
    sv599_minlinkthroughput           : DWord;
    sv599_linkinfovalidtime           : DWord;
    sv599_scavqosinfoupdatetime       : DWord;
    sv599_maxworkitemidletime         : DWord;
  end;

  TSERVER_INFO_598 = record
    sv598_maxrawworkitems             : DWord;
    sv598_maxthreadsperqueue          : DWord;
    sv598_producttype                 : DWord;
    sv598_serversize                  : DWord;
    sv598_connectionlessautodisc      : DWord;
    sv598_sharingviolationretries     : DWord;
    sv598_sharingviolationdelay       : DWord;
    sv598_maxglobalopensearch         : DWord;
    sv598_removeduplicatesearches     : DWord;
    sv598_lockviolationoffset         : DWord;
    sv598_lockviolationdelay          : DWord;
    sv598_mdlreadswitchover           : DWord;
    sv598_cachedopenlimit             : DWord;
    sv598_otherqueueaffinity          : DWord;
    sv598_restrictnullsessaccess      : Bool;
    sv598_enablewfw311directipx       : Bool;
    sv598_queuesamplesecs             : DWord;
    sv598_balancecount                : DWord;
    sv598_preferredaffinity           : DWord;
    sv598_maxfreerfcbs                : DWord;
    sv598_maxfreemfcbs                : DWord;
    sv598_maxfreelfcbs                : DWord;
    sv598_maxfreepagedpoolchunks      : DWord;
    sv598_minpagedpoolchunksize       : DWord;
    sv598_maxpagedpoolchunksize       : DWord;
    sv598_sendsfrompreferredprocesso  : Bool;
    sv598_cacheddirectorylimit        : DWord;
    sv598_maxcopylength               : DWord;
    sv598_enablebulktransfer          : Bool;
    sv598_enablecompression           : Bool;
    sv598_autosharewks                : Bool;
    sv598_autoshareserver             : Bool;
  end;

  TSERVER_INFO_1005 = record
    sv1005_comment                    : LPWSTR;
  end;

  TSERVER_INFO_1107 = record
    sv1107_users                      : DWord;
  end;

  TSERVER_INFO_1010 = record
    sv1010_disc                       : LongInt;
  end;

  TSERVER_INFO_1016 = record
    sv1016_hidden                     : Bool;
  end;

  TSERVER_INFO_1017 = record
    sv1017_announce                   : DWord;
  end;

  TSERVER_INFO_1018 = record
    sv1018_anndelta                   : DWord;
  end;

  TSERVER_INFO_1501 = record
    sv1501_sessopens                  : DWord;
  end;

  TSERVER_INFO_1502 = record
    sv1502_sessvcs                    : DWord;
  end;

  TSERVER_INFO_1503 = record
    sv1503_opensearch                 : DWord;
  end;

  TSERVER_INFO_1506 = record
    sv1506_maxworkitems               : DWord;
  end;

  TSERVER_INFO_1509 = record
    sv1509_maxrawbuflen               : DWord;
  end;

  TSERVER_INFO_1510 = record
    sv1510_sessusers                  : DWord;
  end;

  TSERVER_INFO_1511 = record
    sv1511_sessconns                  : DWord;
  end;

  TSERVER_INFO_1512 = record
    sv1512_maxnonpagedmemoryusage     : DWord;
  end;

  TSERVER_INFO_1513 = record
    sv1513_maxpagedmemoryusage        : DWord;
  end;

  TSERVER_INFO_1514 = record
    sv1514_enablesoftcompat           : Bool;
  end;

  TSERVER_INFO_1515 = record
    sv1515_enableforcedlogoff         : Bool;
  end;

  TSERVER_INFO_1516 = record
    sv1516_timesource                 : Bool;
  end;

  TSERVER_INFO_1518 = record
    sv1518_lmannounce                 : Bool;
  end;

  TSERVER_INFO_1520 = record
    sv1520_maxcopyreadlen             : DWord;
  end;

  TSERVER_INFO_1521 = record
    sv1521_maxcopywritelen            : DWord;
  end;

  TSERVER_INFO_1522 = record
    sv1522_minkeepsearch              : DWord;
  end;

  TSERVER_INFO_1523 = record
    sv1523_maxkeepsearch              : DWord;
  end;

  TSERVER_INFO_1524 = record
    sv1524_minkeepcomplsearch         : DWord;
  end;

  TSERVER_INFO_1525 = record
    sv1525_maxkeepcomplsearch         : DWord;
  end;

  TSERVER_INFO_1528 = record
    sv1528_scavtimeout                : DWord;
  end;

  TSERVER_INFO_1529 = record
    sv1529_minrcvqueue                : DWord;
  end;

  TSERVER_INFO_1530 = record
    sv1530_minfreeworkitems           : DWord;
  end;

  TSERVER_INFO_1533 = record
    sv1533_maxmpxct                   : DWord;
  end;

  TSERVER_INFO_1534 = record
    sv1534_oplockbreakwait            : DWord;
  end;

  TSERVER_INFO_1535 = record
    sv1535_oplockbreakresponsewait    : DWord;
  end;

  TSERVER_INFO_1536 = record
    sv1536_enableoplocks              : Bool;
  end;

  TSERVER_INFO_1537 = record
    sv1537_enableoplockforceclose     : Bool;
  end;

  TSERVER_INFO_1538 = record
    sv1538_enablefcbopens             : Bool;
  end;

  TSERVER_INFO_1539 = record
    sv1539_enableraw                  : Bool;
  end;

  TSERVER_INFO_1540 = record
    sv1540_enablesharednetdrives      : Bool;
  end;

  TSERVER_INFO_1541 = record
    sv1541_minfreeconnections         : Bool;
  end;

  TSERVER_INFO_1542 = record
    sv1542_maxfreeconnections         : Bool;
  end;

  TSERVER_INFO_1543 = record
    sv1543_initsesstable              : DWord;
  end;

  TSERVER_INFO_1544 = record
    sv1544_initconntable              : DWord;
  end;

  TSERVER_INFO_1545 = record
    sv1545_initfiletable              : DWord;
  end;

  TSERVER_INFO_1546 = record
    sv1546_initsearchtable            : DWord;
  end;

  TSERVER_INFO_1547 = record
    sv1547_alertschedule              : DWord;
  end;

  TSERVER_INFO_1548 = record
    sv1548_errorthreshold             : DWord;
  end;

  TSERVER_INFO_1549 = record
    sv1549_networkerrorthreshold      : DWord;
  end;

  TSERVER_INFO_1550 = record
    sv1550_diskspacethreshold         : DWord;
  end;

  TSERVER_INFO_1552 = record
    sv1552_maxlinkdelay               : DWord;
  end;

  TSERVER_INFO_1553 = record
    sv1553_minlinkthroughput          : DWord;
  end;

  TSERVER_INFO_1554 = record
    sv1554_linkinfovalidtime          : DWord;
  end;

  TSERVER_INFO_1555 = record
    sv1555_scavqosinfoupdatetime      : DWord;
  end;

  TSERVER_INFO_1556 = record
    sv1556_maxworkitemidletime        : DWord;
  end;

  TSERVER_INFO_1557 = record
    sv1557_maxrawworkitems            : DWord;
  end;

  TSERVER_INFO_1560 = record
    sv1560_producttype                : DWord;
  end;

  TSERVER_INFO_1561 = record
    sv1561_serversize                 : DWord;
  end;

  TSERVER_INFO_1562 = record
    sv1562_connectionlessautodisc     : DWord;
  end;

  TSERVER_INFO_1563 = record
    sv1563_sharingviolationretries    : DWord;
  end;

  TSERVER_INFO_1564 = record
    sv1564_sharingviolationdelay      : DWord;
  end;

  TSERVER_INFO_1565 = record
    sv1565_maxglobalopensearch        : DWord;
  end;

  TSERVER_INFO_1566 = record
    sv1566_removeduplicatesearches    : Bool;
  end;

  TSERVER_INFO_1567 = record
    sv1567_lockviolationretries       : DWord;
  end;

  TSERVER_INFO_1568 = record
    sv1568_lockviolationoffset        : DWord;
  end;

  TSERVER_INFO_1569 = record
    sv1569_lockviolationdelay         : DWord;
  end;

  TSERVER_INFO_1570 = record
    sv1570_mdlreadswitchover          : DWord;
  end;

  TSERVER_INFO_1571 = record
    sv1571_cachedopenlimit            : DWord;
  end;

  TSERVER_INFO_1572 = record
    sv1572_criticalthreads            : DWord;
  end;

  TSERVER_INFO_1573 = record
    sv1573_restrictnullsessaccess     : DWord;
  end;

  TSERVER_INFO_1574 = record
    sv1574_enablewfw311directipx      : DWord;
  end;

  TSERVER_INFO_1575 = record
    sv1575_otherqueueaffinity         : DWord;
  end;

  TSERVER_INFO_1576 = record
    sv1576_queuesamplesecs            : DWord;
  end;

  TSERVER_INFO_1577 = record
    sv1577_balancecount               : DWord;
  end;

  TSERVER_INFO_1578 = record
    sv1578_preferredaffinity          : DWord;
  end;

  TSERVER_INFO_1579 = record
    sv1579_maxfreerfcbs               : DWord;
  end;

  TSERVER_INFO_1580 = record
    sv1580_maxfreemfcbs               : DWord;
  end;

  TSERVER_INFO_1581 = record
    sv1581_maxfreemlcbs               : DWord;
  end;

  TSERVER_INFO_1582 = record
    sv1582_maxfreepagedpoolchunks     : DWord;
  end;

  TSERVER_INFO_1583 = record
    sv1583_minpagedpoolchunksize      : DWord;
  end;

  TSERVER_INFO_1584 = record
    sv1584_maxpagedpoolchunksize      : DWord;
  end;

  TSERVER_INFO_1585 = record
    sv1585_sendsfrompreferredprocessor: Bool;
  end;

  TSERVER_INFO_1586 = record
    sv1586_maxthreadsperqueue         : DWord;
  end;

  TSERVER_INFO_1587 = record
    sv1587_cacheddirectorylimit       : DWord;
  end;

  TSERVER_INFO_1588 = record
    sv1588_maxcopylength              : DWord;
  end;

  TSERVER_INFO_1589 = record
    sv1589_enablebulktransfer         : DWord;
  end;

  TSERVER_INFO_1590 = record
    sv1590_enablecompression          : DWord;
  end;

  TSERVER_INFO_1591 = record
    sv1591_autosharewks               : DWord;
  end;

  TSERVER_INFO_1592 = record
    sv1592_autoshareserver            : DWord;
  end;

(*
 A special structure definition is required in order for this
 structure to work with RPC.  The problem is that having addresslength
 indicate the number of bytes in address means that RPC must know the
 link between the two.
*)
  TSERVER_TRANSPORT_INFO_0 = record
    svti0_numberofvcs                 : DWord;
    svti0_transportname               : LPSTR;
    svti0_transportaddress            : Pointer;
    svti0_transportaddresslength      : DWord;
    svti0_networkaddress              : LPWSTR;
  end;

  TSERVER_TRANSPORT_INFO_1 = record
    svti1_numberofvcs                 : DWord;
    svti1_transportname               : LPWSTR;
    svti1_transportaddress            : Pointer;
    svti1_transportaddresslength      : DWord;
    svti1_networkaddress              : LPWSTR;
    svti1_domain                      : LPWSTR;
  end;

const
{ The platform ID indicates the levels to use for platform-specific information. }
  SV_PLATFORM_ID_OS2                       = 400;
  SV_PLATFORM_ID_NT                        = 500;

{ Mask to be applied to svX_version_major in order to obtain the major version number. }
  MAJOR_VERSION_MASK                       = $0F;

{ Bit-mapped values for svX_type fields. X = 1, 2 or 3. }
  SV_TYPE_WORKSTATION                      = $00000001;
  SV_TYPE_SERVER                           = $00000002;
  SV_TYPE_SQLSERVER                        = $00000004;
  SV_TYPE_DOMAIN_CTRL                      = $00000008;
  SV_TYPE_DOMAIN_BAKCTRL                   = $00000010;
  SV_TYPE_TIME_SOURCE                      = $00000020;
  SV_TYPE_AFP                              = $00000040;
  SV_TYPE_NOVELL                           = $00000080;
  SV_TYPE_DOMAIN_MEMBER                    = $00000100;
  SV_TYPE_PRINTQ_SERVER                    = $00000200;
  SV_TYPE_DIALIN_SERVER                    = $00000400;
  SV_TYPE_XENIX_SERVER                     = $00000800;
  SV_TYPE_SERVER_UNIX                      = SV_TYPE_XENIX_SERVER;
  SV_TYPE_NT                               = $00001000;
  SV_TYPE_WFW                              = $00002000;
  SV_TYPE_SERVER_MFPN                      = $00004000;
  SV_TYPE_SERVER_NT                        = $00008000;
  SV_TYPE_POTENTIAL_BROWSER                = $00010000;
  SV_TYPE_BACKUP_BROWSER                   = $00020000;
  SV_TYPE_MASTER_BROWSER                   = $00040000;
  SV_TYPE_DOMAIN_MASTER                    = $00080000;
  SV_TYPE_SERVER_OSF                       = $00100000;
  SV_TYPE_SERVER_VMS                       = $00200000;
  SV_TYPE_WINDOWS                          = $00400000;
  SV_TYPE_DFS                              = $00800000;
  SV_TYPE_CLUSTER_NT                       = $01000000;
  SV_TYPE_TERMINALSERVER                   = $02000000;
  SV_TYPE_DCE                              = $10000000;
  SV_TYPE_ALTERNATE_XPORT                  = $20000000;
  SV_TYPE_LOCAL_LIST_ONLY                  = $40000000;
  SV_TYPE_DOMAIN_ENUM                      = $80000000;
  SV_TYPE_ALL                              = $FFFFFFFF;

{ Special value for sv102_disc that specifies infinite disconnect time. }
  SV_NODISC                                = (-1); { No autodisconnect timeout enforced }

{ Values of svX_security field. X          = 2 or 3. }
  SV_USERSECURITY                          = 1;
  SV_SHARESECURITY                         = 0;

{ Values of svX_hidden field. X            = 2 or 3. }
  SV_HIDDEN                                = 1;
  SV_VISIBLE                               = 0;

{ Values for ParmError parameter to NetServerSetInfo. }
  SV_PLATFORM_ID_PARMNUM                   = 101;
  SV_NAME_PARMNUM                          = 102;
  SV_VERSION_MAJOR_PARMNUM                 = 103;
  SV_VERSION_MINOR_PARMNUM                 = 104;
  SV_TYPE_PARMNUM                          = 105;
  SV_COMMENT_PARMNUM                       = 5;
  SV_USERS_PARMNUM                         = 107;
  SV_DISC_PARMNUM                          = 10;
  SV_HIDDEN_PARMNUM                        = 16;
  SV_ANNOUNCE_PARMNUM                      = 17;
  SV_ANNDELTA_PARMNUM                      = 18;
  SV_USERPATH_PARMNUM                      = 112;

  SV_ULIST_MTIME_PARMNUM                   = 401;
  SV_GLIST_MTIME_PARMNUM                   = 402;
  SV_ALIST_MTIME_PARMNUM                   = 403;
  SV_ALERTS_PARMNUM                        = 11;
  SV_SECURITY_PARMNUM                      = 405;
  SV_NUMADMIN_PARMNUM                      = 406;
  SV_LANMASK_PARMNUM                       = 407;
  SV_GUESTACC_PARMNUM                      = 408;
  SV_CHDEVQ_PARMNUM                        = 410;
  SV_CHDEVJOBS_PARMNUM                     = 411;
  SV_CONNECTIONS_PARMNUM                   = 412;
  SV_SHARES_PARMNUM                        = 413;
  SV_OPENFILES_PARMNUM                     = 414;
  SV_SESSREQS_PARMNUM                      = 417;
  SV_ACTIVELOCKS_PARMNUM                   = 419;
  SV_NUMREQBUF_PARMNUM                     = 420;
  SV_NUMBIGBUF_PARMNUM                     = 422;
  SV_NUMFILETASKS_PARMNUM                  = 423;
  SV_ALERTSCHED_PARMNUM                    = 37;
  SV_ERRORALERT_PARMNUM                    = 38;
  SV_LOGONALERT_PARMNUM                    = 39;
  SV_ACCESSALERT_PARMNUM                   = 40;
  SV_DISKALERT_PARMNUM                     = 41;
  SV_NETIOALERT_PARMNUM                    = 42;
  SV_MAXAUDITSZ_PARMNUM                    = 43;
  SV_SRVHEURISTICS_PARMNUM                 = 431;

  SV_SESSOPENS_PARMNUM                     = 501;
  SV_SESSVCS_PARMNUM                       = 502;
  SV_OPENSEARCH_PARMNUM                    = 503;
  SV_SIZREQBUF_PARMNUM                     = 504;
  SV_INITWORKITEMS_PARMNUM                 = 505;
  SV_MAXWORKITEMS_PARMNUM                  = 506;
  SV_RAWWORKITEMS_PARMNUM                  = 507;
  SV_IRPSTACKSIZE_PARMNUM                  = 508;
  SV_MAXRAWBUFLEN_PARMNUM                  = 509;
  SV_SESSUSERS_PARMNUM                     = 510;
  SV_SESSCONNS_PARMNUM                     = 511;
  SV_MAXNONPAGEDMEMORYUSAGE_PARMNUM        = 512;
  SV_MAXPAGEDMEMORYUSAGE_PARMNUM           = 513;
  SV_ENABLESOFTCOMPAT_PARMNUM              = 514;
  SV_ENABLEFORCEDLOGOFF_PARMNUM            = 515;
  SV_TIMESOURCE_PARMNUM                    = 516;
  SV_ACCEPTDOWNLEVELAPIS_PARMNUM           = 517;
  SV_LMANNOUNCE_PARMNUM                    = 518;
  SV_DOMAIN_PARMNUM                        = 519;
  SV_MAXCOPYREADLEN_PARMNUM                = 520;
  SV_MAXCOPYWRITELEN_PARMNUM               = 521;
  SV_MINKEEPSEARCH_PARMNUM                 = 522;
  SV_MAXKEEPSEARCH_PARMNUM                 = 523;
  SV_MINKEEPCOMPLSEARCH_PARMNUM            = 524;
  SV_MAXKEEPCOMPLSEARCH_PARMNUM            = 525;
  SV_THREADCOUNTADD_PARMNUM                = 526;
  SV_NUMBLOCKTHREADS_PARMNUM               = 527;
  SV_SCAVTIMEOUT_PARMNUM                   = 528;
  SV_MINRCVQUEUE_PARMNUM                   = 529;
  SV_MINFREEWORKITEMS_PARMNUM              = 530;
  SV_XACTMEMSIZE_PARMNUM                   = 531;
  SV_THREADPRIORITY_PARMNUM                = 532;
  SV_MAXMPXCT_PARMNUM                      = 533;
  SV_OPLOCKBREAKWAIT_PARMNUM               = 534;
  SV_OPLOCKBREAKRESPONSEWAIT_PARMNUM       = 535;
  SV_ENABLEOPLOCKS_PARMNUM                 = 536;
  SV_ENABLEOPLOCKFORCECLOSE_PARMNUM        = 537;
  SV_ENABLEFCBOPENS_PARMNUM                = 538;
  SV_ENABLERAW_PARMNUM                     = 539;
  SV_ENABLESHAREDNETDRIVES_PARMNUM         = 540;
  SV_MINFREECONNECTIONS_PARMNUM            = 541;
  SV_MAXFREECONNECTIONS_PARMNUM            = 542;
  SV_INITSESSTABLE_PARMNUM                 = 543;
  SV_INITCONNTABLE_PARMNUM                 = 544;
  SV_INITFILETABLE_PARMNUM                 = 545;
  SV_INITSEARCHTABLE_PARMNUM               = 546;
  SV_ALERTSCHEDULE_PARMNUM                 = 547;
  SV_ERRORTHRESHOLD_PARMNUM                = 548;
  SV_NETWORKERRORTHRESHOLD_PARMNUM         = 549;
  SV_DISKSPACETHRESHOLD_PARMNUM            = 550;
  SV_MAXLINKDELAY_PARMNUM                  = 552;
  SV_MINLINKTHROUGHPUT_PARMNUM             = 553;
  SV_LINKINFOVALIDTIME_PARMNUM             = 554;
  SV_SCAVQOSINFOUPDATETIME_PARMNUM         = 555;
  SV_MAXWORKITEMIDLETIME_PARMNUM           = 556;
  SV_MAXRAWWORKITEMS_PARMNUM               = 557;
  SV_PRODUCTTYPE_PARMNUM                   = 560;
  SV_SERVERSIZE_PARMNUM                    = 561;
  SV_CONNECTIONLESSAUTODISC_PARMNUM        = 562;
  SV_SHARINGVIOLATIONRETRIES_PARMNUM       = 563;
  SV_SHARINGVIOLATIONDELAY_PARMNUM         = 564;
  SV_MAXGLOBALOPENSEARCH_PARMNUM           = 565;
  SV_REMOVEDUPLICATESEARCHES_PARMNUM       = 566;
  SV_LOCKVIOLATIONRETRIES_PARMNUM          = 567;
  SV_LOCKVIOLATIONOFFSET_PARMNUM           = 568;
  SV_LOCKVIOLATIONDELAY_PARMNUM            = 569;
  SV_MDLREADSWITCHOVER_PARMNUM             = 570;
  SV_CACHEDOPENLIMIT_PARMNUM               = 571;
  SV_CRITICALTHREADS_PARMNUM               = 572;
  SV_RESTRICTNULLSESSACCESS_PARMNUM        = 573;
  SV_ENABLEWFW311DIRECTIPX_PARMNUM         = 574;
  SV_OTHERQUEUEAFFINITY_PARMNUM            = 575;
  SV_QUEUESAMPLESECS_PARMNUM               = 576;
  SV_BALANCECOUNT_PARMNUM                  = 577;
  SV_PREFERREDAFFINITY_PARMNUM             = 578;
  SV_MAXFREERFCBS_PARMNUM                  = 579;
  SV_MAXFREEMFCBS_PARMNUM                  = 580;
  SV_MAXFREELFCBS_PARMNUM                  = 581;
  SV_MAXFREEPAGEDPOOLCHUNKS_PARMNUM        = 582;
  SV_MINPAGEDPOOLCHUNKSIZE_PARMNUM         = 583;
  SV_MAXPAGEDPOOLCHUNKSIZE_PARMNUM         = 584;
  SV_SENDSFROMPREFERREDPROCESSOR_PARMNUM   = 585;
  SV_MAXTHREADSPERQUEUE_PARMNUM            = 586;
  SV_CACHEDDIRECTORYLIMIT_PARMNUM          = 587;
  SV_MAXCOPYLENGTH_PARMNUM                 = 588;
  SV_ENABLEBULKTRANSFER_PARMNUM            = 589;
  SV_ENABLECOMPRESSION_PARMNUM             = 590;
  SV_AUTOSHAREWKS_PARMNUM                  = 591;
  SV_AUTOSHARESERVER_PARMNUM               = 592;

{ Single-field infolevels for NetServerSetInfo. }
  SV_COMMENT_INFOLEVEL                     = (PARMNUM_BASE_INFOLEVEL + SV_COMMENT_PARMNUM);
  SV_USERS_INFOLEVEL                       = (PARMNUM_BASE_INFOLEVEL + SV_USERS_PARMNUM);
  SV_DISC_INFOLEVEL                        = (PARMNUM_BASE_INFOLEVEL + SV_DISC_PARMNUM);
  SV_HIDDEN_INFOLEVEL                      = (PARMNUM_BASE_INFOLEVEL + SV_HIDDEN_PARMNUM);
  SV_ANNOUNCE_INFOLEVEL                    = (PARMNUM_BASE_INFOLEVEL + SV_ANNOUNCE_PARMNUM);
  SV_ANNDELTA_INFOLEVEL                    = (PARMNUM_BASE_INFOLEVEL + SV_ANNDELTA_PARMNUM);
  SV_SESSOPENS_INFOLEVEL                   = (PARMNUM_BASE_INFOLEVEL + SV_SESSOPENS_PARMNUM);
  SV_SESSVCS_INFOLEVEL                     = (PARMNUM_BASE_INFOLEVEL + SV_SESSVCS_PARMNUM);
  SV_OPENSEARCH_INFOLEVEL                  = (PARMNUM_BASE_INFOLEVEL + SV_OPENSEARCH_PARMNUM);
  SV_MAXWORKITEMS_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_MAXWORKITEMS_PARMNUM);
  SV_MAXRAWBUFLEN_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_MAXRAWBUFLEN_PARMNUM);
  SV_SESSUSERS_INFOLEVEL                   = (PARMNUM_BASE_INFOLEVEL + SV_SESSUSERS_PARMNUM);
  SV_SESSCONNS_INFOLEVEL                   = (PARMNUM_BASE_INFOLEVEL + SV_SESSCONNS_PARMNUM);
  SV_MAXNONPAGEDMEMORYUSAGE_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + SV_MAXNONPAGEDMEMORYUSAGE_PARMNUM);
  SV_MAXPAGEDMEMORYUSAGE_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + SV_MAXPAGEDMEMORYUSAGE_PARMNUM);
  SV_ENABLESOFTCOMPAT_INFOLEVEL            = (PARMNUM_BASE_INFOLEVEL + SV_ENABLESOFTCOMPAT_PARMNUM);
  SV_ENABLEFORCEDLOGOFF_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEFORCEDLOGOFF_PARMNUM);
  SV_TIMESOURCE_INFOLEVEL                  = (PARMNUM_BASE_INFOLEVEL + SV_TIMESOURCE_PARMNUM);
  SV_LMANNOUNCE_INFOLEVEL                  = (PARMNUM_BASE_INFOLEVEL + SV_LMANNOUNCE_PARMNUM);
  SV_MAXCOPYREADLEN_INFOLEVEL              = (PARMNUM_BASE_INFOLEVEL + SV_MAXCOPYREADLEN_PARMNUM);
  SV_MAXCOPYWRITELEN_INFOLEVEL             = (PARMNUM_BASE_INFOLEVEL + SV_MAXCOPYWRITELEN_PARMNUM);
  SV_MINKEEPSEARCH_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_MINKEEPSEARCH_PARMNUM);
  SV_MAXKEEPSEARCH_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_MAXKEEPSEARCH_PARMNUM);
  SV_MINKEEPCOMPLSEARCH_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_MINKEEPCOMPLSEARCH_PARMNUM);
  SV_MAXKEEPCOMPLSEARCH_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_MAXKEEPCOMPLSEARCH_PARMNUM);
  SV_SCAVTIMEOUT_INFOLEVEL                 = (PARMNUM_BASE_INFOLEVEL + SV_SCAVTIMEOUT_PARMNUM);
  SV_MINRCVQUEUE_INFOLEVEL                 = (PARMNUM_BASE_INFOLEVEL + SV_MINRCVQUEUE_PARMNUM);
  SV_MINFREEWORKITEMS_INFOLEVEL            = (PARMNUM_BASE_INFOLEVEL + SV_MINFREEWORKITEMS_PARMNUM);
  SV_MAXMPXCT_INFOLEVEL                    = (PARMNUM_BASE_INFOLEVEL + SV_MAXMPXCT_PARMNUM);
  SV_OPLOCKBREAKWAIT_INFOLEVEL             = (PARMNUM_BASE_INFOLEVEL + SV_OPLOCKBREAKWAIT_PARMNUM);
  SV_OPLOCKBREAKRESPONSEWAIT_INFOLEVEL     = (PARMNUM_BASE_INFOLEVEL + SV_OPLOCKBREAKRESPONSEWAIT_PARMNUM);
  SV_ENABLEOPLOCKS_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEOPLOCKS_PARMNUM);
  SV_ENABLEOPLOCKFORCECLOSE_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEOPLOCKFORCECLOSE_PARMNUM);
  SV_ENABLEFCBOPENS_INFOLEVEL              = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEFCBOPENS_PARMNUM);
  SV_ENABLERAW_INFOLEVEL                   = (PARMNUM_BASE_INFOLEVEL + SV_ENABLERAW_PARMNUM);
  SV_ENABLESHAREDNETDRIVES_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + SV_ENABLESHAREDNETDRIVES_PARMNUM);
  SV_MINFREECONNECTIONS_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_MINFREECONNECTIONS_PARMNUM);
  SV_MAXFREECONNECTIONS_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREECONNECTIONS_PARMNUM);
  SV_INITSESSTABLE_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_INITSESSTABLE_PARMNUM);
  SV_INITCONNTABLE_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_INITCONNTABLE_PARMNUM);
  SV_INITFILETABLE_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_INITFILETABLE_PARMNUM);
  SV_INITSEARCHTABLE_INFOLEVEL             = (PARMNUM_BASE_INFOLEVEL + SV_INITSEARCHTABLE_PARMNUM);
  SV_ALERTSCHEDULE_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_ALERTSCHEDULE_PARMNUM);
  SV_ERRORTHRESHOLD_INFOLEVEL              = (PARMNUM_BASE_INFOLEVEL + SV_ERRORTHRESHOLD_PARMNUM);
  SV_NETWORKERRORTHRESHOLD_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + SV_NETWORKERRORTHRESHOLD_PARMNUM);
  SV_DISKSPACETHRESHOLD_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_DISKSPACETHRESHOLD_PARMNUM);
  SV_MAXLINKDELAY_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_MAXLINKDELAY_PARMNUM);
  SV_MINLINKTHROUGHPUT_INFOLEVEL           = (PARMNUM_BASE_INFOLEVEL + SV_MINLINKTHROUGHPUT_PARMNUM);
  SV_LINKINFOVALIDTIME_INFOLEVEL           = (PARMNUM_BASE_INFOLEVEL + SV_LINKINFOVALIDTIME_PARMNUM);
  SV_SCAVQOSINFOUPDATETIME_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + SV_SCAVQOSINFOUPDATETIME_PARMNUM);
  SV_MAXWORKITEMIDLETIME_INFOLEVEL         = (PARMNUM_BASE_INFOLEVEL + SV_MAXWORKITEMIDLETIME_PARMNUM);
  SV_MAXRAWWORKITEMS_INFOLOEVEL            = (PARMNUM_BASE_INFOLEVEL + SV_MAXRAWWORKITEMS_PARMNUM);
  SV_PRODUCTTYPE_INFOLOEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_PRODUCTTYPE_PARMNUM);
  SV_SERVERSIZE_INFOLOEVEL                 = (PARMNUM_BASE_INFOLEVEL + SV_SERVERSIZE_PARMNUM);
  SV_CONNECTIONLESSAUTODISC_INFOLOEVEL     = (PARMNUM_BASE_INFOLEVEL + SV_CONNECTIONLESSAUTODISC_PARMNUM);
  SV_SHARINGVIOLATIONRETRIES_INFOLOEVEL    = (PARMNUM_BASE_INFOLEVEL + SV_SHARINGVIOLATIONRETRIES_PARMNUM);
  SV_SHARINGVIOLATIONDELAY_INFOLOEVEL      = (PARMNUM_BASE_INFOLEVEL + SV_SHARINGVIOLATIONDELAY_PARMNUM);
  SV_MAXGLOBALOPENSEARCH_INFOLOEVEL        = (PARMNUM_BASE_INFOLEVEL + SV_MAXGLOBALOPENSEARCH_PARMNUM);
  SV_REMOVEDUPLICATESEARCHES_INFOLOEVEL    = (PARMNUM_BASE_INFOLEVEL + SV_REMOVEDUPLICATESEARCHES_PARMNUM);
  SV_LOCKVIOLATIONRETRIES_INFOLOEVEL       = (PARMNUM_BASE_INFOLEVEL + SV_LOCKVIOLATIONRETRIES_PARMNUM);
  SV_LOCKVIOLATIONOFFSET_INFOLOEVEL        = (PARMNUM_BASE_INFOLEVEL + SV_LOCKVIOLATIONOFFSET_PARMNUM);
  SV_LOCKVIOLATIONDELAY_INFOLOEVEL         = (PARMNUM_BASE_INFOLEVEL + SV_LOCKVIOLATIONDELAY_PARMNUM);
  SV_MDLREADSWITCHOVER_INFOLOEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_MDLREADSWITCHOVER_PARMNUM);
  SV_CACHEDOPENLIMIT_INFOLOEVEL            = (PARMNUM_BASE_INFOLEVEL + SV_CACHEDOPENLIMIT_PARMNUM);
  SV_CRITICALTHREADS_INFOLOEVEL            = (PARMNUM_BASE_INFOLEVEL + SV_CRITICALTHREADS_PARMNUM);
  SV_RESTRICTNULLSESSACCESS_INFOLOEVEL     = (PARMNUM_BASE_INFOLEVEL + SV_RESTRICTNULLSESSACCESS_PARMNUM);
  SV_ENABLEWFW311DIRECTIPX_INFOLOEVEL      = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEWFW311DIRECTIPX_PARMNUM);
  SV_OTHERQUEUEAFFINITY_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_OTHERQUEUEAFFINITY_PARMNUM);
  SV_QUEUESAMPLESECS_INFOLEVEL             = (PARMNUM_BASE_INFOLEVEL + SV_QUEUESAMPLESECS_PARMNUM);
  SV_BALANCECOUNT_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_BALANCECOUNT_PARMNUM);
  SV_PREFERREDAFFINITY_INFOLEVEL           = (PARMNUM_BASE_INFOLEVEL + SV_PREFERREDAFFINITY_PARMNUM);
  SV_MAXFREERFCBS_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREERFCBS_PARMNUM);
  SV_MAXFREEMFCBS_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREEMFCBS_PARMNUM);
  SV_MAXFREELFCBS_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREELFCBS_PARMNUM);
  SV_MAXFREEPAGEDPOOLCHUNKS_INFOLEVEL      = (PARMNUM_BASE_INFOLEVEL + SV_MAXFREEPAGEDPOOLCHUNKS_PARMNUM);
  SV_MINPAGEDPOOLCHUNKSIZE_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + SV_MINPAGEDPOOLCHUNKSIZE_PARMNUM);
  SV_MAXPAGEDPOOLCHUNKSIZE_INFOLEVEL       = (PARMNUM_BASE_INFOLEVEL + SV_MAXPAGEDPOOLCHUNKSIZE_PARMNUM);
  SV_SENDSFROMPREFERREDPROCESSOR_INFOLEVEL = (PARMNUM_BASE_INFOLEVEL + SV_SENDSFROMPREFERREDPROCESSOR_PARMNUM);
  SV_MAXTHREADSPERQUEUE_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_MAXTHREADSPERQUEUE_PARMNUM);
  SV_CACHEDDIRECTORYLIMIT_INFOLEVEL        = (PARMNUM_BASE_INFOLEVEL + SV_CACHEDDIRECTORYLIMIT_PARMNUM);
  SV_MAXCOPYLENGTH_INFOLEVEL               = (PARMNUM_BASE_INFOLEVEL + SV_MAXCOPYLENGTH_PARMNUM);
  SV_ENABLEBULKTRANSFER_INFOLEVEL          = (PARMNUM_BASE_INFOLEVEL + SV_ENABLEBULKTRANSFER_PARMNUM);
  SV_ENABLECOMPRESSION_INFOLEVEL           = (PARMNUM_BASE_INFOLEVEL + SV_ENABLECOMPRESSION_PARMNUM);
  SV_AUTOSHAREWKS_INFOLEVEL                = (PARMNUM_BASE_INFOLEVEL + SV_AUTOSHAREWKS_PARMNUM);
  SV_AUTOSHARESERVER_INFOLEVEL             = (PARMNUM_BASE_INFOLEVEL + SV_AUTOSHARESERVER_PARMNUM);

  SVI1_NUM_ELEMENTS                        = 5;
  SVI2_NUM_ELEMENTS                        = 40;
  SVI3_NUM_ELEMENTS                        = 44;

{ Maxmimum length for command string to NetServerAdminCommand. }
  SV_MAX_CMD_LEN                           = PATHLEN;

{ Masks describing AUTOPROFILE parameters }
  SW_AUTOPROF_LOAD_MASK                    = $1;
  SW_AUTOPROF_SAVE_MASK                    = $2;

{ Max size of svX_srvheuristics. }
  SV_MAX_SRV_HEUR_LEN                      = 32; { Max heuristics info string length. }

{ Equate for use with sv102_licenses. }
  SV_USERS_PER_LICENSE                     = 5;

{-------------------- LMUSE.H -----------------------------------------------}
type
  TNetUseAdd                     = function(UncServerName: LPWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetUseDel                     = function(UncServerName, UseName: LPWSTR; ForceCond: DWord): NET_API_STATUS; stdcall;
  TNetUseEnum                    = function(UncServerName: LPWSTR; Level : DWord; var Buffer: Pointer; PrefMaxSize : DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetUseGetInfo                 = function(UncServerName, UseName: LPWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;

{-------------------- LMWKSTA.H ---------------------------------------------}
type
  TNetWkstaGetInfo               = function(ServerName: LPWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetWkstaSetInfo               = function(ServerName: LPWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetWkstaUserGetInfo           = function(Reserved: LPWSTR; Level: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetWkstaUserSetInfo           = function(ServerName: LPWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetWkstaUserEnum              = function(ServerName: LPWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;
  TNetWkstaTransportAdd          = function(ServerName: LPWSTR; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS; stdcall;
  TNetWkstaTransportDel          = function(ServerName, TransportName: LPWSTR; Ucond: DWord): NET_API_STATUS; stdcall;
  TNetWkstaTransportEnum         = function(ServerName: LPWSTR; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS; stdcall;

{-------------------- LMAPIBUF.H --------------------------------------------}
type
  TNetApiBufferAllocate          = function(ByteCount: Cardinal; var Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetApiBufferFree              = function(Buffer: Pointer): NET_API_STATUS; stdcall;
  TNetApiBufferReallocate        = function(OldBuffer: Pointer; NewByteCount : Cardinal; var NewBuffer: Pointer): NET_API_STATUS; stdcall;
  TNetApiBufferSize              = function(Buffer: Pointer; var ByteCount: DWord): NET_API_STATUS; stdcall;

{-------------------- LMSTATS.H ---------------------------------------------}
type
  TNetStatisticsGet              = function(Server, Service: LPWSTR; Level, Options: DWord; var Buffer: Pointer): NET_API_STATUS; stdcall;

{-------------------- WINBASE.H ---------------------------------------------}
type
  TGetSidIdentifierAuthority     = function(SID: PSID): PSIDIdentifierAuthority; stdcall;
  TGetSidSubAuthority            = function(SID: PSID; nSubAuthority: DWord): PDWord; stdcall;
  TGetSidSubAuthorityCount       = function(SID: PSID): PByte; stdcall;
  TLookupAccountName             = function(SystemName, AccountName: PChar; Sid: Pointer; var cbSid: DWord; DomainName: PChar; var cbDomainName: DWord; var peUse: DWord): Boolean; stdcall;
  TLookupAccountSid              = function(SystemName: PChar; Sid: PSID; AccountName: PChar; var AccountLen: DWord; DomainName: PChar; var DomainLen; var peUse: DWord): Boolean; stdcall;

{ ============================================================================}
  {LMACCESS}
  function StNetUserAdd(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetUserEnum(ServerName: string; Level, Filter: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetUserGetInfo(ServerName, UserName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetUserSetInfo(ServerName, UserName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetUserDel(ServerName, UserName: string): NET_API_STATUS;
  function StNetUserGetGroups(ServerName, UserName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord): NET_API_STATUS;
  function StNetUserSetGroups(ServerName, UserName: string; Level: DWord; Buffer: Pointer; NumEntries: DWord): NET_API_STATUS;
  function StNetUserGetLocalGroups(ServerName, UserName: string; Level: DWord; Flags: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries : DWord): NET_API_STATUS;
  function StNetUserModalsGet(ServerName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetUserModalsSet(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetUserChangePassword(DomainName, UserName, OldPassword, NewPassword: string): NET_API_STATUS;

  function StNetGroupAdd(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetGroupAddUser(ServerName, GroupName, UserName: string): NET_API_STATUS;
  function StNetGroupEnum(ServerName: string; Level: DWord; Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetGroupGetInfo(ServerName, GroupName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetGroupSetInfo(ServerName, GroupName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetGroupDel(ServerName, GroupName: string): NET_API_STATUS;
  function StNetGroupDelUser(ServerName, GroupName, UserName: string): NET_API_STATUS;
  function StNetGroupGetUsers(ServerName, GroupName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ResumeHandle: Pointer): NET_API_STATUS;
  function StNetGroupSetUsers(ServerName, GroupName: string; Level: DWord; Buffer: Pointer; var TotalEntries : DWord): NET_API_STATUS;

  function StNetLocalGroupAdd(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetLocalGroupAddMember(ServerName, GroupName: string; MembersID: PSID): NET_API_STATUS;
  function StNetLocalGroupEnum(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; var ResumeHandle: DWord): NET_API_STATUS;
  function StNetLocalGroupGetInfo(ServerName, GroupName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetLocalGroupSetInfo(ServerName, GroupName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetLocalGroupDel(ServerName, GroupName: string): NET_API_STATUS;
  function StNetLocalGroupDelMember(ServerName, GroupName: string; MembersID: PSID): NET_API_STATUS;
  function StNetLocalGroupGetMembers(ServerName, GroupName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ResumeHandle: Pointer): NET_API_STATUS;
  function StNetLocalGroupSetMembers(ServerName, GroupName: string; Level: DWord; Buffer: Pointer; var TotalEntries: DWord): NET_API_STATUS;
  function StNetLocalGroupAddMembers(ServerName, GroupName: string; Level: DWord; Buffer: Pointer; TotalEntries: DWord): NET_API_STATUS;
  function StNetLocalGroupDelMembers(ServerName, GroupName: string; Level: DWord; Buffer: Pointer; TotalEntries: DWord): NET_API_STATUS;

  function StNetQueryDisplayInformation(ServerName: string; Level, Index, EntriesRequested, PrefMaxLen: DWord; var ReturnedCount: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetGetDisplayInformationIndex(ServerName: string; Level: DWord; Prefix: string; var Index: DWord): NET_API_STATUS;

  function StNetAccessAdd(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetAccessEnum(ServerName, BasePath: string; Recursive, Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetAccessGetInfo(ServerName, Resource: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetAccessSetInfo(ServerName, Resource: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetAccessDel(ServerName, Resource: string): NET_API_STATUS;
  function StNetAccessGetUserPerms(ServerName, UGname, Resource: string; var Perms: DWord): NET_API_STATUS;

  function StNetGetDCName(ServerName, DomainName: string; var Buffer: string): NET_API_STATUS;
  function StNetGetAnyDCName(ServerName, DomainName: string; var Buffer: string): NET_API_STATUS;
  function StI_NetLogonControl(ServerName: string; FunctionCode, QueryLevel: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StI_NetLogonControl2(ServerName: string; FunctionCode, QueryLevel: DWord; Data: Pointer; var Buffer: Pointer): NET_API_STATUS;
  function StNetEnumerateTrustedDomains(ServerName: string; var Buffer: Pointer): NET_API_STATUS;

  {LMALERT}
  function StNetAlertRaise(AlertEventName: string; Buffer: Pointer; BufferSize: DWord): NET_API_STATUS;
  function StNetAlertRaiseEx(AlertEventName: string; VariableInfo: Pointer; VariableInfoSize: DWord; ServiceName: string): NET_API_STATUS;

  function StNetShareAdd(ServerName: string; Level: DWORD; Buffer: Pointer; var ParmErr: DWORD): NET_API_STATUS;
  function StNetShareEnum(ServerName: string; Level: DWORD; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; var ResumeHandle: DWord): NET_API_STATUS;
  function StNetShareEnumSticky(ServerName: string; Level: DWORD; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetShareGetInfo(ServerName, NetName: string; Level: DWORD; var Buffer: Pointer): NET_API_STATUS;
  function StNetShareSetInfo(ServerName, NetName: string; Level: DWORD; var Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetShareDel(ServerName, NetName: string; Reserved: DWord): NET_API_STATUS;
  function StNetShareDelSticky(ServerName, NetName: string; Reserved: DWord): NET_API_STATUS;
  function StNetShareCheck(ServerName, Device: string; var ShareType: DWord): NET_API_STATUS;

  function StNetSessionEnum(ServerName, UncClientName, UserName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetSessionDel(ServerName, UncClientName, UserName: string): NET_API_STATUS;
  function StNetSessionGetInfo(ServerName, UncClientName, UserName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;

  function StNetConnectionEnum(ServerName, Qualifier: string; Level: DWord; var Buffer : Pointer; PrefMaxLen : DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;

  function StNetFileClose(ServerName: string; FileID: DWord): NET_API_STATUS;
  function StNetFileEnum(ServerName, BasePath, UserName: string; Level: DWord; var Buffer: Pointer; PrefMexLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetFileGetInfo(ServerName: string; FileID, Level: DWord; var Buffer: Pointer): NET_API_STATUS;

  {LMMSG}
  function StNetMessageNameAdd(ServerName, MsgName: string): NET_API_STATUS;
  function StNetMessageNameEnum(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ResumeHandle: PDWord): NET_API_STATUS;
  function StNetMessageNameGetInfo(ServerName, MsgName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetMessageNameDel(ServerName, MsgName: string): NET_API_STATUS;
  function StNetMessageBufferSend(ServerName, MsgName, FromName: string; Buffer: Pointer; BufferLen: DWord): NET_API_STATUS;

  {LMREMUTL}
  function StNetRemoteTOD(UncServerName: string; var Buffer: Pointer): NET_API_STATUS;
  function StNetRemoteComputerSupports(UncServerName: string; OptionsWanted: DWord; var OptionsSupported: DWord): NET_API_STATUS;

  {LMSERVER}
  function StNetServerEnum(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ServerType: DWord; Domain: string; var ResumeHandle: DWord): NET_API_STATUS;
  function StNetServerEnumEx(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord; ServerType: DWord; Domain, FirstNameToReturn : string): NET_API_STATUS;
  function StNetServerGetInfo(ServerName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetServerSetInfo(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetServerDiskEnum(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetServerComputerNameAdd(ServerName, EmulatedDomainName, EmulatedServerName: string): NET_API_STATUS;
  function StNetServerComputerNameDel(ServerName, EmulatedServerName: string): NET_API_STATUS;
  function StNetServerTransportAdd(ServerName: string; Level: DWord; Buffer: Pointer): NET_API_STATUS;
  function StNetServerTransportAddEx(ServerName: WideString; Level: DWord; Buffer: Pointer): NET_API_STATUS;
  function StNetServerTransportDel(ServerName: string; Level: DWord; Buffer: Pointer): NET_API_STATUS;
  function StNetServerTransportEnum(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;

  {LMUSE}
  function StNetUseAdd(UncServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetUseDel(UncServerName, UseName: string; ForceCond: DWord): NET_API_STATUS;
  function StNetUseEnum(UncServerName: string; Level : DWord; var Buffer: Pointer; PrefMaxSize : DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetUseGetInfo(UncServerName, UseName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;

  {LMWKSTA}
  function StNetWkstaGetInfo(ServerName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetWkstaSetInfo(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetWkstaUserGetInfo(Reserved: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
  function StNetWkstaUserSetInfo(Reserved: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetWkstaUserEnum(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
  function StNetWkstaTransportAdd(ServerName: string; Level: DWord; Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
  function StNetWkstaTransportDel(ServerName, TransportName: string; Ucond: DWord): NET_API_STATUS;
  function StNetWkstaTransportEnum(ServerName: string; Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead, TotalEntries, ResumeHandle: DWord): NET_API_STATUS;

  {LMAPIBUF}
  function StNetApiBufferAllocate(ByteCount: Cardinal; var Buffer: Pointer): NET_API_STATUS;
  function StNetApiBufferFree(Buffer: Pointer): NET_API_STATUS;
  function StNetApiBufferReallocate(OldBuffer: Pointer; NewByteCount : Cardinal; var NewBuffer: Pointer): NET_API_STATUS;
  function StNetApiBufferSize(Buffer: Pointer; var ByteCount: DWord): NET_API_STATUS;

  {LMSTATS}
  function StNetStatisticsGet(ServerName, ServiceName: string; Level, Options: DWord; var Buffer: Pointer): NET_API_STATUS;

  {WINBASE}
  function StGetSidIdentifierAuthority(Sid: PSID): PSIDIdentifierAuthority;
  function StGetSidSubAuthority(Sid: PSID; SubAuthority: DWord): DWord;
  function StGetSidSubAuthorityCount(SID: PSID): Byte;
  function StLookupAccountName(SystemName, AccountName: string; var Sid: Pointer; var SidLength: DWord; var DomainName: string; var peUse: DWord): Boolean;
  function StLookupAccountSid(SystemName: string; Sid: PSid; var AccountName, DomainName: string; var peUse: DWord): Boolean;


type
  TLMWideChar = record
    Value : PWideChar;
    Length: DWord;
  end;

  TNetDisplayUserArray         = array[0..(MaxInt div SizeOf(TNET_DISPLAY_USER))-1]          of TNET_DISPLAY_USER;
  TNetDisplayGroupArray        = array[0..(MaxInt div SizeOf(TNET_DISPLAY_GROUP))-1]         of TNET_DISPLAY_GROUP;

  TUserInfo3Array              = array[0..(MaxInt div SizeOf(TUSER_INFO_3))-1]               of TUSER_INFO_3;

  TLocalGroupInfo1Array        = array[0..(MaxInt div SizeOf(TLOCALGROUP_INFO_1))-1]         of TLOCALGROUP_INFO_1;
  TLocalGroupUsersInfo0Array   = array[0..(MaxInt div SizeOf(TLOCALGROUP_USERS_INFO_0))-1]   of TLOCALGROUP_USERS_INFO_0;
  TLocalGroupMembersInfo1Array = array[0..(MaxInt div SizeOf(TLOCALGROUP_MEMBERS_INFO_1))-1] of TLOCALGROUP_MEMBERS_INFO_1;
  TLocalGroupMembersInfo2Array = array[0..(MaxInt div SizeOf(TLOCALGROUP_MEMBERS_INFO_2))-1] of TLOCALGROUP_MEMBERS_INFO_2;

  TGroupInfo1Array             = array[0..(MaxInt div SizeOf(TGROUP_INFO_1))-1]              of TGROUP_INFO_1;
  TGroupUsersInfo0Array        = array[0..(MaxInt div SizeOf(TGROUP_USERS_INFO_0))-1]        of TGROUP_USERS_INFO_0;

  TShareInfo1Array             = array[0..(MaxInt div SizeOf(TSHARE_INFO_1))-1]              of TSHARE_INFO_1;

  TMsgInfo0Array               = array[0..(MaxInt div SizeOf(TMSG_INFO_0))-1]                of TMSG_INFO_0;

  procedure CvtToWideChar(const S: string; var WS: TLMWideChar);
  procedure CvtToWideCharLM(const S: string; var WS: TLMWideChar);

implementation

uses SysUtils, StUtils, AnsiStrings;

var
  {LMACCESS}
  _NetUserAdd                    : TNetUserAdd                    = nil;
  _NetUserEnum                   : TNetUserEnum                   = nil;
  _NetUserGetInfo                : TNetUserGetInfo                = nil;
  _NetUserSetInfo                : TNetUserSetInfo                = nil;
  _NetUserDel                    : TNetUserDel                    = nil;
  _NetUserGetGroups              : TNetUserGetGroups              = nil;
  _NetUserSetGroups              : TNetUserSetGroups              = nil;
  _NetUserGetLocalGroups         : TNetUserGetLocalGroups         = nil;
  _NetUserModalsGet              : TNetUserModalsGet              = nil;
  _NetUserModalsSet              : TNetUserModalsSet              = nil;
  _NetUserChangePassword         : TNetUserChangePassword         = nil;

  _NetGroupAdd                   : TNetGroupAdd                   = nil;
  _NetGroupAddUser               : TNetGroupAddUser               = nil;
  _NetGroupEnum                  : TNetGroupEnum                  = nil;
  _NetGroupGetInfo               : TNetGroupGetInfo               = nil;
  _NetGroupSetInfo               : TNetGroupSetInfo               = nil;
  _NetGroupDel                   : TNetGroupDel                   = nil;
  _NetGroupDelUser               : TNetGroupDelUser               = nil;
  _NetGroupGetUsers              : TNetGroupGetUsers              = nil;
  _NetGroupSetUsers              : TNetGroupSetUsers              = nil;

  _NetLocalGroupAdd              : TNetLocalGroupAdd              = nil;
  _NetLocalGroupAddMember        : TNetLocalGroupAddMember        = nil;
  _NetLocalGroupEnum             : TNetLocalGroupEnum             = nil;
  _NetLocalGroupGetInfo          : TNetLocalGroupGetInfo          = nil;
  _NetLocalGroupSetInfo          : TNetLocalGroupSetInfo          = nil;
  _NetLocalGroupDel              : TNetLocalGroupDel              = nil;
  _NetLocalGroupDelMember        : TNetLocalGroupDelMember        = nil;
  _NetLocalGroupGetMembers       : TNetLocalGroupGetMembers       = nil;
  _NetLocalGroupSetMembers       : TNetLocalGroupSetMembers       = nil;
  _NetLocalGroupAddMembers       : TNetLocalGroupAddMembers       = nil;
  _NetLocalGroupDelMembers       : TNetLocalGroupDelMembers       = nil;

  _NetQueryDisplayInformation    : TNetQueryDisplayInformation    = nil;
  _NetGetDisplayInformationIndex : TNetGetDisplayInformationIndex = nil;

  _NetAccessAdd                  : TNetAccessAdd                  = nil;
  _NetAccessEnum                 : TNetAccessEnum                 = nil;
  _NetAccessGetInfo              : TNetAccessGetInfo              = nil;
  _NetAccessSetInfo              : TNetAccessSetInfo              = nil;
  _NetAccessDel                  : TNetAccessDel                  = nil;
  _NetAccessGetUserPerms         : TNetAccessGetUserPerms         = nil;

  _NetGetDCName                  : TNetGetDCName                  = nil;
  _NetGetAnyDCName               : TNetGetAnyDCName               = nil;
  _I_NetLogonControl             : TI_NetLogonControl             = nil;
  _I_NetLogonControl2            : TI_NetLogonControl2            = nil;
  _NetEnumerateTrustedDomains    : TNetEnumerateTrustedDomains    = nil;

  {LMALERT}
  _NetAlertRaise                 : TNetAlertRaise                 = nil;
  _NetAlertRaiseEx               : TNetAlertRaiseEx               = nil;

  {LMSHARE}
  _NetShareAdd                   : TNetShareAdd                   = nil;
  _NetShareEnum                  : TNetShareEnum                  = nil;
  _NetShareEnumSticky            : TNetShareEnumSticky            = nil;
  _NetShareGetInfo               : TNetShareGetInfo               = nil;
  _NetShareSetInfo               : TNetShareSetInfo               = nil;
  _NetShareDel                   : TNetShareDel                   = nil;
  _NetShareDelSticky             : TNetShareDelSticky             = nil;
  _NetShareCheck                 : TNetShareCheck                 = nil;

  _NetSessionEnum                : TNetSessionEnum                = nil;
  _NetSessionDel                 : TNetSessionDel                 = nil;
  _NetSessionGetInfo             : TNetSessionGetInfo             = nil;

  _NetConnectionEnum             : TNetConnectionEnum             = nil;

  _NetFileClose                  : TNetFileClose                  = nil;
  _NetFileEnum                   : TNetFileEnum                   = nil;
  _NetFileGetInfo                : TNetFileGetInfo                = nil;

  {LMMSG}
  _NetMessageNameAdd             : TNetMessageNameAdd             = nil;
  _NetMessageNameEnum            : TNetMessageNameEnum            = nil;
  _NetMessageNameGetInfo         : TNetMessageNameGetInfo         = nil;
  _NetMessageNameDel             : TNetMessageNameDel             = nil;
  _NetMessageBufferSend          : TNetMessageBufferSend          = nil;

  {LMREMUTL}
  _NetRemoteTOD                  : TNetRemoteTOD                  = nil;
  _NetRemoteComputerSupports     : TNetRemoteComputerSupports     = nil;

  {LMSERVER}
  _NetServerEnum                 : TNetServerEnum                 = nil;
  _NetServerEnumEx               : TNetServerEnumEx               = nil;
  _NetServerGetInfo              : TNetServerGetInfo              = nil;
  _NetServerSetInfo              : TNetServerSetInfo              = nil;
  _NetServerDiskEnum             : TNetServerDiskEnum             = nil;
  _NetServerComputerNameAdd      : TNetServerComputerNameAdd      = nil;
  _NetServerComputerNameDel      : TNetServerComputerNameDel      = nil;
  _NetServerTransportAdd         : TNetServerTransportAdd         = nil;
  _NetServerTransportAddEx       : TNetServerTransportAddEx       = nil;
  _NetServerTransportDel         : TNetServerTransportDel         = nil;
  _NetServerTransportEnum        : TNetServerTransportEnum        = nil;

  {LMUSE}
  _NetUseAdd                     : TNetUseAdd                     = nil;
  _NetUseDel                     : TNetUseDel                     = nil;
  _NetUseEnum                    : TNetUseEnum                    = nil;
  _NetUseGetInfo                 : TNetUseGetInfo                 = nil;

  {LMWKSTA}
  _NetWkstaGetInfo               : TNetWkstaGetInfo               = nil;
  _NetWkstaSetInfo               : TNetWkstaSetInfo               = nil;
  _NetWkstaUserGetInfo           : TNetWkstaUserGetInfo           = nil;
  _NetWkstaUserSetInfo           : TNetWkstaUserSetInfo           = nil;
  _NetWkstaUserEnum              : TNetWkstaUserEnum              = nil;
  _NetWkstaTransportAdd          : TNetWkstaTransportAdd          = nil;
  _NetWkstaTransportDel          : TNetWkstaTransportDel          = nil;
  _NetWkstaTransportEnum         : TNetWkstaTransportEnum         = nil;

  {LMAPIBUF}
  _NetApiBufferAllocate          : TNetApiBufferAllocate          = nil;
  _NetApiBufferFree              : TNetApiBufferFree              = nil;
  _NetApiBufferReallocate        : TNetApiBufferReallocate        = nil;
  _NetApiBufferSize              : TNetApiBufferSize              = nil;

  {LMSTATS}
  _NetStatisticsGet              : TNetStatisticsGet              = nil;

  {WINBASE}
  _GetSidIdentifierAuthority     : TGetSidIdentifierAuthority     = nil;
  _GetSidSubAuthority            : TGetSidSubAuthority            = nil;
  _GetSidSubAuthorityCount       : TGetSidSubAuthorityCount       = nil;
  _LookupAccountName             : TLookupAccountName             = nil;
  _LookupAccountSid              : TLookupAccountSid              = nil;

var
  NETAPI32    : THandle = 0;
  ADVAPI32    : THandle = 0;

  NETAPI32DLL : PChar   = 'netapi32.dll';
  ADVAPI32DLL : PChar   = 'advapi32.dll';

function IsNetApi32: Boolean;
begin
  if NETAPI32 = 0 then
    NETAPI32 := LoadLibrary(NETAPI32DLL);

  Result := (NETAPI32 <> 0);
end;

function IsAdvApi32: Boolean;
begin
  if ADVAPI32 = 0 then
    ADVAPI32 := LoadLibrary(ADVAPI32DLL);

  Result := (ADVAPI32 <> 0);
end;

function ServerUncName(Name: string): string;
begin
  { NT 4 and 9x need it in a UNC form, but Windows 2000 does not (it can have dns names) }
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion < 5) then
    Result := '\\' + Name
  else
    Result := Name;
end;

procedure CvtToWideChar(const S: string; var WS: TLMWideChar);
var
  S1 : string;
begin
  { Quick check to make sure that WS is not alreay allocated }
  if WS.Value <> nil then
    FreeMem(WS.Value, WS.Length);

  S1 := Trim(S);

  { Now get the new pointer and size }
  if Length(S1) > 0 then begin
    WS.Length := (Length(S1) + 1) * 2;
    GetMem(WS.Value, WS.Length);
    StringToWideChar(S1, WS.Value, WS.Length);
  end else begin
    WS.Length := 0;
    WS.Value := nil;
  end;
end;

procedure CvtToWideCharLM(const S: string; var WS: TLMWideChar);
var
  S1 : string;
begin
  { Quick check to make sure that WS is not alreay allocated }
  if WS.Value <> nil then
    StNetApiBufferFree(WS.Value);

  S1 := Trim(S);

  { Now get the new pointer and size }
  WS.Length := (Length(S1) + 1) * 2;
  StNetApiBufferAllocate(WS.Length, Pointer(WS.Value));
  StringToWideChar(S1, WS.Value, WS.Length);
end;


{LMACCESS}
function StNetUserAdd(ServerName: string; Level: DWord; Buffer: Pointer;
                      var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserAdd = nil) then
      @_NetUserAdd := GetProcAddress(NETAPI32, 'NetUserAdd');

    if (@_NetUserAdd <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetUserAdd(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserEnum(ServerName: string; Level, Filter: DWord;
                       var Buffer: Pointer; PrefMaxLen: DWord; var EntriesRead,
                       TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserEnum = nil) then
      @_NetUserEnum := GetProcAddress(NETAPI32, 'NetUserEnum');

    if (@_NetUserEnum <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetUserEnum(S.Value, Level, Filter, Buffer, PrefMaxLen,
                               EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserGetInfo(ServerName, UserName: string; Level: DWord;
                          var Buffer: Pointer): NET_API_STATUS;
var
  S, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserGetInfo = nil) then
      @_NetUserGetInfo := GetProcAddress(NETAPI32, 'NetUserGetInfo');

    if (@_NetUserGetInfo <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetUserGetInfo(S.Value, U.Value, Level, Buffer);
      finally
        FreeMem(U.Value, U.Length);
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserSetInfo(ServerName, UserName: string; Level: DWord;
                          Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
var
  S, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserSetInfo = nil) then
      @_NetUserSetInfo := GetProcAddress(NETAPI32, 'NetUserSetInfo');

    if (@_NetUserSetInfo <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetUserSetInfo(S.Value, U.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserDel(ServerName, UserName: string): NET_API_STATUS;
var
  S, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserDel = nil) then
      @_NetUserDel := GetProcAddress(NETAPI32, 'NetUserDel');

    if (@_NetUserDel <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetUserDel(S.Value, U.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserGetGroups(ServerName, UserName: string; Level: DWord;
                            var Buffer: Pointer; PrefMaxLen: DWord;
                            var EntriesRead, TotalEntries: DWord): NET_API_STATUS;
var
  S, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserGetGroups = nil) then
      @_NetUserGetGroups := GetProcAddress(NETAPI32, 'NetUserGetGroups');

    if (@_NetUserGetGroups <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetUserGetGroups(S.Value, U.Value, Level, Buffer,
                                    PrefMaxLen, EntriesRead, TotalEntries);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(U.value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserSetGroups(ServerName, UserName: string; Level: DWord;
                            Buffer: Pointer; NumEntries: DWord): NET_API_STATUS;
var
  S, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserSetGroups = nil) then
      @_NetUserSetGroups := GetProcAddress(NETAPI32, 'NetUserSetGroups');

    if (@_NetUserSetGroups <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetUserSetGroups(S.Value, U.Value, Level, Buffer, NumEntries);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(U.value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserGetLocalGroups(ServerName, UserName: string; Level: DWord;
                                 Flags: DWord; var Buffer: Pointer;
                                 PrefMaxLen: DWord; var EntriesRead,
                                 TotalEntries : DWord): NET_API_STATUS;
var
  S, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserGetLocalGroups = nil) then
      @_NetUserGetLocalGroups := GetProcAddress(NETAPI32, 'NetUserGetLocalGroups');

    if (@_NetUserGetLocalGroups <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetUserGetLocalGroups(S.Value, U.Value, Level, Flags,
                                         Buffer, PrefMaxLen, EntriesRead,
                                         TotalEntries);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(U.value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserModalsGet(ServerName: string; Level: DWord;
                            var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserModalsGet = nil) then
      @_NetUserModalsGet := GetProcAddress(NETAPI32, 'NetUserModalsGet');

    if (@_NetUserModalsGet <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetUserModalsGet(S.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserModalsSet(ServerName: string; Level: DWord;
                            Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserModalsSet = nil) then
      @_NetUserModalsSet := GetProcAddress(NETAPI32, 'NetUserModalsSet');

    if (@_NetUserModalsSet <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetUserModalsSet(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUserChangePassword(DomainName, UserName, OldPassword,
                                 NewPassword: string): NET_API_STATUS;
var
  D, U, O, N : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUserChangePassword = nil) then
      @_NetUserChangePassword := GetProcAddress(NETAPI32, 'NetUserChangePassword');

    if (@_NetUserChangePassword <> nil) then begin
      { determine if UNC identifiers needed }
      DomainName := ServerUncName(DomainName);

      D.Value := nil;
      U.Value := nil;
      O.Value := nil;
      N.Value := nil;
      try
        { allocate the unicode domain name }
        CvtToWideChar(DomainName, D);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { allocate the unicode old password }
        CvtToWideChar(OldPassword, O);

        { allocate the unicode new password }
        CvtToWideChar(NewPassword, N);

        { call the API }
        Result := _NetUserChangePassword(D.Value, U.Value, O.Value, N.Value);
      finally
        FreeMem(D.Value, D.Length);
        FreeMem(U.Value, U.Length);
        FreeMem(O.Value, O.Length);
        FreeMem(N.Value, N.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetGroupAdd(ServerName: string; Level: DWord; Buffer: Pointer;
                       var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupAdd = nil) then
      @_NetGroupAdd := GetProcAddress(NETAPI32, 'NetGroupAdd');

    if (@_NetGroupAdd <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetGroupAdd(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGroupAddUser(ServerName, GroupName, UserName: string): NET_API_STATUS;
var
  S, G, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupAddUser = nil) then
      @_NetGroupAddUser := GetProcAddress(NETAPI32, 'NetGroupAddUser');

    if (@_NetGroupAddUser <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetGroupAddUser(S.Value, G.Value, U.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGroupEnum(ServerName: string; Level: DWord; Buffer: Pointer;
                        PrefMaxLen: DWord; var EntriesRead, TotalEntries,
                        ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupEnum = nil) then
      @_NetGroupEnum := GetProcAddress(NETAPI32, 'NetGroupEnum');

    if (@_NetGroupEnum <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetGroupEnum(S.Value, Level, Buffer, PrefMaxLen,
                                EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGroupGetInfo(ServerName, GroupName: string; Level: DWord;
                           var Buffer: Pointer): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupGetInfo = nil) then
      @_NetGroupGetInfo := GetProcAddress(NETAPI32, 'NetGroupGetInfo');

    if (@_NetGroupGetInfo <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetGroupGetInfo(S.Value, G.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGroupSetInfo(ServerName, GroupName: string; Level: DWord;
                           Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupSetInfo = nil) then
      @_NetGroupSetInfo := GetProcAddress(NETAPI32, 'NetGroupSetInfo');

    if (@_NetGroupSetInfo <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetGroupSetInfo(S.Value, G.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32Error(EStNetException, GetLastError);
  end;
end;

function StNetGroupDel(ServerName, GroupName: string): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupDel = nil) then
      @_NetGroupDel := GetProcAddress(NETAPI32, 'NetGroupDel');

    if (@_NetGroupDel <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetGroupDel(S.Value, G.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGroupDelUser(ServerName, GroupName, UserName: string): NET_API_STATUS;
var
  S, G, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupDelUser = nil) then
      @_NetGroupDelUser := GetProcAddress(NETAPI32, 'NetGroupDelUser');

    if (@_NetGroupDelUser <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetGroupDelUser(S.Value, G.Value, U.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGroupGetUsers(ServerName, GroupName: string; Level: DWord;
                            var Buffer: Pointer; PrefMaxLen: DWord;
                            var EntriesRead, TotalEntries: DWord;
                            ResumeHandle: Pointer): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupGetUsers = nil) then
      @_NetGroupGetUsers := GetProcAddress(NETAPI32, 'NetGroupGetUsers');

    if (@_NetGroupGetUsers <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetGroupGetUsers(S.Value, G.Value, Level, Buffer,
                                    PrefMaxLen, EntriesRead, TotalEntries,
                                    ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGroupSetUsers(ServerName, GroupName: string; Level: DWord;
                            Buffer: Pointer; var TotalEntries : DWord): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGroupSetUsers = nil) then
      @_NetGroupSetUsers := GetProcAddress(NETAPI32, 'NetGroupSetUsers');

    if (@_NetGroupSetUsers <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetGroupSetUsers(S.Value, G.Value, Level, Buffer, TotalEntries);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetLocalGroupAdd(ServerName: string; Level: DWord; Buffer: Pointer;
                            var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupAdd = nil) then
      @_NetLocalGroupAdd := GetProcAddress(NETAPI32, 'NetLocalGroupAdd');

    if (@_NetLocalGroupAdd <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetLocalGroupAdd(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupAddMember(ServerName, GroupName: string;
                                  MemberSID: PSID): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupAddMember = nil) then
      @_NetLocalGroupAddMember := GetProcAddress(NETAPI32, 'NetLocalGroupAddMember');

    if (@_NetLocalGroupAddMember <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupAddMember(S.Value, G.Value, MemberSID)
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupEnum(ServerName: string; Level: DWord;
                             var Buffer: Pointer; PrefMaxLen: DWord;
                             var EntriesRead, TotalEntries: DWord;
                             var ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupEnum = nil) then
      @_NetLocalGroupEnum := GetProcAddress(NETAPI32, 'NetLocalGroupEnum');

    if (@_NetLocalGroupEnum <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetLocalGroupEnum(S.Value, Level, Buffer, PrefMaxLen,
                                     EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupGetInfo(ServerName, GroupName: string; Level: DWord;
                                var Buffer: Pointer): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupGetInfo = nil) then
      @_NetLocalGroupGetInfo := GetProcAddress(NETAPI32, 'NetLocalGroupGetInfo');

    if (@_NetLocalGroupGetInfo <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupGetInfo(S.Value, G.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupSetInfo(ServerName, GroupName: string; Level: DWord;
                                Buffer: Pointer;
                                var ParmErr: DWord): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupSetInfo = nil) then
      @_NetLocalGroupSetInfo := GetProcAddress(NETAPI32, 'NetLocalGroupSetInfo');

    if (@_NetLocalGroupSetInfo <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupSetInfo(S.Value, G.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupDel(ServerName, GroupName: string): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupDel = nil) then
      @_NetLocalGroupDel := GetProcAddress(NETAPI32, 'NetLocalGroupDel');

    if (@_NetLocalGroupDel <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupDel(S.Value, G.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupDelMember(ServerName, GroupName: string;
                                  MemberSID: PSID): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupDelMember = nil) then
      @_NetLocalGroupDelMember := GetProcAddress(NETAPI32, 'NetLocalGroupDelMember');

    if (@_NetLocalGroupDelMember <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupDelMember(S.Value, G.Value, MemberSID)
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupGetMembers(ServerName, GroupName: string; Level: DWord;
                                   var Buffer: Pointer; PrefMaxLen: DWord;
                                   var EntriesRead, TotalEntries: DWord;
                                   ResumeHandle: Pointer): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupGetMembers = nil) then
      @_NetLocalGroupGetMembers := GetProcAddress(NETAPI32, 'NetLocalGroupGetMembers');

    if (@_NetLocalGroupGetMembers <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupGetMembers(S.Value, G.Value, Level, Buffer,
                                           PrefMaxLen, EntriesRead,
                                           TotalEntries, ResumeHandle)
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupSetMembers(ServerName, GroupName: string; Level: DWord;
                                   Buffer: Pointer;
                                   var TotalEntries: DWord): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupSetMembers = nil) then
      @_NetLocalGroupSetMembers := GetProcAddress(NETAPI32, 'NetLocalGroupSetMembers');

    if (@_NetLocalGroupSetMembers <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupSetMembers(S.Value, G.Value, Level, Buffer,
                                           TotalEntries);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupAddMembers(ServerName, GroupName: string; Level: DWord;
                                   Buffer: Pointer;
                                   TotalEntries: DWord): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupAddMembers = nil) then
      @_NetLocalGroupAddMembers := GetProcAddress(NETAPI32, 'NetLocalGroupAddMembers');

    if (@_NetLocalGroupAddMembers <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupAddMembers(S.Value, G.Value, Level, Buffer,
                                           TotalEntries);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetLocalGroupDelMembers(ServerName, GroupName: string; Level: DWord;
                                   Buffer: Pointer; TotalEntries: DWord): NET_API_STATUS;
var
  S, G : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetLocalGroupDelMembers = nil) then
      @_NetLocalGroupDelMembers := GetProcAddress(NETAPI32, 'NetLocalGroupDelMembers');

    if (@_NetLocalGroupDelMembers <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      G.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode group name }
        CvtToWideChar(GroupName, G);

        { call the API }
        Result := _NetLocalGroupDelMembers(S.Value, G.Value, Level, Buffer,
                                           TotalEntries);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(G.Value, G.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetQueryDisplayInformation(ServerName: string; Level, Index,
                                      EntriesRequested, PrefMaxLen: DWord;
                                      var ReturnedCount: DWord;
                                      var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWidechar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetQueryDisplayInformation = nil) then
      @_NetQueryDisplayInformation := GetProcAddress(NETAPI32, 'NetQueryDisplayInformation');

    if (@_NetQueryDisplayInformation <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetQueryDisplayInformation(S.Value, Level, Index,
                                              EntriesRequested, PrefMaxLen,
                                              ReturnedCount, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGetDisplayInformationIndex(ServerName: string; Level: DWord;
                                         Prefix: string;
                                         var Index: DWord): NET_API_STATUS;
var
  S, P : TLMWidechar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGetDisplayInformationIndex = nil) then
      @_NetGetDisplayInformationIndex := GetProcAddress(NETAPI32, 'NetGetDisplayInformationIndex');

    if (@_NetGetDisplayInformationIndex <> nil) then begin
      { determine if UNC identifiers needed }
      ServerName := ServerUncName(ServerName);

      S.Value := nil;
      P.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode prefix }
        CvtToWideChar(Prefix, P);

        { call the API }
        Result := _NetGetDisplayInformationIndex(S.Value, Level, P.Value, Index);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(P.Value, P.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetAccessAdd(ServerName: string; Level: DWord; Buffer: Pointer;
                        var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWidechar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAccessAdd = nil) then
      @_NetAccessAdd := GetProcAddress(NETAPI32, 'NetAccessAdd');

    if (@_NetAccessAdd <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetAccessAdd(S.Value, Level, Buffer, ParmErr)
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetAccessEnum(ServerName, BasePath: string; Recursive, Level: DWord;
                         var Buffer: Pointer; PrefMaxLen: DWord;
                         var EntriesRead, TotalEntries,
                         ResumeHandle: DWord): NET_API_STATUS;
var
  S, B : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAccessEnum = nil) then
      @_NetAccessEnum := GetProcAddress(NETAPI32, 'NetAccessEnum');

    if (@_NetAccessEnum <> nil) then begin
      S.Value := nil;
      B.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode base path }
        CvtToWideChar(BasePath, B);

        { call the API }
        Result := _NetAccessEnum(S.Value, B.Value, Recursive, Level, Buffer,
                                 PrefMaxLen, EntriesRead, TotalEntries,
                                 ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(B.Value, B.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetAccessGetInfo(ServerName, Resource: string; Level: DWord;
                            var Buffer: Pointer): NET_API_STATUS;
var
  S, R : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAccessGetInfo = nil) then
      @_NetAccessGetInfo := GetProcAddress(NETAPI32, 'NetAccessGetInfo');

    if (@_NetAccessGetInfo <> nil) then begin
      S.Value := nil;
      R.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode resource }
        CvtToWideChar(Resource, R);

        { call the API }
        Result := _NetAccessGetInfo(S.Value, R.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(R.Value, R.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetAccessSetInfo(ServerName, Resource: string; Level: DWord;
                            Buffer: Pointer; var ParmErr: DWord): NET_API_STATUS;
var
  S, R : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAccessSetInfo = nil) then
      @_NetAccessSetInfo := GetProcAddress(NETAPI32, 'NetAccessSetInfo');

    if (@_NetAccessSetInfo <> nil) then begin
      S.Value := nil;
      R.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode resource }
        CvtToWideChar(Resource, R);

        { call the API }
        Result := _NetAccessSetInfo(S.Value, R.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(R.Value, R.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetAccessDel(ServerName, Resource: string): NET_API_STATUS;
var
  S, R : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAccessDel = nil) then
      @_NetAccessDel := GetProcAddress(NETAPI32, 'NetAccessDel');

    if (@_NetAccessDel <> nil) then begin
      S.Value := nil;
      R.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode resource }
        CvtToWideChar(Resource, R);

        { call the API }
        Result := _NetAccessDel(S.Value, R.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(R.Value, R.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetAccessGetUserPerms(ServerName, UGname, Resource: string;
                                 var Perms: DWord): NET_API_STATUS;
var
  S, U, R : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAccessGetUserPerms = nil) then
      @_NetAccessGetUserPerms := GetProcAddress(NETAPI32, 'NetAccessGetUserPerms');

    if (@_NetAccessGetUserPerms <> nil) then begin
      S.Value := nil;
      U.Value := nil;
      R.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode user/group name }
        CvtToWideChar(UGname, U);

        { allocate the unicode resource }
        CvtToWideChar(Resource, R);

        { call the API }
        Result := _NetAccessGetUserPerms(S.Value, U.Value, R.Value, Perms);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(U.Value, U.Length);
        FreeMem(R.Value, R.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetGetDCName(ServerName, DomainName: string;
                        var Buffer: string): NET_API_STATUS;
var
  S, D: TLMWideChar;
  DB : Pointer;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGetDCName = nil) then
      @_NetGetDCName := GetProcAddress(NETAPI32, 'NetGetDCName');

    if (@_NetGetDCName <> nil) then begin
      S.Value := nil;
      D.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode domain name }
        CvtToWideChar(DomainName, D);

        { call the API }
        Result := _NetGetDCName(S.Value, D.Value, DB);
        if Result = NERR_SUCCESS then begin
          Buffer := WideCharToString(DB);
          StNetApiBufferFree(DB);
        end;
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(D.Value, D.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetGetAnyDCName(ServerName, DomainName: string;
                           var Buffer: string): NET_API_STATUS;
var
  S, D : TLMWideChar;
  B : PByte;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetGetAnyDCName = nil) then
      @_NetGetAnyDCName := GetProcAddress(NETAPI32, 'NetGetAnyDCName');

    if (@_NetGetAnyDCName <> nil) then begin
      S.Value := nil;
      D.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode domain name }
        CvtToWideChar(DomainName, D);

        { call the API }
        Result := _NetGetAnyDCName(S.Value, D.Value, B);
        if Result = NERR_SUCCESS then begin
          Buffer := string(AnsiStrings.StrPas(PAnsiChar(B)));
          StNetApiBufferFree(B);
        end;
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(D.Value, D.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StI_NetLogonControl(ServerName: string; FunctionCode, QueryLevel: DWord;
                             var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_I_NetLogonControl = nil) then
      @_I_NetLogonControl := GetProcAddress(NETAPI32, 'I_NetLogonControl');

    if (@_I_NetLogonControl <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _I_NetLogonControl(S.Value, FunctionCode, QueryLevel, Buffer)
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StI_NetLogonControl2(ServerName: string; FunctionCode,
                              QueryLevel: DWord; Data: Pointer;
                              var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_I_NetLogonControl2 = nil) then
      @_I_NetLogonControl2 := GetProcAddress(NETAPI32, 'I_NetLogonControl2');

    if (@_I_NetLogonControl2 <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _I_NetLogonControl2(S.Value, FunctionCode, QueryLevel, Data, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetEnumerateTrustedDomains(ServerName: string;
                                      var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetEnumerateTrustedDomains = nil) then
      @_NetEnumerateTrustedDomains := GetProcAddress(NETAPI32, 'NetEnumerateTrustedDomains');

    if (@_NetEnumerateTrustedDomains <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetEnumerateTrustedDomains(S.Value, Buffer)
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


{LMALERT}
function StNetAlertRaise(AlertEventName: string; Buffer: Pointer;
                         BufferSize: DWord): NET_API_STATUS;
var
  A : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAlertRaise = nil) then
      @_NetAlertRaise := GetProcAddress(NETAPI32, 'NetAlertRaise');

    if (@_NetAlertRaise <> nil) then begin
      A.Value := nil;
      try
        { allocate the unicode alert event name }
        CvtToWideChar(AlertEventName, A);

        { call the API }
        Result := _NetAlertRaise(A.Value, Buffer, BufferSize);
      finally
        FreeMem(A.Value, A.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetAlertRaiseEx(AlertEventName: string; VariableInfo:
                           Pointer; VariableInfoSize: DWord;
                           ServiceName: string): NET_API_STATUS;
var
  A, S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetAlertRaiseEx = nil) then
      @_NetAlertRaiseEx := GetProcAddress(NETAPI32, 'NetAlertRaiseEx');

    if (@_NetAlertRaiseEx <> nil) then begin
      A.Value := nil;
      S.Value := nil;
      try
        { allocate the unicode alert event name }
        CvtToWideChar(AlertEventName, A);

        { allocate the unicode service name }
        CvtToWideChar(ServiceName, S);

        { call the API }
        Result := _NetAlertRaiseEx(A.Value, VariableInfo, VariableInfoSize, S.Value);
      finally
        FreeMem(A.Value, A.Length);
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetShareAdd(ServerName: string; Level: DWORD; Buffer: Pointer;
                       var ParmErr: DWORD): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareAdd = nil) then
      @_NetShareAdd := GetProcAddress(NETAPI32, 'NetShareAdd');

    if (@_NetShareAdd <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetShareAdd(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetShareEnum(ServerName: string; Level: DWORD; var Buffer: Pointer;
                        PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord;
                        var ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareEnum = nil) then
      @_NetShareEnum := GetProcAddress(NETAPI32, 'NetShareEnum');

    if (@_NetShareEnum <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetShareEnum(S.Value, Level, Buffer, PrefMaxLen, EntriesRead,
                                TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetShareEnumSticky(ServerName: string; Level: DWORD;
                              var Buffer: Pointer; PrefMaxLen: DWord;
                              var EntriesRead, TotalEntries,
                              ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareEnumSticky = nil) then
      @_NetShareEnumSticky := GetProcAddress(NETAPI32, 'NetShareEnumSticky');

    if (@_NetShareEnumSticky <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetShareEnumSticky(S.Value, Level, Buffer, PrefMaxLen,
                                      EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetShareGetInfo(ServerName, NetName: string; Level: DWORD;
                           var Buffer: Pointer): NET_API_STATUS;
var
  S, N : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareGetInfo = nil) then
      @_NetShareGetInfo := GetProcAddress(NETAPI32, 'NetShareGetInfo');

    if (@_NetShareGetInfo <> nil) then begin
      S.Value := nil;
      N.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode net name }
        CvtToWideChar(NetName, N);

        { call the API }
        Result := _NetShareGetInfo(S.Value, N.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(N.Value, N.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetShareSetInfo(ServerName, NetName: string; Level: DWORD;
                           var Buffer: Pointer;
                           var ParmErr: DWord): NET_API_STATUS;
var
  S, N : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareSetInfo = nil) then
      @_NetShareSetInfo := GetProcAddress(NETAPI32, 'NetShareSetInfo');

    if (@_NetShareSetInfo <> nil) then begin
      S.Value := nil;
      N.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode net name }
        CvtToWideChar(NetName, N);

        { call the API }
        Result := _NetShareSetInfo(S.Value, N.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(N.Value, N.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetShareDel(ServerName, NetName: string; Reserved: DWord): NET_API_STATUS;
var
  S, N : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareDel = nil) then
      @_NetShareDel := GetProcAddress(NETAPI32, 'NetShareDel');

    if (@_NetShareDel <> nil) then begin
      S.Value := nil;
      N.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode net name }
        CvtToWideChar(NetName, N);

        { call the API }
        Result := _NetShareDel(S.Value, N.Value, Reserved)
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(N.Value, N.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetShareDelSticky(ServerName, NetName: string; Reserved: DWord): NET_API_STATUS;
var
  S, N : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareDelSticky = nil) then
      @_NetShareDelSticky := GetProcAddress(NETAPI32, 'NetShareDelSticky');

    if (@_NetShareDelSticky <> nil) then begin
      S.Value := nil;
      N.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode net name }
        CvtToWideChar(NetName, N);

        { call the API }
        Result := _NetShareDelSticky(S.Value, N.Value, Reserved);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(N.Value, N.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetShareCheck(ServerName, Device: string; var ShareType: DWord): NET_API_STATUS;
var
  S, D : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetShareCheck = nil) then
      @_NetShareCheck := GetProcAddress(NETAPI32, 'NetShareCheck');

    if (@_NetShareCheck <> nil) then begin
      S.Value := nil;
      D.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode device name }
        CvtToWideChar(Device, D);

        { call the API }
        Result := _NetShareCheck(S.Value, D.Value, ShareType);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(D.Value, D.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetSessionEnum(ServerName, UncClientName, UserName: string;
                          Level: DWord; var Buffer: Pointer; PrefMaxLen: DWord;
                          var EntriesRead, TotalEntries,
                          ResumeHandle: DWord): NET_API_STATUS;
var
  S, C, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetSessionEnum = nil) then
      @_NetSessionEnum := GetProcAddress(NETAPI32, 'NetSessionEnum');

    if (@_NetSessionEnum <> nil) then begin
      S.Value := nil;
      C.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode UNC client name }
        CvtToWideChar(UncClientName, C);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetSessionEnum(S.Value, C.Value, U.Value, Level, Buffer,
                                  PrefMaxLen, EntriesRead, TotalEntries,
                                  ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(C.Value, C.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetSessionDel(ServerName, UncClientName, UserName: string): NET_API_STATUS;
var
  S, C, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetSessionDel = nil) then
      @_NetSessionDel := GetProcAddress(NETAPI32, 'NetSessionDel');

    if (@_NetSessionDel <> nil) then begin
      S.Value := nil;
      C.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode UNC client name }
        CvtToWideChar(UncClientName, C);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetSessionDel(S.Value, C.Value, U.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(C.Value, C.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetSessionGetInfo(ServerName, UncClientName, UserName: string;
                             Level: DWord; var Buffer: Pointer): NET_API_STATUS;
var
  S, C, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetSessionGetInfo = nil) then
      @_NetSessionGetInfo := GetProcAddress(NETAPI32, 'NetSessionGetInfo');

    if (@_NetSessionGetInfo <> nil) then begin
      S.Value := nil;
      C.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode UNC client name }
        CvtToWideChar(UncClientName, C);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetSessionGetInfo(S.Value, C.Value, U.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(C.Value, C.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetConnectionEnum(ServerName, Qualifier: string; Level: DWord;
                             var Buffer : Pointer; PrefMaxLen : DWord;
                             var EntriesRead, TotalEntries,
                             ResumeHandle: DWord): NET_API_STATUS;
var
  S, Q : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetConnectionEnum = nil) then
      @_NetConnectionEnum := GetProcAddress(NETAPI32, 'NetConnectionEnum');

    if (@_NetConnectionEnum <> nil) then begin
      S.Value := nil;
      Q.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode qualifier }
        CvtToWideChar(Qualifier, Q);

        { call the API }
        Result := _NetConnectionEnum(S.Value, Q.Value, Level, Buffer, PrefMaxLen,
                                     EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(Q.Value, Q.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


function StNetFileClose(ServerName: string; FileID: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetFileClose = nil) then
      @_NetFileClose := GetProcAddress(NETAPI32, 'NetFileClose');

    if (@_NetFileClose <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetFileClose(S.Value, FileID);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetFileEnum(ServerName, BasePath, UserName: string; Level: DWord;
                       var Buffer: Pointer; PrefMexLen: DWord; var EntriesRead,
                       TotalEntries, ResumeHandle: DWord): NET_API_STATUS;
var
  S, B, U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetFileEnum = nil) then
      @_NetFileEnum := GetProcAddress(NETAPI32, 'NetFileEnum');

    if (@_NetFileEnum <> nil) then begin
      S.Value := nil;
      B.Value := nil;
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode base path }
        CvtToWideChar(BasePath, B);

        { allocate the unicode user name }
        CvtToWideChar(UserName, U);

        { call the API }
        Result := _NetFileEnum(S.Value, B.Value, U.Value, Level, Buffer,
                               PrefMexLen, EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(B.Value, B.Length);
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetFileGetInfo(ServerName: string; FileID, Level: DWord;
                          var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetFileGetInfo = nil) then
      @_NetFileGetInfo := GetProcAddress(NETAPI32, 'NetFileGetInfo');

    if (@_NetFileGetInfo <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetFileGetInfo(S.Value, FileID, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


{LMMSG}
function StNetMessageNameAdd(ServerName, MsgName: string): NET_API_STATUS;
var
  S, M : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetMessageNameAdd = nil) then
      @_NetMessageNameAdd := GetProcAddress(NETAPI32, 'NetMessageNameAdd');

    if (@_NetMessageNameAdd <> nil) then begin
      S.Value := nil;
      M.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode message name }
        CvtToWideChar(MsgName, M);

        { call the API }
        Result := _NetMessageNameAdd(S.Value, M.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(M.Value, M.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetMessageNameEnum(ServerName: string; Level: DWord; var Buffer:
                              Pointer; PrefMaxLen: DWord; var EntriesRead,
                              TotalEntries: DWord;
                              ResumeHandle: PDWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetMessageNameEnum = nil) then
      @_NetMessageNameEnum := GetProcAddress(NETAPI32, 'NetMessageNameEnum');

    if (@_NetMessageNameEnum <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetMessageNameEnum(S.Value, Level, Buffer, PrefMaxLen,
                                      EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetMessageNameGetInfo(ServerName, MsgName: string; Level: DWord;
                                 var Buffer: Pointer): NET_API_STATUS;
var
  S, M : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetMessageNameGetInfo = nil) then
      @_NetMessageNameGetInfo := GetProcAddress(NETAPI32, 'NetMessageNameGetInfo');

    if (@_NetMessageNameGetInfo <> nil) then begin
      S.Value := nil;
      M.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode message name }
        CvtToWideChar(MsgName, M);

        { call the API }
        Result := _NetMessageNameGetInfo(S.Value, M.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(M.Value, M.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetMessageNameDel(ServerName, MsgName: string): NET_API_STATUS;
var
  S, M : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetMessageNameDel = nil) then
      @_NetMessageNameDel := GetProcAddress(NETAPI32, 'NetMessageNameDel');

    if (@_NetMessageNameDel <> nil) then begin
      S.Value := nil;
      M.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode message name }
        CvtToWideChar(MsgName, M);

        { call the API }
        Result := _NetMessageNameDel(S.Value, M.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(M.Value, M.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetMessageBufferSend(ServerName, MsgName, FromName: string;
                                Buffer: Pointer; BufferLen: DWord): NET_API_STATUS;
var
  S, M, F : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetMessageBufferSend = nil) then
      @_NetMessageBufferSend := GetProcAddress(NETAPI32, 'NetMessageBufferSend');

    if (@_NetMessageBufferSend <> nil) then begin
      S.Value := nil;
      M.Value := nil;
      F.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode message name }
        CvtToWideChar(MsgName, M);

        { allocate the unicode from name }
        CvtToWideChar(FromName, F);

        { call the API }
        Result := _NetMessageBufferSend(S.Value, M.Value, F.Value, Buffer, BufferLen);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(M.Value, M.Length);
        FreeMem(F.Value, F.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


{LMREMUTL}
function StNetRemoteTOD(UncServerName: string; var Buffer: Pointer): NET_API_STATUS;
var
  U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetRemoteTOD = nil) then
      @_NetRemoteTOD := GetProcAddress(NETAPI32, 'NetRemoteTOD');

    if (@_NetRemoteTOD <> nil) then begin
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(UncServerName, U);

        { call the API }
        Result := _NetRemoteTOD(U.Value, Buffer);
      finally
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetRemoteComputerSupports(UncServerName: string; OptionsWanted: DWord;
                                     var OptionsSupported: DWord): NET_API_STATUS;
var
  U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetRemoteComputerSupports = nil) then
      @_NetRemoteComputerSupports := GetProcAddress(NETAPI32, 'NetRemoteComputerSupports');

    if (@_NetRemoteComputerSupports <> nil) then begin
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(UncServerName, U);

        { call the API }
        Result := _NetRemoteComputerSupports(U.Value, OptionsWanted, OptionsSupported);
      finally
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


{LMSERVER}
function StNetServerEnum(ServerName: string; Level: DWord; var Buffer: Pointer;
                         PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord;
                          ServerType: DWord; Domain: string;
                          var ResumeHandle: DWord): NET_API_STATUS;
var
  S, D : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerEnum = nil) then
      @_NetServerEnum := GetProcAddress(NETAPI32, 'NetServerEnum');

    if (@_NetServerEnum <> nil) then begin
      S.Value := nil;
      D.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode domain name }
        CvtToWideChar(Domain, D);

        { call the API }
        Result := _NetServerEnum(S.Value, Level, Buffer, PrefMaxLen, EntriesRead,
                                 TotalEntries, ServerType, D.Value, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(D.Value, D.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerEnumEx(ServerName: string; Level: DWord; var Buffer: Pointer;
                           PrefMaxLen: DWord; var EntriesRead, TotalEntries: DWord;
                           ServerType: DWord; Domain,
                           FirstNameToReturn : string): NET_API_STATUS;
var
  S, D, F : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerEnumEx = nil) then
      @_NetServerEnumEx := GetProcAddress(NETAPI32, 'NetServerEnumEx');

    if (@_NetServerEnumEx <> nil) then begin
      S.Value := nil;
      D.Value := nil;
      F.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode domain name }
        CvtToWideChar(Domain, D);

        { allocate the unicode first name to return }
        CvtToWideChar(FirstNameToReturn, F);

        { call the API }
        Result := _NetServerEnumEx(S.Value, Level, Buffer, PrefMaxLen,
                                   EntriesRead, TotalEntries, ServerType,
                                   D.Value, F.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(D.Value, D.Length);
        FreeMem(F.Value, F.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerGetInfo(ServerName: string; Level: DWord;
                            var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerGetInfo = nil) then
      @_NetServerGetInfo := GetProcAddress(NETAPI32, 'NetServerGetInfo');

    if (@_NetServerGetInfo <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetServerGetInfo(S.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerSetInfo(ServerName: string; Level: DWord; Buffer: Pointer;
                            var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerSetInfo = nil) then
      @_NetServerSetInfo := GetProcAddress(NETAPI32, 'NetServerSetInfo');

    if (@_NetServerSetInfo <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetServerSetInfo(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerDiskEnum(ServerName: string; Level: DWord;
                             var Buffer: Pointer; PrefMaxLen: DWord;
                             var EntriesRead, TotalEntries,
                             ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerDiskEnum = nil) then
      @_NetServerDiskEnum := GetProcAddress(NETAPI32, 'NetServerDiskEnum');

    if (@_NetServerDiskEnum <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetServerDiskEnum(S.Value, Level, Buffer, PrefMaxLen,
                                     EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerComputerNameAdd(ServerName, EmulatedDomainName,
                                    EmulatedServerName: string): NET_API_STATUS;
var
  S, D, E : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerComputerNameAdd = nil) then
      @_NetServerComputerNameAdd := GetProcAddress(NETAPI32, 'NetServerComputerNameAdd');

    if (@_NetServerComputerNameAdd <> nil) then begin
      S.Value := nil;
      D.Value := nil;
      E.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode domain name }
        CvtToWideChar(EmulatedDomainName, D);

        { allocate the unicode emulated server name }
        CvtToWideChar(EmulatedServerName, E);

        { call the API }
        Result := _NetServerComputerNameAdd(S.Value, D.Value, E.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(D.Value, D.Length);
        FreeMem(E.Value, E.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerComputerNameDel(ServerName, EmulatedServerName: string): NET_API_STATUS;
var
  S, E : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerComputerNameDel = nil) then
      @_NetServerComputerNameDel := GetProcAddress(NETAPI32, 'NetServerComputerNameDel');

    if (@_NetServerComputerNameDel <> nil) then begin
      S.Value := nil;
      E.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode emulated server name }
        CvtToWideChar(EmulatedServerName, E);

        { call the API }
        Result := _NetServerComputerNameDel(S.Value, E.Value);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(E.Value, E.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerTransportAdd(ServerName: string; Level: DWord; Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerTransportAdd = nil) then
      @_NetServerTransportAdd := GetProcAddress(NETAPI32, 'NetServerTransportAdd');

    if (@_NetServerTransportAdd <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetServerTransportAdd(S.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerTransportAddEx(ServerName: WideString; Level: DWord; Buffer: Pointer): NET_API_STATUS;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerTransportAddEx = nil) then
      @_NetServerTransportAddEx := GetProcAddress(NETAPI32, 'NetServerTransportAddEx');

    if (@_NetServerTransportAddEx <> nil) then begin
      try
        Result := _NetServerTransportAddEx(PWideChar(ServerName), Level, Buffer);
      finally
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerTransportDel(ServerName: string; Level: DWord; Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerTransportDel = nil) then
      @_NetServerTransportDel := GetProcAddress(NETAPI32, 'NetServerTransportDel');

    if (@_NetServerTransportDel <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetServerTransportDel(S.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetServerTransportEnum(ServerName: string; Level: DWord;
                                  var Buffer: Pointer; PrefMaxLen: DWord;
                                  var EntriesRead, TotalEntries,
                                  ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetServerTransportEnum = nil) then
      @_NetServerTransportEnum := GetProcAddress(NETAPI32, 'NetServerTransportEnum');

    if (@_NetServerTransportEnum <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetServerTransportEnum(S.Value, Level, Buffer, PrefMaxLen,
                                          EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


{LMUSE}
function StNetUseAdd(UncServerName: string; Level: DWord; Buffer: Pointer;
                     var ParmErr: DWord): NET_API_STATUS;
var
  U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUseAdd = nil) then
      @_NetUseAdd := GetProcAddress(NETAPI32, 'NetUseAdd');

    if (@_NetUseAdd <> nil) then begin
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(UncServerName, U);

        { call the API }
        Result := _NetUseAdd(U.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUseDel(UncServerName, UseName: string; ForceCond: DWord): NET_API_STATUS;
var
  U, N : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUseDel = nil) then
      @_NetUseDel := GetProcAddress(NETAPI32, 'NetUseDel');

    if (@_NetUseDel <> nil) then begin
      U.Value := nil;
      N.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(UncServerName, U);

        { allocate the unicode use name }
        CvtToWideChar(UseName, N);

        { call the API }
        Result := _NetUseDel(U.Value, N.Value, ForceCond);
      finally
        FreeMem(U.Value, U.Length);
        FreeMem(N.Value, N.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUseEnum(UncServerName: string; Level : DWord; var Buffer: Pointer;
                      PrefMaxSize : DWord; var EntriesRead, TotalEntries,
                      ResumeHandle: DWord): NET_API_STATUS;
var
  U : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUseEnum = nil) then
      @_NetUseEnum := GetProcAddress(NETAPI32, 'NetUseEnum');

    if (@_NetUseEnum <> nil) then begin
      U.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(UncServerName, U);

        { call the API }
        Result := _NetUseEnum(U.Value, Level, Buffer, PrefMaxSize, EntriesRead,
                              TotalEntries, ResumeHandle);
      finally
        FreeMem(U.Value, U.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetUseGetInfo(UncServerName, UseName: string; Level: DWord;
                         var Buffer: Pointer): NET_API_STATUS;
var
  U, N : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetUseGetInfo = nil) then
      @_NetUseGetInfo := GetProcAddress(NETAPI32, 'NetUseGetInfo');

    if (@_NetUseGetInfo <> nil) then begin
      U.Value := nil;
      N.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(UncServerName, U);

        { allocate the unicode use name }
        CvtToWideChar(UseName, N);

        { call the API }
        Result := _NetUseGetInfo(U.Value, N.Value, Level, Buffer);
      finally
        FreeMem(U.Value, U.Length);
        FreeMem(N.Value, N.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;


{LMWKSTA}
function StNetWkstaGetInfo(ServerName: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaGetInfo = nil) then
      @_NetWkstaGetInfo := GetProcAddress(NETAPI32, 'NetWkstaGetInfo');

    if (@_NetWkstaGetInfo <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetWkstaGetInfo(S.Value, Level, Buffer);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetWkstaSetInfo(ServerName: string; Level: DWord; Buffer: Pointer;
                           var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaSetInfo = nil) then
      @_NetWkstaSetInfo := GetProcAddress(NETAPI32, 'NetWkstaSetInfo');

    if (@_NetWkstaSetInfo <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetWkstaSetInfo(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetWkstaUserGetInfo(Reserved: string; Level: DWord; var Buffer: Pointer): NET_API_STATUS;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaUserGetInfo = nil) then
      @_NetWkstaUserGetInfo := GetProcAddress(NETAPI32, 'NetWkstaUserGetInfo');

    if (@_NetWkstaUserGetInfo <> nil) then
      Result := _NetWkstaUserGetInfo(nil, Level, Buffer)
    else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetWkstaUserSetInfo(Reserved: string; Level: DWord; Buffer: Pointer;
                               var ParmErr: DWord): NET_API_STATUS;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaUserSetInfo = nil) then
      @_NetWkstaUserSetInfo := GetProcAddress(NETAPI32, 'NetWkstaUserSetInfo');

    if (@_NetWkstaUserSetInfo <> nil) then
      Result := _NetWkstaUserSetInfo(nil, Level, Buffer, ParmErr)
    else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetWkstaUserEnum(ServerName: string; Level: DWord; var Buffer: Pointer;
                            PrefMaxLen: DWord; var EntriesRead, TotalEntries,
                            ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaUserEnum = nil) then
      @_NetWkstaUserEnum := GetProcAddress(NETAPI32, 'NetWkstaUserEnum');

    if (@_NetWkstaUserEnum <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetWkstaUserEnum(S.Value, Level, Buffer, PrefMaxLen,
                                    EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetWkstaTransportAdd(ServerName: string; Level: DWord; Buffer: Pointer;
                                var ParmErr: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaTransportAdd = nil) then
      @_NetWkstaTransportAdd := GetProcAddress(NETAPI32, 'NetWkstaTransportAdd');

    if (@_NetWkstaTransportAdd <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetWkstaTransportAdd(S.Value, Level, Buffer, ParmErr);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetWkstaTransportDel(ServerName, TransportName: string; Ucond: DWord): NET_API_STATUS;
var
  S, T : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaTransportDel = nil) then
      @_NetWkstaTransportDel := GetProcAddress(NETAPI32, 'NetWkstaTransportDel');

    if (@_NetWkstaTransportDel <> nil) then begin
      S.Value := nil;
      T.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode transport name }
        CvtToWideChar(TransportName, T);

        { call the API }
        Result := _NetWkstaTransportDel(S.Value, T.Value, Ucond);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(T.Value, T.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetWkstaTransportEnum(ServerName: string; Level: DWord;
                                 var Buffer: Pointer; PrefMaxLen: DWord;
                                 var EntriesRead, TotalEntries,
                                 ResumeHandle: DWord): NET_API_STATUS;
var
  S : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetWkstaTransportEnum = nil) then
      @_NetWkstaTransportEnum := GetProcAddress(NETAPI32, 'NetWkstaTransportEnum');

    if (@_NetWkstaTransportEnum <> nil) then begin
      S.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { call the API }
        Result := _NetWkstaTransportEnum(S.Value, Level, Buffer, PrefMaxLen,
                                         EntriesRead, TotalEntries, ResumeHandle);
      finally
        FreeMem(S.Value, S.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

{LMAPIBUF}
function StNetApiBufferAllocate(ByteCount: Cardinal; var Buffer: Pointer): NET_API_STATUS;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetApiBufferAllocate = nil) then
      @_NetApiBufferAllocate := GetProcAddress(NETAPI32, 'NetApiBufferAllocate');

    if (@_NetApiBufferAllocate <> nil) then
      Result := _NetApiBufferAllocate(ByteCount, Buffer)
    else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetApiBufferFree(Buffer: Pointer): NET_API_STATUS;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetApiBufferFree = nil) then
      @_NetApiBufferFree := GetProcAddress(NETAPI32, 'NetApiBufferFree');

    if (@_NetApiBufferFree <> nil) then
      Result := _NetApiBufferFree(Buffer)
    else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetApiBufferReallocate(OldBuffer: Pointer; NewByteCount : Cardinal;
                                  var NewBuffer: Pointer): NET_API_STATUS;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetApiBufferReallocate = nil) then
      @_NetApiBufferReallocate := GetProcAddress(NETAPI32, 'NetApiBufferReallocate');

    if (@_NetApiBufferReallocate <> nil) then
      Result := _NetApiBufferReallocate(OldBuffer, NewByteCount, NewBuffer)
    else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

function StNetApiBufferSize(Buffer: Pointer; var ByteCount: DWord): NET_API_STATUS;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetApiBufferSize = nil) then
      @_NetApiBufferSize := GetProcAddress(NETAPI32, 'NetApiBufferSize');

    if (@_NetApiBufferSize <> nil) then
      Result := _NetApiBufferSize(Buffer, ByteCount)
    else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

{LMSTATS}
function StNetStatisticsGet(ServerName, ServiceName: string; Level, Options: DWord;
                            var Buffer: Pointer): NET_API_STATUS;
var
  S, S1 : TLMWideChar;
begin
  Result := 0;
  if IsNetApi32 then begin
    if (@_NetStatisticsGet = nil) then
      @_NetStatisticsGet := GetProcAddress(NETAPI32, 'NetStatisticsGet');

    if (@_NetStatisticsGet <> nil) then begin
      S.Value := nil;
      S1.Value := nil;
      try
        { allocate the unicode server name }
        CvtToWideChar(ServerName, S);

        { allocate the unicode service name }
        CvtToWideChar(ServiceName, S1);

        { call the API }
        Result := _NetStatisticsGet(S.Value, S1.Value, Level, Options, Buffer);
      finally
        FreeMem(S.Value, S.Length);
        FreeMem(S1.Value, S1.Length);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, NETAPI32DLL);
  end;
end;

{WINBASE}
function StGetSidIdentifierAuthority(Sid: PSID): PSIDIdentifierAuthority;
begin
  Result := nil;
  if IsAdvApi32 then begin
    if (@_GetSidIdentifierAuthority = nil) then
      @_GetSidIdentifierAuthority := GetProcAddress(ADVAPI32, 'GetSidIdentifierAuthority');

    if (@_GetSidIdentifierAuthority <> nil) then begin
      try
        Result := _GetSidIdentifierAuthority(Sid);
      finally
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    RaiseStWin32ErrorEx(EStNetException, GetLastError, ADVAPI32DLL);
  end;
end;

function StGetSidSubAuthority(Sid: PSID; SubAuthority: DWord): DWord;
begin
  Result := 0;
  if IsAdvApi32 then begin
    if (@_GetSidSubAuthority = nil) then
      @_GetSidSubAuthority := GetProcAddress(ADVAPI32, 'GetSidSubAuthority');

    if (@_GetSidSubAuthority <> nil) then begin

      try
        Result := _GetSidSubAuthority(Sid, SubAuthority)^;
      finally
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, ADVAPI32DLL);
  end;
end;

function StGetSidSubAuthorityCount(SID: PSID): Byte;
begin
  Result := 0;
  if IsAdvApi32 then begin
    if (@_GetSidSubAuthorityCount = nil) then
      @_GetSidSubAuthorityCount := GetProcAddress(ADVAPI32, 'GetSidSubAuthorityCount');

    if (@_GetSidSubAuthorityCount <> nil) then begin

      try
        Result := _GetSidSubAuthorityCount(Sid)^;
      finally
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, ADVAPI32DLL);
  end;
end;

function StLookupAccountName(SystemName, AccountName: string; var Sid: Pointer;
                             var SidLength: DWord; var DomainName: string;
                             var peUse: DWord): Boolean;
var
  BufLen : DWord;
  Buffer : Pointer;
  ErrorD : DWord;
begin
  Result := False;
  if IsAdvApi32 then begin
    if (@_LookupAccountName = nil) then
      {$IFDEF UNICODE}
      @_LookupAccountName := GetProcAddress(ADVAPI32, 'LookupAccountNameW');
      {$ELSE}
      @_LookupAccountName := GetProcAddress(ADVAPI32, 'LookupAccountNameA');
      {$ENDIF}

    if (@_LookupAccountName <> nil) then begin
      { This just sets up the buffer sizes }
      BufLen := 0;
      Buffer := nil;
      Result := _LookupAccountName(PChar(SystemName), PChar(AccountName),
                                    nil, SidLength, nil, BufLen, peUse);
      ErrorD := GetLastError;

      { now for the real call }
      if ErrorD = ERROR_INSUFFICIENT_BUFFER then begin
        StNetApiBufferAllocate(BufLen, Buffer);
        StNetApiBufferAllocate(SidLength, Sid);
        try
          try
            Result := _LookupAccountName(PChar(SystemName),
                                          PChar(AccountName), Sid, SidLength,
                                          Buffer, BufLen, peUse);
            if Result then
              DomainName := StrPas(PChar(Buffer))
            else
              StNetApiBufferFree(Sid);
          finally
            StNetApiBufferFree(Buffer);
          end;
        except
          StNetApiBufferFree(Sid);
          raise;
        end;
      end else begin
        SetLastError(ErrorD);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, ADVAPI32DLL);
  end;
end;

function StLookupAccountSid(SystemName: string; Sid: PSid; var AccountName,
                            DomainName: string; var peUse: DWord): Boolean;
var
  A, D       : Pointer;
  ALen, DLen : DWord;
  ErrorD     : DWord;
begin
  Result := False;
  if IsAdvApi32 then begin
    if (@_LookupAccountSid = nil) then
    {$IFDEF UNICODE}
      @_LookupAccountSid := GetProcAddress(ADVAPI32, 'LookupAccountSidW');
    {$ELSE}
      @_LookupAccountSid := GetProcAddress(ADVAPI32, 'LookupAccountSidA');
    {$ENDIF}

    if (@_LookupAccountSid <> nil) then begin
      { This just sets up the buffer sizes }
      ALen := 0;
      DLen := 0;

      A := nil;
      D := nil;
      Result := _LookupAccountName(PChar(SystemName), Sid, nil, ALen, nil,
                                    DLen, peUse);
      ErrorD := GetLastError;
      if ErrorD = ERROR_INSUFFICIENT_BUFFER then begin
        StNetApiBufferAllocate(ALen, A);
        StNetApiBufferAllocate(DLen, D);
        try
          Result := _LookupAccountName(PChar(SystemName), Sid, A, ALen, D,
                                        DLen, peUse);
          if Result then begin
            AccountName := StrPas(PChar(A));
            DomainName  := StrPas(PChar(D));
          end;
        finally
          StNetApiBufferFree(A);
          StNetApiBufferFree(D);
        end;
      end else begin
        SetLastError(ErrorD);
      end;
    end else
      RaiseStWin32Error(EStNetException, GetLastError);
  end else begin
    { EStNetException }
    RaiseStWin32ErrorEx(EStNetException, GetLastError, ADVAPI32DLL);
  end;
end;

initialization

finalization
  if (NETAPI32 <> 0) then
    FreeLibrary(NETAPI32);

  if (ADVAPI32 <> 0) then
    FreeLibrary(ADVAPI32);

end.
