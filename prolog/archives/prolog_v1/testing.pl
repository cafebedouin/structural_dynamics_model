:- module(testing, [check/2, check/1]).

:- use_module(scenario_manager).
:- use_module(report_generator).
:- use_module(data_verification).

/**
 * check(+File, +IntervalID)
 * 1. Clears the Knowledge Base.
 * 2. Loads the target data file.
 * 3. Verifies data integrity (completeness/ranges).
 * 4. Generates the full structural analysis report.
 */
check(File, IntervalID) :-
    % Step 1: Clear and Load
    scenario_manager:load_and_run(File, IntervalID),
    
    % Step 2: Detailed Integrity Check
    format('~n--- [DATA INTEGRITY CHECK] ---~n'),
    data_verification:verify_interval_completeness(IntervalID),
    
    % Step 3: Generate Analysis Report
    report_generator:generate_full_report(IntervalID).

/**
 * check(+File)
 * A convenience version for files where the IntervalID 
 * is the same name as the file (minus .pl).
 */
check(File) :-
    atom_concat(IntervalID, '.pl', File),
    check(File, IntervalID).

:- writeln('╔════════════════════════════════════════════════════════════════╗').
:- writeln('║  INDIVIDUAL FILE TESTER LOADED                                 ║').
:- writeln('╟────────────────────────────────────────────────────────────────╢').
:- writeln('║  Usage: ?- check(\'filename.pl\', interval_id).                 ║').
:- writeln('╚════════════════════════════════════════════════════════════════╝').
