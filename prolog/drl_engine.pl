% ============================================================================
% CORE ENGINE: drl_engine.pl
% ============================================================================
:- module(drl_engine, [
    load_corpus/1,
    evaluate_tension/3,
    structural_twin/3
]).

% --- Logic Constants (Section II-D) ---
% Calibration values for the Power Modifier function (pi)
power_modifier(individual_powerless, 1.5).   % Experience MORE extraction
power_modifier(individual_moderate, 1.0).    % Baseline
power_modifier(individual_powerful, 0.5).    % Experience LESS extraction
power_modifier(collective_organized, 0.7).   % Shared burden
power_modifier(institutional, -0.2).         % NET BENEFICIARY
power_modifier(analytical, 1.0).             % Neutral observer

% --- 1. INGESTION ---
load_corpus(Directory) :-
    directory_files(Directory, Files),
    include(is_prolog_file, Files, CleanFiles),
    maplist(ingest_constraint, CleanFiles).

ingest_constraint(File) :-
    % Loads the data entries without logic module collisions
    consult(File).

% --- 2. EVALUATION: The chi Function ---
% chi(C, P) = X_base(C) * pi(P)
evaluate_tension(ConstraintID, AgentIndex, result(Chi, Type)) :-
    % Access the standardized fact structure
    constraint_data(ConstraintID, Data),
    member(base_extractiveness(X_base), Data),
    AgentIndex = context(Power, _, _, _),
    power_modifier(Power, Pi),
    Chi is X_base * Pi,
    classify_by_chi(Chi, Data, Type).

% --- 3. CLASSIFICATION LOGIC (Section III-A) ---
classify_by_chi(Chi, Data, tangled_rope) :-
    % TR: Hybrid coordination/extraction (0.40 <= chi <= 0.90)
    Chi >= 0.40, Chi <= 0.90,
    member(coord_function(true), Data), !.

classify_by_chi(Chi, _, snare) :-
    % Snare: Pure extraction (chi >= 0.66)
    Chi >= 0.66, !.

classify_by_chi(Chi, _, rope) :-
    % Rope: Pure coordination (chi <= 0.35)
    Chi <= 0.35, !.

classify_by_chi(_, _, mountain) :-
    % Mountain: Default for natural/unscaled limits
    true.

% --- 4. ISOMORPHISM (CROSS-DOMAIN ANALYSIS) ---
structural_twin(C1, C2, Index) :-
    evaluate_tension(C1, Index, result(Chi1, _)),
    evaluate_tension(C2, Index, result(Chi2, _)),
    abs(Chi1 - Chi2) < 0.05, % Isomorphic tolerance
    C1 \= C2.

is_prolog_file(F) :- file_name_extension(_, pl, F).

