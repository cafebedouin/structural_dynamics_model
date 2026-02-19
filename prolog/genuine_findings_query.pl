% ============================================================================
% DIAGNOSTIC QUERY SCRIPT — Full profiles for 6 genuine abductive findings
% ============================================================================
% Run: cd prolog && swipl -l stack.pl -l covering_analysis.pl \
%        -l dirac_classification.pl -l maxent_classifier.pl \
%        -l abductive_engine.pl -l genuine_findings_query.pl \
%        -g "run_query, halt."
% ============================================================================

:- use_module(covering_analysis).
:- use_module(config).
:- use_module(narrative_ontology).
:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(signature_detection, [constraint_signature/2, structural_purity/2]).
:- use_module(boltzmann_compliance, [boltzmann_compliant/2, cross_index_coupling/2]).
:- use_module(purity_scoring, [purity_score/2, factorization_subscore/2, scope_invariance_subscore/2, coupling_cleanliness_subscore/2, excess_extraction_subscore/2]).
:- use_module(dirac_classification).
:- use_module(maxent_classifier).
:- use_module(drl_lifecycle).
:- use_module(logical_fingerprint).
:- use_module(abductive_engine).

:- use_module(library(lists)).

run_query :-
    format(user_error, '[query] Loading corpus...~n', []),
    corpus_loader:load_all_testsets,
    constraint_indexing:default_context(Context),

    % Run MaxEnt
    format(user_error, '[query] Running MaxEnt...~n', []),
    maxent_classifier:maxent_run(Context, _),

    % Run abductive engine
    format(user_error, '[query] Running abductive engine...~n', []),
    abductive_engine:abductive_run(Context, _Summary),

    % Get genuine findings
    abductive_engine:abductive_genuine(Context, AllGenuine),
    length(AllGenuine, NGenuine),
    format('GENUINE_COUNT: ~w~n~n', [NGenuine]),

    % Profile each genuine finding
    forall(
        member(H, AllGenuine),
        profile_hypothesis(H, Context)
    ).

profile_hypothesis(hypothesis(C, Class, Anomaly, EvidenceLines, Explanations, Confidence, Investigation), Context) :-
    format('========================================~n'),
    format('CONSTRAINT: ~w~n', [C]),
    format('HYPOTHESIS_CLASS: ~w~n', [Class]),
    format('ANOMALY: ~w~n', [Anomaly]),
    format('CONFIDENCE: ~4f~n', [Confidence]),
    format('INVESTIGATION: ~w~n', [Investigation]),
    format('~n'),

    % --- Deterministic classifier ---
    format('--- DETERMINISTIC CLASSIFIER ---~n'),
    (drl_core:dr_type(C, Context, DetType) -> true ; DetType = unknown),
    format('DR_TYPE: ~w~n', [DetType]),
    (drl_core:base_extractiveness(C, BaseEps) -> true ; BaseEps = unknown),
    format('BASE_EXTRACTIVENESS: ~w~n', [BaseEps]),
    (drl_core:get_raw_suppression(C, RawSupp) -> true ; RawSupp = unknown),
    format('RAW_SUPPRESSION: ~w~n', [RawSupp]),
    (   config:param(theater_metric_name, TM),
        narrative_ontology:constraint_metric(C, TM, TR)
    ->  true ; TR = 0.0),
    format('THEATER_RATIO: ~w~n', [TR]),
    (constraint_indexing:extractiveness_for_agent(C, Context, Chi) -> true ; Chi = unknown),
    format('CHI_VALUE: ~w~n', [Chi]),

    % Classify from metrics to find which clause matched
    format('~n--- THRESHOLD ANALYSIS ---~n'),
    (   drl_core:base_extractiveness(C, BE2),
        constraint_indexing:extractiveness_for_agent(C, Context, Chi2),
        drl_core:get_raw_suppression(C, Supp2)
    ->  % Check each type
        (drl_core:classify_from_metrics(C, BE2, Chi2, Supp2, Context, mountain) -> format('MATCHES_MOUNTAIN: true~n') ; format('MATCHES_MOUNTAIN: false~n')),
        (drl_core:classify_from_metrics(C, BE2, Chi2, Supp2, Context, snare) -> format('MATCHES_SNARE: true~n') ; format('MATCHES_SNARE: false~n')),
        (drl_core:classify_from_metrics(C, BE2, Chi2, Supp2, Context, scaffold) -> format('MATCHES_SCAFFOLD: true~n') ; format('MATCHES_SCAFFOLD: false~n')),
        (drl_core:classify_from_metrics(C, BE2, Chi2, Supp2, Context, rope) -> format('MATCHES_ROPE: true~n') ; format('MATCHES_ROPE: false~n')),
        (drl_core:classify_from_metrics(C, BE2, Chi2, Supp2, Context, tangled_rope) -> format('MATCHES_TANGLED_ROPE: true~n') ; format('MATCHES_TANGLED_ROPE: false~n')),
        (drl_core:classify_from_metrics(C, BE2, Chi2, Supp2, Context, piton) -> format('MATCHES_PITON: true~n') ; format('MATCHES_PITON: false~n')),

        % Nearest thresholds
        config:param(rope_chi_ceiling, RCC),
        config:param(snare_chi_floor, SCF),
        config:param(tangled_rope_chi_floor, TRCF),
        format('DIST_FROM_ROPE_CHI_CEIL: ~4f~n', [abs(Chi2 - RCC)]),
        format('DIST_FROM_SNARE_CHI_FLOOR: ~4f~n', [abs(Chi2 - SCF)]),
        format('DIST_FROM_TR_CHI_FLOOR: ~4f~n', [abs(Chi2 - TRCF)])
    ;   true
    ),

    % --- Structural signatures ---
    format('~n--- STRUCTURAL SIGNATURES ---~n'),
    (signature_detection:constraint_signature(C, Sig) -> true ; Sig = unknown),
    format('SIGNATURE: ~w~n', [Sig]),
    (purity_scoring:purity_score(C, Purity) -> true ; Purity = unknown),
    format('PURITY_SCORE: ~w~n', [Purity]),
    (boltzmann_compliance:boltzmann_compliant(C, BoltzResult) -> true ; BoltzResult = unknown),
    format('BOLTZMANN: ~w~n', [BoltzResult]),
    (boltzmann_compliance:cross_index_coupling(C, CouplingScore) -> true ; CouplingScore = unknown),
    format('COUPLING: ~w~n', [CouplingScore]),
    (signature_detection:structural_purity(C, SPurity) -> true ; SPurity = unknown),
    format('STRUCTURAL_PURITY: ~w~n', [SPurity]),

    % Purity subscores
    (purity_scoring:factorization_subscore(C, FS) -> true ; FS = unknown),
    (purity_scoring:scope_invariance_subscore(C, SIS) -> true ; SIS = unknown),
    (purity_scoring:coupling_cleanliness_subscore(C, CCS) -> true ; CCS = unknown),
    (purity_scoring:excess_extraction_subscore(C, EES) -> true ; EES = unknown),
    format('PURITY_SUBSCORES: factorization=~w scope_inv=~w coupling=~w excess=~w~n',
           [FS, SIS, CCS, EES]),

    % --- MaxEnt ---
    format('~n--- MAXENT ---~n'),
    (maxent_classifier:maxent_distribution(C, Context, Dist) -> format('DISTRIBUTION: ~w~n', [Dist]) ; format('DISTRIBUTION: unavailable~n')),
    (maxent_classifier:maxent_entropy(C, Context, HNorm) -> format('ENTROPY: ~4f~n', [HNorm]) ; format('ENTROPY: unavailable~n')),
    (maxent_classifier:maxent_confidence(C, Context, MConf) -> format('MAXENT_CONFIDENCE: ~4f~n', [MConf]) ; format('MAXENT_CONFIDENCE: unavailable~n')),
    (maxent_classifier:maxent_disagreement(C, Context, Disagree) -> format('DISAGREEMENT: ~w~n', [Disagree]) ; format('DISAGREEMENT: unavailable~n')),
    (maxent_classifier:maxent_top_type(C, Context, TopType) -> format('TOP_TYPE: ~w~n', [TopType]) ; format('TOP_TYPE: unavailable~n')),

    % --- Dirac orbits ---
    format('~n--- DIRAC ORBITS ---~n'),
    (dirac_classification:gauge_orbit(C, OrbitPoints) -> format('ORBIT: ~w~n', [OrbitPoints]) ; format('ORBIT: unavailable~n')),
    (   dirac_classification:preserved_under_context_shift(C, PreservedResult)
    ->  format('PRESERVED: ~w~n', [PreservedResult])
    ;   format('PRESERVED: unavailable~n')
    ),

    % Orbit type list
    (   dirac_classification:gauge_orbit(C, OP2)
    ->  findall(T, member(orbit_point(T, _), OP2), OTypes),
        sort(OTypes, UniqueOTypes),
        length(UniqueOTypes, NUniqueO),
        format('ORBIT_TYPES: ~w (~w unique)~n', [UniqueOTypes, NUniqueO])
    ;   true
    ),

    % --- Drift detection ---
    format('~n--- DRIFT ---~n'),
    (   drl_lifecycle:scan_constraint_drift(C, DriftEvents)
    ->  (   DriftEvents = []
        ->  format('DRIFT_EVENTS: none~n')
        ;   format('DRIFT_EVENTS: ~w~n', [DriftEvents])
        )
    ;   format('DRIFT_EVENTS: scan_failed~n')
    ),

    % --- Perspectival gaps ---
    format('~n--- MISMATCHES ---~n'),
    findall(
        mismatch(Ctx2, ErrType, Severity),
        drl_core:dr_mismatch(C, Ctx2, ErrType, Severity),
        Mismatches
    ),
    (   Mismatches = []
    ->  format('MISMATCHES: none~n')
    ;   forall(member(M, Mismatches), format('MISMATCH: ~w~n', [M]))
    ),

    % --- Logical fingerprints ---
    format('~n--- FINGERPRINT ---~n'),
    (logical_fingerprint:fingerprint_voids(C, Voids) -> format('VOIDS: ~w~n', [Voids]) ; format('VOIDS: unavailable~n')),
    (logical_fingerprint:fingerprint_shift(C, Shift) -> format('SHIFT: ~w~n', [Shift]) ; format('SHIFT: unavailable~n')),
    (logical_fingerprint:fingerprint_zone(C, Zone) -> format('ZONE: ~w~n', [Zone]) ; format('ZONE: unavailable~n')),
    (logical_fingerprint:fingerprint_coupling(C, FPCoupling) -> format('FP_COUPLING: ~w~n', [FPCoupling]) ; format('FP_COUPLING: unavailable~n')),

    % --- Hypothesis details ---
    format('~n--- HYPOTHESIS ---~n'),
    format('EVIDENCE_LINES:~n'),
    forall(member(EL, EvidenceLines), format('  ~w~n', [EL])),
    format('EXPLANATIONS:~n'),
    forall(member(Ex, Explanations), format('  ~w~n', [Ex])),

    format('~n========================================~n~n').
