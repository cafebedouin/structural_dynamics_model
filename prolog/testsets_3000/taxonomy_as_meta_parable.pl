% ============================================================================
% CONSTRAINT STORY: taxonomy_as_meta_parable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taxonomy_as_meta_parable, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: taxonomy_as_meta_parable
 *   human_readable: Taxonomy as Meta-Parable: Framework Adoption vs Empirical Validation
 *   domain: epistemology/philosophy_of_science/cognitive_science
 *
 * SUMMARY:
 *   The taxonomy-as-meta-parable constraint captures a recursive epistemic
 *   trap: a framework that claims to describe how narrative compression
 *   enables cultural transmission is itself a narrative compression
 *   attempting to achieve cultural transmission. This creates a structural
 *   instability where the framework's adoption can outpace its empirical
 *   validation, leading to premature conceptual lock-in. The constraint
 *   exhibits extraction through citation capture (framework adoption becomes
 *   mandatory for publication), suppression of alternatives (competing
 *   taxonomies face career penalties), and theater (invocation of the
 *   framework becomes performative rather than generative). The meta-parable
 *   structure is inherently vulnerable to the dynamics it describes: if
 *   parables propagate more readily than mechanism-level understanding, then
 *   a taxonomy organized around parables will propagate through its parable
 *   layer (the organizing principle itself) rather than through validation of
 *   its component claims. The measurements show increasing theater ratio and
 *   extractiveness over the interval as the framework transitions from
 *   coordination tool to mandatory citation ritual.
 *
 * KEY AGENTS:
 *   - Framework Authors: Primary beneficiary (institutional/arbitrage) — capture citation advantage, editorial positions, and grant priority through framework adoption
 *   - Early Adopter Scholars: Secondary beneficiary (powerful/mobile) — gain career advantage by mastering framework before it becomes mandatory; retain exit options
 *   - Empirical Validation Process: Primary victim (powerless/trapped) — abstract epistemic commons captured by framework's circular validation structure; cannot exit or organize
 *   - Alternative Framework Developers: Secondary victim (moderate/constrained) — face career penalties for proposing competing taxonomies; exit is costly but possible
 *   - Downstream Researchers: Mixed position (moderate/constrained) — benefit from coordination function but bear extraction cost of forced conceptual framing
 *   - Meta-Science Coalition: Organized agents (organized/constrained) — replication initiatives and open science advocates building alternative validation pathways with sunset logic
 *   - Journal Editorial System: Institutional actor (institutional/arbitrage) — maintains framework as performative ritual; sees own process as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both coordination function and extraction mechanism as structural features
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taxonomy_as_meta_parable, 0.68).
domain_priors:suppression_score(taxonomy_as_meta_parable, 0.72).
domain_priors:theater_ratio(taxonomy_as_meta_parable, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taxonomy_as_meta_parable, extractiveness, 0.68).
narrative_ontology:constraint_metric(taxonomy_as_meta_parable, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taxonomy_as_meta_parable, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taxonomy_as_meta_parable, snare).
narrative_ontology:human_readable(taxonomy_as_meta_parable, "Taxonomy as Meta-Parable: Framework Adoption vs Empirical Validation").
narrative_ontology:topic_domain(taxonomy_as_meta_parable, "epistemology/philosophy_of_science/cognitive_science").

domain_priors:requires_active_enforcement(taxonomy_as_meta_parable).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taxonomy_as_meta_parable, framework_authors).
narrative_ontology:constraint_beneficiary(taxonomy_as_meta_parable, early_adopter_scholars).
narrative_ontology:constraint_victim(taxonomy_as_meta_parable, empirical_validation_process).
narrative_ontology:constraint_victim(taxonomy_as_meta_parable, alternative_framework_developers).
narrative_ontology:constraint_victim(taxonomy_as_meta_parable, downstream_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPIRICAL VALIDATION PROCESS (SNARE) — The abstract epistemic commons cannot exit the framework's gravitational field once it achieves citation dominance. Validation becomes circular: the taxonomy defines what counts as valid evidence, then claims validation from evidence it pre-selected. Maximum extraction — the validation process is captured by the framework it should be testing.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE FRAMEWORK DEVELOPER (SNARE) — Faces career penalty for proposing competing taxonomies once the dominant framework captures journal editorial boards and grant review panels. Exit is structurally possible but professionally costly. The constraint extracts by making the framework's organizing principle the default lens — alternatives must justify their existence against it rather than being evaluated on independent merit.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOWNSTREAM RESEARCHER (TANGLED ROPE) — Benefits from the taxonomy's compression function (it provides ready-made conceptual categories and citation scaffolding) but also bears extraction cost (must frame findings in the taxonomy's terms to achieve publication, even when the fit is forced). Genuine coordination function exists alongside asymmetric extraction. The taxonomy solves a real problem (organizing a complex domain) while simultaneously constraining how that domain can be conceptualized.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FRAMEWORK AUTHORS (ROPE) — Primary beneficiaries who experience the constraint as pure coordination. The taxonomy enables communication of mechanism-level insights that would otherwise remain tacit. Citation accumulation, editorial board positions, and grant success follow from solving a genuine coordination problem. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EARLY ADOPTER SCHOLAR (ROPE) — Gains career advantage by mastering and applying the taxonomy before it becomes mandatory. Experiences low extraction because adoption is voluntary and strategically beneficial. Can exit to alternative frameworks if the taxonomy proves unproductive, but chooses to stay because the coordination benefits are real.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: META-SCIENCE COALITION (SCAFFOLD) — Organized agents (replication initiatives, open science advocates, meta-analytic reviewers) see the taxonomy's dominance as temporary. As empirical evidence accumulates, frameworks that compress poorly or predict weakly will be replaced by better organizing principles. The constraint has a sunset: generational turnover and cumulative evidence eventually route around frameworks that extract more than they coordinate. Estimated sunset: 15-25 years for sufficient evidence to accumulate and alternative frameworks to mature.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: JOURNAL EDITORIAL SYSTEM (PITON) — Maintains the taxonomy as an organizing principle long after its predictive utility has degraded. Reviewers demand that submissions engage with the framework not because it illuminates the phenomenon but because it has become the field's shared vocabulary. The ritual persists through institutional inertia. High theater ratio — the framework is invoked performatively to signal disciplinary competence rather than to generate insight.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes that the taxonomy solves a genuine coordination problem (making mechanism-level structure culturally transmissible) while simultaneously creating extraction risk (framework adoption can outpace empirical validation, leading to premature conceptual lock-in). The meta-parable structure is inherently unstable: a compression layer that claims to describe transmission dynamics is itself subject to those dynamics, creating recursive validation challenges. The analytical perspective sees both the coordination function and the extraction mechanism as structural features of how knowledge frameworks propagate.
constraint_indexing:constraint_classification(taxonomy_as_meta_parable, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taxonomy_as_meta_parable_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taxonomy_as_meta_parable, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taxonomy_as_meta_parable, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taxonomy_as_meta_parable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taxonomy_as_meta_parable, TR),
    TR >= 0.70.

:- end_tests(taxonomy_as_meta_parable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The framework captures career and publication benefits through citation dominance, creating asymmetric extraction from those who must engage with it to achieve professional success. The extraction is severe because the framework's organizing principle can propagate independently of empirical validation — adoption becomes self-reinforcing through network effects rather than through predictive accuracy. Suppression (0.72): High. Significant barriers to proposing alternative frameworks include editorial board capture, grant review panel composition, career risk of challenging dominant paradigms, and the coordination cost of building a competing scholarly community. The suppression is structural rather than conspiratorial — it emerges from the framework's network effects and institutional embedding. Theater ratio (0.65): Moderate-high. Framework invocation increasingly serves performative functions (signaling disciplinary competence, satisfying reviewer expectations) rather than generative functions (producing novel predictions, organizing empirical findings). The theater has increased over the interval as the framework has transitioned from active research tool to mandatory citation ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — a taxonomy attempting to organize a complex domain — appears as pure coordination (Rope) to beneficiaries, mixed coordination-extraction (Tangled Rope) to downstream users and analytical observers, pure extraction (Snare) to victims, temporary problem with sunset (Scaffold) to organized reformers, and degraded ritual (Piton) to institutional maintainers. The framework authors genuinely solve a coordination problem (making mechanism-level structure transmissible) while simultaneously creating extraction risk (premature conceptual lock-in). The perspectival gap is not a disagreement about facts but a difference in structural position: beneficiaries experience the coordination benefits directly, victims bear the extraction costs directly, and analytical observers see both as features of the same mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework authors are primary beneficiaries with arbitrage exit options — they experience the constraint as pure coordination (low d, negative chi). Early adopters are secondary beneficiaries with mobile exit options — they gain strategic advantage while retaining the ability to pivot if the framework proves unproductive (low d, low chi). Downstream researchers occupy a mixed position — they benefit from the coordination function but bear extraction costs when forced to frame findings in the taxonomy's terms (moderate d, moderate chi). Alternative framework developers are victims with constrained exit options — they face career penalties but can exit at significant cost (high d, high chi). The empirical validation process is the primary victim with trapped exit options — it cannot escape the framework's circular validation structure (very high d, maximum chi). The meta-science coalition has organized power and constrained exit — they see a sunset path through cumulative evidence and generational turnover (moderate d, moderate chi). The journal editorial system maintains the framework performatively (piton classification from theater gate rather than high chi). The analytical observer recognizes both coordination and extraction as structural features (moderate d, moderate chi, tangled rope classification).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that coordination and extraction are not mutually exclusive categories but perspectival readings of the same structural dynamics. The framework genuinely coordinates (it provides shared vocabulary and conceptual scaffolding) AND genuinely extracts (it captures citation flows and suppresses alternatives). The Tangled Rope classification from the analytical perspective captures this duality: the taxonomy has both a legitimate coordination function (making complex phenomena culturally transmissible) and an extraction mechanism (framework adoption outpacing empirical validation). The Snare classification from victim perspectives is also legitimate — they experience the constraint as pure extraction because they bear the costs (career penalties, suppressed alternatives, circular validation) without receiving the benefits (citation advantage, editorial positions). The mandatrophy is resolved not by choosing one classification but by recognizing that the presheaf over observation sites IS the answer: the constraint's type depends on where you stand relative to the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_adoption_threshold,
    'At what adoption threshold does a taxonomy transition from coordination tool to extraction mechanism?',
    'Longitudinal analysis of framework citation patterns vs empirical validation milestones; identification of inflection point where citation growth decouples from evidence accumulation',
    'If threshold is low (20-30% field adoption): many frameworks misclassified as extractive when they are still in legitimate coordination phase. If threshold is high (70-80%): extractive lock-in persists unchallenged for extended periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_adoption_threshold, empirical, 'Adoption threshold distinguishing coordination from extraction').

omega_variable(
    compression_fidelity_tradeoff,
    'Does the taxonomy''s compression function necessarily distort the phenomena it organizes, or can high-fidelity compression exist?',
    'Comparison of predictive accuracy between taxonomy-guided research and atheoretical empirical investigation; measurement of information loss in the compression layer',
    'If high-fidelity compression is possible: the framework is pure coordination (Rope from more perspectives). If compression necessarily distorts: the framework is inherently extractive (Snare from more perspectives), and the only question is whether the coordination benefit justifies the epistemic cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compression_fidelity_tradeoff, conceptual, 'Whether compression can preserve empirical fidelity').

omega_variable(
    recursive_validation_paradox,
    'Can a taxonomy that describes transmission dynamics validate itself without circular reasoning?',
    'Philosophical analysis of self-referential validation structures; identification of external validation criteria independent of the framework''s own organizing principles',
    'If self-validation is coherent: the meta-parable structure is stable (lower extractiveness). If self-validation is circular: the framework cannot escape its own transmission constraints (higher extractiveness, confirms Snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recursive_validation_paradox, conceptual, 'Whether self-referential frameworks can achieve non-circular validation').

omega_variable(
    alternative_framework_suppression_mechanism,
    'Is the suppression of alternative frameworks a structural feature of taxonomy dominance or a contingent social phenomenon?',
    'Cross-disciplinary comparison of framework turnover rates; analysis of whether dominant taxonomies in other fields exhibit similar suppression patterns or whether suppression varies with institutional structure',
    'If structural: suppression is inherent to framework adoption (confirms high suppression score). If contingent: institutional reform could reduce suppression without reducing coordination benefit (suggests Tangled Rope rather than Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_suppression_mechanism, empirical, 'Whether alternative framework suppression is structural or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taxonomy_as_meta_parable, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tax_meta_tr_t0, taxonomy_as_meta_parable, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tax_meta_tr_t3, taxonomy_as_meta_parable, theater_ratio, 3, 0.48).
narrative_ontology:measurement(tax_meta_tr_t6, taxonomy_as_meta_parable, theater_ratio, 6, 0.58).
narrative_ontology:measurement(tax_meta_tr_t10, taxonomy_as_meta_parable, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(tax_meta_be_t0, taxonomy_as_meta_parable, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tax_meta_be_t3, taxonomy_as_meta_parable, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(tax_meta_be_t6, taxonomy_as_meta_parable, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(tax_meta_be_t10, taxonomy_as_meta_parable, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taxonomy_as_meta_parable, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of both parable_as_transmission_layer (mountain — the structural claim that narrative compression enables cultural transmission) and empirical_social_substrate_split (tangled_rope — the tension between empirical validation and social adoption). The taxonomy inherits structural features from both: it attempts to leverage parable-layer transmission (from the mountain constraint) while navigating the empirical-social split (from the tangled_rope constraint). The meta-parable's extractiveness (0.68) is higher than either upstream constraint because it compounds their risks: it uses narrative compression (parable layer) to organize claims about empirical-social dynamics, creating recursive validation challenges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
