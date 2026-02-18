% ============================================================================
% CONSTRAINT STORY: hammurabi_lex_talionis
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_hammurabi_lex_talionis, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: hammurabi_lex_talionis
 * human_readable: The Law of Retaliation (Lex Talionis) in Hammurabi's Code
 * domain: political/social
 * * SUMMARY:
 * A rigid legal system from ancient Mesopotamia based on reciprocal justice ("an eye for an eye"),
 * where penalties are strictly stratified by social class. It enforces social
 * order through high-stakes physical and financial liability, providing stability
 * for commerce while cementing a severe class-based asymmetry.
 * * KEY AGENTS:
 * - The Slave (Wardu): Subject (Powerless), property status with minimal agency.
 * - The Gentleman (Amelu): Beneficiary (Powerful), elite with high protection but also high liability.
 * - The King (Hammurabi): Beneficiary (Institutional), source of enforcement.
 * - The Historian: Auditor (Analytical), observing the structure across millennia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(hammurabi_lex_talionis, 0.50). % High enough for Snare/Tangled Rope, reflects functional stability.
domain_priors:suppression_score(hammurabi_lex_talionis, 0.80).   % Alternatives framed as rebellion; law is divinely ordained.
domain_priors:theater_ratio(hammurabi_lex_talionis, 0.10).       % Highly functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hammurabi_lex_talionis, extractiveness, 0.50).
narrative_ontology:constraint_metric(hammurabi_lex_talionis, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(hammurabi_lex_talionis, theater_ratio, 0.10).

% Constraint self-claim: The code claims to be divine, immutable law.
narrative_ontology:constraint_claim(hammurabi_lex_talionis, tangled_rope).
narrative_ontology:human_readable(hammurabi_lex_talionis, "The Law of Retaliation (Lex Talionis) in Hammurabi's Code").
narrative_ontology:topic_domain(hammurabi_lex_talionis, "political/social").

% Binary flags & structural properties
domain_priors:requires_active_enforcement(hammurabi_lex_talionis). % Requires state-level execution/mutilation.
narrative_ontology:constraint_beneficiary(hammurabi_lex_talionis, amelu_class). % Derives has_coordination_function/1
narrative_ontology:constraint_victim(hammurabi_lex_talionis, wardu_class). % Derives has_asymmetric_extraction/1

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SLAVE (SNARE)
% Subject to physical mutilation with zero exit options. The law is a trap.
constraint_indexing:constraint_classification(hammurabi_lex_talionis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE GENTLEMAN/AMELU (ROPE)
% For the elite, the law is a coordination mechanism protecting property and
% providing predictable outcomes. Extraction is felt as a reasonable cost.
% χ = 0.50 * π(powerful:0.6) * σ(regional:0.9) = 0.27 (<= 0.35 Rope threshold)
constraint_indexing:constraint_classification(hammurabi_lex_talionis, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE KING/INSTITUTION (ROPE)
% From the perspective of the state, the law is pure coordination infrastructure.
% The institutional power modifier makes effective extraction negative.
constraint_indexing:constraint_classification(hammurabi_lex_talionis, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system has a genuine coordination function (stability) but also
% clear asymmetric extraction and requires active enforcement. This is the
% canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(hammurabi_lex_talionis, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hammurabi_lex_talionis_tests).

test(perspectival_gap_slave_vs_king) :-
    constraint_indexing:constraint_classification(hammurabi_lex_talionis, TypeSlave,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hammurabi_lex_talionis, TypeKing,
        context(agent_power(institutional), _, _, _)),
    assertion(TypeSlave == snare),
    assertion(TypeKing == rope),
    assertion(TypeSlave \= TypeKing).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(hammurabi_lex_talionis, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

:- end_tests(hammurabi_lex_talionis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect a system that is both functional and brutal.
 * Extractiveness (0.50) is moderate because the code imposes severe liability
 * on the powerful (e.g., a builder is killed if their house collapses and kills
 * the owner), providing genuine stability. Suppression (0.80) is high as the
 * law is presented as divine and immutable. The key insight is the perspectival
 * gap: it is a Snare for a slave, a Rope for the elite, but structurally a
 * Tangled Rope for the analytical observer. The system's claim to be
 * `natural_law` (a Mountain) is falsified by the analysis, which reveals its
 * constructed, extractive nature.
 *
 * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A pure Snare classification
 * would miss the genuine coordination function that allowed Babylonian society
 * to flourish. A pure Rope classification would ignore the brutal, asymmetric
 * extraction imposed on the lower classes. The Tangled Rope classification
 * correctly identifies the constraint's dual nature, preventing mislabeling
 * and providing a more accurate model of how such systems persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_hammurabi,
    'To what extent was the Code a codification of existing customary law versus a top-down imposition of royal power?',
    'Archaeological evidence of pre-Code legal rulings and contracts from different social strata.',
    'If mostly customary, it functioned more like a Rope that was later formalized. If mostly imposed, it was a constructed Snare from the outset.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hammurabi_lex_talionis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This was a highly stable, codified system. The metrics are modeled as
% constant across its effective lifecycle, showing no significant drift after
% its establishment.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(hlt_tr_t0, hammurabi_lex_talionis, theater_ratio, 0, 0.10).
narrative_ontology:measurement(hlt_tr_t5, hammurabi_lex_talionis, theater_ratio, 5, 0.10).
narrative_ontology:measurement(hlt_tr_t10, hammurabi_lex_talionis, theater_ratio, 10, 0.10).

% Extraction over time (stable):
narrative_ontology:measurement(hlt_ex_t0, hammurabi_lex_talionis, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(hlt_ex_t5, hammurabi_lex_talionis, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(hlt_ex_t10, hammurabi_lex_talionis, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The code is a foundational legal framework.
narrative_ontology:coordination_type(hammurabi_lex_talionis, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */