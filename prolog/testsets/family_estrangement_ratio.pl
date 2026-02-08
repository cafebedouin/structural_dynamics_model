% ============================================================================
% CONSTRAINT STORY: family_estrangement_ratio
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_family_estrangement_ratio, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: family_estrangement_ratio
 * human_readable: The "Family is Forever" Dogma
 * domain: social
 * * SUMMARY:
 * The social dogma that "family is forever" suppresses the option for individuals to cleanly end harmful relationships. This forces a choice between enduring harm (often due to a "bad magic ratio" of negative interactions) or suffering social liquidation (stigma, exclusion from life events). The dogma serves to maintain the stability of the broader family network at the direct expense of the estranged individual's well-being and social access.
 * * KEY AGENTS:
 * - The Estranged Individual: Subject, forced to choose between harm and social isolation.
 * - The Family Network: Beneficiary, whose stability is preserved by the "forever" precedent.
 * - The Analytical Observer: Auditor, who identifies the hybrid coordination/extraction function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(family_estrangement_ratio, 0.80). % Extracts social standing, access to rituals, and family support.
domain_priors:suppression_score(family_estrangement_ratio, 0.70).   % Suppresses the alternative of clean relationship closure.
domain_priors:theater_ratio(family_estrangement_ratio, 0.10).       % Low theater; the enforcement and consequences are real.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(family_estrangement_ratio, extractiveness, 0.80).
narrative_ontology:constraint_metric(family_estrangement_ratio, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(family_estrangement_ratio, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a mechanism for social cohesion and support.
narrative_ontology:constraint_claim(family_estrangement_ratio, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(family_estrangement_ratio). % Enforced by family members pushing for reconciliation and judging silence.

% Structural property derivation hooks:
% The presence of both a beneficiary and a victim is the key signature of a Tangled Rope.
narrative_ontology:constraint_beneficiary(family_estrangement_ratio, family_network_stability). % The precedent of "forever" protects the group structure.
narrative_ontology:constraint_victim(family_estrangement_ratio, estranged_individual). % The individual is extracted of social access for their safety.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE ESTRANGED INDIVIDUAL (SNARE)
% The dogma is a trap. Staying means enduring harm; leaving means social liquidation
% and stigma. Both options are extractive.
constraint_indexing:constraint_classification(family_estrangement_ratio, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE FAMILY NETWORK (ROPE)
% For the network, the "forever" dogma is a coordination tool (a Rope) that binds
% the fabric together, ensuring continuity and stability across generations.
constraint_indexing:constraint_classification(family_estrangement_ratio, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees the dual function: it's a coordination mechanism for the group
% (beneficiary) that relies on asymmetric extraction from individuals (victims)
% and requires active enforcement. This is the canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(family_estrangement_ratio, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_estrangement_ratio_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(family_estrangement_ratio, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_estrangement_ratio, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(family_estrangement_ratio, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties_present) :-
    % Verify all three required properties for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(family_estrangement_ratio, _),
    narrative_ontology:constraint_victim(family_estrangement_ratio, _),
    domain_priors:requires_active_enforcement(family_estrangement_ratio).

:- end_tests(family_estrangement_ratio_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the perspectival gap between the individual and the group. For the estranged individual (powerless, trapped), the "family is forever" dogma is a Snare that forces a choice between two harmful outcomes. For the family network (institutional, mobile), the same dogma is a Rope that ensures group cohesion and stability.
 *
 * The analytical perspective resolves this tension by classifying it as a Tangled Rope. This is the correct classification because the constraint possesses all three required properties:
 * 1. A coordination function (maintaining network stability for beneficiaries).
 * 2. Asymmetric extraction (cost is borne by the victim for their own safety).
 * 3. Active enforcement (social pressure, judgment, pushing for reconciliation).
 *
 * The original file's classification of the analytical view as a Mountain was incorrect. While the "magic ratio" of interactions might be a mountain-like psychological law, the *social constraint* built upon it is a constructed, hybrid system.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification correctly prevents mandatrophy. It acknowledges the valid coordination function perceived by the beneficiary (the Rope view) while simultaneously accounting for the severe, asymmetric extraction experienced by the victim (the Snare view). The system does not incorrectly dismiss the entire structure as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_family_estrangement_ratio_1,
    'Is the stigma of estrangement a functional protection of the family unit or an extractive tool to force compliance?',
    'Comparison of health outcomes for individuals in reconcile-at-all-costs families vs. closure-accepting families.',
    'If protection: leans Mountain. If extraction: confirms Snare/Tangled Rope.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_family_estrangement_ratio_2,
    'Can a larger family network actually heal divisions, or is the prevalence of the silent epidemic proof that the network is a failed Scaffold?',
    'Audit of long-term reconciliation rates mediated by extended family networks.',
    'If efficacy is high: Rope function is strong. If low: Rope function is largely theatrical, closer to a Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(family_estrangement_ratio, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This social dogma is deeply entrenched and has not changed significantly over the
% measured interval. The extraction and low theater ratio are stable properties.
%
% Theater ratio over time (stable):
narrative_ontology:measurement(fer_tr_t0, family_estrangement_ratio, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fer_tr_t5, family_estrangement_ratio, theater_ratio, 5, 0.10).
narrative_ontology:measurement(fer_tr_t10, family_estrangement_ratio, theater_ratio, 10, 0.10).

% Extraction over time (stable):
narrative_ontology:measurement(fer_ex_t0, family_estrangement_ratio, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(fer_ex_t5, family_estrangement_ratio, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(fer_ex_t10, family_estrangement_ratio, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The dogma functions as a mechanism to enforce a particular social structure.
narrative_ontology:coordination_type(family_estrangement_ratio, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */