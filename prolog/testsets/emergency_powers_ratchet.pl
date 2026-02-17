% ============================================================================
% CONSTRAINT STORY: emergency_powers_ratchet
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_emergency_powers_ratchet, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: emergency_powers_ratchet
 * human_readable: The Permanent Crisis Scaffold
 * domain: political/legal/social
 * * SUMMARY:
 * This constraint represents the systemic tendency for temporary "emergency"
 * legal powers, enacted during a crisis (war, pandemic, economic collapse),
 * to persist and become normalized as permanent administrative fixtures.
 * While initially a "Scaffold" for urgent coordination, the ratchet mechanism
 * transforms it into a "Tangled Rope" or "Snare" for the populace, as the
 * high-suppression state outlives the crisis that justified it.
 * * KEY AGENTS:
 * - Civil Subject: Subject (Powerless)
 * - Executive State Body: Beneficiary (Institutional)
 * - Constitutional Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the normalization of emergency law siphons
% the subject's fundamental liberties into the state's permanent
% administrative surplus.
domain_priors:base_extractiveness(emergency_powers_ratchet, 0.84).
domain_priors:suppression_score(emergency_powers_ratchet, 0.78).
domain_priors:theater_ratio(emergency_powers_ratchet, 0.65). % Crisis theater remains necessary to justify the "temporary" status.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(emergency_powers_ratchet, extractiveness, 0.84).
narrative_ontology:constraint_metric(emergency_powers_ratchet, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(emergency_powers_ratchet, theater_ratio, 0.65).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a temporary coordination mechanism for public good.
narrative_ontology:constraint_claim(emergency_powers_ratchet, tangled_rope).
narrative_ontology:human_readable(emergency_powers_ratchet, "The Permanent Crisis Scaffold").

% Binary flags
% Required for Scaffold classification. The clauses exist on paper, but their
% enforcement or renewal is the locus of the ratchet.
narrative_ontology:has_sunset_clause(emergency_powers_ratchet).
% Required for Tangled Rope. The powers are not self-enforcing.
domain_priors:requires_active_enforcement(emergency_powers_ratchet).

% Structural property derivation hooks:
% Required for Tangled Rope and Scaffold classifications.
narrative_ontology:constraint_beneficiary(emergency_powers_ratchet, executive_state_body).
narrative_ontology:constraint_victim(emergency_powers_ratchet, civil_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: the "emergency" measures restrict their
% options for exit or redress, yet there is no observable end-point
% to the state of exception.
constraint_indexing:constraint_classification(emergency_powers_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The state views the expanded powers as a Rope—the only way to coordinate
% complex societal responses and maintain order in an increasingly
% volatile environment.
constraint_indexing:constraint_classification(emergency_powers_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.84) and suppression (0.78) with both coordination
% and asymmetric extraction functions trigger the Tangled Rope signature.
constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE LEGAL ARCHITECT (SCAFFOLD)
% During the initial 'immediate' horizon, the powers are perceived as a
% temporary Scaffold for coordination, justified by the sunset clause.
constraint_indexing:constraint_classification(emergency_powers_ratchet, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(emergency_powers_ratchet).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_powers_ratchet_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional state.
    constraint_indexing:constraint_classification(emergency_powers_ratchet, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_powers_ratchet, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(emergency_powers_ratchet, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % This is a high-extraction constraint, validating it exceeds the Snare threshold.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(emergency_powers_ratchet, ExtMetricName, E),
    E >= 0.46.

:- end_tests(emergency_powers_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the
 * coordination benefit of the emergency has been subsumed by the permanent
 * extraction of civil liberties. The suppression score (0.78) reflects the
 * high coercion needed to maintain this state of exception. The temporal
 * data models the "ratchet" effect, where extraction and theater start low
 * during a genuine crisis and then accumulate over time as the powers become
 * institutionalized. The presence of a (deceptively) temporary sunset clause
 * allows for the initial Scaffold classification, which is key to the
 * constraint's political viability.
 *
 * * PERSPECTIVAL GAP:
 * The Civil Subject feels a Snare because their legal baseline is
 * permanently lowered. The Executive Body sees a Rope because the
 * administrative efficiency gained during the crisis is too valuable to
 * relinquish for future coordination needs.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction (0.84) risks a simple Snare classification, which would
 * ignore the constraint's origins and ongoing function as a coordination
 * mechanism (however distorted). The Tangled Rope classification resolves this
 * by acknowledging both the genuine coordination intent (Rope) that is now
 * inextricably "tangled" with a high-extraction predatory structure (Snare).
 * This prevents mislabeling and allows for more nuanced analysis of how to
 * "untangle" the two functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_crisis_definition_epr,
    'Is the "crisis" a discrete event (making the powers a Snare) or a permanent feature of the environment (making them a Mountain)?',
    'Tracking the frequency of crisis declarations vs the expiration of associated powers over a historical time horizon.',
    'If powers expire: Snare of policy. If crises are constant and powers are necessary: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(emergency_powers_ratchet, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the ratchet effect. The constraint begins as a
% low-extraction, low-theater Scaffold during a real crisis, but extraction
% and performative theater accumulate as the powers are normalized.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(epr_tr_t0, emergency_powers_ratchet, theater_ratio, 0, 0.10).
narrative_ontology:measurement(epr_tr_t5, emergency_powers_ratchet, theater_ratio, 5, 0.45).
narrative_ontology:measurement(epr_tr_t10, emergency_powers_ratchet, theater_ratio, 10, 0.65).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(epr_ex_t0, emergency_powers_ratchet, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(epr_ex_t5, emergency_powers_ratchet, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(epr_ex_t10, emergency_powers_ratchet, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The powers are a mechanism for enforcing state directives.
narrative_ontology:coordination_type(emergency_powers_ratchet, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */