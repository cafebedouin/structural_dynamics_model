% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the historical process by which the concept of
 *   'honor' itself was redefined to exclude violence, particularly dueling.
 *   This 'contraction reading' posits that dueling became structurally
 *   unthinkable not primarily due to external costs or legal prohibitions,
 *   but because the very definition of what constituted honorable behavior
 *   shifted. It is claimed as a Mountain because the redefinition of a
 *   fundamental social concept, once established, becomes an unchangeable
 *   feature of the social landscape, persisting regardless of individual
 *   enforcement. The metrics reflect a declining need for active suppression
 *   as the conceptual shift solidified.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.15).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.05).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, 'dba9d112-8c43-4685-8333-a363a0eb4bdf').
narrative_ontology:cs_kernel_codification('dba9d112-8c43-4685-8333-a363a0eb4bdf', implicit).
narrative_ontology:cs_authority_grounding('dba9d112-8c43-4685-8333-a363a0eb4bdf', practice).
narrative_ontology:cs_interpretation_layer_present('dba9d112-8c43-4685-8333-a363a0eb4bdf').
narrative_ontology:cs_reading_relation('dba9d112-8c43-4685-8333-a363a0eb4bdf', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('dba9d112-8c43-4685-8333-a363a0eb4bdf', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('dba9d112-8c43-4685-8333-a363a0eb4bdf', foundational, honor_excludes_private_violence).
narrative_ontology:cs_axiom_status(honor_excludes_private_violence, holdable).
narrative_ontology:cs_axiom_grounding('dba9d112-8c43-4685-8333-a363a0eb4bdf', honor_excludes_private_violence, conventional).
narrative_ontology:cs_axiom('dba9d112-8c43-4685-8333-a363a0eb4bdf', secondary, state_monopoly_on_violence_is_honorable).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_honorable, holdable).
narrative_ontology:cs_axiom_grounding('dba9d112-8c43-4685-8333-a363a0eb4bdf', state_monopoly_on_violence_is_honorable, conventional).
narrative_ontology:cs_reference_frame('dba9d112-8c43-4685-8333-a363a0eb4bdf', post_enlightenment_civility).
narrative_ontology:cs_drift_state('dba9d112-8c43-4685-8333-a363a0eb4bdf', contemporary_social_norms, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dba9d112-8c43-4685-8333-a363a0eb4bdf', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, civil_society).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, state_legal_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, aristocratic_elites).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, non_violence_as_honor).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__contraction_reading, state_monopoly_on_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reduction of private violence and the redefinition of honor towards civic virtues. Actively promotes and reinforces the new understanding of honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, civil_society, beneficiary,
    organized, generational, analytical, national).

% Benefits from the increased legitimacy of its monopoly on violence and the reduced need to prosecute dueling. Its legal framework is reinforced by the cultural shift.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, state_legal_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Historically bound by codes of honor that included dueling, they now face social opprobrium and legal consequences for engaging in violence. Their identity is challenged by the redefinition of honor, making exit from the new norm difficult without losing social standing.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, aristocratic_elites, payer,
    powerful, biographical, identity_locked, regional).

% Analyze the historical evolution of honor codes and the mechanisms by which dueling became culturally illegitimate. They observe the structural shift in the conceptual space of honor.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, honor_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social expectations around honor, shifting the definition away from violent confrontation towards non-violent means of resolving disputes and upholding reputation.
% TRANSFER_FUNCTION: Transfers the social cost of private violence (deaths, injuries, feuds) from individuals and society to a more stable, non-violent social order. It also transfers the 'right' to adjudicate honor from individuals to collective social norms and state institutions.
% ABSENT_VOICES: Those who clung to the older, more violent conception of honor were gradually marginalized and silenced by the evolving social consensus and legal enforcement. Their voices are now largely historical echoes, not active participants in the contemporary definition of honor.
% DISAPPEARANCE_RATIONALE: If this redefinition of honor vanished overnight, it would mean a return to a conceptual space where dueling is a legitimate response. However, the social and legal structures that replaced dueling are deeply entrenched; the world would not revert to widespread dueling, but the conceptual clarity around honor would be lost, leading to potential ambiguity in social expectations.
% FOUNDING_PROBLEM: The problem of private violence, particularly dueling, as a socially sanctioned means of resolving honor disputes, leading to instability and loss of life within elite circles.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal codes, and sociological analyses from various academic disciplines corroborate that dueling was a significant social problem that declined as honor was redefined. The problem is considered 'dead' because dueling is no longer a culturally legitimate practice in most societies.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_violence_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low and declining because the constraint primarily redefines a conceptual space rather than imposing direct costs; the 'cost' is the loss of a former option, which is a conceptual rather than material extraction. Suppression is very low because the constraint operates at the level of cultural definition and internalized norms, requiring minimal active enforcement once the redefinition takes hold. Accessibility collapse is high (0.9) because dueling, as a legitimate option, effectively vanishes from the conceptual landscape. Resistance is low (0.05) because the shift is a gradual cultural evolution, not a contested policy. Theater ratio is zero as there's no performative maintenance of a defunct practice; the shift is genuine.
 *
 * PERSPECTIVAL GAP:
 *   The primary 'gap' is between the historical perspective (where dueling was a legitimate practice) and the post-redefinition perspective (where it is unthinkable). This constraint models the latter, where the conceptual space has contracted. The engine's classification will reflect this 'mountain-like' stability of the redefined concept, even if the historical path to that redefinition involved contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil society and the state legal apparatus are beneficiaries, as they gain from a more peaceful and orderly social environment. Aristocratic elites, while initially 'payers' in the sense of losing a traditional means of defending honor, eventually become identity-locked into the new definition, making their 'cost' more about adaptation than ongoing extraction. Honor theorists are observers, analyzing the shift.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_redefinition,
    'Did the redefinition of honor primarily cause the decline of dueling, or was it a consequence of other factors (e.g., rising legal costs, social opprobrium)?',
    'Comparative historical analysis across different societies with varying legal and social pressures, examining the temporal sequencing of conceptual shifts versus external disincentives.',
    'If redefinition was primary, this ''contraction reading'' holds as a Mountain. If external costs were primary, the ''drop reading'' (a Snare or Tangled Rope) would be more accurate, and this constraint would be a secondary effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_redefinition, empirical, 'Determines the causal primacy of conceptual redefinition versus external pressures in the decline of dueling.').

omega_variable(
    conceptual_vs_material_extraction,
    'Is the ''loss'' of dueling as an honorable option a form of extraction, and if so, how should conceptual extraction be weighted against material extraction?',
    'Philosophical analysis of ''conceptual freedom'' and its relationship to well-being, combined with historical accounts of how individuals experienced the loss of dueling as an option.',
    'If conceptual extraction is weighted highly, the extractiveness of this constraint might be higher, potentially shifting it towards a Rope or even Tangled Rope, as it ''extracts'' a former freedom. If weighted low, its Mountain classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_vs_material_extraction, conceptual, 'Examines the nature and weight of conceptual extraction in the redefinition of honor.').

omega_variable(
    natural_vs_constructed_redefinition,
    'Is the redefinition of honor a ''natural'' evolution of social norms, or was it actively constructed and enforced by specific agents (e.g., moral entrepreneurs, state actors)?',
    'Detailed historical case studies identifying specific agents, their motivations, and the mechanisms they used to promote the new definition of honor.',
    'If actively constructed, the ''emerges_naturally'' claim for this Mountain would be challenged, potentially reclassifying it as a Tangled Rope or Snare, depending on the beneficiaries and victims of the construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_redefinition, empirical, 'Assesses whether the redefinition of honor was a natural social evolution or an actively constructed process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(hono_tr_t1750, honor_violence_legitimacy__contraction_reading, theater_ratio, 1750, 0.0).
narrative_ontology:measurement(hono_tr_t1800, honor_violence_legitimacy__contraction_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(hono_tr_t1850, honor_violence_legitimacy__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.25).
narrative_ontology:measurement(hono_be_t1750, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1750, 0.2).
narrative_ontology:measurement(hono_be_t1800, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hono_be_t1850, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.15).
narrative_ontology:measurement(hono_su_t1750, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1750, 0.1).
narrative_ontology:measurement(hono_su_t1800, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(hono_su_t1850, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1850, 0.06).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'honor_violence_legitimacy' kernel. This 'contraction reading' focuses on the redefinition of honor itself, distinct from the 'drop reading' (external costs) and the 'composite reading' (both factors).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
