% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious_studies/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents the reading of the Temple sacrifice obligation
 *   as suspended, neither fulfilled nor violated, pending messianic
 *   restoration. It is a foundational theological position within Judaism
 *   that addresses the practical impossibility of performing sacrifices since
 *   the destruction of the Second Temple. This reading maintains the
 *   integrity of the commandment without imposing an impossible burden on
 *   adherents. It is one of several readings of the broader
 *   'temple_sacrifice_obligation' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.02).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, mountain).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious_studies/halakhic_authority").

domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, 'dc715e9a-a5f6-4451-91dc-7e528a8e0444').
narrative_ontology:cs_kernel_codification('dc715e9a-a5f6-4451-91dc-7e528a8e0444', formalized).
narrative_ontology:cs_authority_grounding('dc715e9a-a5f6-4451-91dc-7e528a8e0444', lineage).
narrative_ontology:cs_interpretation_layer_present('dc715e9a-a5f6-4451-91dc-7e528a8e0444').
narrative_ontology:cs_reading_relation('dc715e9a-a5f6-4451-91dc-7e528a8e0444', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('dc715e9a-a5f6-4451-91dc-7e528a8e0444', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('dc715e9a-a5f6-4451-91dc-7e528a8e0444', foundational, obligation_is_temporally_contingent).
narrative_ontology:cs_axiom_status(obligation_is_temporally_contingent, holdable).
narrative_ontology:cs_axiom_grounding('dc715e9a-a5f6-4451-91dc-7e528a8e0444', obligation_is_temporally_contingent, deontological).
narrative_ontology:cs_axiom('dc715e9a-a5f6-4451-91dc-7e528a8e0444', foundational, current_non_performance_is_not_violation).
narrative_ontology:cs_axiom_status(current_non_performance_is_not_violation, holdable).
narrative_ontology:cs_axiom_grounding('dc715e9a-a5f6-4451-91dc-7e528a8e0444', current_non_performance_is_not_violation, deontological).
narrative_ontology:cs_reference_frame('dc715e9a-a5f6-4451-91dc-7e528a8e0444', pre_destruction_halakha).
narrative_ontology:cs_drift_state('dc715e9a-a5f6-4451-91dc-7e528a8e0444', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('dc715e9a-a5f6-4451-91dc-7e528a8e0444', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit Jewish law. This reading defers the obligation to a future messianic era, maintaining the theoretical validity of the commandment without requiring current observance. Their authority is maintained by the continuity of tradition, not by enforcing a currently impossible obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Adhere to Jewish law. Under this reading, they are not currently obligated to perform sacrifices, nor are they in violation for not doing so. Their focus is on other mitzvot (commandments) and maintaining the knowledge of sacrifice laws for future restoration. They experience no extraction from this specific constraint.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_jews, observer,
    moderate, biographical, identity_locked, global).

% The future community that will, according to tradition, resume temple sacrifices. This reading preserves the integrity of the obligation for them, ensuring its theoretical framework remains intact and unviolated by the current impossibility of performance. This is a conceptual beneficiary, not an active agent.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_era_community, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__messianic_suspension, messianic_era_community).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding of a central religious obligation across generations, ensuring its theoretical integrity and future applicability despite current practical impossibility, preventing despair or reinterpretation that would nullify the commandment.
% TRANSFER_FUNCTION: No direct transfer of material resources. It transfers the 'burden' of an impossible obligation from current adherents to a future messianic era, preserving the spiritual and legal integrity of the commandment.
% ABSENT_VOICES: No voices are actively suppressed by this reading, as it defers the obligation. Those who might argue for a more active, symbolic 'fulfillment' of the obligation in the present are accommodated by other readings (e.g., study as occupation) rather than excluded.
% DISAPPEARANCE_RATIONALE: If this understanding of suspension vanished, it would create a profound theological crisis. Observant Jews would either be in perpetual violation of a core commandment or forced to radically reinterpret its nature, fundamentally altering the structure of Jewish religious practice and belief regarding the Temple.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central commandment of animal sacrifice impossible to fulfill, creating a crisis of religious obligation and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem of the Temple's destruction and the inability to perform sacrifices remains a live theological and practical issue for observant Jews, attested by centuries of rabbinic literature and daily prayers for restoration, from outside the immediate beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_obligation__messianic_suspension),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low because no current action is required, and no party is actively paying or being coerced by this specific interpretation. Suppression is minimal as it's a widely accepted theological stance, not one requiring active enforcement against dissent. Theater ratio is negligible as the constraint's function is purely conceptual and deferential. Accessibility collapse is high because the physical impossibility of performing sacrifices is a 'natural' barrier, making alternatives to suspension non-existent in the present. Resistance is low because this reading alleviates an impossible burden.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap for this specific reading, as its core function is to resolve a theological dilemma by deferring the obligation. All parties who accept this reading experience it similarly as a relief from an impossible burden. Divergence arises when comparing this reading to others that propose alternative forms of 'fulfillment' in the present.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are agenda-setters, maintaining the framework. Observant Jews are observers, adhering to the framework without direct extraction. The 'messianic_era_community' is a conceptual beneficiary, as the obligation's integrity is preserved for their future. No direct victims exist under this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_theological_construct,
    'Is the suspension of the obligation a ''natural law'' (an inevitable consequence of the Temple''s destruction) or a ''theological construct'' (a chosen interpretation by authorities)?',
    'Analysis of early rabbinic debates: if alternative interpretations were viable and actively suppressed, it''s a construct; if suspension was the only logical conclusion, it''s closer to natural law.',
    'If a construct, the ''emerges_naturally'' claim is weaker, and the authority''s role in shaping the constraint becomes more prominent, potentially shifting its classification towards a ''rope'' or ''tangled_rope'' if active enforcement of this reading is found.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_theological_construct, conceptual, 'Ambiguity between natural consequence and theological interpretation.').

omega_variable(
    relationship_to_study_as_occupation,
    'Does this reading implicitly suppress or de-emphasize the ''study_as_occupation'' reading, or do they genuinely coexist as equally valid approaches?',
    'Empirical study of educational curricula and communal messaging: if ''study_as_occupation'' is systematically marginalized, then this reading exerts subtle suppressive force.',
    'If suppression is detected, the ''suppression'' metric would increase, and the constraint might shift towards a ''tangled_rope'' if the ''messianic_suspension'' reading is actively promoted to the detriment of alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relationship_to_study_as_occupation, empirical, 'Potential for subtle suppression of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.01).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.01).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.01).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.01).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.01).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.02).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.02).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'temple_sacrifice_obligation' kernel. It defines the obligation as suspended, influencing how other readings (study as occupation, study as archiving) are understood in relation to actual fulfillment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
