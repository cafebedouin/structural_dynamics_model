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
 *   that holds it to be suspended, neither fulfilled nor violated, pending
 *   messianic restoration. This reading emphasizes the deferral of the
 *   practical obligation while maintaining the theoretical knowledge. It is a
 *   'mountain' in the sense that the physical impossibility of sacrifice is
 *   an unchangeable fact, and the halakhic interpretation of suspension is
 *   widely accepted within its tradition, imposing minimal extraction or
 *   suppression.
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
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, 'c0ad26ef-6172-45b4-979c-4e6f5d48123e').
narrative_ontology:cs_kernel_codification('c0ad26ef-6172-45b4-979c-4e6f5d48123e', fixed_text).
narrative_ontology:cs_authority_grounding('c0ad26ef-6172-45b4-979c-4e6f5d48123e', lineage).
narrative_ontology:cs_interpretation_layer_present('c0ad26ef-6172-45b4-979c-4e6f5d48123e').
narrative_ontology:cs_reading_relation('c0ad26ef-6172-45b4-979c-4e6f5d48123e', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('c0ad26ef-6172-45b4-979c-4e6f5d48123e', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('c0ad26ef-6172-45b4-979c-4e6f5d48123e', foundational, physical_impossibility_suspends_obligation).
narrative_ontology:cs_axiom_status(physical_impossibility_suspends_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c0ad26ef-6172-45b4-979c-4e6f5d48123e', physical_impossibility_suspends_obligation, deontological).
narrative_ontology:cs_axiom('c0ad26ef-6172-45b4-979c-4e6f5d48123e', foundational, study_is_knowledge_maintenance_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_knowledge_maintenance_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('c0ad26ef-6172-45b4-979c-4e6f5d48123e', study_is_knowledge_maintenance_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('c0ad26ef-6172-45b4-979c-4e6f5d48123e', halakhic_continuity_in_absence).
narrative_ontology:cs_drift_state('c0ad26ef-6172-45b4-979c-4e6f5d48123e', contemporary_diaspora_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c0ad26ef-6172-45b4-979c-4e6f5d48123e', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, observant_jews).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, messianic_era_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the legal tradition, affirming that the obligation for Temple sacrifices is suspended, not abrogated, pending messianic restoration. They maintain the knowledge of the laws without requiring current observance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Are released from the practical burden of performing sacrifices that are currently impossible. They are obligated to study the laws, but this study is not a substitute for the actual sacrifice, nor does it fulfill the obligation itself. They benefit from clarity on their current religious duties.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_jews, beneficiary,
    moderate, biographical, identity_locked, global).

% The future community that will resume sacrifices upon messianic restoration. They benefit from the preservation of the detailed laws and the clear understanding that the obligation is merely suspended, ensuring continuity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_era_community, beneficiary,
    analytical, civilizational, analytical, universal).

% The historical institution that performed sacrifices. Their role is currently inert, but their practices and laws are preserved for future reference. They serve as a conceptual referent for the suspended obligation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, historical_temple_priesthood, observer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__messianic_suspension, historical_temple_priesthood).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious practice of observant Jews by providing a clear halakhic status for the Temple sacrifice obligation in the absence of the Temple, preventing confusion or attempts at unauthorized substitutes.
% TRANSFER_FUNCTION: Transfers the practical burden of sacrifice from the present generation to a future messianic era, while transferring the responsibility for knowledge preservation to contemporary scholars and students.
% ABSENT_VOICES: Those who might advocate for symbolic or alternative forms of sacrifice as a current fulfillment of the obligation are implicitly excluded by this reading, which insists on literal suspension. Their voices are present in other readings of the kernel.
% DISAPPEARANCE_RATIONALE: If this understanding of suspension vanished, the physical world would remain unchanged, as the Temple is still absent. However, the halakhic framework for observant Jews would be thrown into disarray, forcing a re-evaluation of current religious duties regarding sacrifice.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of Jewish worship (sacrifices) impossible, creating a profound crisis regarding the continuity of religious obligation.
% FOUNDING_PROBLEM_CORROBORATION: The problem of the Temple's absence and the inability to perform sacrifices remains a live theological and practical issue for observant Jews, attested by centuries of rabbinic literature and contemporary religious discourse outside the immediate halakhic authorities.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.05) because no current material or behavioral cost is imposed by this reading; rather, it relieves a burden. Suppression is negligible (0.02) as it requires no active enforcement beyond the general adherence to halakhic authority. Theater ratio is minimal (0.01) as the claim is straightforward and not performative. Accessibility collapse is high (0.95) because the physical absence of the Temple makes alternatives for literal sacrifice impossible. Resistance is low (0.01) as this reading is widely accepted within the tradition.
 *
 * PERSPECTIVAL GAP:
 *   There is little perspectival gap within this reading, as its core premise (suspension due to physical impossibility) is widely shared. Divergence arises when comparing this reading to others that propose alternative forms of 'fulfillment' in the interim.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are agenda-setters, defining the terms of suspension. Observant Jews are beneficiaries, released from an impossible obligation. The future messianic community is also a beneficiary, as the knowledge is preserved for them. No direct victims are identified by this reading, as it primarily defers an obligation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_suspension,
    'Is the suspension of the obligation a temporary abrogation, or a deferral of an active but currently impossible duty?',
    'Further halakhic analysis and consensus on the precise theological status of an ''impossible'' mitzvah (commandment).',
    'If temporary abrogation, the constraint''s extractiveness might be even lower (no latent duty). If active but impossible, it reinforces the ''mountain'' aspect of the physical constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nature_of_suspension, conceptual, 'Clarifying the precise halakhic nature of the suspended obligation.').

omega_variable(
    relationship_to_study,
    'What is the precise relationship between the suspended obligation and the study of its laws? Is study merely preparatory, or does it carry a distinct, albeit secondary, form of religious merit?',
    'Analysis of rabbinic texts on the merit of Torah study concerning currently non-applicable laws.',
    'If study is seen as a form of ''occupation'' of the obligation (as in the ''study_as_occupation'' reading), this constraint would have a higher, albeit still low, extractiveness due to the burden of study. This reading holds study as maintenance, not fulfillment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relationship_to_study, conceptual, 'Distinguishing study as knowledge maintenance from study as a form of fulfillment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.01).
narrative_ontology:measurement(temp_tr_t25, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 25, 0.01).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 50, 0.01).
narrative_ontology:measurement(temp_tr_t75, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 75, 0.01).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t25, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(temp_be_t75, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(temp_su_t25, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 50, 0.02).
narrative_ontology:measurement(temp_su_t75, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 75, 0.02).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
