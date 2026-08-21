% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Occupation of Obligation in Temple's Absence
 *   domain: religious_studies/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the halakhic reading that the study of
 *   sacrifice laws constitutes a legitimate fulfillment ('occupation') of the
 *   divine obligation in the absence of the Temple. This reading provides a
 *   crucial mechanism for religious continuity and identity for observant
 *   Jews. It is one reading of the 'temple_sacrifice_obligation' kernel,
 *   which also includes 'messianic_suspension' and 'study_as_archiving' as
 *   sibling readings. This reading is characterized by low extractiveness, as
 *   it offers a path to fulfillment rather than imposing a burden without
 *   recourse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.1).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Occupation of Obligation in Temple's Absence").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious_studies/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'c645adac-678d-41b2-9865-a9f93470c999').
narrative_ontology:cs_kernel_codification('c645adac-678d-41b2-9865-a9f93470c999', fixed_text).
narrative_ontology:cs_authority_grounding('c645adac-678d-41b2-9865-a9f93470c999', lineage).
narrative_ontology:cs_interpretation_layer_present('c645adac-678d-41b2-9865-a9f93470c999').
narrative_ontology:cs_reading_relation('c645adac-678d-41b2-9865-a9f93470c999', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('c645adac-678d-41b2-9865-a9f93470c999', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('c645adac-678d-41b2-9865-a9f93470c999', foundational, torah_study_as_equivalent_to_performance).
narrative_ontology:cs_axiom_status(torah_study_as_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('c645adac-678d-41b2-9865-a9f93470c999', torah_study_as_equivalent_to_performance, theological).
narrative_ontology:cs_reference_frame('c645adac-678d-41b2-9865-a9f93470c999', post_temple_destruction_halakhic_adaptation).
narrative_ontology:cs_drift_state('c645adac-678d-41b2-9865-a9f93470c999', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c645adac-678d-41b2-9865-a9f93470c999', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jews).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, halakhic_continuity_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_as_worship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakha, including the laws of sacrifice. They define the parameters of 'study as occupation' and guide the community in fulfilling this obligation. Their professional and religious identity is deeply intertwined with this interpretive tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Find a path to fulfill the divine commandment regarding sacrifices even in the absence of the Temple. Study provides spiritual meaning and continuity, integrating them into the ongoing halakhic tradition. Their religious identity is bound to this practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jews, beneficiary,
    moderate, biographical, identity_locked, global).

% Advocate for the immediate rebuilding of the Temple and the resumption of physical sacrifices, viewing study as an insufficient substitute. Their perspective is marginalized by the dominant halakhic consensus that prioritizes study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_activists, excluded,
    powerless, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and universally accepted method for observant Jews to engage with the divine commandment of sacrifices, maintaining halakhic continuity and communal identity in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers the spiritual and communal obligation of sacrifice from physical performance to intellectual and devotional study, from the individual to the collective scholarly tradition.
% ABSENT_VOICES: Those who believe the obligation is entirely suspended or that study is merely archiving, not fulfilling, the commandment are largely excluded from the mainstream halakhic discourse that defines legitimate practice. Messianic activists who prioritize physical restoration are also marginalized.
% DISAPPEARANCE_RATIONALE: If the understanding of 'study as occupation' vanished, observant Jews would face a profound crisis of religious practice and identity, lacking a clear path to fulfill a central divine commandment. The entire structure of post-Temple Judaism would be destabilized, requiring a fundamental re-evaluation of halakhic authority and continuity.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central divine commandment of animal sacrifices impossible to perform, creating a crisis of religious obligation and continuity for the Jewish people.
% FOUNDING_PROBLEM_CORROBORATION: The problem of performing sacrifices in the absence of the Temple remains live and is universally acknowledged across all branches of Judaism, including those who disagree on the solution. Historical texts, rabbinic responsa, and contemporary theological discussions attest to its enduring significance.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because this reading provides a solution to an otherwise impossible religious obligation, making it a net benefit for observant Jews. Suppression is minimal (0.05) as adherence is voluntary and driven by religious commitment, not coercion. Theater ratio is zero because the activity (study) is genuinely believed to fulfill the obligation, not merely to perform a ritual without function. Accessibility collapse is high (0.8) because, within this framework, there are few other accepted ways to fulfill the sacrifice obligation in the Temple's absence. Resistance is low (0.05) because this reading is widely accepted within mainstream halakhic Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic scholars and observant Jews, this constraint is a 'rope' that provides a vital path for religious observance and continuity. From the perspective of messianic activists, it might be seen as a 'snare' that diverts attention and energy from the imperative of rebuilding the Temple, but this is a minority view not reflected in the core structure of the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are agenda-setters, defining and guiding the practice. Observant Jews are beneficiaries, gaining a means of fulfilling a core religious duty. Both are identity-locked, as their religious identity is deeply intertwined with this interpretive tradition. Messianic activists are excluded, as their alternative approach is not recognized as legitimate within this dominant framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively prevents mandatrophy by adapting an ancient obligation to a changed reality. The mandate (sacrifice) is transformed into a new, performable function (study), ensuring its continued relevance and preventing its atrophy. The classification as a 'rope' reflects its genuine coordination function in solving a profound religious problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_substitution,
    'Does study genuinely constitute a full ''occupation'' of the sacrifice obligation, or is it a lesser, temporary substitute?',
    'Theological and halakhic debate, potentially resolved by a future authoritative rabbinic consensus or the rebuilding of the Temple itself.',
    'If deemed a lesser substitute, the extractiveness might be perceived as higher by some adherents (a ''snare'' of incomplete fulfillment), and the constraint might shift towards a ''tangled_rope'' or ''scaffold'' if its temporary nature becomes more emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_substitution, conceptual, 'The degree to which study fully replaces physical sacrifice.').

omega_variable(
    messianic_tension,
    'How does the ''study as occupation'' reading interact with the messianic expectation of Temple rebuilding and renewed sacrifices?',
    'Ongoing theological discourse and the unfolding of historical events related to the Temple Mount and messianic movements.',
    'If messianic expectations intensify and gain broader halakhic endorsement, this reading could be challenged, potentially leading to a reclassification as a ''scaffold'' (temporary until the Temple is rebuilt) or even a ''piton'' if its function is seen as having atrophied in favor of direct action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_tension, preference, 'Tension between current practice and future messianic fulfillment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.0).
narrative_ontology:measurement(temp_tr_t650, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 650, 0.0).
narrative_ontology:measurement(temp_tr_t1300, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1300, 0.0).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1950, 0.0).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(temp_be_t650, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 650, 0.1).
narrative_ontology:measurement(temp_be_t1300, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1300, 0.1).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1950, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t650, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 650, 0.05).
narrative_ontology:measurement(temp_su_t1300, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1300, 0.05).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'temple_sacrifice_obligation' kernel. This reading ('study_as_occupation') asserts that study fulfills the obligation, distinguishing it from 'messianic_suspension' (obligation suspended) and 'study_as_archiving' (study preserves knowledge but doesn't fulfill). Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
