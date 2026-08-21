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
 *   This constraint represents a specific halakhic (Jewish legal) reading
 *   that posits the diligent study of the laws pertaining to Temple
 *   sacrifices as a legitimate and spiritually equivalent fulfillment of the
 *   obligation to perform those sacrifices, in the absence of the Temple.
 *   This reading emerged after the destruction of the Second Temple and
 *   became a foundational principle for post-Temple Jewish life. It is a
 *   'rope' because it provides a coordination mechanism for religious
 *   observance, with minimal extraction, as it offers a path to fulfilling a
 *   divine command that would otherwise be impossible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Occupation of Obligation in Temple's Absence").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious_studies/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'd64c4677-ac4a-4961-99ac-982ff84235ac').
narrative_ontology:cs_kernel_codification('d64c4677-ac4a-4961-99ac-982ff84235ac', fixed_text).
narrative_ontology:cs_authority_grounding('d64c4677-ac4a-4961-99ac-982ff84235ac', lineage).
narrative_ontology:cs_interpretation_layer_present('d64c4677-ac4a-4961-99ac-982ff84235ac').
narrative_ontology:cs_reading_relation('d64c4677-ac4a-4961-99ac-982ff84235ac', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('d64c4677-ac4a-4961-99ac-982ff84235ac', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('d64c4677-ac4a-4961-99ac-982ff84235ac', foundational, study_is_equivalent_to_action).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('d64c4677-ac4a-4961-99ac-982ff84235ac', study_is_equivalent_to_action, theological).
narrative_ontology:cs_axiom('d64c4677-ac4a-4961-99ac-982ff84235ac', foundational, divine_command_must_be_fulfillable).
narrative_ontology:cs_axiom_status(divine_command_must_be_fulfillable, holdable).
narrative_ontology:cs_axiom_grounding('d64c4677-ac4a-4961-99ac-982ff84235ac', divine_command_must_be_fulfillable, deontological).
narrative_ontology:cs_reference_frame('d64c4677-ac4a-4961-99ac-982ff84235ac', post_temple_rabbinic_consensus).
narrative_ontology:cs_drift_state('d64c4677-ac4a-4961-99ac-982ff84235ac', contemporary_halakhic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d64c4677-ac4a-4961-99ac-982ff84235ac', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jews).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_as_equivalent_to_performance).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, continuity_of_halakhic_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakha, including the principle that study of sacrifice law is equivalent to performing the sacrifices. They are the primary beneficiaries of this reading, as it legitimizes their intellectual pursuit as a fulfillment of divine command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Fulfill their religious obligation through study, providing a tangible means of connection to the divine command in the absence of the Temple. This reading offers a path to spiritual continuity and avoids the burden of an unfulfillable command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jews, beneficiary,
    organized, biographical, identity_locked, global).

% Advocate for the rebuilding of the Temple and literal restoration of sacrifices, viewing study as a temporary measure rather than a full occupation of the obligation. They are excluded from the mainstream halakhic discourse that elevates study to this status.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_activists, excluded,
    moderate, generational, constrained, regional).

% Analyze the historical development of halakhic rulings and the sociological function of such interpretations in maintaining religious continuity. They observe the constraint's operation without being bound by its religious claims.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and accessible means for observant Jews to fulfill the divine commandment regarding sacrifices, even in the absence of the Temple, ensuring continuity of religious practice and identity.
% TRANSFER_FUNCTION: Transfers the spiritual merit and fulfillment of the sacrificial obligation from the physical act of sacrifice to the intellectual and devotional act of study, from the individual to the divine.
% ABSENT_VOICES: Those who believe the obligation is strictly suspended until the Temple's rebuilding, or that study is merely archiving, would object. Their voices are marginalized by the dominant halakhic consensus that elevates study to a form of performance.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, observant Jews would face a profound crisis of religious practice, as a central divine command would become unfulfillable, leading to widespread spiritual distress and a re-evaluation of core theological tenets. The entire structure of post-Temple Judaism would be destabilized.
% FOUNDING_PROBLEM: The destruction of the Second Temple rendered the central act of Jewish worship—sacrifices—impossible, creating a profound crisis of religious observance and continuity.
% FOUNDING_PROBLEM_CORROBORATION: The problem of Temple absence remains live for all observant Jews. The solution (study as occupation) is attested by centuries of rabbinic tradition and is foundational to contemporary Orthodox Judaism, corroborated by its widespread acceptance and integration into daily practice.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low because this reading provides a solution to an otherwise impossible religious dilemma, offering spiritual benefit without imposing significant material cost or coercion. Suppression is low, as adherence is voluntary and driven by internal religious commitment rather than external enforcement. Theater ratio is negligible, as the study is considered a genuine, functional fulfillment of the obligation, not a mere performance. Accessibility collapse is high because, for adherents of this reading, there are no other viable alternatives to fulfilling the sacrifice obligation in the Temple's absence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of observant Jews, this is a pure rope, providing a vital spiritual lifeline. From the perspective of messianic activists, it might be seen as a form of 'identity_locked' constraint that defers the true (literal) fulfillment of the obligation, though this reading itself does not impose that deferral as a cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are agenda-setters, as they interpret and transmit this principle. Observant Jews are beneficiaries, as they gain a means to fulfill a core religious obligation. There are no direct 'victims' in this reading, as the constraint offers a solution rather than imposing a burden. Messianic activists are 'excluded' as their alternative reading is not incorporated into this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equivalence_of_study_and_performance,
    'Is the spiritual equivalence of study to performance a theological truth or a rabbinic innovation to cope with historical circumstances?',
    'Theological debate and textual analysis within the halakhic tradition; historical-critical scholarship examining the emergence of the doctrine.',
    'If primarily an innovation, the ''rope'' classification holds, but the underlying ''mountain'' of divine command might be seen as having been ''reinterpreted'' rather than directly fulfilled, potentially increasing the conceptual extractiveness for those who prioritize literal performance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equivalence_of_study_and_performance, conceptual, 'Theological vs. pragmatic grounding of study''s equivalence.').

omega_variable(
    relationship_to_messianic_era,
    'Does this reading implicitly foreclose or merely coexist with the messianic expectation of literal sacrifice restoration?',
    'Analysis of halakhic texts regarding the messianic era and the role of sacrifices; observation of contemporary practice and discourse among adherents.',
    'If it implicitly forecloses, it might be seen as a more ''tangled rope'' for those holding messianic expectations. If it merely coexists, its ''rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relationship_to_messianic_era, conceptual, 'How this reading interacts with messianic expectations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.02).
narrative_ontology:measurement(temp_tr_t650, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 650, 0.02).
narrative_ontology:measurement(temp_tr_t1300, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1300, 0.02).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1950, 0.02).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(temp_be_t650, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 650, 0.15).
narrative_ontology:measurement(temp_be_t1300, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1300, 0.15).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1950, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t650, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 650, 0.05).
narrative_ontology:measurement(temp_su_t1300, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1300, 0.05).
narrative_ontology:measurement(temp_su_t1950, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1950, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'temple_sacrifice_obligation' kernel. This reading (study_as_occupation) provides a means of fulfilling the obligation, while 'study_as_archiving' views study as preservation, and 'messianic_suspension' views the obligation as suspended.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
