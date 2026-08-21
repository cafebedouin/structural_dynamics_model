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
 *   human_readable: Halakhic Obligation: Study as Vicarious Temple Service
 *   domain: religious/halakhic/commitment_systems
 *
 * SUMMARY:
 *   This constraint describes the halakhic (Jewish legal) interpretation that
 *   the study of Temple sacrifice laws constitutes a legitimate fulfillment
 *   of the divine obligation in the absence of the physical Temple. This
 *   reading emerged and solidified after the destruction of the Second
 *   Temple, providing a vital mechanism for religious continuity and identity
 *   for observant Jews. It is a core tenet of rabbinic Judaism, allowing
 *   adherents to engage with a central commandment that is otherwise
 *   impossible to perform.
 *
 * KEY AGENTS:
 *   - halakhic_scholars: Primary agenda_setter and beneficiary (institutional/arbitrage) — interpret, teach, and embody the fulfillment.
 *   - observant_jews: Primary beneficiary and payer (moderate/constrained) — fulfill the obligation through study, dedicating time and resources.
 *   - messianic_activists: Excluded (organized/constrained) — hold alternative readings that challenge this interpretation.
 *   - archival_scholars: Excluded (institutional/constrained) — hold alternative readings that challenge this interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.15).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.1).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Halakhic Obligation: Study as Vicarious Temple Service").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'c814de93-522c-4af4-91c1-c8d7e7ff71bd').
narrative_ontology:cs_kernel_codification('c814de93-522c-4af4-91c1-c8d7e7ff71bd', fixed_text).
narrative_ontology:cs_authority_grounding('c814de93-522c-4af4-91c1-c8d7e7ff71bd', lineage).
narrative_ontology:cs_interpretation_layer_present('c814de93-522c-4af4-91c1-c8d7e7ff71bd').
narrative_ontology:cs_reading_relation('c814de93-522c-4af4-91c1-c8d7e7ff71bd', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('c814de93-522c-4af4-91c1-c8d7e7ff71bd', temple_sacrifice_obligation__study_as_archiving, forecloses).
narrative_ontology:cs_axiom('c814de93-522c-4af4-91c1-c8d7e7ff71bd', foundational, torah_study_is_equivalent_to_action).
narrative_ontology:cs_axiom_status(torah_study_is_equivalent_to_action, holdable).
narrative_ontology:cs_axiom_grounding('c814de93-522c-4af4-91c1-c8d7e7ff71bd', torah_study_is_equivalent_to_action, theological).
narrative_ontology:cs_axiom('c814de93-522c-4af4-91c1-c8d7e7ff71bd', secondary, divine_will_accommodates_impossibility).
narrative_ontology:cs_axiom_status(divine_will_accommodates_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('c814de93-522c-4af4-91c1-c8d7e7ff71bd', divine_will_accommodates_impossibility, theological).
narrative_ontology:cs_reference_frame('c814de93-522c-4af4-91c1-c8d7e7ff71bd', halakhic_continuity_through_study).
narrative_ontology:cs_drift_state('c814de93-522c-4af4-91c1-c8d7e7ff71bd', contemporary_rabbinic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c814de93-522c-4af4-91c1-c8d7e7ff71bd', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_jews).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_as_ultimate_value).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, halakhic_adaptability).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, continuity_of_divine_commandment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The rabbinic authorities and scholars who interpret, transmit, and legitimize the doctrine that study fulfills the sacrifice obligation. Their authority and purpose are deeply intertwined with this interpretation, making exit from this framework an identity-altering event.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Individuals who adhere to Jewish law and seek to fulfill divine commandments. This interpretation provides them with a legitimate and accessible means to engage with the sacrifice obligation, which is central to their religious identity. Abandoning this interpretation would mean a fundamental shift in their religious practice and self-conception.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_jews, beneficiary,
    moderate, biographical, identity_locked, global).

% Groups or individuals who prioritize the immediate rebuilding of the Temple and believe the obligation is suspended until then. They are excluded from the interpretive framework of 'study as occupation' because their core premise of suspension directly contradicts the idea of fulfillment through study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_activists, excluded,
    organized, generational, constrained, regional).

% Scholars who view the study of sacrifice laws primarily as a means of preserving knowledge for a future restoration, without believing it fulfills the obligation in the present. Their position is foreclosed by the 'study as occupation' reading's claim of active fulfillment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, archival_scholars, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, legitimate, and accessible means for observant Jews to fulfill the divine obligation of Temple service in its physical absence, thereby maintaining continuity of religious practice, identity, and communal purpose across generations.
% TRANSFER_FUNCTION: Transfers the spiritual merit, fulfillment, and communal focus of the Temple sacrifice obligation from the physically impossible act to the intellectually and spiritually accessible act of study, from the individual to the collective body of scholars and students.
% ABSENT_VOICES: Those who believe the obligation is suspended until the Temple's rebuilding ('messianic_activists') or that study is merely preservation, not fulfillment ('archival_scholars'), would object. They are absent from the interpretive consensus of this reading because their core premises are incompatible with its claim of active fulfillment.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, observant Jews would face a profound crisis of religious obligation and identity, lacking a legitimate means to engage with a central commandment. This would lead to widespread theological and practical reorganization, potentially fragmenting religious communities and altering core aspects of Jewish practice.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the central divine commandment of animal sacrifice impossible to perform, creating a profound crisis of religious obligation, continuity, and identity for observant Jews.
% FOUNDING_PROBLEM_CORROBORATION: This reading is corroborated by centuries of rabbinic literature (e.g., Talmudic discussions, Maimonides' Mishneh Torah), legal codes, and the lived practice of millions of observant Jews. Independent historical and sociological analyses of Jewish life post-Temple destruction also attest to its functional role in maintaining religious continuity and identity.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The constraint is classified as a Rope because it solves a genuine collective-action problem (how to fulfill a divine commandment when its physical performance is impossible) with minimal coercive overhead. Participants are net beneficiaries, gaining spiritual fulfillment and maintaining religious identity. Extractiveness is low (0.15) because the 'cost' of study is largely self-imposed and yields direct spiritual benefit. Suppression is low (0.10) as adherence is voluntary and deeply internalized within the community, not externally enforced. Theater ratio is low (0.05) because the act of study is genuinely believed to fulfill the obligation, not merely to perform a ritualistic substitute. Accessibility collapse is high (0.90) because the physical Temple's absence makes the original form of the obligation impossible. Resistance is low (0.05) due to the widespread acceptance of this interpretation within mainstream rabbinic Judaism.
 *
 * PERSPECTIVAL GAP:
 *   While the 'study as occupation' reading is widely accepted, alternative readings (e.g., 'messianic_suspension' or 'study_as_archiving') exist. From the perspective of those holding this reading, it is a legitimate and fulfilling path. From the perspective of 'messianic_activists', this reading might be seen as delaying or diminishing the urgency of messianic restoration. From 'archival_scholars', it might be seen as overstating the efficacy of study. The engine computes these divergences from the structural relationships and declared axioms.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars are beneficiaries and agenda-setters, as they interpret, transmit, and embody this path of fulfillment, deriving authority and purpose from it. Observant Jews are also beneficiaries, as they gain a legitimate means of fulfilling a core religious obligation, though they 'pay' through their dedication to study. There are no direct 'victims' in this reading, as the obligation is considered fulfilled. Alternative readings are 'excluded' from the dominant discourse of this specific interpretation, as their premises are incompatible with its core claim of fulfillment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively prevents mandatrophy by adapting an ancient obligation to a new reality. The original mandate (Temple sacrifice) became impossible, but the 'study as occupation' interpretation provided a new, viable mandate (study) that maintains the core function of fulfilling the divine commandment. It avoids becoming a piton by genuinely fulfilling a live obligation, rather than merely performing an atrophied function. It avoids being a snare by providing a net benefit to its adherents without coercion or suppressed alternatives within its own framework of fulfillment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid reading of the ''temple_sacrifice_obligation'' kernel, or is it a distinct, independent constraint?',
    'Analysis of historical rabbinic responsa and theological debates regarding the nature of sacrifice and study in the absence of the Temple.',
    'If a distinct constraint, its classification would be evaluated independently, potentially altering its relationship to other religious obligations. As a reading, its legitimacy is tied to the kernel''s authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as the ''study_as_occupation'' reading of the ''temple_sacrifice_obligation'' kernel, distinguishing it from sibling readings like ''messianic_suspension'' and ''study_as_archiving''.').

omega_variable(
    fulfillment_vs_substitution_ambiguity,
    'Does study truly ''fulfill'' the obligation of Temple sacrifice, or does it merely serve as a temporary ''substitution'' until physical performance is possible?',
    'Theological and halakhic analysis of the precise meaning of ''occupation'' (עיסוק) in this context, and its equivalence to ''performance'' (עשייה).',
    'If merely a substitution, the extractiveness might be slightly higher (as the ''true'' obligation remains unfulfilled), and the ''rope'' classification might weaken towards a ''scaffold'' (temporary support). If full fulfillment, the low extractiveness and ''rope'' classification are robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_vs_substitution_ambiguity, conceptual, 'Clarifies the nature of study''s relationship to the original sacrifice obligation.').

omega_variable(
    impact_of_temple_rebuilding,
    'How would the rebuilding of the Temple and the resumption of physical sacrifices impact the validity and status of ''study as occupation'' as a means of fulfilling the obligation?',
    'Hypothetical halakhic ruling by a Sanhedrin (supreme rabbinic court) in a post-Temple era, or analysis of prophetic texts and eschatological traditions.',
    'If physical sacrifice becomes possible, this reading would likely be superseded or relegated to a secondary status, potentially becoming a ''piton'' (inertial practice) or ''scaffold'' (transitional support) for those unable to perform physical sacrifices. Its current ''rope'' function would cease.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_of_temple_rebuilding, empirical, 'Examines the counterfactual scenario of the Temple''s restoration and its effect on this interpretive constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.05).
narrative_ontology:measurement(temp_tr_t200, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 200, 0.05).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 400, 0.05).
narrative_ontology:measurement(temp_tr_t600, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 600, 0.05).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 800, 0.05).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1000, 0.05).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(temp_be_t200, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 200, 0.15).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 400, 0.15).
narrative_ontology:measurement(temp_be_t600, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 600, 0.15).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 800, 0.15).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(temp_su_t200, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 200, 0.1).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 400, 0.1).
narrative_ontology:measurement(temp_su_t600, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 600, 0.1).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 800, 0.1).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 1000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
