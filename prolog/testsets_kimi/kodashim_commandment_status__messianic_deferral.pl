% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status â Messianic Deferral Reading
 *   domain: religious/historical
 *
 * SUMMARY:
 *   The kernel concerns the halakhic status of sacrificial commandments
 *   (kodashim) after the Temple's destruction. The messianic_deferral reading
 *   holds these commandments remain fully binding in principle but are
 *   temporarily suspended for performance; study of their tractates maintains
 *   communal readiness for messianic restoration. This reading competes with
 *   study_as_performance (study itself fulfills the commandment) and
 *   performance_only (the commandment is a dead letter without an altar).
 *   Structurally, the constraint coordinates across time to preserve
 *   expertise but extracts from present-generation resources and attention.
 *   The claim is tangled_rope: genuine coordination function layered with
 *   asymmetric extraction as deferral extends indefinitely.
 *
 * KEY AGENTS:
 *   - rabbinic_academy_system: agenda_setter (institutional/generational/constrained) â administers the deferral doctrine and curriculum
 *   - comprehensive_torah_students: dual-positioned payer/beneficiary (moderate/biographical/identity_locked) â bear opportunity cost while gaining status from comprehensive study
 *   - communal_funding_base: payer (organized/biographical/constrained) â funds the educational apparatus that maintains Kodashim study
 *   - present_generation_communal_needs: excluded victim (powerless/immediate/trapped) â underfunded due to resource diversion to messianic preparation
 *   - halakhic_analyst: observer (analytical/civilizational/analytical) â evaluates the structural account of the deferral doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.48).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.35).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.48).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status â Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/historical").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'bf1090f2-35bb-43a4-b9fb-29ad20add77d').
narrative_ontology:cs_kernel_codification('bf1090f2-35bb-43a4-b9fb-29ad20add77d', fixed_text).
narrative_ontology:cs_authority_grounding('bf1090f2-35bb-43a4-b9fb-29ad20add77d', lineage).
narrative_ontology:cs_interpretation_layer_present('bf1090f2-35bb-43a4-b9fb-29ad20add77d').
narrative_ontology:cs_reading_relation('bf1090f2-35bb-43a4-b9fb-29ad20add77d', kodashim_commandment_status__performance_only, influences).
narrative_ontology:cs_reading_relation('bf1090f2-35bb-43a4-b9fb-29ad20add77d', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('bf1090f2-35bb-43a4-b9fb-29ad20add77d', foundational, sacrificial_commandment_not_abrogated).
narrative_ontology:cs_axiom_status(sacrificial_commandment_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('bf1090f2-35bb-43a4-b9fb-29ad20add77d', sacrificial_commandment_not_abrogated, deontological).
narrative_ontology:cs_axiom('bf1090f2-35bb-43a4-b9fb-29ad20add77d', foundational, study_maintains_messianic_readiness).
narrative_ontology:cs_axiom_status(study_maintains_messianic_readiness, holdable).
narrative_ontology:cs_axiom_grounding('bf1090f2-35bb-43a4-b9fb-29ad20add77d', study_maintains_messianic_readiness, instrumental).
narrative_ontology:cs_reference_frame('bf1090f2-35bb-43a4-b9fb-29ad20add77d', commandment_normatively_active).
narrative_ontology:cs_drift_state('bf1090f2-35bb-43a4-b9fb-29ad20add77d', post_temple_two_millennia, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bf1090f2-35bb-43a4-b9fb-29ad20add77d', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_academy_system).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, comprehensive_torah_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_communal_needs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, comprehensive_torah_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, communal_funding_base).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, temple_restoration_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the halakhic curriculum and determines that tractate Kodashim remains a standard component of advanced study despite the Temple's destruction. Justifies this as maintaining competence for messianic restoration. Collects communal funding, student enrollment, and institutional prestige from preserving the full corpus.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_academy_system, agenda_setter,
    institutional, generational, constrained, global).

% Devote years of cognitive labor to mastering sacrificial procedures they will almost certainly never perform, under the justification that this maintains readiness for a future Temple. Their professional and social identity is fused with comprehensive Talmudic mastery; exiting the curriculum means identity rupture as much as educational choice.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, comprehensive_torah_students, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, comprehensive_torah_students, beneficiary).

% Donates to and subsidizes yeshivot where Kodashim study consumes significant hours and faculty resources. Funds are diverted from immediate communal welfare and practical halakhic education. Social and religious pressure constrains reallocation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, communal_funding_base, payer,
    organized, biographical, constrained, national).

% Comprises the poor, medically needy, and materially distressed whose immediate relief competes for the same communal resources devoted to messianic-preparation study. Not present in the halakhic curriculum-setting conversation and cannot voice the trade-off.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_communal_needs, excluded,
    powerless, immediate, trapped, local).

% Evaluates whether the deferral doctrine preserves genuine expertise or has drifted into institutional self-maintenance and generational extraction. Sits outside the religious obligation structure and assesses the kernel's contested readings comparatively.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, halakhic_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, rabbinic_academy_system).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves complex sacrificial jurisprudence across generational gaps so that priestly and judicial expertise would exist if the Temple were restored; maintains textual continuity and normative attachment to a central biblical practice during indefinite institutional interruption.
% TRANSFER_FUNCTION: Moves communal educational resources, student cognitive labor, and philanthropic funds from present-applicable Torah study and immediate welfare to the maintenance of obsolete procedural knowledge; transfers prestige and institutional legitimacy to academies that keep the full curriculum intact.
% ABSENT_VOICES: The materially distressed who would benefit from redirected communal funds; practically-minded halakhists who would prioritize current applicable law; and adherents of the performance_only reading who regard the commandment as a dead letter without an altarâall are structurally absent from the curriculum-setting conversation.
% DISAPPEARANCE_RATIONALE: If the messianic deferral doctrine vanished, yeshivot would reallocate Kodashim study hours to practical halakha or reduce overall study burden; communal funding would shift toward immediate welfare and current legal education; the rabbinic claim to comprehensive mastery would contract and the identity economy of Talmudic students would reconfigure.
% FOUNDING_PROBLEM: The destruction of the Second Temple created a crisis of continuity for sacrificial commandments; the rabbinic community needed a mechanism to preserve expertise, textual fluency, and normative attachment during an indefinite suspension of performance.
% FOUNDING_PROBLEM_CORROBORATION: The Talmudic sages attest the historical rupture of Temple destruction and the emergency need for study-as-remembrance. Contemporary academic historians and critical Talmud scholars corroborate the rupture but dispute whether current yeshiva curriculum proportions reflect genuine preservation needs or institutional self-maintenance; they attest from outside the beneficiary set that the founding crisis has evolved into organizational persistence.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint diverts substantial communal resources and cognitive labor to non-applicable study over a biographical horizon. Suppression is moderate-low (0.35): compliance is largely internalized through religious identity rather than external coercion, though institutional and social pressure maintain the curriculum. Theater ratio is moderate (0.40) and rising: the meticulous study of sacrifices increasingly functions as a performance of continuity rather than genuine preparation for an imminent restoration. Accessibility collapse is moderate (0.45): within the Orthodox framework alternatives are limited, but the existence of competing readings (siblings) prevents total collapse. Resistance is low-moderate (0.30): mostly latent, expressed through resource-allocation tensions rather than open rejection.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as sacred continuity and necessary preparation for redemption; the excluded victim seat would experience it as resource misallocation to an indefinitely deferred contingency. The engine should compute this divergence from the structural asymmetry in exit optionsâthe academy is constrained by tradition but institutionally empowered, while the needy are trapped by poverty and exclusion from the halakhic conversation.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic academy system sits near the beneficiary end: it gains institutional prestige, enrollment, and generational continuity from maintaining the full curriculum. Torah students sit in a dual position: they pay opportunity cost but benefit from identity and status as comprehensive scholars; their identity_locked exit pulls them toward the beneficiary side in practice, though structurally they are also targets. The communal funding base and present-generation communal needs are on the target side: they bear the cost of resource diversion without receiving the coordination benefit directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâTemple destruction and the threatened loss of sacrificial expertiseâwas genuine. However, after two millennia, the proportion of study and resources devoted to Kodashim no longer tracks the actual probability of restoration. The mandate has partially atrophied but persists because the rabbinic academy system's authority and identity are fused with comprehensive study. This is not yet a piton because the coordination function (preserving textual competence) remains real, but the rising theater ratio signals drift toward performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Which reading of the kodashim_commandment_status kernelâmessianic deferral, study as performance, or performance onlyâcorrectly captures the normative structure?',
    'Comparative halakhic history and sociology of the yeshiva curriculum; tracking which reading dominates under different political conditions such as Zionist state-building versus diaspora.',
    'If study_as_performance is true, extraction drops because study is the fulfillment; if performance_only is true, extraction drops because the commandment is a dead letter; this reading''s moderate extraction depends on its specific intermediate status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Indeterminacy among three contested readings of the same kernel').

omega_variable(
    messianic_horizon_indefiniteness,
    'Does the messianic deferral horizon have an upper bound, or does indefinite suspension functionally converge on abrogation?',
    'Observational tracking of restoration or non-arrival; sociological analysis of whether communities eventually abandon deferral doctrines after sufficient temporal distance.',
    'If indefinite deferral is structurally equivalent to abrogation, the constraint shifts from tangled_rope toward piton (theater dominates); if restoration is a genuine future contingency, the coordination function retains validity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_horizon_indefiniteness, empirical, 'Whether infinite deferral equals abrogation in effect').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression structural (institutional curriculum requirements, communal funding pressure) or internalized (religious identity fusion making exit unthinkable)?',
    'Post-exit trajectory analysis: if students who leave the yeshiva system still feel compelled to justify their lack of Kodashim study, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint functions more as identity_coordination with higher hidden extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.25).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__messianic_deferral, theater_ratio, 40, 0.32).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__messianic_deferral, theater_ratio, 60, 0.36).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__messianic_deferral, theater_ratio, 80, 0.38).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__messianic_deferral, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__messianic_deferral, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__messianic_deferral, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__messianic_deferral, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kodashim_commandment_status kernel, which decomposes into three structurally distinct constraints: messianic_deferral (this file), study_as_performance, and performance_only. The epsilon values differ because the beneficiary/victim structures and the functional role of study differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
