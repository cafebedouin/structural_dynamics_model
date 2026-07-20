% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Temple Sacrifice Commitment â Study as Exercise Reading
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   'temple_sacrifice_commitment': the claim that study of sacrificial law is
 *   itself performance of the divine command. The kernel arises from the
 *   fixed textual command to bring sacrifices, confronted with the material
 *   impossibility after the Second Temple's destruction. Four live readings
 *   contest the kernel: performance_only (material instantiation required),
 *   hybrid_preparatory (suspended maintenance), study_as_exercise (this
 *   reading: intellectual engagement as full occupation), and
 *   symbolic_transformation (authorized transformation into prayer/study).
 *   This reading claims zero extractiveness: no party is harmed, and the
 *   studying community benefits through maintained covenant fidelity.
 *
 * KEY AGENTS:
 *   - studying_community: Primary beneficiary (organized/identity_locked) â maintains covenant through study
 *   - halakhic_authority: Agenda setter (institutional/analytical) â transmits and enforces the interpretive principle
 *   - materialist_adherents: Excluded voice (moderate/constrained) â denies the reading's validity condition
 *   - scholarly_observer: Analytical observer â maps the commitment system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.02).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.1).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment â Study as Exercise Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, '42ef15cf-7852-4a1d-8ce1-7d4ec73c7827').
narrative_ontology:cs_kernel_codification('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', fixed_text).
narrative_ontology:cs_authority_grounding('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', lineage).
narrative_ontology:cs_interpretation_layer_present('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827').
narrative_ontology:cs_reading_relation('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', foundational, torah_study_fulfills_sacrifice_command).
narrative_ontology:cs_axiom_status(torah_study_fulfills_sacrifice_command, holdable).
narrative_ontology:cs_axiom_grounding('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', torah_study_fulfills_sacrifice_command, theological).
narrative_ontology:cs_axiom('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', foundational, absence_of_temple_does_not_suspend_covenant).
narrative_ontology:cs_axiom_status(absence_of_temple_does_not_suspend_covenant, holdable).
narrative_ontology:cs_axiom_grounding('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', absence_of_temple_does_not_suspend_covenant, theological).
narrative_ontology:cs_reference_frame('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', sacrifice_command_active_in_study).
narrative_ontology:cs_drift_state('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', post_temple_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('42ef15cf-7852-4a1d-8ce1-7d4ec73c7827', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engages in daily Torah study of sacrificial law (korbanot) as direct fulfillment of the divine command; in the absence of the Temple, intellectual engagement is understood to occupy the commitment and maintain covenant fidelity. Leaving this practice implies leaving the covenant identity that constitutes the community.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, generational, identity_locked, global).

% Rabbinic courts and recognized decisors (poskim) who teach, adjudicate, and transmit the principle that Torah study of sacrifice constitutes valid performance of the commandment; they maintain the interpretive framework and pedagogical institutions that sustain this reading across generations.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, halakhic_authority, agenda_setter,
    institutional, generational, analytical, global).

% Jews and Jewish movements who hold that divine command requires material altar, priesthood, and blood-offering; they regard study without sacrifice as archival memory, not active command-occupation. Their premise is structurally excluded from the study-as-exercise framework because it denies the reading's core validity condition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, materialist_adherents, excluded,
    moderate, biographical, constrained, global).

% External academic or theological observer mapping how the study-as-exercise reading stabilizes covenant commitment under conditions of material impossibility; neither collects from nor pays into the constraint.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, scholarly_observer, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective covenant fidelity and shared religious identity across a dispersed community in the absence of the central cultic site; coordinates practice around textual study when material conditions for sacrifice are impossible.
% TRANSFER_FUNCTION: Moves the locus of covenantal performance from the material register (altar, priest, blood) to the intellectual register (Talmudic analysis, mnemonic rehearsal, legal reasoning); no material transfer occurs, but religious attention and collective commitment are centralized in study institutions.
% ABSENT_VOICES: Material-performance advocates (those holding the performance_only reading) would object that study without altar is not command-fulfillment; they are structurally absent because the study-as-exercise framework treats their premise as invalidating the entire arrangement.
% DISAPPEARANCE_RATIONALE: If the principle that study equals performance vanished, the vast edifice of yeshiva learning devoted to sacrificial law would lose its performative status and likely contract into antiquarian interest; the community would need to reorganize covenant maintenance around mourning, prayer, or suspended messianic expectation.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the material infrastructure (altar, priesthood, ritual purity) required for biblical sacrifice; the community required a modality to continue actively occupying the sacrificial covenant rather than archiving it as defunct.
% FOUNDING_PROBLEM_CORROBORATION: The studying community attests the problem is live through continuous practice. The performance_only tradition corroborates the historical rupture (Temple destruction) from outside the beneficiary set, but explicitly denies that study resolves it, instead treating the commitment as suspended. No purely external (non-covenantal) corroborator exists.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the constraint coordinates genuine collective-action maintenance of identity without material transfer or coercion; suppression is low (0.10) because persistence depends on interpretive consensus and identity-internalization rather than active enforcement against exit; theater is minimal (0.08) because the study practice is functional for covenant maintenance within its own framework. Accessibility collapse is moderate (0.35): for non-committed observers, alternatives (prayer, messianic waiting) remain visible, but for identity-committed participants the study frame largely absorbs the ritual space. Resistance is negligible (0.08) because the constraint operates within a community that affirms it. The flat measurement series reflect centuries-long stability of this reading.
 *
 * PERSPECTIVAL GAP:
 *   The studying community and the halakhic authority compute as coordination beneficiaries. The excluded materialist seat, if forced into the framework, would compute as a high-target trapped agent because the constraint's premise contradicts their core commitment; however, they are not actual stakeholders in this constraint's operation. The engine's per-seat classification will diverge sharply between the beneficiary seats (rope/mountain-like) and the counterfactual excluded seat (snare-like), but the latter is not an occupied position.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community is the declared beneficiary (low d, near 0.0); study maintains their covenant identity and they experience the practice as fulfillment. The halakhic authority is agenda-setter with analytical exit; its directionality is low because it sustains rather than extracts from the community. No victim group is declared. The excluded materialist adherents would experience high directionality if seated inside this constraint, but they are structurally outside it â the constraint does not govern them.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy because the founding problem (Temple absence) remains live and the coordination function (covenant fidelity through study) is actively served. The constraint is not a piton because its theater ratio is low and its function is not atrophied; it is not a scaffold because it carries no sunset clause and is not framed as transitional. The R5 genealogy (founding problem live + disappearance rearranges) confirms active coordination rather than zombie persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intellectual_performance_ontological_status,
    'Does Torah study of sacrifice law possess the same ontological status as material sacrifice, or is it a functional substitute authorized by rabbinic jurisdiction?',
    'Systematic analysis of Talmudic and medieval halakhic sources on whether study creates a spiritual sacrifice (korban ha-lev) or a legally authorized substitute (taqqanah).',
    'If ontologically equivalent, the constraint functions as a fixed interpretive mountain within the tradition; if jurisdictionally authorized, it remains a rope maintained by interpretive consensus and revisable by later authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_performance_ontological_status, conceptual, 'Ontological vs jurisdictional grounding of study-as-performance').

omega_variable(
    communal_benefit_individual_cost_asymmetry,
    'Does the zero-extractiveness claim hold at the individual seat when study time displaces earning capacity and domestic labor?',
    'Economic ethnography and time-use studies of full-time Torah-study communities (kollel structures), measuring opportunity cost and subjective benefit.',
    'If individuals bear substantial opportunity cost without perceived individual benefit, the beneficiary declaration may describe only the collective seat, and individual seats could compute as moderate targets despite the zero-extraction claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(communal_benefit_individual_cost_asymmetry, empirical, 'Individual cost beneath collective benefit in study obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t25, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 25, 0.08).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 50, 0.08).
narrative_ontology:measurement(temp_tr_t75, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 75, 0.08).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(temp_be_t25, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 25, 0.02).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 50, 0.02).
narrative_ontology:measurement(temp_be_t75, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 75, 0.02).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 100, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel. It is structurally distinct from its siblings because it claims zero extractiveness and full occupation through study, whereas performance_only claims archival suspension, hybrid_preparatory claims suspended preparation, and symbolic_transformation claims authorized transformation. Each reading has a different beneficiary/victim structure and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
