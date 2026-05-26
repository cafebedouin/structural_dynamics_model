% ============================================================================
% CONSTRAINT STORY: study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_study_as_exercise, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: study_as_exercise
 *   human_readable: Study of Sacrifice Law as Performative Mitzvah Exercise
 *   domain: religious_studies/rabbinic_judaism/commitment_systems
 *
 * SUMMARY:
 *   In Rabbinic Judaism after the destruction of the Second Temple (70 CE),
 *   the performance of sacrificial rituals became impossible. The codashim
 *   (Mishnah's Order of Holy Things) consists of six tractates detailing
 *   sacrifice law in meticulous halakhic detail, but with no possibility of
 *   practical performance. The question of how to maintain the mitzvah of
 *   understanding sacrifice law became structurally urgent. The
 *   study-as-exercise reading constitutes one response: the
 *   intellectual-spiritual engagement with the legal texts IS itself the
 *   performance of the mitzvah. This is not a substitute for sacrifice, nor a
 *   mere study of a dead law, but the continuation of the mitzvah through a
 *   different modality. The beneficiary is the engaged scholar whose
 *   consciousness, through textual study, participates in cosmic order
 *   maintenance. There is no victim group because no one is deprived — the
 *   constraint is purely coordinative. The reading sits in competition with
 *   (or in coexistence with, depending on interpretive choice) the
 *   performance_only reading (which holds that studying sacrifice law is a
 *   subordinate good without itself constituting the mitzvah) and the
 *   substitution_archive reading (which holds that study preserves the legal
 *   knowledge for potential future performance but is itself an archive
 *   function, not the mitzvah). This constraint story instantiates ONE of
 *   those three readings: the one in which study and exercise are identical.
 *
 * KEY AGENTS:
 *   - Engaged scholars: Primary beneficiary (organized/civilizational/mobile) — participate in cosmic order through textual engagement; benefit from framework coherence and interpretive community
 *   - Interpretive institutional authority: Secondary beneficiary (powerful/generational/arbitrage) — maintains legitimacy through successful encoding and transmission of framework; benefits from scholarly engagement
 *   - The cosmic order (abstract): Beneficiary (analytical) — maintained through scholar participation, though grounding of this claim is under-determined (omega variable)
 *   - Analytical observer: External position (analytical/civilizational/analytical) — evaluates framework coherence and efficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(study_as_exercise, 0.0).
domain_priors:suppression_score(study_as_exercise, 0.0).
domain_priors:theater_ratio(study_as_exercise, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(study_as_exercise, theater_ratio, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(study_as_exercise, rope).
narrative_ontology:human_readable(study_as_exercise, "Study of Sacrifice Law as Performative Mitzvah Exercise").
narrative_ontology:topic_domain(study_as_exercise, "religious_studies/rabbinic_judaism/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(study_as_exercise, fixed_text).
narrative_ontology:cs_authority_grounding(study_as_exercise, lineage).
narrative_ontology:cs_interpretation_layer_present(study_as_exercise).
narrative_ontology:cs_kernel_id(study_as_exercise, kodashim_corpus).
narrative_ontology:cs_reading_relation(study_as_exercise, performance_only, coexists_with).
narrative_ontology:cs_reading_relation(study_as_exercise, substitution_archive, coexists_with).
narrative_ontology:cs_axiom(study_as_exercise, foundational, study_is_performance_not_substitute).
narrative_ontology:cs_axiom_status(study_is_performance_not_substitute, holdable).
narrative_ontology:cs_axiom_grounding(study_as_exercise, study_is_performance_not_substitute, deontological).
narrative_ontology:cs_axiom(study_as_exercise, foundational, intellectual_engagement_cosmic_efficacy).
narrative_ontology:cs_axiom_status(intellectual_engagement_cosmic_efficacy, holdable).
narrative_ontology:cs_axiom_grounding(study_as_exercise, intellectual_engagement_cosmic_efficacy, theological).
narrative_ontology:cs_reference_frame(study_as_exercise, textual_study_fulfillment_framework).
narrative_ontology:cs_drift_state(study_as_exercise, modern_academic_judaism_era, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(study_as_exercise, engaged_scholars).
narrative_ontology:constraint_beneficiary(study_as_exercise, cosmic_order_maintenance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENGAGED SCHOLAR (ROPE) — Scholars who adopt the study-as-exercise reading perceive pure coordination: intellectual-spiritual engagement with the sacrifice texts IS the fulfillment, not a substitute for or proxy of actual sacrifice. Low extraction because the beneficiary (the scholar's own spiritual condition and cosmic participation) is internal to the activity itself. Exit is mobile — scholars can leave the interpretive tradition, but those committed to it see no coercion, only shared practice.
constraint_indexing:constraint_classification(study_as_exercise, rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERPRETIVE INSTITUTIONAL AUTHORITY (ROPE) — Rabbinic authorities and yeshiva hierarchies that codify study-as-exercise see coordination function: maintaining the interpretive tradition, transmitting knowledge across generations, and upholding the framework that reading produces cosmic meaning from textual engagement. Beneficiary: the institution's legitimacy derives from successfully encoding and transmitting this framework. Arbitrage exit available but not exercised — institutional authority has collective interest in the framework's stability.
constraint_indexing:constraint_classification(study_as_exercise, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (ROPE) — From an external viewpoint, the study-as-exercise reading constitutes a genuine coordination mechanism: it solves the problem of post-Temple ritual maintenance by redefining the object of coordination from material sacrifice to intellectual-spiritual practice. The mechanism is economically efficient (no animal sacrifice required) and epistemically legitimate (textual study can demonstrate halachic depth). Low theater because the framework is coherent and internally consistent. Rope classification holds across all analytical positions — no hidden extraction, no suppression mechanism.
constraint_indexing:constraint_classification(study_as_exercise, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(study_as_exercise_tests).
:- end_tests(study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.0): Zero. This reading defines the constraint as pure coordination with no extraction mechanism. The activity (study) and the benefit (spiritual fulfillment, cosmic participation) are internal to the agent's own engagement. There is no external beneficiary extracting value from the agent's work, no suppression mechanism preventing exit, and no theater because the framework makes no claims about results beyond the agent's own consciousness. Suppression (0.0): Zero. Scholars can exit the interpretive tradition without material penalty, legal barrier, or economic deprivation. The only binding mechanism is commitment to the framework itself — a deontic choice, not a coercive barrier. Theater ratio (0.05): Minimal. The framework is internally coherent and transparent about its mechanism — intellectual engagement constitutes performance. The near-zero theater reflects that the framework makes no performative claims beyond what it transparently asserts.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives converge on rope classification. There is minimal perspectival gap because the constraint's structure — pure coordination with no extraction — is invariant across observer positions. The engaged scholar sees coordination (their own practice). The institutional authority sees coordination (framework transmission). The analytical observer sees coordination (mechanism efficiency). The gap that DOES exist is at the kernel level (omega variable): whether this reading forecloses the performance_only reading or coexists with it within the same halakhic framework. This gap is not reflected in constraint classification but in the reading_relations declaration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is derived from the pure-beneficiary, zero-suppression structure. Engaged scholars occupy the position of beneficiaries with mobile exit options, yielding low d values that produce negative or near-zero effective extraction chi. The institutional authority occupies a beneficiary position with arbitrage exit, also yielding low d. The analytical observer is positioned as a neutral evaluator with zero experiential stake in the outcome. The constraint has no victim group, so d derivation is unidirectional — only beneficiary positions exist.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not active for this constraint. The zero extractiveness and pure-coordination structure eliminate the tension between coordination and extraction classification. All perspectives converge on rope. The constraint's internal logic (study = performance, no victim group, mobile exit) is robust across the observable space. Mandatrophy would arise only if the performative sufficiency of study were empirically contested (omega variable) — in which case the engine might detect a gap between claimed rope and emergent snare (if study is revealed as functionally a substitute rather than performance). But from WITHIN the study-as-exercise framework, mandatrophy does not manifest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performative_sufficiency,
    'Does intellectual engagement with sacrifice texts genuinely constitute performance of the mitzvah, or does it constitute a substitute for performance that the reading rhetorically frames as equivalent?',
    'Textual analysis of foundational responsa (Maimonides, Talmudic sources) on whether study fulfills or merely commemorates the original mitzvah; comparison with other mitzvot that survived Temple destruction to distinguish genuine substitution logic from functional replacement.',
    'If study is genuine performance: rope classification confirmed; beneficiary = engaged scholar; zero extractiveness. If study is functionally a substitute: rope classification holds but with higher theater; beneficiary = institutional authority maintaining continuity narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_sufficiency, conceptual, 'Whether study genuinely fulfills the mitzvah or substitutes for it').

omega_variable(
    cosmic_order_claim_grounding,
    'What empirical or normative content grounds the claim that study of sacrifice law maintains cosmic order? Is this a metaphorical organizational principle, a literal theological claim, or a deontic commitment?',
    'Exegetical analysis of Maimonidean and Kabbalistic sources on the relationship between human action (study) and cosmic consequence (maintaining order); determination of whether the claim is amenable to falsification or is internally defined.',
    'If literal/empirical: rope classification vulnerable to challenge if cosmic maintenance is empirically undetermined. If deontic: rope classification more robust but requires explicit commitment to the framework. If metaphorical: theater ratio rises because the coordination function is performatively maintained rather than substantively validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cosmic_order_claim_grounding, conceptual, 'Grounding of cosmic order maintenance claim').

omega_variable(
    reading_kernelization_status,
    'Is the study-as-exercise reading an alternative instantiation of the kodashim_corpus kernel, or does it function as a competing reading that forecloses the performance_only reading''s core premise within the same halakhic framework?',
    'Historical analysis of talmudic and responsa literature: did authorities holding the performance_only reading explicitly foreclose study-as-exercise, or did they coexist as dual valid interpretations? Structural analysis of whether accepting study-as-exercise logically requires rejecting performance_only or whether both can be held in tension.',
    'If coexists: study-as-exercise and performance_only are readings of the same kernel, both livable within rabbinic tradition. If forecloses: study-as-exercise represents a watershed moment where the older understanding is formally superseded. Affects reading_relations declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernelization_status, empirical, 'Whether study-as-exercise forecloses or coexists with performance-only reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(study_as_exercise, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(study_as_exercise, performance_only).
narrative_ontology:affects_constraint(study_as_exercise, substitution_archive).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel decomposes into three structurally distinct readings with different ε values and beneficiary/victim structures. study_as_exercise (ε=0.0, rope, no victims) is upstream of and influences both performance_only and substitution_archive readings by establishing the interpretive precedent that post-Temple mitzvah continuation is possible. Each reading is a separate constraint story linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
