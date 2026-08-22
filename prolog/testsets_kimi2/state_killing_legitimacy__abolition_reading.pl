% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Abolitionist Reading: Categorical Prohibition on State Killing
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the international human rights
 *   prohibition on capital punishment, institutionalized in treaty law,
 *   constitutional interpretation, and supranational jurisprudence. From the
 *   abolitionist reading, this constraint is a categorical moral absolute
 *   grounded in inherent human dignity: it benefits condemned persons by
 *   absolutely shielding them from state execution, and it extracts from
 *   state punitive power by disabling execution regardless of domestic law,
 *   democratic majorities, or policy utility. The reading presents the
 *   constraint as a natural-law floor (Mountain claim), but its operation
 *   depends on active enforcement by international courts, treaty monitoring
 *   bodies, and constitutional tribunals, while meeting sustained resistance
 *   from retentionist regimes. The beneficiary-victim structure is
 *   deliberately inverted relative to retributive and deterrence readings:
 *   here the condemned person is the rights-bearer beneficiary and the state
 *   killing apparatus is the victim of the norm's absolute stricture.
 *
 * KEY AGENTS:
 *   - Condemned persons (rights-bearer beneficiary / powerless / trapped): Receive absolute protection from execution; their survival depends entirely on the constraint's enforcement.
 *   - State punitive apparatus (structural victim / institutional / constrained): The institutional capacity for execution is categorically disabled by supranational norm enforcement.
 *   - Retentionist governments (concrete payer / institutional / constrained): Claim sovereign authority over capital sentencing but are blocked by treaty and judicial enforcement.
 *   - International human rights courts (agenda-setter / institutional / analytical): Administer and interpret the absolute prohibition through binding judgments and monitoring.
 *   - Abolitionist legal scholars (analytical observer / moderate / analytical): Supply the doctrinal framework and monitor compliance without bearing costs or receiving protection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.88).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.82).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, mountain).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Abolitionist Reading: Categorical Prohibition on State Killing").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).
domain_priors:emerges_naturally(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '1e5d26e6-4242-4c39-b5fe-fe0780787eb7').
narrative_ontology:cs_kernel_codification('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', formalized).
narrative_ontology:cs_authority_grounding('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', lineage).
narrative_ontology:cs_interpretation_layer_present('1e5d26e6-4242-4c39-b5fe-fe0780787eb7').
narrative_ontology:cs_reading_relation('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', foundational, human_dignity_inviolable).
narrative_ontology:cs_axiom_status(human_dignity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', human_dignity_inviolable, deontological).
narrative_ontology:cs_axiom('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', foundational, state_killing_categorically_prohibited).
narrative_ontology:cs_axiom_status(state_killing_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', state_killing_categorically_prohibited, deontological).
narrative_ontology:cs_reference_frame('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', inherent_dignity_absolute).
narrative_ontology:cs_drift_state('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', contemporary_retentionist_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1e5d26e6-4242-4c39-b5fe-fe0780787eb7', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_punitive_apparatus).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, retentionist_governments).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, inherent_human_dignity).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__abolition_reading, right_to_life_absolute).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals convicted of capital offenses who face state execution. The constraint categorically removes the state's power to kill them, converting a death sentence into an absolute shield regardless of procedural stage, domestic law, or popular will. Their only exit from the threat of execution is the constraint's operation; without it, they are killed.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, beneficiary,
    powerless, immediate, trapped, universal).

% The institutional complex of execution protocols, death-row infrastructure, and capital-case legal procedures maintained by retentionist states. The constraint disables this apparatus entirely, extracting its operative capacity and rendering its personnel, facilities, and doctrinal frameworks functionally obsolete regardless of domestic statutory authorization.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_punitive_apparatus, payer,
    institutional, generational, constrained, national).

% States that retain capital punishment in domestic law and claim sovereign authority over criminal sentencing. They are structurally blocked from executing by treaty obligations, supranational court judgments, and diplomatic conditionality. Their exit is constrained by the high costs of treaty denunciation, institutional isolation, and reputational damage.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retentionist_governments, payer,
    institutional, generational, constrained, national).

% Supranational judicial and monitoring bodies that interpret absolute life protections, invalidate domestic execution statutes, and issue binding judgments against retentionist practices. They administer the constraint's enforcement but do not themselves bear its costs or collect its extraction.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_courts, agenda_setter,
    institutional, civilizational, analytical, global).

% Academic and advocacy actors who construct the doctrinal linkage between inherent human dignity and categorical abolition, monitor state compliance, and file amicus interventions. They neither pay the constraint's costs nor receive its protective benefit, but supply its interpretive framework.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, abolitionist_legal_scholars, observer,
    moderate, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global legal consensus around an absolute floor for human dignity by unilaterally disabling a category of state violence, establishing a universal standard that no domestic system may fall below.
% TRANSFER_FUNCTION: Transfers absolute protective immunity to condemned persons and transfers the cost of disabled punitive capacity to state execution systems and retentionist governments.
% ABSENT_VOICES: Families of murder victims who favor execution; prison staff and prosecutors whose careers are built around capital-case procedure; retentionist political majorities whose democratic choice for capital punishment is overridden by supranational norm enforcement.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, retentionist states would resume executions, constitutional and treaty bars would empty, the supranational human rights architecture would lose its foundational absolute norm, and condemned persons would lose their existential shield â the global legal order would reorganize around permissible state killing.
% FOUNDING_PROBLEM: The historical absence of normative limits on state killing, enabling executions for criminal, political, colonial, and racial purposes without structural restraint.
% FOUNDING_PROBLEM_CORROBORATION: Documented by abolitionist historians and international legal scholars outside the benefiting parties; explicitly contested by retentionist governments and retributive justice theorists who deny that state killing constitutes a problem at all.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_legitimacy__abolition_reading),
    narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.88 because the prohibition is categorical â it extracts the entirety of the state's claimed execution power, leaving zero legal space for capital punishment regardless of context. Suppression is 0.82 because the constraint's persistence depends on actively suppressing state execution through treaty enforcement, constitutional litigation, and diplomatic conditionality; retentionist alternatives are structurally barred but politically persistent. Theater ratio is moderate-low (0.25) because the norm carries substantial functional content (actual stays of execution, treaty-based invalidation of statutes) alongside performative dimensions (symbolic resolutions, ritualized monitoring). Accessibility collapse is high (0.78) because once the dignity-absolute framework is accepted, execution becomes conceptually impossible as a state option. Resistance is moderate (0.55) reflecting active retentionist resistance (US federal and state levels, Saudi Arabia, Iran, China, Singapore) and the contested status of the norm in pluralistic legal orders.
 *
 * PERSPECTIVAL GAP:
 *   The state_punitive_apparatus seat and the condemned_persons seat compute diametrically: from the retentionist perspective the constraint is an external imposition that disables legitimate sovereign punishment; from the condemned person's perspective it is the absolute shield that prevents their killing. The engine derives this divergence from the identical structural arrangement â one agent's existential protection is another's total disability.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned person is the structural beneficiary (d near 0.0) because the constraint's entire function is to prevent their death; they collect absolute protection. The state punitive apparatus is the structural target (d near 1.0) because the constraint exists to disable its operative capacity. International courts administering the norm have low d (they are unharmed, they coordinate), while retentionist governments have high d (their sovereign choice is overridden).
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading prevents the mislabeling of this constraint as a Rope (coordination of society around shared punishment goals) by grounding the classification in the beneficiary-victim asymmetry: the condemned person is not a co-equal participant but a rights-bearer who receives protection without cost, while the state pays in disabled capacity. The reading also prevents mislabeling as pure Mountain by documenting the active enforcement apparatus and retentionist resistance that a genuine natural law would not require or encounter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_right,
    'Is the categorical prohibition on state killing a discovered natural law inherent in human dignity, or a constructed human rights convention assembled through post-war political consensus?',
    'Historical sociology of the norm''s emergence and cross-cultural variation in dignity-based legal orders; evidence of consistent convergence independent of institutional contact would support natural-law framing; evidence of treaty-contingent adoption and regional clustering would support constructed framing.',
    'If constructed, the Mountain claim is a false summit and the constraint reclassifies toward tangled_rope or snare; if natural law, the beneficiary structure and enforcement dependence require explanation as derivative rather than constitutive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Ambiguity between natural law and constructed convention for the dignity-based prohibition').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of state killing structural (treaty bars, conditional trade, court judgments) or internalized (normative identity shift in state self-conception)?',
    'Post-denunciation behavior: if states resume execution immediately after treaty withdrawal, suppression was structural; if hesitation persists due to identity costs, suppression is partially internalized.',
    'Internalized suppression implies higher effective extraction than structural measures suggest; structural-only suppression implies the constraint is more fragile and coercively maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of state killing capacity').

omega_variable(
    kernel_reading_contested,
    'This constraint is one reading of the state_killing_legitimacy kernel. How would sibling readings (retributive, deterrence) restructure the beneficiary-victim asymmetry and the epsilon profile?',
    'Comparative reading analysis across the kernel family: retributive and deterrence readings would treat the condemned as payer and the state as beneficiary, with substantially lower epsilon (justified extraction) and different suppression profiles.',
    'Sibling readings invert the directionality map and would likely compute as tangled_rope or rope rather than mountain, confirming the kernel''s decomposition into structurally distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contested, conceptual, 'Sibling reading structural deltas for the state killing legitimacy kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(skl_abolition_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(skl_abolition_tr_t10, state_killing_legitimacy__abolition_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(skl_abolition_tr_t20, state_killing_legitimacy__abolition_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(skl_abolition_tr_t30, state_killing_legitimacy__abolition_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(skl_abolition_tr_t40, state_killing_legitimacy__abolition_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(skl_abolition_tr_t50, state_killing_legitimacy__abolition_reading, theater_ratio, 50, 0.24).
narrative_ontology:measurement(skl_abolition_tr_t60, state_killing_legitimacy__abolition_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(skl_abolition_tr_t75, state_killing_legitimacy__abolition_reading, theater_ratio, 75, 0.25).

% Extraction over time
narrative_ontology:measurement(skl_abolition_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(skl_abolition_be_t10, state_killing_legitimacy__abolition_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(skl_abolition_be_t20, state_killing_legitimacy__abolition_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(skl_abolition_be_t30, state_killing_legitimacy__abolition_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(skl_abolition_be_t40, state_killing_legitimacy__abolition_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(skl_abolition_be_t50, state_killing_legitimacy__abolition_reading, base_extractiveness, 50, 0.84).
narrative_ontology:measurement(skl_abolition_be_t60, state_killing_legitimacy__abolition_reading, base_extractiveness, 60, 0.86).
narrative_ontology:measurement(skl_abolition_be_t75, state_killing_legitimacy__abolition_reading, base_extractiveness, 75, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(skl_abolition_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(skl_abolition_su_t10, state_killing_legitimacy__abolition_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(skl_abolition_su_t20, state_killing_legitimacy__abolition_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(skl_abolition_su_t30, state_killing_legitimacy__abolition_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(skl_abolition_su_t40, state_killing_legitimacy__abolition_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(skl_abolition_su_t50, state_killing_legitimacy__abolition_reading, suppression_requirement, 50, 0.79).
narrative_ontology:measurement(skl_abolition_su_t60, state_killing_legitimacy__abolition_reading, suppression_requirement, 60, 0.81).
narrative_ontology:measurement(skl_abolition_su_t75, state_killing_legitimacy__abolition_reading, suppression_requirement, 75, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, state_killing_legitimacy__deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the state_killing_legitimacy kernel family. The kernel decomposes into three structurally distinct readings because the epsilon values, beneficiary-victim structures, and foundational axioms differ across readings. This reading (abolition) treats the prohibition as a categorical dignity-based absolute; sibling readings treat state killing as justified by desert or utility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
