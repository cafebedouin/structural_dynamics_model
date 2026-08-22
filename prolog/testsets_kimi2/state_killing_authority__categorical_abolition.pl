% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: State Killing Authority â Categorical Abolition Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the categorical abolition reading of
 *   the state_killing_authority kernel. The standing arrangement under
 *   contest is the state's claimed authority to execute condemned persons.
 *   From this reading, the arrangement is a snare: it extracts life from the
 *   condemned, suppresses abolitionist alternatives, and persists through
 *   coercion and performative legal ritual. The structural delta relative to
 *   sibling readings is that the condemned remain rights-holders (inalienable
 *   life), the state becomes a potential violator rather than a neutral
 *   arbiter, and victims' families are split between retributive
 *   beneficiaries and abolitionist payers marginalized by prosecutors.
 *
 * KEY AGENTS:
 *   - Condemned persons: Primary target (powerless/trapped) â bear the ultimate extraction.
 *   - State execution apparatus: Agenda-setter (institutional/constrained) â administers and enforces the constraint.
 *   - Prosecutorial office: Primary beneficiary (powerful/constrained) â collects political and career gains without direct administration.
 *   - Retributive victims' families: Secondary beneficiary (moderate/constrained) â receive state-validated emotional satisfaction.
 *   - Abolitionist victims' families: Secondary payer (moderate/constrained) â bear marginalization and silencing costs.
 *   - Civil rights observers: Analytical observer (organized/mobile) â monitor and resist the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.95).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.9).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "State Killing Authority â Categorical Abolition Reading").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, 'd6aa45a1-f40f-408c-9f53-0f37b02c678b').
narrative_ontology:cs_kernel_codification('d6aa45a1-f40f-408c-9f53-0f37b02c678b', formalized).
narrative_ontology:cs_authority_grounding('d6aa45a1-f40f-408c-9f53-0f37b02c678b', lineage).
narrative_ontology:cs_interpretation_layer_present('d6aa45a1-f40f-408c-9f53-0f37b02c678b').
narrative_ontology:cs_reading_relation('d6aa45a1-f40f-408c-9f53-0f37b02c678b', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('d6aa45a1-f40f-408c-9f53-0f37b02c678b', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('d6aa45a1-f40f-408c-9f53-0f37b02c678b', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('d6aa45a1-f40f-408c-9f53-0f37b02c678b', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('d6aa45a1-f40f-408c-9f53-0f37b02c678b', foundational, state_killing_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_killing_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('d6aa45a1-f40f-408c-9f53-0f37b02c678b', state_killing_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('d6aa45a1-f40f-408c-9f53-0f37b02c678b', inalienable_life_framework).
narrative_ontology:cs_drift_state('d6aa45a1-f40f-408c-9f53-0f37b02c678b', contemporary_abolitionist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d6aa45a1-f40f-408c-9f53-0f37b02c678b', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, prosecutorial_office).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, retributive_victims_families).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, political_operatives_tough_on_crime).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, abolitionist_victims_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Await execution on death row; legally and physically confined to a specific facility and procedural track. All exitsâescape, clemency, final appealâare structurally blocked or functionally illusory. The constraint takes their life as its direct extraction.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Administers capital punishment through courts, corrections departments, and execution protocols. Sets execution schedules, methods, and eligibility criteria. Operates within statutory and constitutional bounds but functions as the lethal instrument of state policy; cannot easily abolish itself without legislative or executive mandate.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_execution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Seeks and secures death sentences in capital-eligible cases. Gains political capital, career advancement, and electoral advantage from a tough-on-crime posture. Controls charging decisions and plea leverage that channel defendants toward death-eligible trials; benefits from the constraint's existence without directly carrying out executions.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, prosecutorial_office, beneficiary,
    powerful, biographical, constrained, national).

% Seek execution as a form of closure or justice for murdered family members. Receive state validation of their loss through the death sentence and execution. Depend entirely on the prosecutorial process to deliver the outcome they desire; have no independent power to compel or halt an execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, retributive_victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Oppose the execution of their family member's killer. Are silenced or marginalized by prosecutorial discretion and courtroom procedures that privilege retributive narratives and treat execution as the default victim interest. Bear psychological and social costs from a process that claims to speak in their name while overriding their opposition.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_victims_families, payer,
    moderate, biographical, constrained, local).

% Campaign on pro-death-penalty platforms to signal law-and-order credibility to electorates. Collect votes, campaign contributions, and media attention from constituencies that support capital punishment. Do not administer executions but benefit electorally and financially from the constraint's continued existence.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, political_operatives_tough_on_crime, beneficiary,
    powerful, biographical, mobile, national).

% Monitor executions, litigate constitutional challenges, and advocate for abolition. Operate from outside the state apparatus but within the legal system; their exit is mobile in that they can shift focus across jurisdictions or issues, but their influence is constrained by hostile political climates and entrenched procedural rules.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, civil_rights_observers, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The abolitionist reading holds that no genuine collective-action problem is solved by executing the condemned. The claimed coordination functionsâretributive justice and deterrenceâare rejected as cover stories for state-sanctioned killing.
% TRANSFER_FUNCTION: Moves the condemned person's life from the condemned to the state as a demonstration of state power; moves emotional satisfaction to retributive-leaning victims' families; and moves political capital to prosecutorial and political actors who leverage the constraint for career and electoral gain.
% ABSENT_VOICES: Abolitionist victims' families are marginalized within the prosecutorial process; condemned persons are structurally silenced by incarceration and the execution process; international human rights monitors are often excluded from domestic clemency and sentencing proceedings.
% DISAPPEARANCE_RATIONALE: If state killing authority vanished overnight, condemned persons would live, death rows would convert to life-sentence populations, specialized execution infrastructure and protocols would become obsolete, prosecutorial charging practices would shift away from capital charges, and political rhetoric would reorganize around non-lethal punishment frameworks.
% FOUNDING_PROBLEM: Historically: maintaining social order and punishing grave crime in the absence of stable non-lethal institutions. Contemporarily claimed by proponents: delivering retributive justice to victims and deterring future murder.
% FOUNDING_PROBLEM_CORROBORATION: Retributive-justice advocates and some criminologists attest the founding problem is live. Abolitionist scholars, human rights bodies, and empirical criminologists outside the benefiting parties attest that deterrence is empirically unsupported and retribution is not a legitimate state function; they argue the founding problem is dead or was never genuine.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.95, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-maximum (0.95) because the constraint takes life itselfâthe most absolute extraction possible. Suppression is very high (0.90) because the condemned are physically trapped, abolitionist voices are structurally marginalized, and exit alternatives (clemency, abolition) are actively blocked or performative. Theater ratio is moderate-high (0.55) and rising over the interval: executions have become increasingly bureaucratized, medicalized, and hidden behind procedural solemnity that masks raw violence. Accessibility collapse is high (0.85) for the primary targets (condemned persons have no accessible alternative once sentenced), while resistance (0.75) reflects sustained abolitionist litigation, international pressure, and moral challenge. Metrics are authored on a shared time grid to prevent temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats experience the constraint as a legitimate expression of state power or democratic will; the payer seats experience it as lethal extraction enforced by overwhelming institutional machinery. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary/victim declarations. The abolitionist reading does not reconcile these perspectivesâit asserts the payer perspective as structurally true.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned and abolitionist families are structural victims (high directionality toward the target end): they bear the costs of the constraint's operation and have trapped or heavily constrained exits. The prosecutorial office, political operatives, and retributive families are structural beneficiaries (low directionality): they collect political capital, electoral advantage, or emotional validation from the constraint's continued operation. The state execution apparatus sits as agenda-setter with constrained exit; it is not the primary beneficiary of extraction but is the administrative instrument that maintains the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading prevents mislabeling the death penalty as coordination (rope or tangled rope) by explicitly rejecting its claimed founding problemsâretribution and deterrenceâas cover stories. The R5 genealogy interview records a contested founding problem: the state claims it solves murder through desert or deterrence, but abolitionist scholars and human rights bodies outside the benefiting parties attest these problems are empirically unsupported or morally illegitimate. This blocks the mandatrophy pattern in which a dead founding problem is retrospectively relabeled as coordination to preserve the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_relation,
    'This constraint is the categorical_abolition reading of the state_killing_authority kernel; sibling readings retributive_desert and deterrence_instrument instantiate competing constraints from the same kernel.',
    'Comparative analysis of the three constraint stories in the family; no single empirical test resolves the kernel because the sibling disagreement is deontological and normative.',
    'Classification of the authority shifts between snare (this reading), tangled_rope or snare (retributive), and rope/scaffold (deterrence) depending on which reading is adopted as the governing framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation, conceptual, 'Commitater frame: this is one reading of a contested kernel with structurally distinct siblings.').

omega_variable(
    abolitionist_family_marginalization,
    'Are abolitionist victims'' families structurally excluded from the capital process, or merely outvoted within it?',
    'Courtroom participation records and prosecutorial victim-contact protocols: if abolitionist families are systematically denied allocution, notice, or standing relative to retributive families, the exclusion is structural rather than democratic.',
    'If structural, suppression is higher than formal rules suggest and the constraint operates as a deeper snare; if merely outvoted, the cost is democratic disagreement rather than extractive suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolitionist_family_marginalization, empirical, 'Whether abolitionist family opposition is structurally suppressed.').

omega_variable(
    executive_clemency_exit,
    'Does executive clemency constitute a genuine exit option for the condemned, or is it a theatrical safety valve that rarely opens?',
    'Clemency grant rates and procedural analysis: if grants are negligible and decisions are politically determined rather than mercy-driven, the exit is illusory.',
    'If illusory, condemned_persons'' exit_options should be classified as trapped rather than constrained, raising effective extraction toward the full-target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(executive_clemency_exit, empirical, 'Whether clemency provides real exit or performative relief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.25).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__categorical_abolition, theater_ratio, 10, 0.3).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__categorical_abolition, theater_ratio, 20, 0.35).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__categorical_abolition, theater_ratio, 30, 0.42).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.48).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__categorical_abolition, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__categorical_abolition, base_extractiveness, 10, 0.91).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__categorical_abolition, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__categorical_abolition, base_extractiveness, 30, 0.93).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.94).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__categorical_abolition, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__categorical_abolition, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__categorical_abolition, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__categorical_abolition, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__categorical_abolition, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, deterrence_instrument).

% DUAL FORMULATION NOTE:
% This constraint is the categorical_abolition reading of the state_killing_authority kernel. Sibling constraints (retributive_desert, deterrence_instrument) instantiate competing readings of the same kernel. The kernel decomposes into multiple structurally distinct claims because the same natural-language concept ('state killing authority') conflates deontological, retributive, and instrumental justifications that have different epsilon values, victim sets, and beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
