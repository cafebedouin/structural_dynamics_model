% ============================================================================
% CONSTRAINT STORY: instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_instrumentalist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: instrumentalist_reading
 *   human_readable: Instrumentalist Reading of Positional Disagreement as Evidence (Generative Falsifier Pipeline)
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the instrumentalist reading of the kernel
 *   commitment 'positional disagreement becomes evidence.' Under this
 *   reading, tractability is realized specifically because a cheap generative
 *   tool exists to produce candidate falsifiers and alternative-position
 *   samples at scale — the mechanism is the model's classification and
 *   bookkeeping capacity, not any epistemic principle about standpoint,
 *   procedure, or pragmatic cash-value. This decouples the instrumentalist
 *   claim from the standpoint reading (which locates evidentiary weight in
 *   marginalized positional knowledge), the pragmatist reading (which locates
 *   it in practical consequences of belief), and the proceduralist reading
 *   (which locates it in fair deliberative process). The instrumentalist
 *   reading's distinctive extraction path is new relative to all three
 *   siblings: whoever has slack — compute time, tooling fluency, survivable
 *   public error from a wrong generated sample — becomes the de facto
 *   beneficiary of tractability, and whoever lacks slack becomes a victim of
 *   the standard regardless of the substantive merit of their position or
 *   their standpoint-theoretic marginalization. ε is authored here for the
 *   instrumentalist arrangement as it stands, not for what standpoint or
 *   proceduralist advocates would replace it with.
 *
 * KEY AGENTS:
 *   - well_resourced_research_labs: primary beneficiary (institutional/arbitrage) — runs the generate-curate loop at scale
 *   - model_providers: agenda-setter and beneficiary (institutional/arbitrage) — designs the classification/bookkeeping mechanism itself
 *   - tooling_fluent_analysts: secondary beneficiary (organized/mobile) — extracts career advantage from pipeline fluency
 *   - unresourced_domain_experts: primary target (moderate/constrained) — genuine standing, insufficient slack to generate the expected register
 *   - communities_without_tooling_access: primary target (powerless/trapped) — structurally excluded from the evidentiary standard by access alone
 *   - positions_outside_curated_menus: non-agent victim — viewpoints the model under-samples or refuses, never entering the register
 *   - philosophers_of_science: analytical observer — traces whether tractability is genuine or merely relocated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(instrumentalist_reading, 0.58).
domain_priors:suppression_score(instrumentalist_reading, 0.42).
domain_priors:theater_ratio(instrumentalist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(instrumentalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(instrumentalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(instrumentalist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(instrumentalist_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(instrumentalist_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(instrumentalist_reading, "Instrumentalist Reading of Positional Disagreement as Evidence (Generative Falsifier Pipeline)").
narrative_ontology:topic_domain(instrumentalist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:requires_active_enforcement(instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(instrumentalist_reading, 'd890ba3c-55be-4cd0-8510-92885951c4ef').
narrative_ontology:cs_kernel_codification('d890ba3c-55be-4cd0-8510-92885951c4ef', distributed).
narrative_ontology:cs_authority_grounding('d890ba3c-55be-4cd0-8510-92885951c4ef', practice).
narrative_ontology:cs_interpretation_layer_present('d890ba3c-55be-4cd0-8510-92885951c4ef').
narrative_ontology:cs_reading_relation('d890ba3c-55be-4cd0-8510-92885951c4ef', instrumentalist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('d890ba3c-55be-4cd0-8510-92885951c4ef', instrumentalist_reading__pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('d890ba3c-55be-4cd0-8510-92885951c4ef', instrumentalist_reading__proceduralist_reading, influences).
narrative_ontology:cs_axiom('d890ba3c-55be-4cd0-8510-92885951c4ef', foundational, tractability_realized_through_generative_capacity_not_principle).
narrative_ontology:cs_axiom_status(tractability_realized_through_generative_capacity_not_principle, holdable).
narrative_ontology:cs_axiom_grounding('d890ba3c-55be-4cd0-8510-92885951c4ef', tractability_realized_through_generative_capacity_not_principle, instrumental).
narrative_ontology:cs_axiom('d890ba3c-55be-4cd0-8510-92885951c4ef', secondary, evidentiary_standing_tracks_slack_not_standpoint).
narrative_ontology:cs_axiom_status(evidentiary_standing_tracks_slack_not_standpoint, holdable).
narrative_ontology:cs_axiom_grounding('d890ba3c-55be-4cd0-8510-92885951c4ef', evidentiary_standing_tracks_slack_not_standpoint, empirically_contingent).
narrative_ontology:cs_reference_frame('d890ba3c-55be-4cd0-8510-92885951c4ef', argumentative_stalemate_baseline).
narrative_ontology:cs_drift_state('d890ba3c-55be-4cd0-8510-92885951c4ef', post_generative_tooling_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d890ba3c-55be-4cd0-8510-92885951c4ef', '').
narrative_ontology:cs_kernel_id(instrumentalist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(instrumentalist_reading, well_resourced_research_labs).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, model_providers).
narrative_ontology:constraint_beneficiary(instrumentalist_reading, tooling_fluent_analysts).
narrative_ontology:constraint_victim(instrumentalist_reading, unresourced_domain_experts).
narrative_ontology:constraint_victim(instrumentalist_reading, communities_without_tooling_access).
narrative_ontology:constraint_victim(instrumentalist_reading, positions_outside_curated_menus).
narrative_ontology:constraint_vindicates(instrumentalist_reading, positional_disagreement_is_tractable_via_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have the compute budget, engineering staff, and time to run large batches of generated falsifiers and alternative-position samples, then curate and bookkeep the results into publishable claims. The tractability of the disagreement is realized through their possession of the generative loop, not through any special epistemic standing.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, well_resourced_research_labs, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(instrumentalist_reading, well_resourced_research_labs, agenda_setter).

% Build and tune the generative models whose classification and bookkeeping capacity IS the mechanism that turns disagreement into tractable evidence. Set default sampling temperature, refusal behavior, and agreeableness, which determines which alternative positions are even generatable as candidates. Collect usage revenue and reputational credit for 'solving' positional disagreement at scale.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, model_providers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(instrumentalist_reading, model_providers, beneficiary).

% Know how to prompt, filter, and iterate the generative pipeline efficiently. Can produce a defensible-looking menu of falsifiers cheaply and move on to the next dispute, gaining career and argumentative advantage over those who must resolve disagreements the slow way.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, tooling_fluent_analysts, beneficiary,
    organized, biographical, mobile, national).

% Hold genuine domain knowledge but lack the compute time, subscription budget, or tooling fluency to run the generate-and-curate loop at the scale their well-resourced counterparts do. Their disagreements are treated as less tractable not because their position is weaker but because they cannot produce the register of candidate falsifiers the instrumentalist standard now expects as evidence.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, unresourced_domain_experts, payer,
    moderate, biographical, constrained, national).

% Lack reliable internet, hardware, or institutional subscriptions needed to run generative tools at all. Their positional disagreements are effectively excluded from the new evidentiary standard by default, regardless of the substantive merit of their standpoint.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, communities_without_tooling_access, payer,
    powerless, biographical, trapped, regional).

% Candidate viewpoints or falsifiers that the generative model systematically under-samples, refuses, or smooths away due to alignment tuning or agreeableness defaults. These positions never enter the curated menu that counts as 'the evidence,' regardless of whether they are true.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, positions_outside_curated_menus, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(instrumentalist_reading, positions_outside_curated_menus).

% Analyze whether the instrumentalist mechanism actually resolves positional disagreement or merely relocates the disagreement into a new layer — which candidates the model was willing to generate and which curator selected among them.
narrative_ontology:constraint_stakeholder(instrumentalist_reading, philosophers_of_science, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, cheap procedure for generating and bookkeeping candidate falsifiers and alternative-position samples, letting disputing parties compare a common register of machine-produced alternatives rather than arguing past each other indefinitely.
% TRANSFER_FUNCTION: Moves epistemic authority from whoever holds the strongest argument or deepest domain standing to whoever can run the generate-curate loop fastest and most fluently — shifting credit and dispute-resolution power toward compute access, tooling literacy, and model-provider design choices, and away from unresourced but substantively engaged parties.
% ABSENT_VOICES: Communities without tooling access and positions the model systematically under-generates (via alignment tuning, refusal behavior, or agreeableness defaults) never enter the curated menu that counts as evidence under this reading — they would object that their absence from the register is being read as absence of merit.
% DISAPPEARANCE_RATIONALE: Well-resourced labs and tooling-fluent analysts would lose a fast, legible way to adjudicate disputes and would need to fall back on slower argumentative or empirical methods — a real rearrangement for them. Unresourced experts and excluded communities would arguably be no worse off, since the instrumentalist standard was never treating their standing as the relevant evidence anyway; some would say the world improves for them if it disappeared. Whether disappearance rearranges the world or restores it depends on whose position in the arrangement you ask from.
% FOUNDING_PROBLEM: Positional disagreement (rival standpoints, contested framings, competing priors) traditionally resisted tractable resolution because generating and comparing alternative positions at scale was expensive and slow, so disputes stalled on rhetorical stalemate or institutional fiat.
% FOUNDING_PROBLEM_CORROBORATION: Model providers and well-resourced labs attest the founding problem is now substantially addressed, citing throughput gains in dispute resolution. Philosophers of science and several unresourced domain experts, corroborating from outside the beneficiary set, attest the founding problem persists in a relocated form: what changed is who can afford to generate the register, not whether positional disagreement is genuinely more tractable in principle.
narrative_ontology:disappearance_verdict(instrumentalist_reading, contested).
narrative_ontology:founding_problem_status(instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(instrumentalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(instrumentalist_reading, 0.58, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(instrumentalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(instrumentalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-rising 0.58 by interval end: the coordination function (a shared register for comparing alternative positions) is real and non-trivial, but the standard's cost structure systematically favors slack-holders, and that asymmetry deepens as the pipeline becomes institutionally normalized as 'the' evidentiary procedure. Suppression is lower (0.42) because no one is coercively barred from disagreeing — the exclusion operates through access and curation rather than direct prohibition, which is exactly what makes this reading distinct from a snare. Theater ratio rises modestly (0.31) as bookkeeping and menu-curation activity increasingly substitutes for genuine adjudication of the underlying disagreement. Accessibility collapse is moderate (0.47): once the instrumentalist standard is adopted, unresourced parties do not lose the ability to disagree, but they lose the ability to have their disagreement counted as tractable evidence on the same terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Model providers and well-resourced labs sit near the full-beneficiary end: they set the terms of the mechanism (what gets generated, what counts as a candidate falsifier) and capture reputational and institutional credit for resolving disagreements at scale. Tooling-fluent analysts are secondary beneficiaries via mobility and skill arbitrage. Unresourced domain experts and access-poor communities sit near the full-target end: constrained or trapped exit options, no capacity to generate the expected register, and their disagreements are read as less tractable through no fault of their argument's substance. Positions outside the curated menu are a non-agent victim class — they never enter the evidentiary record at all, which is a structurally different harm than being outvoted within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — positional disagreement resisting tractable adjudication — was genuinely live prior to cheap generative tooling. Whether it remains live in its original form, or has been replaced by a narrower problem (who can afford to run the loop), is exactly the contested status this story authors. Treating the instrumentalist mechanism as having 'solved' positional disagreement in general, rather than having solved it conditional on slack, would be the mandatrophic move — declaring the founding problem dead when it has only been relocated onto those without tooling access.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumentalist_reading_identity,
    'Is the tractability this reading describes a genuine advance in resolving positional disagreement, or a relabeling of an old resource-asymmetry (who can afford to investigate) as a new epistemic one (who can afford to generate)?',
    'Compare dispute-resolution outcomes for slack-rich vs. slack-poor parties before and after generative-tool adoption, controlling for the substantive merit of positions as judged by independent domain review.',
    'If outcomes track slack rather than merit even after independent review, the instrumentalist mechanism is substantially extraction riding on a real but thin coordination function — consistent with the tangled_rope claim. If outcomes track merit once slack is controlled for, the coordination function dominates and the story is closer to a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalist_reading_identity, empirical, 'Whether instrumentalist tractability tracks merit or resource slack.').

omega_variable(
    sibling_reading_disagreement_location,
    'Where exactly does the instrumentalist reading''s core premise (tractability via generative classification/bookkeeping capacity, no epistemic principle required) diverge structurally from the pragmatist reading''s core premise (tractability via practical consequences of belief)?',
    'Identify a disputed case where the two readings would recommend opposite resolutions — e.g., a candidate falsifier that is cheaply generatable but has no practical consequence either way — and determine which reading''s recommendation the field actually follows.',
    'If the field follows generatability regardless of practical stakes, the instrumentalist and pragmatist readings are genuinely distinct kernels rather than notational variants; if practical consequence always trails and explains generatability, the two readings may collapse into one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_location, conceptual, 'Locating the structural disagreement between instrumentalist and pragmatist readings.').

omega_variable(
    model_agreeableness_as_gatekeeping,
    'Does model-provider tuning for agreeableness and refusal-avoidance function as a novel, largely invisible gatekeeping mechanism over which positions can even become candidate falsifiers?',
    'Audit generation refusal rates and sampling diversity for controversial vs. uncontroversial positional claims across model versions and providers.',
    'If refusal/agreeableness tuning systematically under-samples specific classes of positions, the curated-menu extraction path is confirmed as structural rather than incidental, strengthening the case for tangled_rope over rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(model_agreeableness_as_gatekeeping, empirical, 'Whether model tuning constitutes a hidden gatekeeping layer over evidentiary menus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(instrumentalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, instrumentalist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(inst_tr_t4, instrumentalist_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(inst_tr_t8, instrumentalist_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(inst_tr_t12, instrumentalist_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(inst_tr_t16, instrumentalist_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(inst_tr_t20, instrumentalist_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(inst_tr_t24, instrumentalist_reading, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, instrumentalist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(inst_be_t4, instrumentalist_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(inst_be_t8, instrumentalist_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(inst_be_t12, instrumentalist_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(inst_be_t16, instrumentalist_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(inst_be_t20, instrumentalist_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(inst_be_t24, instrumentalist_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(inst_su_t0, instrumentalist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(inst_su_t4, instrumentalist_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(inst_su_t8, instrumentalist_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(inst_su_t12, instrumentalist_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(inst_su_t16, instrumentalist_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(inst_su_t20, instrumentalist_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(inst_su_t24, instrumentalist_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(instrumentalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(instrumentalist_reading, 0.12).
narrative_ontology:affects_constraint(instrumentalist_reading, standpoint_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, pragmatist_reading).
narrative_ontology:affects_constraint(instrumentalist_reading, proceduralist_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language kernel 'positional disagreement becomes tractable evidence' (positional_disagreement_as_evidence). The instrumentalist_reading is distinguished by locating the evidentiary mechanism in a generative tool's classification/bookkeeping capacity rather than in standpoint marginalization (standpoint_reading), practical consequence (pragmatist_reading), or fair deliberative procedure (proceduralist_reading). Each reading carries its own ε, beneficiary/victim structure, and claimed type; they are linked here rather than merged because the beneficiary sets and extraction paths differ substantially by reading — per the ε-invariance principle, this is four constraints, not one constraint viewed four ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
