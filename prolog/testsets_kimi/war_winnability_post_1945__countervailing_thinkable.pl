% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Nuclear Victory Thinkable
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   After 1945, nuclear weapons introduced a novel constraint on great-power
 *   war: total victory became catastrophically costly, yet strategic
 *   communities in nuclear-armed states preserved the intellectual and
 *   operational infrastructure for limited, counterforce-based 'victory.'
 *   This constraint story captures the reading that winnability remains
 *   thinkable and operationally planned. It treats the doctrine as a
 *   commitment system kernel reading: the same empirical reality (nuclear
 *   arsenals) supports three structurally distinct constraints
 *   (countervailing_thinkable, deterrence_unthinkable,
 *   rhetorical_contraction). This file instantiates only the
 *   countervailing_thinkable reading.
 *
 * KEY AGENTS:
 *   - military_industrial_complex: Primary beneficiary (institutional/arbitrage) â collects mission continuity and procurement flows
 *   - nuclear_planner_class: Primary agenda-setter (institutional/identity_locked) â authors and enforces the doctrine
 *   - arms_control_institutions: Primary victim (organized/constrained) â legitimacy undermined by winnable-war planning
 *   - civilian_populations: Diffuse payer (powerless/trapped) â bear catastrophic risk without voice
 *   - disarmament_advocates: Excluded voice (moderate/constrained) â structurally absent from planning
 *   - strategic_analysts: Analytical observer (analytical/analytical) â evaluates empirical plausibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.62).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.58).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Nuclear Victory Thinkable").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, 'ea054304-3a10-44aa-b189-00e4e5a4fcfc').
narrative_ontology:cs_kernel_codification('ea054304-3a10-44aa-b189-00e4e5a4fcfc', distributed).
narrative_ontology:cs_authority_grounding('ea054304-3a10-44aa-b189-00e4e5a4fcfc', expertise).
narrative_ontology:cs_interpretation_layer_present('ea054304-3a10-44aa-b189-00e4e5a4fcfc').
narrative_ontology:cs_reading_relation('ea054304-3a10-44aa-b189-00e4e5a4fcfc', war_winnability_post_1945__deterrence_unthinkable, forecloses).
narrative_ontology:cs_reading_relation('ea054304-3a10-44aa-b189-00e4e5a4fcfc', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('ea054304-3a10-44aa-b189-00e4e5a4fcfc', foundational, limited_counterforce_victory_achievable).
narrative_ontology:cs_axiom_status(limited_counterforce_victory_achievable, holdable).
narrative_ontology:cs_axiom_grounding('ea054304-3a10-44aa-b189-00e4e5a4fcfc', limited_counterforce_victory_achievable, empirically_contingent).
narrative_ontology:cs_axiom('ea054304-3a10-44aa-b189-00e4e5a4fcfc', foundational, deterrence_requires_winnability_credibility).
narrative_ontology:cs_axiom_status(deterrence_requires_winnability_credibility, holdable).
narrative_ontology:cs_axiom_grounding('ea054304-3a10-44aa-b189-00e4e5a4fcfc', deterrence_requires_winnability_credibility, instrumental).
narrative_ontology:cs_reference_frame('ea054304-3a10-44aa-b189-00e4e5a4fcfc', constrained_winnability_as_baseline).
narrative_ontology:cs_drift_state('ea054304-3a10-44aa-b189-00e4e5a4fcfc', post_cold_war_precision_strike_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ea054304-3a10-44aa-b189-00e4e5a4fcfc', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_institutions).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, counterforce_deterrence_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects sustained public investment and institutional mission continuity from the permanent planning and procurement of counterforce nuclear systems. Benefits from doctrinal frameworks that treat limited nuclear victory as achievable, which justify advanced delivery platforms, targeting infrastructure, and war-gaming budgets.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Professional military and civilian strategists who author classified nuclear war plans, run counterforce exercises, and adjudicate strategic rationality within the national security apparatus. Their professional identity and career advancement depend on the continued intellectual viability of limited nuclear war scenarios.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_planner_class, agenda_setter,
    institutional, biographical, identity_locked, national).

% International bodies and treaty frameworks whose purpose is to limit nuclear competition and reduce arsenals. Their legitimacy and effectiveness are undermined when major powers plan for winnable nuclear wars, draining diplomatic energy and weakening compliance incentives.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_institutions, payer,
    organized, generational, constrained, global).

% Bear the catastrophic risk inherent in strategic doctrines that normalize the thinkability of limited nuclear use. They cannot opt out of deterrence relationships, have no direct voice in targeting policy, and would suffer the direct humanitarian consequences if counterforce planning fails to stay limited.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations, payer,
    powerless, civilizational, trapped, global).

% NGO activists, peace researchers, and international-law advocates who argue that any planning for nuclear victory violates humanitarian norms and increases catastrophe risk. They are structurally excluded from classified planning processes and doctrinal reviews.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% Academic and independent policy analysts who study nuclear doctrine without institutional stakes in procurement or planning. They evaluate the empirical plausibility of counterforce claims and the opportunity costs of nuclear modernization.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains deterrence credibility by preserving a spectrum of limited nuclear options, ensuring that adversaries cannot count on nuclear thresholds to shield fait accompli aggression, and that escalation can be controlled rather than automatically total.
% TRANSFER_FUNCTION: Moves public capital, institutional mission, and strategic legitimacy from arms-control frameworks and civilian security into the permanent counterforce planning bureaucracy, advanced weapons procurement, and the professional class that sustains limited-war doctrine.
% ABSENT_VOICES: Disarmament advocates and civilian populations in potential target states are excluded from classified planning cells; their objections to the thinkability of limited victory are not incorporated into nuclear posture reviews.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, nuclear planning would shift away from counterforce victory scenarios toward minimum deterrence or disarmament, the MIC would lose its core modernization rationale, and arms control institutions would regain negotiating leverage â fundamentally reorganizing the global nuclear order.
% FOUNDING_PROBLEM: How to stabilize deterrence after 1945 without surrendering all war-planning capacity to the point that adversaries exploit perceived self-deterrence, while avoiding an unrestricted arms race.
% FOUNDING_PROBLEM_CORROBORATION: No corroboration from fully independent sources exists: the empirical case for counterforce necessity is classified and advanced by the same institutions that benefit from it. Independent academic critics and humanitarian-law scholars dispute the founding problem's continued relevance, but cannot access the targeting data needed to falsify it.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the doctrine channels vast public resources into counterforce systems and undermines cheaper alternatives (minimum deterrence, disarmament). Suppression (0.58) is moderate-high: classification, professional socialization, and institutional exclusion keep disarmament alternatives marginal. Theater ratio (0.45) reflects significant performative maintenance â nuclear posture reviews and counterforce exercises justify budgets partly by dramatizing controllability that empirical evidence questions. Accessibility collapse (0.60) indicates that while alternatives are intellectually available, they are politically collapsed inside planning circles. Resistance (0.55) captures sustained but institutionally weak opposition from arms control and humanitarian communities. The measurement series trace the Cold War intensification, post-Cold War partial drift, and modernization-era reconsolidation of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (nuclear planners) experiences the constraint as genuine coordination: without credible counterforce, deterrence fails. The payer seats (civilians, arms control institutions) experience the same structure as extraction that perpetuates risk and diverts resources. The engine computes this divergence from structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The military-industrial complex is the structural beneficiary (low d, subsidy via mission continuity). The nuclear planner class sits near symmetric but leans beneficiary: their identity is fused with the constraint, giving them biographical returns. Arms control institutions and civilian populations are structural targets (high d): the constraint extracts legitimacy from the former and security from the latter. Disarmament advocates are excluded, receiving no directional flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by separating the genuine coordination function (deterrence stability through escalation control) from the extraction layer (MIC mission continuity at the expense of arms control). If the coordination function were absent, the doctrine would be a pure snare. If the extraction were absent, it would be a rope. The temporal measurements show that extraction intensified during procurement surges even when the strategic environment relaxed, confirming the hybrid character.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterforce_controllability,
    'Is limited nuclear war via counterforce actually controllable, or have empirical findings (nuclear winter, command-and-control fragility, accidental escalation) overridden the achievability premise?',
    'Declassification and independent review of war-game outcomes, command-system stress tests, and climatic impact models; comparison with historical near-miss incidents.',
    'If uncontrollable, the coordination function collapses and the constraint becomes a snare: pure extraction by the MIC under the cover of a stability narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_controllability, empirical, 'Whether limited nuclear war is empirically achievable or a fiction').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of disarmament alternatives structural (classification, institutional exclusion) or internalized (planners and policymakers genuinely believe counterforce is the only rational option)?',
    'Career-path analysis of nuclear planners post-retirement and comparison with Soviet/Russian doctrinal evolution where different internalization patterns produced different postures.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint persists even if formal barriers fall because the profession carries it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of alternatives').

omega_variable(
    gain_concentration,
    'Does the extracted surplus concentrate in the military-industrial complex, or is it diffused across the broader national security state and allied academic institutions?',
    'Financial-flow tracing from nuclear modernization budgets to prime contractors, plus network analysis of funding ties between strategic studies centers and defense ministries.',
    'Concentrated capture would confirm snare-like dynamics; diffuse distribution would indicate a broader institutional equilibrium closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_concentration, empirical, 'Concentration of extraction in MIC versus diffuse security state').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wwct_tr_t0, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wwct_tr_t15, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 15, 0.28).
narrative_ontology:measurement(wwct_tr_t30, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 30, 0.4).
narrative_ontology:measurement(wwct_tr_t45, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 45, 0.48).
narrative_ontology:measurement(wwct_tr_t60, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 60, 0.42).
narrative_ontology:measurement(wwct_tr_t75, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(wwct_be_t0, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(wwct_be_t15, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(wwct_be_t30, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(wwct_be_t45, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 45, 0.68).
narrative_ontology:measurement(wwct_be_t60, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(wwct_be_t75, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(wwct_su_t0, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(wwct_su_t15, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(wwct_su_t30, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(wwct_su_t45, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 45, 0.72).
narrative_ontology:measurement(wwct_su_t60, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(wwct_su_t75, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 75, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel war_winnability_post_1945. The kernel decomposes into three structurally distinct claims: countervailing_thinkable (this file), deterrence_unthinkable, and rhetorical_contraction. Each has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
