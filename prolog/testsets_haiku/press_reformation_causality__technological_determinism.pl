% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Printing Press as Autonomous Determinant of Reformation Inevitability
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the technological determinism reading of the
 *   contested kernel 'press_reformation_causality.' The reading claims that
 *   the printing press, as an autonomous enabling technology with intrinsic
 *   causal power, made the spread of vernacular scripture inevitable and
 *   Reformation success a determined outcome. This reading treats the
 *   technology as a mountain — a fixed structural fact whose properties
 *   dictate downstream social outcomes — and recasts Reformation actors
 *   (Luther, printers, princes) as passive respondents to technological
 *   capacity rather than as strategic agents with theological, political, and
 *   economic motives. The reading is one of three coherent framings; the
 *   others (co_constitution and strategic_deployment) decompose the causal
 *   story differently and assign agency differently. This story is the
 *   technological determinism reading only.
 *
 * KEY AGENTS:
 *   - printing_press (treated as autonomous technology, not as human artifact)
 *   - european_scribal_networks (passive medium, displaced by technology)
 *   - reformation_propagandists (downstream responders, not agents)
 *   - vernacular_literacy (enabled by technology, not chosen by constituencies)
 *   - manuscript_culture (passive predecessor, technologically superseded)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.18).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.05).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.18).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Printing Press as Autonomous Determinant of Reformation Inevitability").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '925ef1e1-cc3c-41e1-a902-4a4d53d89378').
narrative_ontology:cs_kernel_codification('925ef1e1-cc3c-41e1-a902-4a4d53d89378', formalized).
narrative_ontology:cs_authority_grounding('925ef1e1-cc3c-41e1-a902-4a4d53d89378', expertise).
narrative_ontology:cs_interpretation_layer_present('925ef1e1-cc3c-41e1-a902-4a4d53d89378').
narrative_ontology:cs_reading_relation('925ef1e1-cc3c-41e1-a902-4a4d53d89378', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('925ef1e1-cc3c-41e1-a902-4a4d53d89378', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('925ef1e1-cc3c-41e1-a902-4a4d53d89378', foundational, printing_press_autonomous_technological_determinant).
narrative_ontology:cs_axiom_status(printing_press_autonomous_technological_determinant, holdable).
narrative_ontology:cs_axiom_grounding('925ef1e1-cc3c-41e1-a902-4a4d53d89378', printing_press_autonomous_technological_determinant, empirically_contingent).
narrative_ontology:cs_axiom('925ef1e1-cc3c-41e1-a902-4a4d53d89378', foundational, human_actors_passive_responders_to_technological_capacity).
narrative_ontology:cs_axiom_status(human_actors_passive_responders_to_technological_capacity, holdable).
narrative_ontology:cs_axiom_grounding('925ef1e1-cc3c-41e1-a902-4a4d53d89378', human_actors_passive_responders_to_technological_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('925ef1e1-cc3c-41e1-a902-4a4d53d89378', technological_causal_determinism_framework).
narrative_ontology:cs_drift_state('925ef1e1-cc3c-41e1-a902-4a4d53d89378', contemporary_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('925ef1e1-cc3c-41e1-a902-4a4d53d89378', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, mechanistic_historical_narrative).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, technology_studies_determinism_school).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A doctrine claiming that technological capacity determines historical outcomes; does not collect material benefit but does accumulate scholarly authority and funding allocation through the deterministic reading.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, mechanistic_historical_narrative, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, mechanistic_historical_narrative).

% An interpretive tradition grounding historical explanation in technological capability; vindicates research programs emphasizing material infrastructure over human agency and intentionality.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, technology_studies_determinism_school, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(press_reformation_causality__technological_determinism, technology_studies_determinism_school).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this constraint is a causal claim about historical inevitability, not a social coordination mechanism. The printing press did not 'solve' a collective action problem; it was a technology that enabled a consequence.
% TRANSFER_FUNCTION: None — no transfer of resources or benefits between agents. The constraint is a claim about determinism, not about extraction or coordination.
% ABSENT_VOICES: Reformation agents (Luther, Calvin, printers, bishops, princes) would object to being characterized as passive responders to a technological determinant. Religious historians emphasizing doctrinal dispute, political historians emphasizing princely strategy, and social historians emphasizing literacy and circulation networks would all challenge the technological determinism framing.
% DISAPPEARANCE_RATIONALE: If the printing press had not been invented, would the Reformation have been impossible, merely delayed, or achieved differently through manuscript circulation and oral transmission? Determinists say 'impossible or severely curtailed'; others argue the religious contestation was live and would have found expression through available media. The disagreement is about the causal weight of technological capability versus human motivation and institutional change.
% FOUNDING_PROBLEM: How do we explain large-scale historical change? The technological determinism reading offers a mechanism: a technology with autonomous causal power, downstream of which human actors necessarily respond.
% FOUNDING_PROBLEM_CORROBORATION: Early McLuhan-influenced media history and some technology studies scholarship defend the technological determinism framing (Eisenstein, media archaeology tradition). Reformation historians (Andrew Pettegree, Brad Gregory), social historians of print (Adrian Johns), and agency-centered scholars of technology (Langdon Winner, Luciano Floridi) corroborate the alternative: technology is one variable among many, and the framing obscures the real causal work done by theology, politics, and deliberate deployment.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The technological determinism reading assigns the printing press a causal status as mountain — accessibility_collapse is very high (0.92) because once printing technology exists, the trajectory toward mass vernacular reproduction appears structurally inevitable; resistance is nearly zero (0.02) because the reading denies that human will or doctrinal dispute drove the outcome — technology did. Extractiveness is very low (0.18) because a genuine mountain collecting no rents should show negligible extraction; the modest value reflects that scholars and institutions invested in deterministic narratives do accrue professional authority through this framing, but the reading denies that extraction is the mechanism's point. Theater is minimal (0.08) because the reading claims a structural mechanism, not performative cover for hidden extraction. The measurement series show extractiveness rising slightly mid-interval (1500–1525) when determinism arguments are actively deployed in historical discourse, then declining as subsequent scholarship accumulates counter-evidence — this oscillation marks the constraint's epistemic life, not its material operation. Suppression is low throughout: a genuine natural law meets little active suppression; rising slightly during the interval reflects growing defensive rhetoric from determinism defenders against empirical challenges.
 *
 * PERSPECTIVAL GAP:
 *   The technological determinism reading privileges a single seat — the technology itself — so no perspectival gap arises from multiple stakeholders with different power/exit profiles. Instead, the gap is between the technological determinism reading (THIS constraint) and the other two readings (strategic_deployment, co_constitution). From the technological determinism seat: the press is autonomous, human actors respond. From the strategic_deployment seat: printers and reformers deliberately chose printing as a weapon, and would have used other media if available. From the co_constitution seat: technology and human agency entrained each other through feedback loops — neither was autonomous. The engine does not compute these cross-reading gaps; they are documented in omega variables and reading_relations (Rule 2). This story reports only what the technological determinism reading entails.
 *
 * DIRECTIONALITY LOGIC:
 *   The two declared stakeholders are non-agents (doctrines, not actors): 'mechanistic_historical_narrative' and 'technology_studies_determinism_school' are institutional beneficiaries of the deterministic reading insofar as it vindicates their research programs and funding claims. However, the technological determinism reading denies that benefit-seeking is the mechanism — it claims the reading is true because the technology's properties necessitate the outcome. This is the false-summit signal: beneficiaries are declared, yet the reading asserts naturality. The omega variables document the tension.
 *
 * MANDATROPHY ANALYSIS:
 *   The technological determinism reading avoids false mandatrophy claims by denying that intentionality or coordination is the mechanism — there is no mandate to solve a problem, only a causal arrow from technological capacity to outcome. However, the reading is vulnerable to false-summit detection: beneficiaries exist (institutions, scholars, funding allocators invested in deterministic narratives gain authority through the reading), and the claimed naturalness (emerges_naturally: true) conflicts with the beneficiary declaration. The omega on 'natural_law_vs_institutional_benefit' documents whether the reading's truth status is independent of who benefits from adopting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_institutional_benefit,
    'Is the technological determinism reading true because the printing press''s properties structurally necessitate the Reformation outcome, or is it defended because institutions and scholars invested in deterministic narratives accrue authority through it?',
    'Epistemic archaeology: compare the historical persistence of the determinism reading against the accumulation of counter-evidence from Reformation studies. If the reading persists despite empirical challenges (Pettegree''s recovery of elite manuscript circulation, Johns''s evidence of printer agency, historians'' documentation of doctrinal deliberation), the beneficiary-structure hypothesis is supported. If the reading is revised in line with evidence, the naturalism hypothesis is supported.',
    'If institutional benefit is the driver, the constraint is a false summit: it appears as natural law but operates as a beneficiary-capturing narrative, legitimating particular research programs while obscuring others. Reclassification would be from mountain to tangled_rope (coordination of scholarly authority) or snare (suppression of alternative explanations). If the reading is genuinely natural, it should track evidence and be abandoned if evidence contradicts it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_benefit, empirical, 'Whether the technological determinism reading''s persistence is driven by its truth or by institutional benefit.').

omega_variable(
    determinism_vs_enablement_boundary,
    'Does ''the technology made it inevitable'' mean the technology was sufficient for the outcome, or merely necessary but not sufficient?',
    'Counterfactual analysis: If the printing press had not been invented, would the Reformation have occurred through alternative media (manuscript circulation, oral preaching, visual art), merely delayed, with reduced scope, or not at all? No single historical source answers this; the disagreement is conceptual.',
    'If the press was sufficient, the determinism reading stands as a mountain. If the press was only necessary (among other factors like theology, politics, literacy investment), the reading misconstrues enablement as determination, and the constraint should be reclassified to capture the causal pluralism — rope (coordination of multiple enabling conditions) or co_constitution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(determinism_vs_enablement_boundary, conceptual, 'Whether technological capability is sufficient or merely necessary for the historical outcome.').

omega_variable(
    technology_autonomy_vs_constitution,
    'Is the printing press a naturally occurring physical constraint (autonomous technology) or a human-constituted artifact whose properties emerge from design choices, investment decisions, and institutional embedding?',
    'Genealogy of printing: trace the design choices (Gutenberg''s movable type, paper selection, page layout, binding choices) and institutional decisions (merchant investment, guild control, royal privilege, church permission) that shaped what ''the printing press'' became. If the technology is shown to be path-dependent on human choices, its autonomy claim is compromised.',
    'If printing is constituted through human choice, the technological determinism reading is undermined: the reading would be confusing ''what humans built and chose to deploy'' with ''what technology necessitates.'' The constraint would reclassify from mountain to tangled_rope (coordination) or strategic_deployment (reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_autonomy_vs_constitution, conceptual, 'Whether the printing press is an autonomous natural constraint or a constituted human artifact.').

omega_variable(
    determinism_reading_vs_co_constitution_boundary,
    'Can the technological determinism reading and the co_constitution reading both be true simultaneously, or does the determinism reading foreclose the co_constitution reading?',
    'Logical analysis: determinism claims the press''s properties necessitate the outcome independently of human agency; co_constitution claims technology and agency entrain each other. In formal logic, these are contrary claims. But in historical practice, might both readings capture different scales (technology determines at the macro level, agency matters at the micro level)? If scalable, they coexist; if not, one forecloses the other.',
    'Determines the reading_relations entry: if determinism forecloses co_constitution, relation=''forecloses''; if both can hold at different scales, relation=''coexists_with''. This affects the kernel''s logical structure and which readings are live options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(determinism_reading_vs_co_constitution_boundary, conceptual, 'Logical relationship between technological determinism and co-constitution readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causality__technological_determinism, theater_ratio, 1440, 0.02).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causality__technological_determinism, theater_ratio, 1470, 0.04).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__technological_determinism, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__technological_determinism, theater_ratio, 1525, 0.1).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__technological_determinism, theater_ratio, 1550, 0.08).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causality__technological_determinism, base_extractiveness, 1440, 0.05).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causality__technological_determinism, base_extractiveness, 1470, 0.08).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__technological_determinism, base_extractiveness, 1500, 0.15).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__technological_determinism, base_extractiveness, 1525, 0.22).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__technological_determinism, base_extractiveness, 1550, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causality__technological_determinism, suppression_requirement, 1440, 0.02).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causality__technological_determinism, suppression_requirement, 1470, 0.03).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__technological_determinism, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__technological_determinism, suppression_requirement, 1525, 0.07).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__technological_determinism, suppression_requirement, 1550, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__technological_determinism, 0.25).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint (technological_determinism reading) is part of a three-member constraint family decomposing the contested kernel 'press_reformation_causality.' The three readings are structurally distinct: technological_determinism treats the press as autonomous; strategic_deployment treats printers and reformers as deliberate agents; co_constitution treats technology and agency as entrained. Each reading instantiates a different constraint with different ε values, different beneficiary structures, and different causal mechanisms. The three readings coexist as live interpretive positions in historical scholarship and do not logically foreclose each other (see omega 'determinism_reading_vs_co_constitution_boundary' and reading_relations). All three stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
