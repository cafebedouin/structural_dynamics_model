% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Defended Extractive Structure
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story models market dominance as an actively maintained
 *   institutional arrangement rather than a spontaneous economic outcome.
 *   Under the beneficiary_maintained_reading of the market_naturalization
 *   kernel, incumbent capital holders deploy regulatory capture, strategic
 *   litigation, and ideological production to defend concentrated market
 *   positions against competitive entry and redistributive policy. The
 *   arrangement extracts consumer surplus, suppresses wages, and blocks rival
 *   firms while presenting itself as the natural result of efficiency and
 *   scale. The story captures a structural asymmetry between concentrated
 *   beneficiaries with generational planning horizons and arbitrage-grade
 *   exit options, and dispersed payers with constrained or trapped exits.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders (agenda_setter/beneficiary, institutional/arbitrage): Actively defend dominance through lobbying, capture, and barrier-erection; capture rents.
 *   - consumers (payer, powerless/constrained): Pay elevated prices and suffer reduced choice; lack collective leverage.
 *   - workers (payer, powerless/constrained): Face wage suppression and precarity under dominant employers.
 *   - excluded_competitors (payer, moderate/trapped): Blocked from entry by regulatory and strategic barriers.
 *   - state_regulatory_bodies (agenda_setter, institutional/constrained): Enforce the dominance structure through captured antitrust and regulatory policy.
 *   - economic_historians (observer, analytical/analytical): Document the active defense mechanisms and ideological naturalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.82).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Defended Extractive Structure").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '948872d5-bd38-4475-a1be-b3ec5f4bf9f4').
narrative_ontology:cs_kernel_codification('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', distributed).
narrative_ontology:cs_authority_grounding('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', extraction).
narrative_ontology:cs_interpretation_layer_present('948872d5-bd38-4475-a1be-b3ec5f4bf9f4').
narrative_ontology:cs_reading_relation('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', market_naturalization__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', foundational, market_dominance_is_actively_constructed).
narrative_ontology:cs_axiom_status(market_dominance_is_actively_constructed, holdable).
narrative_ontology:cs_axiom_grounding('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', market_dominance_is_actively_constructed, empirically_contingent).
narrative_ontology:cs_axiom('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', foundational, incumbent_rent_extraction_is_structural).
narrative_ontology:cs_axiom_status(incumbent_rent_extraction_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', incumbent_rent_extraction_is_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', naturalized_market_order).
narrative_ontology:cs_drift_state('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', contemporary_empirical_record, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('948872d5-bd38-4475-a1be-b3ec5f4bf9f4', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, workers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, excluded_competitors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control dominant positions across consolidated sectors; deploy lobbying, regulatory capture, strategic litigation, and ideological production to block antitrust enforcement and erect barriers to entry. Capture surplus via supracompetitive pricing, wage suppression, and rent extraction. Can relocate capital across jurisdictions if any single market becomes hostile.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary).

% Pay artificially elevated prices and accept reduced product variety because dominant firms control supply. Individual exit is limited by the absence of competitive alternatives in concentrated markets; collective action is fragmented by scale and coordination costs.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, consumers, payer,
    powerless, biographical, constrained, national).

% Face suppressed wages and precarious employment conditions because dominant employers set labor-market terms with limited competition. Geographic immobility, skill specificity, and non-compete clauses constrain exit to similarly dominated labor markets.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, workers, payer,
    powerless, biographical, constrained, national).

% Startups and smaller firms blocked from market entry by predatory pricing, patent thickets, platform exclusivity, and regulatory barriers shaped by incumbents. Innovation is either acquired and shelved or priced out of the market; capital access is gated by incumbent-controlled financing networks.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, excluded_competitors, payer,
    moderate, biographical, trapped, national).

% Administer antitrust, patent, and regulatory standards that structurally favor incumbents. Staffed heavily by revolving-door personnel from dominant firms; possess formal authority to reform markets but exercise it to maintain the existing dominance structure due to capture and dependency on incumbent-generated policy expertise.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, state_regulatory_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Analyze market concentration trends and document active defense mechanisms. Identify the divergence between competitive-market ideology and concentrated outcomes. Do not collect or pay within the constraint; their seat is analytical.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, economic_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates control over capital-intensive supply chains and investment decisions to reduce uncertainty and coordinate output across consolidated sectors, replacing fragmented competition with centralized pricing and capacity planning.
% TRANSFER_FUNCTION: Moves consumer surplus, labor income, and competitor profits to incumbent capital holders through supracompetitive pricing, wage suppression, and regulatory capture, while externalizing enforcement costs to the public regulatory apparatus.
% ABSENT_VOICES: Consumer collectives, organized labor seeking sectoral bargaining, and would-be competitors are structurally excluded from antitrust and regulatory proceedings; anti-monopoly scholars and independent researchers are marginalized in mainstream economic discourse and policy formation.
% DISAPPEARANCE_RATIONALE: If the active defense of dominance vanished overnight, entry barriers would fall, pricing power would erode, wages would rise toward competitive levels, and the institutional apparatus of regulatory capture would lose its primary function; the political economy of multiple sectors would reorganize around contested rather than concentrated markets.
% FOUNDING_PROBLEM: Capital-intensive industries in the late 19th and early 20th centuries faced coordination failures: price wars, overproduction, and insufficient economies of scale to justify large fixed-capital investments.
% FOUNDING_PROBLEM_CORROBORATION: Independent economic historians and post-1980 antitrust revisionist scholars attest that scale economies are achievable without dominance in most modern sectors; incumbent-funded industry associations and captured regulators claim the problem remains live, but corroboration from outside the benefiting partiesâpeer-reviewed economic history, legislative testimony by independent experts, and cross-national sectoral analysisâsupports the dead-status reading.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint systematically transfers surplus from consumers, workers, and excluded competitors to incumbent capital holders through supracompetitive pricing and wage-setting. Suppression (0.78) reflects active mechanismsâregulatory capture, predatory pricing, patent thickets, and platform exclusivityâthat block alternatives. Theater ratio (0.45) captures the widening gap between the coordination justification (efficient scale, consumer benefit) and actual enforcement activity, which increasingly serves rent defense. Accessibility collapse (0.65) indicates that while theoretical alternatives (strong antitrust, public options) exist, they are functionally inaccessible due to institutional capture. Resistance (0.55) acknowledges episodic populist and regulatory challenges that have not yet overcome incumbent defensive capacity.
 *
 * PERSPECTIVAL GAP:
 *   The incumbent seat experiences the constraint as necessary coordinationâstable investment horizons, predictable returns, and efficient supply chainsâwhile payer seats experience it as extraction. The engine will compute a low directionality (beneficiary side) for incumbents and high directionality (target side) for consumers and workers, producing divergent per-seat classifications: the incumbent seat may compute near rope/coordination, while worker and consumer seats compute near snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders are declared beneficiaries and agenda-setters with arbitrage-grade exit, placing their directionality near the full-beneficiary end (low d, damped or inverted extraction). Consumers, workers, and excluded competitors are declared victims (role: payer) with constrained or trapped exit, placing their directionality near the full-target end (high d, amplified extraction). State regulatory bodies are agenda-setters but with constrained exit (captured), yielding a moderate d. Economic historians are analytical observers with analytical exit, yielding neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmarket instability from fragmented competition in capital-intensive industriesâis substantially dead for most modern sectors, yet the constraint persists because incumbents actively defend it. Classifying as tangled_rope prevents the error of labeling the arrangement pure coordination (rope) and thereby missing the asymmetric extraction, while also preventing the error of labeling it pure extraction (snare) and missing the genuine resource-allocation coordination it still performs. The mandatrophy signal (dead founding problem plus world_rearranges disappearance verdict) flags the constraint as a candidate for structural reform rather than maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_naturalization_ambiguity,
    'This constraint is the beneficiary_maintained_reading of the market_naturalization kernel; is dominance actively maintained, naturally lapsed, or hybrid? The disagreement centers on whether identifiable beneficiaries continuously suppress alternatives.',
    'Cross-reading comparison of enforcement trends and incumbent profit cycles against entry rates in jurisdictions with varying antitrust intensity.',
    'Determines whether the constraint is a tangled_rope or snare (this reading) versus a piton or lapsed structure (siblings)âthe kernel''s classification is underdetermined by a single reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_naturalization_ambiguity, conceptual, 'Kernel-level ambiguity about whether market dominance is maintained or lapsed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternatives structural (regulatory barriers, legal coercion) or internalized (ideological belief in incumbent superiority)?',
    'Post-deregulation entry trajectory: if entrants emerge immediately after barrier removal, suppression was structural; if inertia persists, internalization is significant.',
    'If internalized, effective suppression exceeds the structural measure, pushing the constraint toward higher extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    marginal_cost_of_dominance,
    'What is the actual ratio of incumbent rent extraction to the coordination benefits provided by centralized market control?',
    'Comparative sectoral analysis of deconcentrated versus concentrated industries on investment, innovation, and consumer surplus metrics.',
    'A wide gap would confirm the extraction-centric classification; a narrow gap would support a stronger coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_cost_of_dominance, empirical, 'Rent-to-coordination benefit ratio.').

omega_variable(
    state_capture_depth,
    'To what extent does the regulatory apparatus act autonomously versus as a direct instrument of incumbent capital?',
    'Quantification of revolving-door flows, campaign finance dependency, and regulatory outcome analysis across jurisdictions.',
    'If the state is fully captured, the directionality of state actors shifts toward beneficiary; if autonomous, it may shift toward neutral or even payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_depth, empirical, 'Depth of regulatory capture by incumbent capital.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mark_tr_t10, market_naturalization__beneficiary_maintained_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(mark_tr_t20, market_naturalization__beneficiary_maintained_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(mark_tr_t30, market_naturalization__beneficiary_maintained_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(mark_tr_t50, market_naturalization__beneficiary_maintained_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mark_be_t10, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(mark_be_t20, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(mark_be_t30, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(mark_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(mark_be_t50, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mark_su_t10, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(mark_su_t20, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(mark_su_t30, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(mark_su_t40, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(mark_su_t50, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
