% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Price Formation as Natural Equilibrium (Naturalist Reading)
 *   domain: political_economy/housing_markets/market_mechanism
 *
 * SUMMARY:
 *   The naturalist reading holds that price formation in housing (and all
 *   markets) is a natural equilibrium process: supply and demand curves
 *   intersect at a market-clearing price that reflects objective scarcity,
 *   preferences, and production costs. Under this reading, prices are
 *   DISCOVERED, not CONSTRUCTED — they emerge impersonally from the
 *   interaction of millions of decentralized decisions. No agent orchestrates
 *   the outcome; no one is extracted from or extracts. Policy interventions
 *   that constrain supply, subsidize demand, or regulate pricing create
 *   deadweight loss by preventing the equilibrium from forming. The reading
 *   treats price as a natural fact, analogous to the angle of repose of sand
 *   or the boiling point of water: it is what it must be given the
 *   constraints of scarcity and preference. Alternative readings
 *   (institutional, georgist, financialization) contest this by naming
 *   agents, institutions, and power relations that shape prices; the
 *   naturalist reading denies that these shape the underlying equilibrium
 *   itself — they can only disrupt it.
 *
 * KEY AGENTS:
 *   - Individual buyers and sellers: atomistic decision-makers whose aggregate choices produce the equilibrium (no concentrated power; no organized actor)
 *   - The market mechanism itself: the impersonal process of price discovery (not an agent, but the analytical object)
 *   - Policy makers (as obstacles): any intervention that prevents equilibrium from forming
 *   - Analytical observer (this reading's seat): economists who measure the natural equilibrium against policy-created departures
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.05).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.02).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/market_mechanism").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, '4603323f-8fb7-4c20-86ff-68611f5ff7b9').
narrative_ontology:cs_kernel_codification('4603323f-8fb7-4c20-86ff-68611f5ff7b9', fixed_text).
narrative_ontology:cs_authority_grounding('4603323f-8fb7-4c20-86ff-68611f5ff7b9', expertise).
narrative_ontology:cs_interpretation_layer_present('4603323f-8fb7-4c20-86ff-68611f5ff7b9').
narrative_ontology:cs_reading_relation('4603323f-8fb7-4c20-86ff-68611f5ff7b9', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('4603323f-8fb7-4c20-86ff-68611f5ff7b9', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4603323f-8fb7-4c20-86ff-68611f5ff7b9', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('4603323f-8fb7-4c20-86ff-68611f5ff7b9', foundational, price_formation_by_supply_demand_equilibrium).
narrative_ontology:cs_axiom_status(price_formation_by_supply_demand_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('4603323f-8fb7-4c20-86ff-68611f5ff7b9', price_formation_by_supply_demand_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('4603323f-8fb7-4c20-86ff-68611f5ff7b9', secondary, market_intervention_creates_deadweight_loss).
narrative_ontology:cs_axiom_status(market_intervention_creates_deadweight_loss, holdable).
narrative_ontology:cs_axiom_grounding('4603323f-8fb7-4c20-86ff-68611f5ff7b9', market_intervention_creates_deadweight_loss, instrumental).
narrative_ontology:cs_reference_frame('4603323f-8fb7-4c20-86ff-68611f5ff7b9', neoclassical_equilibrium_framework).
narrative_ontology:cs_drift_state('4603323f-8fb7-4c20-86ff-68611f5ff7b9', contemporary_behavioral_empirical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4603323f-8fb7-4c20-86ff-68611f5ff7b9', '2026-06-12T14:32:17Z').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakeholders authored EMPTY (Pattern-5: an explicit assertion that no
% entity's arrangements depend on this constraint — paired with the
% world_unchanged verdict below, enforced by the schema).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Price discovery: buyers and sellers with heterogeneous preferences and endowments exchange information through price signals, allocating housing to highest-value uses without central planning.
% TRANSFER_FUNCTION: No systematic transfer. Prices clear markets; buyers pay market price, sellers receive it. The distribution reflects initial endowments and willingness to pay, not extraction.
% ABSENT_VOICES: The institutional reading identifies excluded parties (tenants in jurisdictions where zoning limits supply; workers priced out of high-value housing markets by credit constraints). The naturalist reading treats these as participants expressing low demand (low willingness to pay at equilibrium price) rather than excluded voices. Whether this treatment is accurate is the core contest.
% DISAPPEARANCE_RATIONALE: If price formation as a natural equilibrium process disappeared, it would be replaced by... price formation as a natural equilibrium process. The naturalist reading asserts that the mechanism is eternal and impersonal. If prices stopped reflecting scarcity and preference, it would only be because institutions intervened. So 'disappearance' of this constraint means institutional intervention becomes total — no more market discovery. The world WOULD rearrange, but only because the constraint has been replaced by its opposite (constructed price), not because the constraint itself goes away.
% FOUNDING_PROBLEM: The naturalist reading treats price formation as a law of nature, not a solution to a problem. There is no founding moment. Scarcity and preference have always existed; price discovery is nature's eternal mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from OUTSIDE the naturalist reading's beneficiary set (which is empty): empirical economists studying price behavior across diverse markets report that prices approximate equilibrium predictions in many contexts. However, institutional economists, housing scholars, and historians of real estate policy document extensive cases where prices diverge from simple supply-demand equilibrium and track institutional rules instead. The corroboration is CONTESTED. No universal external corroboration exists; different research traditions report different observations.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.05, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The naturalist reading authored as a mountain constraint scores near-zero on extractiveness (0.05), suppression (0.02), and theater_ratio (0.08). These low metrics are consistent with the claim that price formation is a natural process: no extraction occurs because there is no constructed beneficiary set, no suppression is needed because the equilibrium is self-maintaining (agents have no incentive to deviate once price clears the market), and no theater is needed because the mechanism is transparent and impersonal. Accessibility collapse is extremely high (0.92): once you understand the reading's premise (scarcity + preference → equilibrium price), the alternative of a constructed or manipulated price becomes inaccessible — you would have to deny the premise itself. Resistance is negligible (0.03) because the reading is presented as a law of nature, not a contested policy choice. The measurement series is flat across time: the reading treats price equilibrium as a stable, recurring phenomenon independent of historical period (though institutional interventions vary, the underlying natural process is eternal). The small rise in theater_ratio from 0.05 to 0.08 reflects increasing rhetorical use of equilibrium framing to justify policy neutrality, not a change in the underlying mechanism.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is built into the kernel structure. From the naturalist reading's seat (economic theory + equilibrium logic), price is impersonal and natural. From the institutional reading's seat (policy history + rule effects), price is constructed by zoning boards, lenders, tax codes. From the georgist reading's seat (land-value theory), price separates rent from improvement. From the financialization reading's seat (credit dynamics), price is driven by asset demand. The engine cannot compute per-seat classifications on the naturalist reading because there are no named seats with beneficiary/victim relationships. The institutional reading, by contrast, will name seats (zoning boards as beneficiaries; renters as victims) and will compute different d-values per seat. This gap is INTENTIONAL: if the institutional reading computes as extractive while the naturalist reading computes as mountain, the divergence itself is the diagnostic — it reveals whether price is natural or constructed.
 *
 * DIRECTIONALITY LOGIC:
 *   The naturalist reading has NO directionality logic because it names no beneficiaries and no victims. Directionality is defined as the structural relationship of an agent to the constraint; if there are no agents identified as benefiting or paying, there is no directionality to compute. This is a key structural test: does the naturalist reading's claim that price formation is impersonal hold up when the institutional, georgist, and financialization readings each name specific beneficiaries (lenders, landowners, financiers) from the same data? If yes, the mountain claim is coherent. If no, the beneficiaries are structural artifacts the naturalist reading suppresses, and FSM fires.
 *
 * MANDATROPHY ANALYSIS:
 *   The naturalist reading claims price formation was always a natural equilibrium; there is no founding problem to solve and no mandate that could become obsolete. The founding problem in this reading IS the constraint itself: scarcity and preference are eternally present, and price formation is nature's eternal response. Under the mandatrophy lens, the naturalist reading cannot suffer mandatrophy because it makes no time-bound claim about purpose — it claims timeless mechanism. The institutional, georgist, and financialization readings, by contrast, make claims about how INSTITUTIONS or FORCES AROSE and are OPERATING in particular historical contexts, so they carry mandatrophy risk. The naturalist reading deflects mandatrophy by positioning itself above history: the natural law is always operating; institutions just interfere with it or channel it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_reading,
    'Is price formation a natural equilibrium process reflecting objective scarcity, or a constructed outcome shaped by institutional arrangements and power relations?',
    'Comparative institutional analysis: examine price-formation outcomes across jurisdictions with radically different zoning, lending, and tax regimes; empirical test whether prices converge toward equilibrium independent of institutional structure, or whether they track institutional design.',
    'If prices are primarily institutional artifacts, this reading forecloses into institutional_reading (mountain → snare/tangled_rope). If prices reflect scarcity-driven equilibrium despite institutional variation, the mountain claim holds. The test is whether the reading''s core premise (objective scarcity discovered, not constructed) survives cross-jurisdictional evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_reading, empirical, 'Whether price formation is discovered from objective scarcity or constructed by institutions.').

omega_variable(
    deadweight_loss_counterfactual,
    'Does the naturalist reading''s claim that policy interventions create deadweight loss rest on an empirically testable comparison to a counterfactual equilibrium, or on an axiomatic assumption about market efficiency?',
    'Examine whether the reading specifies WHICH interventions, at WHAT scale, produce deadweight loss under WHICH conditions. If the claim is axiomatic (all interventions are efficiency-reducing by definition), it is not empirically resolvable and becomes a foundational axiom rather than a refutable prediction. If empirical, specify the measurement basis: price vs. quantity traded, consumer vs. producer surplus, aggregate welfare metrics.',
    'If axiomatic, the axiom is not empirically_contingent and cannot be foreclosed by empirical drift. If empirical, discrepancies between predicted and observed deadweight losses could trigger axiom_overriding drift (minor or substantial depending on effect size and acknowledgment by the reading''s authority structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadweight_loss_counterfactual, conceptual, 'Whether deadweight loss claims are empirically contingent or axiomatically grounded.').

omega_variable(
    beneficiary_presence_paradox,
    'The naturalist reading claims mountains have no beneficiaries — price formation is natural law, not constructed by any agent. Yet institutional, georgist, and financialization readings each identify beneficiaries (lenders, landlords, financiers, etc.) from THE SAME MARKET DATA. Are the beneficiaries real features of institutional design that the naturalist reading occludes, or observables that remain true under all readings but support different interpretations?',
    'FSM candidate: if the naturalist reading has no authored beneficiaries (true mountain profile: low extraction, negligible suppression, emerges_naturally=true) AND the sibling readings name institutional actors who benefit from the same price-formation arrangements, the false_summit_mountain signature fires. Resolution: examine whether the beneficiaries are artifacts of the reading''s framing (institutional reading: zoning boards, lenders benefit; naturalist reading: no one benefits because equilibrium is impersonal) or whether they are structural features independent of reading. If the latter, beneficiaries belong here and the mountain claim collapses.',
    'If FSM fires, the naturalist reading reclassifies as tangled_rope (natural law reading of an actual institutional arrangement). If FSM does not fire, the beneficiary absence is legitimate and reflects the reading''s core claim (impersonal market forces, no constructed beneficiary set).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_presence_paradox, conceptual, 'Whether price-formation beneficiaries are reading-dependent artifacts or structural features independent of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(pric_tr_t0, observed).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__naturalist_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement_basis(pric_tr_t5, observed).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__naturalist_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(pric_tr_t10, observed).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__naturalist_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(pric_tr_t15, observed).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__naturalist_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(pric_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(pric_be_t0, observed).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__naturalist_reading, base_extractiveness, 5, 0.04).
narrative_ontology:measurement_basis(pric_be_t5, observed).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__naturalist_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement_basis(pric_be_t10, observed).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__naturalist_reading, base_extractiveness, 15, 0.05).
narrative_ontology:measurement_basis(pric_be_t15, observed).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__naturalist_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement_basis(pric_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t0, price_formation_kernel__naturalist_reading, suppression_requirement, 0, 0.01).
narrative_ontology:measurement_basis(pric_su_t0, observed).
narrative_ontology:measurement(pric_su_t5, price_formation_kernel__naturalist_reading, suppression_requirement, 5, 0.01).
narrative_ontology:measurement_basis(pric_su_t5, observed).
narrative_ontology:measurement(pric_su_t10, price_formation_kernel__naturalist_reading, suppression_requirement, 10, 0.02).
narrative_ontology:measurement_basis(pric_su_t10, observed).
narrative_ontology:measurement(pric_su_t15, price_formation_kernel__naturalist_reading, suppression_requirement, 15, 0.02).
narrative_ontology:measurement_basis(pric_su_t15, observed).
narrative_ontology:measurement(pric_su_t20, price_formation_kernel__naturalist_reading, suppression_requirement, 20, 0.02).
narrative_ontology:measurement_basis(pric_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__naturalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__naturalist_reading, 0.03).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the price_formation_kernel. The naturalist reading asserts price is discovered from scarcity + preference. The institutional reading asserts price is constructed by zoning, lending, tax rules. The georgist reading asserts price separates land rent from improvement value. The financialization reading asserts price is driven by credit and asset-price feedback. These are not four perspectives on one constraint; they are four distinct constraints sharing a kernel (contested claim about market mechanism). Each has its own ε, its own beneficiary/victim structure (or absence thereof), its own classification. The network links document their interdependence: changing one reading's empirical status (e.g., institutional reading's evidence of zoning effects) affects the viability of others. The naturalist reading's mountain claim is vulnerable to FSM if the other readings' beneficiaries are real institutional features rather than reading artifacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
