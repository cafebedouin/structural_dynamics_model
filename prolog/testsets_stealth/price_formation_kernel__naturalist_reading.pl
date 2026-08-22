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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Price Formation as Natural Equilibrium Discovery (Naturalist Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   This story instantiates the naturalist reading of housing price
 *   formation: prices emerge spontaneously from decentralized exchange and
 *   encode objective scarcity and preference; they are discovered, not
 *   posted. On this reading the arrangement has no administrator of prices,
 *   no designed beneficiary, and no designed victim — it resembles a natural
 *   process that policy can perturb but not replace. The participants below
 *   are described as this reading sees them: transactors on both sides of
 *   mutually ranked exchange, parameter-setting authorities whose instruments
 *   move costs and credit rather than prices themselves, and an analytical
 *   profession that models the process. Family relationships to the three
 *   sibling readings of the same kernel are documented in
 *   commentary.kernel_context and network.dual_formulation_note; this file
 *   authors only the naturalist reading's own structure. KEY AGENTS (by
 *   structural relationship): - housing_households: Demand-side price-takers
 *   (moderate/constrained) — pay clearing prices, receive use surplus -
 *   property_owners_landlords: Supply-side price-receivers (organized/mobile)
 *   — collect revenues, bear carrying costs - residential_developers:
 *   Signal-responsive suppliers (powerful/mobile) — convert prices into
 *   construction decisions - planning_and_monetary_authorities: Parameter
 *   administrators (institutional/constrained) — set the rules and credit
 *   conditions exchange runs inside - housing_economists: Analytical observer
 *   — models and tests the process from outside the exchange
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.05).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.08).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.04).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.04).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Price Formation as Natural Equilibrium Discovery (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "economic/political").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'afc7a1cb-b21d-42e9-9c1d-c8be8d43195e').
narrative_ontology:cs_kernel_codification('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', distributed).
narrative_ontology:cs_authority_grounding('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', expertise).
narrative_ontology:cs_interpretation_layer_present('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e').
narrative_ontology:cs_reading_relation('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', price_formation_kernel__institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', price_formation_kernel__financialization_reading, forecloses).
narrative_ontology:cs_axiom('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', foundational, prices_discovered_not_constructed).
narrative_ontology:cs_axiom_status(prices_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', prices_discovered_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', foundational, intervention_creates_deadweight_loss).
narrative_ontology:cs_axiom_status(intervention_creates_deadweight_loss, holdable).
narrative_ontology:cs_axiom_grounding('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', intervention_creates_deadweight_loss, instrumental).
narrative_ontology:cs_reference_frame('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', competitive_equilibrium_price_discovery).
narrative_ontology:cs_drift_state('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', post_global_financial_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('afc7a1cb-b21d-42e9-9c1d-c8be8d43195e', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(price_formation_kernel__naturalist_reading, housing_households).
narrative_ontology:constraint_beneficiary(price_formation_kernel__naturalist_reading, property_owners_landlords).
narrative_ontology:constraint_beneficiary(price_formation_kernel__naturalist_reading, residential_developers).
narrative_ontology:constraint_victim(price_formation_kernel__naturalist_reading, housing_households).
narrative_ontology:constraint_victim(price_formation_kernel__naturalist_reading, property_owners_landlords).
narrative_ontology:constraint_victim(price_formation_kernel__naturalist_reading, residential_developers).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, hayek_dispersed_knowledge_thesis).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, walrasian_general_equilibrium_theory).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, first_welfare_theorem_applicability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bid for housing across sale and rental markets and pay the clearing price or go unserved. When they transact they receive housing they demonstrably rank above the price paid. Exit means relocating, doubling up, or switching tenure mode — there is no opting out of price-guided allocation of shelter.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, housing_households, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__naturalist_reading, housing_households, beneficiary).

% Receive market prices and rents for the housing they supply while bearing maintenance, taxes, taxes-on-transactions, and capital at risk. They can withhold units, convert them, or sell and redeploy proceeds — responses that arbitrage local price distortions back toward the margin.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, property_owners_landlords, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__naturalist_reading, property_owners_landlords, payer).

% Read prices as build or do-not-build instructions. They earn margins when prices exceed replacement cost and stand down when they do not; their entry and exit is the supply response that erodes scarcity premiums over time.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, residential_developers, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__naturalist_reading, residential_developers, payer).

% Set zoning envelopes, lending standards, tax treatment, and short-term interest rates — the parameters inside which housing exchange occurs. They cannot post a clearing price; their tools move costs, credit, and permitted supply, and they observe the price response rather than dictate it.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, planning_and_monetary_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Model formation, estimate elasticities, and test whether prices track rents, incomes, and construction costs. They publish the evidence the policy debate consumes and sit outside the exchange itself — neither buying nor selling the housing being priced.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, housing_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__naturalist_reading, diffuse).
narrative_ontology:fixing_cost_class(price_formation_kernel__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates millions of dispersed, local observations of scarcity and preference into a single public statistic — the price — that lets decentralized decision-makers coordinate a heterogeneous, location-fixed good without any central information-holder.
% TRANSFER_FUNCTION: Moves purchasing power from buyers to sellers in exchange for housing services, and allocates the physical stock to the highest-valuing bidders; no party designs or intercepts the transfer.
% ABSENT_VOICES: Tenant organizers, heterodox economists, and prospective residents priced out of the market would object — several sit outside the disciplinary table where this reading's consensus is produced. Their objection targets the frame's closure rather than any finding inside it: 'objective preference' is measured over effective demand, which existing money has already sorted. No seat in this story's set argues the rival positions; the sibling files in the kernel family supply those seats.
% DISAPPEARANCE_RATIONALE: Overnight disappearance of price formation removes the only known mechanism that aggregates local scarcity and preference signals into actionable statistics. Allocation would reorganize around queues, administrative rationing, or lottery; the recorded experience of administered housing systems — chronic shortages, mismatched assignments, grey markets — indicates a severe and persistent rearrangement, not a transitional one.
% FOUNDING_PROBLEM: Strictly none: this reading holds the arrangement was not built but emerged with exchange itself. Functionally, the problem it solves is ancient — how a society allocates a scarce, heterogeneous, location-fixed good across millions of decision-makers who each hold only local knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Hayek's 1945 statement of the dispersed-knowledge problem attests it from within the tradition. Corroboration from outside any beneficiary set — and this reading declares no beneficiary set — comes from comparative economic history: Kornai's analysis of shortage economies documents what allocation looks like when the price mechanism is suppressed, authored by a critic of market arrangements. The corroboration straddles the kernel contest: Kornai attests the coordination function while attributing shortage to institutional structure rather than nature, supporting the function while disputing the naturalist gloss.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.05, 'stealth/ox-alpha', 'none', direct).

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
 *   Authored from the naturalist seat over the fixed referent of standing market price formation in housing, grid points being decades from 1946 (t=0) to 2026 (t=80). Extractiveness is near zero because the reading holds no agent captures the process: prices are payments at mutually ranked terms and the surplus diffuses to both sides of each exchange. Suppression is low (0.08) — participation is formally voluntary, and a budget constraint is not coercion; suppression is authored as a raw structural property and is not scaled by anything in this story. Theater is negligible: every transaction performs its function. Accessibility collapse is high (0.88): once formation is understood as equilibrium discovery, administered-price alternatives collapse into shortages and deadweight loss, leaving no workable rival aggregator of dispersed knowledge. Resistance (0.32) honestly exceeds the pure-mountain profile: housing politics actively contests price levels through rent regulation, tenant movements, and affordability campaigns, and although this reading classifies such resistance as misdirected at symptoms — the process reasserts through queues and suppressed supply — the resistance is descriptively real and is authored as such. Claim and metrics are independently authored: the mountain claim is the reading's structural assertion; the metrics are its descriptive assessment, and any divergence the engine computes is the datum. The single visible disturbance in the series is the mid-2000s episode (t=60), where even equilibrium-leaning assessment conceded temporary price-fundamental froth before reversion. No suppression_requirement series is authored: the enforcement picture (professional gatekeeping, contract law) is static across the interval, so the scalar carries it.
 *
 * PERSPECTIVAL GAP:
 *   From the household seat, a clearing price that excludes them is experienced as hostility and reads naturally as someone's gain; from the landlord seat the same price is ordinary revenue; from the analytical seat it is information. The engine should compute the payer seats' classification far from the administrator-free natural process this reading claims — the gap between the no-capturer assertion and the participants' lived asymmetry is precisely what the sibling stories exist to test.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because this reading asserts there are none to declare: no agent is positioned to benefit from constructing the process or to be victimized by it. Participants cluster near symmetric directionality — buyers pay below their reservation valuations, sellers receive above theirs — so derived d sits near 0.5 and effective extraction stays near the coordination-cost floor (the resource_allocation floor of 0.15 comfortably books the authored 0.05). The absence of beneficiary/victim declarations is substantive, not an omission: declaring them would presuppose the institutional reading's answer to the kernel contest. Whatever directionality the engine derives for the planning authorities reflects their leverage over inputs — zoning, credit, tax — which this reading books as shifts of the curves, not administration of the price itself. Receipt surface: gain_flow is authored 'diffuse' affirmatively — every named seat was re-checked and none captures the process's gains; surplus disperses across transacting parties, which is the reading's central no-capturer assertion rendered on the receipt surface. fixing_cost is 'prohibitive': removing or replacing formation requires a substitute aggregator of dispersed knowledge that does not exist at scale, and every recorded substitute performed worse by this reading's own lights.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy lens barely grips: the arrangement was not founded, so no mandate can outlive its function. The founding coordination problem — aggregating dispersed knowledge about scarcity and preference — remains live, and the reading predicts it remains live under any successor arrangement. The classification risk this story guards against is the mirror image of the usual mandatrophy error: mislabeling genuine, irreplaceable coordination as extraction because its outputs are unequal. If the founding problem were ever dead — if some non-price mechanism achieved equivalent aggregation — the reading itself would dissolve; nothing in the current record supports that.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (naturalist_reading) of the price_formation_kernel; what structural features would the sibling readings (institutional, georgist, financialization) add or remove?',
    'Cross-reading comparison of authored epsilon, beneficiary, and victim structures over the shared referent of standing housing price formation, plus convergence of policy natural experiments on one reading''s predictions.',
    'If the institutional reading''s constructions verify as constitutive, this reading''s epsilon rises sharply and its type migrates from mountain toward tangled_rope or snare; if the georgist decomposition verifies, a separable unearned-rent component appears that this reading currently books as ordinary scarcity price.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of four rival readings of the price-formation kernel.').

omega_variable(
    natural_law_or_constructed_arrangement,
    'Is housing price formation a genuine law-like natural process, or a constructed arrangement whose apparent naturality benefits identifiable agents (incumbent owners, lenders, landholders)?',
    'Natural experiments: abrupt zoning liberalization, rent-control imposition or repeal, sudden credit tightening. If prices re-equilibrate to fundamentals after rule shocks, formation behaves law-like; if rules permanently bend price levels, construction is constitutive.',
    'A law-like verdict certifies the mountain claim; a constructed verdict triggers false-summit detection and reclassification with named beneficiaries and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_constructed_arrangement, empirical, 'The naturality ambiguity: discovered equilibrium versus constructed arrangement benefiting identifiable agents.').

omega_variable(
    fundamentals_tracking_fidelity,
    'Do observed housing prices track objective scarcity and preference closely enough to sustain the discovery claim, or do price-to-rent and price-to-income ratios show persistent, systematic divergence?',
    'Long-run panel analysis of price, rent, income, and construction-cost series across metropolitan areas; event studies around credit-supply shocks.',
    'Sustained divergence substantiates the axiom_overriding drift already authored and shifts weight toward the financialization sibling; tight tracking supports the reference frame as written.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamentals_tracking_fidelity, empirical, 'Whether prices empirically track fundamentals as the reading requires.').

omega_variable(
    preference_objectivity_boundary,
    'Does ''objective preference'' in this reading include investor and financial demand, or only end-use demand for shelter?',
    'Conceptual analysis within the reading''s own texts, combined with decomposition of purchases into owner-occupier versus investor shares over the cycle.',
    'If financial demand counts as preference, financialization evidence is absorbed and the reading survives intact; if only use-demand counts, the financialization sibling''s core premise competes directly and the foreclosure edge between them hardens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preference_objectivity_boundary, conceptual, 'Locates the precise fault line between this reading and the financialization sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pfnat_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(pfnat_tr_t0, observed).
narrative_ontology:measurement(pfnat_tr_t10, price_formation_kernel__naturalist_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement_basis(pfnat_tr_t10, observed).
narrative_ontology:measurement(pfnat_tr_t20, price_formation_kernel__naturalist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(pfnat_tr_t20, observed).
narrative_ontology:measurement(pfnat_tr_t30, price_formation_kernel__naturalist_reading, theater_ratio, 30, 0.04).
narrative_ontology:measurement_basis(pfnat_tr_t30, observed).
narrative_ontology:measurement(pfnat_tr_t40, price_formation_kernel__naturalist_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement_basis(pfnat_tr_t40, observed).
narrative_ontology:measurement(pfnat_tr_t50, price_formation_kernel__naturalist_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement_basis(pfnat_tr_t50, observed).
narrative_ontology:measurement(pfnat_tr_t60, price_formation_kernel__naturalist_reading, theater_ratio, 60, 0.07).
narrative_ontology:measurement_basis(pfnat_tr_t60, observed).
narrative_ontology:measurement(pfnat_tr_t70, price_formation_kernel__naturalist_reading, theater_ratio, 70, 0.05).
narrative_ontology:measurement_basis(pfnat_tr_t70, observed).
narrative_ontology:measurement(pfnat_tr_t80, price_formation_kernel__naturalist_reading, theater_ratio, 80, 0.04).
narrative_ontology:measurement_basis(pfnat_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(pfnat_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(pfnat_be_t0, observed).
narrative_ontology:measurement(pfnat_be_t10, price_formation_kernel__naturalist_reading, base_extractiveness, 10, 0.05).
narrative_ontology:measurement_basis(pfnat_be_t10, observed).
narrative_ontology:measurement(pfnat_be_t20, price_formation_kernel__naturalist_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement_basis(pfnat_be_t20, observed).
narrative_ontology:measurement(pfnat_be_t30, price_formation_kernel__naturalist_reading, base_extractiveness, 30, 0.05).
narrative_ontology:measurement_basis(pfnat_be_t30, observed).
narrative_ontology:measurement(pfnat_be_t40, price_formation_kernel__naturalist_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement_basis(pfnat_be_t40, observed).
narrative_ontology:measurement(pfnat_be_t50, price_formation_kernel__naturalist_reading, base_extractiveness, 50, 0.06).
narrative_ontology:measurement_basis(pfnat_be_t50, observed).
narrative_ontology:measurement(pfnat_be_t60, price_formation_kernel__naturalist_reading, base_extractiveness, 60, 0.1).
narrative_ontology:measurement_basis(pfnat_be_t60, observed).
narrative_ontology:measurement(pfnat_be_t70, price_formation_kernel__naturalist_reading, base_extractiveness, 70, 0.06).
narrative_ontology:measurement_basis(pfnat_be_t70, observed).
narrative_ontology:measurement(pfnat_be_t80, price_formation_kernel__naturalist_reading, base_extractiveness, 80, 0.05).
narrative_ontology:measurement_basis(pfnat_be_t80, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(price_formation_kernel__naturalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__naturalist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'how housing prices form' covers four structurally distinct claims and is decomposed per the epsilon-invariance principle into a four-story constraint family sharing the referent of standing market price formation in housing. Each member authors its own epsilon over that fixed referent: this naturalist reading authors ~0.05 (discovery, no capturer, no declared parties); the institutional reading authors high epsilon (construction by zoning, lending standards, tax treatment, and platforms, with identifiable beneficiaries and victims); the georgist reading authors intermediate epsilon (legitimate improvement returns plus a contested unearned land-rent component); the financialization reading authors high epsilon (credit-driven feedback extracting from end-users). The upstream/downstream structure runs from this reading outward: its equilibrium framework is the baseline each sibling defines itself against, and its policy doctrine (non-intervention) is what each sibling's remedies modify. This file instantiates only the naturalist reading; the siblings are separate constraints, not hedges inside this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
