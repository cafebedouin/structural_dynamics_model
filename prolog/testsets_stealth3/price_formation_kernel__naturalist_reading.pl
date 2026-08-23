% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Natural-Equilibrium Price Formation (Naturalist Reading)
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   Under the naturalist reading of price formation, market prices are
 *   discovered, not made: they are the equilibrium points at which objective
 *   scarcity meets registered preference, aggregated by decentralized
 *   exchange. Nobody sets the price of housing any more than anybody sets the
 *   weather; zoning boards, lenders, and platforms appear on this reading
 *   only as background conditions, the way terrain appears in hydrology.
 *   Because the process is natural, it has no parties: no agenda-setter
 *   administers it, no beneficiary collects from it, no victim bears an
 *   imposed cost — the only seat this story names is an analytical observer
 *   testing the account against the record. The claim and the metrics are
 *   authored independently and happen to cohere: the reading's claim is
 *   mountain, and the metrics describe a natural-law profile (negligible
 *   extraction, negligible suppression, near-zero theater, high accessibility
 *   collapse, low resistance). The substantive measurement this story
 *   contributes is cross-reading: the omega variables and network edges carry
 *   the committer structure — what sibling readings of the same kernel would
 *   change — without importing that contest into this constraint's
 *   classification.
 *
 * KEY AGENTS:
 *   - comparative_market_analysts: Analytical observer (analytical/analytical) — tests the discovery account against the empirical record; collects nothing and pays nothing under the arrangement
 *   - No further agents are named: under this reading no party sets, administers, enforces, or collects from price formation — the absence of parties is the reading's substantive claim, not an authoring omission.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.07).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.04).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Natural-Equilibrium Price Formation (Naturalist Reading)").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca').
narrative_ontology:cs_kernel_codification('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', formalized).
narrative_ontology:cs_authority_grounding('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', expertise).
narrative_ontology:cs_interpretation_layer_present('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca').
narrative_ontology:cs_reading_relation('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', price_formation_kernel__georgist_reading, influences).
narrative_ontology:cs_reading_relation('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', foundational, prices_are_discovered_not_constructed).
narrative_ontology:cs_axiom_status(prices_are_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', prices_are_discovered_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', secondary, intervention_creates_deadweight_loss).
narrative_ontology:cs_axiom_status(intervention_creates_deadweight_loss, holdable).
narrative_ontology:cs_axiom_grounding('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', intervention_creates_deadweight_loss, instrumental).
narrative_ontology:cs_reference_frame('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', competitive_equilibrium_price_discovery).
narrative_ontology:cs_drift_state('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', contemporary_housing_market_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fc0d50e5-f7e9-4492-a814-f0d49b0cd3ca', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, hayekian_spontaneous_order).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, consumer_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study how prices form across commodities, housing markets, and asset classes; test whether observed prices track measures of physical scarcity and stated preference; advise governments weighing intervention. They collect no revenue from the price mechanism and bear none of its costs; their stake is epistemic — whether the discovery account or some rival account better describes the record.
narrative_ontology:constraint_stakeholder(price_formation_kernel__naturalist_reading, comparative_market_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates radically distributed knowledge about physical scarcity and consumer preference into a single public signal — price — that coordinates production, allocation, and investment across millions of decoupled decision-makers without central direction or common knowledge.
% TRANSFER_FUNCTION: Moves goods, services, and factors of production toward their highest-valued registered uses, and moves purchasing power from buyers to sellers in voluntary exchange. Under this reading no transfer is coercive: every movement of value executes mutually accepted terms, and the price level itself transfers nothing — it informs.
% ABSENT_VOICES: Households unable to register willingness-to-pay (priced-out renters, would-be owners), holders of non-marketed values (community stability, neighborhood character), and future generations have no voice in the price signal. Under this reading their silence is read as accurate preference revelation — the signal correctly reports that their demand, at current endowments, does not clear — rather than as exclusion from the conversation.
% DISAPPEARANCE_RATIONALE: If natural price formation ceased overnight — if scarcity and preference stopped aggregating into equilibrium signals — economic calculation would fail at scale: producers would lack the information to choose what and how much to produce, exchange would revert to costly barter or administrative rationing, and the entire division of labor would reorganize around whatever substitute coordination mechanism emerged. The reading holds the price system to be load-bearing infrastructure, not decoration.
% FOUNDING_PROBLEM: How to coordinate production and allocation among strangers whose knowledge of scarcity and preference is dispersed, tacit, and inaccessible to any central planner — the economic calculation problem that barter and command arrangements solve badly.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside any benefiting party (this reading identifies none): the price-theory tradition from Walras and Marshall through Hayek documents the coordination function, and the empirical record of comprehensive price controls and planned-economy shortages corroborates that the problem is real and poorly solved by substitutes. No party profits from this attestation under the reading's own account, since the reading identifies no beneficiary set.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.07: even on the discovery account, exchange carries frictional cost (search, spreads, intermediation), which the reading classifies as coordination cost rather than extraction — comparable to the inherent-cost floor of a resource-allocation mechanism. Suppression is 0.04: budget constraints bind hard, but on this reading a binding budget is scarcity registering itself, not coercion administered by the constraint; nothing forbids alternatives. Theater is 0.03: a natural process performs nothing; there is no maintenance activity to be theatrical about. Accessibility collapse is 0.88 — the mountain signature: once an agent understands that price reports scarcity-times-preference, alternatives to accepting the signal collapse (arbitrage erodes persistent deviation; the budget constraint is not negotiable). Resistance is 0.08: episodic moral outrage at prices (gouging, rent spikes) occurs but produces no sustained effective counter-mechanism. The measurement series run on one shared grid (t=0..30, step 5) and are intentionally flat with noise: a genuine natural law does not drift, and a drifting 'law' would be evidence for the constructed alternative — which is exactly what the omega battery is positioned to detect.
 *
 * PERSPECTIVAL GAP:
 *   By design this story has almost no seat divergence: the reading's defining assertion is that no structural asymmetry exists to diverge from. The only authored seat is the analytical observer, whose computed classification should match the story-level mountain profile. The perspectival action for this kernel lives between files, not between seats: the payer and beneficiary seats that sibling readings will author are precisely what this reading denies exist. Per-seat computation on this story therefore functions as a null control for the constraint family.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries and no victims are declared, so the directionality derivation chain has no structural data to read and any seat reverts to its canonical fallback; the observer's epistemic-only relation sits near symmetric (d approximately 0.5). This absence is the substantive claim, not an authoring shortcut: the reading asserts that nobody stands in an extracting relation to price formation, because extraction presupposes a constructor and the reading denies there is one. Effective extraction is correspondingly damped for every seat, and scope amplification is moot at epsilon near 0.07.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating dispersed knowledge under scarcity — is constitutive and permanent, so the arrangement cannot outlive its function and no mandatrophy is declared. The classification discipline runs in both directions here: if this reading is correct, interventionist framings mistake irreducible coordination cost for removable extraction, and labeling the price mechanism a snare would license remedies that destroy the coordination function; if the reading is wrong, this story's flat metrics and no-party structure are the artifact to be caught — which is what the natural_law_or_naturalized_arrangement omega and the cross-reading network edges are built to expose. The story keeps the claim falsifiable inside the apparatus rather than adjudicated by it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is the naturalist_reading of the price_formation_kernel — the claim that observed prices are discovered equilibria of objective scarcity and preference. How would this constraint''s structure change under a sibling reading of the same kernel?',
    'Cross-reading compilation: author the institutional_reading, georgist_reading, and financialization_reading stories over the same standing arrangement and compare beneficiary/victim structures, epsilon, and computed types. Convergence of three independent readings on extracted structure where this reading finds natural equilibrium would indicate the mountain classification is an artifact of this reading''s framing.',
    'If sibling readings consistently identify constructed structure (institutional rules, unearned land rent, credit feedback) where this story finds a party-free natural process, this file''s mountain classification is reading-relative rather than a property of the arrangement; if siblings also compute low-extraction profiles, the mountain claim is robust across the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: this story is one reading of a contested kernel; its classification may be reading-indexed.').

omega_variable(
    objective_scarcity_vs_constructed_supply,
    'Where the kernel disagreement is located: are observed housing prices determined by objective scarcity and registered preference, or is the ''scarcity'' this reading treats as given partly produced by background rules (zoning, lending standards, tax treatment) that the reading holds exogenous?',
    'Identical-demand natural experiments across jurisdictions differing primarily in supply-side rules: if prices track rule variation while geography and preferences are held constant, scarcity is endogenous to the background conditions and the discovery premise fails for housing specifically.',
    'If scarcity is institutionally produced, the reading''s core premise (discovery, not construction) fails in its hardest test domain and the arrangement''s classification shifts away from mountain; if scarcity is genuinely physical and geographic, the mountain claim stands and the policy deadweight-loss axiom gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_scarcity_vs_constructed_supply, empirical, 'Whether the scarcity term in the reading''s account is objective or itself an output of background rules.').

omega_variable(
    natural_law_or_naturalized_arrangement,
    'Is the apparent naturality of price formation a property of reality (like gravity, requiring no defense) or a naturalized description of a contingent arrangement that identifiable actors benefit from presenting as inevitable?',
    'Maintenance-activity test: genuine natural laws require no propagation. Trace whether the naturalist account is actively defended — funded research programs, lobbying against intervention, editorial framing of price criticism as economic illiteracy — and whether defense intensity correlates with positions that collect from non-intervention.',
    'Substantial, correlated maintenance activity would indicate a false summit — a constructed constraint wearing mountain form — triggering false-summit evaluation despite this story''s no-beneficiary declaration; persistence without defense would strengthen the mountain certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_naturalized_arrangement, empirical, 'Naturality test: defended doctrine versus undefended natural law.').

omega_variable(
    deadweight_loss_universality,
    'Does the reading''s policy axiom — that intervening in formed prices creates net deadweight loss — hold across elasticity regimes and market structures, or only under competitive-clearing assumptions with elastic supply?',
    'Meta-analysis of intervention outcomes segmented by supply elasticity and ownership concentration; housing markets with highly inelastic supply and concentrated ownership are the critical test case.',
    'If the deadweight-loss result fails in inelastic-supply domains, the reading''s policy injunction loses generality and its grip on policy discourse weakens; if it holds broadly, the mountain classification and the reading''s practical authority are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadweight_loss_universality, empirical, 'Generality of the reading''s foundational policy claim across market structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(pric_tr_t5, price_formation_kernel__naturalist_reading, theater_ratio, 5, 0.03).
narrative_ontology:measurement(pric_tr_t10, price_formation_kernel__naturalist_reading, theater_ratio, 10, 0.02).
narrative_ontology:measurement(pric_tr_t15, price_formation_kernel__naturalist_reading, theater_ratio, 15, 0.03).
narrative_ontology:measurement(pric_tr_t20, price_formation_kernel__naturalist_reading, theater_ratio, 20, 0.03).
narrative_ontology:measurement(pric_tr_t25, price_formation_kernel__naturalist_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement(pric_tr_t30, price_formation_kernel__naturalist_reading, theater_ratio, 30, 0.03).

% Extraction over time
narrative_ontology:measurement(pric_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(pric_be_t5, price_formation_kernel__naturalist_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(pric_be_t10, price_formation_kernel__naturalist_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(pric_be_t15, price_formation_kernel__naturalist_reading, base_extractiveness, 15, 0.07).
narrative_ontology:measurement(pric_be_t20, price_formation_kernel__naturalist_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(pric_be_t25, price_formation_kernel__naturalist_reading, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(pric_be_t30, price_formation_kernel__naturalist_reading, base_extractiveness, 30, 0.08).

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
% The colloquial label 'price formation' decomposes, per the epsilon-invariance principle, into four structurally distinct claims about the same observable (observed market prices). This file instantiates the naturalist reading alone: one reading, one constraint, one epsilon (0.07, assessed by the reading's own lights over the standing arrangement — never over the arrangements sibling readings would endorse). Each sibling file authors its own epsilon over the same referent; cross-reading divergence in computed type is the measurement the family exists to take. Direction of pressure: the naturalist reading is the discursive default and therefore exerts legitimacy pressure on the sibling readings (see cs_structure.reading_relations), while the siblings supply the counter-evidence driving this reading's axiom_overriding drift state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
