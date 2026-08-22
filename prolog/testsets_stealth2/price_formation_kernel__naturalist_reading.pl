% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Naturalist Reading: Price Formation as Equilibrium Discovery
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This file instantiates the naturalist_reading of the
 *   price_formation_kernel for housing markets: the claim that price
 *   formation is a natural equilibrium process reflecting objective scarcity
 *   and preference — discovered, not constructed — with policy intervention
 *   producing deadweight loss. Per the kernel-reading epsilon rule, the
 *   referent of epsilon is the standing arrangement under contest
 *   (decentralized market formation of housing prices), assessed by this
 *   reading's own lights: the reading finds the arrangement non-extractive,
 *   so epsilon is authored near zero. Following Rule 1, the contest is NOT
 *   described inside this constraint: no sibling-reading content, no averaged
 *   epsilon, no imported beneficiary/victim structure. The stakeholder
 *   surface is legitimately omitted under the mountain exemption because this
 *   reading declares no parties — nobody constructs the process, nobody
 *   administers it, nobody collects from it. The false-summit question (does
 *   the frame conceal identifiable beneficiaries?) is routed to an omega
 *   variable rather than answered by declaring beneficiaries here, which
 *   would collapse this reading into its rivals. Claim/metric independence
 *   holds: claimed_type is mountain from the reading's seat; the metrics are
 *   authored as descriptively true of the arrangement as this reading
 *   assesses it. Sibling readings (institutional, georgist, financialization)
 *   are separate constraint files linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - No beneficiary seat exists under this reading: sellers receive payment equal to marginal valuation at exchange, which the frame counts as exchange rather than collection; no agent is declared in base_properties.beneficiaries.
 *   - No victim seat exists under this reading: no group bears asymmetric costs; those priced out of a metro register, in this frame, as having revealed low preference rather than as bearing extraction; no agent is declared in base_properties.victims.
 *   - Analytical seat only: the economics profession holds the frame's authority (expertise-grounded, peer-reviewed), but observes and formalizes rather than administers — the frame's substantive claim is that no administrator exists at all.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.05).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.03).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Naturalist Reading: Price Formation as Equilibrium Discovery").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, '107a3391-9a28-4800-ab23-2096edfb9402').
narrative_ontology:cs_kernel_codification('107a3391-9a28-4800-ab23-2096edfb9402', formalized).
narrative_ontology:cs_authority_grounding('107a3391-9a28-4800-ab23-2096edfb9402', expertise).
narrative_ontology:cs_interpretation_layer_present('107a3391-9a28-4800-ab23-2096edfb9402').
narrative_ontology:cs_reading_relation('107a3391-9a28-4800-ab23-2096edfb9402', price_formation_kernel__institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('107a3391-9a28-4800-ab23-2096edfb9402', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('107a3391-9a28-4800-ab23-2096edfb9402', price_formation_kernel__financialization_reading, coexists_with).
narrative_ontology:cs_axiom('107a3391-9a28-4800-ab23-2096edfb9402', foundational, prices_are_discovered_not_constructed).
narrative_ontology:cs_axiom_status(prices_are_discovered_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('107a3391-9a28-4800-ab23-2096edfb9402', prices_are_discovered_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('107a3391-9a28-4800-ab23-2096edfb9402', secondary, interventions_create_deadweight_loss).
narrative_ontology:cs_axiom_status(interventions_create_deadweight_loss, holdable).
narrative_ontology:cs_axiom_grounding('107a3391-9a28-4800-ab23-2096edfb9402', interventions_create_deadweight_loss, instrumental).
narrative_ontology:cs_reference_frame('107a3391-9a28-4800-ab23-2096edfb9402', natural_equilibrium_price_discovery).
narrative_ontology:cs_drift_state('107a3391-9a28-4800-ab23-2096edfb9402', contemporary_housing_affordability_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('107a3391-9a28-4800-ab23-2096edfb9402', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, hayekian_dispersed_knowledge_thesis).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, walrasian_auctioneer_model).
narrative_ontology:constraint_vindicates(price_formation_kernel__naturalist_reading, deadweight_loss_theorem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dispersed-knowledge allocation problem: millions of decentralized decisions about what to build, where, at what density, and for whom are coordinated by a single summary statistic — price — that aggregates scarcity and preference without any central calculator.
% TRANSFER_FUNCTION: Moves purchasing power from buyers to sellers in exchange for housing services and assets, and allocates access to fixed locations by willingness-to-pay; under this reading the movement is exchange at mutual valuation rather than transfer from a loser to a winner.
% ABSENT_VOICES: The frame claims no excluded voices — it presents itself as the view from nowhere. The methodological exclusion is nonetheless real: preference is measured as effective demand, so prospective residents unable to bid register as low preference rather than as excluded parties, and their counter-claim (that willingness-to-pay conflates capacity with preference) has no seat inside the frame's own apparatus. That absence is commentary-grade here; the institutional sibling file hosts the excluded-renter seat where it becomes structural.
% DISAPPEARANCE_RATIONALE: If prices ceased to aggregate scarcity and preference overnight, every allocation decision in housing — what gets built, where, for whom, at what density — would need replacement machinery: administrative rationing, queues, or lottery. Waiting lists for dwellings, informal premium payments, and misallocation of land to low-value uses would appear immediately. The world rearranges because the coordination function is load-bearing for the entire built environment.
% FOUNDING_PROBLEM: How to coordinate the production and allocation of housing across millions of dispersed actors holding local knowledge, without central calculation of need, scarcity, or preference.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the frame's own tradition: the socialist-calculation literature — including its socialist participants — conceded the coordination power of prices; institutional economists grant that prices aggregate dispersed information while disputing what else they do; planning theorists design around price signals rather than against them. The problem's liveness requires no testimony from within the naturalist school — though under this reading there is no benefiting party whose testimony would be suspect in any case.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.05: under this reading the price system transfers value through exchange at mutual valuation, leaving only transaction frictions — search, brokerage, listing fees — which the reading books as coordination cost rather than extraction; the figure sits below the resource_allocation Boltzmann floor deliberately, since the reading claims no excess above coordination cost. Suppression is 0.03: a natural process is not enforced on anyone; the frame's journal-and-policy defense of itself is argument aimed at rival interpretations, not coercive force applied to participants. Theater is 0.03: price formation is not performed, and the small late-interval rise traces the formalization culture of equilibrium modeling — elegance performing naturality — not functional maintenance. Accessibility collapse is 0.88: scarcity cannot be opted out of, and every alternative allocation mechanism tried at scale (administrative rationing, queues, lotteries) either reintroduces prices informally or produces visible dysfunction that the reading counts as confirmation. Resistance is 0.22: rent regulation and tenant movements persistently resist price LEVELS, but the frame itself is contested mainly at the disciplinary margins, and much practical resistance accepts the frame while disputing parameter values. The measurement series are near-flat as a matter of honesty, not neglect: a mountain does not drift, and the small variations trace the profession's modeling culture rather than functional change. All three tracked metrics share one six-point time grid (1950, 1965, 1980, 1995, 2010, 2025).
 *
 * PERSPECTIVAL GAP:
 *   Inside this file there are no seats to diverge — that absence IS the reading's substantive claim of systemic symmetry. The real perspectival gap lives between files: from the institutional reading's seats, the same price phenomena show agenda-setters (zoning boards, lending regulators, listing platforms) and trapped payers (excluded renters); from the georgist seat, land-rent recipients collect without producing; from the financialization seat, credit-cycle timing separates winners from losers. This file deliberately hosts none of those seats; the engine's cross-file comparison is where the gap gets measured. If the naturalist frame broke for a participant — a tenant who stops seeing rent as signal and starts seeing it as toll — that participant's computed classification would flip, but the seat belongs to the sibling files, not to this one.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations exist, so the derivation chain has no structural data to read: every index falls back to symmetry, d is uniform-or-undefined, chi approximates epsilon for any seat, and scope amplification is inert (global scope raises verification difficulty, but there is no extraction to verify). No directionality overrides are authored because there is nothing to override. This is exactly the profile a genuine mountain should produce — and exactly the profile a successful false summit would mimic, which is why the natural-law-versus-settlement omega carries the diagnostic weight that beneficiary declarations would otherwise carry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating dispersed housing knowledge without central calculation) is live, so obsolescence-driven mandatrophy does not threaten this constraint. The classification risk runs in both directions: mislabeling a genuine coordination mechanism as pure extraction (the hazard the georgist and institutional readings must avoid) or mislabeling a distributive settlement as natural law (the false-summit hazard this file must avoid). Keeping this reading as a clean, party-less mountain preserves the contrast class that makes both errors detectable: if the sibling files show concentrated beneficiaries and trapped payers under the same price phenomena, the divergence locates what the naturalist frame conceals; if they do not, the mountain claim survives contact with its rivals. The epsilon-invariance discipline is what makes the comparison meaningful — one reading, one epsilon, no imported contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_membership_naturalist_reading,
    'This constraint is one reading of the price_formation_kernel: what structural differences would the sibling readings (institutional, georgist, financialization) introduce, and where exactly is the disagreement located?',
    'Comparative classification across the four sibling story files: each instantiates the same kernel with its own epsilon, beneficiary/victim structure, and type; the disagreement''s location is read off which structural element differs first (locus of determination, composition of price, driver of demand).',
    'If the institutional reading is adopted, this constraint gains beneficiaries (incumbent owners, restrictive-jurisdiction municipalities) and victims (excluded renters) and reclassifies away from mountain; if the georgist reading is adopted, the affected class splits into land-rent payers versus improvement traders; if the financialization reading is adopted, extraction tracks credit cycles instead of remaining constant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_membership_naturalist_reading, conceptual, 'Committer structure: this story instantiates the naturalist reading of the price-formation kernel; sibling readings are separate constraints, not parts of this one.').

omega_variable(
    natural_law_vs_self_confirming_settlement,
    'Is the cross-jurisdictional stability of market price formation evidence of a natural law, or of a self-confirming institutional settlement that concentrates asset-appreciation gains with incumbent holders while presenting the outcome as nature?',
    'Compare price behavior across jurisdictions with radically different institutional arrangements (zoning regimes, tenure structures, tax treatment): identical behavior across arrangements supports naturalness; systematic divergence tracking institutions supports construction with identifiable beneficiaries.',
    'If constructed-with-beneficiaries, the false-summit signature fires: beneficiaries become authorable, epsilon rises, and the constraint reclassifies toward tangled_rope; if natural, the mountain claim stands and the sibling readings over-describe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_self_confirming_settlement, empirical, 'Whether the naturalist frame describes law or launders a distributive settlement.').

omega_variable(
    auxiliary_hypothesis_absorption,
    'Are the reading''s auxiliary hypotheses (frictionless adjustment, representative-agent rationality, exogenous preferences) structured so that every anomaly — bubbles, shortages, persistent mispricing — is absorbed as ''distortion'' rather than admitted as counterevidence?',
    'Lakatos-style progressive-versus-degenerating program analysis: track whether the research program yields novel successful predictions or whether the protective belt of auxiliary assumptions grows with each anomaly.',
    'If degenerating, the constraint functions as legitimation rather than description, raising its effective theater_ratio and eroding the mountain claim from within the reading''s own epistemics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(auxiliary_hypothesis_absorption, conceptual, 'Falsifiability structure of the naturalist research program.').

omega_variable(
    land_commodity_scope_generalization,
    'Does the equilibrium-discovery account hold for housing and land specifically — fixed supply, heterogeneous, location-bound, appreciating — or only for the reproducible commodities where it was developed?',
    'Econometric comparison of fundamental-value tracking (hedonic models, rent-price ratios) in housing versus manufactured-goods markets; test whether convergence-to-fundamental occurs in housing at commodity-market speeds.',
    'If housing-specific failure, the constraint narrows to a domain-limited regularity and the universal mountain claim loses scope; the georgist and financialization readings gain their strongest foothold precisely on land.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(land_commodity_scope_generalization, empirical, 'Scope limit of the naturalist account on fixed, heterogeneous assets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pric_tr_t1950, price_formation_kernel__naturalist_reading, theater_ratio, 1950, 0.02).
narrative_ontology:measurement_basis(pric_tr_t1950, observed).
narrative_ontology:measurement(pric_tr_t1965, price_formation_kernel__naturalist_reading, theater_ratio, 1965, 0.02).
narrative_ontology:measurement_basis(pric_tr_t1965, observed).
narrative_ontology:measurement(pric_tr_t1980, price_formation_kernel__naturalist_reading, theater_ratio, 1980, 0.02).
narrative_ontology:measurement_basis(pric_tr_t1980, observed).
narrative_ontology:measurement(pric_tr_t1995, price_formation_kernel__naturalist_reading, theater_ratio, 1995, 0.03).
narrative_ontology:measurement_basis(pric_tr_t1995, observed).
narrative_ontology:measurement(pric_tr_t2010, price_formation_kernel__naturalist_reading, theater_ratio, 2010, 0.03).
narrative_ontology:measurement_basis(pric_tr_t2010, observed).
narrative_ontology:measurement(pric_tr_t2025, price_formation_kernel__naturalist_reading, theater_ratio, 2025, 0.03).
narrative_ontology:measurement_basis(pric_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(pric_be_t1950, price_formation_kernel__naturalist_reading, base_extractiveness, 1950, 0.04).
narrative_ontology:measurement_basis(pric_be_t1950, observed).
narrative_ontology:measurement(pric_be_t1965, price_formation_kernel__naturalist_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement_basis(pric_be_t1965, observed).
narrative_ontology:measurement(pric_be_t1980, price_formation_kernel__naturalist_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement_basis(pric_be_t1980, observed).
narrative_ontology:measurement(pric_be_t1995, price_formation_kernel__naturalist_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement_basis(pric_be_t1995, observed).
narrative_ontology:measurement(pric_be_t2010, price_formation_kernel__naturalist_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement_basis(pric_be_t2010, observed).
narrative_ontology:measurement(pric_be_t2025, price_formation_kernel__naturalist_reading, base_extractiveness, 2025, 0.05).
narrative_ontology:measurement_basis(pric_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(pric_su_t1950, price_formation_kernel__naturalist_reading, suppression_requirement, 1950, 0.02).
narrative_ontology:measurement_basis(pric_su_t1950, observed).
narrative_ontology:measurement(pric_su_t1965, price_formation_kernel__naturalist_reading, suppression_requirement, 1965, 0.02).
narrative_ontology:measurement_basis(pric_su_t1965, observed).
narrative_ontology:measurement(pric_su_t1980, price_formation_kernel__naturalist_reading, suppression_requirement, 1980, 0.02).
narrative_ontology:measurement_basis(pric_su_t1980, observed).
narrative_ontology:measurement(pric_su_t1995, price_formation_kernel__naturalist_reading, suppression_requirement, 1995, 0.03).
narrative_ontology:measurement_basis(pric_su_t1995, observed).
narrative_ontology:measurement(pric_su_t2010, price_formation_kernel__naturalist_reading, suppression_requirement, 2010, 0.03).
narrative_ontology:measurement_basis(pric_su_t2010, observed).
narrative_ontology:measurement(pric_su_t2025, price_formation_kernel__naturalist_reading, suppression_requirement, 2025, 0.03).
narrative_ontology:measurement_basis(pric_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__naturalist_reading, resource_allocation).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, price_formation_kernel__financialization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how housing prices form' covers four structurally distinct claims that share one kernel. This file is the upstream member: the naturalist frame is the default against which the other three define themselves, and its policy dominance shapes the legitimacy conditions and resource availability of each sibling (hence the network edges). Each sibling gets its own epsilon, its own beneficiary/victim structure, and its own claimed type; the epsilon values differ because the readings disagree about what the standing arrangement IS, not because any single constraint's epsilon is observable-dependent. Sibling constraint_ids follow the kernel-family naming convention used in this file's header.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
