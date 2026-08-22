% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Actively Defended Market Dominance (Beneficiary-Maintained Reading of the Market Naturalization Kernel)
 *   domain: political economy/economic history/institutional analysis
 *
 * SUMMARY:
 *   The kernel 'market naturalization' — the standing presentation of
 *   concentrated market outcomes as natural, meritocratic, and efficient — is
 *   contested across three readings that instantiate structurally different
 *   constraints. This file instantiates the beneficiary_maintained_reading:
 *   the arrangement under contest is the naturalized-dominance regime of the
 *   1980-2020 interval, and this reading assesses it as continuously
 *   reproduced by an identifiable beneficiary class. Incumbent capital
 *   holders fund the political, legal, and intellectual apparatus that
 *   defends existing market structure; public enforcement capacity against
 *   concentration was converted into defense capacity for it; and the
 *   resulting rents — rising markups, falling labor share, suppressed entry —
 *   flow back to the defending class. The epsilon referent is the standing
 *   arrangement itself, assessed by this reading's own lights; it is not the
 *   competitive arrangement this reading would prefer, and no sibling
 *   reading's valuation is averaged in. Sibling files:
 *   market_naturalization__lapsed_alternative_reading (persistence by
 *   inertia, no active maintenance) and market_naturalization__hybrid_reading
 *   (mixed defended/lapsed structure); both are linked in
 *   network.affects_constraints, and the inter-reading differences are
 *   carried in omega variables rather than inside this constraint's
 *   classification. KEY AGENTS (by structural relationship): -
 *   incumbent_capital_holders: Primary beneficiary and agenda setter
 *   (institutional/arbitrage) — funds and directs the defense apparatus,
 *   collects the rents - policy_research_institutes: Secondary beneficiary
 *   (institutional/identity_locked) — produces the legitimating scholarship;
 *   organizational identity fused with the framework -
 *   revolving_door_regulators: Administrator with beneficiary secondary
 *   position (institutional/arbitrage) — runs the enforcement machinery,
 *   collects deferred private rewards - industrial_workers: Primary payer
 *   (organized/trapped) — bears wage-share loss and monopsony suppression -
 *   monopoly_price_paying_consumers: Payer with incidental benefits
 *   (powerless/constrained) — bears markups on essential goods -
 *   small_business_entrants: Payer (moderate/constrained) — faces platform
 *   tolls, predation, forced sale or marginalization -
 *   antitrust_revival_coalition: Excluded voice (organized/constrained) —
 *   structural-remedy advocates outside the agenda-setting rooms -
 *   economic_historians: Analytical observer (analytical/analytical) —
 *   documents the defensive genealogy from outside the policy pipeline
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.8).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.72).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, snare).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Actively Defended Market Dominance (Beneficiary-Maintained Reading of the Market Naturalization Kernel)").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political economy/economic history/institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '6f591c6b-60b8-4062-9604-e70283905cd1').
narrative_ontology:cs_kernel_codification('6f591c6b-60b8-4062-9604-e70283905cd1', distributed).
narrative_ontology:cs_authority_grounding('6f591c6b-60b8-4062-9604-e70283905cd1', extraction).
narrative_ontology:cs_interpretation_layer_present('6f591c6b-60b8-4062-9604-e70283905cd1').
narrative_ontology:cs_reading_relation('6f591c6b-60b8-4062-9604-e70283905cd1', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f591c6b-60b8-4062-9604-e70283905cd1', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('6f591c6b-60b8-4062-9604-e70283905cd1', foundational, market_dominance_requires_active_defense).
narrative_ontology:cs_axiom_status(market_dominance_requires_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('6f591c6b-60b8-4062-9604-e70283905cd1', market_dominance_requires_active_defense, empirically_contingent).
narrative_ontology:cs_axiom('6f591c6b-60b8-4062-9604-e70283905cd1', secondary, naturalization_is_funded_production).
narrative_ontology:cs_axiom_status(naturalization_is_funded_production, holdable).
narrative_ontology:cs_axiom_grounding('6f591c6b-60b8-4062-9604-e70283905cd1', naturalization_is_funded_production, empirically_contingent).
narrative_ontology:cs_reference_frame('6f591c6b-60b8-4062-9604-e70283905cd1', beneficiary_defended_naturalization).
narrative_ontology:cs_drift_state('6f591c6b-60b8-4062-9604-e70283905cd1', contemporary_antitrust_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6f591c6b-60b8-4062-9604-e70283905cd1', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, policy_research_institutes).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, revolving_door_regulators).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, industrial_workers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, monopoly_price_paying_consumers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, small_business_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, monopoly_price_paying_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and control the dominant firms whose market positions the arrangement protects. Fund trade associations, campaign committees, litigation, and research institutes that defend existing market structure; place executives and allies in agencies, courts, and editorial positions. Capital moves freely across jurisdictions and asset classes, so exit from any single national arrangement is available; the class-level arrangement is the one thing they cannot exit without dissolving their advantage. Wealth transmits across generations, giving the class a long horizon and patience with slow institutional work.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary).

% Produce the studies, editorials, curricula, and expert testimony that present existing market outcomes as the product of efficiency and merit. Funded predominantly by the dominant firms and wealthy families whose positions they defend. Donor bases, scholarly reputations, and staff career ladders are all built on the framework; pivoting to a different account of market outcomes would cost them their audience, their funding, and their intellectual identity at once, so none has.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, policy_research_institutes, beneficiary,
    institutional, biographical, identity_locked, national).

% Staff the agencies charged with policing concentration. Across the interval their enforcement posture favored consent decrees, efficiency rebuttals, and non-intervention. Senior officials routinely move to lucrative positions at the firms and firms' counsel they previously oversaw, and junior staff calibrate careers accordingly. They administer the enforcement machinery day to day and personally collect deferred private rewards from the same machinery's restraint.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, revolving_door_regulators, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, revolving_door_regulators, beneficiary).

% Sell labor into markets where employer concentration suppresses wages and limits mobility. Non-compete agreements, occupational licensing webs, and geographic ties to depressed regional labor markets bound their exit. Union density fell steeply across the interval, thinning the organizations that once aggregated their bargaining position. They carry the falling labor share, eroded job security, and the gap between productivity growth and median pay.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, industrial_workers, payer,
    organized, biographical, trapped, national).

% Buy food, pharmaceuticals, connectivity, housing-adjacent services, and retail goods from concentrated markets at markups documented to run well above competitive benchmarks. They receive real convenience, variety, and integration from the dominant firms' products, but for essentials there is no realistic substitute and no individual voice; their remedy channel is collective politics they participate in only diffusely.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, monopoly_price_paying_consumers, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, monopoly_price_paying_consumers, beneficiary).

% Attempt entry into markets dominated by incumbents and meet platform tolls, predatory pricing windows, patent and data thickets, and acquisition-or-marginalization dynamics. The lucky ones are bought out, recycling their capital and ambition back into the incumbent ecosystem; the rest exit or survive at the margins. Their complaints reach policymakers through the same trade-association channels the incumbents fund.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, small_business_entrants, payer,
    moderate, biographical, constrained, national).

% Organizers, progressive prosecutors, heterodox economists, and affected communities pressing for structural remedies: breakups, merger moratoria, public options, interoperability mandates. They command growing public attention and occasional electoral success but sit outside the rooms where enforcement priorities are actually set; their proposals are characterized in mainstream fora as radical departures from sound economics, and their access runs through gatekeepers funded by the other side.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, antitrust_revival_coalition, excluded,
    organized, biographical, constrained, national).

% Study the corporate-liberal origins of the naturalization project: the trade-association campaigns, foundation-funded academic programs, and doctrinal entrepreneurship that built the legitimacy apparatus. Publish findings outside the policy pipeline, with no enforcement lever and no funding dependence on the parties described. Their archive is the main external check on the arrangement's self-account.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem for dispersed capital holders: no single firm can fund systemic defense of market structure alone, and the shared naturalization account aligns their contributions, staffs the institutes and associations, and stabilizes investor expectations that existing property arrangements will be honored. It also gives administrators a workable doctrinal standard for deciding cases.
% TRANSFER_FUNCTION: Moves purchasing power from workers (suppressed wage share and monopsony-depressed pay), consumers (markups above competitive benchmarks), and small suppliers and entrants (platform tolls, payment delays, forced sales) to incumbent shareholders and executives; and moves agenda-setting influence from the general public to capital-funded institutions via campaign finance, funded expertise, and revolving-door placement.
% ABSENT_VOICES: The antitrust revival coalition, independent labor organizers, and heterodox economists would contest the naturalness premise directly and press structural remedies; they are kept out of the agenda-setting rooms by the same funded gatekeeping the arrangement maintains. Affected communities in company towns dominated by single employers have no seat at all. The unanimity of elite consensus on 'sound economics' partly reflects these absences rather than settled agreement.
% DISAPPEARANCE_RATIONALE: If the defense apparatus vanished overnight — the funded institutes went quiet, campaign spending dried up, the revolving door stopped — antitrust enforcement would revive under existing statutes within a few electoral cycles, merger review would tighten, non-compete and organizing restrictions would face active repeal, and entry, wage share, and markup margins would all begin moving. The arrangement's persistence demonstrably depends on continuous expenditure; nothing about it reproduces itself unattended.
% FOUNDING_PROBLEM: Concentrated wealth has faced recurring democratic challenge since the late nineteenth century — populism, labor militancy, trust-busting, socialist parties, and later the regulatory state itself. The naturalization project was built to solve the legitimacy problem of concentration: to recast its outcomes as natural, meritocratic, and efficient, so that redistribution and structural remedy register as interference with a natural order rather than as ordinary politics.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: business and labor historians have documented the defensive origin of the legitimacy apparatus (trade-association campaigns, foundation-funded economics programs, deliberate doctrinal entrepreneurship) using archival sources independent of capital-funded institutions; survey researchers independently document the legitimacy erosion that the escalating defense expenditure responds to. No element of the status assessment relies on the arrangement's own beneficiaries' attestations.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.80 at interval end: the markup and labor-share literatures document a large and widening gap between prices and marginal cost and between productivity and median wages across the interval, tempered by the genuine consumer surplus the dominant firms' products also deliver. Suppression is 0.72: a mix of structural mechanisms (non-compete coverage, right-to-work diffusion, campaign-finance dependence, platform dependency) and ideological mechanisms (meritocratic framing that lowers remedial demand); it is authored as a raw structural property and is not scaled by power or scope — the engine owns that arithmetic. Theater ratio 0.33 and rising: early-interval defense activity was mostly functional (deregulation, merger approval, doctrinal victories in court); a growing later share is performative (commissioned studies manufactured for citation, astroturf comment campaigns, ritualized efficiency testimony) while real policy defense continues alongside. Accessibility_collapse 0.50: the remedial toolkit (antitrust statutes, union law, public options) remains legible and formally available — alternatives have not collapsed the way a natural law's would — but practical access to them is suppressed. Resistance 0.62: recurrent surges (occupy movements, primary campaigns, the antitrust revival) that have shifted rhetoric without yet displacing the arrangement. The three measurement series share one six-point grid (0, 8, 16, 24, 32, 40) so every metric is authored at every examined time point; the trajectories are monotonic ratchets rather than cycles — each crisis (early-80s recession, 2008, pandemic) briefly damaged the arrangement's legitimacy and then triggered renewed defense funding, so no oscillatory phase is modeled. Coordination type is declared as identity_coordination: the operative coordination function is the beneficiary class's shared legitimating identity ('market outcomes are deserved outcomes'), which solves their collective-action problem of funding systemic defense no single firm would undertake alone. The FNL gaming caveat applies with force here: the identity function is genuine for the coordinating class while serving as cover for everyone else, and the coupling test should be read accordingly.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats compute differently from the same structure. From the incumbent capital holders' position, the arrangement is a legitimate solution to their own collective-action problem: they face democratic and competitive threats, they organize a response, and the returns justify the expenditure — a rope-shaped world seen from inside the coordinating class. From the trapped worker seat and the constrained consumer and entrant seats, the same structure operates as enforced extraction: the coordination they are offered (stable prices, convenient platforms, 'flexibility') arrives bundled with suppressed wages, markups, and foreclosed entry, with exit priced out of reach. The administrator seat (revolving-door regulators) is genuinely dual: it experiences the machinery as public service while collecting private deferred compensation from the entities it oversees. The engine computes these per-seat classifications from the structural data; the authored snare claim does not adjudicate between them — it records this reading's assessment of the whole structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders sit nearest the beneficiary pole: they receive the transfer, control the rules through funded agenda-setting, and hold arbitrage-grade exit (mobile capital across jurisdictions and asset classes) — derived d near 0. Policy research institutes are pure beneficiaries with identity_locked exit: they collect funding and reputational returns and cannot pivot without dissolving their donor base and scholarly identity, which pins them near the subsidized end. Revolving-door regulators combine administration with personal collection via post-agency employment; their arbitrage exit places them low-d despite their nominal public mandate. Industrial workers sit nearest the target pole: trapped by geography, non-competes, and employer concentration, bearing the wage-share transfer — derived d near 1. Small-business entrants are targets with constrained exit. Consumers are the one seat where the automatic derivation would mislead: their dual payer/beneficiary position (markups paid, convenience received) would derive a near-symmetric d, but the story establishes their net position as payer — the markups fall on essentials with no realistic substitute, while the convenience surplus is real but smaller. A directionality override sets the powerless atom to 0.75 for this story to record that net-target position. Economic historians hold the analytical seat: d is undefined-for-classification and they collect nothing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing social legitimacy for concentrated wealth against recurring democratic challenge — remains live: concentration has grown, and the legitimacy deficit it generates has grown with it, which is precisely why defense expenditure rises rather than fades. Mandatrophy is therefore not resolved, and the classification guards against two opposite mislabels. Against rope: a rope requires participants to be net beneficiaries with unsuppressed alternatives; here the coordination function serves the coordinating class while the coordinated majority pays, and the alternatives (structural remedies, independent organization) are actively suppressed — the coordination story is cover. Against mountain: the kernel's own content claims naturalness, and a reader accepting that claim would classify the arrangement as a natural law needing no defense; this reading's structural data — measurable defense expenditure rising with concentration — is exactly the observable that falsifies the naturality claim, and the FSM machinery exists to catch the mountain-framed version of this same arrangement wherever it is authored as one. The honest downgrade path is also specified: if the counterfactual omega resolves toward persistence-without-defense, this constraint degrades toward an inertial remnant maintained theatrically — a piton-shaped successor — rather than remaining a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This file instantiates one reading — beneficiary_maintained — of the kernel market_naturalization. Would instantiating the lapsed_alternative or hybrid reading instead change the constraint''s structure?',
    'Compare the three family files directly: the lapsed_alternative reading assigns persistence to inertia with no active maintenance (low enforcement cost, vestigial beneficiary class); the hybrid reading partitions the arrangement into a defended core and a lapsed remainder. The disagreement is located in one structural element: whether active maintenance is causally necessary for persistence.',
    'Under the lapsed reading, effective extraction collapses toward inertial-remnant levels and the beneficiary class becomes vestigial; under the hybrid reading, extraction divides between a defended component and an inertial remainder, changing gain_flow, fixing_cost, and the temporal trajectory. This file''s snare shape holds only under the beneficiary_maintained premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: this constraint is one of three readings of the market_naturalization kernel; sibling readings alter beneficiary structure, enforcement profile, and epsilon.').

omega_variable(
    counterfactual_persistence_without_defense,
    'Would market dominance persist at observed levels if the defense apparatus (political expenditure, capture, ideological production) were removed — that is, is the maintenance causally load-bearing or redundant insurance?',
    'Natural experiments: jurisdictions and periods with exogenous enforcement shocks or campaign-finance discontinuities; event studies around lobbying cessation; cross-country comparison of dominance persistence under differing defense intensity.',
    'If dominance persists largely without defense, the lapsed_alternative reading gains ground and this constraint degrades toward an inertial remnant with theatrical maintenance; if dominance decays rapidly without defense, the beneficiary_maintained premise is confirmed and the snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_persistence_without_defense, empirical, 'Whether observed defense spending is causally necessary for dominance persistence or epiphenomenal.').

omega_variable(
    beneficiary_class_boundary,
    'Is ''incumbent capital holders'' the correct seat for gain receipt, or do the gains accrue to a broader managerial-shareholder coalition in which professional managers, fund intermediaries, and controlling families hold distinguishable positions?',
    'Ownership and control attribution studies separating cash-flow rights from voting control; compensation-flow analysis distinguishing executive rents from passive shareholder returns.',
    'A coalition finding would split the beneficiary seat, lower the derived directionality concentration, and complicate the gain_flow designation; a controlling-owner finding would sharpen it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_class_boundary, conceptual, 'Where the boundary of the benefiting class sits: owners versus managers versus intermediary funds.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the public acquiescence that lowers enforcement demand structural (dependence on employers, credit, and platforms) or internalized (meritocratic belief that market outcomes are deserved)?',
    'Post-shock attitude trajectories: if meritocratic belief declines quickly when material conditions deteriorate (as after 2008 and during the pandemic), the internalized component is shallow and structural dependency carries the suppression; slow belief persistence after conditions change indicates deep internalization.',
    'If internalization is deep, measured suppression understates the constraint''s durability — enforcement capacity could decay while acquiescence persists, masking continued extraction; if shallow, suppression tracks enforcement spending directly as the series assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the suppression that keeps remedial demand low.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mkt_nat_benef_maint_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mkt_nat_benef_maint_tr_t8, market_naturalization__beneficiary_maintained_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(mkt_nat_benef_maint_tr_t16, market_naturalization__beneficiary_maintained_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(mkt_nat_benef_maint_tr_t24, market_naturalization__beneficiary_maintained_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(mkt_nat_benef_maint_tr_t32, market_naturalization__beneficiary_maintained_reading, theater_ratio, 32, 0.3).
narrative_ontology:measurement(mkt_nat_benef_maint_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.33).

% Extraction over time
narrative_ontology:measurement(mkt_nat_benef_maint_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mkt_nat_benef_maint_be_t8, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(mkt_nat_benef_maint_be_t16, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(mkt_nat_benef_maint_be_t24, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(mkt_nat_benef_maint_be_t32, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 32, 0.76).
narrative_ontology:measurement(mkt_nat_benef_maint_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(mkt_nat_benef_maint_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(mkt_nat_benef_maint_su_t8, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(mkt_nat_benef_maint_su_t16, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(mkt_nat_benef_maint_su_t24, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(mkt_nat_benef_maint_su_t32, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(mkt_nat_benef_maint_su_t40, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, identity_coordination).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial claim 'market dominance is natural' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel. This file (beneficiary_maintained) carries high epsilon, an identifiable beneficiary class, and active enforcement — snare-shaped. market_naturalization__lapsed_alternative_reading carries low maintenance cost and a vestigial beneficiary class — piton-leaning. market_naturalization__hybrid_reading partitions extraction between a defended core and an inertial remainder. Historical influence runs from the lapsed reading to this one: the 'no maintenance needed' premise is what beneficiary defense exploits and funds; this reading's expenditure-record evidence feeds the hybrid synthesis. Each family member links the others; the epsilon differences are documented in each file's narrative context and routed through omega variables rather than averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
