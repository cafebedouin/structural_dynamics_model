% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance Naturalization — Hybrid Reading (Lapsed Core, Maintained Perimeter)
 *   domain: political economy/economic history/institutional analysis
 *
 * SUMMARY:
 *   A dominant platform's grip on a two-sided market is colloquially treated
 *   as one thing — 'market dominance' — but the kernel market_naturalization
 *   splits into three structurally distinct readings, each a separate
 *   constraint with its own epsilon. This file authors ONLY the hybrid
 *   reading: the arrangement as a mixture in which some pillars of dominance
 *   have lapsed into self-reproduction (installed base, developer habits,
 *   accumulated data) while others are actively maintained (distribution
 *   access controls, self-preferencing, acquisition of nascent rivals, fee
 *   floors backed by policy work). The referent of epsilon is the standing
 *   arrangement — the incumbent-governed market order as it actually operates
 *   — assessed by this reading's lights; the endorsed alternative (open,
 *   contestable markets) is NOT the referent. Claim and metrics are
 *   independent: claimed_type tangled_rope states the hybrid structure (a
 *   real coordination function, asymmetric extraction, active enforcement);
 *   the metric values state what the arrangement's operation looks like
 *   descriptively. KEY AGENTS (by structural relationship): -
 *   dominant_platform_incumbent: Agenda-setter and principal collector
 *   (institutional/arbitrage) — administers the arrangement and captures its
 *   gains - institutional_investors_incumbent_equity: Beneficiary
 *   (powerful/mobile) — collects returns without running anything -
 *   complementary_developers_ecosystem: Dual-positioned beneficiary-payer
 *   (organized/identity_locked) — coordinated by and paying into the same
 *   structure - merchants_and_sellers: Primary payer (organized/constrained)
 *   — bears the fee and terms burden - end_consumers: Diffuse
 *   payer-beneficiary (moderate/constrained) — convenience in, degraded
 *   competition out - nascent_competitors: Payer under suppression
 *   (moderate/trapped) — absorbed or blocked - rival_distribution_channels:
 *   Excluded challenger (powerful/trapped) — barred from the channel and the
 *   conversation - antitrust_enforcement_agencies: Observer with enforcement
 *   powers (institutional/analytical) - economic_historians: Analytical
 *   observer (analytical/analytical) — sees the full structure across
 *   historical episodes
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.58).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.6).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance Naturalization — Hybrid Reading (Lapsed Core, Maintained Perimeter)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political economy/economic history/institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8').
narrative_ontology:cs_kernel_codification('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', distributed).
narrative_ontology:cs_authority_grounding('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', distributed).
narrative_ontology:cs_reading_relation('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', market_naturalization__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', market_naturalization__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', foundational, dominance_requires_component_level_audit).
narrative_ontology:cs_axiom_status(dominance_requires_component_level_audit, holdable).
narrative_ontology:cs_axiom_grounding('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', dominance_requires_component_level_audit, empirically_contingent).
narrative_ontology:cs_axiom('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', foundational, maintained_perimeter_is_load_bearing).
narrative_ontology:cs_axiom_status(maintained_perimeter_is_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', maintained_perimeter_is_load_bearing, empirically_contingent).
narrative_ontology:cs_reference_frame('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', mixed_lapse_maintenance_regime).
narrative_ontology:cs_drift_state('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', contemporary_platform_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b4b64fc8-aa20-4e68-8c3f-7df4ae9b94c8', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, dominant_platform_incumbent).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, institutional_investors_incumbent_equity).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, complementary_developers_ecosystem).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, merchants_and_sellers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, end_consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, nascent_competitors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, end_consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, complementary_developers_ecosystem).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, consumer_welfare_standard_doctrine).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, network_effects_natural_monopoly_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the marketplace and its surrounding infrastructure: sets seller fees, API terms, and ranking rules; acquires nascent rivals before they scale; funds lobbying and litigation that shape the rules governing its own position. Early advantages — installed base, developer habits, decades of accumulated data — now reproduce themselves without daily attention, while specific margins such as distribution access, self-preferencing, and fee floors are defended continuously through contracts, product design, and policy work. Revenue arrives as take rates, advertising rents, and data advantages.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, dominant_platform_incumbent, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, dominant_platform_incumbent, beneficiary).

% Hold large equity positions in the incumbent through index funds and active managers. Returns depend on the durability of the incumbent's margins; they vote proxies and press management on moat defense. Capital can rotate to other holdings if returns compress, so their exposure is a portfolio choice rather than a livelihood.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, institutional_investors_incumbent_equity, beneficiary,
    powerful, biographical, mobile, global).

% Build applications, tools, and content on the incumbent's platform. They gain access to a large coordinated user base and standardized interfaces they did not have to build. They pay revenue shares, accept unilateral term changes, and have reorganized their engineering, skills, and customer relationships around the incumbent's stack; rebuilding elsewhere would mean abandoning accumulated code, reviews, and audience.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, complementary_developers_ecosystem, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, complementary_developers_ecosystem, payer).

% Sell goods and services through the marketplace. They reach demand they could not assemble alone but pay fees above comparable standalone processing and logistics costs, accept ranking rules they cannot audit, and fund fulfillment programs they helped capitalize. Multi-homing onto smaller venues is possible but fragments their sales and raises unit costs.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, merchants_and_sellers, payer,
    organized, biographical, constrained, global).

% Use the integrated service for search, purchase, and delivery in one place. They receive convenience, price transparency, and fast fulfillment. Where rivals have been acquired or blocked, they pay somewhat higher prices or accept heavier advertising loads and thinner service than a contested market would produce; switching is possible but habitual attachment makes it rare.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, end_consumers, payer,
    moderate, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, end_consumers, beneficiary).

% Startups attempting to compete on some margin of the incumbent's business. They face rapid feature copying, preferential ranking of the incumbent's own offerings, and acquisition offers that arrive precisely when independent growth starts to threaten the incumbent. Their realistic liquidity paths run through the incumbent itself, which converts would-be rivals into portfolio assets.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, nascent_competitors, payer,
    moderate, immediate, trapped, global).

% Operators of alternative sales, advertising, or payment channels that would compete on lower fees. Some were acquired outright; others are contractually or technically barred from interoperating with the incumbent's user base. They advocate mandatory interoperability and non-discrimination rules from outside the processes that set those rules.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, rival_distribution_channels, excluded,
    powerful, biographical, trapped, global).

% Investigate self-preferencing, acquisitions of nascent rivals, and fee levels; bring cases under abuse-of-dominance doctrines; negotiate remedies. Their doctrinal toolkit was shaped over decades in part by scholarship and lobbying funded by the incumbent, and case timelines run far longer than product cycles.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, antitrust_enforcement_agencies, observer,
    institutional, generational, analytical, national).

% Compare the current arrangement with historical dominance episodes — railroads, oil trusts, telephone monopolies — across whole sectors. They track which elements of past dominions decayed on their own once founding conditions passed and which required continuous defense, and publish outside any party's control.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__hybrid_reading, dominant_platform_incumbent).
narrative_ontology:fixing_cost_class(market_naturalization__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves two-sided market coordination: a single venue matches many buyers with many sellers and standardizes payments, logistics, trust signals, and discovery, so neither side must find the other or verify counterparties independently.
% TRANSFER_FUNCTION: Moves a percentage of merchant revenue and a slice of consumer surplus — through fees above standalone-service cost and added advertising load — from sellers and buyers to the incumbent and its shareholders; additionally moves nascent competitors' independence to the incumbent through acquisition.
% ABSENT_VOICES: Rival channel operators and would-be interoperating services are absent — acquired, barred, or lacking standing in the standards and rule-setting processes. Consumers appear only as aggregate analytics, with no seat. Future competitors who would object to today's rules being locked in do not yet exist to speak.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, merchants would migrate to competing venues and direct channels within quarters, payment and logistics providers would re-enter at competitive rates, and the incumbent's accumulated data advantage would begin decaying — commerce would reorganize around open protocols and multiple venues, as it did before consolidation.
% FOUNDING_PROBLEM: Fragmented, low-trust commerce: dispersed buyers and sellers with no common venue, no reliable payment rail, pervasive counterparty fraud, and search costs that kept narrow markets illiquid.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, independent standards bodies, published industrial-organization research, and competition-authority findings attest that baseline trust, payment, and discovery infrastructure no longer depends on the incumbent's exclusive control — the founding problem is substantially solved even where curation disputes continue. The incumbent and its funded trade associations attest the opposite. Corroboration for the 'solved' reading therefore exists from non-beneficiary sources; the 'still live' reading rests chiefly on beneficiary testimony.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: fees and terms sit measurably above standalone-service costs, but part of the arrangement's cost is genuine coordination the market would repurchase in some form, so epsilon lands mid-range rather than high. Suppression 0.60: alternatives are not uniformly blocked — some died competitively (lapse) while specific channels are contractually or technically closed (maintenance); the scalar blends both mechanisms. Theater 0.31: security review, curation, and small-seller programs are functional, but a growing share of activity defends the perimeter rather than serves users. Accessibility_collapse 0.52: roughly half the alternative landscape collapsed without enforcement (habit, installed base) and half remains nominally open but practically barred. Resistance 0.55: merchant coalitions, agency dockets, and legislative proposals are live but slow relative to product cycles. The temporal series share one grid (t=0..40, step 8). The suppression_requirement series is deliberately non-monotonic: heavy early enforcement (0.70) relaxes during the lapse phase (0.52 at t=16) as network effects carry the position unaided, then re-intensifies (0.66 at t=32) when nascent threats force renewed acquisition and contracting, settling at 0.60 — the shape itself is this reading's signature. Extractiveness rises monotonically (0.42 to 0.58) as maintained margins compound; theater rises gently as defensive activity grows. Suppression is authored as a raw structural property; only extractiveness is engine-scaled by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural facts. From the incumbent seat the arrangement is infrastructure it built and keeps safe — coordination first, returns second. From the merchant seat the same fee schedule is a toll detached from cost. Developers sit between: they experience genuine enablement and unpayable exit in the same breath, which is what identity-lock does to a seat. Investors experience the arrangement as a portable return stream — their exit is trivial, so the constraint barely touches them despite their beneficiary position. Agencies see doctrine-shaped evidence; historians see the pattern repeating across centuries. The engine derives these divergences from power, exit, and declared position; nothing in the claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation: the incumbent (agenda_setter, listed among beneficiaries) sits near the beneficiary pole. Institutional investors (beneficiary, mobile exit) sit nearest the subsidy end — easy exit damps their effective burden toward zero. Complementary developers declare both roles with identity_locked exit, pulling their derived d toward the target side despite nominal benefit. Merchants, consumers, and nascent competitors (victims; constrained or trapped exit) sit near the full-target pole, with trapped nascent competitors highest. Excluded rival channels sit outside the benefit flow entirely — their exclusion is the enforcement object. Observers carry analytical exits and feed no extraction arithmetic. Commercial seats carry global scope, which amplifies effective extraction modestly through verification difficulty; the nationally scoped agency seat is unaffected.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented, low-trust commerce — is substantially solved: trust rails, payment standards, and discovery mechanisms now exist independently of the incumbent's exclusive control, and non-beneficiary sources corroborate this. Yet the arrangement persists and compounds. The hybrid reading prevents two opposite mislabels. Read as pure lapse (the sibling lapsed_alternative_reading), the arrangement would masquerade as a natural outcome — a false summit that launders maintained extraction as physics. Read as pure maintenance (beneficiary_maintained_reading), the genuine coordination function and the honestly-lapsed margins would be miscounted as extraction, overstating effective extraction and licensing overcorrection that destroys real coordination value. Tangled_rope holds both truths: coordination worth having, extraction worth removing, and the boundary between them moving — which is why the temporal series, not the scalar, carries the diagnostic weight. The R5 mismatch (founding_problem_status contested, leaning dead, against disappearance_verdict world_rearranges) flags the zombie dynamic: the mandate is largely gone; the machine runs on.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_partition_validity,
    'Does the hybrid reading''s partition of market dominance into a lapsed core and an actively maintained perimeter hold up margin by margin, or does the split dissolve under audit?',
    'Component-level audit of each dominance margin (distribution access, data accumulation, developer habits, fee setting, self-preferencing): classify each as reproducing without expenditure or requiring continuous defense, using enforcement budgets, litigation dockets, and lobbying disclosures.',
    'If most margins prove lapsed, the lapsed_alternative_reading becomes the correct constraint and effective extraction falls toward inertia levels; if most prove maintained, the beneficiary_maintained_reading governs and extraction rises toward snare levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_partition_validity, empirical, 'Whether the lapse/maintenance partition underlying this reading is empirically stable.').

omega_variable(
    lapse_vs_suppression_attribution,
    'For each collapsed alternative, was it outcompeted on the merits (genuine lapse) or suppressed by exclusionary conduct (maintenance misrecorded as lapse)?',
    'Counterfactual pricing and entry analysis per collapsed alternative: internal documents, acquisition timing relative to threat, and natural experiments from jurisdictions where the conduct was restrained.',
    'Reattribution moves margins between the lapsed and maintained columns, shifting measured suppression and the accessibility_collapse profile; a heavily suppressed ledger supports the beneficiary_maintained sibling instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_vs_suppression_attribution, empirical, 'Attribution of collapsed alternatives to competition versus suppression.').

omega_variable(
    domain_extractiveness_variance,
    'How much does the standing arrangement''s extractiveness vary across the domains the incumbent dominates (retail marketplace, advertising, app distribution, cloud services)?',
    'Per-domain fee benchmarking against standalone service costs and pre-dominance baselines; separate stories per domain if the spread exceeds the scalar''s resolution.',
    'The single epsilon of 0.58 is a cross-domain average; resolving the spread would decompose this story into per-domain constraints with distinct victim sets and types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_extractiveness_variance, empirical, 'Cross-domain heterogeneity masked by the scalar extractiveness value.').

omega_variable(
    lapsed_core_naturalness,
    'Is the lapsed core a genuine emergent feature of network economics that would arise under any competitive regime, or the residue of past suppression now misread as natural?',
    'Historical comparison with dominance episodes that arose without exclusionary conduct, and industrial-organization modeling of network-effect markets under enforced neutrality rules.',
    'If the lapsed core is itself sedimented suppression, the mountain-like component of this reading is a false summit and the whole arrangement reads as maintained extraction; if genuinely emergent, part of the measured extraction is the irreducible price of network coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_core_naturalness, conceptual, 'Framing ambiguity in the naturalness of the lapsed component.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint instantiates the hybrid reading of the market_naturalization kernel; what would change structurally under each sibling reading?',
    'Comparison against the sibling files: the lapsed_alternative_reading authors the same arrangement with no active-enforcement machinery and a thinned victim set (drifting mountain-ward); the beneficiary_maintained_reading authors it with comprehensive active defense and an enlarged victim set (drifting snare-ward). The disagreement is located in the causal weight assigned to active maintenance versus inertia.',
    'Adopting a sibling reading changes the victim roster, the enforcement profile, and the computed per-seat classifications for every seat; this file''s epsilon is valid only under the hybrid partition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame record of the kernel, this reading, and the sibling structural deltas.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__hybrid_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__hybrid_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__hybrid_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__hybrid_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__hybrid_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mark_be_t8, market_naturalization__hybrid_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(mark_be_t16, market_naturalization__hybrid_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(mark_be_t24, market_naturalization__hybrid_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(mark_be_t32, market_naturalization__hybrid_reading, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(mark_be_t40, market_naturalization__hybrid_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(mark_su_t8, market_naturalization__hybrid_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(mark_su_t16, market_naturalization__hybrid_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(mark_su_t24, market_naturalization__hybrid_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(mark_su_t32, market_naturalization__hybrid_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(mark_su_t40, market_naturalization__hybrid_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'market dominance' fails the epsilon-invariance test — measuring it as pure inertia yields negligible extraction, measuring it as pure defense yields high extraction — so it is three constraints, not one. This file is the hybrid member. Family links run through network.affects_constraints in all three files. The hybrid reading sits evidentially upstream of both pure readings because its margin-by-margin audit assigns each dominance pillar to the lapse or maintenance column, and those assignments are the raw material each sibling generalizes from. Upstream/downstream here is evidentiary, not causal: the hybrid audit constrains what the siblings can coherently claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
