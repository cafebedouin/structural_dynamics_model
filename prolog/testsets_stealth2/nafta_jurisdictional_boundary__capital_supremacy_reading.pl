% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary — Capital Supremacy Reading (Treaty Text as Supreme Law Over Domestic Standards)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   Under the capital supremacy reading, the trade agreement's text operates
 *   as supreme law: domestic regulatory standards yield where they conflict
 *   with treaty obligations, capital mobility and regulatory harmonization
 *   function as mandatory obligations, and investor-state machinery converts
 *   domestic rulemaking into internationally reviewable acts carrying
 *   monetary liability. The arrangement retains a genuine coordination core —
 *   predictable market access, neutral dispute settlement, stabilized
 *   investment — while transferring regulatory authority and liability upward
 *   and compensation flows toward mobile capital. This story instantiates ONE
 *   reading of the nafta_jurisdictional_boundary kernel; the
 *   embedded-liberalism and sovereignty-primacy readings are separate
 *   constraints with different victim sets and different epsilon, linked via
 *   network.affects_constraints. Per the epsilon referent rule,
 *   extractiveness is authored for the standing arrangement under contest
 *   (treaty supremacy as it actually operates), assessed by this reading's
 *   own lights. KEY AGENTS (by structural relationship): -
 *   mobile_capital_holders: Primary beneficiary (powerful/arbitrage) —
 *   collects compensation and market-access rents; can relocate -
 *   export_oriented_multinationals: Secondary beneficiary
 *   (institutional/arbitrage) — gains disciplined partner standards and
 *   stable access - investor_state_arbitration_industry: Derivative
 *   beneficiary (organized/mobile) — fee income scales with enforcement
 *   breadth - free_trade_commission: Agenda setter
 *   (institutional/constrained) — administers, interprets, constitutes
 *   panels; revolving-door tilt - constitutional_courts: Agenda setter
 *   (institutional/identity_locked) — decide supremacy questions inside each
 *   legal order - national_regulatory_agencies: Primary target
 *   (institutional/constrained) — lose jurisdictional authority measure by
 *   measure - domestic_environmental_standard_regimes: Target
 *   (moderate/trapped) — territorial standards bear amend-or-pay outcomes -
 *   domestic_labor_standard_regimes: Target (organized/trapped) — no
 *   international defense comparable to investor protections -
 *   subnational_governments: Target and excluded voice (moderate/trapped) —
 *   their laws reviewed by fora they cannot access -
 *   import_competing_domestic_firms: Target (organized/constrained) — locked
 *   open without remedy - consumer_households: Incidental beneficiary
 *   carrying diffuse costs (moderate/mobile) - trade_justice_coalitions:
 *   Excluded voice (organized/constrained) — objects from outside the
 *   negotiating rooms
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.7).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.58).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary — Capital Supremacy Reading (Treaty Text as Supreme Law Over Domestic Standards)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '34647ee9-34e9-43a2-891d-0e20f47e90c6').
narrative_ontology:cs_kernel_codification('34647ee9-34e9-43a2-891d-0e20f47e90c6', fixed_text).
narrative_ontology:cs_authority_grounding('34647ee9-34e9-43a2-891d-0e20f47e90c6', lineage).
narrative_ontology:cs_interpretation_layer_present('34647ee9-34e9-43a2-891d-0e20f47e90c6').
narrative_ontology:cs_reading_relation('34647ee9-34e9-43a2-891d-0e20f47e90c6', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('34647ee9-34e9-43a2-891d-0e20f47e90c6', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('34647ee9-34e9-43a2-891d-0e20f47e90c6', foundational, treaty_supremacy_over_domestic_regulation).
narrative_ontology:cs_axiom_status(treaty_supremacy_over_domestic_regulation, holdable).
narrative_ontology:cs_axiom_grounding('34647ee9-34e9-43a2-891d-0e20f47e90c6', treaty_supremacy_over_domestic_regulation, conventional).
narrative_ontology:cs_axiom('34647ee9-34e9-43a2-891d-0e20f47e90c6', foundational, capital_mobility_mandatory_obligation).
narrative_ontology:cs_axiom_status(capital_mobility_mandatory_obligation, holdable).
narrative_ontology:cs_axiom_grounding('34647ee9-34e9-43a2-891d-0e20f47e90c6', capital_mobility_mandatory_obligation, instrumental).
narrative_ontology:cs_axiom('34647ee9-34e9-43a2-891d-0e20f47e90c6', secondary, nondiscrimination_insufficient_safeguard).
narrative_ontology:cs_axiom_status(nondiscrimination_insufficient_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('34647ee9-34e9-43a2-891d-0e20f47e90c6', nondiscrimination_insufficient_safeguard, conventional).
narrative_ontology:cs_reference_frame('34647ee9-34e9-43a2-891d-0e20f47e90c6', treaty_text_as_supreme_law).
narrative_ontology:cs_drift_state('34647ee9-34e9-43a2-891d-0e20f47e90c6', post_2015_isds_reform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('34647ee9-34e9-43a2-891d-0e20f47e90c6', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, mobile_capital_holders).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, export_oriented_multinationals).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_industry).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_standard_regimes).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standard_regimes).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, import_competing_domestic_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, consumer_households).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, consumer_households).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, indirect_expropriation_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, minimum_standard_of_treatment_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, market_access_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own cross-border investments shielded by treaty guarantees against uncompensated host-state interference. When a host regulation devalues an investment they can initiate international arbitration seeking monetary compensation, and they can relocate portfolios toward jurisdictions offering stronger protections. They finance the lobbying that keeps the scope of protected obligations broad.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, mobile_capital_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Move goods through tariff-free channels secured by the agreement and depend on stable rules-of-origin and customs procedure. They press negotiating governments for broad disciplines on partner-country measures that would raise their costs or segment their markets, and they gain whenever a partner's domestic standard is struck down, amended, or never adopted.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, export_oriented_multinationals, beneficiary,
    institutional, generational, arbitrage, global).

% Specialist law firms, arbitrators, and administering institutions whose caseload exists because the treaty makes host-state regulation justiciable by foreign investors. Every dispute generates fees, and doctrinal breadth in awards expands future dockets. Senior practitioners rotate between advocacy and adjudication seats, reinforcing interpretive continuity.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_industry, beneficiary,
    organized, biographical, mobile, global).

% The trade ministries and joint commission that administer the agreement: they issue authoritative interpretations, constitute dispute panels, and decide which disputes proceed. Their personnel circulate between government and the industries the disciplines protect, and their interpretations have repeatedly narrowed the defenses available to respondent governments.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, free_trade_commission, agenda_setter,
    institutional, generational, constrained, continental).

% National apex courts that decide, when challenged, whether treaty obligations displace domestic statutes and what deference domestic institutions owe award creditors. Their precedent lines determine how far the treaty's reach extends inside each legal order, and they are bound by their own prior holdings in ways that make reversal slow even as surrounding doctrine shifts.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, national).

% Environment, health, labor, and financial agencies that draft and enforce domestic standards. When a measure they adopt is challenged as inconsistent with treaty obligations they must defend it before international panels rather than domestic courts, may owe compensation if they lose, and increasingly screen proposed rules against anticipated liability before issuing them.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_regulatory_agencies, payer,
    institutional, generational, constrained, national).

% The statutes, enforcement bodies, and scientific advisory structures that maintain air, water, chemical, and conservation standards within each country. Their measures are territorial and cannot relocate; when a standard triggers an investor claim or a trade-panel ruling, the available remedies are amendment, withdrawal, or payment, and any replacement standard must be designed under the same exposure.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_standard_regimes, payer,
    moderate, generational, trapped, national).

% Union-negotiated wage floors, workplace-safety rules, and collective-bargaining frameworks. The treaty's labor side-process offers consultation without sanctions, so labor protections have no international defense comparable to the protections investors enjoy when disciplined measures affect commercial operations; organizing capacity is local while the obligations binding governments are continental.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standard_regimes, payer,
    organized, biographical, trapped, national).

% Provinces, states, and municipalities that legislate land use, resource moratoria, and procurement preferences. Their measures have been subjects of investor claims even when adopted through full domestic legislative process, yet they held no seat in treaty negotiation and hold no standing in the dispute system that reviews their laws.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments, excluded).

% Producers whose home markets are opened by locked tariff schedules. They cannot obtain protective tariffs or discriminatory standard adjustments because the agreement forbids them, and their requests for relief route through adjustment programs rather than trade remedies with teeth. Exit means restructuring, relocating production, or closing.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, import_competing_domestic_firms, payer,
    organized, biographical, constrained, national).

% Buy imported goods at prices lowered by tariff elimination and gain product variety. They also absorb residual risks of weakened domestic precautionary standards indirectly, through exposure and remediation costs, and their preferences reach negotiators only diffusely through electoral cycles.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, consumer_households, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, consumer_households, payer).

% Labor federations, environmental organizations, and development groups that opposed the agreement's investment and dispute chapters. They were excluded from negotiating rooms operating under fast-track procedures; their influence runs through street mobilization, domestic litigation, and pressure on later renegotiations rather than through any seat in the treaty's institutions.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_justice_coalitions, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, mobile_capital_holders).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cross-border exchange where unilateral action fails: reduces transaction uncertainty for goods and capital moving across three legal orders, supplies neutral dispute resolution in place of diplomatic retaliation and tariff escalation, and stabilizes long-horizon investment against arbitrary host-state revision.
% TRANSFER_FUNCTION: Moves regulatory authority and liability exposure from domestic institutions upward to treaty-level obligations, and moves compensation payments and compliance rents from host-state treasuries and standard-bearing populations to mobile capital holders and the professional services that represent them.
% ABSENT_VOICES: Labor federations, environmental communities, and subnational governments whose standards bear the costs were absent from the negotiating rooms, which operated under closed fast-track procedures; they object from outside through mobilization and domestic litigation, and their absence from the table is part of why the investment chapter's scope went unopposed at signing.
% DISAPPEARANCE_RATIONALE: Cross-border supply chains, political-risk pricing, and dispute-resolution expectations are organized around the arrangement; overnight disappearance would force renegotiation of thousands of commercial arrangements, repricing of sovereign and project risk, and an immediate scramble to rebuild bilateral investment protection — the North American trading system would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: Securing cross-border commerce and investment against arbitrary state interference: for the NAFTA generation specifically, locking in market-oriented reform in Mexico and insulating investors against expropriation risk of the kind that swept developing states in the 1960s and 1970s; more generally, preventing beggar-thy-neighbor tariff and standard wars.
% FOUNDING_PROBLEM_CORROBORATION: Trade historians and development economists, writing from outside the benefiting parties, corroborate the original expropriation-insurance problem as real for its era. Regulatory-policy scholars and former tribunal participants critical of the system attest that extending the same machinery to ordinary, non-confiscatory domestic regulation answers no live founding problem and manufactures the risk it polices. No source outside the benefiting parties attests that policing ordinary domestic regulation was the arrangement's founding purpose; the attestation record is therefore split, and the split is itself the signal.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the obligation structure decouples liability from wrongdoing: lawful, non-discriminatory, publicly motivated regulation can ground compensable claims, so the transfer is driven by the boundary placement rather than by misconduct. It is bounded below totality because the same arrangement delivers real market-access and price gains to the economies whose regulators pay. Suppression (0.58) reflects enforcement by monetary liability and precedent rather than by force: formal exit exists but is priced at decade-scale diplomatic and economic cost, and the trajectory rises with the docket through 2014, then declines as the reform wave (intra-EU rulings, USMCA narrowing, denunciations) cut the machinery's scope. Theater ratio (0.38 and rising) tracks the growing share of activity that performs responsiveness — labor and environment side-processes that consult without sanctioning, reviews that document rather than bind — relative to the dispute machinery that actually moves money and law. Accessibility collapse (0.55): alternatives remain visible (renegotiation, withdrawal, domestic override) but each carries prohibitive cost, and regulator design space is narrowed rather than eliminated. Resistance (0.68) is unusually high for a legal instrument: mass mobilization, electoral reversals, tribunal-reform movements, and state exits from parallel instruments — resistance is the mechanism behind the post-2014 extraction decline. All three tracked series run on one shared seven-point grid (1994–2024 at five-year steps) so every metric is authored at every examined time point; the shape is rise-peak-partial-retreat, not a cycle, and no intermittent-reinforcement dynamic is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute different types from identical text. From mobile capital's position the arrangement is a purchased insurance product functioning as intended; from the regulatory agencies' and standard regimes' position the same structure operates as jurisdictional dispossession with a price tag. The agenda-setter seats split internally: the commission's interpretive practice tilts toward claimant-side outcomes while the constitutional courts are bound by their own precedent lines and cannot simply adopt either side's reading. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place mobile capital holders, export multinationals, and the arbitration industry near the beneficiary end (low d), amplified toward subsidy by their arbitrage-grade exit: they can restructure investments and forum-shop in ways trapped targets cannot. Victim declarations place the standard regimes, regulatory agencies, subnational governments, and import-competing firms near the target end (high d); the territorial, non-relocatable character of standards and the identity-binding of court precedent push those seats toward the full-target end. Consumer households sit near symmetric — genuine price gains against diffuse externality costs. No directionality overrides are authored: the override surface keys on power atoms alone, and this story's institutional seats diverge sharply in role (an institutional-wide override would misdirect both the captured commission and the constrained agencies), so the role-and-exit derivation is left to do the differentiating. The commission's revolving-door capture is documented in commentary rather than forced through an override it would contaminate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — insurance against outright expropriation — is largely solved for its original object, while the arrangement persisted and expanded into ordinary regulation, the classic signature of a mandate outlived and repurposed. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure snare erases the real coordination achievement (dispute settlement did replace tariff retaliation and confiscation risk, and market integration delivered measurable gains); reading it as pure rope hides the asymmetric transfer through which the same clauses tax domestic standard-setting and pay mobile capital. The genealogy interview records the mismatch honestly: founding_problem_status is contested rather than dead because the parties genuinely dispute whether the extended mandate serves any live problem, and the corroboration record splits along exactly the beneficiary/non-beneficiary line the R5 provenance rule anticipates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the capital_supremacy_reading of the nafta_jurisdictional_boundary kernel; would instantiating the embedded_liberalism_reading or the sovereignty_primacy_reading instead produce a structurally different constraint with a different victim set and epsilon?',
    'Author the two sibling stories as separate files and compare computed classifications: under embedded_liberalism, non-discriminatory domestic standards leave the victim set and epsilon falls toward coordination-cost levels; under sovereignty_primacy, the arrangement collapses to a thin coordination mechanism with states retaining full regulatory authority.',
    'The sibling readings change WHO pays: embedded_liberalism removes domestic standard regimes from the target set; sovereignty_primacy removes jurisdictional loss from regulatory agencies entirely. This file''s classification is valid only for the capital supremacy placement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three readings of the NAFTA jurisdictional-boundary kernel; sibling readings are separate constraints.').

omega_variable(
    boundary_placement_disagreement,
    'Where exactly does the jurisdictional boundary fall — which categories of domestic measure (non-discriminatory precautionary regulation, taxation, procurement preference, subnational land-use law) fall inside treaty jurisdiction and which remain domestic?',
    'Doctrinal analysis of award reasoning and of the treaty''s exception clauses as applied, not as drafted: classify challenged measures by outcome rather than by text.',
    'Each category moved outside the boundary shrinks the victim set and lowers measured extraction; the readings disagree precisely on this placement, so resolving it per-category partitions the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_placement_disagreement, conceptual, 'Location of the inter-reading disagreement: the placement of the treaty/domestic-law boundary.').

omega_variable(
    police_powers_convergence,
    'Is arbitral practice converging on a police-powers exception that removes ordinary, non-arbitrary public-welfare regulation from compensable interference?',
    'Longitudinal coding of award reasoning: track the frequency with which tribunals accept police-powers and proportionality defenses against indirect-expropriation and fair-treatment claims.',
    'If the exception consolidates, the capital supremacy reading structurally degrades toward the embedded liberalism placement — the victim set contracts to discriminatory or confiscatory measures only, and effective extraction falls accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(police_powers_convergence, empirical, 'Whether tribunal practice is narrowing the reading''s reach from inside.').

omega_variable(
    regulatory_chill_attribution,
    'How much of the observed weakening or withholding of domestic standard-setting is attributable to anticipated treaty liability rather than to ordinary budgetary and political cycles?',
    'Natural experiments comparing matched jurisdictions exposed and not exposed to investor claims over equivalent policy domains, controlling for fiscal conditions.',
    'High attribution raises the behavioral component of suppression above what formal enforcement alone shows; low attribution means the scalar suppression mostly reflects formal machinery, and the constraint operates less pervasively than its docket suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_attribution, empirical, 'Attribution of regulatory-chill effects to the liability shadow versus ordinary policy variation.').

omega_variable(
    state_exit_availability,
    'Is treaty denunciation a genuinely available exit for a signatory state, or is it structurally barred by capital-market discipline, successor-liability doctrine, and investor expectations priced into sovereign borrowing?',
    'Compare market reactions and subsequent investment terms for states that have denounced investment treaties with counterfactual peers that retained them.',
    'If exit is effectively barred, suppression is structural and the trapped-target characterization of signatory governments hardens; if exit is real at tolerable cost, part of the measured suppression is chosen commitment rather than confinement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_exit_availability, empirical, 'Whether formal state exit from the arrangement is practically exercisable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement_basis(naft_tr_t1994, observed).
narrative_ontology:measurement(naft_tr_t1999, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 1999, 0.22).
narrative_ontology:measurement_basis(naft_tr_t1999, observed).
narrative_ontology:measurement(naft_tr_t2004, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2004, 0.26).
narrative_ontology:measurement_basis(naft_tr_t2004, observed).
narrative_ontology:measurement(naft_tr_t2009, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2009, 0.3).
narrative_ontology:measurement_basis(naft_tr_t2009, observed).
narrative_ontology:measurement(naft_tr_t2014, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2014, 0.33).
narrative_ontology:measurement_basis(naft_tr_t2014, observed).
narrative_ontology:measurement(naft_tr_t2019, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2019, 0.36).
narrative_ontology:measurement_basis(naft_tr_t2019, observed).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement_basis(naft_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 1994, 0.54).
narrative_ontology:measurement_basis(naft_be_t1994, observed).
narrative_ontology:measurement(naft_be_t1999, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 1999, 0.61).
narrative_ontology:measurement_basis(naft_be_t1999, observed).
narrative_ontology:measurement(naft_be_t2004, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2004, 0.67).
narrative_ontology:measurement_basis(naft_be_t2004, observed).
narrative_ontology:measurement(naft_be_t2009, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2009, 0.72).
narrative_ontology:measurement_basis(naft_be_t2009, observed).
narrative_ontology:measurement(naft_be_t2014, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2014, 0.75).
narrative_ontology:measurement_basis(naft_be_t2014, observed).
narrative_ontology:measurement(naft_be_t2019, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2019, 0.72).
narrative_ontology:measurement_basis(naft_be_t2019, observed).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2024, 0.7).
narrative_ontology:measurement_basis(naft_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement_basis(naft_su_t1994, observed).
narrative_ontology:measurement(naft_su_t1999, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 1999, 0.45).
narrative_ontology:measurement_basis(naft_su_t1999, observed).
narrative_ontology:measurement(naft_su_t2004, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2004, 0.56).
narrative_ontology:measurement_basis(naft_su_t2004, observed).
narrative_ontology:measurement(naft_su_t2009, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2009, 0.65).
narrative_ontology:measurement_basis(naft_su_t2009, observed).
narrative_ontology:measurement(naft_su_t2014, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2014, 0.7).
narrative_ontology:measurement_basis(naft_su_t2014, observed).
narrative_ontology:measurement(naft_su_t2019, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2019, 0.64).
narrative_ontology:measurement_basis(naft_su_t2019, observed).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2024, 0.58).
narrative_ontology:measurement_basis(naft_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'NAFTA supremacy over domestic regulation' covers three structurally distinct claims about where the treaty/domestic-law boundary falls, each with its own victim set, its own epsilon, and its own classification. This file instantiates the capital_supremacy placement (boundary at maximum inclusion; domestic standards inside treaty jurisdiction). The embedded_liberalism sibling (boundary mid-placement; non-discriminatory standards protected) and the sovereignty_primacy sibling (boundary at minimum; treaty subordinate to domestic law) are separate stories linked here. Upstream/downstream structure: arbitral practice under this reading eroded the credibility of the embedded-liberalism balance claim, and the resulting backlash feeds the sovereignty-primacy reaction — this reading influences the first sibling and forecloses the second within any single legal framework. Sibling IDs are assumed to follow the manifest's naming pattern; adjust if the sibling files were minted differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
