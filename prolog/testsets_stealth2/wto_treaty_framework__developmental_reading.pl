% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework — Developmental Reading (Policy Space as Equal-Status Commitment)
 *   domain: international trade law / development economics / political economy
 *
 * SUMMARY:
 *   The WTO Agreement is a single persisting treaty kernel read incompatibly
 *   by its parties. This file instantiates the developmental reading:
 *   development policy space as an equal-status treaty commitment, Special
 *   and Differential Treatment as permanent structural accommodation of
 *   asymmetric starting conditions, and technology-transfer obligations as a
 *   core commitment binding developed members and constraining multinational
 *   IP rights. Under this reading the framework is a trade-coordination
 *   structure carrying a standing North–South transfer: Global South members
 *   hold the accommodation rights; IP rights holders and developed-country
 *   exporters bear the obligations and the foregone access. The sibling file
 *   (wto_treaty_framework__market_access_reading) instantiates the opposite
 *   reading — symmetric liberalization with S&D as temporary transitional
 *   exception — with inverted victim/beneficiary structure and a different ε;
 *   per the ε-invariance principle each file authors one reading with one
 *   stable ε, and neither hedges across the contest. The colloquial label
 *   'the WTO treaty framework' conflates the two; this decomposition
 *   separates them. The claim/metric gap is deliberate: tangled_rope is
 *   CLAIMED from structure (genuine coordination plus asymmetric transfer
 *   through the same rules plus active enforcement), while the metrics are
 *   authored descriptively — including a theater ratio that rises as the Doha
 *   development label outlives its operational content.
 *
 * KEY AGENTS:
 *   - developing_country_members: Primary beneficiary, dual-positioned as bound party (organized/constrained) — holds the accommodation rights and negotiates as coalitions
 *   - least_developed_members: Deep beneficiary (powerless/trapped) — preference-dependent, longest transitions
 *   - infant_industry_producers: Protected domestic producers (moderate/constrained) — the accommodation's industrial constituency
 *   - generic_medicines_producers: Licensing-authority beneficiaries (moderate/constrained) — the Doha health acquis's commercial leg
 *   - multinational_ip_rights_holders: Primary target (institutional/arbitrage) — bears technology-transfer obligations and licensing exposure; partially recovers via TRIPS-plus channels outside the treaty
 *   - developed_country_exporters: Secondary target (powerful/mobile) — bears conditional access and subsidized competition; mitigates via FDI substitution and trade redirection
 *   - wto_ministerial_conference: Agenda-setter (institutional/constrained) — consensus gatekeeper; cannot redefine the bargain
 *   - wto_dispute_settlement_body: Enforcement seat (institutional/constrained) — partially paralyzed since December 2019
 *   - consumers_in_protected_developing_markets: Excluded voice (powerless/constrained) — bears diffuse costs behind the walls, holds no seat
 *   - trade_policy_analysts: Analytical observer (analytical/analytical) — maps the framework's operation from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.48).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.4).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework — Developmental Reading (Policy Space as Equal-Status Commitment)").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international trade law / development economics / political economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, 'b3c4fb89-48a9-43f8-8919-38b3383a2589').
narrative_ontology:cs_kernel_codification('b3c4fb89-48a9-43f8-8919-38b3383a2589', fixed_text).
narrative_ontology:cs_authority_grounding('b3c4fb89-48a9-43f8-8919-38b3383a2589', lineage).
narrative_ontology:cs_interpretation_layer_present('b3c4fb89-48a9-43f8-8919-38b3383a2589').
narrative_ontology:cs_reading_relation('b3c4fb89-48a9-43f8-8919-38b3383a2589', wto_treaty_framework__market_access_reading, forecloses).
narrative_ontology:cs_axiom('b3c4fb89-48a9-43f8-8919-38b3383a2589', foundational, development_policy_space_equal_status).
narrative_ontology:cs_axiom_status(development_policy_space_equal_status, holdable).
narrative_ontology:cs_axiom_grounding('b3c4fb89-48a9-43f8-8919-38b3383a2589', development_policy_space_equal_status, conventional).
narrative_ontology:cs_axiom('b3c4fb89-48a9-43f8-8919-38b3383a2589', foundational, technology_transfer_core_commitment).
narrative_ontology:cs_axiom_status(technology_transfer_core_commitment, holdable).
narrative_ontology:cs_axiom_grounding('b3c4fb89-48a9-43f8-8919-38b3383a2589', technology_transfer_core_commitment, instrumental).
narrative_ontology:cs_axiom('b3c4fb89-48a9-43f8-8919-38b3383a2589', secondary, sd_permanent_structural_accommodation).
narrative_ontology:cs_axiom_status(sd_permanent_structural_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('b3c4fb89-48a9-43f8-8919-38b3383a2589', sd_permanent_structural_accommodation, conventional).
narrative_ontology:cs_reference_frame('b3c4fb89-48a9-43f8-8919-38b3383a2589', development_accommodation_bargain).
narrative_ontology:cs_drift_state('b3c4fb89-48a9-43f8-8919-38b3383a2589', post_doha_stall_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b3c4fb89-48a9-43f8-8919-38b3383a2589', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developing_country_members).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_members).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, infant_industry_producers).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, generic_medicines_producers).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developed_country_exporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, developing_country_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Roughly two-thirds of the WTO membership, self-designated as developing. They hold treaty-protected flexibility: higher bound tariffs with unbound headroom, longer transition periods, subsidy space for industrial policy, and compulsory-licensing authority over patents. They negotiate through coalitions (African Group, G-90) that aggregate weight no single member holds. They also bear their own bindings — tariff schedules, post-transition TRIPS floors, and dispute exposure — so they are simultaneously the accommodation's holders and bound parties. Leaving the framework would forfeit MFN market access and the legal anchor for their flexibility claims; their option is to work the text and the waivers, not to walk.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developing_country_members, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__developmental_reading, developing_country_members, payer).

% The poorest members, whose market access depends heavily on preference programs (Everything But Arms, AGOA, DFQF commitments) and whose transition periods run longest — pharmaceutical patent protection deferred to 2033. They lack the administrative capacity to litigate disputes or to fully use the flexibility they formally hold; their leverage comes almost entirely from coalition membership and moral framing in ministerials. Exit would strip preferences they cannot replace.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, least_developed_members, beneficiary,
    powerless, generational, trapped, global).

% Domestic manufacturers in developing members operating behind tariff walls and subsidy programs the framework tolerates under S&D and transition provisions — steel, autos, electronics assembly. Their viability depends on continued policy space, which their governments defend in negotiations on their behalf. They bear no treaty obligations of their own; their costs are the higher input prices and smaller export markets the walls create.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, infant_industry_producers, beneficiary,
    moderate, generational, constrained, national).

% Producers of off-patent medicines (concentrated in India, with regional capacity elsewhere) whose business model rests on the compulsory-licensing authority and the LDC pharmaceutical transition. The Doha Declaration on TRIPS and Public Health and the Article 31bis export mechanism are the legal basis of their export trade. They hold no seat in negotiations; their interests arrive through the public-health positions of their home governments.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, generic_medicines_producers, beneficiary,
    moderate, biographical, constrained, global).

% Pharmaceutical, software, and content industries whose patent and copyright rents are the object of the framework's technology-transfer and compulsory-licensing provisions. They won binding IP enforcement at the founding (TRIPS), but the development carve-outs limit it, and they have since rebuilt protection through TRIPS-plus provisions in bilateral and regional FTAs, unilateral watch lists, and pricing strategies. Their treaty-level losses are real but partially recoverable outside the WTO.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, multinational_ip_rights_holders, payer,
    institutional, biographical, arbitrage, global).

% Export industries and their governments in OECD members, who accept reduced and conditional access to developing markets in exchange for the framework's rules: higher tariffs, subsidized competitors, and local-content regimes that S&D shields from challenge. Their response capacity is high — they redirect trade, invest behind the barriers (FDI substituting for exports), and litigate selectively. They fund the secretariat and underwrite the preference programs.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, developed_country_exporters, payer,
    powerful, biographical, mobile, global).

% The membership meeting at ministerial level and in the General Council: it sets the negotiating agenda, grants waivers, appoints Appellate Body members by consensus, and adopts interpretations. Every member holds a formal vote; in practice consensus governs, so the agenda moves only when developed and developing coalitions both assent — which is why the Doha development mandates have been stuck since 2008. The conference cannot impose either reading of the treaty; it can only fail to agree.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_ministerial_conference, agenda_setter,
    institutional, generational, constrained, global).

% Panels and (until December 2019) the Appellate Body, which adjudicate members' disputes and authorize retaliation; since the Appellate Body's paralysis, appeals into the void have weakened the finality of rulings. Its members are appointed by consensus, so the body administers the bargain but cannot redefine it. It is the machinery through which the framework's obligations — including, in principle, the technology-transfer commitments — would be enforced.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Households in developing members paying the prices behind tariff walls and subsidy-financed industries — food, fuel, electronics. They are represented at the table only through their governments, which weigh consumer costs against industrial-policy and revenue goals; no seat exists for the consumer interest as such. Their objection — that permanent protection is a regressive transfer from poor households to protected producers — is voiced in domestic politics, not in treaty negotiation.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, consumers_in_protected_developing_markets, excluded,
    powerless, immediate, constrained, national).

% Academic economists, legal scholars, and think-tank researchers who map the framework's operation from outside the negotiating room: preference utilization rates, S&D provision uptake, technology-transfer flows under Article 66.2, and the distributional record of liberalization. Their analyses feed both readings of the framework and the periodic ministerial assessments; they hold no votes and bear no obligations.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__developmental_reading, trade_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__developmental_reading, developing_country_members).
narrative_ontology:fixing_cost_class(wto_treaty_framework__developmental_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the standing collective-action problem of trade: without common bindings, states face the prisoner's dilemma of protectionism and retaliatory spiral. The framework binds tariffs, guarantees non-discrimination (MFN, national treatment), and replaces power-based settlement with adjudication. Under this reading it additionally coordinates development: S&D, transition periods, and licensing authority are standing structural features that let late-industrializers pursue industrialization inside the rules rather than outside them.
% TRANSFER_FUNCTION: Moves enforceable market access and rule-discipline from every member into the common framework; moves accommodation — tariff headroom, subsidy space, transition time, compulsory-licensing authority, preference eligibility — toward developing-country members; and moves technology-transfer obligations and licensing exposure onto IP rights holders and developed-country producers, whose rents and access are the standing price of the bargain.
% ABSENT_VOICES: Consumers in protected developing markets, workers in import-competing developed-country sectors, and would-be exporters excluded by preference conditionality would object but have no seat: trade negotiations are conducted state-to-state, and these constituencies appear only filtered through their governments' positions. The consumer seat is authored as excluded for exactly this reason — its costs are real and diffuse, and its absence from the room is what lets permanent protection persist as 'development policy.'
% DISAPPEARANCE_RATIONALE: Tariff bindings, MFN coverage, binding dispute settlement, the Doha public-health acquis, and the LDC transitions all dissolve; preference programs lose their legal frame; trade reorganizes around power-based bilateralism and the security-statecraft agenda. Developing members lose the textual anchor for their flexibility claims — and IP holders lose the compulsory-licensing ceiling too. Both sides lose structure, which is why both defend the framework while fighting over its reading.
% FOUNDING_PROBLEM: The interwar tariff-war spiral and the postwar problem of rebuilding trade among economies at radically different development levels: the GATT/WTO was built to prevent beggar-thy-neighbor protectionism while leaving room for late-industrializers to develop behind temporary barriers — a bargain carried from the Havana Charter's development mandate through GATT Part IV and the Enabling Clause into the WTO's S&D provisions and the Doha mandates.
% FOUNDING_PROBLEM_CORROBORATION: Development economists outside the treaty system (the comparative industrial-policy literature — Rodrik, Chang, Akyüz) attest the development-accommodation problem as live and unresolved; IP-intensive industry associations and several developed-country trade ministries attest the opposite — that transitional accommodation has become permanent rent and the problem is settled or subordinated to security-era industrial policy. The 2015 Nairobi ministerial declaration records the membership itself splitting on whether the Doha mandates stand. No corroborator outside the contest speaks with a single voice; the split is the finding.
narrative_ontology:disappearance_verdict(wto_treaty_framework__developmental_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__developmental_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__developmental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__developmental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__developmental_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   ε's referent is the standing WTO arrangement as this reading assesses it — never the reading's endorsed alternative. Extraction is moderate (0.48 at interval end): the arrangement binds all members into liberalization discipline while — as this reading holds — structurally reducing the burden on Global South members; the extraction that remains concentrates on IP rights holders (technology-transfer obligations, compulsory-licensing exposure) and developed-country exporters (subsidized competition, conditional access). Suppression (0.40) is structural, not internalized: binding dispute settlement with authorized retaliation is the coercive core, softened by transition periods and S&D shields. The suppression_requirement series is authored because enforcement capacity is the interval's dynamic: DSU machinery matured through the 2000s (high-profile rulings, retaliation authorizations), then decayed after the Appellate Body paralysis of December 2019. Theater (0.42) rises monotonically: the operational core (tariff schedules, the Doha public-health acquis) remains functional while a growing share of development activity is declaratory — ministerial language and the Doha label persisting after the S&D operationalization program stalled. Accessibility collapse (0.55): alternatives exist (bilateral and regional FTAs, plurilaterals, unilateral preference programs) but are partly collapsed by MFN network effects, preference dependence, and the value of the legal anchor. Resistance (0.55) is sustained and seat-specific: TRIPS-plus lobbying against the licensing carve-outs, S&D-aggregation deadlock, waiver fights, and the Appellate Body blockage are all resistance to this reading's program. The three series share one grid (t=0,5,10,15,20,25,30) with every metric authored at every point. The extractiveness arc — rise to the 2001–2005 Doha peak, decline as TRIPS-plus arbitrage re-expanded IP rights outside the treaty, partial recovery with the narrow 2022 waiver — is the interval's central dynamic. claimed_type is authored from structure, not from these values; the engine computes per-seat types from the data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from one structure. From a developing-country member's position the framework is accommodation-bearing coordination it fought to write into the text (Part IV, Enabling Clause, Doha); from the multinational IP holder's position the same rules are enforced rent surrender it has spent the interval clawing back through TRIPS-plus FTAs; from the developed exporter's position it is conditional access and subsidized competition borne with high mobility. The agenda-setting seats (ministerial conference, dispute settlement body) sit near the administrative middle — they administer the bargain and cannot redefine it by consensus. The sibling reading inverts the asymmetry entirely: under market_access_reading the developing-country seat computes as the discipline target. That divergence is the measurement this corpus exists to take; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-d seats: developing_country_members (organized power, constrained exit) derive near-beneficiary d; least_developed_members (powerless, trapped, preference-dependent) sit nearest the subsidy end; infant_industry_producers and generic_medicines_producers derive low d as the protected and licensing-empowered classes. Victim declarations drive the high-d seats: multinational_ip_rights_holders bear the obligations, but their arbitrage (TRIPS-plus FTAs, unilateral pressure, pricing strategy) operates outside this constraint and partially recovers what it surrenders — their effective borne extraction is real but mitigated, placing them well short of full-target d; developed_country_exporters' mobility (FDI substitution, trade redirection) mitigates similarly. The ministerial conference and dispute settlement body are administrative seats near symmetric. Consumers behind the tariff walls bear diffuse real costs but hold no seat in the conversation — their position registers as absence, not directionality, which is the absent-voices finding. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the right per-seat relationships, and the override surface (keyed by power atom) cannot distinguish the heterogeneous institutional seats here.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's mandate vehicle was the Doha Development Agenda: operationalize S&D, make technology transfer real, deliver the development round. That mandate is contested-to-dead in practice — the round stalled in 2008, Nairobi 2015 recorded members declining to reaffirm it, and the theater series tracks the widening gap between declaratory development activity and operational content. The tangled_rope classification prevents two mislabels: pure rope (the win-win trade-peace story) misses the asymmetric transfer running through the same rules — someone is coordinated and someone pays through one structure; pure snare misses that the beneficiaries genuinely defend the arrangement and the coordination function (bindings, adjudication) is real — its disappearance would rearrange the trade world for every seat. The residual risk is drift: if the development content fully atrophies while liberalization discipline persists, this constraint converges toward the sibling reading's shape; the network edge to market_access_reading registers that drift path, and the theater trajectory — not the label — is what would date the transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_sandd_status,
    'This constraint is the developmental_reading of kernel wto_treaty_framework. The sibling reading (market_access_reading) holds the same text as symmetric liberalization obligation with S&D as temporary transitional exceptions. Which structural premise holds: is S&D a permanent structural accommodation of equal treaty rank, or a transitional exception subordinate to market access?',
    'An adopted S&D aggregation decision, a binding operationalization of Enabling Clause obligations, or authoritative dispute-settlement treatment of S&D justiciability would fix the status; the Nairobi 2015 non-reaffirmation and the stalled Doha round mark the drift meanwhile.',
    'If the sibling premise prevails, this constraint''s victim and beneficiary sets invert — developing members become discipline targets rather than accommodation holders — ε for Global South seats rises sharply, technology-transfer obligations collapse toward hortatory, and the classification re-derives toward the market_access_reading file''s profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_sandd_status, conceptual, 'Reading contest within the WTO kernel: permanent structural accommodation versus temporary exception.').

omega_variable(
    tech_transfer_obligation_enforceability,
    'Are the TRIPS Article 66.2 and 67 technology-transfer commitments binding, enforceable obligations (as this reading holds — its core commitment), or hortatory best-endeavor language with no operative content?',
    'A test dispute against a developed member''s Article 66.2 record, a reporting-and-review mechanism with consequence, or an empirical audit comparing 66.2 incentive reports against measured transfer flows.',
    'If hortatory, the reading''s core commitment is performative: borne extraction on IP holders from this element falls toward zero, theater_ratio rises further, and the developmental claim keeps its text while losing its operational core.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tech_transfer_obligation_enforceability, empirical, 'Enforceability of the technology-transfer core commitment.').

omega_variable(
    compulsory_licensing_usability,
    'Is the compulsory-licensing authority (TRIPS Article 31, the 31bis export mechanism, the LDC pharmaceutical transition) practically usable by members without manufacturing capacity, or structurally barred by remuneration terms, export-parcel limits, and capacity constraints?',
    'Post-31bis and post-2022-waiver licensing records: the mechanism has seen roughly one notification in two decades against its design intent; a systematic audit of attempted and abandoned licenses would settle usability.',
    'If unusable, the licensing component of policy space is nominal — the accommodation''s real content shrinks to tariff headroom and transition time, and borne extraction on IP holders from this element falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_usability, empirical, 'Practical usability of the compulsory-licensing authority.').

omega_variable(
    self_designation_beneficiary_boundary,
    'Developing-country status at the WTO is self-designated with no graduation criteria — does the beneficiary class include advanced economies whose continued claims dilute the accommodation available to the poorest members, and would objective criteria redraw the structure?',
    'An S&D aggregation decision adopting per-capita or sectoral graduation; comparative analysis of flexibility use across income cohorts.',
    'With graduation enforced, per-LDC accommodation rises while aggregate borne extraction on developed producers and IP holders falls, and the asymmetry sharpens around the poorest members; without it, the beneficiary class blurs toward ''everyone but the smallest developed states.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_designation_beneficiary_boundary, conceptual, 'Beneficiary-set boundary under self-designation.').

omega_variable(
    policy_space_development_efficiency,
    'Does preserved policy space actually produce industrialization (this reading''s working premise), or does it subsidize protected incumbents and rent-seeking coalitions at the expense of the consumers behind the walls?',
    'Comparative development outcomes under binding versus flexible trade regimes, controlling for state capacity and initial conditions — the East Asian record against the protected-stagnation record.',
    'If the latter, the accommodation''s beneficiary claim weakens: gains accrue to protected elites rather than to development, the coordination leg of the structure erodes toward pure transfer, and the excluded consumer seat''s objection becomes the dominant one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_space_development_efficiency, empirical, 'Whether the accommodation produces development or rents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dev_reading_tr_t0, wto_treaty_framework__developmental_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(wto_dev_reading_tr_t5, wto_treaty_framework__developmental_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(wto_dev_reading_tr_t10, wto_treaty_framework__developmental_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(wto_dev_reading_tr_t15, wto_treaty_framework__developmental_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(wto_dev_reading_tr_t20, wto_treaty_framework__developmental_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(wto_dev_reading_tr_t25, wto_treaty_framework__developmental_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(wto_dev_reading_tr_t30, wto_treaty_framework__developmental_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(wto_dev_reading_be_t0, wto_treaty_framework__developmental_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wto_dev_reading_be_t5, wto_treaty_framework__developmental_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(wto_dev_reading_be_t10, wto_treaty_framework__developmental_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(wto_dev_reading_be_t15, wto_treaty_framework__developmental_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(wto_dev_reading_be_t20, wto_treaty_framework__developmental_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(wto_dev_reading_be_t25, wto_treaty_framework__developmental_reading, base_extractiveness, 25, 0.44).
narrative_ontology:measurement(wto_dev_reading_be_t30, wto_treaty_framework__developmental_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(wto_dev_reading_su_t0, wto_treaty_framework__developmental_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(wto_dev_reading_su_t5, wto_treaty_framework__developmental_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(wto_dev_reading_su_t10, wto_treaty_framework__developmental_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(wto_dev_reading_su_t15, wto_treaty_framework__developmental_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(wto_dev_reading_su_t20, wto_treaty_framework__developmental_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(wto_dev_reading_su_t25, wto_treaty_framework__developmental_reading, suppression_requirement, 25, 0.44).
narrative_ontology:measurement(wto_dev_reading_su_t30, wto_treaty_framework__developmental_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the WTO treaty framework' conflates two structurally distinct instantiations of one kernel. This file authors the developmental reading: ε ≈ 0.48, borne extraction concentrated on IP rights holders and developed-country exporters, accommodation flowing to Global South members, S&D held as permanent structural accommodation. The sibling file authors the market_access_reading: liberalization as symmetric obligation, S&D as temporary transitional exception, borne extraction concentrated on developing-country policy autonomy. The readings share the fixed treaty text but disagree on the legal status of S&D and the rank of policy space; the developmental reading draws on an older textual lineage (Part IV, Enabling Clause) whose operationalization the market-access reading's practice has partially overridden. Each file keeps a single stable ε per the ε-invariance principle; neither hedges across the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
