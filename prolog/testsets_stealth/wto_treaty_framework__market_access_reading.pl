% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__market_access_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__market_access_reading, []).

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
 *   constraint_id: wto_treaty_framework__market_access_reading
 *   human_readable: WTO Treaty Framework — Market-Access Reading (Symmetric Liberalization Obligation)
 *   domain: international_trade_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the WTO treaty kernel: the
 *   market-access reading, under which trade liberalization is a symmetric
 *   universal obligation, non-discrimination and market access are the
 *   treaty's primary purpose, and Special & Differential Treatment provisions
 *   are temporary transitional exceptions rather than structural
 *   entitlements. Under this reading the constraint binds tariffs, subsidies,
 *   and local-content requirements identically across members of radically
 *   unequal economic strength; infant industries in late-developing economies
 *   bear the resulting loss of the protective toolkit every earlier
 *   industrializer used, while multinational corporations collect the
 *   arrangement's principal rents. The claim and the metrics are independent
 *   authored facts: the claimed type (tangled_rope) states what I believe is
 *   structurally true — a genuine coordination function carrying asymmetric
 *   extraction through the same structure — while the metrics describe the
 *   arrangement's actual operation without being tuned to any predicted
 *   engine output. Per the kernel-reading rules, the contest with the sibling
 *   developmental reading is recorded in omega variables and kernel_context,
 *   not averaged into this constraint's ε.
 *
 * KEY AGENTS:
 *   - major_trading_powers: Agenda-setter and dual-positioned beneficiary (institutional/arbitrage) — wrote and administer the symmetric-obligation rules while retaining de facto exceptions in agriculture and security
 *   - multinational_corporations: Primary beneficiary (powerful/arbitrage) — collect tariff certainty, TRIPS royalty streams, and GATS access across borders; can relocate around residual exposure
 *   - advanced_economy_exporters: Beneficiary (organized/mobile) — hold contractually secured MFN access for capital goods and manufactures
 *   - developed_country_services_industries: Beneficiary (organized/mobile) — GATS commitments lock open finance, telecom, and logistics markets
 *   - infant_industries_developing_countries: Primary target (moderate/trapped) — denied the tariff, subsidy, and local-content instruments of every earlier late-industrializer
 *   - smallholder_farmers_global_south: Target (powerless/trapped) — absorb subsidized-Northern agricultural competition with no exit and no compensating export access
 *   - developing_country_governments: Payer with partial beneficiary position (moderate/constrained) — bear compliance and policy-space loss while receiving the DSU rule-based shield
 *   - civil_society_and_labor_movements: Excluded voice (organized/constrained) — outside the negotiating rooms; would press labor, environmental, and adjustment safeguards
 *   - academic_trade_economists: Analytical observer (analytical/analytical) — document welfare effects, asymmetry, and rent flows from a seat that neither collects nor pays
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, 0.75).
domain_priors:suppression_score(wto_treaty_framework__market_access_reading, 0.5).
domain_priors:theater_ratio(wto_treaty_framework__market_access_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(wto_treaty_framework__market_access_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__market_access_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__market_access_reading, "WTO Treaty Framework — Market-Access Reading (Symmetric Liberalization Obligation)").
narrative_ontology:topic_domain(wto_treaty_framework__market_access_reading, "international_trade_law/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__market_access_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__market_access_reading, 'db62a196-9886-49c1-b837-8fe233f53ec6').
narrative_ontology:cs_kernel_codification('db62a196-9886-49c1-b837-8fe233f53ec6', fixed_text).
narrative_ontology:cs_authority_grounding('db62a196-9886-49c1-b837-8fe233f53ec6', lineage).
narrative_ontology:cs_interpretation_layer_present('db62a196-9886-49c1-b837-8fe233f53ec6').
narrative_ontology:cs_reading_relation('db62a196-9886-49c1-b837-8fe233f53ec6', wto_treaty_framework__developmental_reading, coexists_with).
narrative_ontology:cs_axiom('db62a196-9886-49c1-b837-8fe233f53ec6', foundational, symmetric_reciprocity_binding_all_members).
narrative_ontology:cs_axiom_status(symmetric_reciprocity_binding_all_members, holdable).
narrative_ontology:cs_axiom_grounding('db62a196-9886-49c1-b837-8fe233f53ec6', symmetric_reciprocity_binding_all_members, instrumental).
narrative_ontology:cs_axiom('db62a196-9886-49c1-b837-8fe233f53ec6', foundational, special_treatment_legitimate_only_as_transition).
narrative_ontology:cs_axiom_status(special_treatment_legitimate_only_as_transition, holdable).
narrative_ontology:cs_axiom_grounding('db62a196-9886-49c1-b837-8fe233f53ec6', special_treatment_legitimate_only_as_transition, empirically_contingent).
narrative_ontology:cs_reference_frame('db62a196-9886-49c1-b837-8fe233f53ec6', gatt_reciprocal_liberalization_bargain).
narrative_ontology:cs_drift_state('db62a196-9886-49c1-b837-8fe233f53ec6', post_doha_industrial_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db62a196-9886-49c1-b837-8fe233f53ec6', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__market_access_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, advanced_economy_exporters).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developed_country_services_industries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, infant_industries_developing_countries).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, smallholder_farmers_global_south).
narrative_ontology:constraint_victim(wto_treaty_framework__market_access_reading, developing_country_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, major_trading_powers).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__market_access_reading, developing_country_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft negotiating texts, convene green-room meetings, and control the sequencing of rounds; they wrote the symmetric-obligation architecture they now administer and retain calibrated retaliation capacity that shapes what smaller members dare propose. When multilateral processes stall they can route commerce through mega-regional agreements they dominate, so the constraint never fully binds their own conduct.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, major_trading_powers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, major_trading_powers, beneficiary).

% Operate global value chains stitched together by bound tariffs, TRIPS intellectual-property protections, and GATS services commitments. They collect the arrangement's rents directly — tariff certainty, patent and copyright royalty streams, guaranteed services market access — and can relocate production, book profits, and shop jurisdictions to minimize whatever residual exposure remains.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Sell aircraft, machinery, pharmaceuticals, and manufactured goods into markets opened by MFN nondiscrimination and successive tariff-cutting rounds. Their access is contractually secured against importer backsliding, and they lobby their home governments to keep the obligation architecture intact.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, advanced_economy_exporters, beneficiary,
    organized, biographical, mobile, continental).

% Financial, telecommunications, and logistics firms hold GATS commitments that lock open service markets they would otherwise face by sector-by-sector political contest. Their delivery mode is cross-border or commercial presence, so they carry little of the adjustment cost the opening imposes on host-country incumbents.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developed_country_services_industries, beneficiary,
    organized, biographical, mobile, global).

% Firms in late-industrializing economies that need protected learning periods — tariff cover, subsidized credit, local-content linkage requirements — to reach cost competitiveness. The symmetric obligation removes precisely the toolkit every earlier industrializer used at the same stage; the firms cannot relocate their learning process abroad, and their maturation horizon spans decades while the obligation binds immediately.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, infant_industries_developing_countries, payer,
    moderate, generational, trapped, national).

% Compete against agricultural exports from economies that subsidize production and export credits at scales no smallholder operation can match — cotton, rice, dairy being canonical cases. Land, crops, and community roots make exit impossible short of crop switching or migration; they absorb the price effects of liberalization without any compensating access to the export markets being opened.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, smallholder_farmers_global_south, payer,
    powerless, biographical, trapped, regional).

% Bear the compliance burden: ceded tariff headroom, surrendered subsidy and local-content instruments, TRIPS implementation costs, and dispute-settlement exposure. They simultaneously receive the rule-based shield — binding adjudication and MFN access that constrain large-member retaliation against them in ways raw power politics never did. Withdrawal is legally available but economically ruinous and historically unprecedented at scale, so their exit is constrained rather than closed.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, developing_country_governments, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(wto_treaty_framework__market_access_reading, developing_country_governments, beneficiary).

% Shut out of negotiating rooms where commitments are drafted; they observe ministerials from barricades and parallel forums. They would press labor-standard floors, environmental safeguards, and distributional adjustment mechanisms, but their input enters only through domestic ratification politics after the texts are effectively settled.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, civil_society_and_labor_movements, excluded,
    organized, biographical, constrained, global).

% Model welfare effects, document the gap between formal symmetry and substantive asymmetry, and track compliance and rent flows. They see the full structure — coordination gains, extraction channels, and distribution — without collecting from or paying into it.
narrative_ontology:constraint_stakeholder(wto_treaty_framework__market_access_reading, academic_trade_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_treaty_framework__market_access_reading, multinational_corporations).
narrative_ontology:fixing_cost_class(wto_treaty_framework__market_access_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts bilateral power bargaining into multilateral rules: MFN nondiscrimination eliminates discriminatory-bloc dynamics, tariff bindings give traders predictable long-horizon access, and binding dispute settlement gives small members a rule-based shield against large-member retaliation. It solves the collective-action problem of sequential defection in trade policy.
% TRANSFER_FUNCTION: Moves enforceable market access from every member's domestic market to all exporters — valued most by advanced-equence firms; moves policy instruments (tariff headroom, subsidy capacity, local-content tools, IP flexibility) from developing-country governments into the treaty's discipline, exercised most by multinational firms; moves intellectual-property rents from generic manufacturers and public systems to patent-holding firms via TRIPS.
% ABSENT_VOICES: Labor movements, environmental organizations, informal-sector workers, and subsistence producers outside formal delegations are absent from green-room negotiations; consumer and taxpayer interests in subsidy programs are diffuse and unrepresented. Their objections — distributional adjustment, ecological externalities, subsidy accountability — enter only through domestic politics after commitments are struck.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would trigger an immediate scramble: applied tariffs float upward toward unbound ceilings, preferential-agreement negotiation explodes to fill the vacuum, supply chains reprice against tariff uncertainty, and IP enforcement collapses to national discretion. The trading order would rearrange around whatever bilateral power permits — the arrangement's dependencies are real on every seat.
% FOUNDING_PROBLEM: The interwar slide from Smoot-Hawley into retaliatory trade blocs and competitive devaluation, which deepened the Depression and was read by postwar planners as systemic failure requiring locked-in, judicially insulated liberalization that domestic political cycles could not unwind.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: economic historians of the interwar collapse — with no stake in the WTO — attest both the reality of the founding problem and its recurrence risk, and IMF surveillance together with academic monitoring documents repeated trade-conflict episodes since 2008. Developing-country trade ministries, who are payers rather than beneficiaries, corroborate that the original problem was real while disputing this reading's claim that symmetric obligation is the correct remedy.
narrative_ontology:disappearance_verdict(wto_treaty_framework__market_access_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_treaty_framework__market_access_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_treaty_framework__market_access_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_treaty_framework__market_access_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_treaty_framework__market_access_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__market_access_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_treaty_framework__market_access_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_treaty_framework__market_access_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75 at interval end) because the obligation strips policy instruments from exactly the members least able to substitute for them, while the largest beneficiaries retain de facto exceptions (agricultural subsidies, security carve-outs) that the formal symmetry does not touch. Suppression (0.50) is a raw structural property, unscaled by power or scope: it reflects legally binding obligations backed by authorized retaliation and market-access leverage, moderated by the fact that exit (withdrawal) technically exists and enforcement capacity has visibly decayed. Theater (0.32) captures real machinery — thousands of tariff bindings, hundreds of disputes, standing review bodies — increasingly diluted by performative ministerials after the Doha collapse. Accessibility_collapse (0.60): the industrial-policy toolkit is substantially but not completely closed — Article XX general exceptions, bound-tariff water, LDC flexibilities, and safeguard channels remain. Resistance (0.65) is high and organized: the Cancún revolt and G20/G90 coalitions, the Appellate Body blockage, PTA proliferation, and the contemporary industrial-subsidy race are all active pushback from targeted seats. The temporal series run on one shared seven-point grid (every tracked metric authored at every point, 1995–2025 anchors): base_extractiveness accumulates monotonically as TRIPS/TRIMS/GATS phase in and the Trade Facilitation Agreement shifts customs costs onto poor administrations; theater_ratio climbs as round-negotiation turns performative; suppression_requirement DECLINES deliberately — the story traces enforcement-capacity decay (Appellate Body paralysis from 2019, open non-compliance by major members), not intensification, and the scalar suppression reflects the end-state of that decay. There is no cyclical dynamic; drift is monotonic.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute fundamentally different constraints from identical treaty text. From the major-trading-powers position the arrangement is an achievement they built: orderly openness, predictable access, adjudicated disputes — a rope they administer. From the infant-industry and smallholder positions the same structure is enforced dispossession of the developmental toolkit, sustained by access leverage they cannot refuse. Developing-country governments straddle the divide — paying policy space for a rule-based shield they genuinely value — which is why their seat is the pivot on which the tangled_rope classification turns. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Major trading powers sit nearest the beneficiary pole (d near 0.0): they collect the arrangement's principal gains, wrote its rules, and hold arbitrage-grade exit through mega-regionals. Multinational corporations follow (arbitrage exit, direct rent collection), then mobile advanced-economy exporters and services industries. At the target pole: smallholder farmers and infant industries — trapped, with the latter's whole economic function (domestic learning) being what the constraint removes — sit nearest d = 1.0. Developing-country governments derive high d from payer status and constrained exit, damped slightly by their secondary beneficiary position (the DSU shield). Academic observers sit at the symmetric midpoint. The beneficiary/victim declarations map onto real flows: rents flow to cross-border firms and exporting sectors; instrument loss and subsidized competition fall on domestic producers and the governments responsible for them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protectionist spirals of the interwar type — remains live, so this is not a mandatrophy case and the constraint is not drifting toward piton on mandate obsolescence. The classification work the framework performs here is preventing mislabeling in both directions: a pure-snare reading would erase the coordination function that small members affirmatively defend (their DSU shield is worth real concessions to them), and a pure-rope reading would erase the documented asymmetric extraction running through the same MFN structure. Tangled_rope holds both truths: genuine collective-action solution AND asymmetric extraction requiring active enforcement. The temporal series monitor the two drift risks separately — extraction accumulation (rising ε) signals rent-layering onto coordination; enforcement decay (falling suppression_requirement) with retained obligations signals the piton trajectory in which the constraint persists theatrically. The omega on enforcement-decay interpretation carries that ambiguity forward unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the wto_treaty_framework kernel — the market_access_reading. What would the sibling developmental_reading change structurally, and where exactly is the disagreement located?',
    'Compile and compare the sibling story: the developmental reading reclassifies S&D provisions from transitional exceptions to permanent structural accommodations, elevates technology-transfer obligations to core commitments, moves infant industries from victim set toward protected-rights holders, and authors a higher ε over the same treaty referent. The disagreement is located in two specific structural elements: the ontological status of S&D (temporary exception vs. permanent accommodation) and whether development policy space is a concession granted by the liberalization bargain or an equal-status commitment the bargain must respect.',
    'If the developmental reading is adopted as the framework''s operative meaning, the victim and beneficiary sets partially invert, ε rises further, and the constraint trends toward snare from that seat; this reading''s transitional axiom becomes the contested element rather than settled background.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file is the market-access reading of the WTO kernel; the sibling developmental reading is a separate constraint with different ε, victim sets, and S&D ontology.').

omega_variable(
    formal_symmetry_substantive_asymmetry,
    'Does identical formal obligation across radically unequal economies constitute extraction, or the fair price of a rules-based system that shields weak states from power politics?',
    'Counterfactual comparison of developing-country outcomes inside versus outside the framework, controlling for size and initial conditions, against the historical record of the policy toolkits available to earlier late-industrializers (United States, Germany, Japan, Korea) under no comparable constraint.',
    'If the DSU shield value exceeds the instrument loss for typical small members, ε is overstated and the constraint leans rope; if instrument loss dominates, the formal symmetry is extraction wearing equality''s clothing and the measured ε understates the harm to trapped seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_symmetry_substantive_asymmetry, empirical, 'Whether formal symmetry of obligation is substantively fair or a mechanism of asymmetric extraction.').

omega_variable(
    sd_transitional_axiom_validity,
    'Have S&D provisions actually functioned as transitions to full symmetric participation — this reading''s foundational premise — or as indefinite fig leaves over a permanently asymmetric arrangement?',
    'Track graduation rates across the interval: how many members moved from S&D-reliant to fully reciprocal participation, and whether graduation tracked genuine capability convergence or was blocked by persistent subsidy asymmetries in agriculture and services.',
    'If transitions systematically fail to complete, the reading''s empirically contingent foundational axiom loses its warrant, strengthening the engine''s foreclosure computation against the developmental sibling and undermining this reading''s own legitimacy account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sd_transitional_axiom_validity, empirical, 'Empirical validity of the transitional-exception premise on which the market-access reading rests.').

omega_variable(
    enforcement_decay_interpretation,
    'Does the falling suppression_requirement series indicate healthy internalization of rules (normalization) or terminal enforcement decay (piton drift)?',
    'Compare compliance rates on panel and Appellate Body rulings before and after the 2019 appointment blockage; count openly defiant measures by major members; test whether preferential agreements replicate and independently enforce equivalent disciplines.',
    'Normalization supports continued tangled_rope stability; decay with retained obligations pushes toward piton — obligations persisting theatrically, unenforced — or toward fragmentation into power-based bilateralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_interpretation, empirical, 'Ambiguity in the enforcement-decay trajectory: normalization versus piton drift.').

omega_variable(
    membership_net_benefit_distribution,
    'Is the aggregate net benefit of membership positive for late-developing members, and how is that surplus distributed between domestic constituencies and foreign firms?',
    'Member-level welfare accounting separating terms-of-trade effects, TRIPS royalty outflows, and DSU shield value; firm-level profit data compared against host-country wage and productivity gains in global-value-chain sectors.',
    'A positive and broadly shared surplus supports the coordination-function half of the tangled_rope claim; concentrated outflows to foreign firms support the extraction half and raise effective extraction for the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_net_benefit_distribution, empirical, 'Distribution of the membership surplus between domestic constituencies and foreign rent collectors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__market_access_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_treaty_framework__market_access_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(wto__tr_t5, wto_treaty_framework__market_access_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(wto__tr_t10, wto_treaty_framework__market_access_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(wto__tr_t15, wto_treaty_framework__market_access_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(wto__tr_t20, wto_treaty_framework__market_access_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(wto__tr_t25, wto_treaty_framework__market_access_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement(wto__tr_t30, wto_treaty_framework__market_access_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_treaty_framework__market_access_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(wto__be_t5, wto_treaty_framework__market_access_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(wto__be_t10, wto_treaty_framework__market_access_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(wto__be_t15, wto_treaty_framework__market_access_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(wto__be_t20, wto_treaty_framework__market_access_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(wto__be_t25, wto_treaty_framework__market_access_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(wto__be_t30, wto_treaty_framework__market_access_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_treaty_framework__market_access_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(wto__su_t5, wto_treaty_framework__market_access_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(wto__su_t10, wto_treaty_framework__market_access_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(wto__su_t15, wto_treaty_framework__market_access_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(wto__su_t20, wto_treaty_framework__market_access_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(wto__su_t25, wto_treaty_framework__market_access_reading, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(wto__su_t30, wto_treaty_framework__market_access_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__market_access_reading, resource_allocation).
narrative_ontology:affects_constraint(wto_treaty_framework__market_access_reading, wto_treaty_framework__developmental_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the WTO treaty framework' conflates two structurally distinct readings of one kernel. This file instantiates the market_access_reading: liberalization as symmetric universal obligation, market access as primary purpose, S&D as transitional exception. The sibling file wto_treaty_framework__developmental_reading instantiates the developmental reading: policy space as equal-status commitment, S&D as permanent structural accommodation, technology transfer as core obligation. The readings share a referent — the standing WTO obligations — and differ in ε, victim sets, and the ontological status of S&D; per the ε-invariance principle they are modeled as two linked stories, not one story with a measurement parameter. The upstream reading (higher institutional entrenchment, control of agenda-setting) influences the downstream reading's operating environment without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
