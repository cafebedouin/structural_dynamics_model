% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: Trade Agreement as Sovereign-Subordinate Coordination Instrument (Sovereignty Primacy Reading)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   Three sovereign states operate a shared trade agreement as a coordination
 *   instrument: agreed tariff schedules, rules of origin, customs procedures,
 *   and a panel system for disputes, all implemented through ordinary
 *   domestic statute and subordinate to each state's legislative supremacy.
 *   Regulatory agencies issue labor, environmental, and health rules for
 *   their own territories without external direction; treaty committees
 *   receive information and hear disputes, and governments decide outcomes.
 *   Participation is maintained by continuing consent: implementing
 *   legislation can be amended, safeguards granted, and the whole text
 *   withdrawn on notice — a revisability exercised in practice when the
 *   member governments renegotiated and replaced the original agreement. The
 *   arrangement's costs register as compliance spending, adjustment in
 *   import-exposed sectors, and professional fees in the dispute machinery.
 *   KEY AGENTS (by structural relationship): member_state_governments — joint
 *   agenda setters (institutional/mobile), negotiate, implement by statute,
 *   withdraw on notice; export_oriented_manufacturers — primary beneficiaries
 *   (organized/mobile), hold preferential access; cross_border_consumers —
 *   diffuse beneficiaries (moderate/mobile); import_competing_producers —
 *   cost-bearing seat (organized/constrained), petition domestic government
 *   for relief; domestic_regulatory_agencies — jurisdictional beneficiaries
 *   (institutional/identity_locked), constituted by the territorial authority
 *   the boundary preserves; trade_dispute_panels — machinery staff
 *   (moderate/mobile), findings bind no legislature;
 *   labor_environmental_advocates — excluded seat (organized/constrained);
 *   constitutional_international_law_scholars — analytical observers
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.16).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.1).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "Trade Agreement as Sovereign-Subordinate Coordination Instrument (Sovereignty Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, export_oriented_manufacturers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_consumers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, member_state_governments).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_producers).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, dualist_treaty_implementation_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, legislative_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and signed the treaty text and administer it jointly through a trilateral commission and ministerial consultations. Domestic legislatures implement obligations by ordinary statute and can amend or repeal that implementation at will; the treaty carries a withdrawal clause exercisable on six months' notice. Governments collect the coordination gains — predictable market access for their exporters, cheaper imports for their consumers — while keeping every domestic regulatory decision inside their own legislatures and agencies.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, member_state_governments, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, member_state_governments, beneficiary).

% Sell into neighboring markets at preferential tariff rates under agreed rules of origin. They plan plants and supply chains around the schedule of concessions and press their governments to defend the text when partners propose changes. If terms sour they can redirect output to other markets or relocate production, though relocation carries real cost.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, export_oriented_manufacturers, beneficiary,
    organized, biographical, mobile, continental).

% Buy goods whose prices and variety reflect lowered tariffs and expanded supplier pools. They are diffuse and do not organize around trade policy specifically; their stake registers through elections and purchasing choices rather than through seats in the treaty's committees.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_consumers, beneficiary,
    moderate, biographical, mobile, continental).

% Face competition from imports that entered under scheduled tariff reductions. Some modernized and shifted product mix; others petition their own governments for safeguards, antidumping duties, or adjustment aid — channels that remain fully open because domestic law governs within the territory. When a partner challenges a granted safeguard, the government weighs the dispute outcome against the domestic policy objective and decides.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_producers, payer,
    organized, biographical, constrained, national).

% Write and enforce labor, environmental, and health rules for their territories under domestic statutes. The treaty's committees may receive their measures as information; no tribunal directs them to issue or withhold a rule. Agency mandates, career paths, and statutory authorities are built entirely around territorial jurisdiction — the agencies are constituted by the authority this boundary preserves, and they cannot operate under a different premise without ceasing to be the institutions they are.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, identity_locked, national).

% Staff the arbitration and panel machinery that hears disputes between the member governments. Panels issue findings and, in the investment chapter, assess compensation; governments decide whether to comply, renegotiate, or absorb the consequences. Panelists and secretariat staff build careers on caseload, giving them a stake in the machinery's continued operation, though their findings bind no legislature.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels, beneficiary,
    moderate, biographical, mobile, continental).

% Seek enforceable transnational standards on labor conditions and environmental protection. The treaty's design gives them consultative committees and submission processes rather than sanction-backed chapters; their proposals for binding cross-border enforcement sat outside the adopted architecture. They continue organizing through domestic politics and public campaigns.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_environmental_advocates, excluded,
    organized, biographical, constrained, continental).

% Analyze how the treaty text, implementing statutes, and panel findings interact, and document instances where governments invoked domestic legislative supremacy to discount or reshape adverse findings. They publish from university and institute seats and take no side in the commercial flows.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, constitutional_international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of cross-border commerce under fragmented national rules: tariff schedules, rules of origin, customs procedures, and market-access commitments are harmonized once, trilaterally, instead of negotiated bilaterally per firm and product, and a standing panel system gives traders predictable interpretation of the shared text.
% TRANSFER_FUNCTION: Moves market access and tariff concessions reciprocally among the member states (each lowers barriers to the others' exports); moves compliance and administrative costs onto firms and customs administrations; moves dispute-resolution workload onto the panel machinery. Every flow runs through voluntary, statutorily implemented participation that domestic legislation can reverse.
% ABSENT_VOICES: Labor and environmental advocates seeking enforceable transnational standards were given consultative committee seats but no sanction-backed chapter; import-displaced workers' communities hold no seat in the treaty architecture at all, their recourse running entirely through domestic politics. Under this reading's design that exclusion is intentional — domestic law is the proper venue — but the excluded voices object that the 'proper venue' has historically delivered slow and inadequate adjustment.
% DISAPPEARANCE_RATIONALE: Tariff schedules, rules-of-origin supply chains, and trilateral committee functions would unwind overnight: cross-border supply chains would reprice under reverted tariff schedules, exporters would lose preferential access, customs procedures would re-fragment, and the dispute machinery would dissolve. A large rearrangement — though each element is revertible through the same domestic-law channels the reading preserves.
% FOUNDING_PROBLEM: The interwar tariff-war spiral left states wanting market access for their exporters without surrendering domestic regulatory authority: a standing coordination device that locks in reciprocal liberalization while leaving domestic law supreme within each territory.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: contemporaneous legislative ratification debates in all three capitals record market fragmentation and tariff-war memory as the motivating problem; GATT/WTO institutional histories and international-political-economy scholarship document the founding bargain; import-competing industry testimony in ratification hearings attests the cost side. No corroborating source is a treaty beneficiary.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.16 at interval end) because obligations enter the compliance-cost set without overriding force: the heaviest recorded impositions were compensation assessments governments could accept, contest, or renegotiate, and the end-state figure sits barely above the inherent transaction cost of any multi-party resource-allocation mechanism. Suppression is low (0.10) because the arrangement holds through reciprocity and mutual interest rather than active coercive maintenance — implementing statutes are amendable, safeguards grantable, withdrawal available on notice, and all three levers were visibly exercised. Theater ratio (0.30) reflects a growing symbolic layer — consultative committees, environmental cooperation reports, summit communiques — accumulating atop a functional coordination core. Accessibility collapse is moderate-low (0.35): alternatives (legislative override, safeguard action, withdrawal, renegotiation) remain visible and demonstrably usable, as the replacement of the original agreement showed. Resistance (0.35) is real and was effective: the political backlash of the late 2010s did not merely protest the arrangement, it rewrote it — behavior consistent with a constraint that answers to its participants rather than one that must be defended against them. The temporal series run on one shared grid (1994/2000/2006/2012/2018/2026) with every tracked metric authored at every point; the hump shape in both extractiveness and suppression_requirement tracks the investor-state arbitration era (machinery built up through the mid-2000s, then deliberately rolled back when the successor agreement narrowed it) — this is a genuine enforcement-capacity dynamic, which is why suppression_requirement is authored here rather than left to the static scalar. The 2026 points are marked projected: authored assessment of the successor agreement's matured operation. Claim and metrics are independent authored facts: the rope claim states the structure as this reading holds it; the metrics state the operation as descriptively assessed from this seat.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently from the same treaty text. From the member-government seat the arrangement is a revocable instrument they wrote and can rewrite — an extension of domestic policy, not a limit on it. From the exporter seat it is purchased access worth defending. From the import-competing producer seat it is imposed exposure, mitigable only through the domestic channels the boundary preserves. From the regulatory-agency seat it is constitutive protection: the boundary is what the agency is made of, so no vantage inside the agency can even frame the alternative readings as live options. From the panel seat it is a livelihood-generating caseload whose outputs bind no one. The engine derives these divergent classifications from the structural data; this story does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: exporters, consumers, governments, and agencies all sit near the beneficiary end, with governments doubly positioned (agenda setters who also collect the coordination gains). The agencies' identity_locked exit deepens rather than complicates their beneficiary position — their stake in the boundary is constitutive, not incidental. Import_competing_producers are authored as a payer seat but deliberately NOT declared in base_properties.victims: under this reading their exposure is the reciprocal bargain's agreed price, addressable through retained domestic authority (safeguards, antidumping action, adjustment programs) rather than extraction borne without recourse — the open domestic safety valve is the reading's defining feature, and declaring them victims would assert the rival readings' conclusion from inside this one. No directionality_overrides are authored: the override mechanism keys on power atoms, and this story contains same-power seats with opposed structural relationships (organized exporters vs. organized import-competing producers; institutional governments vs. institutional agencies), so any power-atom-level override would collide across seats the derivation already distinguishes correctly through roles and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented national rules taxing cross-border exchange — remains live as long as sovereign states with distinct regulatory regimes trade, and the disappearance verdict is world_rearranges with stakeholders on every seat: the live-problem/live-arrangement pairing produces no dead-mandate signal. The classification discipline matters here in two directions. Reading the arrangement as pure extraction (as the arrangement's fiercest critics do) mistakes voluntary compliance costs and reciprocal concession prices for coerced transfer, ignoring the demonstrated revisability that distinguishes a bargain from a trap. Reading it as an irreversible feature of the integration landscape (the inevitability narrative) erases the withdrawal clause, the legislative override, and the actual replacement of the original text — treating a revisable instrument as bedrock. The rope claim, with cheap fixing cost and diffuse gain receipt, keeps both errors out: the arrangement is neither a snare to abolish nor a summit to accept, but a standing bargain its principals continuously re-author.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the sovereignty_primacy_reading of the nafta_jurisdictional_boundary kernel; would the capital_supremacy or embedded_liberalism readings of the same treaty text yield a different constraint with different epsilon, beneficiaries, and victims?',
    'Compare the sibling stories compiled from the same kernel: identical referent (the treaty''s jurisdictional operation), divergent epsilon and victim sets. The disagreement is located in one structural element — whether treaty text possesses hierarchical force over subsequent domestic legislation.',
    'Under the capital_supremacy reading the same text becomes an overriding constraint with high epsilon and domestic_regulatory_agencies as victims; under the embedded_liberalism reading a balanced framework with partial policy space. This story''s low epsilon and rope structure hold only within the sovereignty-primacy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints from the same text.').

omega_variable(
    isds_chilling_effect_residual,
    'Does the mere availability of investor-state compensation chill domestic regulatory initiatives beyond the voluntary compliance cost this reading counts?',
    'Difference-in-differences on regulatory initiation rates in arbitration-exposed sectors versus unexposed sectors before and after investor-state arbitration became available under the investment chapter.',
    'A measurable chilling effect would mean effective extraction exceeds the voluntary-cost account, pushing the computed classification away from pure coordination even within this reading''s own referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isds_chilling_effect_residual, empirical, 'Residual regulatory chilling attributable to compensation availability alone.').

omega_variable(
    retaliation_price_vs_coercion,
    'Is suspension of trade concessions following an adverse panel finding a contractual price that preserves voluntariness, or structural coercion that suppresses the regulatory alternative?',
    'Examine whether states in fact exercise the regulatory alternative after absorbing retaliation, and whether retaliation magnitudes track the regulated good''s political salience rather than the harm established.',
    'If retaliation functions as coercion, suppression is understated and the arrangement carries a hybrid coordination-plus-pressure component; if it prices like contract damages, the coordination reading stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_price_vs_coercion, conceptual, 'Whether countermeasure suspension is a price or a coercive barrier — locates the exit-options characterization of member states.').

omega_variable(
    membership_lock_in_voluntariness,
    'Does decades-deep supply-chain integration make membership effectively involuntary despite the legal withdrawal right, converting formally mobile exit into constrained exit?',
    'Estimate switching costs from trade-dependence ratios and supply-chain asset specificity; observe whether withdrawal threats are honored or discounted in practice, and at what political cost.',
    'High lock-in would raise effective extraction for the member-state seats and erode the voluntariness premise on which this reading''s low epsilon rests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_lock_in_voluntariness, empirical, 'Whether legal mobility of exit survives economic entanglement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_sov_primacy_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.14).
narrative_ontology:measurement_basis(nafta_sov_primacy_tr_t1994, observed).
narrative_ontology:measurement(nafta_sov_primacy_tr_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement_basis(nafta_sov_primacy_tr_t2000, observed).
narrative_ontology:measurement(nafta_sov_primacy_tr_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2006, 0.21).
narrative_ontology:measurement_basis(nafta_sov_primacy_tr_t2006, observed).
narrative_ontology:measurement(nafta_sov_primacy_tr_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement_basis(nafta_sov_primacy_tr_t2012, observed).
narrative_ontology:measurement(nafta_sov_primacy_tr_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement_basis(nafta_sov_primacy_tr_t2018, observed).
narrative_ontology:measurement(nafta_sov_primacy_tr_t2026, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2026, 0.3).
narrative_ontology:measurement_basis(nafta_sov_primacy_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(nafta_sov_primacy_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.13).
narrative_ontology:measurement_basis(nafta_sov_primacy_be_t1994, observed).
narrative_ontology:measurement(nafta_sov_primacy_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.16).
narrative_ontology:measurement_basis(nafta_sov_primacy_be_t2000, observed).
narrative_ontology:measurement(nafta_sov_primacy_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.21).
narrative_ontology:measurement_basis(nafta_sov_primacy_be_t2006, observed).
narrative_ontology:measurement(nafta_sov_primacy_be_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2012, 0.2).
narrative_ontology:measurement_basis(nafta_sov_primacy_be_t2012, observed).
narrative_ontology:measurement(nafta_sov_primacy_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.18).
narrative_ontology:measurement_basis(nafta_sov_primacy_be_t2018, observed).
narrative_ontology:measurement(nafta_sov_primacy_be_t2026, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2026, 0.16).
narrative_ontology:measurement_basis(nafta_sov_primacy_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(nafta_sov_primacy_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.1).
narrative_ontology:measurement_basis(nafta_sov_primacy_su_t1994, observed).
narrative_ontology:measurement(nafta_sov_primacy_su_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2000, 0.16).
narrative_ontology:measurement_basis(nafta_sov_primacy_su_t2000, observed).
narrative_ontology:measurement(nafta_sov_primacy_su_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2006, 0.24).
narrative_ontology:measurement_basis(nafta_sov_primacy_su_t2006, observed).
narrative_ontology:measurement(nafta_sov_primacy_su_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2012, 0.2).
narrative_ontology:measurement_basis(nafta_sov_primacy_su_t2012, observed).
narrative_ontology:measurement(nafta_sov_primacy_su_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2018, 0.15).
narrative_ontology:measurement_basis(nafta_sov_primacy_su_t2018, observed).
narrative_ontology:measurement(nafta_sov_primacy_su_t2026, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2026, 0.1).
narrative_ontology:measurement_basis(nafta_sov_primacy_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the NAFTA jurisdictional boundary' decomposes into three structurally distinct constraints — one per reading of the kernel nafta_jurisdictional_boundary — per the epsilon-invariance principle. Each reading authors its own epsilon over the same referent (the treaty's jurisdictional operation as it stands): this sovereignty-primacy story authors low epsilon (obligations as voluntary compliance costs, no overriding force, full regulatory retention); the capital-supremacy sibling authors high epsilon with regulatory agencies as victims of overriding obligations; the embedded-liberalism sibling authors intermediate epsilon with partial but genuine policy space. Family links run through network.affects_constraints in all three files. The capital-supremacy reading sits upstream: its enforcement expansions supplied the drift that this reading's revival_pressure answers, and its text is what this reading's revival reconstructed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
