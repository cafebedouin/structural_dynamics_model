% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: Trade Agreement Text as Sovereign-Subordinate Coordination Mechanism (Sovereignty-Primacy Reading)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primacy reading of the NAFTA
 *   jurisdictional-boundary kernel: trade agreement text operates as a
 *   coordination mechanism strictly subordinate to sovereign domestic law,
 *   and each state retains full regulatory authority over labor,
 *   environmental, and health standards inside its territory. On this reading
 *   the treaty enters domestic life only through ratification and
 *   implementing statute; obligations weigh on ministries as voluntary
 *   compliance costs, never as overriding commands; and the arrangement's
 *   costs beyond those are side-effects of liberalization that retained
 *   domestic channels can remedy. KEY AGENTS (by structural relationship):
 *   signatory_governments — joint principals and agenda-setters
 *   (institutional/mobile) who may withdraw on notice; free_trade_commission
 *   — standing administrative organ (institutional/constrained) acting on
 *   delegated consent; exporting_firms — primary beneficiaries
 *   (powerful/mobile); foreign_investors — conditional beneficiaries through
 *   the investor-state channel (powerful/arbitrage);
 *   domestic_regulatory_agencies — jurisdiction-preserving beneficiaries
 *   (institutional/constrained); consumers — diffuse beneficiaries
 *   (moderate/mobile); import_competing_producers and
 *   displaced_manufacturing_workers — adjustment-cost bearers
 *   (organized/constrained and moderate/trapped); constitutional_courts —
 *   analytical observers enforcing the subordination doctrine
 *   (institutional/analytical); labor_environmental_advocates — excluded
 *   voices (moderate/trapped). Constraint-family note: the colloquial label
 *   'what the trade agreement does to domestic law' decomposes into three
 *   structurally distinct claims — this file authors the low-extraction
 *   sovereignty-primacy reading;
 *   nafta_jurisdictional_boundary__capital_supremacy_reading authors the
 *   override hierarchy with substantially higher epsilon and
 *   regulatory-autonomy victims;
 *   nafta_jurisdictional_boundary__embedded_liberalism_reading authors an
 *   intermediate policy-space balance. Same text, different asserted legal
 *   hierarchy, therefore different epsilon, victim sets, and classifications;
 *   all three are linked via network.affects_constraints. Claim and metrics
 *   are independent authored facts: the claimed type reflects what this
 *   reading makes structurally true, the metrics describe the arrangement's
 *   actual operation over 1994-2020.
 *
 * KEY AGENTS:
 *   - signatory_governments: joint principals and agenda-setters (institutional/mobile) — negotiate, administer, and may withdraw on six months' written notice
 *   - free_trade_commission: standing administrative organ (institutional/constrained) — supervises implementation and dispute procedures on delegated consent alone
 *   - exporting_firms: primary beneficiary seat (powerful/mobile) — hold duty-free access and integrated cross-border supply chains
 *   - foreign_investors: conditional beneficiary seat (powerful/arbitrage) — collect episodic awards through the investor-state channel; relocate capital freely
 *   - domestic_regulatory_agencies: jurisdiction-preserving beneficiaries (institutional/constrained) — keep full standard-setting authority within their territories
 *   - consumers: diffuse beneficiaries (moderate/mobile) — receive lower prices and wider selection
 *   - import_competing_producers: adjustment-cost payers (organized/constrained) — bear liberalization's competitive losses while retaining domestic petition channels
 *   - displaced_manufacturing_workers: concentrated loss-bearers (moderate/trapped) — absorb plant closures with geographically bound exit
 *   - constitutional_courts: analytical observers (institutional/analytical) — enforce the subordination doctrine case by case
 *   - labor_environmental_advocates: excluded voices (moderate/trapped) — no negotiating seat; act only through domestic politics and publicity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.1).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "Trade Agreement Text as Sovereign-Subordinate Coordination Mechanism (Sovereignty-Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '48b70ab3-e30e-435e-97be-525abbb2c57d').
narrative_ontology:cs_kernel_codification('48b70ab3-e30e-435e-97be-525abbb2c57d', fixed_text).
narrative_ontology:cs_authority_grounding('48b70ab3-e30e-435e-97be-525abbb2c57d', distributed).
narrative_ontology:cs_reading_relation('48b70ab3-e30e-435e-97be-525abbb2c57d', nafta_jurisdictional_boundary__capital_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('48b70ab3-e30e-435e-97be-525abbb2c57d', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('48b70ab3-e30e-435e-97be-525abbb2c57d', foundational, treaty_obligations_enter_as_compliance_costs_only).
narrative_ontology:cs_axiom_status(treaty_obligations_enter_as_compliance_costs_only, holdable).
narrative_ontology:cs_axiom_grounding('48b70ab3-e30e-435e-97be-525abbb2c57d', treaty_obligations_enter_as_compliance_costs_only, conventional).
narrative_ontology:cs_axiom('48b70ab3-e30e-435e-97be-525abbb2c57d', foundational, territorial_regulatory_authority_is_inalienable).
narrative_ontology:cs_axiom_status(territorial_regulatory_authority_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('48b70ab3-e30e-435e-97be-525abbb2c57d', territorial_regulatory_authority_is_inalienable, deontological).
narrative_ontology:cs_reference_frame('48b70ab3-e30e-435e-97be-525abbb2c57d', domestic_supremacy_compact).
narrative_ontology:cs_drift_state('48b70ab3-e30e-435e-97be-525abbb2c57d', contemporary_usmca_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('48b70ab3-e30e-435e-97be-525abbb2c57d', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_governments).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, free_trade_commission).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_firms).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, foreign_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, consumers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_producers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, displaced_manufacturing_workers).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, dualist_treaty_implementation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The three national governments negotiated, ratified, and periodically renegotiate the agreement. They set its rules through ministerial commissions, appoint panelists, and decide whether to comply with adverse rulings, pay awarded damages, or withdraw altogether on six months' written notice. Tariff revenue forgone and adjustment-assistance spending flow out; secure market access for home exporters and predictable dispute management flow in. Exit looks like a formal withdrawal notice, reversion to prior tariff schedules, and fallback to WTO rules.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_governments, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_governments, beneficiary).

% A standing ministerial organ of the three governments that supervises implementation, issues interpretive notes, and administers dispute-settlement procedures. It convenes regularly, maintains working groups, and can accelerate panel timelines. It holds no independent treasury or coercive apparatus; its influence rests entirely on the continuing consent of its member governments.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, free_trade_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Manufacturers and agribusinesses selling across the three borders. They gained duty-free treatment under the agreement's schedules and built integrated supply chains around it. Expanded market access and lower input costs flow to them; lobbying support for continuation flows from them. Production can in principle shift elsewhere, but their installed cross-border footprints make continuation their strong preference.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_firms, beneficiary,
    powerful, biographical, mobile, continental).

% Companies and funds owning assets in a partner country. They may bring claims against a host government before investor-state tribunals when public measures harm their investments, and they collect damages when they win. Host governments keep the choice to pay, resist, or legislate around awards. Their capital is highly mobile across jurisdictions, which is precisely why host states compete to host it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, foreign_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Environmental, labor, health, and safety bureaucracies inside each government. Under the agreement's terms they keep full authority to set and enforce standards within their territory; trade obligations reach them only as considerations their ministries weigh, not as commands. Preserved jurisdiction and modest side-accord cooperation funding flow to them; occasional defenses of challenged measures flow from them.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Households buying imported goods. Lower tariffs translate into lower prices and wider product selection. They carry residual costs indirectly through the taxes that fund adjustment programs. Their exit is ordinary shopping behavior; they are diffuse, organizing politically rarely and mostly through price effects rather than representation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, consumers, beneficiary,
    moderate, immediate, mobile, national).

% Firms producing goods that compete with imports from partner countries. Liberalized access eroded their protected margins and some sectors contracted sharply. They retain a domestic political channel — petitions for tariffs, safeguards, antidumping duties — which their governments remain free to grant at the cost of compensating partners. Exit means retooling, consolidating, or relocating production, all expensive.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_producers, payer,
    organized, biographical, constrained, national).

% Production workers in regions where import competition eliminated plants, concentrated in furniture, apparel, electronics assembly, and auto parts. Losses arrive as job loss, depressed local wages, and reduced mobility. Retraining programs exist but reached a minority of those eligible. Exit is geographically and skill-bound: mortgages, family networks, and non-transferable skills hold them in place while they wait on domestic politics for relief.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, displaced_manufacturing_workers, payer,
    moderate, biographical, trapped, regional).

% National supreme and constitutional courts in the three countries. They adjudicate whether treaty obligations displace domestic statutes and have consistently held that the agreement operates only through domestic implementing legislation — in their jurisprudence the treaty text never strikes down a national law on its own force. They rule on the boundary itself rather than collecting or bearing anything from the arrangement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Unions and environmental organizations that sought seats at the negotiating table and received none; they forced the labor and environmental side accords through public pressure only after the core text was largely settled. They contest investor-state claims and campaign for enforceable labor chapters. They hold no formal position in the agreement's institutions and can act only through domestic politics and publicity.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_environmental_advocates, excluded,
    moderate, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__sovereignty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single trilateral schedule for tariff elimination, common rules of origin, and standing consultation and panel procedures, replacing a web of bilateral bargains renegotiated product by product; it solves the mutual tariff-escalation and investment-rule uncertainty problems once, centrally, instead of repeatedly.
% TRANSFER_FUNCTION: Moves tariff revenue from member treasuries to importers and consumers through duty elimination; moves secure market access to exporters; concentrates adjustment costs on import-competing sectors and their workforces; routes occasional damage awards from host-government treasuries to successful investor claimants.
% ABSENT_VOICES: Labor unions, environmental organizations, and communities affected by investor claims had no negotiating seats; the side accords answered their post-hoc pressure but left them outside the core institutions. Under this reading their remedy is the retained domestic authority the reading itself guarantees — the advocates dispute that a guaranteed domestic voice substitutes for a seat at the table, and that dispute has no forum inside the arrangement.
% DISAPPEARANCE_RATIONALE: Automotive, agricultural, and energy supply chains calibrated to duty-free access would reprice within months; tariff schedules would revert unless renegotiated; dispute management would fall back to WTO procedures; border-region economies built on integrated production would contract. Nothing collapses — the WTO layer persists and each state's regulatory authority never depended on the text — but the trilateral arrangement would visibly rearrange.
% FOUNDING_PROBLEM: Mutual tariff escalation and unpredictable market access among the three North American economies, compounded by investor uncertainty about expropriation and currency regimes after the 1980s debt crises — a problem the Canada-U.S. FTA had begun addressing bilaterally.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic trade economists document the continued dependence of integrated supply chains on preferential access; import-competing industries' and unions' contemporaneous legislative testimony confirms the disruption the bargain priced; and the fact that all three governments chose renegotiation rather than lapse attests that the underlying coordination problem persists. No beneficiary party's self-report is load-bearing: the strongest corroboration comes from opponents' testimony and from scholarship outside the benefiting parties.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.18 at interval end) because on this reading treaty obligations enter the compliance-cost set rather than the constraint set: states pay for market access with scheduled concessions and the occasional award, and every such payment was and remains a choice they can revoke. Suppression is authored very low (0.10) because enforcement is consent-based — panels sit only when the parties let them, compliance with adverse rulings is discretionary, and Article 2205-style withdrawal is a standing option no one has exercised. Theater_ratio is moderate-low (0.22): tariff schedules and panel procedures did real work, but committee ritual and annual reporting grew steadily as a share of activity. Accessibility_collapse is low-moderate (0.35) because alternatives persist everywhere — WTO rules, bilateral deals, unilateral liberalization, withdrawal — and resistance (0.35) is real but episodic: the 1992 populist backlash and the 2016-2020 renegotiation pressure contested the bargain without dismantling it. The temporal series run on one shared seven-point grid so every tracked metric is authored at every examined time point. Extractiveness traces a single hump rather than monotonic drift: low at entry (0.13), accumulating through the investor-state claim era to a mid-interval peak (0.24) as awards and documented regulatory retreats piled up, then receding (0.18) as panel blockades and the renegotiation reasserted subordination. The suppression_requirement series is authored deliberately — the story specifically tracks enforcement-capacity change: Chapter 20 panel convening was routine at entry (0.28) and decayed steadily (0.10) as one member blocked panel formation for years, modeling enforcement erosion rather than ratcheting. Theater rises gently and dips at the end when renegotiation made the machinery consequential again. The oscillation is one accumulation-release cycle, not intermittent reinforcement: nothing in the record suggests the hump was engineered as a control device.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes materially different per-seat types from this one structural record. From the signatory-governments seat the arrangement is a coordination achievement they built, administer, and can exit at will — near-zero experienced extraction, a cooperative instrument. From the displaced-worker seat the same text operated as concentrated loss with no exit and only indirect political recourse — the highest experienced extraction in the story. Same nominal system, opposite experiences, driven by exit asymmetry: investors hold arbitrage-grade exit and therefore experience even adverse rulings as negotiable costs, while regionally trapped workers experience identical treaty effects as binding fate. Import-competing producers sit between — real losses, but organized power and a domestic petition channel that the sovereignty-primacy premise keeps open. The courts' analytical seat sees the boundary itself and vindicates the subordination doctrine regardless of which side of it a given dispute lands on. The engine derives this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. Signatory governments, the commission, exporting firms, domestic regulatory agencies, and consumers sit near the beneficiary end (low d): the arrangement subsidizes their access, jurisdiction, and purchasing power, and their exits range from mobile to arbitrage. Foreign investors sit slightly above the pure beneficiary pole: they collect episodic awards and their claim exposure induces caution in host regulators, but awards are small in aggregate and case-specific — leverage, not command. Import-competing producers and displaced workers sit near the target end (high d): they bear concentrated adjustment costs, with constrained and trapped exits respectively, and their remedy runs exclusively through the domestic politics this reading guarantees them. Constitutional courts occupy the analytical seat, exempt from the beneficiary-target axis. Two scaling notes the commentary must respect: suppression is a raw structural property and is NOT scaled by directionality or scope — only extractiveness is scaled, and the continental spatial scope amplifies effective extraction modestly for target seats because verification across three jurisdictions is harder. Receipt-surface discipline: gain_flow is authored as 'diffuse' as an affirmative checked claim — re-reading every stakeholder situation, benefits flow broadly (access, prices, jurisdiction) but the small extraction stream dissipates across treasuries, taxpayers, and workforces rather than accruing to any named seat; foreign_investors were considered and rejected as capturers because their award receipts are episodic and immaterial in aggregate, not a structural claim on the arrangement's value. fixing_cost is authored 'cheap': any member government can remove itself by written notice within six months, and since the benefit of fixing is small (extraction is low), the procedural cheapness dominates the cost class even though economic disruption would follow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mutual tariff escalation and unpredictable market access — remains live: all three governments chose renegotiation over lapse, and integrated supply chains still depend on preferential access, so founding_problem_status is live paired with disappearance_verdict world_rearranges; the mismatch consumer finds no dead-mandate flag here and correctly so. Mandatrophy analysis still earns its place in two directions. First, the rope classification prevents mislabeling: adjustment costs borne by workers and producers are side-effects of a voluntary liberalization bargain, remediable through retained sovereign channels, and must not be read as extraction-through-coercion for a capturer's rent — the diffuse receipt surface encodes exactly that refusal. Second, the temporal record guards against drift blindness: the mid-interval accumulation of investor-state extraction is visible in the hump, and had severity crossed warning thresholds the T17 trigger would emit an investigation hypothesis (without reclassifying) — the apparatus catches the reading's main failure mode, quiet migration toward the capital-supremacy sibling's structure, without prejudging it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This story is one reading — sovereignty_primacy_reading — of the kernel nafta_jurisdictional_boundary; the sibling readings capital_supremacy_reading and embedded_liberalism_reading instantiate different constraints from the same treaty text. Where exactly does the disagreement bite, and which hierarchy does practice actually enforce?',
    'Comparative doctrinal analysis of how the three domestic court systems and the investor-state tribunals treat the treaty-to-domestic-law hierarchy, plus amendment history: if tribunal awards are routinely enforced as overriding domestic statutes, the capital-supremacy sibling describes the operative constraint and this story''s epsilon understates it.',
    'If the capital-supremacy hierarchy becomes enforced reality, this reading''s low extraction is misattributed and the family''s classification migrates toward the sibling''s higher-extraction profile; the per-reading decomposition keeps each epsilon stable meanwhile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'One-of-three kernel readings; disagreement located in the hierarchy premise — compliance-cost entry versus override of domestic regulation.').

omega_variable(
    isds_chill_materiality,
    'Does investor-state arbitration exposure, and the regulatory caution it induces in officials who anticipate claims, constitute material extraction under this reading, or are awards and abandoned measures marginal noise?',
    'Aggregate investor-state award totals, counts of measures withdrawn or modified in response to claims, and survey evidence of official self-censorship attributed to claim exposure.',
    'Material chill would raise epsilon above the voluntary-compliance-cost band and push the computed per-seat types away from pure coordination toward a hybrid coordination-plus-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(isds_chill_materiality, empirical, 'Whether investor-state exposure adds real extraction to an otherwise voluntary arrangement.').

omega_variable(
    adjustment_cost_attribution,
    'How much of the wage and employment losses borne by exposed manufacturing regions is attributable to this arrangement rather than to automation and extra-regional import competition?',
    'Regional econometrics that separates intra-agreement trade shocks from technology and China-shock components using commuting-zone level data.',
    'Higher attribution enlarges the target-side directionality and effective extraction computed for the payer seats; near-zero attribution leaves extraction confined to voluntary compliance costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_cost_attribution, empirical, 'Attribution of displacement costs to the agreement rather than to confounding forces.').

omega_variable(
    voluntariness_depth,
    'Is compliance genuinely voluntary when exit carries reputational, market-confidence, and retaliation-expectation costs that no member government has been willing to bear?',
    'Examine actual withdrawal deliberations and asset-market reactions to withdrawal signals; if credible exit is priced as economically catastrophic, the voluntariness premise is nominal rather than real.',
    'If exit is effectively blocked, measured suppression understates the arrangement''s coercive floor and the computed type shifts toward enforced hybrid coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_depth, conceptual, 'Whether the withdrawal option makes participation meaningfully voluntary.').

omega_variable(
    side_agreement_functionality,
    'Do the labor and environmental side institutions perform real oversight or mainly ceremonial reporting?',
    'Compare side-institution caseload, findings, and any measure actually changed against their budgets and staffing levels over the interval.',
    'Highly ceremonial operation raises theater_ratio and supports reading the side accords as legitimation rather than functioning oversight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(side_agreement_functionality, empirical, 'Functional versus theatrical operation of the ancillary labor and environment bodies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_sovereignty_primacy_tr_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_tr_t0, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_tr_t4, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_tr_t4, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_tr_t8, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_tr_t8, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_tr_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_tr_t12, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_tr_t16, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_tr_t16, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_tr_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_tr_t20, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_tr_t26, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 26, 0.22).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(nafta_sovereignty_primacy_be_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_be_t0, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_be_t4, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 4, 0.16).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_be_t4, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_be_t8, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_be_t8, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_be_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_be_t12, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_be_t16, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_be_t16, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_be_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_be_t20, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_be_t26, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 26, 0.18).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(nafta_sovereignty_primacy_su_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_su_t0, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_su_t4, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 4, 0.24).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_su_t4, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_su_t8, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 8, 0.19).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_su_t8, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_su_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 12, 0.16).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_su_t12, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_su_t16, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 16, 0.13).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_su_t16, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_su_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_su_t20, observed).
narrative_ontology:measurement(nafta_sovereignty_primacy_su_t26, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 26, 0.1).
narrative_ontology:measurement_basis(nafta_sovereignty_primacy_su_t26, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'what the trade agreement does to domestic law.' The label conflates three structurally distinct claims distinguished by the legal hierarchy each asserts over the same treaty text. This story authors the sovereignty-primacy reading: obligations as voluntary compliance costs, full retained regulatory authority, low epsilon, adjustment costs as liberalization side-effects. Sibling nafta_jurisdictional_boundary__capital_supremacy_reading authors the override hierarchy — substantially higher epsilon, regulatory-autonomy victims, enforcement-dependent persistence. Sibling nafta_jurisdictional_boundary__embedded_liberalism_reading authors the balanced policy-space framework — intermediate epsilon, discrimination-testing as the boundary mechanism. Upstream/downstream structure among siblings: domestic-court subordination doctrine (this reading's evidentiary base) is cited by embedded-liberalism proponents as proof that policy space survives, and attacked by capital-supremacy proponents as ignoring tribunal practice; each sibling cites the same text, so all three are mutually linked here and each carries its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
