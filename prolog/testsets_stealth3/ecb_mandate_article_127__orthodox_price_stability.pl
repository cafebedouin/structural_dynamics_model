% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate - Orthodox Price-Stability Reading (Exclusive 2 Percent Focus)
 *   domain: economic/constitutional/eu_institutional_governance
 *
 * SUMMARY:
 *   Article 127 TFEU names price stability as the ESCB's primary objective
 *   and directs it to support general Union policies 'without prejudice' to
 *   that objective. The orthodox reading instantiated here holds that this
 *   text requires exclusive operational focus on a quantified 2 percent
 *   inflation target: secondary objectives receive rhetorical acknowledgment
 *   but no instruments, no metrics, and no independent decision weight. The
 *   arrangement genuinely coordinates - a single credible nominal anchor
 *   synchronizes expectations across twenty heterogeneous economies - while
 *   simultaneously transferring real value toward holders of nominal claims
 *   and away from debtors and workers, and while externalizing unpriced
 *   climate risk by keeping the collateral framework market-neutral. KEY
 *   AGENTS (by structural relationship): ecb_governing_council
 *   (agenda-setter, institutional/constrained) translates the primary
 *   objective into the operational target and defends the mandate boundary;
 *   eurozone_bondholders (primary beneficiary, powerful/arbitrage) collect
 *   real-value protection and punish deviation by repricing;
 *   german_stability_coalition (beneficiary, organized/identity_locked)
 *   supplies political and legal legitimacy for the narrow reading;
 *   eurozone_export_manufacturers (dual-positioned beneficiary/payer,
 *   organized/mobile) gain cost predictability but lose demand in
 *   disinflation phases; southern_debtor_households,
 *   peripheral_unemployed_workers, and high_debt_member_state_governments
 *   (targets, trapped or constrained) bear the real burdens;
 *   climate_integration_advocates (excluded, organized/constrained) press
 *   Article 11 integration from outside the decision room;
 *   european_parliament_economic_affairs_committee (observer,
 *   institutional/analytical) scrutinizes without treaty power. FAMILY NOTE
 *   (epsilon-invariance decomposition): the colloquial label 'the ECB
 *   mandate' covers at least three structurally distinct claims. This story
 *   instantiates only the orthodox reading; the siblings
 *   expansive_secondary_objectives and climate_incorporation are separate
 *   constraints with their own epsilon values, beneficiary sets, and
 *   classifications, linked through network.affects_constraints. The epsilon
 *   values differ because the readings allocate the same standing
 *   arrangement's costs differently: this reading narrows the beneficiary set
 *   to creditors and savers and externalizes climate risk, so its epsilon
 *   sits above what a broad-benefit reading of the same text would authorize.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Agenda-setter (institutional/constrained) - translates the treaty primary objective into the operational 2 percent target and defends the mandate boundary in hearings, courts, and markets
 *   - eurozone_bondholders: Primary beneficiary (powerful/arbitrage) - collects real-value protection on nominal claims; disciplines deviation through repricing
 *   - german_stability_coalition: Beneficiary (organized/identity_locked) - supplies electoral and legal legitimacy for the narrow reading; stability culture fused with national post-war identity
 *   - eurozone_export_manufacturers: Secondary beneficiary with payer exposure (organized/mobile) - gains input-cost predictability, loses orders when disinflation depresses demand
 *   - southern_debtor_households: Primary target (powerless/trapped) - variable-rate borrowers whose real burdens rise when disinflation outpaces wage growth; no national currency exit
 *   - peripheral_unemployed_workers: Primary target (powerless/trapped) - absorb employment losses of inflation-first policy without operational representation in the mandate
 *   - high_debt_member_state_governments: Secondary target (moderate/constrained) - finance debts in a currency they do not issue; leverage confined to councils and courts against a creditor veto bloc
 *   - climate_integration_advocates: Excluded voice (organized/constrained) - NGOs, litigators, MEPs, and academics pressing Article 11 environmental integration with no seat in the Council
 *   - european_parliament_economic_affairs_committee: Analytical observer (institutional/analytical) - hearings and resolutions without treaty-amendment power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.58).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.73).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate - Orthodox Price-Stability Reading (Exclusive 2 Percent Focus)").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "economic/constitutional/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, '443e8dde-12d3-415f-b4ef-4ae91adea0e0').
narrative_ontology:cs_kernel_codification('443e8dde-12d3-415f-b4ef-4ae91adea0e0', fixed_text).
narrative_ontology:cs_authority_grounding('443e8dde-12d3-415f-b4ef-4ae91adea0e0', expertise).
narrative_ontology:cs_interpretation_layer_present('443e8dde-12d3-415f-b4ef-4ae91adea0e0').
narrative_ontology:cs_reading_relation('443e8dde-12d3-415f-b4ef-4ae91adea0e0', ecb_mandate_article_127__expansive_secondary_objectives, influences).
narrative_ontology:cs_reading_relation('443e8dde-12d3-415f-b4ef-4ae91adea0e0', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('443e8dde-12d3-415f-b4ef-4ae91adea0e0', foundational, price_stability_primary_objective_supremacy).
narrative_ontology:cs_axiom_status(price_stability_primary_objective_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('443e8dde-12d3-415f-b4ef-4ae91adea0e0', price_stability_primary_objective_supremacy, conventional).
narrative_ontology:cs_axiom('443e8dde-12d3-415f-b4ef-4ae91adea0e0', foundational, secondary_objectives_non_operational_subordination).
narrative_ontology:cs_axiom_status(secondary_objectives_non_operational_subordination, holdable).
narrative_ontology:cs_axiom_grounding('443e8dde-12d3-415f-b4ef-4ae91adea0e0', secondary_objectives_non_operational_subordination, conventional).
narrative_ontology:cs_reference_frame('443e8dde-12d3-415f-b4ef-4ae91adea0e0', primary_objective_supremacy_framework).
narrative_ontology:cs_drift_state('443e8dde-12d3-415f-b4ef-4ae91adea0e0', contemporary_post_strategy_review, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('443e8dde-12d3-415f-b4ef-4ae91adea0e0', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, eurozone_bondholders).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, german_stability_coalition).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, eurozone_export_manufacturers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, southern_debtor_households).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, peripheral_unemployed_workers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, eurozone_export_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets interest rates and asset-purchase programs and decides how the treaty's primary objective becomes an operational target; currently maintains the 2 percent definition and treats the supporting clauses as constraints on deliberation rather than inputs to it. Members answer to national appointment processes and defend the mandate boundary in parliamentary hearings, court filings, and press conferences. Individuals can leave for national politics or academia, but the institution itself has no exit from its own legal frame.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Hold large stocks of nominal euro-denominated sovereign and corporate debt; the credible low-inflation commitment protects the real value of coupons and principal. They police the commitment through repricing - selling periphery paper or demanding term premia whenever policy looks loose. Capital rotates into dollar or franc assets at low transaction cost, so remaining exposed to euro nominal claims is a continuing choice, not a captivity.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eurozone_bondholders, beneficiary,
    powerful, biographical, arbitrage, global).

% A durable bloc spanning the Bundesbank successor tradition, ordoliberal economics faculties, mass-circulation press, and parts of the constitutional-law establishment. It supplies electoral and judicial legitimacy for the narrow reading: it litigated against large-scale bond purchases, stages recurring public defenses of stability culture, and rewards politicians who resist loosening. Monetary stability is fused with national post-war redemption narratives, so members maintain the frame well past the point where material stakes alone would justify it.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, german_stability_coalition, beneficiary,
    organized, generational, identity_locked, national).

% Plan investment, contracts, and wage bargains around predictable input costs and a stable exchange-rate anchor; price stability lowers hedging expense and supports competitiveness against higher-inflation trading partners. During aggressive disinflation phases, however, weak domestic demand compresses order books, so the same policy that steadies costs periodically depresses sales. Production is relocatable across borders at meaningful but not trivial cost.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eurozone_export_manufacturers, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, eurozone_export_manufacturers, payer).

% Carry variable-rate mortgages and consumer credit concentrated in Spain, Greece, Portugal, and Italy. When policy tightens to defend the target faster than wages grow, real debt service and arrears climb. There is no national currency to depreciate and no access to cheaper foreign credit; migration is the principal escape valve and it is expensive, disruptive, and selective.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, southern_debtor_households, payer,
    powerless, biographical, trapped, regional).

% Concentrated among young and low-skilled workers in program countries; they absorb the employment losses that disinflationary policy produces. The mandate names them rhetorically in press conferences but directs no instrument at their situation, and intra-union labor mobility is limited by language, credential recognition, and housing costs. Their recourse is protest and the ballot box, aimed at governments who do not control the instrument.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, peripheral_unemployed_workers, payer,
    powerless, biographical, trapped, regional).

% Finance large public debts in a currency they do not issue. Tighter-for-longer policy raises real financing costs exactly when consolidation is hardest. They petition for flexible interpretation and joint fiscal-monetary instruments but hold no unilateral recourse: defaulting inside the union triggers exclusion dynamics, and exiting the currency is prohibitively disruptive, so their leverage runs through councils and courts where they confront an organized creditor veto bloc.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_state_governments, payer,
    moderate, generational, constrained, national).

% A coalition of environmental NGOs, litigation groups, members of parliament, and academics pressing for the Article 11 environmental-integration obligation to shape collateral eligibility and purchase programs. They file complaints, publish legal analyses, and demonstrate at ECB premises, but hold no seat on the Governing Council and no vote on its instruments; their access runs through litigation and the occasional strategy review.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_integration_advocates, excluded,
    organized, civilizational, constrained, continental).

% Holds the ECB accountable through periodic hearings and the annual-report dialogue; questions the narrow operational reading, commissions analysis of distributional and climate effects, and passes resolutions urging broader interpretation. It cannot amend the treaty, nominate the Council, or direct instruments - its seat is scrutiny without command.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, european_parliament_economic_affairs_committee, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors inflation expectations across twenty heterogeneous economies behind a single measurable target, giving households, firms, wage bargainers, and markets one common nominal reference point and protecting the currency's internal value - solving the time-inconsistency problem of politically tempted deficit monetization in a young multinational currency.
% TRANSFER_FUNCTION: Moves real purchasing power from debtors to holders of nominal claims via disinflationary bias; moves policy attention and instrument design away from employment, regional, and climate objectives toward the single target; and concentrates monetary-credibility rents in the creditor economies that anchor the political coalition defending the narrow reading.
% ABSENT_VOICES: Southern debtor constituencies, organized labor, and climate-integration advocates have no seat in the Governing Council and no vote on its instruments; national governors dominate the room through appointment channels that skew toward boundary-defending traditions. They speak from litigation dockets, demonstration lines, and parliamentary hearings - channels that produce testimony, not decisions.
% DISAPPEARANCE_RATIONALE: If the exclusive-focus reading vanished overnight, the Council's operational weighting would visibly change: employment and climate considerations would acquire decision weight and collateral criteria would move, asset prices and periphery spreads would reprice immediately, the creditor coalition would respond with litigation and political escalation, and the distribution of adjustment costs across the union would shift - the euro-area political economy reorganizes around whichever reading replaces it.
% FOUNDING_PROBLEM: Anchor a newly created multinational currency - lacking historical trust, covering economies with divergent inflation memories - against the chronic political temptation to monetize deficits, a failure mode written large in twentieth-century European history and refreshed by the 1970s stagflation.
% FOUNDING_PROBLEM_CORROBORATION: The original problem is corroborated from outside the benefiting parties: the academic monetary-economics consensus on expectations anchoring, IMF surveillance analyses, and the documented record of 1970s inflation all attest it. Its current status is disputed along the same creditor-debtor line that runs through the whole arrangement: the stability-culture constituency attests the problem is live and recurrent, while southern governments, trade unions, and expansion-minded economists attest that the anchor is secured and the arrangement now oversupplies restriction - no single source outside the beneficiary set settles the question, and that unresolved split is itself the finding.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (end-state): the anchor function is real and conceded even by critics, but the operational reading systematically favors holders of nominal claims - disinflationary bias raises real debt burdens, crisis-era tightening episodes imposed deep output costs on program countries, and secondary objectives receive acknowledgment without weight. Suppression is high (0.73) and is authored as a RAW structural property, unscaled by power or scope: persistence depends on actively holding the boundary - Governing Council voting discipline, constitutional-court defense of narrow interpretations, capital-market punishment of perceived dovishness, and appointment politics favoring boundary-defenders. Theater ratio is moderate (0.36) and rising: press-conference references to employment and climate, strategy-review announcements, and climate 'action plans' accumulate as acknowledgment-without-operational-consequence - performance substituting for revision while the operational core stays fixed. The temporal series run on ONE shared ten-point grid (t=0..27, all metrics authored at every point, basis observed throughout). The trajectories are cyclical rather than monotonic: an institutional pendulum of orthodoxy (2008 and 2011 tightening peaks, 2022-24 restoration), crisis pragmatism (2014-21 flexing), and restoration. The oscillation is partly a maintenance mechanism in its own right - each restoration ratchets boundary defenses (legal precedent, court challenges answered, appointment hardening), an intermittent-reinforcement analog at institutional scale - which is why suppression_requirement ends above where the last relaxation left it. Base_properties values are measured at the end-state phase (t=27, post-restoration): elevated suppression, settled extractiveness, peak acknowledgment-theater. Resistance (0.58) is real but fragmented: payer-side coalitions form (parliamentary alliances, southern-government petitions) yet are disciplined by spread markets that convert creditor exit options into bargaining leverage. Accessibility_collapse (0.6) reflects that monetary-sovereignty alternatives have collapsed entirely for member states while fiscal substitution and discursive alternatives persist only partially.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the bondholder seat the arrangement is insurance: real-value protection purchased at near-zero personal cost, with arbitrage-grade exit meaning the constraint barely binds on them at all. From the trapped payer seats - debtors, peripheral workers - the identical structure operates as enforced transfer, with the trap amplifying their effective burden: they cannot devalue, migrate cheaply, or reprice their way out. From the agenda-setter seat it is neither: it is the institution-defining duty that constitutes the Council's authority and independence. Inter-institutionally, national governments meet the supranational Council through councils and courts where the creditor bloc holds veto leverage; constitutional courts adjudicate the boundary from yet another seat with different incentives. At the same nominal level, member governments are formally equal but diverge sharply in exit options by debt position - a low-debt creditor government experiences the constraint as benign order while a high-debt debtor government experiences it as fiscal vise, so power diverges despite equal treaty standing. Payer coalition potential exists and periodically materializes in parliamentary majorities and joint letters, but it is structurally checked: bondholders' ability to exit converts every payer grievance into a funding-cost threat, fragmenting the would-be coalition.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Eurozone bondholders sit nearest the beneficiary pole (d near 0.0) and their arbitrage exit dampens effective extraction toward subsidy-like treatment; the german_stability_coalition likewise derives low d as a declared beneficiary, though its identity_lock means its position is maintained irrespective of marginal payoff - it would defend the frame below cost. Eurozone export manufacturers are dual-positioned: declared beneficiary with payer exposure, landing mid-range. The three declared victim groups derive high d, with trapped exit options (households, workers) pushing them toward the full-target pole and constrained exit (debtor governments) close behind - the engine amplifies effective extraction for trapped targets relative to mobile ones. The governing council's position derives from its enforcement role: it administers the boundary its own authority rests on, a structural relationship the beneficiary list alone understates. Spatial scope is continental with global financial spillovers, which modestly raises verification difficulty and therefore effective extraction on the payer side; suppression, again, enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - anchoring a young multinational currency against the political temptation to monetize deficits, learned from 1970s stagflation and older hard-money memory - is still partially live: the post-pandemic inflation surge demonstrated that the anchor function retains real value, so this is not a resolved-mandatrophy case and no sunset applies. The classification prevents mislabeling in both directions. Reading the arrangement as pure coordination (rope) would erase the genuine creditor-debtor transfer that the trapped-payer seats experience; reading it as pure extraction (snare) would erase the expectations coordination that even its sharpest critics concede solved a real collective-action problem. The hybrid classification holds both facts. The rising theater ratio tracks the characteristic degradation path without letting performance masquerade as function: acknowledgment of secondary objectives accumulates in communication while operational weight stays at zero, which is precisely the signature the theater metric exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_reading_kernel_contest,
    'Which structural element separates this orthodox reading from its sibling readings of Article 127 TFEU - specifically, does the ''without prejudice'' support clause confer mere acknowledgment of secondary objectives (no instruments, metrics, or decision weight) or operational discretion?',
    'Track Governing Council voting records and strategy-review drafts for proposals assigning explicit decision weight or dedicated instruments to employment or growth objectives; observe whether such assignments survive opposition or are reclassified as price-stability-conditional.',
    'If the clause is read as conferring operational discretion, this reading''s suppression of mandate expansion loses its interpretive foundation, its beneficiary set broadens, and classification shifts toward rope; if exclusivity holds, costs continue concentrating on trapped payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_reading_kernel_contest, conceptual, 'Location of the intra-kernel disagreement: the operational weight assigned to the secondary-objectives clause.').

omega_variable(
    extraction_vs_anchor_cost,
    'Is the distributional asymmetry under exclusive inflation targeting rent accruing to creditors, or an irreducible cost of manufacturing credibility for a young multinational currency?',
    'Compare anchor durability and sacrifice ratios across regimes: jurisdictions with dual-mandate flexible targeting achieved broadly comparable expectation anchoring; if equivalent credibility is attainable with a wider benefit set, the residual component is identifiable as rent.',
    'If rent, effective extraction exceeds coordination cost and the classification moves toward the snare boundary; if irreducible, part of the measured epsilon is legitimate coordination cost and the hybrid coordination-plus-transfer reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_anchor_cost, empirical, 'Whether the creditor-favoring distribution is extractive overhead or inherent credibility cost.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the high suppression of mandate expansion carried by external barriers (constitutionally fixed treaty text, court veto threats, capital-market discipline) or by internalized professional identity (central bankers treating expansion as unthinkable even where legally available)?',
    'Observe behavior in windows where structural barriers loosen: the pandemic emergency-purchase episode showed the Council expanding when emergency framing suspended normal constraints, then restoring the boundary afterward; compare the pace and extent of restoration against what legal necessity alone required.',
    'If suppression is largely internalized, treaty-level reform alone would not lower it; the constraint travels with personnel and training pipelines, raising persistence estimates and making classification robust to legal change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized carrier of the mandate-boundary enforcement.').

omega_variable(
    kernel_codification_framing_underdetermination,
    'Is the kernel the constitutionally fixed treaty text (with drift absorbed by an interpretive layer) or the Council''s self-authored operational definition (the numerical target, revised in 2003 and 2021 by the interpreter itself without treaty amendment)?',
    'Determine which artifact binds when interpretation and quantification diverge: if Council revisions of the numerical definition take effect without treaty change, the operative kernel is the formalized definition and the text functions as standing authorization.',
    'Under the formalized framing the Council is a kernel-reviser, which weakens foreclosure computations against sibling readings proposing definitional change; under the fixed-text framing, sibling challenges must route through interpretation, and this reading''s incumbency advantage persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing_underdetermination, conceptual, 'Alternative framings of what the stabilized kernel is: fixed text versus interpreter-owned quantification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_orthodox_ps_tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.17).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t3, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 3, 0.19).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t6, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 6, 0.22).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t9, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 9, 0.25).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t12, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 12, 0.29).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t15, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 15, 0.31).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t18, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 18, 0.32).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t21, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 21, 0.34).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t24, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 24, 0.31).
narrative_ontology:measurement(ecb_orthodox_ps_tr_t27, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 27, 0.36).

% Extraction over time
narrative_ontology:measurement(ecb_orthodox_ps_be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(ecb_orthodox_ps_be_t3, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(ecb_orthodox_ps_be_t6, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(ecb_orthodox_ps_be_t9, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 9, 0.61).
narrative_ontology:measurement(ecb_orthodox_ps_be_t12, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(ecb_orthodox_ps_be_t15, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(ecb_orthodox_ps_be_t18, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(ecb_orthodox_ps_be_t21, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 21, 0.51).
narrative_ontology:measurement(ecb_orthodox_ps_be_t24, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(ecb_orthodox_ps_be_t27, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 27, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ecb_orthodox_ps_su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(ecb_orthodox_ps_su_t3, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 3, 0.54).
narrative_ontology:measurement(ecb_orthodox_ps_su_t6, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 6, 0.57).
narrative_ontology:measurement(ecb_orthodox_ps_su_t9, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 9, 0.63).
narrative_ontology:measurement(ecb_orthodox_ps_su_t12, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(ecb_orthodox_ps_su_t15, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(ecb_orthodox_ps_su_t18, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(ecb_orthodox_ps_su_t21, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 21, 0.62).
narrative_ontology:measurement(ecb_orthodox_ps_su_t24, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(ecb_orthodox_ps_su_t27, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 27, 0.73).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the ECB mandate' decomposes per the epsilon-invariance principle into three readings of the Article 127 TFEU kernel. This file is the orthodox_price_stability reading (narrow creditor-weighted beneficiary set, climate risk externalized, high suppression of expansion). The siblings - expansive_secondary_objectives (broader beneficiary set including employment-sensitive constituencies, lower epsilon) and climate_incorporation (adds climate-exposed constituencies as protected parties, shifts the victim structure toward carbon-intensive asset holders) - are separate stories with their own epsilon values over the same standing arrangement. The upstream incumbent (this reading) structurally influences both downstream challengers: its legal defense apparatus and interpretive dominance set the admissible forms of any expansion proposal. Each family member links to the others via network.affects_constraints; orphaning any one would hide the contamination path by which a shift in the incumbent reading propagates to the challengers' legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
