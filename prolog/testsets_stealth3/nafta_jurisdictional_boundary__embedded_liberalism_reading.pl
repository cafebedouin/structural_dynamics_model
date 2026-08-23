% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__embedded_liberalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__embedded_liberalism_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nafta_jurisdictional_boundary__embedded_liberalism_reading
 *   human_readable: Trade Agreement Jurisdictional Boundary — Embedded Liberalism Reading (Balanced Market Access with Reserved Policy Space)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   A tri-national trade and investment framework grants enforceable
 *   market-access commitments while expressly reserving each signatory's
 *   right to regulate for legitimate domestic objectives, provided measures
 *   are non-discriminatory. The arrangement is administered through standing
 *   ministerial commissions and ad hoc dispute panels that decide whether a
 *   challenged regulation breaches commitments or falls inside the reserved
 *   policy space. The structural delta of this reading is partial
 *   jurisdictional overlap: regulatory agencies keep defensive authority, but
 *   exercising it requires running the gauntlet of international litigation
 *   whose costs fall on public budgets regardless of outcome. Epsilon's
 *   referent is the standing arrangement — treaty-enforced access with
 *   policy-space exceptions, as operated — assessed by this reading's own
 *   lights; it is not the capital-supremacist arrangement a sibling reading
 *   would read from the same text. KEY AGENTS (by structural relationship): -
 *   foreign_investors_capital_exporters: Primary beneficiary seat
 *   (powerful/arbitrage) — holds enforceable access rights, initiates
 *   challenges, collects compensation - export_oriented_firms: Secondary
 *   beneficiary seat (organized/mobile) — consumes the framework's
 *   predictability without carrying its dispute costs -
 *   isds_arbitration_industry: Fee-capturing beneficiary seat
 *   (organized/mobile) — revenue scales with dispute volume regardless of
 *   outcome - domestic_regulatory_agencies: Primary payer seat
 *   (institutional/identity_locked) — retains defensive authority but bears
 *   defense costs and deterrence - public_treasuries_taxpayers: Primary payer
 *   seat (moderate/trapped) — funds every defense and any award -
 *   domestic_competing_firms: Dual-positioned payer seat
 *   (moderate/constrained) — bears the remedy asymmetry, gains diffusely from
 *   the open market - trade_dispute_tribunals and national_trade_ministries:
 *   Administrative seats (institutional) — decide and interpret the boundary,
 *   bear no fiscal consequence - environmental_labor_advocacy_groups:
 *   Excluded seat (organized/constrained) — would object to the boundary's
 *   placement, holds no procedural standing - trade_law_scholars: Analytical
 *   observer — maps where the operative boundary actually sits The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as tangled_rope
 *   from the authoring seat (genuine access-coordination function plus real
 *   asymmetric cost-shifting through the same structure), while the metrics
 *   independently describe moderate extractive load — the engine computes
 *   per-seat classifications from the structural data; do not reconcile the
 *   claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48).
domain_priors:suppression_score(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.34).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__embedded_liberalism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__embedded_liberalism_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__embedded_liberalism_reading, "Trade Agreement Jurisdictional Boundary — Embedded Liberalism Reading (Balanced Market Access with Reserved Policy Space)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__embedded_liberalism_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__embedded_liberalism_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, foreign_investors_capital_exporters).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_firms).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, isds_arbitration_industry).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, public_treasuries_taxpayers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_competing_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_competing_firms).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__embedded_liberalism_reading, national_trade_ministries).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, embedded_liberalism_compromise).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, nondiscrimination_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__embedded_liberalism_reading, legitimate_objectives_exception_architecture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own or seek productive and financial assets across the three member markets under enforceable access guarantees. May file treaty challenges against host-government measures they judge to impair their investments, recover monetary compensation when challenges succeed, and otherwise treat the possibility of dispute as a priced line item in investment planning. They can structure holdings, insure exposure, and shift forums, so participation is a choice renewed asset by asset.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, foreign_investors_capital_exporters, beneficiary,
    powerful, biographical, arbitrage, global).

% Operate integrated supply chains that depend on predictable tariff-free access and shared rules of origin. Rarely initiate disputes themselves but plan cross-border capacity around the stability the framework supplies. Carry little direct cost of the dispute machinery and would face immediate commercial disruption if reciprocal access guarantees lapsed.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, export_oriented_firms, beneficiary,
    organized, biographical, mobile, continental).

% Specialized counsel, arbitrators, expert witnesses, and litigation funders who staff treaty disputes. Revenues scale with the number and length of proceedings regardless of which side prevails, and professional standing depends on continued demand for the dispute channel. Exposure to regulatory outcomes is reputational rather than financial.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, isds_arbitration_industry, beneficiary,
    organized, biographical, mobile, global).

% Environmental, labor, food-safety, and health regulators who draft and enforce domestic standards under national statutes. When a measure attracts a treaty challenge they must compile the scientific and legal record showing it pursues a legitimate objective and discriminates against no nationality, diverting senior staff and budget for years. Their statutes and missions leave them no lawful alternative to defending the standard; stepping aside would dissolve the mandate that constitutes the agency.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_regulatory_agencies, payer,
    institutional, generational, identity_locked, national).

% Finance the government side of every treaty dispute — outside counsel, expert reports, panel costs — and pay any compensation ultimately awarded. Cannot decline to fund a defense once a challenge is filed. Share only diffusely in the expanded commerce the framework enables, so the connection between what they pay and what they receive runs through macroeconomic aggregates they do not control.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, public_treasuries_taxpayers, payer,
    moderate, generational, trapped, national).

% Domestic producers competing with treaty-shielded foreign entrants. Unlike those entrants they have no standing to invoke the treaty's investment protections against any government. They gain indirectly from cheaper inputs and larger export markets, yet lose ground whenever a foreign rival obtains compensation or negotiating leverage that is unavailable to them.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_competing_firms, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, domestic_competing_firms, beneficiary).

% Ad hoc three-member panels constituted under the treaty's dispute chapters to decide whether a challenged domestic measure breaches market-access commitments or falls within the legitimate-objectives exceptions, together with the trilateral commission that issues binding interpretations of ambiguous provisions. They decide the placement of the boundary case by case but neither fund the process nor bear the fiscal consequences of their rulings.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_dispute_tribunals, agenda_setter,
    institutional, generational, constrained, continental).

% Negotiate, administer, and periodically revise the agreement through the trilateral commission, and author binding interpretations when provisions prove ambiguous. In each revision cycle they weigh domestic regulatory and consumer interests against exporter and investor constituencies, and they control whether policy-space language is strengthened or diluted. Their standing in the arrangement depends on keeping it operating, which limits how far they can push against it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, national_trade_ministries, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__embedded_liberalism_reading, national_trade_ministries, beneficiary).

% Organizations campaigning for stringent domestic standards and against investor challenges to regulation. They held no seat in the original negotiations and have only intermittently been admitted before tribunals as amici. Their influence runs through public opinion, elections, and pressure on national governments rather than through the treaty process itself, so their objections register late and indirectly.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, environmental_labor_advocacy_groups, excluded,
    organized, generational, constrained, continental).

% Academic specialists and legal analysts who track the jurisprudence, code outcomes by sector and disposition, and publish assessments of where the legitimate-objectives boundary effectively sits versus where the text places it. They hold no operational stake in outcomes and no standing before panels beyond occasional amicus submissions.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__embedded_liberalism_reading, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__embedded_liberalism_reading, foreign_investors_capital_exporters).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__embedded_liberalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reciprocal, enforceable market-access commitments across the three signatories reduce arbitrary discrimination and stabilize cross-border commercial expectations; a shared adjudicative channel settles access disputes without tit-for-tat retaliation, and the exceptions architecture tells all parties in advance which domestic measures are off-limits to challenge.
% TRANSFER_FUNCTION: Moves regulatory disputes from domestic courts and agencies to ad hoc international panels; moves litigation costs and, on adverse outcomes, compensation payments from national treasuries to private claimants and their counsel; moves regulatory certainty and negotiating leverage toward investors; moves defensive burden onto the agencies whose standards are challenged.
% ABSENT_VOICES: Environmental and labor advocacy organizations, affected subnational governments and communities, and domestic firms without treaty recourse were absent from the negotiating table and remain marginal in panel proceedings (amicus admission is intermittent and contested). Present, they would press for narrower investor rights, wider legitimate-objectives shields, and parity of remedy between foreign and domestic firms. Their absence is the strongest evidence that the negotiated balance reflects the parties in the room rather than a consensus among all affected seats.
% DISAPPEARANCE_RATIONALE: Continental supply chains are priced and sited on guaranteed reciprocal access; investment contracts reference the treaty dispute channel; thousands of commercial arrangements presume the framework's rules of origin and dispute procedure. Overnight removal would trigger immediate renegotiation of investment protections, repricing of cross-border capacity, and a vacuum in regulatory-dispute settlement that domestic courts would fill slowly and unevenly across the three systems.
% FOUNDING_PROBLEM: dup
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the investor-beneficiary set by trade-policy historiography on the embedded-liberalism settlement, by published accounts of negotiators from all three countries describing the deliberate reservation of regulatory autonomy, and by subsequent developing-country coalitions demanding comparable policy space in multilateral rounds. No source outside the benefiting parties attests that the problem is resolved; exporter and investor associations are the voices claiming the balance is settled, and their interest in that claim is structural.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__embedded_liberalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__embedded_liberalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__embedded_liberalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__embedded_liberalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__embedded_liberalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the arrangement's burden operates through cost-shifting rather than confiscation: regulators usually prevail on the merits when they defend a genuinely non-discriminatory measure, but the defense itself is a multi-year, multi-million-dollar diversion of public resources, and the credible threat of that process deters some regulation before filing. Suppression is authored low-moderate (0.34) as a raw structural property — it is NOT scaled by power or scope; signatories retain formal withdrawal rights on notice, exception clauses are real, and enforcement runs through legal obligation and precedent rather than coercive force, which caps how closed the choice set is. Theater ratio is 0.30: adjudication is mostly functional, but a growing share of activity is formulaic — boilerplate legitimacy defenses, ritualized award reasoning, procedural motions that consume budget without deciding the boundary. Accessibility collapse is 0.40 because alternatives genuinely persist: agencies can redesign measures to fit exceptions, states can carve out sensitive sectors (as later done for tobacco control), and withdrawal is legally available. Resistance is 0.55: sustained civil-society campaigns, state-level carve-outs, and successor-treaty narrowing demonstrate active pushback that the arrangement absorbs rather than crushes.
 *   
 *   All three series run on ONE shared time grid ({0,5,10,15,20,25,30}) so every metric is authored at every examined point. Base extractiveness peaks mid-interval during the heavy challenge era and moderates slightly after authoritative interpretations tightened the exceptions reading and the successor treaty narrowed the investor docket. Theater creeps upward monotonically as proceedings ritualize. Suppression_requirement is authored deliberately — this story specifically traces enforcement-capacity change: machinery built up through the middle of the interval (rising caseload, binding interpretive activity) and partially rolled back at the end (carve-outs, docket narrowing), hence rise-then-partial-decay rather than a flat line.
 *   
 *   Receipt surface: compensation awards and settlement leverage accrue principally to successful claimant investors, so gain_flow names the foreign_investors_capital_exporters seat; counsel captures fees but the value transfer lands in the claimant seat. Fixing cost is prohibitive relative to the benefit of fixing: the demonstrated remedies — binding interpretations, sector carve-outs, successor-treaty narrowing — are all partial modifications, and full removal would require unwinding the enforcement core of a continental commercial framework, a cost far exceeding the moderate burden the cost-shifting imposes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administrative seats compute fundamentally different arrangements from identical text. From the investor seat, the framework is a guarantee — enforceable access, priced risk, a remedy no other commercial relationship offers. From the treasury and regulatory seats, the same structure is a toll booth placed in front of the state's own police powers: authority retained, but only at the price of proving legitimacy repeatedly before panels the agency does not select and cannot refuse. The same-level lateral split is sharpest between two institutionally powered domestic actors: the trade ministry (administrative seat, constrained exit) experiences the framework as its instrument and negotiates its shape, while the environmental or labor regulator at nominally equal institutional rank experiences it as an external audit of its statutory mission — differentiated not by power but by the relationship the framework assigns each. The tribunal seat experiences the arrangement as procedure itself, insulated from every fiscal consequence its rulings generate.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries sit near the subsidized end of directionality: foreign investors combine beneficiary status with arbitrage-grade exit (they can structure, insure, and forum-shop), placing them nearest d≈0; export-oriented firms are beneficiaries with mobile exit; the arbitration industry collects fees from dispute volume with no exposure to outcomes. Declared victims sit near the full-target end: public treasuries are trapped (they cannot decline to fund a filed defense) and bear every award; domestic regulatory agencies are institutional-power victims whose exit is identity-locked — an environmental or labor regulator cannot exit by ceasing to regulate, since that would dissolve the statutory mission that constitutes the agency, so the derivation should place them near the full-target end despite their institutional power; domestic competing firms are victims with a genuine secondary beneficiary position (cheaper inputs, bigger markets), pulling their derived d back toward symmetric. Administrative seats (tribunals, ministries) derive mid-range directionalities: they run the machinery without capturing its transfers. The excluded advocacy seat contributes no directionality — an authored absence is commentary-grade evidence feeding the consensus-provenance question (was the balance unanimous because it is real, or because the objecting seats were never in the room), never a classification override. Effective extraction scales with directionality and scope in the engine; suppression does not scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live — every renegotiation cycle reopens the same tension between commercial openness and the domestic social compact, which is evidence the mandate has not outlived its function. The classification discipline cuts both ways here. Calling the arrangement pure coordination (rope) would erase the documented cost-shifting: treasuries pay to defend laws that survive on the merits, and the threat of payment deters regulation that never gets written. Calling it pure extraction (snare) would erase the delivered function: integrated supply chains, dispute channels that displace tit-for-tat retaliation, and the demonstrable survival of non-discriminatory environmental and labor measures inside the exceptions architecture. Tangled rope keeps both faces visible and forces the question the corpus exists to take: how much of the measured burden is the irreducible price of binding reciprocity, and how much is rent collected through litigation friction? Mandatrophy risk would emerge only if the policy-space function atrophied entirely — if tribunals stopped honoring legitimate-objectives defenses in practice and the exceptions became theatrical — leaving ritualized dispute processing over a boundary nobody defends. The theater_ratio series is the early-warning instrument for that transition; its slow upward creep is monitored, not yet decisive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the embedded_liberalism_reading of the nafta_jurisdictional_boundary kernel; which reading of the boundary — this one, capital_supremacy_reading (treaty text as supreme law overriding domestic standards), or sovereignty_primacy_reading (text subordinate to sovereign law) — do tribunal practice and successive renegotiations actually converge on?',
    'Track the distribution of tribunal holdings on legitimate-objectives defenses across sectors and the investment-chapter text of each negotiated revision; convergence toward categorical investor override signals capital supremacy, convergence toward domestic-only adjudication signals sovereignty primacy.',
    'Under the capital_supremacy_reading the same referent text yields substantially higher epsilon (categorical extraction, weakened coordination claim); under the sovereignty_primacy_reading epsilon collapses toward pure coordination overhead and the type shifts toward rope. The authored values are valid only for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the jurisdictional-boundary kernel governs the arrangement in operation.').

omega_variable(
    legitimate_objectives_boundary_location,
    'Where does the ''legitimate objectives'' boundary sit in operation — which categories of domestic regulation reliably survive challenge, and which are effectively contestable at prohibitive cost?',
    'Systematic coding of tribunal outcomes by sector, measure type, and disposition over the interval, distinguishing wins on the merits from procedural attrition.',
    'A narrow operative boundary raises effective burden on regulators well above the authored baseline and tilts the arrangement toward extraction; a wide boundary supports the balance framing and lowers epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_objectives_boundary_location, empirical, 'Operative width of the legitimate-objectives shield versus its textual promise.').

omega_variable(
    defense_cost_vs_chill_attribution,
    'Is the measured burden on regulators carried primarily by direct defense costs paid per challenge, or by regulatory chill suppressing measures before any challenge is filed?',
    'Compare regulatory proposal rates in treaty-exposed versus unexposed sectors before and after salient tribunal rulings; audit agency legal-expenditure lines against proposal pipelines.',
    'Chill-dominated burden is invisible to fee-level remedies and implies higher true epsilon than the direct-cost record shows; direct-cost dominance makes the burden legible and partially refundable through fee-shifting or insurance mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defense_cost_vs_chill_attribution, empirical, 'Whether the burden operates through paid defense or deterred action.').

omega_variable(
    asymmetric_remedy_valuation,
    'Does granting foreign investors a compensation remedy withheld from domestic firms operate as a levy on domestic competitors, or as efficient risk pricing that enlarges the host economy enough to compensate everyone?',
    'Counterfactual comparison of investment volumes and regulatory stringency across jurisdictions with and without investor-state access, controlling for rule-of-law baselines.',
    'If it operates as a levy, the domestic_competing_firm seat hardens toward full target and the balance tilts toward extraction at that seat; if it is efficient risk pricing, part of the asymmetry belongs to the framework''s genuine coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_remedy_valuation, conceptual, 'Valuation of the foreign-domestic remedy asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__embedded_liberalism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 25, 0.49).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__embedded_liberalism_reading, suppression_requirement, 30, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__embedded_liberalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__embedded_liberalism_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the NAFTA jurisdictional boundary' conflates three structurally distinct claims with different epsilon values over the same treaty text. The embedded_liberalism_reading (this story) authors epsilon for the balanced-framework arrangement as its own lights see it — moderate extraction through litigation cost-shifting inside a genuine coordination function. The capital_supremacy_reading authors high extraction (treaty supremacy, mandatory harmonization); the sovereignty_primacy_reading authors near-zero extraction (bare coordination subordinate to sovereign law). Family linkage runs through this story's network.affects_constraints; the embedded-liberalism reading sits structurally between its siblings and exerts interpretive pressure on both: its exception architecture is cited as proof that balance is achievable (against capital supremacy) and its concession of binding external adjudication is cited as the slippery slope (against sovereignty primacy). Any change to this story's epsilon must not propagate as if it measured the other readings — each is a separate file with a separate referent assessment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
