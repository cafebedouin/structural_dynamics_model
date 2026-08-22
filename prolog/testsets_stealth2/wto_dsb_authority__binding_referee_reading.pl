% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding-Rulings Regime (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   Under the binding-referee reading, the operative constraint is the
 *   bindingness of WTO dispute settlement: members ratified the DSU, reports
 *   are adopted by negative consensus, compliance is owed as a treaty
 *   obligation, and the DSB authorizes retaliation against refusers. Joining
 *   the system exchanged policy discretion in covered domains for enforceable
 *   market access; refusing compliance is breach, not policy choice. The
 *   epsilon referent is the standing arrangement — the binding-rulings regime
 *   as operated from 1995 to the present — assessed by this reading's own
 *   lights, which credit real bindingness and a real sovereignty transfer;
 *   the sibling readings assess the same institution differently and are
 *   authored as separate linked stories, not folded into this one. The claim
 *   (tangled_rope) and the metrics are authored independently: the claim
 *   states what this reading believes is structurally true of the
 *   arrangement; the metrics state what is descriptively true of its
 *   operation, including the post-2019 erosion this reading itself must
 *   survive. KEY AGENTS (by structural relationship): -
 *   wto_dsb_adjudicative_apparatus: Agenda-setter (institutional /
 *   identity_locked) — issues reports, administers compliance surveillance;
 *   its authority exists only insofar as reports bind -
 *   export_oriented_major_powers: Primary beneficiary and co-agenda-setter
 *   (institutional / mobile) — wrote the rules, dominate the complainant
 *   docket, hold credible retaliation - multinational_trading_firms:
 *   Secondary beneficiary (organized / arbitrage) — collect predictability
 *   rents across jurisdictions, hedge exposure geographically -
 *   developing_member_state_governments: Primary target (moderate / trapped)
 *   — surrendered discretion at accession, bear compliance costs, cannot
 *   credibly retaliate or exit - domestic_regulatory_agencies: Target with no
 *   seat (powerless / trapped) — their measures are the subject matter; they
 *   have no standing in the proceedings that strike them down -
 *   academic_trade_law_community: Analytical observer (analytical /
 *   analytical) — documents compliance and interpretive drift, staffs
 *   arbitration rosters - civil_society_trade_monitors: Excluded voice
 *   (powerless / mobile) — track protective-measure losses, hold no
 *   procedural role
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.62).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.68).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding-Rulings Regime (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '9ab8723e-cc96-4514-8f25-afe4c657d2ee').
narrative_ontology:cs_kernel_codification('9ab8723e-cc96-4514-8f25-afe4c657d2ee', fixed_text).
narrative_ontology:cs_authority_grounding('9ab8723e-cc96-4514-8f25-afe4c657d2ee', lineage).
narrative_ontology:cs_interpretation_layer_present('9ab8723e-cc96-4514-8f25-afe4c657d2ee').
narrative_ontology:cs_reading_relation('9ab8723e-cc96-4514-8f25-afe4c657d2ee', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('9ab8723e-cc96-4514-8f25-afe4c657d2ee', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('9ab8723e-cc96-4514-8f25-afe4c657d2ee', foundational, treaty_ratification_creates_binding_adjudication_obligation).
narrative_ontology:cs_axiom_status(treaty_ratification_creates_binding_adjudication_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9ab8723e-cc96-4514-8f25-afe4c657d2ee', treaty_ratification_creates_binding_adjudication_obligation, conventional).
narrative_ontology:cs_axiom('9ab8723e-cc96-4514-8f25-afe4c657d2ee', foundational, interpretation_confined_to_negotiated_text).
narrative_ontology:cs_axiom_status(interpretation_confined_to_negotiated_text, holdable).
narrative_ontology:cs_axiom_grounding('9ab8723e-cc96-4514-8f25-afe4c657d2ee', interpretation_confined_to_negotiated_text, conventional).
narrative_ontology:cs_reference_frame('9ab8723e-cc96-4514-8f25-afe4c657d2ee', uruguay_sovereignty_for_market_access_bargain).
narrative_ontology:cs_drift_state('9ab8723e-cc96-4514-8f25-afe4c657d2ee', post_appellate_body_crisis, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9ab8723e-cc96-4514-8f25-afe4c657d2ee', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_oriented_major_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, multinational_trading_firms).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, developing_member_state_governments).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, pacta_sunt_servanda).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, effective_treaty_interpretation).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, rule_based_trade_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Panelists and Appellate Body members, supported by the Secretariat's legal division, hear disputes, issue reports, and monitor implementation through Article 21.6 reviews. Reports take effect automatically under negative-consensus adoption. The institution's standing rests entirely on its reports being treated as authoritative statements of members' obligations; its members serve fixed terms, are seated by consensus, and have no institutional life outside the system they administer. When the United States blocked new Appellate Body appointments from 2019 onward, sitting members continued serving past term end to finish pending appeals rather than let the docket die.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_dsb_adjudicative_apparatus, agenda_setter,
    institutional, generational, identity_locked, global).

% Large trading economies — the United States, the European Union, Japan, and increasingly China — negotiated the system's rules, nominate its adjudicators, and hold the credible retaliatory capacity that makes authorized countermeasures bite. Their exporters receive enforceable guarantees of market access across more than 160 jurisdictions, and their industries dominate the complainant docket. They also appear as defendants and sometimes lose — the EU in the hormone-beef dispute, the United States in the foreign-sales-corporation and steel cases — and on those occasions accept retaliation, negotiate compensation, or adjust the measure. Leaving the system would mean replacing guaranteed access with bilateral bargains struck under raw market power, which they could afford but consistently prefer not to.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_oriented_major_powers, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, export_oriented_major_powers, agenda_setter).

% Exporters and investors with supply chains spanning many jurisdictions plan production and pricing around the expectation that tariff bindings and nondiscrimination rules will be enforced somewhere. They press home governments to bring cases, supply much of the evidence panels rely on, and shift sourcing when a jurisdiction's treatment turns hostile. Exposure to any single adverse ruling is hedged by geographic diversification.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, multinational_trading_firms, beneficiary,
    organized, biographical, arbitrage, global).

% Governments of smaller and lower-income members accepted disciplines covering subsidies, intellectual property, sanitary standards, and services as the price of accession and market access. Defending or mounting a case requires specialized counsel costing millions; several maintain no permanent delegation in Geneva. When they lose, they must rewrite domestic law within a reasonable period of time or face suspension of concessions against exports they depend on; when they win against a large economy, the authorized retaliation is often worth little because their markets are small relative to the injury. Withdrawal would forfeit most-favored-nation access to every major market simultaneously.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, developing_member_state_governments, payer,
    moderate, biographical, trapped, global).

% National food-safety, health, environmental, and consumer-protection authorities draft measures — hormone bans, dolphin-safe labeling, tobacco plain packaging, renewable-content rules — that trading partners challenge as barriers. The agencies have no standing before panels: proceedings run between states, amicus submissions are admitted only at panel discretion, and the officials who wrote the measure learn of its fate when the report issues. Compliance typically means rescinding or diluting the measure; refusal means their country's exports elsewhere absorb retaliation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulatory_agencies, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, domestic_regulatory_agencies, excluded).

% Scholars and practitioners of international economic law document compliance rates, classify Appellate Body interpretive moves, and debate whether the system applies or makes law. They staff arbitration rosters (including the MPIA roster assembled after the Appellate Body paralysis), publish the case analyses governments cite, and maintain the GATT-era historical record against which the system's novelty is judged.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, academic_trade_law_community, observer,
    analytical, generational, analytical, global).

% Public-interest organizations, labor federations, and development groups track how rulings affect health, environmental, and labor protections. They petition for amicus participation, publish shadow analyses of pending disputes, and campaign over measures struck down — but hold no procedural role: cases belong to states, hearings are closed, and their concerns enter only when a government chooses to voice them.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, civil_society_trade_monitors, excluded,
    powerless, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, export_oriented_major_powers).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts bilateral trade frictions from discretionary retaliation into rule-based adjudication: disputing members get a neutral forum, automatically adopted reports, and a supervised compliance path, so exporters can treat market-access commitments as enforceable rather than revocable at a partner's whim. It solves the collective-action problem of mutual restraint from unilateral trade weapons that defined the GATT's later decades.
% TRANSFER_FUNCTION: Moves policy discretion from national capitals to multilateral adjudication within covered domains; moves the right to retaliate from unilateral discretion to DSB authorization; and moves compliance costs toward defendants — disproportionately smaller, capacity-poor states — while market-access security accrues to exporters operating from large economies.
% ABSENT_VOICES: Domestic regulatory agencies and the publics behind the measures struck down have no standing; civil-society and labor voices are confined to discretionary amicus channels; developing-country defendants without Geneva delegations are outspent in every proceeding. Negative-consensus adoption records unanimity partly because the seats that would dissent were never in the room.
% DISAPPEARANCE_RATIONALE: Overnight disappearance returns disputes to unilateral determination and retaliation: import measures would answer perceived infractions directly, market access would be repriced through bilateral power bargains, and the web of scheduled commitments would unravel as each member reassessed what its concessions were worth without an enforcement backstop.
% FOUNDING_PROBLEM: GATT dispute settlement produced non-binding reports that violating governments could block, leaving aggrieved exporters to their governments' unilateral weapons — Section 301 investigations, voluntary export restraints, raw power bargaining. Uruguay Round negotiators built automatic, binding adjudication to lock in market-access concessions and disarm unilateralism.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists but is layered. Independent scholarship outside the major-capital beneficiary set documents the GATT-era blockage record, and the post-2019 reversion to unilateral tariff warfare during the Appellate Body paralysis shows the founding problem recurring when enforcement weakens — evidence from outside the benefiting parties. The strongest present-need attestation still comes from the system's own architects and heavy users, so the genealogy is corroborated at the historical layer and contested at the present-need layer.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) sits in tangled-rope territory because the arrangement delivers real, widely consumed coordination — peaceful settlement, stabilized market-access expectations — while concentrating its costs: compliance burdens fall on defendants, disproportionately capacity-poor ones, and the discretion surrendered at accession is not recovered equally by all members. Suppression (0.68) reflects the enforcement spine: negative-consensus adoption makes reports self-executing, authorized retaliation prices non-compliance, and withdrawal from the WTO forfeits most-favored-nation access to every major market at once, so exit is nominally open and practically prohibitive. Theater (0.35) is moderate and rising: adjudication is real work, but the post-2019 share of activity that is performative — appeals filed into a paralyzed Appellate Body purely to stall adoption, compliance reviews of implementation everyone knows is stalled — has grown. Accessibility collapse (0.52): alternatives exist (bilateral FTAs, plurilaterals, unilateral measures) but none reproduces globally enforceable access at scale, so understanding the constraint narrows options substantially without closing them. Resistance (0.58) is substantial and organized: appointment blockage, void appeals, reasonable-period stretching, and periodic defection threats. All three tracked series share one time grid (points 0–30, step 5); trajectories are monotonic drift, not cyclical — no intermittent-reinforcement mechanism is claimed. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine, via directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical formal status: every WTO member is formally equal in the DSB, yet the complainant-heavy major-power seats experience the arrangement as enforceable access they purchased, the defendant-heavy developing-state seats experience it as obligations serviced under retaliation threat, and the apparatus seat experiences it as the constituting condition of its own authority. Same-level lateral divergence runs on constraint-specific factors rather than global power rank alone: litigation budgets (millions per dispute), retaliation credibility (market size determines whether authorized countermeasures hurt), and Geneva staffing. Coalition potential partially offsets the weakest seats: developing members have pooled complaints and built the MPIA to preserve binding appeal after the paralysis, raising their effective seat power without changing their formal one — the engine should read their per-seat classification with that coalition capacity in view.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: major powers collect enforceable access and hold the retaliation capacity that makes enforcement credible — though slightly above the pure-beneficiary floor because bindingness binds them too when they defend (hormones, FSC, steel), they remain net collectors. Multinational firms sit nearest the beneficiary pole: geographic arbitrage hedges any single ruling. Developing-state governments derive near-full-target directionality: the victim declaration plus trapped exit (withdrawal forfeits all MFN access) places them at the extracted end. Domestic regulatory agencies sit at the extreme target end — they bear the constraint's subject-matter costs with zero procedural exit. The adjudicative apparatus is subsidized by the constraint: its authority exists only while reports bind, giving it near-beneficiary directionality despite administering rather than collecting. Receipt of the arrangement's gains concentrates in the major-power complainant seats — enforceable access for their exporters and meaningful retaliation rights — so gain_flow names that seat rather than diffuse. No directionality overrides are authored: the derivation from beneficiary/victim declarations, power atoms, and exit options captures these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unilateral trade warfare under a non-binding GATT system — is contested rather than dead: the post-2019 tariff waves show the problem recurring where enforcement weakens, arguing the mandate is live; the system's drift toward policing regulatory diversity rather than border barriers argues the mandate has migrated. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination would erase the capacity-asymmetric compliance burden its victims document; reading it as pure extraction would erase the settlement function even its critics use. The live degradation pathway is toward piton, not snare: if void appeals normalize and compliance becomes optional, the arrangement persists theatrically — reports issue, surveillance continues — while bindingness hollows out; the rising theater_ratio series is the early indicator. Mandatrophy is not resolved: the arrangement has neither outlived its function nor been retired. Fixing — unwinding bindingness — is prohibitive for the membership that could do it: it would forfeit the access security every exporter plans around and reopen thousands of scheduled commitments, which is why the arrangement survives its crises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the DSB''s operative constraint accurately characterized by this binding-referee reading, or by a sibling reading of the same kernel (advisory coordination, judicial activism)?',
    'Observe member conduct under adverse rulings across stress periods: compliance-or-priced-breach behavior supports bindingness; settlement patterns insensitive to ruling outcomes support the advisory characterization; systematically successful interpretive-overreach challenges support the activism framing.',
    'If the advisory reading is correct, measured compliance pressure reflects bargaining leverage rather than obligation and the constraint classifies nearer rope; if the activism reading is correct, the arrangement''s extraction includes interpretive rents no member consented to, raising effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the wto_dsb_authority kernel characterizes the operative constraint; this story instantiates only the binding-referee reading and routes the contest here.').

omega_variable(
    bindingness_decay_trajectory,
    'Does the binding-referee constraint survive the Appellate Body appointment crisis, or is bindingness decaying toward advisory reality?',
    'Track post-2019 compliance rates, the frequency of ''appeal into the void'' filings used to stall report adoption, and uptake of the MPIA interim appeal mechanism; sustained void-appealing without acceptance of retaliation costs indicates decay.',
    'Decay would reduce effective suppression, continue raising theater_ratio, and shift classification toward piton — hollowed authority maintained theatrically — while the coordination shell persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_decay_trajectory, empirical, 'Durability of bindingness under active repudiation by a founding member.').

omega_variable(
    sovereignty_entrenchment_depth,
    'Is the surrendered policy discretion irreversibly entrenched, or recoverable through exit, amendment, or priced non-compliance?',
    'Examine the amendment record (the DSU core has never been amended), the credibility of withdrawal threats, and whether members treat retaliation payments as ordinary operating cost (recoverable discretion) or as breach to be avoided (entrenched obligation).',
    'If discretion is recoverable, the trapped-exit coding overstates lock-in and long-run classification stability falls; if entrenched, the suppression component is durable and the sovereignty-transfer premise of this reading holds across power shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_entrenchment_depth, conceptual, 'Reversibility of the sovereignty-for-market-access exchange.').

omega_variable(
    enforcement_asymmetry_contingency,
    'Is the asymmetric incidence of compliance costs — concentrated on capacity-poor defendants — intrinsic to the design or contingent on the current distribution of market power?',
    'Compare compliance-cost incidence across defendant capacity tiers over the interval; observe whether rising complainant capacity among developing members (BRIC complainants, African Group initiatives) flattens the gradient.',
    'If the asymmetry is contingent, extraction tracks power distributions and could invert as complainant capacity diffuses; if intrinsic to design features (implementation burden, retaliation economics), the classification is stable across power shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_contingency, empirical, 'Whether the extraction asymmetry is design-intrinsic or power-contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t0, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t5, wto_dsb_authority__binding_referee_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t5, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t10, wto_dsb_authority__binding_referee_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t10, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t15, wto_dsb_authority__binding_referee_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t15, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t20, wto_dsb_authority__binding_referee_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t20, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t25, wto_dsb_authority__binding_referee_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t25, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_tr_t30, wto_dsb_authority__binding_referee_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto_dsb_binding_referee_be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t0, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t5, wto_dsb_authority__binding_referee_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t5, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t10, wto_dsb_authority__binding_referee_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t10, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t15, wto_dsb_authority__binding_referee_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t15, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t20, wto_dsb_authority__binding_referee_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t20, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t25, wto_dsb_authority__binding_referee_reading, base_extractiveness, 25, 0.64).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t25, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_be_t30, wto_dsb_authority__binding_referee_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_binding_referee_su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t0, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t5, wto_dsb_authority__binding_referee_reading, suppression_requirement, 5, 0.51).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t5, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t10, wto_dsb_authority__binding_referee_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t10, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t15, wto_dsb_authority__binding_referee_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t15, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t20, wto_dsb_authority__binding_referee_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t20, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t25, wto_dsb_authority__binding_referee_reading, suppression_requirement, 25, 0.66).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t25, observed).
narrative_ontology:measurement(wto_dsb_binding_referee_su_t30, wto_dsb_authority__binding_referee_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(wto_dsb_binding_referee_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'WTO dispute settlement.' The single institution supports three structurally distinct constraints corresponding to three readings of the wto_dsb_authority kernel, each with its own epsilon, beneficiary/victim structure, and classification: this binding-referee story (reports bind; discretion surrendered; non-compliance is breach), the advisory-coordination story (reports are expert opinions facilitating settlement; discretion retained; materially lower epsilon), and the judicial-activism story (bindingness conceded but interpretive scope contested; extraction includes alleged unauthorized interpretive rents). The binding reading is the formal-legal baseline from which the other two deviate: the advisory reading denies its core premise outright, and the activism reading presupposes its bindingness premise while attacking its interpretive boundary. Each story links the others via network.affects_constraints; contamination propagates across the family because the credibility of any one reading conditions the enforcement environment of the rest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
