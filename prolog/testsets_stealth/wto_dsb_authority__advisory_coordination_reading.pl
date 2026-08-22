% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO Dispute Settlement as Advisory Coordination Mechanism
 *   domain: international law/trade governance/institutional legitimacy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'WTO DSB
 *   authority': the advisory-coordination reading, under which panels supply
 *   expert opinions that facilitate negotiated settlements and member states
 *   retain ultimate policy discretion. The ε referent is the standing
 *   arrangement under contest — the DSB as it actually operates under low
 *   compliance pressure — assessed by this reading's own lights, which
 *   acknowledge both the forum's genuine settlement-facilitation value and
 *   its reliance on bilateral power dynamics for enforcement. The sibling
 *   readings (binding_referee_reading: rulings bind as treaty obligations;
 *   judicial_activism_reading: panels legislate through interpretive drift)
 *   are DIFFERENT constraints with different ε values, victim sets, and
 *   enforcement bases; they are authored as separate files and linked through
 *   network.affects_constraints. The colloquial label 'WTO dispute settlement
 *   authority' decomposes because asking whether the DSB binds yields a
 *   different structural object than asking whether it advises or whether it
 *   overreaches — each question fixes its own ε. KEY AGENTS (by structural
 *   relationship): - major_trading_powers: Primary beneficiary and de facto
 *   agenda-holder (institutional/arbitrage) — converts findings into
 *   negotiating leverage and actively maintains the advisory character -
 *   small_developing_economies: Primary target (powerless/trapped) — bears
 *   proceeding costs, collects relief only by respondent consent -
 *   middle_power_complainants: Secondary target with partial recovery
 *   (organized/constrained) - wto_secretariat: Administrator
 *   (institutional/identity_locked) — runs the process, captures
 *   institutional continuity - geneva_dispute_lawyers: Incidental beneficiary
 *   (moderate/mobile) - civil_society_trade_monitors: Excluded voice
 *   (organized/constrained) - academic_trade_regime_scholars: Analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.46).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.38).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO Dispute Settlement as Advisory Coordination Mechanism").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international law/trade governance/institutional legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__advisory_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, 'fd933a70-cec3-4c20-b478-441a0be14e32').
narrative_ontology:cs_kernel_codification('fd933a70-cec3-4c20-b478-441a0be14e32', fixed_text).
narrative_ontology:cs_authority_grounding('fd933a70-cec3-4c20-b478-441a0be14e32', expertise).
narrative_ontology:cs_interpretation_layer_present('fd933a70-cec3-4c20-b478-441a0be14e32').
narrative_ontology:cs_reading_relation('fd933a70-cec3-4c20-b478-441a0be14e32', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd933a70-cec3-4c20-b478-441a0be14e32', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('fd933a70-cec3-4c20-b478-441a0be14e32', foundational, member_consent_bounds_obligation).
narrative_ontology:cs_axiom_status(member_consent_bounds_obligation, holdable).
narrative_ontology:cs_axiom_grounding('fd933a70-cec3-4c20-b478-441a0be14e32', member_consent_bounds_obligation, conventional).
narrative_ontology:cs_axiom('fd933a70-cec3-4c20-b478-441a0be14e32', foundational, settlement_facilitation_primary_purpose).
narrative_ontology:cs_axiom_status(settlement_facilitation_primary_purpose, holdable).
narrative_ontology:cs_axiom_grounding('fd933a70-cec3-4c20-b478-441a0be14e32', settlement_facilitation_primary_purpose, instrumental).
narrative_ontology:cs_reference_frame('fd933a70-cec3-4c20-b478-441a0be14e32', gatt_consultative_diplomacy_tradition).
narrative_ontology:cs_drift_state('fd933a70-cec3-4c20-b478-441a0be14e32', post_appellate_body_paralysis, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('fd933a70-cec3-4c20-b478-441a0be14e32', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, major_trading_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, wto_secretariat).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, geneva_dispute_lawyers).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, small_developing_economies).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, middle_power_complainants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, middle_power_complainants).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__advisory_coordination_reading, diplomatic_settlement_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% File and answer complaints, sit on the consensus bodies that adopt procedures and appointments, and decide case by case whether a panel report changes policy or becomes a talking point in bilateral talks. Their market size lets them wait out opponents, offer side payments, or absorb the reputational cost of leaving findings unimplemented. Several have parallel dispute channels in preferential agreements they dominate. They collectively hold the appointment power that has kept the appellate bench empty since late 2019, which is what keeps findings advisory in practice.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, major_trading_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, major_trading_powers, agenda_setter).

% Bring complaints against larger partners, financing years of proceedings from scarce budget lines and borrowed expertise. They obtain written findings that produce relief only if the respondent consents to implement; settlement offers arrive priced to their share of the respondent's export market. Withdrawal from the organization would forfeit bound tariff access everywhere at once, so staying in the process is not optional even when the process returns little.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, small_developing_economies, payer,
    powerless, generational, trapped, global).

% Litigate seriously and win often enough to recoup part of their investment; they extract real concessions in implementation negotiations because they command meaningful market access of their own. Their loss sits in the gap between what a panel finds and what they can collect, and in cases that stall when the respondent prefers delay to settlement.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, middle_power_complainants, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, middle_power_complainants, beneficiary).

% Staffs panels, drafts the legal analyses, maintains the docket and rules of procedure, and brokers procedural compromises among delegations. Its budget and mandate depend on continued member use of the process; generations of careers and institutional memory are invested in the multilateral forum, and its leadership publicly campaigns for members to restore full functioning of the appellate tier.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_secretariat, agenda_setter,
    institutional, generational, identity_locked, global).

% Counsel governments and firms through proceedings; demand for their services scales with the volume of disputes filed regardless of whether findings are ever implemented. They move fluidly between private practice, national capitals, and secretariat secondments.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, geneva_dispute_lawyers, beneficiary,
    moderate, biographical, mobile, global).

% Track dispute outcomes for their effects on labor, health, and environmental regulation. They may submit written briefs that panels rarely admit and hold no seat in consultations; their objections surface in domestic ratification politics rather than inside the process.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, civil_society_trade_monitors, excluded,
    organized, biographical, constrained, national).

% Compile compliance rates, settlement terms, and doctrinal development across the case law. Their datasets and critiques inform reform debates in capitals and in the organization itself, but they hold no vote and administer nothing.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, academic_trade_regime_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__advisory_coordination_reading, major_trading_powers).
narrative_ontology:fixing_cost_class(wto_dsb_authority__advisory_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, expert-mediated forum that converts trade disputes from threats of unilateral retaliation into documented disagreements with jointly accepted facts and legal framing, creating focal points, delaying mechanisms, and face-saving paths to negotiated settlement.
% TRANSFER_FUNCTION: Moves litigation costs and bargaining leverage: complainants transfer money, years of process, and political capital into the forum; respondents transfer concessions only voluntarily; panel findings transfer argumentative legitimacy to whichever side's position they support — in practice disproportionately to states whose market power lets them wait out or buy out opponents.
% ABSENT_VOICES: Civil society monitors, affected workers and consumers, and non-state industries outside the consultative machinery would object that settlements trade away regulatory protections behind closed doors; they stand outside the consultations, admitted only as rarely-accepted amicus submissions. Advocates of compulsory binding adjudication were likewise never given a ratification-level vote on the advisory-only equilibrium that now operates.
% DISAPPEARANCE_RATIONALE: If the advisory process vanished overnight, disputes would revert to unilateral retaliation statutes and embassy-level bargaining; the Geneva ecosystem of panels, counsel, and secretariat divisions would dissolve or migrate into preferential agreement chapters; small members would lose the one venue where their complaints generate written records at all.
% FOUNDING_PROBLEM: Mid-century trade conflict ran through tit-for-tat retaliation and diplomatic deadlock in which smaller states had no recourse at all; the 1947 and 1995 designs sought a rules-based forum that would channel disputes away from trade wars and give every member a documented hearing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: interwar trade-war historiography on the retaliatory tariff spirals of the 1930s, the published GATT negotiating record, and regime scholarship by researchers unaffiliated with the major trading powers; small-member ambassadors' ministerial statements independently attest the exclusion problem the forum was built to address. The major powers' own attestation alone would be insufficient, but the external historical record stands on its own.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).
:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.46 at interval end) because the advisory arrangement's costs are real but partly offset by its outputs: complainants receive expert fact-finding and a written record that anchors negotiations, while the power-skew in settlement terms and the uncollectability of findings against large respondents constitute a steady transfer toward market-heavy states. Suppression is moderate-low (0.38): compliance is formally voluntary, which is this reading's defining feature, but membership lock-in and the consensus rule foreclose migration to any compulsory-adjudication alternative. Accessibility collapse is low (0.35) — bilateral diplomacy, retaliation statutes, and preferential-agreement chapters remain visible, usable substitutes. Resistance is moderate-low (0.32): reform coalitions, compliance refusals, and appointment blocks meet the arrangement but do not threaten it. Theater ratio (0.32) reflects a growing performative share: reports increasingly function as positional documents, and appeals lodged into a vacant appellate tier since 2019 are pure procedure. The three temporal series share one grid (1995/2001/2007/2013/2019/2026) so every metric is authored at every examined point. Extractiveness rises monotonically as enforcement decays into bilateral power dynamics; theater rises as findings become negotiation inputs; suppression_requirement FALLS — this is an enforcement-decay trajectory, tracing the erosion of the quasi-binding compliance machinery (high-water compliance culture through roughly 2015, appellate paralysis after December 2019), not an intensification. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from the same structure. From the major powers' position the arrangement is sovereignty-preserving expertise they voluntarily fund and occasionally heed — a coordination good they help administer. From the small-economy position the same forum is a costly procedure that ends in a bargain priced to their market share, with the written findings serving mainly as moral leverage. Middle powers straddle: enough market access to collect real settlements, not enough to compel them. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Major trading powers are declared beneficiaries with arbitrage-grade exit (parallel preferential channels), placing them near the beneficiary end of directionality — the arrangement subsidizes them with legitimacy and leverage. Small developing economies are declared victims with trapped exit, placing them near the full-target end. Middle-power complainants are declared victims but hold organized power and partial recovery; the automatic derivation from victim status would push them toward the full-target end, overstating their position, so an explicit override sets the organized-power class to d=0.62 — net payers who nonetheless collect genuine settlement value. Collateral note: the override applies at the power-atom level, so the excluded civil-society seat (also organized) inherits it; that seat contributes negligibly to the computation as an excluded voice. The secretariat is an administrator whose gains are institutional continuity rather than captured transfer; its near-symmetric position follows from its agenda_setter role without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — channeling disputes away from retaliatory spirals and giving small members a documented hearing — remains live, so no mandatrophy resolution is declared and the founding-problem/disappearance pairing (live x world_rearranges) raises no zombie flag. Classification as tangled_rope prevents both symmetric misreadings: reading the arrangement as pure coordination would hide the power-skewed transfer that small and middle complainants finance; reading it as pure extraction would erase the real information, focal-point, and face-saving functions that keep every member at the table and that even losing complainants cite when they return. The hybrid is the structurally honest verdict: one mechanism, genuine coordination function, asymmetric incidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_wto_dsb_authority,
    'Which reading of the wto_dsb_authority kernel correctly describes the DSB''s operative outputs — advisory opinion (this reading), binding ruling (binding_referee_reading), or illegitimate legislation (judicial_activism_reading)? This constraint is one reading of that kernel; the siblings instantiate different constraints with different epsilon, victim sets, and enforcement bases.',
    'Observe compliance behavior, member statements in the Trade Policy Review and Ministerial record, and appointment politics: sustained voluntary implementation of adverse findings supports the binding reading; open non-implementation met with renegotiation supports the advisory reading; systematic doctrinal expansion beyond textual mandate supports the activism reading.',
    'If the binding reading is descriptively accurate, epsilon and suppression recompute under a treaty-obligation frame with materially higher stakes; if the activism reading, extraction relocates from bilateral power-skew to unauthorized obligation creation and the victim set shifts to all members subject to drifted doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_wto_dsb_authority, conceptual, 'Which reading of the DSB-authority kernel the standing arrangement actually instantiates.').

omega_variable(
    power_asymmetry_extraction_share,
    'How much of the measured extraction is attributable to bilateral power dynamics in settlement and implementation, as opposed to inherent coordination cost of running any expert dispute forum?',
    'Compare settlement terms and implementation rates across dyads of matched legal merit but divergent market-size ratios; isolate the residual explained by respondent market weight.',
    'If power-skew dominates, the arrangement''s effective extraction rises sharply for trapped complainants and the tangle deepens toward its extractive pole; if coordination cost dominates, the arrangement sits close to a clean coordination mechanism with incidental asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_extraction_share, empirical, 'Share of extraction driven by bilateral power versus inherent forum cost.').

omega_variable(
    consensus_rule_dual_role,
    'Is the consensus requirement a sovereignty safeguard that all members equally enjoy, or is it in operation the enforcement mechanism by which the largest member maintains the advisory character against migration to binding adjudication?',
    'Trace the fate of weighted-voting and anti-blocking reform proposals, and the single-member appointment blockage of the appellate tier since 2019: if one actor''s veto reliably produces the advisory equilibrium, the rule functions as that actor''s maintenance instrument.',
    'If the latter, part of the measured suppression is deliberately manufactured rather than emergent, raising the arrangement''s suppression profile and sharpening the asymmetry between the blocking seat and trapped complainants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_rule_dual_role, conceptual, 'Whether the consensus rule protects everyone or enforces the advisory equilibrium for the blocker.').

omega_variable(
    small_state_coalition_potential,
    'Can small developing economies convert numerical strength into coalition power — through joint complainant groups, the Advisory Centre on WTO Law, or cross-issue voting blocs — sufficient to alter settlement terms that bilateral power dynamics currently set?',
    'Track outcomes of multi-complainant cases and ACWL-supported litigation against large respondents relative to solo small-state filings.',
    'If coalition channels work, the trapped-victim seat''s effective extraction falls and the arrangement''s asymmetry narrows without institutional reform; if they fail, the power-skew is structural and durable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_state_coalition_potential, empirical, 'Whether collective action can offset bilateral power asymmetry for small complainants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 1995, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_advisory_tr_t1995, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2001, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2007, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2007, 0.23).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2013, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2013, 0.26).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2019, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2019, 0.29).
narrative_ontology:measurement(wto_dsb_advisory_tr_t2026, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2026, 0.32).

% Extraction over time
narrative_ontology:measurement(wto_dsb_advisory_be_t1995, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement(wto_dsb_advisory_be_t2001, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2001, 0.36).
narrative_ontology:measurement(wto_dsb_advisory_be_t2007, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2007, 0.39).
narrative_ontology:measurement(wto_dsb_advisory_be_t2013, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2013, 0.41).
narrative_ontology:measurement(wto_dsb_advisory_be_t2019, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2019, 0.44).
narrative_ontology:measurement(wto_dsb_advisory_be_t2026, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2026, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_advisory_su_t1995, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(wto_dsb_advisory_su_t2001, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(wto_dsb_advisory_su_t2007, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2007, 0.46).
narrative_ontology:measurement(wto_dsb_advisory_su_t2013, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2013, 0.43).
narrative_ontology:measurement(wto_dsb_advisory_su_t2019, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2019, 0.4).
narrative_ontology:measurement(wto_dsb_advisory_su_t2026, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2026, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, judicial_activism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'WTO dispute settlement authority' decomposes into three structurally distinct constraints — this advisory-coordination reading (moderate epsilon; victims are complainants whose relief depends on respondent consent), the binding_referee_reading (epsilon indexed to sovereignty surrendered under treaty obligation), and the judicial_activism_reading (epsilon indexed to obligations created by interpretive drift). Each has its own stable epsilon, beneficiary/victim structure, and classification; they are linked here because the upstream DSU text and compliance record are cited as evidence within the sibling stories. Historically the binding reading was upstream (the 1995 DSU's apparent design); operationally the advisory reading is currently ascendant following appellate paralysis; the activism reading is parasitic on binding pretensions it denies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__advisory_coordination_reading, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
