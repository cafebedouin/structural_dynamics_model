% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: DSB Advisory Coordination Reading: Panels as Expert Inputs to Negotiated Settlement
 *   domain: international law/trade governance/institutional legitimacy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   wto_dsb_authority: the advisory-coordination reading, on which DSB panels
 *   provide expert advisory opinions that facilitate negotiated settlements
 *   and member states retain ultimate policy discretion. The standing
 *   arrangement under contest — the WTO dispute settlement system — is
 *   assessed here by this reading's own lights: as a consent-based
 *   expert-input mechanism with low compliance pressure, where enforcement of
 *   outcomes depends on bilateral power dynamics rather than institutional
 *   authority. Per the epsilon-referent rule, epsilon is authored for THAT
 *   standing arrangement as this reading constitutes it, never for the
 *   binding arrangement the sibling reading would describe. The sibling
 *   readings (binding_referee_reading, judicial_activism_reading) are
 *   separate constraints in separate files; the contest between readings is
 *   routed to omega variables, not folded into this classification. KEY
 *   AGENTS (by structural relationship): - wto_secretariat: Administrator
 *   ([organized]/[constrained]) — services panels and drafts reports;
 *   institutionally sustained by dispute flow - major_trading_powers: Primary
 *   beneficiary ([institutional]/[arbitrage]) — converts findings into
 *   bargaining leverage at will - small_developing_members: Secondary
 *   beneficiary ([powerless]/[constrained]) — obtains expert assessment
 *   cheaply, cannot compel outcomes - mid_sized_exporting_members: Primary
 *   payer ([moderate]/[mobile]) — funds the process in fees and concessions,
 *   receives settlement anchors - domestic_affected_constituencies: Excluded
 *   seat ([powerless]/[trapped]) — their regulatory space is the substance
 *   traded - independent_trade_law_scholars: Analytical observer
 *   ([analytical]/[analytical]) — documents the settlement-prediction record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.41).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.34).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.27).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "DSB Advisory Coordination Reading: Panels as Expert Inputs to Negotiated Settlement").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international law/trade governance/institutional legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, '03b13d6d-d8a2-48c5-893b-8c6dca90a6c3').
narrative_ontology:cs_kernel_codification('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', formalized).
narrative_ontology:cs_authority_grounding('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', expertise).
narrative_ontology:cs_interpretation_layer_present('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3').
narrative_ontology:cs_reading_relation('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', foundational, state_consent_supremacy).
narrative_ontology:cs_axiom_status(state_consent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', state_consent_supremacy, conventional).
narrative_ontology:cs_axiom('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', foundational, expert_findings_as_negotiation_inputs).
narrative_ontology:cs_axiom_status(expert_findings_as_negotiation_inputs, holdable).
narrative_ontology:cs_axiom_grounding('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', expert_findings_as_negotiation_inputs, instrumental).
narrative_ontology:cs_reference_frame('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', consent_based_advisory_settlement_framework).
narrative_ontology:cs_drift_state('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', contemporary_post_appellate_crisis, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('03b13d6d-d8a2-48c5-893b-8c6dca90a6c3', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, major_trading_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, small_developing_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, mid_sized_exporting_members).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, major_trading_powers).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, mid_sized_exporting_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Services the dispute system: maintains the docket, assists disputing governments in constituting panels, drafts panel reports, and publishes findings. Its staffing, budget line, and professional purpose depend on a steady flow of disputes to administer. Exit for its legal staff means leaving the institution and the professional field built around it.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_secretariat, agenda_setter,
    organized, generational, constrained, global).

% File and receive the largest share of complaints, supply most of the legal talent, and fund a proportional share of the system. An adverse expert finding costs them little they cannot discount: they can slow implementation, reroute the substance through preferential agreements, or fold the finding into a broader bargain. They gain a legitimacy resource they can invoke or set aside as their bargaining position requires.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, major_trading_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, major_trading_powers, payer).

% Use the process to obtain expert assessment of stronger partners' measures that they could never compel bilaterally. The assessment gives them a documented position and negotiating cover, but converting it into changed behavior on the other side depends on the partner's susceptibility to reputation and market-access pressure rather than on any institutional compulsion. Leaving the system would forfeit their only affordable route to third-party factual findings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, small_developing_members, beneficiary,
    powerless, biographical, constrained, global).

% Are the heaviest per-capita users: they bring enough disputes to need the process regularly and pay for it in legal fees, diplomatic attention, and the concessions packaged into settlements. They receive usable settlement anchors in return, and unlike the smallest members they can sometimes shift outcomes through coalitions or market size. When findings cut against them they bargain directly with larger partners instead.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, mid_sized_exporting_members, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, mid_sized_exporting_members, beneficiary).

% Consumer, labor, environmental, and producer groups inside member countries whose regulatory protections and prices are the substance of the bargains struck. Disputes are framed and settled between governments; these groups have no seat, no notice, and no standing in the process, and learn of trades affecting them after positions have been exchanged.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, domestic_affected_constituencies, excluded,
    powerless, biographical, trapped, national).

% Analyze panel reports, settlement patterns, and institutional design from outside the process. They document how often findings predict settlements, whose findings get implemented, and how the system's character changes across crises. Their stake is reputational and intellectual rather than material.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, independent_trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__advisory_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(wto_dsb_authority__advisory_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts bilateral trade frictions into jointly legible expert assessments that anchor negotiated settlements — lowering the cost of face-saving compromise and reducing misperception about what the facts and legal merits actually are.
% TRANSFER_FUNCTION: Moves legal-argument labor, diplomatic attention, and negotiated concession packages among member governments, and moves reputational standing toward positions that survive expert scrutiny; it moves nothing coercively.
% ABSENT_VOICES: Domestic constituencies whose regulatory space is the substance of the bargains — consumers, workers, environmental and producer groups — plus operators of preferential-agreement forums who would compete for dispute resolution. They would object that settlements trade away their interests behind closed doors; they are outside the room because the process is constituted as a government-to-government exchange.
% DISAPPEARANCE_RATIONALE: If the advisory machinery vanished overnight, disputes would revert to raw bilateral power bargaining: small members would lose their only affordable access to credible third-party factual findings, the accumulated body of shared legal argumentation would dissipate within a few dispute cycles, and major powers would lose a legitimacy resource they currently deploy selectively — the settlement economy would reorganize around overt leverage.
% FOUNDING_PROBLEM: GATT-era trade frictions had no reliable way to establish facts and legal merit, so every dispute escalated into political confrontation or indefinite stalemate; the arrangement was built to give negotiators a credible, face-saving factual anchor for compromise.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: retired panelists and former appellate jurists writing in academic capacity, the international-law scholarship documenting the settlement-predictive value of findings, and trade-policy press reporting through the 2019-2025 crisis all attest that credible expert anchoring remains the system's working function. Member governments also attest it, but they are beneficiaries, so the evidentiary weight rests on the scholarly and practitioner record.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.41, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is moderate-low (0.41 at interval end) because the mechanism's costs are procedural — legal labor, diplomatic attention, concessions voluntarily packaged into settlements — with no coercive transfer; the residual above pure coordination cost reflects the binding-era machinery still partially attached to the arrangement. Suppression (0.34) is reputational and procedural, not physical or legal compulsion: states cannot be forced to comply, only made to explain themselves. Theater_ratio (0.47) is elevated and rising: since the 2019 appellate paralysis, a substantial share of system activity is performative — agenda items carried month over month, appeals filed into a void, compliance proceedings referencing a lapsed apex — while the core advisory-and-settlement function continues underneath. Accessibility_collapse (0.42) is low for a coordination mechanism: bilateral bargaining and preferential forums remain fully workable alternatives. Resistance (0.27) is modest: occasional rejection of findings and forum-shopping, against broad voluntary uptake.
 *   
 *   The temporal series runs on one shared six-point grid (every tracked metric authored at every point). It traces a rise-and-partial-retreat: from this reading's vantage, the 1995-2019 period layered compliance machinery (reverse-consensus adoption, appellate review, authorized suspension of concessions) onto the advisory core, which this reading experiences as growing imposition on member discretion — extractiveness and suppression climb together. The 2019 appellate collapse removed the compliance apex; practice reverted toward consultation-and-settlement with partial interim-arbitration patches, so extractiveness and suppression fall back while theater rises (machinery performing without its apex function). Suppression_requirement is authored precisely because enforcement-capacity change IS the dynamic being traced here — build-up, then decay — not a static picture. Despite theater nearing the 0.5 substitution threshold, this is not an atrophied shell: the advisory function is the live function, actively used and defended by its beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the secretariat's position the arrangement is a functioning service it competently delivers; from the major powers' arbitrage position it is a legitimacy inventory drawn down at will; from the small members' constrained position it is access-without-teeth — real information, no compulsion; from the mid-sized payers' position it is a fair price for usable anchors; from the excluded domestic seat it is a closed door behind which their regulatory environment is traded. The advisory reading's distinctive claim is that this divergence is MUTED — that no governed seat is substantially extracted from — and the authored metrics reflect that claim; the sharpest remaining gap concentrates in the excluded seat, which the reading's own framing places outside the governed set.
 *
 * DIRECTIONALITY LOGIC:
 *   Both declared beneficiary groups derive low directionality: major_trading_powers combine beneficiary position with arbitrage-grade exit (they can reroute around any outcome), sitting nearest the beneficiary end; small_developing_members are beneficiaries with constrained exit, slightly less damped but still subsidized by access they could not buy bilaterally. mid_sized_exporting_members carry the payer role with a beneficiary secondary role, placing them near symmetric. The override exists for the organized power atom because the secretariat declares no beneficiary/victim structural data and would otherwise fall to an arbitrary canonical fallback; structurally the institution is sustained by the arrangement it administers, so d is set to 0.3 — near the beneficiary end, reflecting an administrator whose costs (labor, reputational exposure) are real but outweighed by what the arrangement provides it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible, face-saving factual anchoring for negotiation — is live, and the arrangement's function tracks it, so no mandate obsolescence is declared and the status-x-verdict pair (live x world_rearranges) raises no capture-or-zombie flag. The classification discipline matters here in two directions. First, the post-crisis theater spike invites a degraded-shell misreading: an analyst seeing carry-over agendas and void appeals could classify the arrangement as inertia-maintained performance, but the advisory function is the living core, not residue — the theater is the decaying BINDING superstructure, which belongs to the sibling reading's constraint, not this one. Second, the reverse error: attributing the binding era's high extraction to this reading's arrangement would conflate two epsilon-distinct constraints. Keeping the readings separate is what lets the corpus measure the 2019 transition as a real event in the standing arrangement rather than noise in one over-loaded label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the WTO dispute settlement arrangement adequately described by this reading — panels as expert advisors whose outputs facilitate negotiated settlement while member states retain ultimate policy discretion — or does one of the sibling readings of kernel wto_dsb_authority describe the same standing arrangement better?',
    'Compare member compliance behavior and official statements across the 2019-2025 appellate crisis: if governments treat adopted reports as obligations owed regardless of convenience, the binding_referee_reading''s description fits; if they treat them as bargaining inputs weighted by bilateral power, this reading fits.',
    'If the binding reading is descriptively right, epsilon for the standing arrangement rises sharply, active enforcement flips true, and the computed type moves toward enforced-extraction categories; this file''s low-extraction profile would then describe a superseded configuration rather than the standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the DSB-authority kernel describes the standing arrangement.').

omega_variable(
    advisory_power_asymmetry_residue,
    'Does the power-mediated settlement layer introduce asymmetric extraction through the advisory mechanism itself, or is the asymmetry located entirely in the separate bilateral-bargaining game that surrounds it?',
    'Compare settlement terms reached on identical or near-identical panel findings across power-differentiated dyads; if weaker members systematically concede more for the same finding, extraction flows through the mechanism rather than around it.',
    'If extraction flows through the advisory structure, the constraint decomposes: the information function stays low-extraction while the settlement-conversion layer becomes a separately classified extractive arrangement linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_power_asymmetry_residue, empirical, 'Whether power asymmetry contaminates the advisory mechanism or sits in the adjacent bargaining game.').

omega_variable(
    post_crisis_drift_durability,
    'Is the post-2019 return of negotiated settlement a durable revival of the advisory character, or a transitional decay awaiting restoration of binding machinery?',
    'Track multi-party interim appeal arbitration uptake, the rate at which panel findings are cited as settlement anchors, and member positions in the DSU reform talks through the late 2020s.',
    'Determines whether the falling tail of the extractiveness series is a stable endpoint or a trough before renewed compliance pressure; a durable revival supports the rope classification, restoration of binding machinery pushes the arrangement back toward the binding reading''s profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_crisis_drift_durability, empirical, 'Durability of the advisory character''s post-crisis revival.').

omega_variable(
    kernel_codification_framing,
    'Is the kernel best framed as the formally codified DSU text (a fixed formal instrument whose readings diverge over interpretation), or as the accumulated practice of dispute settlement (an implicit kernel that is whatever the system does)?',
    'Examine whether the sibling readings disagree about the TEXT''s meaning or about WHICH PRACTICE is authoritative; textual disagreement supports the formalized framing, practice disagreement supports the implicit framing.',
    'Under the implicit framing, authority_grounding shifts from expertise toward practice, and the drift_state reference frame becomes GATT-era practice rather than the consent-based advisory framework, changing the computed drift vector.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Alternative framings of the kernel''s codification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_adv_tr_t0, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(wto_dsb_adv_tr_t6, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(wto_dsb_adv_tr_t12, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(wto_dsb_adv_tr_t18, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement(wto_dsb_adv_tr_t24, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(wto_dsb_adv_tr_t30, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 30, 0.47).

% Extraction over time
narrative_ontology:measurement(wto_dsb_adv_be_t0, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(wto_dsb_adv_be_t6, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 6, 0.33).
narrative_ontology:measurement(wto_dsb_adv_be_t12, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(wto_dsb_adv_be_t18, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(wto_dsb_adv_be_t24, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(wto_dsb_adv_be_t30, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 30, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_adv_su_t0, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0, 0.14).
narrative_ontology:measurement(wto_dsb_adv_su_t6, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 6, 0.23).
narrative_ontology:measurement(wto_dsb_adv_su_t12, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 12, 0.37).
narrative_ontology:measurement(wto_dsb_adv_su_t18, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(wto_dsb_adv_su_t24, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(wto_dsb_adv_su_t30, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 30, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, information_standard).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel wto_dsb_authority. The colloquial label 'WTO dispute settlement' covers three structurally distinct claims with widely separated epsilon values: this advisory-coordination reading (low extraction, no enforcement, coordination-dominant), the binding-referee reading (high extraction, active enforcement, compliance obligations surrendered into covered domains), and the judicial-activism reading (extraction via interpretive drift beyond mandate). Each is a separate story with its own epsilon, beneficiaries, and classification, linked here as a family. The upstream claim (the DSU's formal authorization of panel procedure) is cited as evidence by both downstream readings, so this story's edges point at both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__advisory_coordination_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
