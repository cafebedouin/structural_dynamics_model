% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO Dispute Settlement Authority — Judicial Activism Reading (Interpretive Drift as Unauthorized Lawmaking)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested commitment: that the
 *   dispute-settlement organs issue binding rulings whose obligations run
 *   past what members negotiated, produced through interpretive method rather
 *   than ratified text, and enforced through automatic adoption and
 *   authorized retaliation. On this reading the standing arrangement delivers
 *   a real service — structured, reasoned settlement of trade grievances —
 *   while simultaneously transferring policy authority that no legislature
 *   conceded: members discover what their treaties mean only after
 *   adjudicators tell them, and the discovery is binding. The claim and the
 *   metrics are independent authored facts: the type is claimed from this
 *   reading's structural view of the arrangement, and the metrics are
 *   authored from its descriptive record; neither is tuned to the other or to
 *   a predicted engine output.
 *
 * KEY AGENTS:
 *   - appellate_body_members: agenda-setter (institutional / identity_locked) — authors the interpretive rulings; jurisdiction grows with each doctrinal move
 *   - policy_autonomy_member_states: primary target (institutional / arbitrage) — bears struck-down measures and retaliation; uniquely able to resist via consensus veto and appointment blockade
 *   - developing_country_members: primary target (powerless / trapped) — bears expanded obligations without litigation capacity or exit
 *   - domestic_regulators: secondary target (moderate / constrained) — redraft national rules to satisfy unratified case outcomes
 *   - winning_exporting_states: primary beneficiary (powerful / mobile) — collects compliance and enlarged market access
 *   - wto_legal_establishment and export_oriented_industries: secondary beneficiaries (organized) — careers, fees, and enforceable access ride on the case law's growth
 *   - institutional_legitimacy_scholars: analytical observer — tracks the mandate-versus-practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.72).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.55).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO Dispute Settlement Authority — Judicial Activism Reading (Interpretive Drift as Unauthorized Lawmaking)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '0b07d315-8053-46b3-b42b-8a601cdb920c').
narrative_ontology:cs_kernel_codification('0b07d315-8053-46b3-b42b-8a601cdb920c', fixed_text).
narrative_ontology:cs_authority_grounding('0b07d315-8053-46b3-b42b-8a601cdb920c', extraction).
narrative_ontology:cs_interpretation_layer_present('0b07d315-8053-46b3-b42b-8a601cdb920c').
narrative_ontology:cs_reading_relation('0b07d315-8053-46b3-b42b-8a601cdb920c', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('0b07d315-8053-46b3-b42b-8a601cdb920c', wto_dsb_authority__advisory_coordination_reading, influences).
narrative_ontology:cs_axiom('0b07d315-8053-46b3-b42b-8a601cdb920c', foundational, interpretive_drift_creates_unauthorized_obligations).
narrative_ontology:cs_axiom_status(interpretive_drift_creates_unauthorized_obligations, holdable).
narrative_ontology:cs_axiom_grounding('0b07d315-8053-46b3-b42b-8a601cdb920c', interpretive_drift_creates_unauthorized_obligations, empirically_contingent).
narrative_ontology:cs_axiom('0b07d315-8053-46b3-b42b-8a601cdb920c', foundational, obligations_require_negotiated_consent).
narrative_ontology:cs_axiom_status(obligations_require_negotiated_consent, holdable).
narrative_ontology:cs_axiom_grounding('0b07d315-8053-46b3-b42b-8a601cdb920c', obligations_require_negotiated_consent, deontological).
narrative_ontology:cs_reference_frame('0b07d315-8053-46b3-b42b-8a601cdb920c', strict_treaty_delegation).
narrative_ontology:cs_drift_state('0b07d315-8053-46b3-b42b-8a601cdb920c', post_appellate_body_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0b07d315-8053-46b3-b42b-8a601cdb920c', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, winning_exporting_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, appellate_body_members).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_legal_establishment).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, export_oriented_industries).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, policy_autonomy_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_country_members).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, domestic_regulators).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, living_treaty_interpretation_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, negative_consensus_adoption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A small body of jurists serving fixed terms hears appeals from panel reports and authors the interpretive rulings that determine what the trade treaties require. Each ruling narrows or widens what members must do, and the body's prestige and jurisdiction grow with each doctrinal move it makes. Its members' professional standing is inseparable from the institution they staff; stepping off the interpretive path would mean repudiating their own prior work.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, appellate_body_members, agenda_setter,
    institutional, biographical, identity_locked, global).

% Major trading members whose domestic measures — environmental rules, health standards, industrial policies — are challenged and struck down in Geneva. When they lose, they must withdraw measures their legislatures enacted or absorb authorized retaliation against their exports. They hold a consensus veto over institutional appointments and rule adoption, and have used it: blocking appellate appointments until the appeals bench emptied, declining to comply with rulings they read as unauthorized, and steering reform negotiations. Formally sovereign equals of every other member, they alone can bring the machinery to a halt.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, policy_autonomy_member_states, payer,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__judicial_activism_reading, policy_autonomy_member_states, agenda_setter).

% Smaller and poorer members that ratified the treaties for predictable access to rich markets. They lack the legal budgets to litigate complex disputes, cannot absorb retaliation aimed at their few export lines, and depend on the system's market access enough that walking out is not survivable. When interpretive rulings expand obligations, they comply or swallow retaliation; several have joined coalitions to demand the interpretive reach be rolled back.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_country_members, payer,
    powerless, generational, trapped, global).

% National agencies and ministries whose rules become the subject matter of disputes. They draft measures under domestic mandates, then watch adjudicators in Geneva decide whether those measures survive. Redrafting to satisfy a ruling consumes budget and political capital they did not plan for, and the standards they must meet arrive as case outcomes rather than as texts their legislature voted on.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, domestic_regulators, payer,
    moderate, biographical, constrained, national).

% Members that initiate disputes and win them, collecting removal of barriers to their exports and, where compliance lags, authorization to retaliate. They alternate between plaintiff and defender roles across disputes, so gains and losses balance unevenly over time, but the net flow of enlarged market access runs to the export-strong. They can pursue bilateral deals or other forums if the system stops delivering.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, winning_exporting_states, beneficiary,
    powerful, biographical, mobile, global).

% The litigators, secretariat lawyers, and academics who staff, argue, and teach the system. Careers, consultancies, and scholarly authority are built on the growing body of case law; each interpretive expansion enlarges the specialty. Their professional identity is fused with the enterprise — treating the case law as ordinary legislation to be curbed would devalue their accumulated expertise.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_legal_establishment, beneficiary,
    organized, biographical, identity_locked, global).

% Firms and industry associations that finance litigation and press for expansive readings that open foreign markets to their goods. They collect enforceable access without bearing the institutional burdens, and can redirect supply chains or shop for friendlier forums if a particular market's rules turn hostile.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, export_oriented_industries, beneficiary,
    organized, biographical, mobile, global).

% Academic and think-tank analysts who track whether the dispute system's exercised authority tracks its negotiated mandate. They publish legitimacy audits, testify to legislatures, and hold no operational stake in outcomes; their seat is observational.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, institutional_legitimacy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, winning_exporting_states).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts trade conflicts among more than 160 members into a standardized legal procedure with published reasoning, replacing ad hoc bilateral retaliation with a shared process for airing and settling market-access grievances.
% TRANSFER_FUNCTION: Moves regulatory autonomy and market access: losing members withdraw domestic measures or accept retaliation against their exports; winning members collect enlarged access; interpretive rulings move policy-setting authority from national legislatures to adjudicative bodies without any new treaty text being ratified.
% ABSENT_VOICES: Domestic constituencies whose protections are traded away — consumers, environmental and public-health advocates, import-competing workers — have no seat in Geneva; the parliaments that ratified the treaties never vote on the interpretive extensions; smaller members were historically absent from the appellate bench that writes the rulings binding them.
% DISAPPEARANCE_RATIONALE: Export-strong members would revert to bilateral pressure and unilateral retaliation; import-defending members would lose a venue that sometimes shields their measures; the trade-law profession, the funded litigation pipeline, and the interim arbitration arrangements built on the system would dissolve or migrate; tariff schedules negotiated against the backdrop of enforceable dispute settlement would be renegotiated under raw power asymmetries.
% FOUNDING_PROBLEM: Under the predecessor GATT procedure, any member could veto adoption of a ruling against it, so findings died quietly and grievances escalated into retaliatory tariff spirals; the 1995 reform created automatic adoption of rulings and authorized retaliation to make findings stick.
% FOUNDING_PROBLEM_CORROBORATION: Trade-history scholarship and contemporaneous negotiating records corroborate the veto-paralysis problem; legislative testimony, audit-office reports, and developing-country reform submissions from outside the benefiting seats attest both that escalation risk persists and that the cure's current scope is disputed — no attestation comes only from the seats that collect from the arrangement.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the obligations members actually perform include components traceable to interpretive choice rather than negotiated text, and the price of refusal is authorized retaliation. Suppression (0.55) is moderate-high but decaying: automatic adoption removed blocking rights and retaliation authorization coerces, yet the appellate enforcement capacity that hardened the machinery was dismantled mid-interval by appointment blockade, and major members now defy findings openly — hence the suppression_requirement series rises to a mid-interval peak (0.63) and falls to 0.55, modeling enforcement-capacity buildup and subsequent decay rather than a static picture. Theater ratio climbs from 0.14 to 0.30: the panel process still performs neutrality and consensus while an increasing share of its output consists of ritualized procedure whose bindingness is contested by the very members it binds. Accessibility collapse is moderate (0.40) because exits persist — bilateral deals, plurilateral arbitration among the willing, plain non-compliance — but the system did monopolize trade dispute settlement for most of the interval. Resistance is high (0.70), the signature of this reading: appointment blockade, open non-compliance, and reform demands are the arrangement meeting organized refusal. All three tracked series share one six-point grid so temporal analysis samples every metric at every examined time.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the appellate seat the arrangement is a jurisprudence it built and staffs; from the trapped developing-member seat it is obligations arriving without consent; from the veto-wielding member's seat it is a system it pays into when it loses and administers when it chooses. Among formally equal members — the same nominal power atom — exit options diverge sharply: arbitrage-grade veto leverage for the largest aggrieved member, bare trapped dependence for the smallest. The engine computes per-seat classifications from this structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Winning exporting states sit near the beneficiary pole: they collect compliance and access, and their mobility keeps them there. The appellate body sits nearest the subsidy end — the arrangement's operation is the accumulation of its own authority. The legal establishment and export industries collect without administering. Policy-autonomy member states are declared victims, but their arbitrage-grade exit pulls their derived directionality back toward symmetry, which matches their lived alternation between plaintiff and defender; trapped developing members and constrained domestic regulators derive near-full-target directionality, which matches who actually swallows the expanded obligations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — veto-blocked rulings and retaliatory spirals — was real and is corroborated from outside the benefiting seats; the arrangement solved it and then kept the machinery running past its negotiated warrant. Classifying as tangled_rope keeps both faces visible: reading the arrangement as pure extraction (snare) erases the genuine coordination that prevents trade wars, while reading it as pure coordination (rope) erases the asymmetric transfer that rides on the same enforcement. The founding problem remains live, so the mismatch consumer finds no zombie flag — the arrangement still performs its founding function while carrying extracted layers on top of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the wto_dsb_authority kernel correctly characterizes the dispute system — this story''s judicial_activism_reading, the binding_referee_reading, or the advisory_coordination_reading?',
    'Comparative analysis across the three sibling stories plus a doctrinal audit of panel and appellate reasoning against treaty text: systematic gaps between holdings and negotiated language support this reading; close tracking supports the binding_referee sibling.',
    'If the binding_referee sibling is correct, this story''s epsilon collapses toward coordination cost and the computed type moves toward rope; if the advisory sibling is correct, the binding apparatus itself is the deviation and the victim set expands to every member subject to adopted rulings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: this constraint is one reading of a three-way-contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    mandate_boundary_locatability,
    'Is there a determinate boundary between interpreting negotiated obligations and creating new ones, such that the charge of exceeding mandate has a stable target?',
    'Doctrinal comparison of contested holdings against uncontested ones, and analysis of which interpretive moves members formally objected to versus accepted; a principled line locatable in members'' own reactions would stabilize the boundary.',
    'If no determinate boundary exists, part of the measured extraction is the unavoidable cost of any adjudication and epsilon falls; if a line exists and is crossed systematically, epsilon stands or rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_boundary_locatability, conceptual, 'Whether interpretation-versus-legislation has a locatable line.').

omega_variable(
    appellate_restoration_trajectory,
    'Will appellate enforcement capacity be restored, or will enforcement fragment permanently into plurilateral arbitration among willing members?',
    'Track appellate appointment fill, reform-negotiation outcomes, and membership growth of the interim arbitration arrangement over the next negotiation cycle.',
    'Restoration re-hardens suppression and recentralizes extraction; permanent fragmentation decays the arrangement toward vestige for non-participants while keeping it fully operative for participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_restoration_trajectory, empirical, 'Enforcement-capacity recovery versus permanent fragmentation.').

omega_variable(
    resistance_representation_scope,
    'Does the observed resistance — appointment blockade, open non-compliance, reform demands — represent broad member rejection of the arrangement''s authority, or a narrow pursuit of leverage by the largest aggrieved member?',
    'Count co-sponsorship of reform proposals, compliance behavior across mid-sized members, and committee voting patterns; breadth of independent replication distinguishes systemic delegitimation from bilateral leverage-seeking.',
    'If resistance is systemic, the suppression series understates fragility and decay accelerates; if it is narrow, the arrangement retains working legitimacy among the majority and the measured suppression is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_representation_scope, empirical, 'Whether resistance is systemic delegitimation or single-actor leverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_tr_t0, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t6, wto_dsb_authority__judicial_activism_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_tr_t6, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t12, wto_dsb_authority__judicial_activism_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_tr_t12, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t18, wto_dsb_authority__judicial_activism_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_tr_t18, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t24, wto_dsb_authority__judicial_activism_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_tr_t24, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_tr_t30, wto_dsb_authority__judicial_activism_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_be_t0, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t6, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_be_t6, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t12, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_be_t12, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t18, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 18, 0.65).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_be_t18, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t24, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_be_t24, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_be_t30, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_su_t0, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t6, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 6, 0.53).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_su_t6, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t12, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 12, 0.59).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_su_t12, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t18, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 18, 0.63).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_su_t18, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t24, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_su_t24, observed).
narrative_ontology:measurement(wto_dsb_judicial_activism_su_t30, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(wto_dsb_judicial_activism_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the wto_dsb_authority kernel: the colloquial label 'WTO dispute settlement authority' covers three structurally distinct claims, written as three stories. This file authors only the judicial_activism_reading; its epsilon is indexed to that reading's account of the standing arrangement and is not comparable by averaging with the siblings. Direction of influence: the binding_referee reading supplies the legitimacy premises this reading attacks; the advisory_coordination reading absorbs this reading's critique as reform pressure toward softer, facilitation-centered settlement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
