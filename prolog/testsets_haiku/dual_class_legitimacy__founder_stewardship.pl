% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Share Structure: Founder Stewardship Reading
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   This constraint models the founder-stewardship reading of dual-class
 *   voting structures in publicly held corporations. A founder or founding
 *   team holds Class A super-voting shares (e.g., 10 votes per share) while
 *   minority shareholders hold Class B ordinary shares (1 vote per share).
 *   The founder uses this control to insulate long-term strategic decisions
 *   from activist pressure, hostile takeovers, and quarterly-earnings
 *   maximization. The reading asserts this is legitimate because: (1) it
 *   solves a real coordination problem (aligning shareholder interests around
 *   decade-spanning missions), (2) minority shareholders consented to the
 *   structure under disclosure, and (3) the founder's identity-fusion with
 *   the mission makes founder stewardship credible. The competing readings of
 *   the same kernel (dual_class_legitimacy) assert different legitimacy
 *   grounds: minority_extraction emphasizes the governance asymmetry and
 *   minority shareholders' lack of proportional control, while
 *   disclosure_consent emphasizes informed consent and transparency. This
 *   story generates ONLY the stewardship reading as a complete, ε-invariant
 *   constraint; the sibling readings are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - founder_principal_shareholders: Institutional power, identity-locked exit, agenda-setting authority. Benefit from control preservation and long-term mission execution. Frame dual-class as coordination.
 *   - class_b_minority_shareholders: Moderate power, mobile exit (can sell shares), subordinate voting. Bear governance asymmetry cost, benefit from founder's mission focus. Accept structure under disclosure.
 *   - securities_regulators: Institutional observer role. Enforce disclosure requirements, permit dual-class under informed consent doctrine.
 *   - corporate_governance_reformers: Moderate power, excluded from governance. Advocate for one-share-one-vote; frame dual-class as unjust founder entrenchment.
 *   - employees_and_stakeholders: Powerless, constrained exit. Benefit from mission-aligned decisions; bear risk if founder misdecision occurs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.42).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.31).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Share Structure: Founder Stewardship Reading").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, 'e6124e1b-0595-4ae9-a55b-c86e66afcef3').
narrative_ontology:cs_kernel_codification('e6124e1b-0595-4ae9-a55b-c86e66afcef3', formalized).
narrative_ontology:cs_authority_grounding('e6124e1b-0595-4ae9-a55b-c86e66afcef3', lineage).
narrative_ontology:cs_interpretation_layer_present('e6124e1b-0595-4ae9-a55b-c86e66afcef3').
narrative_ontology:cs_reading_relation('e6124e1b-0595-4ae9-a55b-c86e66afcef3', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('e6124e1b-0595-4ae9-a55b-c86e66afcef3', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('e6124e1b-0595-4ae9-a55b-c86e66afcef3', foundational, founder_control_enables_long_horizon_mission).
narrative_ontology:cs_axiom_status(founder_control_enables_long_horizon_mission, holdable).
narrative_ontology:cs_axiom_grounding('e6124e1b-0595-4ae9-a55b-c86e66afcef3', founder_control_enables_long_horizon_mission, empirically_contingent).
narrative_ontology:cs_axiom('e6124e1b-0595-4ae9-a55b-c86e66afcef3', foundational, founder_identity_alignment_reduces_agency_costs).
narrative_ontology:cs_axiom_status(founder_identity_alignment_reduces_agency_costs, holdable).
narrative_ontology:cs_axiom_grounding('e6124e1b-0595-4ae9-a55b-c86e66afcef3', founder_identity_alignment_reduces_agency_costs, instrumental).
narrative_ontology:cs_reference_frame('e6124e1b-0595-4ae9-a55b-c86e66afcef3', founder_mission_stewardship_authority).
narrative_ontology:cs_drift_state('e6124e1b-0595-4ae9-a55b-c86e66afcef3', contemporary_activist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e6124e1b-0595-4ae9-a55b-c86e66afcef3', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_principal_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, all_shareholders_via_mission_success).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, class_b_minority_shareholders).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, employees_and_stakeholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__founder_stewardship, class_b_minority_shareholders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold super-voting Class A shares (e.g., 10 votes per share) that grant disproportionate control relative to capital contribution. Set strategy, make long-term investment decisions (R&D, acquisitions, capital allocation), and retain veto power over fundamental changes (board composition, charter amendments, hostile takeovers). Their personal identity and life meaning are intertwined with the organization's mission; stepping away would mean abandoning not just a job but the realization of a decades-long vision. They justify the control as necessary to execute multi-year or multi-decade strategies that would be sabotaged by quarterly-earnings pressure or activist raiders. Their public rhetoric emphasizes founder stewardship and mission alignment; they maintain that all shareholders benefit from insulation from short-term market noise.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, founder_principal_shareholders, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, founder_principal_shareholders, beneficiary).

% Hold ordinary voting Class B shares (1 vote per share) proportional to capital invested. Cannot outvote the founder on any governance matter and have no veto over strategy. They receive dividends if declared and capital appreciation if the business succeeds; they can sell shares at any time on the public market (liquid exit). Under the stewardship reading, they benefit from the founder's long-term focus and are protected from activist short-termism. They consented to the governance structure when they purchased the shares, choosing to invest in the founder's mission at the price of reduced governance control. Under the minority-extraction reading, they pay governance costs (inability to veto founder decisions) without compensation and are trapped if they want to maintain their investment.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, class_b_minority_shareholders, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__founder_stewardship, class_b_minority_shareholders, beneficiary).

% Mutual funds, institutional investors, hedge funds, and traders who set market prices and allocate capital. They price the Class B shares at a discount that reflects the governance restrictions (dual-class discount or premium, depending on founder reputation and track record). They can choose to invest or not invest; they can long the stock, short it, or remain neutral. They treat the dual-class structure as one factor in a valuation matrix, neither fundamentally illegitimate nor transparently fair — it is a bundle of rights and restrictions to be priced.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, capital_market_participants, observer,
    organized, biographical, arbitrage, global).

% Enforce Securities Act disclosure requirements and maintain proxy rules that govern voting and shareholder communication. They do not mandate one-share-one-vote in most jurisdictions (notable exception: California proposed bans in the 2010s, struck down); instead, they rely on the disclosure-consent model — if shareholders are informed of the voting structure and consent to it by purchasing shares, the structure is legitimate. They monitor for fraud, conflicts of interest, and inadequate disclosure but largely stay neutral on governance structure questions.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, securities_regulators, observer,
    institutional, generational, analytical, national).

% Shareholder activists, governance advocates, institutional investors with voting principles, and academics who argue that dual-class structures are unjust and antidemocratic. They believe all shareholders are entitled to voting power proportional to their capital at risk. They advance proposals for governance reforms (sunset provisions that force recapitalization, one-share-one-vote mandate, board independence), file litigation challenging the structures, and lobby regulators and legislatures to ban dual-class voting. They are excluded from governance decisions in founder-controlled companies and have limited ability to enforce their vision within existing organizations; instead, they operate through external pressure (activism, litigation, regulation). Under the minority-extraction reading, their voice represents oppressed minority shareholders.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, corporate_governance_reformers, excluded,
    moderate, biographical, constrained, global).

% Employees, contractors, suppliers, and communities that depend on the organization's long-term viability and mission success. They benefit from founder stewardship because a founder insulated from quarterly pressure can invest in employee development, long-term R&D, supply chain stability, and community contribution. They bear the risk that the founder's decisions are wrong (strategic failures, mission drift, founder misconduct) and have no formal governance recourse. Exit is possible (find another job, find another supplier relationship) but carries costs (opportunity loss, relocation, relationship reset).
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__founder_stewardship, employees_and_stakeholders, beneficiary,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dual_class_legitimacy__founder_stewardship, founder_principal_shareholders).
narrative_ontology:fixing_cost_class(dual_class_legitimacy__founder_stewardship, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns all shareholders and stakeholders around a stable long-term mission, insulating strategy from short-term activist pressure, quarterly-earnings maximization, and hostile takeover threats. Enables multi-year R&D investment, supply-chain optimization, and cultural investment that would be undermined by constant activist oversight. Coordinates founder and shareholders on the premise that founder stewardship produces superior long-horizon value for all stakeholders.
% TRANSFER_FUNCTION: Transfers governance authority from capital-weighted voting to mission-weighted control. Class B shareholders trade governance veto power and board representation in exchange for founder's commitment to long-horizon value creation and mission fidelity. Founder retains decision-making authority on strategy, capital allocation, executive team, and fundamental changes. Value flows from minority shareholders' governance contribution to founder's insulation from short-term pressure.
% ABSENT_VOICES: Future generations of shareholders (post-founder) who did not sign up for the original founder's particular mission; employees whose career stakes ride on founder competence but who have no governance input; minority shareholders who acquired Class B shares on the secondary market years after the dual-class structure was locked in and cannot unwind it except by selling; potential alternative founders or professional managers whose approaches would be systematically rejected by the incumbent founder.
% DISAPPEARANCE_RATIONALE: If the dual-class structure and founder veto rights were abolished overnight, the organization would face immediate activist campaigns to redirect cash to buybacks, dividends, or stock buybacks rather than R&D; potential hostile bidders would emerge; strategy could swing quarter-to-quarter based on activist pressure; many founder-led organizations would be forced to hire activist-friendly boards and abandon long-horizon missions. The long-term innovation and stakeholder-investment profile of founder-led organizations would shift toward short-term profit maximization.
% FOUNDING_PROBLEM: Early-stage founders who raise capital to scale their vision face a dilemma: self-fund (limited scale, slow growth) or raise from public capital markets (cede control to shareholders and activists, constant pressure to maximize quarterly earnings at the expense of long-term mission). Without the ability to maintain control, founders cannot execute strategies that span decades (e.g., building a global social network, developing fundamental AI research, creating an ecosystem of long-term stakeholder trust). The dual-class structure solves this by allowing founders to raise capital without surrendering mission control.
% FOUNDING_PROBLEM_CORROBORATION: Founders and long-term institutional investors consistently attest that the problem persists: activist pressure for short-term earnings is endemic in public markets, hostile takeovers remain a threat, and quarterly guidance creates perverse incentives for executives. Empirical analysis of founder-led companies shows higher R&D spending, higher employee retention, longer product development cycles, and greater stakeholder investment compared to activist-controlled peers (controlling for industry and stage). Securities regulators and courts acknowledge the trade-off and permit dual-class structures under disclosure regimes. Governance reformers dispute whether the problem justifies the solution, but do not deny the founding problem exists.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__founder_stewardship, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__founder_stewardship, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__founder_stewardship, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 (moderate): the founder extracts control asymmetry (not proportional to capital contributed), but the control serves a real coordination function (long-term strategy alignment) that benefits all shareholders. This reading does not claim zero extraction — it claims the extraction is justified by coordination value. Suppression is low (0.31) because the structure operates under disclosure and is market-priced; minority shareholders can exit by selling (mobile exit_options) or accept the terms as part of the bundle. Theater is low-moderate (0.28): some performative maintenance exists (founder rhetoric about stewardship, governance theater about mission alignment), but most of the measured enforcement is genuine coordination problem-solving. Accessibility_collapse is moderate (0.48): alternatives exist (don't buy the stock, short the stock, wait for founder transition, invest in founder-free companies), but the structure is codified in the charter and cannot be unilaterally undone by minority vote. Resistance is substantial (0.64): corporate governance reformers, institutional investors unhappy with control structures, and some minority shareholders actively resist the arrangement through shareholder proposals, litigation, and regulatory advocacy. The measurement series tracks extractiveness drifting upward slightly (founder control becomes more entrenched as founder ages and organization scales, making it harder to dislodge); suppression follows similar trajectory (enforcement to maintain voting restrictions becomes more explicit as minority challenges mount). Theater remains low and stable — the coordination claim is not wearing thin, though it is contested.
 *
 * PERSPECTIVAL GAP:
 *   The founder and long-term mission investors (beneficiary seats) perceive this constraint as genuine rope: solving a real coordination problem, enabling multi-decade strategies that create massive shareholder value, and operating fairly because all parties consented under disclosure. The minority shareholder seat (symmetric to slightly targetted) perceives it as partially extractive: they accepted the governance reduction because they believed in the founder, but they experience ongoing cost (no veto on strategy pivots, no recourse if founder priorities diverge from shareholder interests). The governance-reform seat (excluded) perceives it as snare: an illegitimate entrenchment dressed up as coordination, a cover for founder self-dealing, and a permanent fixture that cannot be dislodged even if the founding problem (multi-decade isolation from markets) no longer applies. The engine computes each seat's classification from these structural positions; the author's claim (rope) reflects the beneficiary-seat framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder principal shareholders sit at d≈0.15 (strong beneficiary end): they collect the control asymmetry and their identity is fused with the mission, making their exit cost extremely high. This combination gives them beneficiary directionality despite bearing some costs (founder decisions can be wrong, personal reputation tied to company outcomes). Class B minority shareholders sit at d≈0.55 (approximately symmetric): they bear governance costs (inability to outvote founder on fundamental decisions) but capture mission-alignment benefits (founder makes decisions favoring long-term value over quarterly earnings; founder has skin in the game via share holdings). Employees and stakeholders sit at d≈0.60 (moderate target direction): they benefit from mission alignment but have no governance recourse and bear full downside risk if founder errs. Securities regulators sit at d=0.5 (analytical observer) — they neither benefit nor bear costs directly; they adjudicate disclosure adequacy. The engine derives these directionality values from beneficiary/victim declarations and exit option asymmetries; no override is needed because the structural data grounds the derivation cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits live founding problem (founder-founders need to execute long-term missions without activist interference) and active functional coordination (dual-class structure genuinely solves the coordination problem by insulating long-term decisions). Therefore, mandatrophy is NOT present — the arrangement is not a zombie pursuing a dead problem. However, the founding_problem_status is contested because some market participants argue the founding problem is substantially solved by now: the markets have evolved to support long-term investing (index funds, ESG criteria favoring long-term value), activist pressure has professionalized (activist funds are not purely extractive), and many successful founder-led companies have transitioned to professional management without loss of mission. The live-vs-contested split means the constraint functions as real coordination for those who believe the founding problem persists, but looks like rent-seeking entrenchment to those who believe it is solved. This reading (stewardship) emphasizes the live side; the minority-extraction reading emphasizes the solved side.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_competence_and_value_alignment,
    'Does founder control actually produce superior long-term value outcomes for all shareholders, or does it primarily entrench founder preferences that may diverge from shareholders'' interests over time?',
    'Multi-decade comparative analysis of founder-led dual-class companies vs. professional-management companies with governance parity: measure total shareholder return, capital efficiency, innovation output, and founder-driven strategic pivots that destroy vs. create value. Control for selection effects (founders self-select into dual-class structures; successful founders raise money more easily).',
    'If founder control produces materially superior outcomes on average, the coordination function is real and extractiveness is overstated by the control asymmetry alone. If outcomes are indistinguishable or worse, the structure is primarily extractive and the mission-alignment claim is founder self-dealing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_competence_and_value_alignment, empirical, 'Whether founder stewardship produces superior value outcomes or merely entrenchment.').

omega_variable(
    reading_vs_minority_extraction_foreclosure,
    'Does the founder-stewardship reading logically foreclose the minority-extraction reading, or do they coexist as competing interpretations of the same structure?',
    'Examine the axioms: stewardship reading asserts ''founder control is coordination''; minority reading asserts ''minority is entitled to governance proportional to capital risk''. These are not logical contradictions — a structure can simultaneously coordinate long-term strategy AND extract governance asymmetry from minority holders. The readings coexist when a reasonable observer can hold both premises: ''the structure serves a coordination function AND the minority bears uncompensated governance costs.'' Foreclosure would require one premise to make the other incoherent (does not obtain here).',
    'If readings coexist, both constraints belong in the family and the engine computes separate per-seat classifications; if one forecloses the other, only one constraint is real and the sibling reading is a misframing. The manifests in the reading_relations choice: coexists_with vs. forecloses determines whether the constraint family is two readings of one kernel or a conflict between incommensurable readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_minority_extraction_foreclosure, conceptual, 'Whether the founder-stewardship and minority-extraction readings are logically compatible or mutually exclusive.').

omega_variable(
    identity_lock_as_exit_suppression,
    'Is the founder''s identity_locked exit option a genuine structural fact (founder identity fused with organization) or a rhetorical device that suppresses the founder''s own exit option as a way to assert commitment?',
    'Examine founder behavior in practice: when founders leave organizations or lose control (forced out, term limits, bankruptcy), do they experience the loss as identity dissolution or as a career transition? Do founders who step down from founder-control positions report identity recovery afterward? Do founders deliberately cultivate identity-lock language to forestall activist challenges?',
    'If identity-lock is genuine, the founder''s exit is structurally trapped and the control arrangement is partially coordination (founder cannot exit easily). If identity-lock is performed, the founder retains hidden arbitrage—they can walk away while claiming they cannot, which overstates the structural interdependence. This affects whether the arrangement is genuinely coordinating all parties equally or selectively suppressing exits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_as_exit_suppression, empirical, 'Whether founder identity-lock is structural commitment or performative entrenchment.').

omega_variable(
    disclosure_consent_sufficiency_under_this_reading,
    'Does the founder-stewardship reading depend on the disclosure-consent reading''s legitimacy claim, or are they independent?',
    'The founder-stewardship reading asserts legitimacy based on mission alignment and long-term value creation (coordination narrative). The disclosure-consent reading asserts legitimacy based on informed choice and transparency (consent narrative). If the stewardship reading REQUIRES disclosure-consent to be valid (i.e., stewardship is legitimate ONLY if shareholders consented with full information), then the two readings are coupled. If stewardship can stand on its own merits independent of how informed the consent was, they are independent readings.',
    'If coupled, disclosure failures undermine the founder-stewardship reading; if independent, the readings coexist even when disclosure is inadequate. The relation affects how to model the kernel''s internal structure: does one reading presuppose the other, or do they offer alternative legitimacy grounds?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_consent_sufficiency_under_this_reading, conceptual, 'Whether founder-stewardship legitimacy presupposes or is independent of disclosure-consent legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.18).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.22).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.25).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__founder_stewardship, theater_ratio, 25, 0.27).
narrative_ontology:measurement(dual_tr_t35, dual_class_legitimacy__founder_stewardship, theater_ratio, 35, 0.28).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__founder_stewardship, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(dual_be_t35, dual_class_legitimacy__founder_stewardship, base_extractiveness, 35, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.21).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__founder_stewardship, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(dual_su_t35, dual_class_legitimacy__founder_stewardship, suppression_requirement, 35, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% The dual_class_legitimacy kernel decomposes into three structurally distinct constraints, one per legitimacy reading. This file (founder_stewardship) asserts the legitimate basis is long-term coordination and founder stewardship aligned with mission success. The minority_extraction reading asserts the legitimate basis is governance proportional to capital risk. The disclosure_consent reading asserts the legitimate basis is informed consent and transparency. Each reading has distinct ε, distinct stakeholder asymmetries, distinct beneficiary/victim structure, and distinct classification. They are linked because they share the same kernel (the dual-class voting structure) but instantiate different constraint logics depending on which legitimacy ground is accepted. Sibling relationships: stewardship coexists_with both minority and disclosure readings; no reading forecloses another (all three remain live positions in actual markets). See cs_structure.reading_relations for formal declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
