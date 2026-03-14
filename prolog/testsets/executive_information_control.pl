% ============================================================================
% CONSTRAINT STORY: executive_information_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_executive_information_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: executive_information_control
 *   human_readable: Executive Information Control in Organizations
 *   domain: organizational_governance/political_economy
 *
 * SUMMARY:
 *   Executive information control is a ubiquitous structural constraint in
 *   large organizations. It operates through formal access restrictions (data
 *   classification, compartmentalization, hierarchical approval gatekeeping),
 *   informal cultural norms ('need to know' ideology, deference to executive
 *   judgment), and legal mechanisms (employment contracts, NDAs,
 *   whistleblower retaliation statutes). The constraint exhibits all
 *   characteristics of a tangled rope: genuine coordination function
 *   (directing information to decision-makers) layered with asymmetric
 *   extraction (executives monopolize material facts to prevent challenges to
 *   their authority). Information asymmetry increases extractiveness (0.42 →
 *   0.58) and theater ratio (0.48 → 0.64) over 20-year interval, indicating
 *   that the constraint accumulates extractive overhead while maintaining its
 *   coordination appearance. Different stakeholders perceive radically
 *   different constraint types: executives see pure coordination (Rope),
 *   trapped employees see pure extraction (Snare), the board experiences
 *   mixed dynamics (Tangled Rope), and organized actors building transparency
 *   mechanisms see a temporary problem with a sunset (Scaffold). The
 *   constraint's false mountain perspective (analytical/civilizational)
 *   reveals how 'organizational necessity' narratives naturalize contingent
 *   institutional designs.
 *
 * KEY AGENTS:
 *   - Executive Leadership: Primary beneficiary (institutional/arbitrage) — controls information flow, captures decision-making authority, can move laterally across organizations
 *   - Rank-and-File Employees: Primary victim (powerless/trapped) — denied information needed to assess job security, career trajectory, organizational direction; locked into employment dependency
 *   - Middle Management: Secondary victim and partial beneficiary (moderate/constrained) — must coordinate without sufficient information; benefits through job security and insider status relative to frontline; constrained by information gatekeepers
 *   - Board of Directors: Nominal supervisors (powerful/constrained) — depend on executives for material information; information asymmetry enables executives to control board decisions; constrained exit requiring proof of executive failure
 *   - Compliance and Audit Functions: Institutional oversight (institutional/arbitrage) — maintain formal controls that lack real enforcement authority; function degraded through executive control of audit scope
 *   - Transparency Coalition: Organized challengers (organized/mobile) — SEC enforcement, investigative journalists, shareholder litigation, regulatory bodies building alternative information pathways with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent information architecture as immutable organizational law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(executive_information_control, 0.58).
domain_priors:suppression_score(executive_information_control, 0.68).
domain_priors:theater_ratio(executive_information_control, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(executive_information_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(executive_information_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(executive_information_control, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(executive_information_control, tangled_rope).
narrative_ontology:human_readable(executive_information_control, "Executive Information Control in Organizations").
narrative_ontology:topic_domain(executive_information_control, "organizational_governance/political_economy").

domain_priors:requires_active_enforcement(executive_information_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(executive_information_control, executive_leadership).
narrative_ontology:constraint_beneficiary(executive_information_control, board_gatekeepers).
narrative_ontology:constraint_victim(executive_information_control, rank_and_file_employees).
narrative_ontology:constraint_victim(executive_information_control, organizational_accountability).
narrative_ontology:constraint_victim(executive_information_control, strategic_decision_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE EMPLOYEE (SNARE) — Structurally locked into information asymmetry. Cannot access strategic data needed to evaluate organizational direction, assess personal career risk, or exit intelligently. Information barriers are reinforced by employment dependency, NDA agreements, and hierarchical norms. High suppression through legal contract and cultural enforcement. No genuine exit option — changing jobs means repeating the same informational lock in another firm.
constraint_indexing:constraint_classification(executive_information_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGEMENT (TANGLED ROPE) — Partially trapped, partially beneficiary. Must coordinate implementation of executive strategy (genuine coordination function) while being denied information needed to do the job well. Information gaps forced upward as questions; information asymmetry enables control. Benefits through job security and insider status relative to frontline, but bears extraction costs through being blamed for execution failures caused by information starvation. Constrained exit: promotions depend on information gatekeepers' approval.
constraint_indexing:constraint_classification(executive_information_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP (ROPE) — Primary beneficiary. Controls information flow as a coordination mechanism: directing which groups see which data enables resource allocation, strategic focus, and organizational alignment. From the executive view, information control IS the coordination function. Can arbitrage between organizations — executives move laterally at same or higher status, taking information monopolies with them. Low experienced extraction because benefits flow toward this agent.
constraint_indexing:constraint_classification(executive_information_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPLIANCE AND AUDIT (PITON) — Maintains theatrical oversight. Auditors and compliance staff conduct formal reviews of information controls but lack authority to change them or to access information outside executive-approved scope. The function persists through regulatory requirement and institutional inertia but has degraded real control power — executives can and do conceal material facts from auditors. Theater ratio high: formal compliance reviews create appearance of information governance without changing actual control structure.
constraint_indexing:constraint_classification(executive_information_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSPARENCY COALITION (SCAFFOLD) — Organized actors (SEC enforcement, investigative journalists, transparency NGOs, stock exchange listing rules) are building alternative information pathways: mandatory disclosures, insider trading enforcement, shareholder litigation discovery. These mechanisms create a sunset for pure information monopolies — executives can control internal communication but cannot entirely control external revelation of material facts. The coalition has agency and sees exit paths (regulatory enforcement, litigation, public exposure). Suppression declines as mechanisms mature.
constraint_indexing:constraint_classification(executive_information_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: BOARD OF DIRECTORS (TANGLED ROPE) — Nominally oversee executives; actually depend on executives for information. Board members receive curated data, attend executive-controlled meetings, see executive-selected documents. Information asymmetry enables executives to control board decisions while maintaining coordination appearance (board still nominally approves strategy). Board members have constrained exit — removing a CEO requires information proving failure, which executives control. Extraction runs from board to executives, despite board's nominal authority.
constraint_indexing:constraint_classification(executive_information_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry in hierarchies appears immutable: all organizations with scale have more information at the top than the bottom; full transparency is incompatible with organizational agility. This perspective naturalizes information control as inherent to complex organizations. However, the structural data contradicts mountain classification — measured suppression (0.68), measurable beneficiary/victim gap, and evidence of alternative information models (federated, distributed organizations) reveal this as false natural law. The constraint is contingent institutional design, not immutable structure.
constraint_indexing:constraint_classification(executive_information_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(executive_information_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(executive_information_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(executive_information_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(executive_information_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(executive_information_control, TR),
    TR >= 0.70.

:- end_tests(executive_information_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Executives capture benefits of information monopoly (decision authority, strategic options, prestige from exclusive knowledge) while subordinates bear costs (limited agency, inability to exit intelligently, restricted career options). The trajectory from 0.42 to 0.58 reflects increasing sophistication in information gatekeeping as organizations scale — data abundance paradoxically increases asymmetry through curated access and algorithmic filtering. Not maximum extraction (0.58 not 0.75+) because some information flow is genuine necessity for organizational function, and some organizations have reduced asymmetry through transparency initiatives. Suppression (0.68): High. Multiple mechanisms reinforce information barriers: legal (NDAs, employment contracts), cultural (need-to-know ideology, hierarchy deference), technical (access control systems), and political (executives punish information sharing). However, suppression is not total (0.68 not 0.90+) — whistleblowers exist, regulatory enforcement reaches some facts, alternative information sources partially bypass executive control. Theater ratio (0.64): Moderate-high. Formal governance structures (board meetings, annual disclosures, audit committees) create appearance of information oversight while executives retain actual control. Theater has increased over interval as regulatory requirements have grown — more compliance theater with less substantive change in power distribution. The theater is not maximal (0.64 not 0.85+) because some information does flow despite asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The executive sees a coordination mechanism (Rope) — distributing information to appropriate decision-makers. The trapped employee sees pure extraction (Snare) — information barriers prevent escape. The board sees mixed dynamics (Tangled Rope) — nominal oversight while being controlled. The middle manager sees partial extraction with coordination (Tangled Rope) — must implement without sufficient information. The transparency coalition sees a temporary problem (Scaffold) — regulatory mechanisms are building exit pathways. The compliance function sees degraded ritual (Piton) — formal controls persist without real constraint. The civilizational analytical view sees natural law (Mountain) — information asymmetry inherent to hierarchy. The perspectival gap reveals structural reality: information control is not a unified phenomenon but a bundle of conflicting interests that different actors resolve through different classifications. The gap between executive's rope and employee's snare is the fundamental structural feature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim status and exit options. Executives (institutional/arbitrage) occupy the beneficiary position with high exit mobility — they can move to competitor firms, take information monopolies with them, experience negative or zero effective extraction (low d). Trapped employees (powerless/trapped) occupy the victim position with no exit — they bear maximum extraction, high d. Middle managers (moderate/constrained) occupy mixed position: partial victim (constrained by information starvation) and partial beneficiary (insider status relative to frontline), with constrained (not mobile/trapped) exit — moderate d. The board (powerful/constrained) has nominal authority but information dependence — the constraint's peculiarity is that structural power (board authority) is negated by information asymmetry, creating a constrained-exit situation despite powerful nominal status. This demonstrates that agent_power is constraint-relative: nominally powerful actors can occupy victim positions if the constraint controls their information.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_necessity,
    'How much information asymmetry is genuinely necessary for organizational coordination, and how much is extractive rent-seeking?',
    'Comparative analysis of organizations with different information architectures (flat vs hierarchical, transparent vs closed); correlation between information asymmetry levels and organizational performance/innovation metrics; measurement of decision quality at different information access levels',
    'If necessary asymmetry is low (< 0.20): information control is primarily extractive (snare classification strengthens). If necessary asymmetry is high (> 0.50): more constraints reclassify as rope (pure coordination). Current estimate: ~0.30 necessary, remainder is extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_necessity, empirical, 'Threshold between necessary information asymmetry and extractive control').

omega_variable(
    exit_option_alternative_models,
    'Do alternative organizational models (cooperative ownership, federated governance, open-source hierarchies) actually reduce information control or merely shift it?',
    'Longitudinal study of organizations that transitioned from hierarchical to federated/distributed information models; measurement of suppression, theater ratio, and extraction before/after transition; tracking of power concentration in ''alternative'' models',
    'If alternatives genuinely lower suppression: exit option for trapped agents improves (mobile becomes viable option). If alternatives merely shift control: exit options remain constrained, and information control is more deeply structural than organizational form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_alternative_models, empirical, 'Whether alternative models provide genuine exit from information control').

omega_variable(
    regulatory_sunset_credibility,
    'Are regulatory mechanisms (SEC disclosure rules, insider trading enforcement, shareholders'' derivative suits) actually closing the executive information monopoly or creating symbolic compliance theater?',
    'Analysis of enforcement action rates and outcomes; comparison of material fact concealment in regulated vs unregulated information flows; measurement of information asymmetry before/after regulatory tightening; tracking of executive compensation correlation with regulatory changes',
    'If mechanisms are effective: scaffold perspective is valid, suppression should be declining over time. If mechanisms are theater: suppression persists despite regulatory apparatus, and scaffold classification is aspirational rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_sunset_credibility, empirical, 'Whether regulatory mechanisms provide real constraint on executive information control').

omega_variable(
    subordinate_identity_lock_extent,
    'To what degree do employees internalize information asymmetry as legitimate hierarchy (identity-locked exit option) vs perceiving it as external barrier (constrained/trapped exit)?',
    'Survey-based measurement of employee belief in ''need to know'' legitimacy; correlation between identity-lock intensity and measured suppression of information requests; longitudinal tracking of exit framing after organizational transparency initiatives',
    'If identity lock is high: suppression metric undercounts true constraint (agents self-suppress). If identity lock is low: measured suppression is closer to structural reality. This affects directionality computation for powerless/moderate perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordinate_identity_lock_extent, empirical, 'Extent of identity-lock in employee perception of information control').

omega_variable(
    board_capture_mechanism,
    'Is board information dependence on executives a necessary coordination feature or an extractive capture mechanism?',
    'Analysis of boards that hire independent information officers, mandate third-party audits, or create independent investigative committees; measurement of board-executive conflict rates and board override frequency before/after information independence measures; correlation between board information access and CEO removal rates',
    'If boards can improve information independence without harming coordination: board perspective reclassifies from tangled rope to rope/scaffold. If information control persists despite independence measures: board capture is deeper (possibly requiring legal/governance reform rather than information architecture change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_capture_mechanism, empirical, 'Whether board information asymmetry is necessary or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(executive_information_control, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exec_info_tr_t0, executive_information_control, theater_ratio, 0, 0.48).
narrative_ontology:measurement(exec_info_tr_t10, executive_information_control, theater_ratio, 10, 0.58).
narrative_ontology:measurement(exec_info_tr_t20, executive_information_control, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(exec_info_be_t0, executive_information_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exec_info_be_t10, executive_information_control, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(exec_info_be_t20, executive_information_control, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(executive_information_control, resource_allocation).
narrative_ontology:affects_constraint(executive_information_control, regulatory_capture).
narrative_ontology:affects_constraint(executive_information_control, board_entrenchment).
narrative_ontology:affects_constraint(executive_information_control, insider_trading_information_asymmetry).

% DUAL FORMULATION NOTE:
% Executive information control decomposes into multiple structurally distinct constraints. The information asymmetry within organizations (this story) has different dynamics than information asymmetry between organizations and regulators (regulatory_capture), or between insiders and outside shareholders (insider_trading). Each has its own ε value and beneficiary/victim structure. They are linked through network effects: executive control of internal information enables regulatory capture; information asymmetry to shareholders enables insider trading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(executive_information_control, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
