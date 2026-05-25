% ============================================================================
% CONSTRAINT STORY: blackstone_smd_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_smd_control, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: blackstone_smd_control
 *   human_readable: Blackstone Senior Managing Director Voting Control
 *   domain: economic/corporate_governance
 *
 * SUMMARY:
 *   Blackstone's dual-class governance structure, formalized during the 2007
 *   IPO, creates an institutional arrangement where Senior Managing Directors
 *   hold absolute voting control over the firm's general partner, while
 *   limited partners and public shareholders hold residual claims but no
 *   governance veto. This constraint exhibits the core mandatrophy: it solves
 *   a genuine coordination problem (enabling long-term strategic capital
 *   deployment without external interference) while simultaneously creating
 *   asymmetric extraction (governance premium accruing disproportionately to
 *   SMDs). The structure was inherited from legacy private equity partnership
 *   models established in the 1980s when firms were small,
 *   founder-controlled, and had no public investors. As Blackstone scaled to
 *   $1 trillion+ in AUM, the governance structure created an increasingly
 *   stark asymmetry: LPs commit multi-year capital but have no strategic
 *   voice; public shareholders own voting units but cannot control the
 *   general partner; professional managers (non-SMDs) execute strategy but
 *   have no voting power. The constraint persists because: (1) it enables SMD
 *   capture of disproportionate economic rents (coordination benefit becomes
 *   extraction), (2) regulatory frameworks have not successfully constrained
 *   it (suppression remains high), and (3) the legitimating
 *   narrative—alignment of interests, long-term thinking—provides theater
 *   that obscures the extraction mechanism.
 *
 * KEY AGENTS:
 *   - Senior Managing Directors: Primary beneficiary (institutional/arbitrage) — absolute voting control, governance premium, ability to set compensation and strategy unilaterally
 *   - Limited Partners: Primary victim (powerless/trapped) — capital locked in for 10-15 years, no governance veto, bear downside risk without upside control
 *   - Public Shareholders: Secondary victim (moderate/constrained) — hold non-voting or subordinate-voting units, trapped in information asymmetry relative to SMDs
 *   - Fund Beneficiaries: Tertiary victim (powerless/trapped) — pension funds, endowments, retail investors whose assets are pooled without governance voice
 *   - Regulatory Agencies (SEC, DOL, state fiduciaries): Organized oversight (organized/constrained) — create coordination rules (disclosure, conflict rules) but have constrained enforcement against entrenched structures
 *   - Governance Reform Coalition: Reform agents (organized/mobile) — institutional investor coalitions, governance advocates pushing for sunset clauses and LP veto rights
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees systemic coordination-extraction hybrid requiring structural decomposition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_smd_control, 0.58).
domain_priors:suppression_score(blackstone_smd_control, 0.72).
domain_priors:theater_ratio(blackstone_smd_control, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_smd_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(blackstone_smd_control, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(blackstone_smd_control, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_smd_control, tangled_rope).
narrative_ontology:human_readable(blackstone_smd_control, "Blackstone Senior Managing Director Voting Control").
narrative_ontology:topic_domain(blackstone_smd_control, "economic/corporate_governance").

domain_priors:requires_active_enforcement(blackstone_smd_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_smd_control, senior_managing_directors).
narrative_ontology:constraint_beneficiary(blackstone_smd_control, blackstone_partnership).
narrative_ontology:constraint_victim(blackstone_smd_control, limited_partners).
narrative_ontology:constraint_victim(blackstone_smd_control, public_shareholders).
narrative_ontology:constraint_victim(blackstone_smd_control, fund_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LIMITED PARTNERS & FUND BENEFICIARIES (SNARE) — Cannot exit the governance structure once capital is committed. Trapped by illiquidity and lock-up periods. SMD voting control over the general partner creates a structural asymmetry: LPs bear downside risk but have no veto over strategic decisions. Extraction runs at maximum intensity.
constraint_indexing:constraint_classification(blackstone_smd_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC SHAREHOLDERS (SNARE) — Hold voting units with no control over the general partner's strategic decisions. Dual-class structure concentrates voting power in SMD hands while public shareholders bear portfolio risk. Exit is theoretically available (sell stock) but constrained by information asymmetries and lock-in effects of strategic importance. SMD control extracts governance premium.
constraint_indexing:constraint_classification(blackstone_smd_control, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SENIOR MANAGING DIRECTORS (ROPE) — Absolute voting control over the general partner enables coordination on firm strategy, capital allocation, and compensation. SMDs experience this as a coordination mechanism: unified control allows long-term strategic thinking without dilution by external investors. Control creates asymmetric benefit but is essential to the operational model.
constraint_indexing:constraint_classification(blackstone_smd_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY & FIDUCIARY OVERSIGHT (TANGLED ROPE) — SEC, DOL (under ERISA), and state fiduciary standards create a coordination function (disclosure, conflict-of-interest rules) but also enforce extraction limits. Regulators have constrained exit (cannot withdraw oversight) but organized power (rule-making authority). The structure exhibits both: genuine coordination requirement (fiduciary duty, transparency) and asymmetric extraction (SMD control subordinates LP interests).
constraint_indexing:constraint_classification(blackstone_smd_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM ADVOCATES (SCAFFOLD) — Organized agents (investor coalitions, governance reformers, legislative bodies) see the SMD control structure as a temporary power imbalance subject to sunset through regulatory or structural change. Potential for modified dual-class sunset clauses, mandatory independent governance committees, or enhanced LP veto rights. Low extractiveness from this perspective because exit pathways exist (regulatory change, alternative fund structures).
constraint_indexing:constraint_classification(blackstone_smd_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY BANKING NORMS (PITON) — The dual-class structure was copied from legacy private equity partnership models (KKR, Carlyle, Apollo) established in the 1980s-1990s, before institutional scale and public markets became dominant. The mechanism persists through institutional inertia: it worked for founding partners, it became canonical, it is defended as 'preserving alignment.' But its functional role has degraded — modern mega-funds operate with professional management independent of voting control. Theater ratio is moderate (0.45) because the stated rationale (alignment of incentives) is partially theater, partially real.
constraint_indexing:constraint_classification(blackstone_smd_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint solves a genuine coordination problem (strategic autonomy for long-term capital deployment) while simultaneously creating asymmetric extraction (governance concentration, information privileging, rent extraction). Both functions coexist structurally: the control mechanism enables pooled capital strategies (coordination) AND enables SMD disproportionate benefit capture (extraction). Classical tangled rope: cannot remove the coordination without destroying the extraction mechanism, and vice versa.
constraint_indexing:constraint_classification(blackstone_smd_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_smd_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_smd_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_smd_control, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blackstone_smd_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(blackstone_smd_control, TR),
    TR >= 0.70.

:- end_tests(blackstone_smd_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. SMDs capture disproportionate economic rents through absolute voting control, but the extraction is not total (0.70+) because: (1) SMD compensation, while asymmetric, is partially performance-linked (provides incentive alignment), (2) fund performance is genuinely superior, partially justifying the rent, (3) LP alternatives (other fund managers) exist, constraining extreme extraction. The trajectory from 0.42 (2007) to 0.58 (2025) reflects growing asymmetry as the firm scaled: initial SMD group shared benefits widely; post-scale consolidation concentrates power. Suppression (0.72): High. Structural barriers to LP exit include 10-15 year lock-ups, illiquidity, reputational costs of redemption, regulatory complexity, and lack of equally-scaled alternatives. SMDs have shaped the regulatory environment (lobbying, revolving-door presence) to maintain these barriers. Theater ratio (0.45): Moderate. The legitimating narrative—'SMD control ensures alignment and long-term thinking'—contains real functional truth (centralized strategy does enable contrarian positions) but also substantial theater: the same strategic benefits could be achieved with LP veto rights or independent governance committees. As the firm has matured, the alignment rationale has become increasingly performative (SMDs already have massive personal wealth at stake; additional control provides limited additional incentive). Claimed type (tangled_rope): Structural data supports this classification. The constraint is NOT a pure snare (extraction mechanism with no coordination benefit) because SMD control genuinely enables long-term capital strategies. But it IS NOT a rope (pure coordination with minimal extraction) because the governance asymmetry creates rents far exceeding the coordination function's value.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal and irreconcilable from within the constraint. LPs see a snare: they are trapped, extraction runs one direction (toward SMDs), and they have no veto. SMDs see a rope: they experience the control mechanism as coordination (enabling strategic autonomy). Regulators see a tangled rope: they must permit the coordination (SMDs need strategic autonomy) while constrained in their ability to limit extraction (regulatory capture, legal deference to private contracts). Reform advocates see a scaffold: the structure is temporary, sunset clauses and LP veto rights represent exit pathways being built. Legacy banking norms see a piton: the dual-class structure persists through institutional inertia long after its functional rationale has degraded. The analytical observer sees tangled rope: both functions (coordination and extraction) are structurally real and cannot be separated without destroying the mechanism entirely. This is the diagnostic signature of mandatrophy resolution: no single perspective is 'correct'; the constraint IS a tangled rope from the system level, even though individual agents experience it as snare, rope, or scaffold.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the control mechanism. SMDs: beneficiary status (control produces asymmetric benefit) + arbitrage exit options (can use control to avoid constraints) = low d (≈0.10-0.25), producing negative or minimal experienced extraction chi. They experience the constraint as enabling, not constraining. LPs: victim status (control subordinates their interests) + trapped exit (locked capital, no governance veto) = high d (≈0.90-0.95), producing maximum experienced extraction chi. The constraint appears coercive from their perspective. Regulators: organized power (rule-making authority) but constrained exit (cannot withdraw oversight), mixed victim/beneficiary (enforce coordination but constrained in limiting extraction) = moderate-high d (≈0.50-0.60), producing moderate experienced extraction chi. Reform advocates: organized power (coalition, legislative voice) with mobile exit (alternative governance models available) = lower d (≈0.40-0.50) because they see extraction pathways to exit. The engine's directionality derivation chain produces these d values automatically from beneficiary/victim declarations and exit options; no override needed because the structural data is clean.
 *
 * MANDATROPHY ANALYSIS:
 *   CASE STUDY: This constraint perfectly exemplifies the mandatrophy problem and its resolution. The constraint cannot be classified as a single type without losing critical structural information: (1) If classified as Rope (pure coordination), the analysis erases the extraction mechanism and legitimizes SMD rent-seeking. (2) If classified as Snare (pure extraction), the analysis erases the genuine coordination benefit and ignores why the constraint persists despite LP opposition. (3) Classifying as Tangled Rope reveals the structural truth: both functions coexist, neither dominates, and the constraint cannot be reformed without destroying both simultaneously. The reformation challenge is NOT to eliminate the constraint (it has legitimate coordination function) but to decouple the coordination function from the extraction mechanism. Possible decoupling paths: (a) maintain SMD control over operative strategy while implementing LP veto over compensation and capital allocation (preserves coordination, limits extraction), (b) implement multi-class voting where SMDs retain strategic veto but LPs gain governance participation (hybrid power-sharing), (c) sunset clauses requiring periodic LP ratification of the control structure (scaffold logic applied to governance). Each reform path trades off: (a) preserves SMD autonomy but requires SMD agreement to compensation limits (unlikely), (b) reduces extraction but introduces governance deadlock risk (common with multi-class structures), (c) creates certainty for reform timeline but invites SMD preemption or coalition-building to block sunset. The mandatrophy does NOT resolve to a single 'correct' type; it resolves to recognition that the constraint's coordination and extraction functions are structurally coupled, and reform requires either accepting the coupling (and valuing SMD autonomy) or accepting deadlock risk in decoupling attempts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    smds_vs_institutional_manager_distinction,
    'Are SMDs a distinct governance class or are they professional managers whose voting control reflects legitimate principal-agent alignment?',
    'Comparative analysis of compensation structures: SMDs as profit-participating partners vs. professional managers with performance-linked pay; analysis of decision-making independence and veto power for non-SMD executives',
    'If SMDs are distinct class: extraction is structural and requires governance reform. If SMDs are managers: control structure reflects legitimate operational necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smds_vs_institutional_manager_distinction, conceptual, 'Whether SMD control represents distinct class power or operational necessity').

omega_variable(
    lp_exit_capacity_actual,
    'What fraction of LP capital is in locked-up periods vs. redeemable, and what actual exit rate occurs in practice?',
    'Historical redemption data; analysis of lock-up term lengths and compliance rates; investor surveys on perceived ability to exit',
    'High actual exit rate (>30% annually): exit_options upgrade from trapped to constrained, reducing experienced extraction. Low actual exit rate (<5%): confirms trapped classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lp_exit_capacity_actual, empirical, 'Actual LP exit capacity and redemption rates').

omega_variable(
    smd_control_necessity_for_performance,
    'Is SMD voting control causally necessary for the superior risk-adjusted returns Blackstone achieves, or is control orthogonal to performance?',
    'Peer comparison analysis (KKR, Apollo, Carlyle comparative returns with/without voting control); regression analysis of control concentration vs. fund performance; historical analysis of performance changes after governance modifications',
    'If control is necessary: coordination perspective dominates; extraction is a legitimate cost of coordination. If orthogonal: control is pure extraction mechanism; classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smd_control_necessity_for_performance, empirical, 'Causal relationship between SMD control and fund performance').

omega_variable(
    regulatory_capture_scope,
    'To what extent do SMDs exert disproportionate influence over the regulatory framework governing their own governance structure?',
    'Lobbying expenditure analysis; revolving door tracking (SMDs to regulatory agencies); regulatory agency capture indicators; comparison to peer influence levels',
    'If significant capture: suppression upgrades from 0.72 to 0.85+, indicating institutional enforcement of extraction. If minimal: suppression estimate confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_scope, empirical, 'Degree of SMD regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_smd_control, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blac_tr_t0, blackstone_smd_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(blac_tr_t8, blackstone_smd_control, theater_ratio, 8, 0.42).
narrative_ontology:measurement(blac_tr_t18, blackstone_smd_control, theater_ratio, 18, 0.45).

% Extraction over time
narrative_ontology:measurement(blac_be_t0, blackstone_smd_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(blac_be_t8, blackstone_smd_control, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(blac_be_t18, blackstone_smd_control, base_extractiveness, 18, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_smd_control, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(blackstone_smd_control, 0.35).
narrative_ontology:affects_constraint(blackstone_smd_control, private_equity_fee_extraction).
narrative_ontology:affects_constraint(blackstone_smd_control, institutional_investor_governance_power).
narrative_ontology:affects_constraint(blackstone_smd_control, regulatory_capture_finance).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the general private equity fee extraction mechanism (ε≈0.50, snare) but represents a distinct structural constraint. The SMD voting control is a specific governance mechanism enabling and entrenching the broader fee extraction pattern. Affects institutional investor governance power (how large capital pools can constrain or resist extraction) and regulatory capture dynamics (SMD influence over fiduciary standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(blackstone_smd_control, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
