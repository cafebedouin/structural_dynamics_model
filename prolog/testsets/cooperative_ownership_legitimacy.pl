% ============================================================================
% CONSTRAINT STORY: cooperative_ownership_legitimacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cooperative_ownership_legitimacy, []).

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
 *   constraint_id: cooperative_ownership_legitimacy
 *   human_readable: Cooperative Ownership Legitimacy in Market Economies
 *   domain: economic_governance/organizational_structure
 *
 * SUMMARY:
 *   Cooperative ownership legitimacy represents a structural tension between
 *   the genuinely egalitarian governance ideals that distinguish cooperatives
 *   from conventional firms and the capital discipline and decision latency
 *   costs that those governance structures impose. Over the 20-year interval,
 *   the constraint has shifted: cooperatives have become more established
 *   (lower initial extractiveness of capital access barriers) but
 *   simultaneously more susceptible to governance theater (rising theater
 *   ratio as formal democratic structures persist while actual
 *   decision-making consolidates). The constraint exhibits the full range of
 *   Deferential Realism types depending on observational position. From the
 *   trapped member-worker's perspective, cooperative membership is a snare —
 *   capital lock-in prevents exit despite nominal ownership. From the
 *   organized movement's perspective, cooperatives solve genuine collective
 *   action problems (Rope). From the state's perspective, support systems are
 *   temporary scaffolding. From the institutional cooperative financial
 *   system's perspective, the constraint is tangled — coordinating capital
 *   with extracting discipline simultaneously. From the legal system's
 *   perspective, cooperative recognition is increasingly performative
 *   (Piton). From the analytical observer's perspective, the constraint risks
 *   being naturalized as inherent to economics (false Mountain). The key
 *   structural question is whether extracted extraction reflects legitimate
 *   coordination costs (genuine Tangled Rope) or the failure of cooperative
 *   governance structures to prevent elite capture and power concentration
 *   (degraded Piton or pure Snare).
 *
 * KEY AGENTS:
 *   - Trapped Member-Workers: Primary victims (powerless/trapped) — face capital lock-in despite ownership claims; high vulnerability to cooperative failure; no alternative local employment
 *   - Mobile Cooperative Members: Secondary victims and partial beneficiaries (moderate/constrained) — enjoy governance participation but accept lower career advancement and capital returns
 *   - Cooperative Movement Organizations: Primary beneficiaries (organized/mobile) — solve collective action through federations, networks, technical assistance; mobile exit options reduce experienced extraction
 *   - Cooperative Financial Systems: Institutional beneficiary and partial victim (institutional/constrained) — coordinate capital access while enforcing behavioral discipline; constrained by regulations and competitive pressure
 *   - State Cooperative Support Systems: Institutional beneficiary with arbitrage options (organized/arbitrage) — provide temporary scaffolding for cooperative legitimacy and capital access
 *   - Cooperative Legal Recognition: Institutional structure (institutional/arbitrage) — maintains legal form through inertia despite declining functional distinctiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as economic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cooperative_ownership_legitimacy, 0.38).
domain_priors:suppression_score(cooperative_ownership_legitimacy, 0.48).
domain_priors:theater_ratio(cooperative_ownership_legitimacy, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cooperative_ownership_legitimacy, extractiveness, 0.38).
narrative_ontology:constraint_metric(cooperative_ownership_legitimacy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(cooperative_ownership_legitimacy, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cooperative_ownership_legitimacy, tangled_rope).
narrative_ontology:human_readable(cooperative_ownership_legitimacy, "Cooperative Ownership Legitimacy in Market Economies").
narrative_ontology:topic_domain(cooperative_ownership_legitimacy, "economic_governance/organizational_structure").

domain_priors:requires_active_enforcement(cooperative_ownership_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cooperative_ownership_legitimacy, cooperative_member_workers).
narrative_ontology:constraint_beneficiary(cooperative_ownership_legitimacy, cooperative_governance_structures).
narrative_ontology:constraint_victim(cooperative_ownership_legitimacy, capital_discipline).
narrative_ontology:constraint_victim(cooperative_ownership_legitimacy, scalability_constraints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED MEMBER-WORKER (SNARE) — Worker-owners in small cooperatives face high switching costs: divesting from cooperative shares, losing internal job security, forfeiting accumulated governance relationships. Structurally trapped by capital lock-in despite nominal ownership stake. High suppression of alternatives (limited outside employment in same locale, capital illiquidity). Maximum extraction from this position — the cooperative system extracts stability expectations and governance participation while the member remains materially vulnerable to cooperative failure.
constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MOBILE COOPERATIVE MEMBER (TANGLED ROPE) — Workers with skills portable to non-cooperative firms face constrained exit: career advancement in cooperatives is slower (limited hierarchy for advancement), capital returns are lower than equity-like returns in startups, but genuine coordination benefits exist (workplace democracy, voice in governance, profit-sharing). Mixed extraction — pays a career cost for governance participation but receives real benefits in autonomy and fairness.
constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COOPERATIVE MOVEMENT (ROPE) — Organized cooperative federations and networks solve collective action problems: member education, capital pooling, technical assistance, market access for small producers. The coordination function is genuine and high-value. Exit options are mobile — federations can be reformed, alliances shifted — so effective extraction is low. Benefits flow to member organizations through economies of scale and knowledge transfer.
constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: COOPERATIVE FINANCIAL SYSTEMS (TANGLED ROPE) — Credit unions and cooperative banks coordinate capital mobilization (coordination benefit) but enforce discipline on member behavior: reserve requirements, voting power tied to capital contribution (extractive asymmetry). The system extracts compliance and conservative financial behavior while providing genuine access to capital that mainstream banks deny. Institutional actors are constrained by regulatory frameworks and competitive pressure from conventional finance.
constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE COOPERATIVE SUPPORT SYSTEMS (SCAFFOLD) — Government programs (tax exemptions, favorable lending, cooperative development agencies) temporarily solve the capital access and legitimacy barriers that prevent cooperative scaling. These supports have sunset logic: as cooperatives mature and establish creditworthiness, supports should decline. Current state: supports persist longer than intended (theater), but the structural mechanism is genuinely temporary coordination. Exit options are arbitrage — states can shift priorities toward other models.
constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COOPERATIVE LEGAL RECOGNITION (PITON) — Legal codes recognizing cooperative forms (1990 ICA definition, national cooperative laws) persist largely through institutional inertia. The original coordination function (enabling alternative ownership structures) has atrophied as mainstream corporations adopted some cooperative features (employee stock ownership, profit-sharing). Legal recognition is now substantially performative — the legal form exists but lacks the distinctive functional value it originally provided. Theater ratio (0.62) reflects that cooperative identity increasingly serves legitimacy functions (appearing principled) rather than enforcement of actual governance democracy.
constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital discipline and coordination costs are inherent to economic organization: any system must solve capital allocation and member accountability. From this view, the cooperative-capitalist distinction naturalizes as a fundamental trade-off. However, the structural data contradicts mountain classification — the constraints are contingent institutional choices, not natural laws. This perspective instantiates the oracle gap: the analytical observer's native instruments (assuming capital scarcity requires market discipline) prevent seeing that cooperative constraints are enforced, not inevitable.
constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cooperative_ownership_legitimacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cooperative_ownership_legitimacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(cooperative_ownership_legitimacy, TR),
    TR >= 0.70.

:- end_tests(cooperative_ownership_legitimacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The cooperative constraint extracts from member-workers through capital lock-in and time costs of governance participation, but the extraction is not severe because genuine benefits exist (profit-sharing, voice in governance, workplace security). The measured value reflects that cooperatives deliver on some promises while imposing costs that capitalism also externalizes but into different hands. The 20-year trajectory shows rising extractiveness (0.22 to 0.38) as initial capital barriers decline but governance theater increases — the burden shifts from capital scarcity to legitimacy performance. Suppression (0.48): Moderate. Significant barriers to exit include capital illiquidity, specialized skills with limited portability, and replacement employment scarcity in cooperative-dense regions. But suppression is not total — some members do leave cooperatives, alternative employment exists, and in developed economies mobility is structurally possible even when costly. Theater ratio (0.62): Moderate-high and rising. The growth of theater reflects that cooperative governance has become increasingly performative: formal democratic structures (member votes, board elections) persist but actual decision-making consolidates through founder influence, technical expertise concentration, and path dependency. Early cooperatives (t=0) had lower theater because either governance was more genuinely participatory or the performance was more directly tied to survival. Mature cooperatives (t=20) maintain formal democracy while operational power concentrates — hence higher theater.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap is between the trapped member-worker (Snare) and the organized cooperative movement (Rope). Both perceive the same constraint, but the trapped agent has no exit option while the organized agent can shift priorities, reform structures, or exit from specific cooperatives while remaining in the movement. This gap reveals that the constraint's 'type' is not intrinsic — it is observer-relative. From the trapped position, it is a Snare. From the organized position, it is a Rope. From the state's position, it is a Scaffold. The analytical observer risks naturalizing the Snare or the Rope as 'the real nature' of cooperatives — either 'cooperatives are inherently exploitative' or 'cooperatives are inherently fair' — when the constraint's structure is perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary directionality ambiguity concerns whether extracted extraction (capital discipline, governance time costs) represents legitimate coordination costs or extractive overhead. For the trapped member-worker, all overhead is experienced as extraction because exit is not available — their d is high (≈0.92) regardless of whether the costs are 'really' legitimate. For the mobile member, the calculation depends on whether the costs are lower than the capital discipline imposed by market discipline in conventional firms — if cooperative discipline is lighter, their d is lower (≈0.50); if heavier, their d is higher (≈0.70). For the cooperative movement, directionality is determined by whether federations genuinely reduce member costs or whether they extract membership dues and enforce movement ideology — the evidence suggests genuine coordination, supporting low d (≈0.35). For the state, directionality is institutional beneficiary with minimal extraction from this specific constraint (d ≈ 0.08).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in cooperative ownership legitimacy is whether the constraint represents genuine alternative coordination (Rope or Tangled Rope with legitimate overhead) or failed alternative coordination (Snare disguised as Rope, or Piton pretending to be Tangled Rope). The classification depends on resolving the omega variables: If capital scalability is fundamentally limited (low threshold), then cooperatives are inherently small-scale and the constraint's legitimacy claim must accept that limitation. If extraction mechanism is governance capture rather than capital discipline, then the Snare classification is accurate despite cooperative claims. If identity-fusion is the primary binding mechanism, then members are trapped by internalized ideology rather than external barriers — a sophisticated form of identity_locked constraint. If theater content is cynical rather than aspirational, then the constraint is Piton, not Tangled Rope. The mandatrophy is resolved by determining which of these empirical questions applies. The claimed tangled_rope classification assumes: (1) capital scalability is real but limited, (2) extraction mechanism is legitimate capital discipline rather than pure capture, (3) identity-fusion is minimal, and (4) theater is aspirational — members genuinely believe in governance despite imperfect implementation. If any of these assumptions fail, the classification shifts toward Snare or Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_scalability_threshold,
    'At what organizational scale does the coordination benefit of cooperative governance become exceeded by capital discipline extraction costs?',
    'Longitudinal comparative analysis of cooperative vs conventional firm performance across size categories; measurement of governance participation costs (time spent in meetings, decision latency) vs capital productivity gains',
    'If threshold is low (< 100 members): cooperatives face fundamental scaling limits, and cooperative legitimacy claim depends on accepting small-scale as virtue. If threshold is high (> 1000 members): cooperatives can compete with conventional firms at scale, and legitimacy is not constrained by size.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_scalability_threshold, empirical, 'Scale at which cooperative coordination benefits are exceeded by capital discipline costs').

omega_variable(
    extraction_mechanism_locus,
    'Is the primary extraction in cooperatives located in capital discipline (member behavior control) or in governance capture (how democratic rules are subverted in practice)?',
    'Detailed audit of failed cooperatives: do failures stem from capital constraints (insufficient investment, slow growth) or from governance failures (founder control, elite capture, unequal power despite formal equality)? Analysis of internal power distributions in mature cooperatives.',
    'If capital discipline: the constraint is partly legitimate economic necessity (higher Rope classification). If governance capture: the cooperative legitimacy claim is false (pure Snare or degraded Piton). Determines whether the tangled_rope classification is accurate or reflects failed aspirations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_locus, empirical, 'Whether extraction stems from capital discipline or governance capture').

omega_variable(
    movement_identity_fusion,
    'Is the cooperative movement''s legitimacy constraint driven by identity-fusion (participant''s self-concept constituted through cooperative membership) or by structural incentives (genuine economic benefits)?',
    'Survey and interview analysis: exit intention and cost assessment by members; comparison of stated vs revealed preference for cooperative membership (what premium would members accept to stay vs leave?); analysis of member behavior change when identity frame is challenged (e.g., explicit profit-sharing comparisons with conventional firms)',
    'If identity-fused: members are trapped by identity lock even when economic case is weak (classification shift toward Snare). If incentive-based: genuine Tangled Rope or Rope classification holds. Informs whether the constraint is self-imposed through identity commitment or externally enforced through economic necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(movement_identity_fusion, empirical, 'Degree of identity-fusion vs structural incentive in cooperative membership').

omega_variable(
    legitimacy_theater_content,
    'Does the high theater ratio (0.62) reflect aspirational governance (members believe in democratic ideals even if imperfectly practiced) or cynical performance (member participation as legitimacy ritual rather than actual governance)?',
    'Analysis of meeting agendas and actual decision outcomes: are member votes determinative or largely confirmatory? Measurement of decision latency (time from proposal to implementation) in cooperatives vs conventional firms. Tracking of member satisfaction with governance influence over time.',
    'If aspirational: theater reflects genuine commitment despite imperfect implementation (Tangled Rope holds). If cynical: theater masks extraction mechanism (Snare or Piton). Determines whether the constraint''s legitimacy claim is recoverable or fundamentally compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_theater_content, empirical, 'Whether theater in cooperative governance is aspirational or cynical').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cooperative_ownership_legitimacy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coop_own_tr_t0, cooperative_ownership_legitimacy, theater_ratio, 0, 0.38).
narrative_ontology:measurement(coop_own_tr_t10, cooperative_ownership_legitimacy, theater_ratio, 10, 0.52).
narrative_ontology:measurement(coop_own_tr_t20, cooperative_ownership_legitimacy, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(coop_own_be_t0, cooperative_ownership_legitimacy, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(coop_own_be_t10, cooperative_ownership_legitimacy, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(coop_own_be_t20, cooperative_ownership_legitimacy, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cooperative_ownership_legitimacy, resource_allocation).
narrative_ontology:affects_constraint(cooperative_ownership_legitimacy, labor_voice_mechanisms).
narrative_ontology:affects_constraint(cooperative_ownership_legitimacy, capital_access_finance).
narrative_ontology:affects_constraint(cooperative_ownership_legitimacy, organizational_democracy).

% DUAL FORMULATION NOTE:
% Cooperative ownership legitimacy is downstream of broader capital allocation mechanisms and upstream of specific cooperative sectoral implementations (agriculture, finance, consumer goods). The constraint's extractiveness reflects institutional and legal choices that differ by jurisdiction — decomposition into regional stories may be warranted if the extractiveness values diverge significantly across countries with different cooperative legal frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cooperative_ownership_legitimacy, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
