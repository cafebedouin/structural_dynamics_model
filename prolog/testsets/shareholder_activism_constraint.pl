% ============================================================================
% CONSTRAINT STORY: shareholder_activism_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shareholder_activism_constraint, []).

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
 *   constraint_id: shareholder_activism_constraint
 *   human_readable: Shareholder Activism Constraint
 *   domain: corporate_governance/financial_markets
 *
 * SUMMARY:
 *   Shareholder activism represents a structural constraint on corporate
 *   management arising from the separation of ownership and control. Activist
 *   investors use concentrated ownership stakes, proxy contests, and public
 *   pressure campaigns to force management alignment with shareholder
 *   interests. The constraint operates through governance mechanisms
 *   (shareholder votes, board seats, strategic pressure) but generates
 *   heterogeneous experiences across different agent types. The activist
 *   investor benefits from leverage and value capture; management experiences
 *   quarterly pressure and takeover threat; passive index funds face forced
 *   participation in activism they didn't initiate; workers' pension funds
 *   balance long-term returns against exploitation risks. The constraint
 *   exhibits both genuine coordination (aligning management with shareholder
 *   interests) and asymmetric extraction (concentrating decision-making
 *   power, shifting risk to management, creating theater through proxy voting
 *   rituals). The extractiveness has increased from 0.38 to 0.52 over the
 *   measurement interval as activism has become more frequent and
 *   institutionalized; theater has risen from 0.42 to 0.58 as annual meeting
 *   rituals have become more performative while actual change mechanisms
 *   migrate to behind-closed-doors pressure campaigns.
 *
 * KEY AGENTS:
 *   - Activist Investors: Primary beneficiary (institutional/arbitrage) — capture value through governance leverage and exit arbitrage; can move capital to new targets
 *   - Corporate Management: Primary victim (powerless/trapped) — faces quarterly pressure, board threat, potential displacement; limited exit options without career damage
 *   - Passive Index Funds: Secondary victim (moderate/constrained) — forced to participate in activism governance; cannot exit holdings without tracking error; bear governance participation costs
 *   - Workers' Pension Funds: Organized victim (organized/mobile) — fiduciary duty forces activism engagement; have exit through capital reallocation but constrained by diversification requirements and long-term horizons
 *   - Proxy Voting System: Institutional actor (institutional/arbitrage) — maintains performative annual meeting ritual; proxy advisors (ISS, Glass Lewis) function as gatekeeping intermediaries
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent ownership structures as inherent to capitalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shareholder_activism_constraint, 0.52).
domain_priors:suppression_score(shareholder_activism_constraint, 0.48).
domain_priors:theater_ratio(shareholder_activism_constraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shareholder_activism_constraint, extractiveness, 0.52).
narrative_ontology:constraint_metric(shareholder_activism_constraint, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(shareholder_activism_constraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shareholder_activism_constraint, tangled_rope).
narrative_ontology:human_readable(shareholder_activism_constraint, "Shareholder Activism Constraint").
narrative_ontology:topic_domain(shareholder_activism_constraint, "corporate_governance/financial_markets").

domain_priors:requires_active_enforcement(shareholder_activism_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shareholder_activism_constraint, activist_investors).
narrative_ontology:constraint_beneficiary(shareholder_activism_constraint, institutional_shareholders).
narrative_ontology:constraint_victim(shareholder_activism_constraint, management_discretion).
narrative_ontology:constraint_victim(shareholder_activism_constraint, long_term_strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED EXECUTIVE (SNARE) — Corporate management faces quarterly pressure cycles, threat of takeover, board displacement. Exit options are minimal: cannot leave the firm without career damage; cannot ignore shareholder demands without lawsuit risk. Maximum extraction from this position.
constraint_indexing:constraint_classification(shareholder_activism_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ACTIVIST INVESTOR (ROPE) — Benefits from the activism constraint through governance leverage and value capture. Can arbitrage between multiple targets; if one engagement fails, moves capital elsewhere. Experiences the constraint as coordination mechanism: mobilizing shareholder pressure to align management incentives.
constraint_indexing:constraint_classification(shareholder_activism_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: PASSIVE INDEX FUND (TANGLED ROPE) — Must hold index positions and cannot easily exit holdings; forced to engage in shareholder activism to protect index value. Faces cost burden of governance participation (proxy voting, shareholder meetings) with limited exit options. Also benefits from improved governance outcomes that raise fund returns. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(shareholder_activism_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WORKER PENSION FUND (TANGLED ROPE) — Organized agent (union/pension trustee) with mobile capital but fiduciary duty to beneficiaries forces engagement with activism. Benefits from governance improvements that raise long-term returns. Bears extraction through costly governance participation and exposure to management retaliation (which can depress stock prices). Moderate agency with significant constraint.
constraint_indexing:constraint_classification(shareholder_activism_constraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROXY VOTING SYSTEM (PITON) — Annual shareholder meetings and proxy contests are substantially performative rituals. Most votes are predetermined; proxy advisors (ISS, Glass Lewis) exercise enormous gate-keeping power without formal accountability. The ritual persists through institutional inertia despite low functional verification that votes actually change corporate behavior. Theater dominates; coordination function attenuated.
constraint_indexing:constraint_classification(shareholder_activism_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, the separation of ownership and control in corporations creates an irreducible agency conflict: shareholders cannot directly manage the firm; management cannot fully align with dispersed shareholder interests. This perspective sees the activism constraint as an immutable response to this structural problem. However, the empirical base_properties show this classification as a false summit — the constraint's extractiveness (0.52) and theater (0.58) reveal it as a contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(shareholder_activism_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shareholder_activism_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(shareholder_activism_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shareholder_activism_constraint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(shareholder_activism_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(shareholder_activism_constraint, TR),
    TR >= 0.70.

:- end_tests(shareholder_activism_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, rising. Activist investors capture substantial value through governance leverage, board seat threats, and strategic pressure. The extractiveness has increased as activism has professionalized and scaled — a 2010 activist campaign differed significantly from 2020 campaigns in sophistication and institutional acceptance. However, extractiveness is not as severe as pure snare (0.66+) because the constraint also delivers genuine governance improvements in many cases. Suppression (0.48): Moderate. Management faces real barriers to resistance — proxy vote rules, fiduciary duty litigation risk, shareholder litigation, media pressure — but also possesses significant capacity to delay or deflect activism (information asymmetry, complex capital structures, strategic alternatives). Theater ratio (0.58): Moderate-high. Annual shareholder meetings are substantially performative: voting outcomes are largely predetermined by proxy adviser recommendations; actual strategic change happens through pressure campaigns and board negotiations, not through shareholder votes. The theater has increased as activism has become more sophisticated — the public proxy contest has become a ritualized negotiation theater masking real power dynamics conducted off-camera.
 *
 * PERSPECTIVAL GAP:
 *   The activist investor sees pure coordination (Rope): 'We're aligning management with shareholder interests, creating value.' Management sees extraction (Snare): 'We're under constant threat, forced to abandon long-term strategy for quarterly earnings.' Index funds see a mixed burden (Tangled Rope): 'We benefit from governance improvements but bear the costs of activism participation.' The proxy voting system sees its own degradation (Piton): 'Annual meetings are theater; real power is exercised through concentrated activist stakes and threat of board displacement.' The analytical observer risks naturalizing this as inevitable agency conflict (Mountain) but should recognize that alternative ownership structures (stakeholder capitalism, long-term ownership norms, patient capital) could reduce extractiveness — the constraint is not immutable.
 *
 * DIRECTIONALITY LOGIC:
 *   Activist investors occupy the beneficiary position with arbitrage exit options — they can move capital between targets, amplifying their power relative to trapped insiders. d ≈ 0.15 (beneficiary with mobile exit). Management occupies the victim position with trapped exit — they cannot abandon their firms without career damage and have minimal ability to resist coordinated shareholder pressure. d ≈ 0.95 (victim with trapped exit). Passive index funds occupy an intermediate position: they are technically beneficiaries (governance improvements raise fund returns) but face constrained exit (cannot sell index holdings without tracking error) and bear activism participation costs. d ≈ 0.60 (mixed beneficiary-victim with constrained exit). Pension funds are organized (can coordinate with other long-term investors) but still constrained (fiduciary duty, diversification requirements). d ≈ 0.55 (organized victim with mobile but constrained exit). The piton perspective derives from the proxy advisor gatekeeping power and ritualized annual meetings — high theater (0.58) signals degraded function maintained by institutional inertia rather than effectiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing shareholder activism as a hybrid coordination-extraction mechanism. The genuine coordination function (aligning management with shareholder interests, reducing agency drift) is real and valuable. But the extraction function (concentrating decision power in activist hands, forcing management to sacrifice autonomy, creating theater through proxy voting) is also real. The constraint cannot be classified as pure coordination (Rope) because it involves suppression (0.48) and asymmetric distribution of benefits. It cannot be classified as pure extraction (Snare) because it delivers genuine governance improvements in many cases. The Tangled Rope classification captures this: activism genuinely coordinates and genuinely extracts, with extractive benefits concentrated on activist investors and coordination benefits dispersed across all shareholders. The measurement trajectory (extractiveness rising from 0.38 to 0.52, theater rising from 0.42 to 0.58) shows that as activism has become more professionalized and institutionalized, the extractive dimension has grown relative to the pure coordination dimension — the mechanism has shifted toward snare-like dynamics as it has matured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    activist_vs_passive_alignment,
    'Do activist shareholders impose value-destructive constraints on management, or do they correct genuine agency failures?',
    'Longitudinal analysis of firm performance post-activism campaign; comparison of returns under active vs passive ownership; stock price reaction to campaign announcement',
    'If value-destructive: activism is primarily extractive (snare). If value-corrective: activism is coordination (rope). If mixed: tangled rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(activist_vs_passive_alignment, empirical, 'Whether activist shareholder campaigns create or destroy long-term firm value').

omega_variable(
    short_termism_mechanism,
    'Does shareholder activism create short-term pressure that forces management to sacrifice long-term strategy, or is this a cover story for management resistance to needed changes?',
    'Analysis of capital allocation changes post-activism; R&D spending patterns; comparison of long-term vs short-term value realization; management testimony about constraint perception',
    'If short-termism real: suppression metric should increase. If cover story: management exaggerates constraint to defend discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(short_termism_mechanism, empirical, 'Whether quarterly pressure from activism actually forces suboptimal capital allocation').

omega_variable(
    proxy_advisor_capture,
    'Do proxy advisors (ISS, Glass Lewis) function as gatekeepers for activist demands, or do they provide genuine independent scrutiny of shareholder proposals?',
    'Analysis of ISS recommendation rates across activist vs non-activist proposals; correlation between ISS recommendation and shareholder vote outcome; investigation of ISS client conflicts',
    'If gatekeepers: proxy voting theater is high, piton classification confirmed. If independent: proxy system has real coordination function, should reclassify toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_advisor_capture, empirical, 'Proxy advisor independence and gatekeeping power in shareholder activism').

omega_variable(
    index_fund_exit_illusion,
    'Do passive index funds truly face immobile capital, or could they exit through index alternatives and threaten forced exit to influence management?',
    'Analysis of index fund switching costs; availability of alternative indices that exclude activist targets; theoretical modeling of switching as credible exit threat',
    'If immobile: index fund classification as constrained (tangled rope) correct. If mobile: reclassify as mobile (rope or lower chi tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_fund_exit_illusion, empirical, 'Whether passive index funds face genuine capital immobility or illusory constraint').

omega_variable(
    suppression_mechanism_structural,
    'Is management suppression of activism-driven changes structural (legal, structural incentives) or theatrical (managment resistance masquerading as external constraint)?',
    'Comparative analysis of activism success rates across regulatory regimes with different shareholder protections; analysis of management disclosure about activism impact; legal barrier identification',
    'If structural: suppression metric (0.48) correct and conservation-worthy. If theatrical: suppression should be lower, reclassifying some perspectives toward lower-extraction types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural, empirical, 'Whether measured suppression reflects structural barriers or manufactured resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shareholder_activism_constraint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shar_tr_t0, shareholder_activism_constraint, theater_ratio, 0, 0.42).
narrative_ontology:measurement(shar_tr_t5, shareholder_activism_constraint, theater_ratio, 5, 0.5).
narrative_ontology:measurement(shar_tr_t10, shareholder_activism_constraint, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(shar_be_t0, shareholder_activism_constraint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(shar_be_t5, shareholder_activism_constraint, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(shar_be_t10, shareholder_activism_constraint, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shareholder_activism_constraint, resource_allocation).
narrative_ontology:affects_constraint(shareholder_activism_constraint, corporate_short_termism).
narrative_ontology:affects_constraint(shareholder_activism_constraint, executive_compensation_escalation).
narrative_ontology:affects_constraint(shareholder_activism_constraint, board_capture_by_insiders).

% DUAL FORMULATION NOTE:
% Shareholder activism is downstream of the general separation of ownership and control problem in public corporations. Upstream constraints include regulatory frameworks governing proxy access, shareholders' rights, and fiduciary duties. This story focuses on the activism mechanism itself; related constraints address specific domains (short-termism, compensation, board capture) where activism operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shareholder_activism_constraint, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
