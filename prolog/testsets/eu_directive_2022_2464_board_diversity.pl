% ============================================================================
% CONSTRAINT STORY: eu_directive_2022_2464_board_diversity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_directive_2022_2464_board_diversity, []).

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
 *   constraint_id: eu_directive_2022_2464_board_diversity
 *   human_readable: EU Directive 2022/2464 Board Diversity Quota
 *   domain: corporate_governance/regulatory_policy
 *
 * SUMMARY:
 *   EU Directive 2022/2464 mandates that publicly listed companies achieve
 *   40% women on supervisory boards (or 33% on combined management and
 *   supervisory boards) by June 2026. The constraint exhibits structural
 *   tension between a genuine coordination function (solving collective
 *   action problems in governance diversity) and asymmetric extraction
 *   through compliance costs, tokenism risks, and credential-signaling
 *   ambiguity. Different actors experience the same regulatory mandate as
 *   pure coordination (the EU institution), mixed coordination-extraction
 *   (the qualified woman director), snare mechanics (the excluded candidate
 *   pool), strategic constraint (the multinational corporation), performative
 *   bureaucracy (the compliance infrastructure), or immutable demographic law
 *   (the false-summit analytical view). Base extractiveness (0.38) reflects
 *   moderate asymmetry: the directive creates real benefits for represented
 *   women and solving governance heterogeneity but imposes costs through
 *   implementation and potential tokenism. Suppression (0.45) captures
 *   significant barriers to substantive inclusion despite the quota:
 *   gatekeeping mechanisms in informal networks, credential stacking
 *   expectations, and sponsorship asymmetries persist beneath the numerical
 *   mandate. Theater ratio (0.55) indicates moderate performative content:
 *   the directive generates substantial reporting and compliance
 *   infrastructure that may not translate to substantive board influence
 *   changes.
 *
 * KEY AGENTS:
 *   - Qualified Woman Directors: Primary beneficiary (moderate/constrained) — gain visibility and formal access; bear tokenism and credential-signaling costs. Representation increased 10-15% annually post-directive in many jurisdictions.
 *   - Excluded Candidate Pool: Primary victim (powerless/trapped) — women not yet in professional/board pipeline remain excluded despite quota; structural gatekeeping persists beneath quota mandate.
 *   - EU Regulatory Institutions: Secondary beneficiary (institutional/arbitrage) — gain policy coordination lever and enforcement authority across member states; no material extraction cost.
 *   - Multinational Corporations: Mixed position (powerful/mobile) — benefit from governance clarity and reduced internal coordination ambiguity; bear implementation costs and candidate-identification burden. Exit via non-EU operations provides constraint relief.
 *   - Corporate Compliance Infrastructure: Institutional actor (organized/constrained) — develops reporting and monitoring systems; sustains through regulatory oversight; theater content increases as procedural burden grows.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating demographic representation as an immutable natural law rather than the contingent outcome of institutional selection mechanisms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_directive_2022_2464_board_diversity, 0.38).
domain_priors:suppression_score(eu_directive_2022_2464_board_diversity, 0.45).
domain_priors:theater_ratio(eu_directive_2022_2464_board_diversity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_directive_2022_2464_board_diversity, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_directive_2022_2464_board_diversity, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(eu_directive_2022_2464_board_diversity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_directive_2022_2464_board_diversity, tangled_rope).
narrative_ontology:human_readable(eu_directive_2022_2464_board_diversity, "EU Directive 2022/2464 Board Diversity Quota").
narrative_ontology:topic_domain(eu_directive_2022_2464_board_diversity, "corporate_governance/regulatory_policy").

domain_priors:requires_active_enforcement(eu_directive_2022_2464_board_diversity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_directive_2022_2464_board_diversity, underrepresented_women_directors).
narrative_ontology:constraint_beneficiary(eu_directive_2022_2464_board_diversity, eu_regulatory_institutions).
narrative_ontology:constraint_beneficiary(eu_directive_2022_2464_board_diversity, workplace_equity_advocates).
narrative_ontology:constraint_victim(eu_directive_2022_2464_board_diversity, corporate_board_selection_autonomy).
narrative_ontology:constraint_victim(eu_directive_2022_2464_board_diversity, candidate_pool_quality_concerns).
narrative_ontology:constraint_victim(eu_directive_2022_2464_board_diversity, implementation_resource_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CANDIDATE POOL (SNARE) — Structurally trapped outside board selection despite qualification. The quota creates appearance of inclusion while actual gatekeeping mechanisms (network effects, informal sponsorship, credential stacking) persist. Suppression is high: the pool has no enforcement mechanism to challenge substantive exclusion. The quota is performative relative to the barrier structure.
constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: QUALIFIED WOMAN DIRECTOR (TANGLED ROPE) — Genuine coordination function: the directive solves the collective action problem of boardroom homogeneity and increases candidate visibility. But also bears costs: tokenism risk, heightened scrutiny, expectation to represent all women, potential credential-signaling ambiguity (selected for quota or for merit?). Moderate extraction through normative expectations and representation burden.
constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EU REGULATORY INSTITUTION (ROPE) — Pure coordination function: solves the collective action problem of coordinating corporate governance diversity across 27 member states. Experiences the constraint as enabling coordination and market harmonization. Benefits from enforcement authority and policy lever. No extraction — the regulatory body gains from the coordination mechanism.
constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: MULTINATIONAL CORPORATION (TANGLED ROPE) — Coordination benefit: clearer governance standards reduce uncertainty in compliance. But extraction through implementation costs, candidate pipeline development, potential talent loss if forced choices between qualified men and quota-driven women selections create perceived internal inequity. Mobile exit via non-EU operations reduces effective extraction, but EU market importance constrains exit. Mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CORPORATE COMPLIANCE INFRASTRUCTURE (PITON) — The directive has catalyzed reporting mechanisms and monitoring systems that persist through institutional inertia. Theater ratio is moderate-high: much of the infrastructure is procedural reporting rather than substantive board function change. The actual impact on board decision-making is contested. Compliance persists because regulatory oversight is real and penalties exist, not because the mechanism optimally serves board effectiveness.
constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STATISTICAL PARITY VIEW (MOUNTAIN) — From a civilizational perspective, the constraint may be treated as reflecting immutable demographic realities: if women represent X% of qualified candidates, board composition should reflect X%. This perspective naturalizes statistical representation as a law. However, this risks confusing correlation (current representation) with causal constraint (qualification availability). The engine's false summit detector identifies this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_directive_2022_2464_board_diversity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_directive_2022_2464_board_diversity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_directive_2022_2464_board_diversity, TR),
    TR >= 0.70.

:- end_tests(eu_directive_2022_2464_board_diversity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The directive creates measurable benefits (formal pathway to board access, visibility for qualified women) but also measurable costs (implementation burden for corporations, potential credential-signaling ambiguity, tokenism risk). The value reflects that this is a genuine Tangled Rope — not pure coordination (no extraction) and not pure extraction (significant coordination value). The trajectory shows increasing extractiveness over the implementation period as corporations realize compliance costs and potential talent-management complications. Suppression (0.45): Moderate-high. The directive creates formal pathways, reducing legal/regulatory suppression, but structural suppression persists: informal networks, sponsorship asymmetries, credential-stacking norms, and cultural gatekeeping remain largely unaddressed by the quota. The quota removes one suppression mechanism but cannot eliminate all gatekeeping. Theater ratio (0.55): Moderate and increasing. The directive catalyzes substantial reporting infrastructure (board composition metrics, compliance documentation) that is procedurally mandated but may not correlate with substantive board influence changes. As corporations develop compliance systems, theater content increases relative to functional change.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal that the same regulatory mandate is experienced as pure coordination (EU institution), mixed coordination-extraction (qualified woman director, multinational corporation), snare mechanics (excluded candidate pool), performative bureaucracy (compliance infrastructure), or immutable law (false-summit analytical view). The perspectival gap is largest between the EU institution (perceives pure coordination) and the excluded candidate pool (perceives snare extraction). The qualified woman director's Tangled Rope classification is the equilibrium perspective — neither pure coordination nor pure extraction, acknowledging genuine benefits alongside genuine costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from power level, exit options, and beneficiary/victim status. The qualified woman director (moderate/constrained) has moderately high d: constrained exit (career damage from quota rejection, but options exist) combined with victim status (tokenism burden, credential ambiguity). The EU institution (institutional/arbitrage) has low d: beneficiary status with exit options (arbitrage at the policy level via discretionary enforcement). The multinational corporation (powerful/mobile) has moderate d: beneficiary from governance clarity but victim to implementation costs; mobile exit (non-EU operations) reduces d. The excluded candidate pool (powerless/trapped) has maximum d: trapped exit with victim status. The suppression metric is unscaled and reflects actual structural barriers: gatekeeping mechanisms, informal networks, credential stacking that exist regardless of power context.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by disambiguating the quota's structural function from its social framing. The framing narrative is 'solving exclusion through equal representation,' which could support either pure Rope (if the quota substantively solves inclusion) or pure Snare (if it creates tokenistic cover for unchanged gatekeeping). The structural data — beneficiary/victim declarations, suppression persistence, theater ratio increase, implementation costs — clarifies that the constraint is Tangled Rope: genuine coordination value (solving homogeneity and visibility problems) mixed with asymmetric extraction (implementation costs, credential-signaling ambiguity, tokenism burden). The mandatrophy is resolved not by choosing between pure coordination and pure extraction, but by recognizing that regulatory quotas can simultaneously solve coordination problems AND create extraction mechanisms through implementation asymmetry and symbolic cover for persistent gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    candidate_pipeline_constraint,
    'Is the low representation of women directors caused by insufficient qualified candidates or by gatekeeping mechanisms in recruitment and selection?',
    'Longitudinal study of candidate pipeline across educational, professional, and board-adjacent roles; comparison of woman vs man advancement rates within qualified cohorts; analysis of sponsorship patterns and informal network effects',
    'If pipeline constraint: quota may be premature or require simultaneous pipeline-building; classification remains Tangled Rope but extraction value decreases as genuine coordination addressing bottleneck. If gatekeeping: quota directly targets the extraction mechanism; classification remains Snare/Tangled Rope and extraction values are accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(candidate_pipeline_constraint, empirical, 'Whether representation gap reflects candidate availability or selection gatekeeping').

omega_variable(
    tokenism_mechanism_strength,
    'Does the quota generate substantive board inclusion or primarily performative representation through tokenism?',
    'Analysis of board committee placement for women directors; study of decision-making influence and voice pattern changes; measurement of women director retention and advancement rates post-appointment; comparison of board diversity metrics vs. substantive governance outcomes',
    'If tokenism dominant: extraction value increases (theater ratio rises as quota becomes cover for unchanged power dynamics). Classification drifts toward Piton. If substantive inclusion: extraction value decreases and classification drifts toward pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tokenism_mechanism_strength, empirical, 'Degree to which quota produces tokenism vs substantive inclusion').

omega_variable(
    international_regulatory_arbitrage,
    'Do corporations subject to the directive migrate operations or board locations to avoid compliance, thereby reducing effective suppression and extraction?',
    'Tracking of corporate board relocations, subsidiary redomiciliation, and governance structure changes post-directive; analysis of board composition for companies with multiple listing venues (EU vs non-EU)',
    'If significant arbitrage: suppression and extraction values should decrease; exit_options upgrade from trapped/constrained to mobile for mobile corporations; perspectival gap narrows as regulatory reach diminishes. If minimal arbitrage: values remain stable and regulatory enforcement is effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_regulatory_arbitrage, empirical, 'Degree of regulatory arbitrage via board relocation').

omega_variable(
    merit_signal_ambiguity,
    'Does the quota create credential-signaling ambiguity that reduces the signal value of women board member appointments?',
    'Labor market analysis: tracking of post-board career outcomes for women directors appointed before vs after quota; salary/option compensation differential relative to male counterparts; market reaction to women director appointments (stock price changes); third-party credibility assessments of board diversity vs board performance correlations',
    'If ambiguity high: extraction value increases through reduced career signal value; psychological cost to beneficiary group increases. If ambiguity low: quota is pure coordination and extraction decreases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merit_signal_ambiguity, empirical, 'Whether quota creates merit-signaling ambiguity for appointed women').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_directive_2022_2464_board_diversity, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_d_tr_t0, eu_directive_2022_2464_board_diversity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eu_d_tr_t2, eu_directive_2022_2464_board_diversity, theater_ratio, 2, 0.48).
narrative_ontology:measurement(eu_d_tr_t4, eu_directive_2022_2464_board_diversity, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(eu_d_be_t0, eu_directive_2022_2464_board_diversity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(eu_d_be_t2, eu_directive_2022_2464_board_diversity, base_extractiveness, 2, 0.29).
narrative_ontology:measurement(eu_d_be_t4, eu_directive_2022_2464_board_diversity, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_directive_2022_2464_board_diversity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_directive_2022_2464_board_diversity, 0.12).
narrative_ontology:affects_constraint(eu_directive_2022_2464_board_diversity, executive_pay_ratio_regulation).
narrative_ontology:affects_constraint(eu_directive_2022_2464_board_diversity, corporate_governance_transparency_standards).

% DUAL FORMULATION NOTE:
% The board diversity constraint is distinct from underlying candidate pipeline constraints. If the pipeline constraint (availability of qualified women in board-adjacent professional roles) is decomposed into a separate story, it would show higher extractiveness (ε > 0.50) reflecting the gatekeeping mechanisms that the quota addresses. This story focuses on the regulatory mandate itself; pipeline decomposition would show upstream extraction mechanisms that the directive targets but does not eliminate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_directive_2022_2464_board_diversity, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
