% ============================================================================
% CONSTRAINT STORY: sotu_1996_clinton_media_industry_rating_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1996_clinton_media_industry_rating_system, []).

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
 *   constraint_id: sotu_1996_clinton_media_industry_rating_system
 *   human_readable: Industry-Led TV Rating System (SOTU 1996 Clinton Media Initiative)
 *   domain: social_policy/media_regulation
 *
 * SUMMARY:
 *   The television rating system established through the 1996
 *   Telecommunications Act and industry self-regulatory response creates a
 *   hybrid institutional constraint where the entertainment industry
 *   voluntarily adopted content identification standards (analogous to movie
 *   ratings) coupled with V-chip technology enabling parental filtering. The
 *   constraint benefits large broadcast networks and organized parent
 *   coalitions by avoiding direct government censorship while distributing
 *   implementation costs to content creators and independent producers. Over
 *   the 25-year interval (1996-2021), the constraint's functional value has
 *   degraded as cable and streaming distribution have reduced reliance on
 *   broadcast networks, yet the rating system persists through institutional
 *   inertia. The constraint exhibits Tangled Rope structure: genuine
 *   coordination function (parents gain filtering information, networks avoid
 *   regulatory burden) coexists with asymmetric extraction (independent
 *   creators bear compliance costs, major networks gain competitive advantage
 *   through 'family friendly' branding).
 *
 * KEY AGENTS:
 *   - Broadcast Networks (Major): Primary beneficiary (institutional/arbitrage) — avoid government regulation, gain consumer perception of responsibility, benefit from advertiser segmentation
 *   - Federal Government: Secondary beneficiary (institutional/arbitrage) — achieves content regulation without constitutional exposure, minimal implementation cost
 *   - Parent Advocacy Coalition: Tertiary beneficiary (organized/mobile) — gain actionable information and filtering technology; well-organized and benefit-aligned
 *   - Independent Content Creators: Primary victim (powerless/trapped) — forced to comply with standardized rating categories, bear production delays and homogenization pressure, cannot exit broadcast distribution
 *   - Content Review Board Infrastructure: Institutional actor (institutional/arbitrage) — maintains rating enforcement through inertia; sees own process as degraded but persists because alternatives would be more controversial
 *   - Viewers (Non-Compliant): Secondary victim (powerless/constrained) — experience content restrictions based on categorical labels regardless of individual preference
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies the constraint as functional hybrid that becomes increasingly performative as distribution channels fragment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1996_clinton_media_industry_rating_system, 0.52).
domain_priors:suppression_score(sotu_1996_clinton_media_industry_rating_system, 0.48).
domain_priors:theater_ratio(sotu_1996_clinton_media_industry_rating_system, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1996_clinton_media_industry_rating_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1996_clinton_media_industry_rating_system, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1996_clinton_media_industry_rating_system, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1996_clinton_media_industry_rating_system, tangled_rope).
narrative_ontology:human_readable(sotu_1996_clinton_media_industry_rating_system, "Industry-Led TV Rating System (SOTU 1996 Clinton Media Initiative)").
narrative_ontology:topic_domain(sotu_1996_clinton_media_industry_rating_system, "social_policy/media_regulation").

domain_priors:requires_active_enforcement(sotu_1996_clinton_media_industry_rating_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1996_clinton_media_industry_rating_system, broadcast_networks).
narrative_ontology:constraint_beneficiary(sotu_1996_clinton_media_industry_rating_system, parents_with_filtering_access).
narrative_ontology:constraint_victim(sotu_1996_clinton_media_industry_rating_system, content_creators_independent).
narrative_ontology:constraint_victim(sotu_1996_clinton_media_industry_rating_system, advertisers_restricted_demographics).
narrative_ontology:constraint_victim(sotu_1996_clinton_media_industry_rating_system, viewers_non_compliant_with_labels).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT CONTENT CREATOR (SNARE) — Forced to adopt standardized rating categories that flatten nuance. Cannot exit the rating system; distribution through broadcast networks requires compliance. Bears extraction costs (production delays for content review, homogenization pressure) with minimal benefit. No appeal mechanism or granularity options. Maximum suppression: the rating system constrains creative expression at the production level.
constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BROADCAST NETWORK EXECUTIVE (TANGLED ROPE) — Faces coordination burden (implementing rating infrastructure, training review staff, integrating V-chip metadata) but avoids direct government regulation. Benefits from self-regulatory legitimacy and consumer perception of responsibility. Constrained by the need to maintain advertiser relationships and audience reach while complying with rating standards. Mixed extraction and coordination: the constraint enforces voluntary adoption but converts it into competitive advantage through 'family friendly' branding.
constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL GOVERNMENT (ROPE) — Achieves content regulation through industry self-governance, avoiding constitutional confrontation over direct censorship. Extraction is minimal because the government benefits from the appearance of non-intervention while the industry shoulders the implementation cost. The V-chip mandate is an enabling technology, not a regulatory burden on government. The government has arbitrage: it could regulate directly, but self-regulation is cheaper and less constitutionally exposed.
constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR ADVERTISER (TANGLED ROPE) — Benefits from audience segmentation enabled by ratings (more precise targeting of child-safe programming) but constrained by restricted ad placement in restricted-rating content. Can shift budget between networks but cannot exit the rating system's constraint on advertiser-content matching. Experiences both coordination benefit (clearer content environment for child-targeted products) and extraction (reduced inventory in high-value slots due to restrictions).
constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PARENT ADVOCACY COALITION (ROPE) — Organized beneficiary of the rating system. Gains actionable information and filtering technology. Extraction is minimal because the coalition has collective power and clear benefits. Can theoretically exit (not use V-chip) but benefits from the system's public availability. Experiences the constraint as pure coordination: the industry and government solved a collective action problem (providing parents information) that parents could not solve alone.
constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CONTENT REVIEW BOARD INFRASTRUCTURE (PITON) — The rating enforcement mechanism persists through institutional inertia despite degrading functionality. Initial theater ratio (~0.40 at launch) has risen to 0.65 as rating categories become increasingly divorced from actual content severity. Reviewers make binary category calls on multidimensional content. The theater increases as content complexity outpaces the rating system's granularity. The infrastructure is maintained because alternatives (pre-broadcast censorship, direct government rating, algorithm-driven filtering) would be more controversial, not because the rating system effectively measures content risk.
constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint is neither pure coordination (parents gain, but creators lose) nor pure extraction (government avoids regulation, but independent content is homogenized). It is a hybrid where voluntary industry compliance substitutes for government regulation, shifting costs from regulators to content creators while distributing benefits to large-scale producers and organized parents. The system appears consensual (industry voluntarily adopted ratings) but structural asymmetry is embedded: networks could not realistically refuse to participate without triggering direct government regulation.
constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1996_clinton_media_industry_rating_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1996_clinton_media_industry_rating_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1996_clinton_media_industry_rating_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1996_clinton_media_industry_rating_system, TR),
    TR >= 0.70.

:- end_tests(sotu_1996_clinton_media_industry_rating_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high with increasing trend. Initial launch extractiveness (0.28) reflected genuine coordination benefit: networks avoided regulation, parents gained filtering tools, creators faced compliance costs but broadcast distribution was essential. By 2021, extractiveness has risen to 0.65 because (1) broadcast networks' regulatory threat power has diminished as streaming bypasses the system, (2) the rating categories have become increasingly misaligned with content complexity, (3) independent creators face the same compliance burden but derive less benefit from broadcast access. The rising trend indicates metric substitution: the constraint's coordination function is degrading while its extraction function persists through institutional inertia. Suppression (0.48): Moderate. Independent creators face genuine barriers to exit (broadcast networks still control significant distribution and advertising reach) but streaming and cable offer partial alternatives. The suppression is not absolute — creators can choose non-broadcast distribution at the cost of reduced audience. Theater ratio (0.65): Moderate-high and rising. Initial theater (0.40) reflected the rating system's functional purpose: clear information for parental decision-making. By 2021, theater has risen to 0.68 because rating categories are coarse relative to content multidimensionality, review boards make binary decisions on dimensional content, and the system persists despite algorithmic alternatives (Netflix parental controls, YouTube age-gating) that are more granular. The rising theater indicates the constraint is becoming performative — maintained because it signals parental responsibility rather than because it effectively segments content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence because it creates a structured coalition of beneficiaries (networks, government, organized parents) against a dispersed group of victims (independent creators, unorganized viewers). The beneficiaries perceive coordination (Rope, Tangled Rope); the victims perceive extraction (Snare). The review board perceives degradation (Piton). The analytical observer perceives the constraint as a disguised regulatory mechanism that avoids constitutional exposure by converting government authority into industry self-governance. The gap is not measurement ambiguity but genuine structural asymmetry: the constraint's primary function is risk distribution (who bears the cost of content determination?), and that distribution is radically unequal.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status and exit options. Major broadcast networks are beneficiaries with arbitrage options (can exit by returning to pre-1996 unrated status, but would face regulatory pressure; low exit cost relative to benefit). Federal government is a beneficiary with arbitrage (could regulate directly but self-regulation is cheaper). Parents are beneficiaries with mobile options (can choose to use or not use V-chip, but benefit from public availability). Independent creators are victims with trapped options (cannot realistically exit broadcast distribution without abandoning primary audience). The directional asymmetry (d high for victims, d low for beneficiaries) produces the Tangled Rope classification: coordination function is real (government avoids regulation), but extraction mechanism is equally real (creators bear non-reciprocal burden). The constraint's directionality vector points from independent creators toward major networks and government, not evenly distributed.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through recognition that the constraint is genuinely hybrid. The coordination function is not illusory: parents do benefit from standardized ratings, networks do avoid direct regulation, government does achieve content policy without constitutional confrontation. But the extraction function is equally real: independent creators bear non-reciprocal compliance burden, the rating categories' functional value degrades over time, and the constraint persists through institutional inertia rather than continued utility. The constraint does not collapse into pure extraction (Snare) because the coordination benefit is structural and measurable. It does not collapse into pure coordination (Rope) because the asymmetric burden on creators and the rising theater ratio indicate extraction is embedded. Tangled Rope is the irreducible classification because both functions are present and neither is parasitic on the other. The mandatrophy would be false if we tried to argue the constraint is 'really' coordination (ignoring creator burden) or 'really' extraction (ignoring parental information benefit). Both are real. The constraint resolves mandatrophy by showing that institutional arrangements can be simultaneously beneficial and extractive, and that beneficiary perspective and victim perspective on the same constraint need not be reconcilable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rating_category_granularity,
    'Do the V-chip rating categories (Y7, PG-14, etc.) actually correspond to meaningful content risk dimensions, or do they flatten multidimensional content attributes (violence, language, sexual content, thematic intensity) into arbitrary bins?',
    'Longitudinal analysis of parental outcomes: correlation between ratings and actual parental concern; parent surveys on rating utility vs algorithmic filtering; developmental psychology research on content sensitivity by age group',
    'If categories are meaningful: theater ratio is appropriate (0.65). If categories are arbitrary: theater ratio should be higher (0.75+), indicating performative rather than functional review. Reclassification: Piton if theater > 0.70.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rating_category_granularity, empirical, 'Whether rating categories correspond to meaningful content risk dimensions').

omega_variable(
    voluntary_vs_coercive_compliance,
    'Is the industry''s adoption of self-rating genuinely voluntary, or is it functionally coercive (networks comply because direct government regulation would be more costly)?',
    'Historical counterfactual analysis: would Congress have imposed mandatory government rating absent industry self-regulation? Comparative analysis with countries using government ratings (cost-benefit to networks). Simulated network exit costs (revenue loss, regulatory retaliation).',
    'If genuinely voluntary: constraint is more Rope-like (coordination benefit outweighs enforcement cost). If functionally coercive: constraint is more Snare-like for networks (forced compliance via regulatory threat). Directionality d for ''broadcast_networks'' victim group shifts upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_vs_coercive_compliance, empirical, 'Whether industry self-rating adoption is genuinely voluntary or functionally coercive').

omega_variable(
    v_chip_uptake_vs_intended_use,
    'Do households that install V-chip blocking actually use it to enforce parental preferences, or is the device adopted for symbolic/compliance reasons (appearing to care about content filtering without active enforcement)?',
    'Household surveys on V-chip activation rates, blocking rules, and enforcement consistency; behavioral data on parental V-chip usage vs passive adoption; generational tracking of V-chip device presence vs actual use',
    'If actively used: parent benefit is substantial, coordination function is real. If symbolically adopted: parent benefit is illusory, coordination function is degraded (theater > 0.70 from parent perspective), constraint reclassifies toward Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(v_chip_uptake_vs_intended_use, empirical, 'Whether V-chip devices are actively used or symbolically adopted').

omega_variable(
    independent_creator_exit_feasibility,
    'Can independent content creators realistically exit the broadcast network distribution channel to avoid rating system compliance (e.g., through streaming, cable, niche distribution)?',
    'Market analysis of alternative distribution channels; cost-benefit of independent distribution vs broadcast compliance; trajectory of streaming adoption by content creators; revenue comparison between broadcast and non-broadcast distribution for independent creators 1996-2026',
    'If exit is feasible: ''independent content creators'' exit_options upgrade from ''trapped'' to ''constrained'' or ''mobile''; d value decreases; chi decreases; classification may shift from Snare to Tangled Rope. If exit is infeasible: Snare classification is confirmed; suppression remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(independent_creator_exit_feasibility, empirical, 'Whether independent creators can realistically exit broadcast distribution').

omega_variable(
    institutional_benefit_asymmetry,
    'Does the rating system primarily benefit large networks and major producers over independent creators (i.e., is the extracted rents concentrated on larger institutional actors)?',
    'Network-by-network analysis of compliance costs and benefits; cost data on rating infrastructure by network size; indie creator burden vs network burden; market concentration pre- and post-rating adoption',
    'If benefits concentrate on large networks: constraint is better modeled as Snare with institutional winner (network) and institutional loser (indie creator). If benefits distribute: constraint is more genuinely Tangled Rope. Reclassify ''beneficiaries'' to reflect institutional asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_benefit_asymmetry, empirical, 'Whether rating system benefits concentrate on large institutional actors').

omega_variable(
    rating_capture_by_political_factions,
    'Does the rating system become captured by one political coalition (e.g., conservative groups using ratings to restrict sexual/LGBTQ+ content, or progressive groups restricting violence), making the ostensibly neutral rating standard into an extraction mechanism for political preferences?',
    'Content analysis of rated vs unrated programs by political category; tracking of rating appeals and reversals by content type; coalition analysis of rating board composition and lobbying pressure over time',
    'If captured: constraint reclassifies from Tangled Rope to Snare (political extraction), or to Piton (theater increases as ratings become performative cover for political preference). Beneficiary group ''parents_with_filtering_access'' splits into ''politically aligned parents'' and ''politically opposed parents'', the latter becoming victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rating_capture_by_political_factions, empirical, 'Whether rating system becomes captured by political coalitions for preference extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1996_clinton_media_industry_rating_system, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tvrat_theater_launch_1996, sotu_1996_clinton_media_industry_rating_system, theater_ratio, 0, 0.4).
narrative_ontology:measurement(tvrat_theater_2001_categories_strain, sotu_1996_clinton_media_industry_rating_system, theater_ratio, 5, 0.48).
narrative_ontology:measurement(tvrat_theater_2006_cable_fragmentation, sotu_1996_clinton_media_industry_rating_system, theater_ratio, 10, 0.58).
narrative_ontology:measurement(tvrat_theater_2011_algorithm_pressure, sotu_1996_clinton_media_industry_rating_system, theater_ratio, 15, 0.62).
narrative_ontology:measurement(tvrat_theater_2016_streaming_bypass, sotu_1996_clinton_media_industry_rating_system, theater_ratio, 20, 0.65).
narrative_ontology:measurement(tvrat_theater_2021_legacy_constraint, sotu_1996_clinton_media_industry_rating_system, theater_ratio, 25, 0.68).

% Extraction over time
narrative_ontology:measurement(tvrat_extract_launch_1996, sotu_1996_clinton_media_industry_rating_system, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tvrat_extract_2001_maturation, sotu_1996_clinton_media_industry_rating_system, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(tvrat_extract_2006_streaming_pressure, sotu_1996_clinton_media_industry_rating_system, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(tvrat_extract_2011_cable_growth, sotu_1996_clinton_media_industry_rating_system, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(tvrat_extract_2016_streaming_dominance, sotu_1996_clinton_media_industry_rating_system, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(tvrat_extract_2021_legacy_broadcast, sotu_1996_clinton_media_industry_rating_system, base_extractiveness, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1996_clinton_media_industry_rating_system, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1996_clinton_media_industry_rating_system, broadcast_content_homogenization).
narrative_ontology:affects_constraint(sotu_1996_clinton_media_industry_rating_system, streaming_platform_rating_divergence).
narrative_ontology:affects_constraint(sotu_1996_clinton_media_industry_rating_system, parental_control_technology_adoption).

% DUAL FORMULATION NOTE:
% The rating system creates two structurally distinct constraints: (1) the coordination constraint on networks to standardize ratings (benefits networks and government through regulatory stability), and (2) the extraction constraint on independent creators (costs of compliance, homogenization pressure). These are not separate observables of one constraint but separate constraints in an institutional cluster. The standardization constraint (ε~0.30) has lower extractiveness because networks voluntarily benefit from coordination. The compliance constraint on creators (ε~0.65) has higher extractiveness because creators face costs without reciprocal benefit. Both are represented in this single story because they are mechanistically linked through the same institutional arrangement, but the network decomposition could split them if analysis required finer granularity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1996_clinton_media_industry_rating_system, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
