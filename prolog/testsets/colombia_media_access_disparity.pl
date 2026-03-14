% ============================================================================
% CONSTRAINT STORY: colombia_media_access_disparity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colombia_media_access_disparity, []).

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
 *   constraint_id: colombia_media_access_disparity
 *   human_readable: Colombia Media Access Disparity
 *   domain: political_economy/media_access
 *
 * SUMMARY:
 *   Colombia's media access disparity operates as a hybrid
 *   coordination-extraction mechanism that concentrates information power in
 *   urban broadcast consortiums while excluding rural, indigenous, and
 *   low-income populations from meaningful media participation. The
 *   constraint exhibits genuine coordination features (standardized
 *   Spanish-language national broadcasting) alongside asymmetric extraction
 *   (concentration of content production, gatekeeping of spectrum allocation,
 *   underrepresentation of minority populations). The disparity has both
 *   structural geographic components (infrastructure deployment costs in
 *   Andean terrain and Amazon regions) and contingent institutional
 *   components (regulatory frameworks protecting incumbent broadcasters,
 *   underinvestment in community media, absence of minority-language
 *   broadcasting mandates). Theater ratio (0.58) reflects the regulatory
 *   rituals (spectrum licensing, program approval) that maintain incumbent
 *   advantage while nominally serving public interest coordination. The
 *   constraint's extractiveness has increased over the measurement interval
 *   as digital alternatives (internet, mobile) have emerged without
 *   displacing traditional broadcast gatekeeping, suggesting that the
 *   mechanism is mutating rather than resolving.
 *
 * KEY AGENTS:
 *   - Rural Indigenous Communities: Primary victim (powerless/trapped) — geographic isolation and infrastructure absence create total confinement; no benefit from constraint structure; zero exit capacity
 *   - Afro-Colombian Populations: Primary victim (moderate/trapped) — urban concentration but systematic underrepresentation in content production; face representation gatekeeping with limited recourse
 *   - Low-Income Urban Populations: Secondary victim (moderate/constrained) — have access through mobile phones but at high cost; benefit from national news narratives but bear disproportionate access burden
 *   - Major Broadcast Consortiums: Primary beneficiary (institutional/arbitrage) — Caracol, RCN, others; capture advertising revenue and political influence through national reach; geographic monopoly protection
 *   - Bogota Urban Elites: Secondary beneficiary (institutional/arbitrage) — primary content audience; benefit from programming targeted to their demographics and consumption patterns
 *   - Community Radio Movements: Organized actors (organized/constrained) — NGOs, indigenous broadcasting initiatives, digital access programs; perceive constraint as temporary with exit pathways through regulatory opening
 *   - National Media Authority (Autoridad Nacional de Televisión): Institutional regulator (institutional/arbitrage) — maintains spectrum allocation and licensing frameworks; sees own mandate as increasingly theatrical as digital alternatives emerge
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy-contingent arrangements (spectrum gatekeeping, infrastructure underinvestment) as geographic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colombia_media_access_disparity, 0.52).
domain_priors:suppression_score(colombia_media_access_disparity, 0.68).
domain_priors:theater_ratio(colombia_media_access_disparity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colombia_media_access_disparity, extractiveness, 0.52).
narrative_ontology:constraint_metric(colombia_media_access_disparity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(colombia_media_access_disparity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colombia_media_access_disparity, tangled_rope).
narrative_ontology:human_readable(colombia_media_access_disparity, "Colombia Media Access Disparity").
narrative_ontology:topic_domain(colombia_media_access_disparity, "political_economy/media_access").

domain_priors:requires_active_enforcement(colombia_media_access_disparity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colombia_media_access_disparity, major_broadcast_consortiums).
narrative_ontology:constraint_beneficiary(colombia_media_access_disparity, bogota_urban_elites).
narrative_ontology:constraint_beneficiary(colombia_media_access_disparity, spanish_language_content_producers).
narrative_ontology:constraint_victim(colombia_media_access_disparity, rural_indigenous_communities).
narrative_ontology:constraint_victim(colombia_media_access_disparity, afro_colombian_populations).
narrative_ontology:constraint_victim(colombia_media_access_disparity, low_income_urban_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL INDIGENOUS COMMUNITIES (SNARE) — Geographic isolation, lack of electricity infrastructure in remote regions, and absence of indigenous-language broadcasting create structural confinement. These communities cannot exit or organize collective response. They bear full cost of information asymmetry while gaining no benefit from the media constraint structure. Maximum extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(colombia_media_access_disparity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME URBAN POPULATIONS (TANGLED ROPE) — Access exists but at high cost; mobile phones enable limited access to radio and streaming but data costs are prohibitive. These populations participate in media consumption but on constrained terms. Mixed coordination-extraction: they benefit from shared national news narratives but bear disproportionate cost of access and are underrepresented in content production. Exit requires resource outlay (devices, connectivity) that most cannot afford.
constraint_indexing:constraint_classification(colombia_media_access_disparity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR BROADCAST CONSORTIUMS (ROPE) — Perceive the constraint as coordination mechanism: standardized Spanish-language broadcasting coordinates national discourse and enables advertising reach. They benefit from geographic monopoly and regulatory protection. Exit is available through arbitrage — they can shift resources to streaming platforms or international markets. Experiences the constraint as solving a collective coordination problem (national media standard) with net benefit.
constraint_indexing:constraint_classification(colombia_media_access_disparity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COMMUNITY RADIO AND DIGITAL ACCESS MOVEMENTS (SCAFFOLD) — Organized actors (NGOs, community radio initiatives, internet access programs) perceive the media disparity as a temporary coordination failure with a sunset clause. Initiatives like Plan Vive Digital targeting rural connectivity and community broadcasting licenses represent alternative pathways with genuine sunset logic. As mobile coverage expands and regulatory frameworks open to community licenses, the traditional broadcast monopoly's extraction force diminishes. High agency and clear exit path.
constraint_indexing:constraint_classification(colombia_media_access_disparity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NATIONAL BROADCAST REGULATORY AUTHORITY (PITON) — The regulator maintains spectrum allocation and licensing frameworks that nominally serve coordination (allocating scarce radio frequencies fairly) but largely function as theatrical enforcement of incumbent advantage. The regulatory ritual persists through institutional inertia despite erosion of its coordination rationale — digital media and internet platforms make traditional spectrum scarcity a decreasing concern. The regulator views its own mandate as partially degraded, maintained because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(colombia_media_access_disparity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / GEOGRAPHIC DETERMINISM VIEW (MOUNTAIN) — From a civilizational perspective, Colombia's media disparity appears as a natural consequence of geography: Andean terrain and Amazon remoteness create inherent barriers to signal propagation and infrastructure deployment. This perspective naturalizes infrastructure costs as immutable physical limits. However, the structural data contradicts this classification — the constraint's extractiveness and suppression reflect policy choices (spectrum allocation, licensing regimes, infrastructure investment) more than geography. The engine will identify this as a false summit, revealing that geographic framing naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(colombia_media_access_disparity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colombia_media_access_disparity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colombia_media_access_disparity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colombia_media_access_disparity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colombia_media_access_disparity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colombia_media_access_disparity, TR),
    TR >= 0.70.

:- end_tests(colombia_media_access_disparity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The original research group (major broadcast consortiums) captures substantial benefits from geographic and regulatory concentration: national advertising reach, political influence through editorial control, and barrier protection against competitors. However, the extraction is not total (0.66+) because mobile/internet alternatives are emerging, and regulatory pressure for community licensing is building. The measurement trend (0.38 → 0.52 over interval) reflects that digital alternatives create new gatekeeping mechanisms (data pricing, platform algorithms) that partially replicate broadcast extraction rather than eliminating it. Suppression (0.68): High. Multiple reinforcing barriers prevent exit or alternative media formation: geographic infrastructure costs, regulatory gatekeeping of spectrum, high content production costs, and absence of public broadcasting investment. These barriers are both structural (terrain) and institutional (policy). Theater ratio (0.58): Moderate-high. Regulatory frameworks (spectrum licensing, program approval boards) perform coordination functions (allocating scarce frequencies fairly, ensuring content standards) but increasingly function as theatrical enforcement of incumbent advantage as digital abundance makes spectrum scarcity less salient. The trend (0.48 → 0.58) indicates growing theater ratio as regulatory processes maintain incumbent protection despite eroding functional rationale.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests structurally distinct classes depending on observer position. The beneficiary sees beneficial coordination (rope); the victim sees total extraction (snare); the organized reformer sees a solvable problem (scaffold); the degraded institution sees its own ritualized theater (piton); the civilizational analyst risks false naturalization (mountain). These are not measurement noise or contextual variations — they reflect real structural differences in how the constraint operates for different agents. The gap is diagnostic: it reveals that the constraint's primary function (broadcast coordination) is asymmetrically distributed relative to its extraction effects (concentrated gatekeeping).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position: beneficiary/victim status combined with exit options and power level. Broadcast consortiums have high power (institutional), benefit from the constraint, and have exit options (arbitrage to streaming, international expansion) — they experience low d (~0.15), producing negative effective extraction chi. Rural indigenous communities are victims (powerless, trapped) with zero exit capacity — they experience high d (~0.95), producing maximum chi. Low-income urban populations are partial victims with constrained exits (high costs to participate, limited production opportunities) — they experience moderate-high d (~0.65), producing moderate chi. Organized movements have power (organized) and clear exit pathways (alternative regulatory frameworks, technology adoption) — they experience low-moderate d (~0.40), producing moderate chi. The regulatory authority has institutional power but is partially captured by incumbent beneficiaries, creating ambiguity in d (~0.25-0.35 range without override).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all six types are legitimate reads from different structural positions, but the analytical observer's mountain classification is a false summit. Geographic barriers (terrain, distance) are real structural facts, but they do not explain the media access disparity. Countries with similar or worse geography (Papua New Guinea, Bolivia) maintain higher media pluralism through different regulatory regimes. The constraint's extractiveness and suppression reflect policy choices: spectrum allocation regimes, licensing requirements, infrastructure investment patterns, and content production gatekeeping. These choices appear natural only from the beneficiary's perspective, where the constraint solves a real coordination problem (how to broadcast nationally in a geographically dispersed country). From the victim's perspective, these same choices are extractive — they concentrate power in ways that geography alone does not require. The mandatrophy is stable: the constraint is tangled rope (mixed coordination and extraction) with different perceptual framings reflecting real structural asymmetries, not observer bias.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_investment_prioritization,
    'How much of the rural media access disparity reflects geographic cost barriers versus policy underinvestment in non-urban infrastructure?',
    'Comparison of infrastructure deployment timelines and budgets across regions with similar terrain; analysis of regulatory barrier effects on private sector rural deployment; cost-benefit analysis of alternative distribution technologies (satellite, mesh networks) relative to incumbent investment patterns.',
    'If primarily geographic: constraint classification shifts toward mountain-like immutability, reducing assigned mandatrophy responsibility. If primarily policy: constraint is contingent institutional arrangement; mandatrophy assignment remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_investment_prioritization, empirical, 'Geographic versus policy drivers of rural media disparity').

omega_variable(
    linguistic_content_production_capacity,
    'Is the absence of indigenous-language broadcasting due to insufficient audience scale (coordination problem) or to extractive gatekeeping by production consortiums?',
    'Analysis of production cost curves for low-audience indigenous-language content; examination of regulatory permission for community-produced indigenous broadcasting; historical comparison with jurisdictions that support minority-language media.',
    'If coordination problem: tangled rope classification may require rebalancing toward rope (genuine coordination function predominates). If gatekeeping: classification reinforces snare perception for indigenous communities; mandatrophy increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_content_production_capacity, empirical, 'Audience scale versus gatekeeping in indigenous-language media production').

omega_variable(
    mobile_connectivity_substitution_effect,
    'As mobile phone penetration and internet coverage expand, does traditional broadcast media retain extractive capacity or does substitution genuinely reduce the constraint''s force?',
    'Longitudinal tracking of mobile vs broadcast consumption patterns; analysis of content reach and engagement metrics across platforms; examination of whether mobile expansion opens new extractive mechanisms (data pricing, platform gatekeeping) that replicate or exceed traditional broadcast extraction.',
    'If genuine substitution: scaffold perspective confirmed; sunset logic is structural. If extractive mechanism replicates: constraint mutates rather than resolves; new form emerges (platform dependency replacing broadcast dependency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mobile_connectivity_substitution_effect, empirical, 'Whether mobile connectivity substitutes for or replicates broadcast media extraction').

omega_variable(
    regulatory_capture_depth,
    'To what extent are media regulatory bodies captured by incumbent broadcasters versus independent regulators serving public interest goals?',
    'Analysis of regulatory decision patterns; funding source transparency for regulatory personnel; comparison of regulatory stance across administrations with different political affiliations; examination of public interest mandate enforcement frequency.',
    'If captured: institutional perspective shows identity_locked dynamics (regulator''s identity fused with incumbent interests); directionality overrides required. If independent: institutional perspective classifies as constrained rather than arbitrage; whole perspectival structure shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Regulatory capture versus independence in media authority').

omega_variable(
    afro_colombian_content_representation,
    'Is the underrepresentation of Afro-Colombian content due to insufficient production investment (coordination failure) or to explicit cultural gatekeeping by programming executives?',
    'Comparison of Afro-Colombian population share versus media representation across content categories; analysis of production studio ownership and decision-making demographics; historical examination of regulatory advocacy for representation requirements.',
    'If coordination failure: suggests rope-like features with market-driven solution pathway. If gatekeeping: reinforces snare/tangled-rope classification; mandatrophy indicates deliberate extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(afro_colombian_content_representation, empirical, 'Investment disparity versus cultural gatekeeping in Afro-Colombian media representation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colombia_media_access_disparity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colo_tr_t0, colombia_media_access_disparity, theater_ratio, 0, 0.48).
narrative_ontology:measurement(colo_tr_t5, colombia_media_access_disparity, theater_ratio, 5, 0.54).
narrative_ontology:measurement(colo_tr_t10, colombia_media_access_disparity, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(colo_be_t0, colombia_media_access_disparity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(colo_be_t5, colombia_media_access_disparity, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(colo_be_t10, colombia_media_access_disparity, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colombia_media_access_disparity, information_standard).
narrative_ontology:boltzmann_floor_override(colombia_media_access_disparity, 0.12).
narrative_ontology:affects_constraint(colombia_media_access_disparity, colombia_political_discourse_concentration).
narrative_ontology:affects_constraint(colombia_media_access_disparity, indigenous_language_preservation).

% DUAL FORMULATION NOTE:
% This constraint is upstream of political discourse concentration (broadcast media gatekeeping affects which political narratives reach audiences) and downstream of indigenous language preservation (media access disparity is both cause and effect of language maintenance barriers). Decomposition follows the ε-invariance principle: broadcast coordination (ε~0.40, rope-like) is structurally distinct from advertising extraction (ε~0.55, snare-like), but the stories are unified here because the beneficiary (broadcast consortiums) is the same. If regulatory mandates separate advertising from public interest broadcasting, decomposition into separate stories would be warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colombia_media_access_disparity, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
