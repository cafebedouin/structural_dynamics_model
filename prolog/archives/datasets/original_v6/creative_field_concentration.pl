% ============================================================================
% CONSTRAINT STORY: creative_field_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creative_field_concentration, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: creative_field_concentration
 *   human_readable: Creative Field Concentration and Systemic Talent Extraction
 *   domain: cultural_economy/creative_labor
 *
 * SUMMARY:
 *   Creative field concentration describes the structural phenomenon where
 *   access to creative audiences, funding, and viability flows through a
 *   shrinking set of platform gatekeepers and capital-concentrated producers.
 *   Emerging creators face systematically increasing barriers to independent
 *   sustainability: portfolio requirements demand unpaid work, network access
 *   requires existing social capital, and algorithmic amplification requires
 *   platform participation with asymmetric rights extraction. The constraint
 *   exhibits tangled coordination-extraction hybrid structure: platforms
 *   solve a genuine coordination problem (matching diverse creators with
 *   differentiated audiences) while simultaneously extracting excess value
 *   through rights consolidation, algorithmic precarity, and talent
 *   bottlenecking. The suppression (0.62) reflects barriers that appear low
 *   (low barrier to entry — anyone can upload) but are actually high (high
 *   barrier to sustainability without platform dependence). Theater ratio
 *   (0.68) captures that cultural institutions (arts councils, academies,
 *   critics) maintain gatekeeping authority despite eroding functional
 *   necessity, increasingly operating as performative credentialing divorced
 *   from market validation.
 *
 * KEY AGENTS:
 *   - Emerging Creators: Primary victims (powerless/trapped) — face unpaid apprenticeship, rights extraction, algorithmic precarity with no viable exit
 *   - Creative Labor Market: Aggregate victim (powerless/trapped at generational scope) — systematic extraction of creative value; low barrier to entry masks high barrier to viability
 *   - Platform Gatekeepers: Primary beneficiaries (institutional/arbitrage) — capture attention routing, audience data, rights leverage; experience constraint as pure coordination
 *   - Established Creators: Secondary beneficiaries/mixed (powerful/mobile) — benefited from early platform access; now experience mixed coordination (distribution access) and extraction (algorithmic precarity, rights extraction)
 *   - Creative Worker Coalition: Organized actors (organized/constrained) — unions, guilds, cooperatives building alternative models with sunset logic; constrained by institutional resistance
 *   - Legacy Arts Institutions: Secondary beneficiaries (institutional/arbitrage) — gatekeeping role persists through inertia; declining functional necessity but maintaining prestige authority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent platform economics as inherent Pareto distribution law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creative_field_concentration, 0.58).
domain_priors:suppression_score(creative_field_concentration, 0.62).
domain_priors:theater_ratio(creative_field_concentration, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creative_field_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(creative_field_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(creative_field_concentration, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creative_field_concentration, tangled_rope).
narrative_ontology:human_readable(creative_field_concentration, "Creative Field Concentration and Systemic Talent Extraction").
narrative_ontology:topic_domain(creative_field_concentration, "cultural_economy/creative_labor").

domain_priors:requires_active_enforcement(creative_field_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creative_field_concentration, platform_gatekeepers).
narrative_ontology:constraint_beneficiary(creative_field_concentration, capital_concentrated_producers).
narrative_ontology:constraint_beneficiary(creative_field_concentration, cultural_intermediaries).
narrative_ontology:constraint_victim(creative_field_concentration, emerging_creators).
narrative_ontology:constraint_victim(creative_field_concentration, creative_labor_market).
narrative_ontology:constraint_victim(creative_field_concentration, cultural_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Structurally trapped by gatekeeping requirements (portfolio, network access, capital for tools/distribution). Faces maximum extraction: unpaid exposure work, rights assignment, algorithmic precarity. No viable exit without abandoning creative aspiration. Bears full suppression — barriers are material (market access, algorithm control) and psychological (internalized gatekeeping narratives).
constraint_indexing:constraint_classification(creative_field_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREATIVE LABOR FORCE (SNARE) — Aggregate of emerging creators at generational scope. Structural trap: creative sector requires sustained unpaid apprenticeship (exposure work, portfolio building, network cultivation) before viability. No collective exit option; individual exit means leaving the field. Suppression is systemic: low barrier to entry (anyone can try) masks high barrier to sustainability (only well-capitalized can afford the extraction period).
constraint_indexing:constraint_classification(creative_field_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM GATEKEEPER (ROPE) — Institutional beneficiary with arbitrage exit. Experiences the constraint as pure coordination: aggregating creator talent, matching audiences with content, managing attention allocation. Net beneficiary position — extraction runs toward this agent. Can exit (license alternative creators, shift platform model) at acceptable cost. From this perspective, the constraint solves a genuine coordination problem: which creators reach which audiences?
constraint_indexing:constraint_classification(creative_field_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED CREATOR (TANGLED ROPE) — Individual creator who survived the extraction period and achieved platform prominence. Experiences mixed coordination and extraction: benefits from platform distribution (coordination function) while bearing ongoing rights extraction and algorithmic precarity (asymmetric extraction). Mobile exit option (can shift platforms, self-publish) but switching costs are high due to audience lock-in. Moderate extraction relative to powerless peers because they have agency and leverage.
constraint_indexing:constraint_classification(creative_field_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: CREATIVE WORKER COALITION (SCAFFOLD) — Organized agents (unions, guilds, professional associations) see the concentration as a temporary coordination failure with sunset logic. Alternative models emerging: direct patronage, community funding, cooperative platforms, rights reversion campaigns. Coalition has agency and sees exit paths, but facing institutional resistance and legal barriers. Constrained exit because dismantling platform concentration requires regulatory action outside the creative sector alone.
constraint_indexing:constraint_classification(creative_field_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY ARTS INSTITUTION (PITON) — Museums, academies, arts councils maintain gatekeeping roles despite eroding functional necessity. Traditional credentialing and curation now compete with algorithmic discovery and direct audience connection. The institutional role persists through inertia (established prestige, funding infrastructure, cultural authority narratives) rather than demonstrated function. Theater ratio reflects performative accreditation divorced from actual market validation.
constraint_indexing:constraint_classification(creative_field_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, talent concentration may appear to be a natural law: most fields show power-law distribution (Pareto principle, preferential attachment). Scarcity of attention is a universal structural fact. This perspective risks naturalizing what is contingent institutional arrangement (platform economics, capital requirements for tool access, algorithmic amplification) as an inherent feature of all creative systems. Engine will flag this as false summit.
constraint_indexing:constraint_classification(creative_field_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creative_field_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creative_field_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creative_field_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creative_field_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creative_field_concentration, TR),
    TR >= 0.70.

:- end_tests(creative_field_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms extract significant value through audience routing (attention allocation asymmetry), rights consolidation, algorithmic precarity (creators cannot predict revenue), and talent concentration (only established creators achieve viability). The extraction is not total (some creators do succeed, platforms do provide distribution) but systematic — the system reliably captures more value from creator output than creators capture from their work. Measurement trajectory (0.32→0.45→0.58) reflects accelerating platform consolidation: early digital era had lower extraction (more platforms, lower capital requirements); recent consolidation has intensified extraction through algorithmic curation and network effects. Suppression (0.62): High. Multiple layered barriers: portfolio gatekeeping (requires prior work and capital), network access gatekeeping (requires existing connections), algorithmic gatekeeping (platform-controlled amplification), capital gatekeeping (professional tools require investment), and rights gatekeeping (platform terms strip reversion rights). Suppression is both structural (material barriers) and internalized (platform validation as de facto creative legitimacy). Theater ratio (0.68): High. Cultural institutions maintain gatekeeping authority (jury selection, funding eligibility, critical validation) despite reduced functional necessity — platform audiences increasingly discover and validate creators directly. The institutional curation persists through legacy prestige and funding infrastructure rather than demonstrated ability to predict or validate creative value. Trajectory (0.48→0.58→0.68) shows theater increasing as algorithmic systems increasingly replace human judgment but institutional authority persists performatively.
 *
 * PERSPECTIVAL GAP:
 *   The maximum gap exists between the Snare perspective (emerging creator experiencing maximum suppression and extraction) and the Rope perspective (platform gatekeeper experiencing the constraint as coordination). This gap reveals the extraction mechanism: platforms solve a real coordination problem (audience discovery) while simultaneously concentrating the benefits toward established creators and gatekeepers. The established creator's Tangled Rope perspective bridges these extremes — they see both coordination benefit (platform distribution) and extraction cost (algorithmic precarity, rights extraction). The organized coalition's Scaffold perspective suggests the constraint is improvable — alternative platforms, direct patronage, and cooperative models reduce the platform's extraction lever. The legacy institution's Piton perspective reveals that cultural authority persists performatively even as functional necessity erodes (algorithmic discovery increasingly replaces institutional curation). The false summit (Mountain) occurs when concentration is naturalized as 'Pareto distribution law' — this misidentifies contingent platform architecture (algorithmic amplification, network effects, rights consolidation, capital requirements) as inherent properties of creative systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) determines how much extraction each agent experiences. Platforms (beneficiaries with arbitrage exit) derive low d (near 0.0) — full beneficiary position with exit options. Emerging creators (victims with trapped exit) derive high d (near 1.0) — full target position with no exit. Established creators (powerful/mobile with mixed beneficiary-victim status) derive moderate d (around 0.50-0.60) — they have agency and exit options but face ongoing extraction from algorithmic precarity. The organized coalition (constrained exit despite organization) derives moderately elevated d reflecting their constrained ability to exit institutional structures. The legacy institutions (institutional/arbitrage like platforms) derive low d — they're not being extracted from, though their functional necessity is eroding. The sigmoid function f(d) maps these d values to experienced extractiveness multipliers: low d produces negative or minimal chi (extraction runs toward beneficiary, experienced as coordination), high d produces elevated chi (extraction runs toward target, experienced as oppressive). The platform's Rope perspective emerges because d≈0 produces f(d)≈-0.01 (institutional beneficiary position) — they experience minimal extraction and see the system as coordinating. The emerging creator's Snare perspective emerges because d≈0.95 produces f(d)≈1.42 (powerless target position) — they experience maximum extraction scaled by f(d), producing high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that platform concentration simultaneously solves a coordination problem AND extracts excess value through asymmetric accumulation. This is the defining structure of Tangled Rope: genuine coordination function (matching creators to audiences) coexists with asymmetric extraction (platform capture of attention routing, rights, and data). The system would collapse without the coordination function — creators do need audiences and would benefit from efficient matching. But the coordination is achieved through enforced platform dependence rather than competitive or cooperative mechanisms. The Snare perspective (emerging creator) shows that for powerless agents, the extraction dominates — they experience the system as pure extraction because they have no exit option and cannot benefit from the coordination unless they first survive the extraction period. The Rope perspective (platform) shows that for beneficiaries, the extraction is invisible — they experience the system as coordination because they derive all benefits and no costs. The Tangled Rope perspective (established creator) shows that the mixed structure is real: both coordination and extraction are present, but asymmetrically distributed. The mandatrophy is false if the system is merely Rope (pure coordination) — but the victims perspective shows it is not purely coordinative. The mandatrophy is false if the system is merely Snare (pure extraction) — but the platform's coordination function is real and beneficial. The actual mandatrophy is real but contingent: the coordination could be achieved through alternative architectures (cooperative platforms, guild-based distribution, direct patronage) that reduce asymmetric extraction. The constraint is Tangled Rope because the present architecture uniquely concentrates benefits toward gatekeepers while solving the coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exposure_vs_exploitation_threshold,
    'What volume and duration of unpaid exposure work crosses from legitimate skill-building into systematic labor extraction?',
    'Time-use studies comparing hours unpaid to total creative labor; skill acquisition curves; correlation between apprenticeship duration and eventual income',
    'If apprenticeship < 2 years: most creators classified as unnecessarily suppressed (snare intensifies). If apprenticeship > 5 years: extraction normalized as ''paying dues'' (suppression appears lower). Threshold determines whether the system is coordinative or extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exposure_vs_exploitation_threshold, empirical, 'Duration threshold distinguishing skill-building from systematic extraction').

omega_variable(
    capital_requirement_exogeneity,
    'Is the high capital requirement for creative tools (software, equipment, studio access) an inherent property of creativity or a contingent effect of IP/licensing regimes and platform monopolies?',
    'Historical comparison with pre-digital creative apprenticeship; analysis of creative output in high-capital vs low-capital environments; correlation between tool democratization and creator diversity',
    'If exogenous (inherent): capital gatekeeping is justified coordination tax. If contingent (policy-dependent): capital requirements are artificial suppression mechanism maintained through IP law and platform control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_requirement_exogeneity, conceptual, 'Whether capital barriers to creativity are inherent or policy-contingent').

omega_variable(
    algorithmic_concentration_inevitability,
    'Does algorithmic discovery (rather than human curation) necessarily concentrate attention on established creators, or does this concentration reflect specific algorithmic choices that could be redesigned?',
    'Comparative analysis of different algorithmic architectures; experiments with diversity-optimized ranking; historical data on attention distribution under human vs algorithmic curation',
    'If inevitable: concentration is a structural property (mountain-like). If contingent: concentration is maintained through design choices (tangled rope mechanism continues by active enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_concentration_inevitability, empirical, 'Whether algorithmic concentration reflects necessity or design choice').

omega_variable(
    network_effect_lock_in_reversibility,
    'Can creator audiences and platforms transition away from concentrated gatekeepers once alternative distribution exists, or is network lock-in effectively permanent?',
    'Case studies of successful platform transitions; analysis of audience switching costs; viral adoption rates of decentralized alternatives when functionality reaches parity',
    'If reversible: emerging creators have real exit option in future (constraints scaffold toward resolution). If permanent: lock-in is structural suppression mechanism (snare classification intensifies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_lock_in_reversibility, empirical, 'Whether network effects lock creators into concentrated platforms permanently').

omega_variable(
    identity_lock_creative_identity,
    'To what extent do emerging creators internalize the platform''s definition of creative value, making exit psychologically unthinkable even when material barriers are removed?',
    'Longitudinal tracking of creators who exit concentrated platforms; analysis of identity reframing required; comparison of success rates for creators who exit vs those who stay',
    'If identity-locked (high): even material barrier removal doesn''t enable exit (constraint more severe). If primarily material (low): barrier removal enables rapid transition (constraint more improvable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_creative_identity, empirical, 'Degree of identity fusion with platform validation systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creative_field_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crecon_tr_t0, creative_field_concentration, theater_ratio, 0, 0.48).
narrative_ontology:measurement(crecon_tr_t5, creative_field_concentration, theater_ratio, 5, 0.58).
narrative_ontology:measurement(crecon_tr_t10, creative_field_concentration, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(crecon_be_t0, creative_field_concentration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(crecon_be_t5, creative_field_concentration, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(crecon_be_t10, creative_field_concentration, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creative_field_concentration, resource_allocation).
narrative_ontology:affects_constraint(creative_field_concentration, artist_precarity_labor_extraction).
narrative_ontology:affects_constraint(creative_field_concentration, cultural_monopoly_gatekeeping).
narrative_ontology:affects_constraint(creative_field_concentration, attention_economy_winner_take_all).

% DUAL FORMULATION NOTE:
% Creative field concentration decomposes into structurally distinct constraints: (1) artist_precarity_labor_extraction focuses on the unpaid apprenticeship and labor extraction mechanisms for powerless agents; (2) cultural_monopoly_gatekeeping focuses on institutional credentialing capture; (3) attention_economy_winner_take_all focuses on algorithmic amplification. This story models the unified constraint structure spanning all three domains. Upstream constraints about platform technology (network effects, algorithmic bias) influence how field concentration operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
