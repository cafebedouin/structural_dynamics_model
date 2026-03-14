% ============================================================================
% CONSTRAINT STORY: creative_output_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creative_output_capacity, []).

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
 *   constraint_id: creative_output_capacity
 *   human_readable: Creative Output Capacity Constraint
 *   domain: cognitive_economics/cultural_production
 *
 * SUMMARY:
 *   Creative output capacity constraints emerge from the structural mismatch
 *   between the abundance of creative production and the scarcity of audience
 *   attention, distribution channels, and legitimizing institutions.
 *   Individual creators possess capacity to produce content but lack
 *   mechanisms to reach audiences at scale without institutional
 *   intermediaries (publishers, galleries, labels, streaming platforms,
 *   journals). This constraint operates through gatekeeping — institutions
 *   selectively approve and amplify certain outputs while suppressing others,
 *   justifying this as necessary curation and discovery assistance. The
 *   constraint exhibits tangled rope properties: genuine coordination
 *   function (matching creators with audiences, providing technical
 *   infrastructure, managing quality assessment) layered with significant
 *   extraction (institutional margin-taking, creative control limitations,
 *   asymmetric benefits to institutional actors). The extractiveness has
 *   declined over the measurement interval (0.65 to 0.52) as alternative
 *   distribution technologies matured (Patreon, Substack, YouTube, TikTok,
 *   decentralized platforms). Theater ratio has increased (0.42 to 0.55) as
 *   traditional credentialing institutions have declined in gatekeeping
 *   function but persist through institutional inertia and status signaling.
 *
 * KEY AGENTS:
 *   - Individual Creators: Primary victims (powerless/trapped or moderate/constrained depending on career stage) — bear extraction through institutional margin-taking, creative control limitations, and requirement to conform output to institutional demand signals
 *   - Institutional Gatekeepers (Publishers, Labels, Galleries, Studios): Primary beneficiaries (institutional/arbitrage) — capture margin on distribution, legitimacy provision, and audience curation; benefit from creator output without bearing production costs
 *   - Distribution Networks (Streaming platforms, digital retailers): Secondary beneficiaries (institutional/arbitrage) — provide technical infrastructure; extract through transaction fees and algorithmic amplification control
 *   - Alternative Infrastructure (Patreon, Substack, cooperatives, decentralized platforms): Organized agents (organized/constrained) — building parallel pathways with lower extraction and explicit focus on direct creator-audience relationships
 *   - Legacy Credentialing System (MFA programs, degree requirements, critical establishment): Institutional actor (institutional/arbitrage) — maintains gatekeeping function through inertia and status signaling despite attenuation of practical verification role
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the institutional gatekeeping arrangement as inherent to attention scarcity rather than recognizing it as a contingent institutional form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creative_output_capacity, 0.52).
domain_priors:suppression_score(creative_output_capacity, 0.48).
domain_priors:theater_ratio(creative_output_capacity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creative_output_capacity, extractiveness, 0.52).
narrative_ontology:constraint_metric(creative_output_capacity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(creative_output_capacity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creative_output_capacity, tangled_rope).
narrative_ontology:human_readable(creative_output_capacity, "Creative Output Capacity Constraint").
narrative_ontology:topic_domain(creative_output_capacity, "cognitive_economics/cultural_production").

domain_priors:requires_active_enforcement(creative_output_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creative_output_capacity, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(creative_output_capacity, distribution_networks).
narrative_ontology:constraint_victim(creative_output_capacity, individual_creators).
narrative_ontology:constraint_victim(creative_output_capacity, creative_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Individual with creative capacity but no distribution channel faces institutional gatekeeping. Career depends on institutional approval (publishing, recording label, gallery representation). Economic survival requires conforming output to institutional demand signals. No viable alternative distribution until network effects reach critical mass. Maximum extraction experienced — creative vision subordinated to institutional preferences.
constraint_indexing:constraint_classification(creative_output_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER CREATOR (TANGLED ROPE) — Established creator with mixed relationship to gatekeeping constraint. Institutional framework provides legitimacy, funding, and audience reach. Genuine coordination function: platforms enable discovery, marketing, and revenue sharing. Simultaneously bears extraction: institutional cuts, creative control limitations, algorithmic curation preferences. Can build independent audience but at high opportunity cost and loss of platform reach.
constraint_indexing:constraint_classification(creative_output_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DISTRIBUTION INSTITUTION (ROPE) — Publisher, streaming platform, gallery, or label experiences constraint as pure coordination problem: matching creators with audiences, providing technical infrastructure, managing discovery in oversupply conditions. Benefits from creator output without bearing production costs. Transaction fees, margin, or equity extraction justified as coordination overhead. Exit options abundant — can shift focus to different content categories, different markets, or different creator pools.
constraint_indexing:constraint_classification(creative_output_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE INFRASTRUCTURE MOVEMENT (SCAFFOLD) — Decentralized platforms, cooperative distribution networks, and creator unions are building parallel pathways: Patreon, Substack, community-owned streaming, artist collectives. Temporary coordination architecture with explicit sunset: as technology matures and network effects favor alternatives, traditional gatekeeping extracts less (creators have genuine exit options). Low theater — alternative infrastructure emphasizes direct creator-audience connection. Suppression declining as alternatives mature.
constraint_indexing:constraint_classification(creative_output_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CREDENTIALING SYSTEM (PITON) — Institutional gatekeeping (degree requirements, MFA programs, union membership, critical establishment recognition) persists despite attenuation of functional value. Social media and algorithmic discovery have reduced informational role of traditional credentialing. System maintains itself through institutional inertia and status signaling rather than genuine filtering function. Theater ratio high: credentialing rituals persist as identity markers after their practical verification function has degraded. Prestige institutions still extract benefits through tuition and reputation licensing despite reduced gate-keeping power.
constraint_indexing:constraint_classification(creative_output_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SCARCITY VIEW (MOUNTAIN) — From a civilizational perspective, the creative output capacity constraint appears inherent to human attention economics: finite audience attention creates inevitable scarcity, and some gatekeeping mechanism is required to allocate attention across infinite potential outputs. This perspective naturalizes the constraint as an immutable property of human cognition and market structure. However, the structural data contradicts mountain classification — alternative gatekeeping mechanisms (algorithmic curation, peer recommendation, community voting) demonstrate that the specific institutional form is contingent, not natural.
constraint_indexing:constraint_classification(creative_output_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creative_output_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creative_output_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creative_output_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creative_output_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creative_output_capacity, TR),
    TR >= 0.70.

:- end_tests(creative_output_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, declining. The constraint extracts significant value from creators (margin, creative control subordination, conformity pressure) but less than pure snare because institutional platforms do provide genuine coordination services (discovery, distribution, audience building). The declining trajectory reflects maturation of alternative distribution technologies that provide some of these services without institutional extraction. Suppression (0.48): Moderate. Significant barriers exist to independent distribution (network effects favor established platforms, algorithmic reach advantage for institutional content, audience discovery difficulty) but are not insurmountable — alternative platforms exist and some creators successfully exit institutional channels. Theater ratio (0.55): Moderate-high, increasing. Credentialing institutions (MFA programs, journal peer review, critical establishment recognition) increasingly operate as status markers rather than functional gatekeeping — their informational role has declined as algorithmic discovery and social proof have replaced institutional validation. The theater has increased because institutions persist through inertia despite reduced function. Claimed type (tangled_rope): Genuine coordination function (matching, infrastructure, discovery) combines with significant asymmetric extraction (margin, control, audience subordination).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival divergence. Institutional gatekeepers see pure coordination (rope) — they frame their role as curation assistance and discovery infrastructure. Alternative infrastructure advocates see a temporary problem being solved (scaffold) — as technology matures, creators gain genuine exit options. Legacy credentialing sees its own degraded ritual (piton) — prestige and status persistence despite reduced gatekeeping function. Mid-career creators see mixed experience (tangled rope) — institutional support enables reach and legitimacy but extracts significant margin and creative control. Emerging creators see extraction with no exit (snare) — career survival depends on institutional approval, output must conform to institutional preferences. The analytical civilizational observer risks seeing inherent scarcity (mountain) but structural data reveals this as naturalization of a contingent institutional form.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position in the extraction flow. Institutional gatekeepers experience low d (beneficiary status with arbitrage exit options) → negative or low χ (constraint benefits them). Individual creators experience high d (victim status with trap/constrain exit options) → high χ (constraint extracts from them). The piton classification reflects high theater ratio rather than high extraction — the credentialing system persists through status maintenance after its gatekeeping function has atrophied. The scaffold classification reflects organized agents with structured exit pathways (alternative platforms reaching network effects maturity) and declining suppression over time.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing between contingent institutional arrangement (gatekeeping by specific institutions) and potentially inherent structural property (some form of selection from infinite potential outputs). The mountain perspective naturalizes gatekeeping as inherent scarcity when the actual constraint is institutional — alternative gatekeeping mechanisms (algorithmic, peer-driven, market-driven) demonstrate that scarcity of attention can be allocated without institutional gatekeeping institutions. The constraint's true nature is tangled rope (mixed coordination and extraction through specific institutional forms) not mountain (inherent scarcity). The materialization of alternative distribution mechanisms (Patreon, Substack, decentralized platforms) provides the empirical evidence that the institutional gatekeeping form is contingent rather than natural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attention_scarcity_or_institutional,
    'Is the capacity constraint inherent to human attention limits or artificially maintained by institutional gatekeeping structures?',
    'Comparative analysis of content discovery mechanisms: institutional vs algorithmic vs peer-driven. Measurement of audience attention allocation with and without institutional filters.',
    'If inherent: constraint is closer to Mountain (scarcity is natural law). If institutional: constraint is closer to Snare/Tangled Rope (extraction mechanism is contingent). If hybrid: split into separate stories (scarcity of attention vs institutional allocation of attention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_scarcity_or_institutional, conceptual, 'Whether capacity constraint is inherent to attention or maintained institutionally').

omega_variable(
    alternative_distribution_viability,
    'Can alternative distribution networks (Patreon, Substack, decentralized platforms) achieve parity with traditional institutional gatekeeping in creator revenue and audience reach?',
    'Longitudinal data on creator earnings, audience size, and reach distribution across institutional vs alternative platforms. Analysis of network effects (whether creator concentration increases or declines with platform maturity).',
    'If viable parity achieved: scaffold sunset is real, suppression declining, snare classification converts to rope for creators with access to alternatives. If perpetual dominance: institutional gatekeeping is structurally entrenched, snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_distribution_viability, empirical, 'Whether alternative distribution can achieve parity with institutional gatekeeping').

omega_variable(
    creator_identity_fusion,
    'To what extent is creator attachment to institutional gatekeeping (degree requirements, publishing prestige, critical establishment recognition) driven by career necessity vs internalized identity fusion with institutional validation?',
    'Comparative study of creators with and without institutional credentials; analysis of career outcomes for self-taught vs credentialed creators; measurement of psychological distress among rejected creators (persistence of distress post-alternative success indicates identity fusion).',
    'If primarily necessity: exit_options shift to mobile/constrained as alternatives mature (scaffold perspective). If primarily identity fusion: exit_options remain trapped even with viable alternatives (identity_locked classification at biographical horizon; snare classification persists despite structural mobility).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_identity_fusion, empirical, 'Degree of identity fusion with institutional gatekeeping vs material necessity').

omega_variable(
    algorithmic_curation_opacity,
    'Does algorithmic curation by platforms constitute a new extraction mechanism that merely replaces institutional gatekeeping with opaque algorithmic gatekeeping?',
    'Analysis of algorithmic preference patterns: measurement of bias toward certain creator types, content categories, or aesthetics. Comparison of creator visibility distribution and earnings concentration across algorithmic vs human-curated platforms.',
    'If algorithmic curation is neutral: scaffold perspective is validated, theatrical element reduced, exit to alternatives is real relief. If algorithmic curation recreates extraction pattern: constraint transforms rather than dissolves, piton or new snare emerges at technical layer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_curation_opacity, empirical, 'Whether algorithmic curation replaces institutional extraction or reduces it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creative_output_capacity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(creap_tr_t0, creative_output_capacity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(creap_tr_t10, creative_output_capacity, theater_ratio, 10, 0.48).
narrative_ontology:measurement(creap_tr_t20, creative_output_capacity, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(creap_be_t0, creative_output_capacity, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(creap_be_t10, creative_output_capacity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(creap_be_t20, creative_output_capacity, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creative_output_capacity, resource_allocation).
narrative_ontology:affects_constraint(creative_output_capacity, attention_scarcity_constraint).
narrative_ontology:affects_constraint(creative_output_capacity, institutional_legitimacy_gatekeeping).

% DUAL FORMULATION NOTE:
% Creative output capacity decomposes into two structurally distinct constraints: (1) attention_scarcity_constraint (ε≈0.15, mountain) — human attention is finite, some mechanism must allocate it; (2) institutional_legitimacy_gatekeeping (ε≈0.52, tangled rope) — specific institutional forms monopolize the allocation mechanism and extract margin. These have different ε values and different causal dynamics. The current story addresses the institutional constraint; decomposition would isolate the potential scarcity-of-attention component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
