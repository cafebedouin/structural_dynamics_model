% ============================================================================
% CONSTRAINT STORY: digital_language_extinction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_language_extinction, []).

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
 *   constraint_id: digital_language_extinction
 *   human_readable: Digital Language Extinction Through Platform Consolidation
 *   domain: technology/cultural_preservation/computational_linguistics
 *
 * SUMMARY:
 *   Digital language extinction represents a structural constraint where
 *   technological consolidation creates irreversible incentive asymmetries
 *   against minority languages. Platform operators benefit from concentration
 *   (data homogeneity, market efficiency, reduced localization costs).
 *   Minority language communities face suppression through platform design:
 *   no interface support, no predictive text, no voice recognition, and
 *   exclusion from the computational infrastructure that increasingly
 *   mediates economic and social participation. The constraint exhibits a
 *   genuine tangled_rope structure: platform consolidation solves a real
 *   coordination problem (how to provide global digital infrastructure
 *   efficiently) while simultaneously extracting from communities that cannot
 *   exit without abandoning digital participation. The tension between
 *   extractive platform consolidation and the communities it displaces drives
 *   the classification. Extractiveness has increased over the measurement
 *   interval from 0.28 (2010s, multiple platforms still competing) to 0.58
 *   (2020s, near-total consolidation). Theater ratio has increased from 0.35
 *   to 0.65 as language preservation efforts (UNESCO programs, community
 *   documentation, policy commitments) have become increasingly performative
 *   relative to actual reversal of extinction trends.
 *
 * KEY AGENTS:
 *   - Minority Language Speaker: Primary victim (powerless/trapped) — receives no digital support; linguistic data extracted for dominant-language AI; trapped in platform ecosystem by economic participation requirement
 *   - Linguistic Heritage Commons: Primary victim (powerless/trapped) — abstract collective good with no agent, no advocacy, no exit option; intergenerational transmission collapse occurring in real time
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — benefit from market concentration, data homogeneity for AI training, reduced localization costs; experience constraint as efficient resource allocation
 *   - AI/ML Training Data Collectors: Secondary beneficiary (institutional/arbitrage) — extract linguistic data from minority language communities for training dominant-language models
 *   - Linguist and Documentation Community: Moderate victim (moderate/constrained) — access to resources and platforms but also face extraction; significant agency but high countervailing costs
 *   - Open Language Infrastructure Coalition: Organized agents (organized/constrained) — building alternatives (Wikimedia, Mozilla, community keyboards, open-source NLP) with sunset logic
 *   - UNESCO and Policy Apparatus: Institutional actor (institutional/arbitrage) — maintains performative preservation programs; limited functional impact on extinction trends
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing technological extinction as inevitable rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_language_extinction, 0.58).
domain_priors:suppression_score(digital_language_extinction, 0.72).
domain_priors:theater_ratio(digital_language_extinction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_language_extinction, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_language_extinction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_language_extinction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_language_extinction, tangled_rope).
narrative_ontology:human_readable(digital_language_extinction, "Digital Language Extinction Through Platform Consolidation").
narrative_ontology:topic_domain(digital_language_extinction, "technology/cultural_preservation/computational_linguistics").

domain_priors:requires_active_enforcement(digital_language_extinction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_language_extinction, dominant_language_platform_operators).
narrative_ontology:constraint_beneficiary(digital_language_extinction, machine_learning_training_data_collectors).
narrative_ontology:constraint_victim(digital_language_extinction, minority_language_communities).
narrative_ontology:constraint_victim(digital_language_extinction, indigenous_linguistic_heritage).
narrative_ontology:constraint_victim(digital_language_extinction, linguistic_diversity_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY LANGUAGE SPEAKER (SNARE) — Trapped within digital platforms that offer no interface, no keyboard support, no predictive text, no voice recognition in their language. Cannot escape the digital commons without abandoning economic and social participation. Face maximal extraction: their linguistic labor (speech data, text input, social engagement) is collected for training dominant-language models, while they receive no reciprocal tool development. Zero degrees of freedom.
constraint_indexing:constraint_classification(digital_language_extinction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LINGUISTIC HERITAGE (SNARE) — Language as an abstract collective good has no agent to advocate for it and no exit option. Structured obsolescence: younger generations born into digital-first environments receive no native literacy support in minority languages, creating an intergenerational transmission collapse. This perspective models the constraint from the position of the language itself as a victim — a commons with no voice, no alternatives, no agency.
constraint_indexing:constraint_classification(digital_language_extinction, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LINGUIST/DOCUMENTATION COMMUNITY (TANGLED ROPE) — Constrained by funding scarcity and platform dependency, but also beneficiaries of the same infrastructure for data access, computational tools, and publication reach. Face extraction (platforms monetize linguistic data; funding attention flows to high-resource languages) but also coordination benefits (shared standards, open repositories, collaborative documentation platforms). Significant agency but high costs for counter-extraction work.
constraint_indexing:constraint_classification(digital_language_extinction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PLATFORM OPERATORS / AI COMPANIES (ROPE) — Experience the constraint as coordination: centralizing language support on dominant platforms solves the market coordination problem (why build multiple language interfaces when one dominant platform serves 80% of users?). Net beneficiaries through data extraction and market concentration. The constraint feels like efficient resource allocation from their position, not extraction.
constraint_indexing:constraint_classification(digital_language_extinction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN LANGUAGE INFRASTRUCTURE COALITION (SCAFFOLD) — Organized agents (Wikimedia, Mozilla, community-maintained keyboards, open-source NLP) are building alternative verification pathways (decentralized language infrastructure, community-controlled platforms, open-source models). The sunset is structured: as these alternatives mature, the platform dependency's extraction mechanism loses force. Extractiveness declining via technological substitution rather than institutional reform. Estimated sunset: 15-25 years for true alternatives to reach functional parity.
constraint_indexing:constraint_classification(digital_language_extinction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: UNESCO LANGUAGE PRESERVATION APPARATUS (PITON) — Official language preservation efforts (UNESCO Endangered Languages program, digital archiving initiatives, cultural funding) are substantially performative. Documentation projects create archival records without reversing extinction trends; funding is scattered across thousands of languages with minimal per-language impact; policy declarations lack enforcement mechanisms. The apparatus maintains theatrical legitimacy (countries sign language preservation commitments) while the structural constraint — digital platform consolidation and economic displacement — continues unopposed. Theater ratio high; functional preservation impact low.
constraint_indexing:constraint_classification(digital_language_extinction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, language extinction is an immutable feature of technological disruption: smaller languages lose speakers whenever a larger language offers better economic opportunity. Digital platforms simply accelerate a natural process. This perspective risks naturalizing what is actually a contingent institutional arrangement: it is not inevitable that platforms exclude minority languages; it is a business-model choice that digital infrastructure design reinforces. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(digital_language_extinction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_language_extinction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_language_extinction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_language_extinction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_language_extinction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_language_extinction, TR),
    TR >= 0.70.

:- end_tests(digital_language_extinction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and increasing. The platform consolidation model extracts linguistic data (speech, text, input patterns) from minority language communities who provide training data for dominant-language models. Extraction is not total because some communities maintain autonomous linguistic infrastructure and some retain cultural transmission mechanisms outside the platform ecosystem. However, the trend is extraction-increasing as digital participation becomes economically mandatory. Suppression (0.72): High. Structural barriers to minority language digital support include: platform business model (concentration on high-resource languages maximizes ROI), technical debt (legacy infrastructure assumes single-language support), network effects (speaker base too small to justify development investment), economic displacement (users migrate to dominant languages for job access), and intentional design exclusion (platforms optimize for English/Mandarin/Spanish). Psychological suppression is also high: younger generations born into digital-first environments internalize the platform-offered languages as inevitable. Theater ratio (0.65): Moderately high. Language preservation efforts — UNESCO programs, cultural funding, community documentation projects — are substantially performative. They create archival records and policy commitments without reversing extinction trends. Theater has increased as the scale of extinction has accelerated and preservation funding has remained constant or declined.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspectives (minority speaker, linguistic heritage) classify the constraint as pure extraction with no coordination benefit. The rope perspective (platform operators) experiences it as coordination. The tangled_rope perspective (linguist community) sees both extraction and coordination benefits. The scaffold perspective sees a temporary problem with technological exit paths. The piton perspective sees performative preservation efforts. The mountain perspective risks naturalizing the constraint as inevitable technological disruption. The perspectival gap reveals that the constraint's 'necessity' is contingent: it emerges from specific business model choices (concentrate on high-resource languages, monetize user data) implemented in platform architecture, not from the laws of technology itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from power position and exit options. Minority language speakers have d ≈ 0.95 (trapped, powerless victims) experiencing f(d) ≈ 1.42 (maximum experienced extraction). Platform operators have d ≈ 0.10 (institutional power, arbitrage exit, beneficiary status) experiencing f(d) ≈ -0.05 (negative extraction — they benefit). The linguistic heritage itself has d ≈ 1.0 (completely trapped, no exit, abstract victim) with f(d) ≈ 1.42 (maximum experienced extraction even though it has no consciousness to experience it). The linguist community has d ≈ 0.65 (moderate power, constrained exit, mixed beneficiary/victim status) experiencing f(d) ≈ 1.00 (moderate extraction). The open coalition has d ≈ 0.50 (organized power, constrained exit, building exit paths) with f(d) ≈ 0.65 (lower effective extraction due to agency). Scope modifier σ(S) = 1.2 for global scope, amplifying extractiveness via the chi formula. The combination produces χ = 0.58 × f(d) × 1.2, which is maximized for trapped minority speakers and minimized for beneficiary institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT AND UNRESOLVED: The constraint exhibits classic mandatrophy — the platform consolidation mechanism simultaneously solves a genuine coordination problem (efficient global infrastructure) while extracting from communities that cannot exit. At ε = 0.58, the question 'is this extraction or coordination?' cannot be answered with a single type. The rope classification (platform operators' perspective) emphasizes coordination and understates extraction. The snare classification (minority speaker perspective) emphasizes extraction and misses the coordination function. The tangled_rope classification is structurally accurate: the constraint genuinely coordinates digital infrastructure globally AND genuinely extracts from communities excluded from that infrastructure. The mandatrophy is resolved by rejecting the implicit assumption that a single type must apply across all positions. The constraint IS both extraction and coordination; the perspectival gap shows which communities benefit from coordination and which communities bear the extraction cost. Unresolved mandatrophy question: Can an alternative architecture provide equally efficient global coordination without the extraction? (If yes: constraint is snare masquerading as coordination. If no: constraint is genuine tangled_rope.) This remains empirically open.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_transmission_threshold,
    'At what point does generational language shift become irreversible within a digital platform environment?',
    'Longitudinal data on L1 acquisition rates for minority languages in digital-native populations; comparison with pre-digital intergenerational transmission patterns; threshold identification from community case studies',
    'If threshold < 2 generations: constraint exhibits rapid extinction dynamics (classify as snare rather than tangled_rope). If threshold > 4 generations: community-level language shift is reversible through intervention (increases likelihood of successful scaffold exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_threshold, empirical, 'Intergenerational transmission threshold in digital environments').

omega_variable(
    open_infrastructure_sufficiency,
    'Can decentralized open-source language infrastructure (community-maintained keyboards, open NLP models, community platforms) actually compete with centralized platforms in providing digital language support?',
    'Comparative analysis of feature parity, user experience, adoption rates, and maintenance sustainability between open and centralized platforms; case studies of successful and failed open-source language projects',
    'If sufficient: scaffold classification is justified and sunset is real. If insufficient: open infrastructure remains niche, extraction persists, and snare classification dominates for minority language speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_infrastructure_sufficiency, empirical, 'Functional sufficiency of open-source language infrastructure').

omega_variable(
    economic_displacement_versus_cultural_choice,
    'Are minority language users transitioning to dominant languages primarily due to economic incentive structures or genuine preference for the dominant language?',
    'Survey data on language preference with economic incentive held constant; controlled experiments on platform design choices and adoption decisions; ethnographic study of code-switching patterns',
    'If primarily economic: extraction mechanism is suppression via economic pressure (supports snare classification). If primarily preference: transition is voluntary coordination (supports rope classification). If mixed: tangled_rope is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_displacement_versus_cultural_choice, empirical, 'Economic displacement versus cultural language choice').

omega_variable(
    data_extraction_visibility,
    'Are minority language speakers aware that their linguistic data is being extracted for training dominant-language AI models?',
    'Survey data on awareness of data practices; analysis of platform privacy policies and their accessibility to low-resource language communities; ethnographic study of informed consent understanding',
    'If low awareness: suppression operates through opacity; classification remains snare. If high awareness: communities may organize for countervailing power; potentially shifts to tangled_rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_extraction_visibility, empirical, 'Minority speaker awareness of linguistic data extraction').

omega_variable(
    platform_dependency_irreversibility,
    'Is digital platform dependency for minority language communities a structurally reversible condition (Scaffold) or an irreversible structural lock-in (Snare)?',
    'Historical analysis of cases where minority communities successfully exited platform dependency; modeling of switching costs and network effects; identification of technological or institutional interventions that enable exit',
    'If reversible: scaffold logic holds, open infrastructure alternatives can succeed. If irreversible: lock-in is structural, snare classification dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_dependency_irreversibility, conceptual, 'Reversibility of platform dependency for minority languages').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_language_extinction, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dle_tr_t0, digital_language_extinction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dle_tr_t3, digital_language_extinction, theater_ratio, 3, 0.48).
narrative_ontology:measurement(dle_tr_t6, digital_language_extinction, theater_ratio, 6, 0.6).
narrative_ontology:measurement(dle_tr_t9, digital_language_extinction, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(dle_be_t0, digital_language_extinction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dle_be_t3, digital_language_extinction, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(dle_be_t6, digital_language_extinction, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(dle_be_t9, digital_language_extinction, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_language_extinction, global_infrastructure).
narrative_ontology:boltzmann_floor_override(digital_language_extinction, 0.2).
narrative_ontology:affects_constraint(digital_language_extinction, ai_training_data_extraction).
narrative_ontology:affects_constraint(digital_language_extinction, platform_network_effects_lock_in).
narrative_ontology:affects_constraint(digital_language_extinction, intergenerational_language_shift).

% DUAL FORMULATION NOTE:
% Digital language extinction is downstream of platform consolidation (which has its own ε value reflecting network effects and market concentration dynamics) but represents a distinct structural constraint on linguistic communities. Upstream: network effect lock-in and business model concentration. This story: direct linguistic community impact and intergenerational transmission collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_language_extinction, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
