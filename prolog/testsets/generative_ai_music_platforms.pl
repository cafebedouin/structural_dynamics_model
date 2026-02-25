% ============================================================================
% CONSTRAINT STORY: generative_ai_music_platforms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generative_ai_music_platforms, []).

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
 *   constraint_id: generative_ai_music_platforms
 *   human_readable: Generative AI Music Creation Platforms
 *   domain: technological/economic/cultural
 *
 * SUMMARY:
 *   Major technology companies are integrating generative AI models for music
 *   creation directly into their flagship platforms and operating systems.
 *   This provides users with powerful tools to generate music from text
 *   prompts, effectively 'democratizing' music production. However, this
 *   system also functions as a powerful extractive engine. It devalues the
 *   labor of professional musicians, leverages the creative input of
 *   hobbyists to train proprietary models, and centralizes control over a new
 *   mode of cultural production within a few dominant ecosystems.
 *
 * KEY AGENTS:
 *   - Tech Platforms (Google, Apple): Primary beneficiary (institutional/arbitrage) - Control the models, distribution, and data.
 *   - Professional Musicians: Primary victim (organized/constrained) - Face skill devaluation and market flooding.
 *   - Hobbyist Creators: Dual role as beneficiary/victim (powerless/trapped) - Gain tools but surrender data and potential ownership.
 *   - Legacy Music Industry: Inter-institutional actor (institutional/constrained -> arbitrage) - Transitions from victim of disruption to beneficiary through partnership.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generative_ai_music_platforms, 0.65).
domain_priors:suppression_score(generative_ai_music_platforms, 0.75).
domain_priors:theater_ratio(generative_ai_music_platforms, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generative_ai_music_platforms, extractiveness, 0.65).
narrative_ontology:constraint_metric(generative_ai_music_platforms, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(generative_ai_music_platforms, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generative_ai_music_platforms, tangled_rope).
narrative_ontology:human_readable(generative_ai_music_platforms, "Generative AI Music Creation Platforms").
narrative_ontology:topic_domain(generative_ai_music_platforms, "technological/economic/cultural").

domain_priors:requires_active_enforcement(generative_ai_music_platforms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generative_ai_music_platforms, tech_platforms).
narrative_ontology:constraint_beneficiary(generative_ai_music_platforms, hobbyist_creators).
narrative_ontology:constraint_beneficiary(generative_ai_music_platforms, end_consumers).
narrative_ontology:constraint_victim(generative_ai_music_platforms, professional_musicians).
narrative_ontology:constraint_victim(generative_ai_music_platforms, legacy_music_publishers).
narrative_ontology:constraint_victim(generative_ai_music_platforms, hobbyist_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROFESSIONAL MUSICIAN (SNARE) — For professional creators, this technology represents a direct threat to their livelihood. It devalues their skills and floods the market with low-cost content, suppressing wages and licensing fees. The 'coordination' function is perceived as a mechanism of their own displacement. Their exit is constrained as they must operate within this new technological paradigm. d≈0.90, f(d)≈1.33, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(generative_ai_music_platforms, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: TECH PLATFORM (ROPE) — For platform owners (Google, Apple), the system is a pure coordination mechanism. It lowers the barrier to content creation, drives engagement within their ecosystem, and provides vast amounts of data for model training. From their viewpoint, the extraction is a feature that optimizes the system. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09.
constraint_indexing:constraint_classification(generative_ai_music_platforms, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HOBBYIST CREATOR (TANGLED ROPE) — The amateur user gains access to powerful creation tools (a coordination benefit), but pays for it with their creative output (which trains the model) and operates under restrictive terms of service that limit ownership and monetization. They are both beneficiary and victim, trapped within the platform's ecosystem. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(generative_ai_music_platforms, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — This view recognizes both the genuine coordination function (democratizing music creation) and the severe, asymmetric extraction from professional artists and amateur users. The high suppression score reflects the market-centralizing effect of these platforms. This is the canonical classification. d≈0.73, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(generative_ai_music_platforms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY INDUSTRY - SHOCK (SNARE) — Initially, record labels and publishers view this as a purely extractive threat that undermines their catalogs and business models. Their power is institutional, but their exit is constrained because they cannot ignore a market-altering technology. They are a victim of disruption.
constraint_indexing:constraint_classification(generative_ai_music_platforms, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY INDUSTRY - ADAPTED (ROPE) — Over time, the same legacy institutions partner with tech platforms, licensing their catalogs for training and using the AI tools to reduce production costs. Their exit option shifts to arbitrage as they learn to profit from the new system, transforming them into beneficiaries who see it as a coordination tool.
constraint_indexing:constraint_classification(generative_ai_music_platforms, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generative_ai_music_platforms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(generative_ai_music_platforms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generative_ai_music_platforms, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(generative_ai_music_platforms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(generative_ai_music_platforms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65) is high, representing the significant economic value transferred from human creators (professional and amateur) to the platform owners. Suppression (0.75) is also high, as the network effects and ecosystem lock-in make it extremely difficult for alternative, non-extractive tools to compete at scale. Theater Ratio (0.60) reflects the pervasive marketing narrative of 'empowering creativity,' which obscures the underlying extractive model of data collection and market consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The platform architect sees a pure coordination tool (Rope) that unlocks creativity. The professional musician, whose career is threatened, sees a pure extractive mechanism (Snare). The hobbyist creator experiences the conflict directly, receiving a valuable tool in exchange for their creative labor, classifying it as a Tangled Rope. The analysis of the Legacy Music Industry shows how an institution's perspective can shift from Snare to Rope as it adapts its strategy from resistance to co-optation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the structural relationships. The tech platform is a clear beneficiary with arbitrage exit, yielding a negative effective extraction (χ < 0). The professional musician is a victim with constrained options, resulting in a very high positive χ. The hobbyist is declared as both beneficiary and victim, and their 'trapped' status as a user within the ecosystem results in a high derived directionality value, correctly classifying their experience as highly extractive (Tangled Rope bordering on Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical example of resolving mandatrophy. A naive analysis might label the system a Rope ('it's just a tool for creativity') or a Snare ('it's destroying artists' jobs'). The Deferential Realism framework, by modeling both the genuine coordination function and the asymmetric extraction, correctly identifies the full structure as a Tangled Rope. It reveals how a seemingly positive technological advance can simultaneously function as a deeply extractive social and economic arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    copyright_and_ownership,
    'Who holds the legal copyright to AI-generated music: the user who wrote the prompt, the platform that owns the model, or is the work uncopyrightable?',
    'Landmark legal rulings and new legislation (e.g., extensions of the EU AI Act).',
    'If users retain full ownership, the platform is more of a tool (Rope). If the platform retains rights or the work is public domain, its extractive nature is confirmed (Tangled Rope/Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyright_and_ownership, empirical, 'Legal ambiguity of ownership for AI-generated works').

omega_variable(
    stylistic_monoculture,
    'Will the reliance on models trained on existing music lead to stylistic homogenization and creative stagnation?',
    'Long-term musicological and cultural analysis of popular music trends over a 10-20 year period post-AI adoption.',
    'If a monoculture emerges, the ''coordination'' benefit of the platform becomes a negative externality, increasing its effective extractiveness and pushing its classification toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stylistic_monoculture, empirical, 'Potential for AI music tools to cause long-term artistic stagnation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generative_ai_music_platforms, 2023, 2033).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t2023, generative_ai_music_platforms, theater_ratio, 2023, 0.65).
narrative_ontology:measurement(gene_tr_t2028, generative_ai_music_platforms, theater_ratio, 2028, 0.55).
narrative_ontology:measurement(gene_tr_t2033, generative_ai_music_platforms, theater_ratio, 2033, 0.6).

% Extraction over time
narrative_ontology:measurement(gene_be_t2023, generative_ai_music_platforms, base_extractiveness, 2023, 0.5).
narrative_ontology:measurement(gene_be_t2028, generative_ai_music_platforms, base_extractiveness, 2028, 0.58).
narrative_ontology:measurement(gene_be_t2033, generative_ai_music_platforms, base_extractiveness, 2033, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generative_ai_music_platforms, information_standard).
narrative_ontology:affects_constraint(generative_ai_music_platforms, generative_ai_image_platforms).
narrative_ontology:affects_constraint(generative_ai_music_platforms, social_media_content_algorithms).
narrative_ontology:affects_constraint(generative_ai_music_platforms, streaming_service_royalty_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
