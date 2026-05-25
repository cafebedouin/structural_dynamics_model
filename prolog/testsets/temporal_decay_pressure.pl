% ============================================================================
% CONSTRAINT STORY: temporal_decay_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_decay_pressure, []).

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
 *   constraint_id: temporal_decay_pressure
 *   human_readable: Temporal Decay Pressure on Land-Use Prohibitions
 *   domain: disaster_anthropology/commitment_systems/institutional_memory
 *
 * SUMMARY:
 *   The Aneyoshi case (Iwate Prefecture, Japan: a rural village with a stone
 *   marker inscribed 'High tides have come here') exemplifies a structural
 *   constraint on institutional memory in disaster risk management. The
 *   prohibition on building below the marker stone was initially functional
 *   during the period when the hazard (tsunamis) remained within living
 *   memory. Over generations, as no major tsunami struck, the constraint's
 *   causal rationale decayed from lived knowledge to ritual performance to
 *   near-total forgetting. When the 2011 Tōhoku earthquake triggered a
 *   tsunami, residents below the marker — descendants of those who erected it
 *   — were unprepared. The temporal_decay_pressure reading focuses on how
 *   institutional commitments to safety degrade as generational distance from
 *   the hazard increases, creating a tangled rope structure: the prohibition
 *   coordinated genuine hazard avoidance in its origin generation, but as
 *   memory decayed, it became asymmetrically extractive on future generations
 *   who inherited the consequence (hazardous settlement patterns) without the
 *   knowledge (why the prohibition existed). The reading assumes oral
 *   institutional memory is the primary mechanism; alternative readings might
 *   emphasize written legal codes, embodied spatial practices, or successful
 *   ceremonial reinforcement in other cultures.
 *
 * KEY AGENTS:
 *   - Origin Generation (now deceased): Primary beneficiary (institutional/arbitrage) — established the prohibition with full hazard knowledge; captured safety through institutional memory
 *   - Intermediate Generations: Mixed status — benefit from accumulated settlement patterns but partially maintain constraint memory; constrained by both hazard knowledge and economic pressure
 *   - Future Generation (2011 residents): Primary victim (powerless/trapped) — inherited hazardous settlement below the marker without knowledge of causation; no exit from physical geography
 *   - Constraint Custodian (knowledge keepers, elders, historians): Moderate power/constrained — bore responsibility for memory maintenance; constrained by declining institutional support and competing narratives
 *   - Economic Pressure (real estate, agriculture, fishing): Institutional/arbitrage — benefited from temporal decay enabling development below the marker; passively exploited weakened constraint
 *   - Formal Disaster Risk Management (government institutions, zoning, written codes): Organized/constrained — attempted institutional substitution for decaying oral tradition; constrained by competing bureaucratic priorities
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional memory decay as entropic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_decay_pressure, 0.58).
domain_priors:suppression_score(temporal_decay_pressure, 0.68).
domain_priors:theater_ratio(temporal_decay_pressure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_decay_pressure, extractiveness, 0.58).
narrative_ontology:constraint_metric(temporal_decay_pressure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(temporal_decay_pressure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_decay_pressure, tangled_rope).
narrative_ontology:human_readable(temporal_decay_pressure, "Temporal Decay Pressure on Land-Use Prohibitions").
narrative_ontology:topic_domain(temporal_decay_pressure, "disaster_anthropology/commitment_systems/institutional_memory").

domain_priors:requires_active_enforcement(temporal_decay_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_decay_pressure, present_generation_beneficiaries).
narrative_ontology:constraint_beneficiary(temporal_decay_pressure, economic_pressure_beneficiaries).
narrative_ontology:constraint_victim(temporal_decay_pressure, future_generation_safety).
narrative_ontology:constraint_victim(temporal_decay_pressure, constraint_memory_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATION (SNARE) — Inherits a hazardous landscape without the institutional memory or social infrastructure that originally imposed the prohibition. No exit from the physical hazard; no knowledge of why the land was forbidden; maximum extraction through absent covenant.
constraint_indexing:constraint_classification(temporal_decay_pressure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINT CUSTODIAN (TANGLED ROPE) — Bears the burden of maintaining oral tradition and institutional memory across generations. Constrained by limited power to enforce prohibition against mounting economic pressure. Experiences mixed extraction: genuine coordination function (preserving safety knowledge) coupled with asymmetric burden (sole responsibility for memory maintenance without institutional support).
constraint_indexing:constraint_classification(temporal_decay_pressure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ECONOMIC INTEREST (ROPE) — Coordinating legitimate resource use and settlement expansion. Experiences the prohibition as a pure coordination problem to solve or work around. Benefits from temporal decay: as memory fades, the constraint's coordination function weakens while its resource-blocking function persists, creating arbitrage opportunity.
constraint_indexing:constraint_classification(temporal_decay_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FORMAL DRM INSTITUTION (SCAFFOLD) — Written law, zoning codes, and regulatory bodies attempt to replace oral prohibition with institutional structure. Temporary by design: contingent on continued institutional capacity, written records, and bureaucratic continuity. Faces sunset pressure from capacity decay, competing priorities, and transition to informal/forgotten systems.
constraint_indexing:constraint_classification(temporal_decay_pressure, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RITUAL REAFFIRMATION (PITON) — Ceremonies, commemorations, and symbolic enforcement of the prohibition persist long after their functional necessity. Theater ratio high: the ritual's performative content (remembering the disaster) becomes decoupled from its protective function. Maintained through cultural inertia even as economic pressure to develop accumulates.
constraint_indexing:constraint_classification(temporal_decay_pressure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ENTROPIC VIEW (MOUNTAIN) — From civilizational timescale, temporal decay of institutional memory appears as an entropic natural law: human institutions inherently decay, oral traditions inevitably fade, written records deteriorate. The mountain classification naturalizes institutional failure as inevitable physics. However, declared beneficiaries trigger false summit detection — some agents benefit from this 'entropy,' revealing construction rather than naturalness.
constraint_indexing:constraint_classification(temporal_decay_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_decay_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_decay_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_decay_pressure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(temporal_decay_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_decay_pressure, TR),
    TR >= 0.70.

:- end_tests(temporal_decay_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. At t=0 (origin generation), extractiveness is low (0.15) — the prohibition is primarily coordination (hazard avoidance) with minimal asymmetric extraction. As generational distance increases (t=3,6,9), extractiveness rises (0.35→0.52→0.58) because the prohibition shifts from coordination (preventing hazards we know about) to extraction (limiting development options without justification that younger generations accept). The mechanism is temporal decay of causal knowledge: extractiveness rises because justification for the constraint is forgotten, leaving only constraint. Suppression (0.68): High and stable. Barriers to exit include physical geography (cannot relocate the hazard), social/economic ties to place, and declining knowledge of the alternative (why was this area forbidden?). Suppression is high because forgetting the rationale also prevents agents from deliberately choosing to rebuild elsewhere — they don't know why they shouldn't build here. Theater ratio (0.55): Moderate and rising. In early generations, the theater is low — enforcement is functional (people understand the hazard). As memory decays, theater rises — ceremonial reaffirmation of the prohibition persists (stone marker, stories, rituals) even as functional understanding fades. By t=9, theater (0.55) reflects that the constraint is maintained partly through ritual and social obligation rather than understood hazard logic. The trajectory shows Goodhart drift: the measurement (formal prohibition) becomes decoupled from its purpose (hazard avoidance).
 *
 * PERSPECTIVAL GAP:
 *   The origin generation sees rope (pure coordination — we remember why this is dangerous). The constraint custodian sees tangled rope (coordination + extraction through memory burden). The economic interest sees rope (we can work with or around this coordination rule). The future generation sees snare (constraint with no causal justification). The formal institution sees scaffold (temporary coordination problem being solved by written law and zoning). The ritual reaffirmation system sees piton (ceremony persisting through inertia). The analytical observer risks mountain (temporal decay is entropic law). The gap reveals the constraint's true structure: it is tangled rope from multiple directions, not mountain from any.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (present_generation_beneficiaries, economic_pressure_beneficiaries) occupy institutional positions with arbitrage options: they can develop below the marker, relocate, or ignore the prohibition as knowledge fades. Their d-values are low (~0.15-0.20), producing negative or minimal χ. The constraint custodian has moderate power but constrained exit (cannot stop being the knowledge keeper without community fracture); d ≈ 0.55, producing moderate χ. Future generation has powerless/trapped position; d ≈ 0.95, maximum f(d) ≈ 1.42. The analytical observer at civilizational scope with analytical exit derives d ≈ 0.72 from canonical fallback, producing moderate-high χ. The perspectival gap (all perspectives from 0.15-0.95 directionality range) shows strong structural differentiation: the same constraint extracts minimally from beneficiaries, moderately from custodians, and maximally from future victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves mandatrophy by capturing the dual nature: at t=0, the constraint is nearly pure rope (coordination of known hazard). As memory decays (t=9), it becomes snare-like (extraction of settlement options from future actors who don't understand why). The constraint is neither pure coordination nor pure extraction — it is tangled: it coordinates present-day hazard response while extracting future-generation compliance options. The theater_ratio rise (0.25→0.55) reflects the gradual Goodhart drift: enforcement mechanism (remember the marker stone) becomes disconnected from purpose (avoid the hazard). Mandatrophy resolves when the classification recognizes that the same rule serves genuinely different functions for different temporal cohorts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memory_decay_mechanism,
    'Is temporal decay of the prohibition inherent to human cognition and institutional structure, or does it reflect contingent failure of memorialization systems?',
    'Comparison of cultures/regions with systematic memory practices (ceremonial reinforcement, written dual-record, embodied spatial encoding) vs those with minimal reinforcement; correlation between institutional memory strength and compliance persistence',
    'If inherent: mountain classification of temporal decay is justified; suppression from forgetting is unavoidable. If contingent: the decay is a structural failure of commitment systems, and the constraint becomes tangled_rope (coordination failure + asymmetric extraction from future actors).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(memory_decay_mechanism, empirical, 'Whether memory decay is inherent or contingent on memorialization system quality').

omega_variable(
    aneyoshi_reading_ambiguity,
    'Which kernel reading instantiates the Aneyoshi case: the temporal_decay_pressure reading (focusing on how institutional memory fails to persist), or an alternative reading that emphasizes successful multi-generational compliance despite hazard recency drift?',
    'Historiographical analysis of Aneyoshi''s actual compliance trajectory: were the Edo and Meiji period inhabitants forgetting the prohibition, or enforcing it with full knowledge? Did the 2011 tsunami strike an area where the marker stone remained known, or where knowledge had substantially decayed?',
    'If decay reading is correct: temporal_decay_pressure (this constraint) accurately models Aneyoshi. If an alternative successful-memory reading is correct: a sibling constraint emphasizing memory stability and institutional success becomes the primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aneyoshi_reading_ambiguity, empirical, 'Whether Aneyoshi exemplifies memory decay or successful long-term compliance').

omega_variable(
    generational_hazard_recency_drift,
    'Does the physical hazard''s generational recency (time since last disaster event) independently drive forgetting, independent of institutional memory mechanisms? (The ''no one alive remembers'' mechanism.)',
    'Analysis of compliance and belief in prohibition correlated with time since last disaster event; comparison of high-frequency hazard regions (annual flooding) vs low-frequency regions (multi-generational earthquakes)',
    'If hazard recency is the primary driver: suppression value should be recalibrated upward (physical reality makes the constraint harder to maintain). If institutional memory is the limiting factor: suppression value reflects institutional capacity rather than hazard physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_hazard_recency_drift, empirical, 'Whether hazard recency drift independently drives forgetting').

omega_variable(
    extractive_economic_interest_directionality,
    'Do development interests (real estate, agriculture, industry) actively exploit temporal decay to bypass the prohibition, or do they merely take advantage of passively weakening constraints?',
    'Analysis of development timeline correlation with memory decay; evidence of deliberate suppression of prohibition knowledge vs passive benefit from forgetting',
    'If active exploitation: beneficiary directionality toward economic interests is clear; economic_pressure_beneficiaries is directly extractive. If passive: the snare character (extraction through forgetting) applies more than tangled rope (active coordination failure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_economic_interest_directionality, empirical, 'Whether economic interests actively exploit or passively benefit from decay').

omega_variable(
    kernel_contest_aneyoshi,
    'What is the precise kernel this constraint is one reading of, and which sibling readings exist?',
    'Specification of the contested commitment: is it the spatial land-use prohibition itself, the durability of the prohibition across generational boundaries, the causal mechanism connecting hazard event to institutional constraint, or the institutional form (oral vs written vs embodied)?',
    'Different kernels instantiate different readings. If kernel is ''land-use prohibition for hazard mitigation,'' this reading emphasizes temporal decay. If kernel is ''how communities encode hazard knowledge,'' a sibling reading emphasizes successful encoding mechanisms. If kernel is ''intergenerational justice,'' a sibling reading emphasizes future victim status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_aneyoshi, conceptual, 'Precise specification of the contested kernel and alternative readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_decay_pressure, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tdp_theater_t0, temporal_decay_pressure, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tdp_theater_t3, temporal_decay_pressure, theater_ratio, 3, 0.35).
narrative_ontology:measurement(tdp_theater_t6, temporal_decay_pressure, theater_ratio, 6, 0.48).
narrative_ontology:measurement(tdp_theater_t9, temporal_decay_pressure, theater_ratio, 9, 0.55).

% Extraction over time
narrative_ontology:measurement(tdp_extract_t0, temporal_decay_pressure, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tdp_extract_t3, temporal_decay_pressure, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(tdp_extract_t6, temporal_decay_pressure, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(tdp_extract_t9, temporal_decay_pressure, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_decay_pressure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(temporal_decay_pressure, 0.12).
narrative_ontology:affects_constraint(temporal_decay_pressure, institutional_memory_durability).
narrative_ontology:affects_constraint(temporal_decay_pressure, intergenerational_hazard_knowledge_transfer).

% DUAL FORMULATION NOTE:
% Temporal decay of land-use prohibition is one reading of the Aneyoshi kernel. Alternative readings emphasizing successful multi-generational compliance or institutional memory robustness would model the same phenomenon with different ε, theater_ratio, and beneficiary/victim structures. This reading assumes memory decay is the primary failure mode; siblings would emphasize memory success or alternative causal mechanisms. The constraint family links to institutional memory durability (general theory of how institutions persist) and intergenerational hazard knowledge transfer (specific context of disaster anthropology).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temporal_decay_pressure, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
