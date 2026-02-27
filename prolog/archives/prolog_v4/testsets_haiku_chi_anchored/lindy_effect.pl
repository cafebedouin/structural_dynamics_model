% ============================================================================
% CONSTRAINT STORY: lindy_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lindy_effect, []).

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
 *   constraint_id: lindy_effect
 *   human_readable: The Lindy Effect
 *   domain: social/intellectual
 *
 * SUMMARY:
 *   The Lindy Effect is a theorized phenomenon where the future life
 *   expectancy of non-perishable things (ideas, books, technologies) is
 *   proportional to their current age. This creates a structural constraint
 *   in intellectual and cultural domains: survival probability is assigned
 *   retroactively based on historical longevity rather than intrinsic merit.
 *   The constraint exhibits a diagnostic tension between its mathematical
 *   interpretation (survivor bias, a natural law) and its institutional
 *   manifestation (gatekeeping mechanism, enforced). The Lindy effect
 *   functions simultaneously as a coordination mechanism (stable intellectual
 *   canon enables cumulative knowledge-building), an extraction mechanism
 *   (novel ideas face systematic age-bias barriers), a temporary scaffold
 *   (decentralized platforms bypass traditional gatekeepers), a degraded
 *   ritual (tenure and citation systems perform quality-testing but
 *   increasingly function as pure hierarchy enforcement), and a mathematical
 *   inevitability (Bayesian survivor bias). This multiplicity of types from
 *   different perspectives reveals how intellectual gatekeeping can be
 *   simultaneously natural and contingent, legitimate and extractive.
 *
 * KEY AGENTS:
 *   - Established Intellectual Frameworks: Primary beneficiary (institutional/arbitrage) — canonical texts, seminal authors, recognized paradigms accumulate prestige and resources as they age
 *   - Emergent Ideas and Novel Frameworks: Primary victim (powerless/trapped) — new intellectual claims must overcome presumption against youth; face structural barriers to attention and adoption
 *   - Innovation Communities: Secondary agents (moderate/constrained) — researchers and creators both benefit (via time-tested methods) and constrain (via age penalty on novel work)
 *   - Decentralized Knowledge Platforms: Organized agents (organized/mobile) — arXiv, GitHub, open-source communities building alternative discovery pathways that reduce Lindy penalty
 *   - Academic Institutions: Institutional actor (institutional/arbitrage) — tenure, citation metrics, canonical curriculum enforce Lindy bias while nominally serving peer validation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inevitable Bayesian logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lindy_effect, 0.52).
domain_priors:suppression_score(lindy_effect, 0.48).
domain_priors:theater_ratio(lindy_effect, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lindy_effect, extractiveness, 0.52).
narrative_ontology:constraint_metric(lindy_effect, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(lindy_effect, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lindy_effect, tangled_rope).
narrative_ontology:human_readable(lindy_effect, "The Lindy Effect").
narrative_ontology:topic_domain(lindy_effect, "social/intellectual").

domain_priors:requires_active_enforcement(lindy_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lindy_effect, established_intellectual_frameworks).
narrative_ontology:constraint_beneficiary(lindy_effect, incumbent_cultural_institutions).
narrative_ontology:constraint_victim(lindy_effect, emergent_knowledge_claims).
narrative_ontology:constraint_victim(lindy_effect, novel_technological_disruption).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGENT IDEA (SNARE) — New ideas, technologies, and frameworks have no proof of longevity. The Lindy effect creates a structural bias against novelty: survival probability is assigned retroactively based on age, not merit. New ideas must compete against the presumption that older ideas have earned their place through market/epistemic testing. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(lindy_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INNOVATION COMMUNITY (TANGLED ROPE) — Researchers and creators benefit from the Lindy effect when their own work begins to accumulate age (coordination: time-tested methods attract collaborators and funding). But they are constrained by the presumption against novelty while their work is young (extraction: must overcome age bias to gain attention). d≈0.58, f(d)≈0.76, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(lindy_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED INTELLECTUAL INSTITUTIONS (ROPE) — Universities, publishing houses, and canonical intellectual frameworks benefit from Lindy as a coordination mechanism: it creates a stable, predictable canon that enables cumulative knowledge-building. Established scholars experience the constraint as legitimate ordering (teaching from canonical texts, citing seminal works, building on recognized foundations). d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.03. Near-zero effective extraction; net beneficiary.
constraint_indexing:constraint_classification(lindy_effect, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED KNOWLEDGE MOVEMENT (SCAFFOLD) — Open-source software, peer review preprints (arXiv, bioRxiv), and distributed validation communities are building alternative discovery pathways that bypass the Lindy gatekeeper effect. These platforms use direct technical validation and network-based reputation rather than historical age as the primary signal. This is temporary scaffolding with an implicit sunset: as decentralized validation matures (estimated 10-20 years), the Lindy extraction mechanism weakens. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.20.
constraint_indexing:constraint_classification(lindy_effect, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TENURE AND CITATION SYSTEM (PITON) — Academic career advancement and citation metrics have become substantially performative rituals that nominally serve as quality signals but increasingly function as inertial enforcement of established hierarchies. The tenure-via-citations logic once coordinated peer validation; it now extracts value from junior scholars while maintaining an appearance of meritocracy. theater_ratio=0.62 reflects this mixed performance/function. The system is degraded: it persists through institutional momentum despite widespread recognition that citation metrics correlate poorly with actual impact or truthfulness.
constraint_indexing:constraint_classification(lindy_effect, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: MATHEMATICAL SURVIVOR BIAS (MOUNTAIN) — From the analytical view, the Lindy effect is not a constraint but a mathematical observation about survivor bias: things that have survived to age T are statistically more likely to survive to age T+δt (because selection has already removed most fragile variants). This is a tautology of Bayesian logic, not a social mechanism. However, the institutional instantiation (academy, publishing, cultural canon) is NOT a mountain — the structural data (ε=0.52, suppression=0.48, theater=0.62) shows this is contingent enforcement, not mathematical necessity. The engine will flag this as a false summit: conflating the mathematical observation with the social mechanism.
constraint_indexing:constraint_classification(lindy_effect, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lindy_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lindy_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lindy_effect, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lindy_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lindy_effect, TR),
    TR >= 0.70.

:- end_tests(lindy_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Lindy effect systematically disadvantages novel ideas in attention, funding, and institutional recognition during their vulnerable early period. However, the extraction is not as severe as pure suppression (0.70+) because some novel ideas do break through, and the effect is probabilistic rather than absolute. The metric reflects a genuine but not insurmountable barrier. Suppression (0.48): Moderate. The barriers are substantial: publication bias favors established frameworks, funding follows proven directions, career incentives reward work in established paradigms. But suppression is not total — open-source communities, preprint servers, and alternative metrics are creating escape routes. Theater ratio (0.62): Moderate-high. Academic credentials, citation counts, and tenure evaluations are substantially performative: they claim to measure intellectual quality but increasingly measure institutional position. A well-cited mediocre idea (old) may outrank a novel profound idea (young) purely on age. The performance of quality-testing has increased as metrics have become gamified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same mathematical fact (survivor bias in aging phenomena) manifests as radically different constraint types depending on the observer's position. Established scholars experience it as legitimate coordination (canonical texts enable knowledge accumulation). Novel researchers experience it as extraction (age penalty prevents recognition). The decentralized knowledge movement experiences it as a temporary scaffold being replaced. The tenure system experiences itself as degraded ritual. The mathematical observer risks naturalizing the whole arrangement as inevitable. The emergent idea has zero escape routes and experiences pure snare. The perspectival gap is exceptionally wide because the constraint conflates a mathematical property with an institutional mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Established frameworks: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary. Emergent ideas: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction (no alternative distribution of prestige available to new ideas). Innovation community: Mixed (beneficiary when old, victim when young) + constrained → d≈0.58, f(d)≈0.76. Moderate extraction, reflects the temporal asymmetry. Decentralized platforms: Organized + mobile → d≈0.35, f(d)≈0.32. Low extraction; platforms have agency and are building exits. Academic institutions: Institutional + arbitrage → d≈0.10, f(d)≈0.05. Piton classification comes from theater gate, not directionality. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification attempted but will be flagged as false summit by engine because the institutional data contradicts mathematical naturality.
 *
 * MANDATROPHY ANALYSIS:
 *   CONCEPTUAL CONFLICT: The mandatrophy emerges from the collision between the mathematical and institutional interpretations. The mathematical Lindy effect (survivor bias) is a tautology and therefore a mountain: if you select a set of ideas that survived to age T, they are statistically more likely to survive further. This is logically necessary. But the institutional Lindy effect (academy, publishing, canon-formation) is contingent enforcement: the decision to weight historical survival in gatekeeping is a choice, not a law. The tangled_rope classification resolves the mandatrophy by treating the institutional manifestation as the actual constraint. The mathematical observation is a post-hoc justification, not the mechanism. The engine's perspectival analysis shows: (1) The established/institutional perspective sees coordination (rope). (2) The novel idea perspective sees extraction (snare). (3) The analytical perspective attempts mountain but fails the theater gate (theater=0.62 > 0.05) and accessibility_collapse gate (not sufficiently natural-law-like). The false summit detection prevents the constraint from being mislabeled as an immutable law when it is actually a contingent institutional practice justified retroactively by Bayesian logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bayesian_vs_institutional,
    'Is the Lindy effect a mathematical truth about survivor bias, or a social mechanism that enforces age-based gatekeeping?',
    'Empirical study of survival curves: do non-institutional ideas follow Lindy distribution? Do institutional ideas show the same curve? Comparison with pre-selection populations.',
    'If mathematical: the constraint is a Mountain (inevitable). If institutional: the constraint is Tangled Rope (contingent, enforcement-dependent). This directly affects whether the Lindy effect is a feature or a bug.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bayesian_vs_institutional, conceptual, 'Whether Lindy is mathematical survivor bias or institutional gatekeeping').

omega_variable(
    decay_rate_heterogeneity,
    'Do different idea categories (scientific theories, literary works, technologies, philosophical frameworks) exhibit the same Lindy decay rate, or does the effect vary by domain?',
    'Longitudinal analysis of idea survival rates by category; comparison of half-life distributions across domain types.',
    'If homogeneous: supports universal mathematical explanation. If heterogeneous: suggests domain-specific institutional enforcement mechanisms (e.g., technology shows faster turnover than philosophy due to different institutional structures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decay_rate_heterogeneity, empirical, 'Whether Lindy decay rate is universal or domain-dependent').

omega_variable(
    decentralization_efficacy,
    'Do decentralized discovery platforms (arXiv, GitHub, open-source communities) actually reduce the Lindy penalty for novel ideas, or do they reproduce the same age-bias in different form?',
    'Comparative analysis: survival curves for ideas first shared on centralized vs decentralized platforms; tracking of adoption rates for novel vs established frameworks across venues.',
    'If decentralized platforms reduce bias: scaffold perspective confirmed, sunset is real. If bias persists: the Lindy effect is more fundamental than institutional, suggesting a deeper cognitive or coordination constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_efficacy, empirical, 'Whether decentralized platforms reduce Lindy age bias').

omega_variable(
    extraction_vs_coordination_boundary,
    'What is the structural boundary between the Lindy effect as a legitimate coordination mechanism (stable canon enables cumulative knowledge) and as an extraction mechanism (age-bias prevents novelty)?',
    'Analysis of institutional design: Does the age-weighting serve a coordination function (enabling knowledge accumulation) or primarily extract value from novel-idea creators? Can the coordination benefit be decoupled from the age penalty?',
    'If boundary is clear: institutional reform can preserve coordination while eliminating extraction. If boundary is fuzzy: the two functions may be inseparable, and any attempt to remove the Lindy penalty risks destabilizing cumulative knowledge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, conceptual, 'Boundary between coordination and extraction in Lindy mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lindy_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lindy_tr_t0, lindy_effect, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lindy_tr_t5, lindy_effect, theater_ratio, 5, 0.5).
narrative_ontology:measurement(lindy_tr_t10, lindy_effect, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(lindy_be_t0, lindy_effect, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(lindy_be_t5, lindy_effect, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(lindy_be_t10, lindy_effect, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lindy_effect, information_standard).
narrative_ontology:affects_constraint(lindy_effect, academic_citation_bias).
narrative_ontology:affects_constraint(lindy_effect, technological_paradigm_lock_in).
narrative_ontology:affects_constraint(lindy_effect, cultural_canon_formation).

% DUAL FORMULATION NOTE:
% The Lindy effect decomposes into two constraints: (1) mathematical_survivor_bias (ε=0.05, Mountain) — the pure Bayesian observation that age correlates with future survival; (2) lindy_effect_institutional (ε=0.52, Tangled Rope) — the social mechanism that uses age as a gatekeeping criterion in academic, publishing, and cultural institutions. This file documents the institutional constraint. The mathematical version is a downstream observer's natural-law interpretation that the engine flags as a false summit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lindy_effect, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
