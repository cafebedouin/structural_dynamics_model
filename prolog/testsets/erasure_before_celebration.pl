% ============================================================================
% CONSTRAINT STORY: erasure_before_celebration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_erasure_before_celebration, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: erasure_before_celebration
 *   human_readable: Erasure Before Celebration: Cultural Appropriation as Natural Law
 *   domain: cultural_sociology/political_economy/performance_studies
 *
 * SUMMARY:
 *   The erasure-before-celebration pattern describes a recurring sequence in
 *   cultural appropriation: marginalized communities create cultural
 *   practices (music genres, fashion styles, culinary traditions, sporting
 *   events, holidays); these practices gain mainstream visibility; commercial
 *   interests adopt and profit from them; originating communities are
 *   displaced from participation, economic benefit, and cultural authority.
 *   The pattern appears across domains: Irish pub closures during St.
 *   Patrick's Day commercialization (until legal barriers lifted in 1960s);
 *   Black jockeys' displacement from horse racing (1875-1902) while Derby
 *   fashion culture emerged among white elites (post-2011); jazz, rock,
 *   hip-hop following similar trajectories. The constraint is presented as a
 *   natural law of cultural diffusion in academic and popular discourse, but
 *   the structural data reveals identifiable beneficiaries (commercial
 *   interests, dominant cultural institutions) and victims (originating
 *   marginalized communities), suggesting the mountain classification is a
 *   false summit that naturalizes contingent power structures.
 *
 * KEY AGENTS:
 *   - Originating Marginalized Communities: Primary victim (powerless/trapped) — create cultural practices, then displaced from participation and economic benefit during commercialization
 *   - Mainstream Commercial Interests: Primary beneficiary (institutional/arbitrage) — profit from commercialized cultural practices without compensating or including originators
 *   - Dominant Cultural Institutions: Secondary beneficiary (institutional/arbitrage) — museums, media, academic disciplines that gain authority by 'discovering' and codifying practices after displacement
 *   - Cultural Preservation Coalition: Organized resistance (organized/constrained) — heritage protection movements, IP claims, community ownership models attempting to interrupt the cycle
 *   - Cultural Diffusion Theorist: Analytical observer (analytical/analytical) — academic frameworks that naturalize the pattern as inevitable cultural evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(erasure_before_celebration, 0.08).
domain_priors:suppression_score(erasure_before_celebration, 0.03).
domain_priors:theater_ratio(erasure_before_celebration, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(erasure_before_celebration, extractiveness, 0.08).
narrative_ontology:constraint_metric(erasure_before_celebration, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(erasure_before_celebration, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(erasure_before_celebration, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(erasure_before_celebration, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(erasure_before_celebration, mountain).
narrative_ontology:human_readable(erasure_before_celebration, "Erasure Before Celebration: Cultural Appropriation as Natural Law").
narrative_ontology:topic_domain(erasure_before_celebration, "cultural_sociology/political_economy/performance_studies").

domain_priors:emerges_naturally(erasure_before_celebration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(erasure_before_celebration, mainstream_commercial_interests).
narrative_ontology:constraint_beneficiary(erasure_before_celebration, dominant_cultural_institutions).
narrative_ontology:constraint_victim(erasure_before_celebration, originating_marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ORIGINATING COMMUNITY (MOUNTAIN) — Experiences the erasure-celebration cycle as an immutable force of cultural evolution. No exit from the pattern: when your cultural practice becomes commercially valuable, you are already being displaced. The community sees this as natural law because it has happened across every domain they can observe — music, fashion, cuisine, holidays, sports. Resistance appears futile.
constraint_indexing:constraint_classification(erasure_before_celebration, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMERCIAL APPROPRIATOR (MOUNTAIN) — Experiences cultural diffusion as a natural process of market evolution. 'Culture spreads' is treated as a law of economics, not a choice. The beneficiary sees no extraction because the framing naturalizes the displacement: marginalized groups 'lose interest' or 'move on' as the practice 'matures' into the mainstream. The mountain classification here is the beneficiary's naturalized framing, not structural reality.
constraint_indexing:constraint_classification(erasure_before_celebration, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / CULTURAL DIFFUSION VIEW (MOUNTAIN) — Academic frameworks treating cultural appropriation as inevitable diffusion: 'cultural practices naturally spread from margins to center as societies modernize.' This perspective sees the pattern as a universal law of cultural evolution, erasing the specific mechanisms of displacement (economic exclusion, legal barriers, violence) that enable the 'spread.' The mountain classification is a false summit — naturalizing contingent power structures as cultural physics.
constraint_indexing:constraint_classification(erasure_before_celebration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: CULTURAL PRESERVATION COALITION (SNARE) — Organized resistance movements (cultural heritage protection, intellectual property claims, community ownership models) see the constraint as extractive and suppressible, not natural. High extraction: originating communities lose economic access, cultural authority, and participation rights as their practices are commercialized. High suppression: legal systems protect commercial appropriators while denying communities IP rights over cultural practices. The coalition has some power (organized) but constrained exit — they can document and resist but cannot prevent the cycle.
constraint_indexing:constraint_classification(erasure_before_celebration, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(erasure_before_celebration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(erasure_before_celebration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(erasure_before_celebration, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(erasure_before_celebration, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(erasure_before_celebration, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(erasure_before_celebration, ExtMetricName, E),
    domain_priors:suppression_score(erasure_before_celebration, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(erasure_before_celebration),
    narrative_ontology:constraint_metric(erasure_before_celebration, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(erasure_before_celebration, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(erasure_before_celebration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Low, but non-zero. The base extraction reflects that some economic value flows from originating communities to commercial appropriators, but the magnitude is constrained by the fact that many cultural practices have limited commercial value, and some communities do retain participation (though diminished). The low value is appropriate for a claimed mountain — if extraction were high, the mountain classification would be obviously false. Suppression (0.03): Very low. Legal and economic barriers exist (zoning laws, licensing requirements, capital access disparities) but are not universally applied or insurmountable. Many communities maintain cultural practices despite commercialization. The low suppression supports the mountain claim while remaining non-zero to reflect real barriers. Theater ratio (0.15): Low. The cultural diffusion framing has some performative content (academic theories that obscure power dynamics, 'celebration' rhetoric that erases displacement) but is not primarily theatrical — the pattern has real structural features. Accessibility collapse (0.92): Very high. From the displaced community's perspective, the pattern appears nearly universal and unavoidable — every cultural practice that gains mainstream value follows the same trajectory. Resistance (0.08): Very low. Historical resistance efforts (cultural preservation, IP claims) have rarely prevented the cycle, reinforcing the perception of immutability.
 *
 * PERSPECTIVAL GAP:
 *   The displaced community and the commercial appropriator both classify the constraint as mountain, but from opposite structural positions. The community sees an immutable force that always displaces them; the appropriator sees a natural market process that always rewards efficiency and scale. Both perspectives naturalize the pattern, but the community's naturalization is defensive (learned helplessness from repeated displacement) while the appropriator's is ideological (market fundamentalism that erases power). The cultural preservation coalition breaks the mountain consensus by seeing the pattern as extractive and suppressible — their snare classification reveals that the 'natural law' framing serves the beneficiaries' interests. The analytical observer's mountain classification is the most dangerous false summit: academic theories of cultural diffusion provide intellectual cover for the extraction by treating it as inevitable evolution rather than contingent power structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The displaced originating community is the primary victim with trapped exit options — they cannot prevent their cultural practices from being appropriated, and they cannot exit the pattern (creating new practices just restarts the cycle). Derived d ≈ 0.95 (victim + trapped) → high f(d) → high experienced extraction, but the base ε is low, so even high f(d) produces moderate χ. The commercial appropriator is the primary beneficiary with arbitrage exit options — they can choose which cultural practices to commercialize and can exit if profitability declines. Derived d ≈ 0.05 (beneficiary + arbitrage) → negative f(d) → negative experienced extraction (they see only benefit). The cultural preservation coalition is organized with constrained exit — they have collective agency and can resist but cannot fully prevent appropriation. Derived d ≈ 0.55 (victim + organized + constrained) → moderate f(d) → moderate experienced extraction. The analytical observer uses the canonical analytical d ≈ 0.72 → moderate-high f(d), but their mountain classification reveals the false summit: they are naturalizing the pattern rather than analyzing its structural mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   FALSE SUMMIT CANDIDATE: This constraint is presented as a mountain (natural law of cultural diffusion) but has identifiable beneficiaries (commercial interests, dominant institutions) and victims (originating marginalized communities). The FSM detector should flag this for reclassification. The structural evidence against the mountain claim: (1) The pattern does not appear in all societies — cultural diffusion in societies with different IP regimes and power distributions shows different trajectories. (2) The timeline causality is contested — displacement often precedes commercialization (Irish pub closures before St. Patrick's Day became commercial; Black jockey exclusion before Derby fashion culture), suggesting active suppression rather than natural diffusion. (3) Beneficiaries have agency — commercial interests choose which practices to appropriate and actively lobby against community IP claims. The mountain classification serves the beneficiaries by naturalizing their extraction: if the pattern is a law of cultural evolution, resistance is futile and policy intervention is misguided. The omega variables document the irreducible uncertainty: is this genuinely natural diffusion (mountain) or structural displacement (snare)? The resolution mechanism is empirical — comparative analysis and historical counterfactuals can distinguish natural from contingent patterns.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_diffusion_vs_structural_displacement,
    'Is the erasure-celebration pattern a natural law of cultural diffusion or a contingent outcome of specific power structures that could be otherwise?',
    'Comparative analysis of cultural diffusion in societies with different IP regimes, economic structures, and power distributions. Historical counterfactuals: cases where originating communities retained participation and authority during commercialization.',
    'If natural law: mountain classification is correct, resistance is futile, policy interventions are misguided. If structural: the mountain perspectives are false summits, and the pattern is a snare that benefits identifiable actors through suppressible mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_diffusion_vs_structural_displacement, empirical, 'Whether erasure-celebration is natural diffusion or structural displacement').

omega_variable(
    timeline_causality,
    'Does commercialization cause displacement, or does displacement enable commercialization?',
    'Temporal sequencing analysis across multiple cases. Identify whether economic exclusion precedes commercial adoption (displacement enables appropriation) or follows it (appropriation causes displacement).',
    'If displacement precedes: the pattern is a coordinated extraction mechanism (snare). If commercialization precedes: the pattern may be an unintended consequence of market forces (closer to mountain, though still potentially false summit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(timeline_causality, empirical, 'Causal direction between displacement and commercialization').

omega_variable(
    beneficiary_awareness,
    'Do commercial appropriators and cultural institutions actively suppress originating communities, or do they passively benefit from pre-existing suppression?',
    'Documentary evidence of active exclusion (legal barriers, zoning laws, licensing requirements that target originating communities) vs passive benefit (market entry after communities are already displaced by other forces).',
    'If active: clear snare with identifiable extractors. If passive: beneficiaries are opportunistic rather than causal, complicating the extraction attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_awareness, empirical, 'Whether beneficiaries actively suppress or passively benefit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(erasure_before_celebration, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(erase_theater_initial, erasure_before_celebration, theater_ratio, 0, 0.1).
narrative_ontology:measurement(erase_theater_mid, erasure_before_celebration, theater_ratio, 50, 0.12).
narrative_ontology:measurement(erase_theater_final, erasure_before_celebration, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(erase_extract_initial, erasure_before_celebration, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(erase_extract_mid, erasure_before_celebration, base_extractiveness, 50, 0.07).
narrative_ontology:measurement(erase_extract_final, erasure_before_celebration, base_extractiveness, 100, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(erasure_before_celebration, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is a standalone claim about cultural appropriation dynamics. It does not decompose into multiple stories with different epsilon values — the same structural pattern (displacement preceding commercialization) is the observable across all cases. Future constraint stories about specific appropriation cases (e.g., 'jazz_commercialization', 'hip_hop_appropriation') would be downstream of this general pattern and linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
