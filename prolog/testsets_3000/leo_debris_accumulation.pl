% ============================================================================
% CONSTRAINT STORY: leo_debris_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_leo_debris_accumulation, []).

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
 *   constraint_id: leo_debris_accumulation
 *   human_readable: LEO Debris Accumulation and Orbital Access Control
 *   domain: space_infrastructure/environmental_commons
 *
 * SUMMARY:
 *   LEO debris accumulation is a multi-layered constraint that functions
 *   simultaneously as a natural commons problem, an incumbent-protection
 *   mechanism, and a degraded coordination system. Low Earth Orbit contains
 *   approximately 34,000 tracked objects ≥10 cm and an estimated 900,000
 *   objects 1-10 cm. Collision cascades create new debris, raising the
 *   overall collision probability and forcing operators to invest in
 *   avoidance infrastructure. The constraint exhibits tangled rope structure:
 *   there is a genuine coordination function (shared debris tracking,
 *   collision avoidance protocols, deorbiting standards) AND asymmetric
 *   extraction (incumbent operators benefit from the debris field as a
 *   barrier to market entry; new operators bear the externality costs through
 *   higher insurance, tracking, and compliance expenses). The constraint's
 *   extractiveness has grown from 0.32 (early 2000s, when debris density was
 *   manageable) to 0.58 (current), reflecting the accumulation of both
 *   physical debris and regulatory overhead. Theater ratio remains moderate
 *   (0.45), indicating that coordination protocols are substantive but
 *   fragmented — international standards exist but compliance is voluntary
 *   and enforcement is absent.
 *
 * KEY AGENTS:
 *   - Shared Orbital Environment: Victim (powerless/trapped) — commons that cannot exit debris field; bears collision risk and contamination from all operators
 *   - New Entrant Operators: Victim (powerless/trapped) — bear debris avoidance costs and compliance overhead created by historical operators; cannot access LEO below cost threshold
 *   - Active Constellation Operators: Mixed (moderate/constrained) — benefit from debris tracking moat and entry barriers; also constrained by collision avoidance and deorbiting compliance
 *   - Incumbent Operators (Telecom/Navigation): Beneficiary (institutional/arbitrage) — protected by debris-density barriers; market share defended against new entrants by high compliance costs
 *   - Debris Mitigation Researchers: Beneficiary (organized/constrained) — funding and research opportunities driven by debris density; benefit from operator contracts for tracking and removal research
 *   - International Coordination Bodies: Institutional actor (institutional/arbitrage) — maintain voluntary compliance framework; benefit from diplomatic role and research funding without enforcement responsibility
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the debris field as an inevitable consequence of space use rather than a policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(leo_debris_accumulation, 0.58).
domain_priors:suppression_score(leo_debris_accumulation, 0.72).
domain_priors:theater_ratio(leo_debris_accumulation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(leo_debris_accumulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(leo_debris_accumulation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(leo_debris_accumulation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(leo_debris_accumulation, tangled_rope).
narrative_ontology:human_readable(leo_debris_accumulation, "LEO Debris Accumulation and Orbital Access Control").
narrative_ontology:topic_domain(leo_debris_accumulation, "space_infrastructure/environmental_commons").

domain_priors:requires_active_enforcement(leo_debris_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(leo_debris_accumulation, incumbent_satellite_operators).
narrative_ontology:constraint_beneficiary(leo_debris_accumulation, space_debris_mitigation_researchers).
narrative_ontology:constraint_victim(leo_debris_accumulation, new_entrant_operators).
narrative_ontology:constraint_victim(leo_debris_accumulation, shared_orbital_environment).
narrative_ontology:constraint_victim(leo_debris_accumulation, future_space_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SHARED ORBITAL ENVIRONMENT (SNARE) — Cannot exit the debris field or escape collision risk. Bears full cost of accumulated debris from past and present operations. No mechanism for cost recovery or remediation. The orbital commons is powerless against continued debris generation — bears maximum extraction with no alternatives.
constraint_indexing:constraint_classification(leo_debris_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEW ENTRANT OPERATORS (SNARE) — Face structurally higher launch and operational costs due to debris avoidance requirements, tracking systems, and collision insurance premiums. Cannot access LEO without bearing debris externality costs created by historical operators. Trapped by inherited debris field — exit requires abandoning space ambitions entirely. Multi-generational impact: debris will degrade orbital access for decades even if debris generation stops today.
constraint_indexing:constraint_classification(leo_debris_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ACTIVE CONSTELLATION OPERATORS (TANGLED ROPE) — Mix of coordination and extraction. Genuine coordination: all operators benefit from shared debris-tracking infrastructure (Space Force SSA, ESA tracking). Asymmetric extraction: established operators benefit from first-mover advantage and grandfathered operational standards; new operators must meet stricter deorbiting requirements. Constrained by regulatory compliance and insurance costs but possess market power and exit options (migrate to GEO, cease operations with asset value retention).
constraint_indexing:constraint_classification(leo_debris_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT OPERATORS (ROPE) — Primary beneficiaries. Integrated tracking and avoidance systems protect existing constellations and increase their competitive moat against new entrants. Debris mitigation regulations create barriers to market entry that protect incumbent market share. Experience the constraint as coordination (orbital safety management) with asymmetric benefits — their market position is strengthened. High arbitrage: can shift operations to GEO or retire with retained asset value.
constraint_indexing:constraint_classification(leo_debris_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEBRIS MITIGATION RESEARCHERS (TANGLED ROPE) — Organized agents benefit from debris accumulation that drives research funding and policy attention. Genuine coordination function: active debris removal and collision avoidance are collective action problems requiring shared standards. Asymmetric extraction: research funding concentrates where debris is densest (incumbent operators' orbital regions); removal technology development is controlled by well-capitalized firms and government agencies. Coalition has agency but constrained by dependence on operator funding and space launch infrastructure.
constraint_indexing:constraint_classification(leo_debris_accumulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COORDINATION BODIES (PITON) — ITU, IADC, and UN COPUOS maintain the debris tracking and deorbiting frameworks, but compliance is voluntary and enforcement is performative. Theater ratio reflects gap between formal protocols (5-year deorbiting standards, collision avoidance maneuvers) and actual compliance (estimated 40-60% of defunct satellites remain in orbit). The coordination system persists through institutional inertia and diplomatic ritual — it provides tracking data and standards but lacks enforcement mechanism. Degraded function: the institutional framework generates theater of coordination without preventing debris accumulation.
constraint_indexing:constraint_classification(leo_debris_accumulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a long-term/global perspective, orbital debris is treated as an immutable consequence of space use — 'you cannot access space without creating debris.' This naturalizes what is structurally a choice about regulatory standards, operator accountability, and cost allocation. The false summit: debris is not inherent to space access but to unpriced externalities and grandfathered operational freedom. The natural law framing obscures that debris accumulation is driven by institutional arrangements (no liability framework, voluntary compliance, regulatory forbearance on existing operators).
constraint_indexing:constraint_classification(leo_debris_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(leo_debris_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(leo_debris_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(leo_debris_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(leo_debris_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(leo_debris_accumulation, TR),
    TR >= 0.70.

:- end_tests(leo_debris_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. New entrant operators bear structural cost disadvantage (estimated 15-40% higher operational costs due to debris tracking, collision insurance, and compliance overhead). However, the extraction is not as severe as snare-level (ε ≥ 0.46, χ ≥ 0.66) would suggest for victims because incumbent operators also incur debris mitigation costs — the commons is degraded for all actors, not just new entrants. The 0.58 value reflects that new entrants face a higher cost floor than incumbents who amortized tracking systems over decades of operations. Suppression (0.72): High. Entry barriers include specialized tracking infrastructure (requiring government SSA access or expensive private systems), collision insurance (10-50x higher premiums for high-debris orbits), and regulatory compliance (5-year deorbiting standards, pre-launch collision risk assessment). These barriers are structural but not absolute — they can be paid at cost, creating constrained rather than trapped exit for well-capitalized operators. The 0.72 reflects that entry is possible but expensive and heavily regulated. Theater ratio (0.45): Moderate. ITU Radio Regulations, IADC guidelines, and UN COPUOS frameworks provide substantive protocols. However, compliance is voluntary — estimated 40-60% of defunct satellites remain in orbit beyond their 5-year deorbiting window, indicating significant gap between protocol and actual practice. Theater reflects this compliance gap without being as high as piton-level (ε ≤ 0.25, theater ≥ 0.70) because the protocols do drive observable behavior (collision avoidance maneuvers are performed; debris tracking is funded).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural phenomenon can appear as pure coordination (rope), pure extraction (snare), or mixed (tangled rope) depending on the observer's power and exit options. Incumbents experience coordination benefits and low cost (rope). New entrants experience extraction and high cost (snare). The analytical observer risks seeing the debris field as a mountain — natural consequence of space use — when it is actually a tangled rope whose extraction is sustainable only because enforcement is weak and regulatory asymmetry protects beneficiaries from liability.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is determined by their power level, exit options, and structural relationship to debris accumulation. Incumbent operators with institutional power and arbitrage options (can migrate to GEO, cease operations, absorb costs) derive low d and experience coordination benefits. New entrants with powerless status and trapped options (cannot access space without absorbing debris costs) derive high d and experience extraction. The shared orbital environment derives maximum d because it is powerless and has no exit option — it is the pure victim. Researchers benefit from debris funding despite constrained exit, creating moderate d with beneficiary status. Coordination bodies maintain arbitrage (diplomatic role, research funding) without enforcement responsibility, creating low d and rope experience. The asymmetry between incumbents' grandfathered operational freedom and new operators' strict compliance standards is the core extraction mechanism. This asymmetry could be eliminated by retroactive liability or harmonized standards, but such changes face incumbent resistance — the extraction persists because it is institutionally protected, not because it is technically necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that LEO debris is genuinely a tangled rope: it coordinates orbital safety (shared tracking infrastructure, collision avoidance protocols, deorbiting standards) AND extracts asymmetrically (incumbents protected by grandfathered standards; new entrants bear compliance costs). The constraint is not purely extractive (which would make it a snare) because coordination function is real — debris tracking benefits all operators. It is not purely coordinating (which would make it rope) because the regulatory asymmetry creates systematic cost advantage for incumbents. The false summit (mountain classification) naturalizes the debris field as immutable, obscuring that it is a policy choice. The piton classification (coordination bodies) is accurate — the institutional framework is substantially performative; it maintains the forms of coordination while failing to prevent debris accumulation. The decomposition into separate stories is NOT warranted here because all the diverse structural relationships (coordination, extraction, degradation) are aspects of the same constraint mechanism: unpriced orbital externalities + grandfathered regulatory standards + weak international enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kessler_syndrome_threshold_ambiguity,
    'At what debris density threshold does cascade probability become self-sustaining, and has that threshold been reached in LEO?',
    'Empirical collision rate modeling; comparison of NASA/ESA debris cascade simulations under current and projected debris densities; measurement of cascade-triggering probability at observed orbital population levels',
    'If threshold already exceeded: debris field is irreversibly self-generating (mountain-type immutability). If threshold not yet reached: debris accumulation is still a policy choice (tangled rope remains accurate). Classification outcome: mountain vs tangled_rope hinges on this parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kessler_syndrome_threshold_ambiguity, empirical, 'Whether Kessler cascade threshold has been reached in LEO').

omega_variable(
    active_removal_cost_effectiveness,
    'Can active debris removal (ADR) technology reduce debris density faster than new satellite launches increase it, and at what cost ratio?',
    'Techno-economic analysis of ADR mission costs vs launch rate projections; modeling of debris density trajectory under various ADR/launch scenarios; comparison of removal cost per unit debris vs cost to absorb via satellite hardening',
    'If ADR cost-effective: debris mitigation is a solvable coordination problem (scaffold with sunset, or rope with governance). If ADR prohibitively expensive: debris accumulation becomes structural constraint on future space access (snare for new entrants becomes permanent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(active_removal_cost_effectiveness, empirical, 'Cost-effectiveness of active debris removal relative to launch growth').

omega_variable(
    liability_framework_enforceability,
    'Can an orbital debris liability regime (charging operators for debris creation/retention) be enforced without fragmenting space governance and driving operators to non-compliant jurisdictions?',
    'Analysis of existing inter-state liability precedents (shipping pollution, air emissions); modeling of regulatory arbitrage incentives; assessment of enforcement mechanisms for transnational space activities',
    'If enforceable: debris becomes priced, extraction mechanism converts to coordination mechanism, tangled_rope → rope transition possible. If unenforceable: extraction persists, incumbent operators retain grandfathered advantage indefinitely (tangled_rope remains structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_framework_enforceability, conceptual, 'Whether orbital debris liability can be enforced internationally').

omega_variable(
    incumbent_operator_coordination_necessity,
    'Is the incumbent operator integration with debris tracking genuinely necessary for orbital safety, or is it primarily a competitive moat?',
    'Comparative analysis of collision avoidance outcomes for operators with and without integrated tracking; measurement of cost reduction from proprietary tracking vs public SSA use; assessment of whether fragmented operators could achieve equivalent safety via public tracking systems',
    'If genuinely necessary: tangled_rope classification is correct — coordination function is real. If primarily competitive moat: extraction dominates, classification should shift toward snare (higher d for new entrants).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_operator_coordination_necessity, empirical, 'Whether incumbent tracking integration is necessary for safety or primarily competitive differentiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(leo_debris_accumulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(leo_debris_tr_t0, leo_debris_accumulation, theater_ratio, 0, 0.32).
narrative_ontology:measurement(leo_debris_tr_t10, leo_debris_accumulation, theater_ratio, 10, 0.39).
narrative_ontology:measurement(leo_debris_tr_t20, leo_debris_accumulation, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(leo_debris_be_t0, leo_debris_accumulation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(leo_debris_be_t10, leo_debris_accumulation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(leo_debris_be_t20, leo_debris_accumulation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(leo_debris_accumulation, global_infrastructure).
narrative_ontology:boltzmann_floor_override(leo_debris_accumulation, 0.18).
narrative_ontology:affects_constraint(leo_debris_accumulation, commercial_space_access_barriers).
narrative_ontology:affects_constraint(leo_debris_accumulation, international_space_governance_fragmentation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(leo_debris_accumulation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
