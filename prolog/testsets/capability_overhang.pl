% ============================================================================
% CONSTRAINT STORY: capability_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_overhang, []).

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
 *   constraint_id: capability_overhang
 *   human_readable: Capability Overhang: The Asymmetry Between Realized and Unrealized Potential
 *   domain: systems_theory/governance/institutional_dynamics
 *
 * SUMMARY:
 *   Capability overhang describes the structural tension created when
 *   societies, institutions, or systems possess unrealized capabilities that
 *   could address recognized problems but fail to deploy them. The gap
 *   between what-could-be and what-is generates psychological pressure,
 *   institutional friction, and asymmetric extraction. This constraint
 *   operates across scales: from individual organizations (possessing
 *   technical capabilities but lacking authorization or incentives to use
 *   them), to nations (possessing economic or technological capacity but
 *   constrained by political structures), to humanity as a whole (possessing
 *   knowledge to address climate change, pandemic preparedness, poverty but
 *   failing to coordinate implementation). The constraint exhibits all six DR
 *   types from different perspectives, making it diagnostically powerful for
 *   revealing how the same structural phenomenon can appear as immutable law,
 *   temporary coordination failure, extractive mechanism, or pure extraction
 *   depending on the observer's structural position and exit options. The key
 *   distinguishing feature: capability overhang is not about capability
 *   creation but about capability access and distribution. The constraint
 *   operates through suppression of visibility about what's possible and
 *   through information/resource asymmetries that maintain unequal access.
 *
 * KEY AGENTS:
 *   - Resource-Constrained Majority: Primary victims (powerless/trapped) — experience the constraint as inability to access capabilities; trapped by awareness of possibility without access mechanisms
 *   - Capability Controllers: Primary beneficiaries (institutional/arbitrage) — states, corporations, professional bodies that control access; experience the constraint as coordination mechanism and source of discretionary power
 *   - Organized Social Movements: Secondary victims (organized/constrained) — mobilize around capability overhang but face barriers; experience mixed coordination and extraction
 *   - Reformist Institutions: Tertiary actors (powerful/mobile) — government agencies, educational systems, some corporations pushing capability scaling; see sunset mechanisms
 *   - Credentialing and Gatekeeping Systems: Theater maintainers (institutional/arbitrage) — preserve performative meritocracy while maintaining actual capability distribution based on power
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent scientific or economic limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_overhang, 0.68).
domain_priors:suppression_score(capability_overhang, 0.72).
domain_priors:theater_ratio(capability_overhang, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_overhang, extractiveness, 0.68).
narrative_ontology:constraint_metric(capability_overhang, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(capability_overhang, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_overhang, snare).
narrative_ontology:human_readable(capability_overhang, "Capability Overhang: The Asymmetry Between Realized and Unrealized Potential").
narrative_ontology:topic_domain(capability_overhang, "systems_theory/governance/institutional_dynamics").

% --- Structural relationships ---
narrative_ontology:constraint_victim(capability_overhang, resource_constrained_actors).
narrative_ontology:constraint_victim(capability_overhang, social_coordination_systems).
narrative_ontology:constraint_victim(capability_overhang, institutional_adaptability).
narrative_ontology:constraint_victim(capability_overhang, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED MAJORITY (SNARE) — Resource-limited actors and institutions face a structural trap: the knowledge that capabilities *exist* but remain inaccessible creates psychological and material pressure. They cannot exit the awareness of unrealized potential. High suppression from information asymmetry, resource barriers, and institutional gatekeeping. Maximum extraction through the cognitive burden of witnessing possibility without access.
constraint_indexing:constraint_classification(capability_overhang, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED SOCIAL MOVEMENTS (TANGLED ROPE) — Collective organizations perceive both coordination function (capability overhang creates mobilization impetus: 'the system could do better') and asymmetric extraction (those who control capability distribution benefit from the overhang persisting). Movements can mobilize but face substantial resource constraints and institutional resistance. Mixed classification reflects real coordination gains coupled with significant asymmetric extraction.
constraint_indexing:constraint_classification(capability_overhang, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CAPABILITY CONTROLLERS (ROPE) — Institutions that control access to capabilities (states, corporations, professional credentialing bodies) experience the constraint as a coordination mechanism: the overhang itself coordinates behavior by creating scarcity. They have arbitrage options (can reallocate capabilities, can exit by enabling broader access, can maintain the overhang). Net beneficiary position with genuine mobility.
constraint_indexing:constraint_classification(capability_overhang, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORMIST INSTITUTIONS (SCAFFOLD) — Forward-looking institutional actors (some government agencies, some corporations, educational reformers) perceive the overhang as a temporary coordination failure with identifiable sunset mechanisms: skill development initiatives, technology transfer programs, educational expansion, and democratic deepening all represent pathways to reducing the overhang. Mobile agents can exit through scaling these pathways.
constraint_indexing:constraint_classification(capability_overhang, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DEGRADED MERITOCRATIC NARRATIVE (PITON) — The institutional fiction that 'capabilities are allocated by merit and effort' persists long after empirical contradiction. Credentialing systems, competitive frameworks, and advancement narratives maintain performative meritocracy while material capability distribution reflects power, access, and historical advantage. Theater ratio reflects the gap between the meritocratic narrative and actual distribution mechanisms.
constraint_indexing:constraint_classification(capability_overhang, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some capability lag is inherent to complex systems: capabilities are always distributed unevenly because learning, specialization, and knowledge accumulation are inherently local and path-dependent. This view naturalizes the overhang as an unavoidable feature of organized society. However, structural data contradicts mountain classification — this is a false summit, revealing how naturalization disguises contingent institutional arrangements as immutable laws.
constraint_indexing:constraint_classification(capability_overhang, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_overhang_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_overhang, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_overhang, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_overhang, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capability_overhang, TR),
    TR >= 0.70.

:- end_tests(capability_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Capability overhang operates primarily through extraction from those without access to resources or knowledge about capability scaling. The overhang has been increasing over the measurement interval (0.42 → 0.68) as public awareness of unrealized potential has grown without corresponding institutional responsiveness. The extraction mechanism is subtle: constrained actors bear the psychological cost of knowing capabilities exist, the opportunity cost of unrealized benefits, and the direct cost of seeking workarounds or pressuring for access. Theater ratio (0.58): Moderate. Significant performative content in how capability limits are justified ('technical constraints,' 'human capital bottlenecks,' 'safety concerns') when empirical evidence shows similar capabilities can scale rapidly under pressure (wartime production, pandemic response, emergency deployment). The theater is not total — some genuine scalability constraints exist, but the gap between claimed and actual constraints has been widening. Suppression (0.72): High. Multiple suppression mechanisms operate: information asymmetry (constrained actors don't know full capability scope), resource barriers (access to capabilities requires capital, credentials, or networks), institutional gatekeeping (formal barriers to capability distribution), and ideological justification (meritocratic narratives naturalizing unequal capability access). The combination produces high suppression intensity.
 *
 * PERSPECTIVAL GAP:
 *   The gap between powerless/trapped (Snare) and institutional/arbitrage (Rope) perspectives represents the core extraction mechanism. The same capability distribution system appears as entrapment from below and as coordination from above. The scaffold perspective shows a genuine third way: scaling mechanisms that reduce the overhang while maintaining institutional legitimacy. The piton perspective reveals the degradation of merit narratives. The false mountain reveals how naturalization works: claiming that unequal capability distribution is inherent to complexity prevents recognition that institutional choices (not physics) maintain the overhang.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each actor's structural position: constrained actors with trapped exit experience high d (full targets of extraction). Capability controllers with arbitrage options experience low d (they benefit, they can exit by scaling or maintaining overhang). Organized actors with constrained exit but mobilization capacity occupy intermediate d (they experience extraction but have some structural agency). Reformist institutions with mobile exit see low-to-moderate d (they can build scalability bridges). The piton perspective derives from theater ratio rather than high chi — the meritocratic narrative persists through institutional inertia, not because it produces coordination value. The mountain perspective derives from civilization-scale analysis, but is flagged as a false summit — the engine's cross-perspective comparison reveals that the 'immutable law' framing naturalizes contingent institutional choices.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVES MANDATROPHY: The primary challenge is distinguishing capability overhang (high extraction, minimal coordination benefit) from legitimate capability distribution (coordination function, fair allocation). The snare classification is justified because: (1) Primary beneficiaries (capability controllers) do not gain from the constraint's existence as a coordination mechanism but from its maintenance as an extraction mechanism — they could coordinate just as effectively with broader capability access. (2) Primary victims (constrained actors) bear costs without corresponding benefits. (3) The constraint persists through suppression (gatekeeping, information asymmetry, resource rationing), not through coordination necessity. (4) The measurable increase in extractiveness over time (0.42 → 0.68) despite technological capability to reduce the gap suggests intentional overhang maintenance rather than coordination necessity. The organized perspective (Tangled Rope) acknowledges that some coordination value exists — the awareness of unrealized capability does mobilize reform pressure — but this coordination function is secondary to the extraction mechanism. The mandatrophy is resolved by showing that the constraint cannot be reframed as pure coordination without denying the structural benefits to controllers and costs to constrained actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capability_measurement_ambiguity,
    'How should ''unrealized capability'' be measured? Is it potential-that-could-exist-if-resources-were-allocated, or potential-that-exists-but-is-socially-suppressed, or purely speculative future capability?',
    'Longitudinal tracking: compare capabilities that were forecasted as ''unrealized'' with actual deployment timelines and resource requirements; distinguish speculative capability from latent capability through pilot programs and resource stress tests',
    'If overhang is primarily speculative (lower epsilon): constraint is less extractive than measured. If overhang is latent (higher epsilon): constraint is more extractive — resources are being deliberately withheld. If overhang is potential-if-resources-allocated: intermediate extractiveness; the constraint operates through resource rationing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_measurement_ambiguity, empirical, 'How to measure capability overhang: speculative vs latent vs conditional').

omega_variable(
    capability_controller_motivation_heterogeneity,
    'Do capability controllers maintain the overhang through deliberate extraction, institutional inertia, or genuine uncertainty about scalability?',
    'Structural analysis of control mechanisms: are they defended through active suppression (high extraction), through passive institutional friction (piton dynamics), or through genuine technical bottlenecks? Comparison with historical capability deployments showing rapid scaling once political will materializes.',
    'If deliberate extraction: snare classification is correct for all institutional actors. If inertia/theater: piton classification gains strength. If genuine uncertainty: scaffold sunset mechanisms may fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_controller_motivation_heterogeneity, empirical, 'Whether capability controllers deliberately suppress or whether mechanisms are inertial').

omega_variable(
    awareness_suppression_feedback_loop,
    'Does public awareness of capability overhang increase or decrease institutional pressure to close the gap? Or does it create a self-reinforcing trap where awareness without access intensifies extraction?',
    'Comparative institutional analysis: societies/sectors with high public awareness vs low awareness of capability gaps; measurement of policy responsiveness to overhang visibility; psychological impact studies on constrained groups after exposure to capability awareness',
    'If awareness increases pressure: overhang is self-correcting (lower effective extraction). If awareness increases trap: overhang is self-reinforcing (higher effective extraction, potential reclassification to pure snare from organized perspective). If neutral: awareness is theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(awareness_suppression_feedback_loop, empirical, 'Whether awareness of capability overhang increases or decreases institutional responsiveness').

omega_variable(
    capability_overhang_vs_structural_inequality,
    'Is capability overhang a distinct constraint or a manifestation of underlying structural inequality? If distinct, what differentiates it?',
    'Decomposition analysis: remove capability overhang while preserving structural inequality (hypothetical) — does extraction persist? If yes, overhang is secondary manifestation. If no, overhang is primary mechanism. Empirical test: societies with high inequality but low information about capability gaps vs low inequality with high overhang awareness.',
    'If overhang is secondary: this story represents a perspective on structural inequality rather than independent constraint. If overhang is primary: overhang is its own constraint family downstream of resource distribution. Affects network decomposition strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_overhang_vs_structural_inequality, conceptual, 'Whether capability overhang is distinct from or manifestation of structural inequality').

omega_variable(
    scalability_bottleneck_authenticity,
    'When capability controllers claim that capabilities cannot be scaled due to technical, human capital, or institutional constraints, how often is this claim empirically accurate vs deployed as cover for extraction?',
    'Historical case analysis of technology/capability deployments: compare claimed scalability limits with actual scaling achieved under political pressure or wartime conditions; identify false bottleneck patterns (e.g., vaccine production, remote work capability, food production)',
    'If bottlenecks are authentic: scaffold sunset mechanism is slower; overhang is structural not intentional. If bottlenecks are largely theater: overhang is extractive choice; snare classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scalability_bottleneck_authenticity, empirical, 'Whether scalability bottleneck claims are authentic or deployed as extraction cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_overhang, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(capov_tr_t0, capability_overhang, theater_ratio, 0, 0.48).
narrative_ontology:measurement(capov_tr_t10, capability_overhang, theater_ratio, 10, 0.54).
narrative_ontology:measurement(capov_tr_t20, capability_overhang, theater_ratio, 20, 0.58).
narrative_ontology:measurement(capov_tr_t5, capability_overhang, theater_ratio, 5, 0.51).

% Extraction over time
narrative_ontology:measurement(capov_be_t0, capability_overhang, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(capov_be_t10, capability_overhang, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(capov_be_t20, capability_overhang, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(capov_be_t5, capability_overhang, base_extractiveness, 5, 0.49).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_overhang, resource_allocation).
narrative_ontology:affects_constraint(capability_overhang, structural_inequality).
narrative_ontology:affects_constraint(capability_overhang, knowledge_asymmetry).
narrative_ontology:affects_constraint(capability_overhang, institutional_gatekeeping).
narrative_ontology:affects_constraint(capability_overhang, credential_capture).

% DUAL FORMULATION NOTE:
% Capability overhang is downstream of multiple structural constraints (inequality, information asymmetry, gatekeeping) but is distinct from each. It represents the specific mechanism by which unrealized capability becomes an extraction vector. Upstream constraints determine the distribution of capabilities; capability overhang determines the extraction value of maintaining that distribution as a known-but-inaccessible gap. Network links show causal influence: reduce structural inequality and you may reduce capability overhang; improve information flow and you may reduce overhang; reduce gatekeeping and you may reduce overhang. But overhang can persist despite improvements in upstream constraints if institutions strategically maintain the visibility-without-access asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capability_overhang, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
