% ============================================================================
% CONSTRAINT STORY: false_mountain_persistence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_false_mountain_persistence, []).

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
 *   constraint_id: false_mountain_persistence
 *   human_readable: False Mountain Persistence in Constraint Misclassification
 *   domain: social_epistemology/power_indexed_extraction/collective_action
 *
 * SUMMARY:
 *   False mountain persistence is the structural mechanism by which powerless
 *   agents misclassify high-extraction constructed constraints as natural
 *   law, thereby blocking collective action that could transform the
 *   constraint type. This is a meta-constraint: it operates on the
 *   classification process itself, not on a specific domain constraint. The
 *   constraint exhibits tangled rope structure because it serves a genuine
 *   coordination function (not all constraints should be constantly
 *   questioned — some naturalization reduces transaction costs) while
 *   enabling asymmetric extraction (enforcers capture authority rents by
 *   naturalizing mutable constraints to block challenges). The theater ratio
 *   (0.58) reflects the degradation of formal epistemology apparatus:
 *   academic gatekeepers perform objectivity rituals (peer review,
 *   credentialing) without delivering functional verification of constraint
 *   mutability claims. The extractiveness has increased over the interval
 *   (0.32 → 0.48) as enforcers have layered additional naturalization
 *   mechanisms (complexity obfuscation, credentialism, algorithmic opacity)
 *   onto the base coordination function.
 *
 * KEY AGENTS:
 *   - Trapped Agents: Primary victims (powerless/trapped) — experience the misclassification as pure extraction; cannot distinguish constructed from natural constraints due to epistemic isolation
 *   - Constraint Enforcers: Primary beneficiaries (institutional/arbitrage) — capture authority rents by maintaining naturalization narratives; may genuinely believe coordination justification
 *   - Constrained Organizers: Secondary victims (moderate/constrained) — have partial epistemic access but face high costs to challenge naturalization; mixed experience of coordination need and extraction
 *   - Consciousness-Raising Movement: Organized agents (organized/mobile) — building alternative epistemic infrastructure with sunset logic; see false mountain persistence as temporary coordination failure
 *   - Academic Gatekeepers: Institutional actors (institutional/constrained) — maintain degraded epistemology apparatus; perform objectivity without delivering verification (piton perspective)
 *   - Meta-Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint's true tangled rope structure; recognizes both coordination function and extractive abuse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(false_mountain_persistence, 0.48).
domain_priors:suppression_score(false_mountain_persistence, 0.62).
domain_priors:theater_ratio(false_mountain_persistence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(false_mountain_persistence, extractiveness, 0.48).
narrative_ontology:constraint_metric(false_mountain_persistence, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(false_mountain_persistence, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(false_mountain_persistence, tangled_rope).
narrative_ontology:human_readable(false_mountain_persistence, "False Mountain Persistence in Constraint Misclassification").
narrative_ontology:topic_domain(false_mountain_persistence, "social_epistemology/power_indexed_extraction/collective_action").

domain_priors:requires_active_enforcement(false_mountain_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(false_mountain_persistence, constraint_enforcers).
narrative_ontology:constraint_beneficiary(false_mountain_persistence, institutional_gatekeepers).
narrative_ontology:constraint_victim(false_mountain_persistence, trapped_agents).
narrative_ontology:constraint_victim(false_mountain_persistence, collective_action_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED AGENT (SNARE) — Experiences the misclassification mechanism as pure extraction. Cannot distinguish constructed constraints from natural law due to epistemic isolation and suppression of counter-narratives. The naturalization itself is the binding mechanism — resistance appears futile because the constraint is perceived as immutable. Maximum experienced extraction because the agent has internalized the false mountain framing and abandoned agency.
constraint_indexing:constraint_classification(false_mountain_persistence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED ORGANIZER (TANGLED ROPE) — Sees both the coordination problem (how to communicate that the constraint is mutable) and the extraction mechanism (enforcers benefit from naturalization). Has partial epistemic access — can see the constraint is constructed but faces high costs to challenge it. Benefits from occasional successful reframing but bears significant risk of retaliation. Mixed experience: genuine coordination need (building shared understanding) alongside asymmetric extraction (enforcers punish truth-telling).
constraint_indexing:constraint_classification(false_mountain_persistence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONSTRAINT ENFORCER (ROPE) — Experiences the naturalization mechanism as coordination: maintaining social order requires shared narratives about what is changeable vs fixed. The enforcer genuinely believes they are solving a coordination problem (preventing chaos from constant renegotiation of social rules). Net beneficiary — extraction flows toward this agent through preserved authority and reduced challenge costs. Low effective extraction because the agent has exit options and experiences the constraint as functional.
constraint_indexing:constraint_classification(false_mountain_persistence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSCIOUSNESS-RAISING MOVEMENT (SCAFFOLD) — Organized agents building alternative epistemic infrastructure (critical pedagogy, consciousness-raising groups, counter-narrative platforms) see the false mountain persistence as a temporary coordination failure with a sunset. As trapped agents gain access to comparative analysis and historical examples of constraint transformation, the naturalization mechanism loses force. Estimated sunset: 15-30 years for epistemic infrastructure to mature and reach critical mass. Low effective extraction because the movement has agency and sees a path to dissolving the misclassification.
constraint_indexing:constraint_classification(false_mountain_persistence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC GATEKEEPER (PITON) — The formal epistemology apparatus (peer review, credentialing, citation networks) that once functioned to distinguish natural law from constructed constraint has degraded into theater. Gatekeepers maintain the ritual of 'objective analysis' while actually enforcing disciplinary boundaries and protecting institutional authority. The review process persists through inertia despite low functional verification of constraint mutability claims. Theater ratio reflects that the apparatus performs objectivity without delivering it.
constraint_indexing:constraint_classification(false_mountain_persistence, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: META-ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the false mountain persistence is itself a tangled rope: it serves a genuine coordination function (reducing transaction costs of constant renegotiation) while enabling asymmetric extraction (enforcers capture authority rents). The analytical observer sees both the functional role of some naturalization (not all constraints should be constantly questioned) and the extractive abuse (enforcers naturalize mutable constraints to block challenges). This is the constraint's true type — a hybrid that cannot be cleanly separated into pure coordination or pure extraction.
constraint_indexing:constraint_classification(false_mountain_persistence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(false_mountain_persistence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(false_mountain_persistence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(false_mountain_persistence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(false_mountain_persistence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(false_mountain_persistence, TR),
    TR >= 0.70.

:- end_tests(false_mountain_persistence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Constraint enforcers capture significant authority rents by naturalizing mutable constraints, and trapped agents waste agency by not attempting resistance. However, the extraction is not maximal because some naturalization serves a genuine coordination function — not all constraints should be constantly questioned. The value reflects that the career and authority asymmetry is real and substantial, but partly justified by coordination benefits. Suppression (0.62): High. Significant barriers to recognizing false mountains include epistemic isolation (trapped agents lack access to comparative analysis), credentialism (only 'experts' can judge mutability), complexity obfuscation (enforcers make constraints appear more technical than they are), and retaliation risk (organizers who challenge naturalization face social and economic costs). Theater ratio (0.58): Moderate-high. The formal epistemology apparatus (peer review, academic credentialing, citation networks) is substantially performative. Gatekeepers assess credentials and disciplinary conformity but do not functionally verify constraint mutability claims. The theater has increased over the interval as complexity and credentialism have outpaced genuine verification capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism appears as different types depending on the observer's epistemic access and power position. Trapped agents see pure extraction (Snare) — the naturalization blocks their agency with no compensating benefit. Constrained organizers see mixed coordination and extraction (Tangled Rope) — they recognize both the genuine coordination need and the extractive abuse. Constraint enforcers see pure coordination (Rope) — they genuinely believe they are maintaining social order. The consciousness-raising movement sees a temporary problem with a sunset (Scaffold) — alternative epistemic infrastructure is dissolving the naturalization mechanism. Academic gatekeepers see their own degraded ritual (Piton) — the objectivity apparatus persists through inertia despite low functional verification. The meta-analytical observer sees the constraint's true type (Tangled Rope) — a hybrid that cannot be cleanly separated into coordination or extraction. The perspectival gap is diagnostic: if all agents saw the same type, the constraint would not be a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Trapped agents are full victims with no exit options — they bear maximum extraction because the naturalization mechanism blocks their agency entirely. The misclassification is the extraction: by perceiving the constraint as immutable, they abandon resistance attempts that could succeed. Constrained organizers are partial victims with some epistemic access — they see the constraint is constructed but face high costs to challenge it. They experience moderate extraction because they have some agency but limited exit. Constraint enforcers are primary beneficiaries with arbitrage exit — they capture authority rents and can exit to other enforcement roles if challenged. They experience low or negative effective extraction because the naturalization mechanism subsidizes their position. The consciousness-raising movement has mobile exit and organized power — they experience low extraction because they are building alternative pathways and have collective agency. Academic gatekeepers are institutional actors with constrained exit — they maintain the degraded apparatus but are partly trapped by their own credentialing investments. The meta-analytical observer has analytical exit and sees the full structure — they experience the constraint as a tangled rope with both coordination and extraction components.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating that the coordination function (reducing renegotiation costs through some naturalization) and the extraction mechanism (enforcers naturalizing mutable constraints to block challenges) are structurally inseparable. The constraint cannot be decomposed into a pure coordination component (Rope) and a pure extraction component (Snare) because the same naturalization act serves both functions simultaneously. Whether a specific naturalization instance is 'legitimate coordination' or 'extractive abuse' depends on whether the underlying constraint is genuinely immutable — but determining that requires the very epistemic access the naturalization mechanism suppresses. The tangled rope classification captures this irreducible ambiguity: the constraint is BOTH coordination AND extraction, and the boundary between them is itself contested and power-indexed. The omega variable 'coordination_extraction_boundary' formalizes this irreducible uncertainty. The false mountain persistence is a tangled rope because it is a coordination mechanism that enables extraction, and the extraction is what makes the coordination 'work' (by preventing constant challenges that would impose transaction costs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_threshold,
    'What level of epistemic isolation is sufficient to sustain false mountain misclassification despite contradictory evidence?',
    'Experimental measurement of Type I error rates across varying information access conditions; longitudinal tracking of misclassification persistence after counter-narrative exposure',
    'If threshold is low (minimal isolation sufficient): extraction mechanism is robust and hard to disrupt. If threshold is high (requires extreme isolation): consciousness-raising interventions are effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_threshold, empirical, 'Epistemic isolation threshold for sustaining false mountain misclassification').

omega_variable(
    coordination_extraction_boundary,
    'Where is the boundary between legitimate naturalization (reducing renegotiation costs) and extractive naturalization (blocking justified challenges)?',
    'Comparative analysis of constraint transformation outcomes: which naturalized constraints, when challenged, revealed genuine mutability vs which challenges produced net harm through coordination collapse',
    'If boundary is narrow: most naturalization is extractive and should be challenged. If boundary is wide: many challenges waste collective resources on genuinely immutable constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, preference, 'Boundary between legitimate and extractive naturalization').

omega_variable(
    collective_action_recovery_rate,
    'Once trapped agents recognize a false mountain, how quickly can collective action capacity recover from the wasted agency period?',
    'Historical case studies of successful consciousness-raising movements; measurement of time lag between recognition and effective mobilization',
    'If recovery is fast (< 5 years): false mountain persistence is costly but reversible. If recovery is slow (> 20 years): the extraction compounds through lost generations of potential organizers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_recovery_rate, empirical, 'Recovery rate of collective action capacity after false mountain recognition').

omega_variable(
    enforcer_belief_sincerity,
    'Do constraint enforcers genuinely believe the naturalization narratives they propagate, or is the naturalization cynically deployed?',
    'Behavioral analysis of enforcers in private vs public contexts; measurement of consistency between stated beliefs and resource allocation decisions',
    'If sincere: enforcers are also victims of the misclassification (identity_locked). If cynical: enforcers are pure extractors and the tangled rope classification overstates coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcer_belief_sincerity, empirical, 'Sincerity of enforcer belief in naturalization narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(false_mountain_persistence, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmp_tr_t0, false_mountain_persistence, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fmp_tr_t10, false_mountain_persistence, theater_ratio, 10, 0.48).
narrative_ontology:measurement(fmp_tr_t20, false_mountain_persistence, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(fmp_be_t0, false_mountain_persistence, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fmp_be_t10, false_mountain_persistence, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(fmp_be_t20, false_mountain_persistence, base_extractiveness, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(false_mountain_persistence, identity_coordination).
narrative_ontology:affects_constraint(false_mountain_persistence, power_indexed_classification_variance).

% DUAL FORMULATION NOTE:
% False mountain persistence is downstream of power_indexed_classification_variance. The upstream constraint establishes that classification varies by observer power; the downstream constraint models the specific mechanism by which powerless observers misclassify high-extraction constraints as natural law. The upstream constraint is a mountain (the variance itself is structural); the downstream constraint is a tangled rope (the persistence mechanism serves coordination and extraction functions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(false_mountain_persistence, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
