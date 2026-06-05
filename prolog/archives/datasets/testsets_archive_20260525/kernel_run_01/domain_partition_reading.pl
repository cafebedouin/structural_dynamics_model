% ============================================================================
% CONSTRAINT STORY: domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_domain_partition_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition (Compartmentalized Practice Reading)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   The domain partition reading interprets the coexistence of kami and
 *   buddha in Japanese religious practice as a functional
 *   compartmentalization: kami govern life, purity, and fortune; buddhas
 *   govern death and the afterlife. Each domain occupies a separate ritual
 *   niche without requiring ontological unification. This reading emerges
 *   from the institutional formalization of shrine-temple coexistence,
 *   particularly the Meiji-era separation of kami and buddhas
 *   (Shinto-Buddhism separation). The constraint is one reading of the
 *   contested kernel 'kami-buddha ontology' — it claims that the domains are
 *   genuinely separate, that this separation solves a coordination problem
 *   (enabling both systems to coexist without zero-sum competition), and that
 *   the partition is maintained through lay practice and institutional
 *   reinforcement without requiring heavy coercive overhead. The domain
 *   partition reading classifies as Rope from lay and institutional
 *   perspectives (functional coordination, low extraction, low suppression)
 *   but as Piton from a civilizational perspective (institutional inertia,
 *   increasing theater ratio as theological integration pressures modernize
 *   the landscape). The analytical observer sees pure coordination: a
 *   solution to the problem of housing two distinct soteriological frameworks
 *   within a single religious ecology. The constraint's low extractiveness
 *   (0.18) and low suppression (0.12) reflect that no party experiences heavy
 *   domination — shrines and temples each occupy a stable niche,
 *   practitioners can operate the partition intuitively through household
 *   ritual practice, and the cognitive boundary is reinforced through
 *   ordinary behavior rather than coercion.
 *
 * KEY AGENTS:
 *   - Household Practitioners (moderate/mobile): Operate separate altars (kamidana and butsudan); experience the partition as functional coordination enabling them to practice both traditions without contradiction
 *   - Shrine Institutions (institutional/arbitrage): Occupy life-stage ritual niche; benefit from the partition without enforcing it heavily; have exit options (could attempt to expand into death rites) but find the partition stable and non-extractive
 *   - Temple Institutions (institutional/constrained): Occupy death-ritual niche; constrained by domain partition at generational scope but also protected by guaranteed jurisdiction; face institutional suppression if attempting to expand into life-stage domains
 *   - Buddhist Modernizers (moderate/constrained): Generational actors attempting to expand Buddhist practice beyond death rites; experience the partition as suppression of institutional growth; see modernization of lay belief as justification for domain expansion
 *   - Analytical Observer (analytical/analytical): Views the partition as a genuine structural solution to coordinating two ontologically distinct systems without requiring synthesis or domination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_partition_reading, 0.18).
domain_priors:suppression_score(domain_partition_reading, 0.12).
domain_priors:theater_ratio(domain_partition_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_partition_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(domain_partition_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(domain_partition_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_partition_reading, rope).
narrative_ontology:human_readable(domain_partition_reading, "Kami-Buddha Domain Partition (Compartmentalized Practice Reading)").
narrative_ontology:topic_domain(domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(domain_partition_reading, fixed_text).
narrative_ontology:cs_authority_grounding(domain_partition_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(domain_partition_reading).
narrative_ontology:cs_kernel_id(domain_partition_reading, kami_buddha_ontology).
narrative_ontology:cs_reading_relation(domain_partition_reading, fused_ontology_reading, coexists_with).
narrative_ontology:cs_reading_relation(domain_partition_reading, parallel_entities_reading, influences).
narrative_ontology:cs_axiom(domain_partition_reading, foundational, domains_genuinely_distinct).
narrative_ontology:cs_axiom_status(domains_genuinely_distinct, holdable).
narrative_ontology:cs_axiom(domain_partition_reading, foundational, partition_solves_coordination).
narrative_ontology:cs_axiom_status(partition_solves_coordination, holdable).
narrative_ontology:cs_reference_frame(domain_partition_reading, compartmentalized_ritual_domains).
narrative_ontology:cs_drift_state(domain_partition_reading, contemporary_lay_syncretism, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_partition_reading, household_ritual_practitioners).
narrative_ontology:constraint_beneficiary(domain_partition_reading, shrine_temple_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD PRACTITIONER (ROPE) — The domain partition is experienced as functional coordination. The separate kamidana (kami altar) and butsudan (buddha altar) in the same household solve a practical coordination problem: life-cycle events are partitioned cleanly (kami for birth, coming-of-age, marriage, general fortune; buddha for death and ancestor veneration). The practitioner exits easily if desired — compartmentalization requires minimal coercive overhead. Low suppression, low extraction. The constraint is a coordination mechanism that the practitioner can internalize and operate without experiencing domination.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: SHRINE-TEMPLE ECOSYSTEM (ROPE) — Institutional actors (shrines, temples, ritual specialists) benefit from domain compartmentalization without exercising heavy enforcement. Each institution occupies a distinct ritual niche; the partition enables coexistence without zero-sum competition. Shrines manage life-stage rites; temples manage death rites. The extractiveness experienced by the institution is minimal — the partition itself is the benefit, not coercive extraction of surplus. Arbitrage exit options: a shrine could attempt to incorporate death rites, or a temple could attempt to include marriage ceremonies, but the partition makes their current niche stable. No active enforcement is required because the cognitive boundary is self-reinforcing through ritual practice.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: BUDDHIST INSTITUTIONAL MODERNIZER (TANGLED ROPE) — At generational scope, institutional actors attempting to expand Buddhist practice beyond death rites into life-stage ceremonies face significant barriers. The domain partition constrains institutional growth — temples cannot easily claim jurisdiction over marriage ceremonies or coming-of-age rites without disrupting the established pattern. Yet the constraint also coordinates: temples are assured they will retain death ritual monopoly. The modernizer faces suppression (established practice, lay expectation, shrine dominance in life-stage domains) but also genuine coordination benefits (predictable ritual niche, not all-or-nothing competition with shrines). Generational scope reveals that the partition creates asymmetric extraction: it freezes temple jurisdiction while allowing gradual shrine institutionalization. The constraint requires some active enforcement (teaching the partition, reinforcing domain boundaries in ritual practice) but less coercive overhead than total institutional competition would require.
constraint_indexing:constraint_classification(domain_partition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FORMALIZED INSTITUTIONAL STRUCTURE (PITON) — At civilizational scope, the domain partition appears as an institutional inertia. The medieval and Edo-period formalization of shrine-temple coexistence (including the 1872 Meiji separation of kami and buddhas) created a durable institutional boundary that persists in modern practice. Contemporary shrine and temple institutions maintain the partition even when competitive pressures and theological modernization might support recombination. The theater_ratio is moderate (0.35): the ritual performance of separate domains is genuine (not purely theater), but much of the institutional maintenance of the partition persists through performative reinforcement (separate altars, separate ritual specialists, separate institutional budgets) rather than through active theological or practical necessity. The constraint has atrophied in functional importance — modern practitioners often hold syncretic beliefs — but institutional practice persists. This is piton: a former coordination mechanism (Rope) whose primary function has partly degraded, but which persists through institutional and cultural inertia.
constraint_indexing:constraint_classification(domain_partition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / FUNCTIONAL PARTITION VIEW (ROPE) — From the analytical position at civilizational scope, the domain partition solves a genuinely structural coordination problem: how can two ontologically distinct systems (kami-oriented animism and buddha-oriented soteriology) coexist in a single religious ecology without either dominating or requiring integration into a higher-order unified framework? The partition is a solution to coordination without ontological reduction. This perspective sees the constraint as pure coordination (Rope) — there is no hidden extraction, no suppression mechanism, no theater. The separate domains are a coherent religious strategy, not a contingent institutional arrangement. Low extractiveness (0.18), low suppression (0.12), low theater (0.35). This reading instantiates one coherent interpretation of the kami-buddha kernel: domain partition as a structural necessity for religious coexistence.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_partition_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(domain_partition_reading, TR),
    TR >= 0.70.

:- end_tests(domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The domain partition constrains neither shrines nor temples heavily — each occupies a distinct niche without zero-sum competition. Practitioners experience the constraint as enabling, not extractive. The low value reflects that genuine coordination benefits exist: shrines and temples coexist without requiring either to subsume the other. The modest rise from 0.12 to 0.18 over the interval reflects slight institutional enforcement pressure as theological modernization (lay syncretism, academic interest in integration) creates modest pressures to maintain the boundary. Suppression (0.12): Very low. There are no major barriers to practitioners or institutions exiting the partition — shrines could attempt to incorporate death rites, temples could incorporate life-stage rites, practitioners could adopt syncretic beliefs. The partition persists because it is coordination rather than domination. Theater ratio (0.35): Moderate-low. The ritual performance of separate domains is genuine (kamidana and butsudan are actually present in households, separate specialists and institutions do conduct different rites), but a modest component is performative: institutional maintenance of the partition through administrative separation, specialized licensing, and formal boundary-marking. The rise from 0.25 to 0.35 reflects increasing performative effort as underlying lay belief becomes more syncretic and institutional actors work harder to maintain the boundary. Claimed type (Rope): Reflects the coordination function — the partition solves a structural problem (housing two systems without integration) with minimal extraction or coercion.
 *
 * PERSPECTIVAL GAP:
 *   The domain partition reading exhibits a gap between lay/institutional Rope perspectives (coordination experienced as functional) and civilizational Piton perspective (institutional inertia, increasing theater). The household practitioner sees natural coordination: separate altars in the same home make intuitive sense and require no complex enforcement. Shrine and temple institutions see stable niches without heavy extraction. Buddhist modernizers at generational scope experience suppression: the partition constrains institutional growth even as lay syncretism undermines its theological basis. The analytical observer sees this gap as diagnostic: the constraint is genuinely coordination (Rope) at biographical scope but increasingly requires performative reinforcement (Piton) at civilizational scope, as underlying lay belief diverges from institutional compartmentalization. This gap reveals that the domain partition is functioning as coordination so long as practitioners internalize the cognitive boundary, but becomes theatrical maintenance once that internalization breaks down.
 *
 * DIRECTIONALITY LOGIC:
 *   The domain partition is experienced as having near-zero directionality: neither shrines nor temples are being extracted from; practitioners are not trapped or heavily suppressed. The beneficiary and victim declarations are weak in this Rope constraint — all parties benefit from the coordination function (no zero-sum competition between shrines and temples), and there is no clear victim except at generational scope where Buddhist institutions face growth suppression. This weakness is appropriate: in pure coordination (Rope), the directionality is symmetric or near-symmetric because the constraint solves a shared problem. Shrine-temple coexistence benefits both institutions; practitioners benefit from having a cognitive framework for operating multiple traditions simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain-partition reading resolves mandatrophy by anchoring on the genuine coordination function: the partition enables two distinct ontological systems to coexist without requiring synthesis or domination. This reading does NOT claim that kami and buddhas are ontologically unified or fused — those are sibling readings instantiating different solutions to the kami-buddha kernel. The domain-partition reading claims that the kernel permits compartmentalized solutions: the separate domains are not a failure to integrate but a functional choice that solves a coordination problem. Mandatrophy is avoided by being clear about what this reading claims and does not claim: it claims coordination without asserting that other readings (fused ontology, parallel entities) are impossible or illegitimate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_integration_pressure,
    'Is the domain partition an irreducible structural feature of kami-buddha coexistence, or does it require active suppression of ontological integration to maintain?',
    'Historical analysis of theological syncretism movements (e.g., honji suijaku theory, concurrent exploration of unified frameworks); examination of lay belief surveys for actual ontological separation vs intellectual compartmentalization; documentation of institutional resistance to integration proposals',
    'If irreducible: partition is true Rope (coordination without suppression). If requiring suppression: constraint reclassifies toward Tangled Rope or Snare depending on beneficiary power. If integration is already occurring: constraint is degraded toward Piton, and theater_ratio should rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_integration_pressure, empirical, 'Whether domain partition is structurally necessary or maintained through active suppression of integration').

omega_variable(
    lay_syncretic_belief_structure,
    'Do lay practitioners actually experience kami and buddhas as ontologically separate entities, or do they hold syncretic/fused beliefs that the institutional partition obscures?',
    'Ethnographic interview data with household practitioners; analysis of prayer formulations and belief articulations; comparison of institutionally-taught compartmentalization with actual lay reasoning about kami-buddha relationships',
    'If practitioners maintain actual cognitive separation: institutional partition coordinates genuine lay practice (Rope confirmed). If practitioners hold syncretic beliefs: institutional partition is theater maintaining a false boundary (theater_ratio rises, constraint reclassifies toward Piton or Tangled Rope with identity_locked exit for practitioners forced to perform separation they don''t believe).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lay_syncretic_belief_structure, empirical, 'Whether lay beliefs actually instantiate ontological separation or syncretic fusion').

omega_variable(
    institutional_enforcement_opacity,
    'Is the domain partition self-reinforcing through lay practice, or does it require active institutional enforcement (shrine-temple gatekeeping, specialized ritual knowledge, institutional authority)?',
    'Documentation of enforcement mechanisms: licensing of ritual specialists, institutional control over ritual performance, educational emphasis on domain boundaries, institutional response to violations of partition (e.g., temples performing birth rites)',
    'If self-reinforcing through practice: extractiveness and suppression remain low (true Rope). If requiring institutional enforcement: suppression rises (0.12→0.35+), constraint moves toward Tangled Rope or Snare depending on enforcement intensity and victim status of constrained parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_opacity, empirical, 'Whether domain partition is self-reinforcing or requires active institutional enforcement').

omega_variable(
    reading_ontological_status,
    'Is the domain-partition reading a genuine ontological claim about the structure of Japanese religiosity, or is it primarily a reading imposed by scholars and institutional actors for organizational convenience?',
    'Historical-textual analysis of pre-modern theological sources; examination of when domain-partition language appears in religious texts and institutional documents; comparison with sibling readings'' textual bases',
    'If genuine ontological feature: reading is holdable and foundational (the partition captures real structure). If organizational imposition: reading should shift from ''foundational'' to ''secondary'' axiom status, and the constraint may represent institutional power disguised as coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_ontological_status, conceptual, 'Whether domain partition is genuine ontological structure or organizational convenience').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_partition_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doma_tr_t0, domain_partition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(doma_tr_t100, domain_partition_reading, theater_ratio, 100, 0.3).
narrative_ontology:measurement(doma_tr_t200, domain_partition_reading, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(doma_be_t0, domain_partition_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(doma_be_t100, domain_partition_reading, base_extractiveness, 100, 0.16).
narrative_ontology:measurement(doma_be_t200, domain_partition_reading, base_extractiveness, 200, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(domain_partition_reading, fused_ontology_reading).
narrative_ontology:affects_constraint(domain_partition_reading, parallel_entities_reading).

% DUAL FORMULATION NOTE:
% The kami-buddha kernel has at minimum three structurally distinct constraint readings: (1) domain_partition_reading (this file): compartmentalized domains, low extraction, Rope. (2) fused_ontology_reading: syncretic integration, extracted extraction from competing frameworks, likely Tangled Rope or Snare. (3) parallel_entities_reading: distinct entities in non-hierarchical coexistence, varying extraction depending on institutional power relationships. Each reading gets its own constraint_id and its own ε value. They are linked through network.affects_constraints to enable contamination propagation analysis: if practitioners adopt fusion reading, the domain partition's coordination function degrades (theater rises), and the constraint mutates toward Piton. If institutions enforce fusion despite lay syncretism, the constraint may reclassify toward Tangled Rope with identity_locked exit for practitioners forced to perform compartmentalization they don't believe.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
