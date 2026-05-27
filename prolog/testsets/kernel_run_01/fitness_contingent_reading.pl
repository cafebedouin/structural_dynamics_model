% ============================================================================
% CONSTRAINT STORY: fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fitness_contingent_reading, []).

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
 *   constraint_id: fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel
 *   'personhood_boundary': the claim that an entity's membership in the moral
 *   community depends on demonstrating fitness according to criteria
 *   established by an adjudicating authority. This reading classifies as a
 *   snare from the perspective of pre-fitness entities (powerless, trapped,
 *   no exit from the evaluation framework) and from their caregivers
 *   (moderate power, severely constrained). The adjudicating authority itself
 *   experiences coordination (rope) — organizing who counts is a legitimate
 *   function. The philosophical tradition maintaining the criterion shows
 *   piton characteristics — high theater as the criterion is defended against
 *   counterexamples rather than applied to resolve actual cases. The existing
 *   moral community experiences a tangled rope (coordination function mixed
 *   with internal contradiction). The civilizational analytical observer
 *   risks seeing an immutable natural law (mountain) but the structural data
 *   reveals this as a false summit: the criterion is contingent institutional
 *   arrangement with identifiable beneficiaries and systematic exclusion
 *   mechanisms. Extractiveness has risen from 0.52 to 0.68 over the interval
 *   as adjudicating authorities have developed more elaborate fitness tests
 *   and applied them more systematically to previously presumptively-included
 *   groups (disability assessment, economic productivity thresholds). Theater
 *   has risen correspondingly as philosophical defense becomes more elaborate
 *   while actual boundary-setting becomes more arbitrary.
 *
 * KEY AGENTS:
 *   - Pre-fitness Entities (infants, cognitively disabled persons, non-rational beings): Primary victim (powerless/trapped) — bearing the full cost of exclusion from moral community while unable to appeal or demonstrate fitness
 *   - Caregivers (parents, guardians, institutional care staff): Secondary victim (moderate/constrained) — required to provide intensive care without moral recognition of the cared-for entity's worth
 *   - Fitness-Adjudicating Authority (philosophers, state agencies, medical institutions, religious institutions): Primary beneficiary (institutional/arbitrage) — captures the authority to define membership and determine who passes the fitness test
 *   - Existing Moral Community (agents already recognized as having full moral standing): Mixed (powerful/mobile) — both benefits from bounded membership and constrained by having dependents who fail the fitness test
 *   - Philosophical Tradition (schools of thought grounding personhood in demonstrated capacity): Institutional actor (institutional/arbitrage) — maintains the criterion through scholarly work; sees its own tradition as increasingly theatrical but persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional arrangement as an inherent property of personhood itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fitness_contingent_reading, 0.68).
domain_priors:suppression_score(fitness_contingent_reading, 0.72).
domain_priors:theater_ratio(fitness_contingent_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fitness_contingent_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fitness_contingent_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fitness_contingent_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fitness_contingent_reading, snare).
narrative_ontology:human_readable(fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fitness_contingent_reading, '2bcf4b80-08d5-4218-b4c2-d023054165c1').
narrative_ontology:cs_created_at('2bcf4b80-08d5-4218-b4c2-d023054165c1', '').
narrative_ontology:cs_kernel_codification('2bcf4b80-08d5-4218-b4c2-d023054165c1', fixed_text).
narrative_ontology:cs_authority_grounding('2bcf4b80-08d5-4218-b4c2-d023054165c1', lineage).
narrative_ontology:cs_interpretation_layer_present('2bcf4b80-08d5-4218-b4c2-d023054165c1').
narrative_ontology:cs_kernel_id(fitness_contingent_reading, personhood_boundary).
narrative_ontology:cs_reading_relation('2bcf4b80-08d5-4218-b4c2-d023054165c1', birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('2bcf4b80-08d5-4218-b4c2-d023054165c1', potential_based_reading, forecloses).
narrative_ontology:cs_axiom('2bcf4b80-08d5-4218-b4c2-d023054165c1', foundational, demonstrated_capacity_constitutive_personhood).
narrative_ontology:cs_axiom_status(demonstrated_capacity_constitutive_personhood, holdable).
narrative_ontology:cs_axiom('2bcf4b80-08d5-4218-b4c2-d023054165c1', foundational, adjudicating_authority_legitimacy).
narrative_ontology:cs_axiom_status(adjudicating_authority_legitimacy, holdable).
narrative_ontology:cs_reference_frame('2bcf4b80-08d5-4218-b4c2-d023054165c1', capacity_based_moral_agency).
narrative_ontology:cs_drift_state('2bcf4b80-08d5-4218-b4c2-d023054165c1', contemporary_biomedical_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fitness_contingent_reading, fitness_adjudicating_authority).
narrative_ontology:constraint_beneficiary(fitness_contingent_reading, existing_moral_community).
narrative_ontology:constraint_victim(fitness_contingent_reading, pre_fitness_entities).
narrative_ontology:constraint_victim(fitness_contingent_reading, moral_status_undecidables).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRE-FITNESS ENTITY (SNARE) — An infant, cognitively disabled person, or other entity not yet capable of demonstrating fitness bears no moral standing and exists in a state of contingent inclusion. Cannot appeal the fitness criterion; cannot exit the evaluation framework. Maximum extraction: the entity's interests do not count until proven. Suppression is total — the entity is excluded from the moral community pending performance demonstration.
constraint_indexing:constraint_classification(fitness_contingent_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: CAREGIVERS (SNARE) — Parents, guardians, and institutional caregivers must continue supporting entities without moral standing while the adjudicating authority decides membership. Constrained by duty to care while having no formal moral claim on the community to reciprocate. High extraction: caregiving labor is demanded without recognition of the entity's moral worth. Exit options are severely constrained by social obligation and legal liability.
constraint_indexing:constraint_classification(fitness_contingent_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: FITNESS-ADJUDICATING AUTHORITY (ROPE) — The institution or tradition that defines and assesses fitness (e.g., philosophical schools, medical authorities, state agencies) experiences the constraint as a coordination mechanism: it organizes who counts as a moral agent and who does not. Benefits from the authority to make this determination. The coordination function is real — determining membership criteria is necessary. Net beneficiary position; experiences the constraint as enabling order rather than extracting from the vulnerable.
constraint_indexing:constraint_classification(fitness_contingent_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: PHILOSOPHICAL TRADITION (PITON) — The intellectual framework grounding fitness-contingency (e.g., Aristotelian natural teleology, utilitarian rationality, capability theory thresholds) maintains the criterion through ongoing reinterpretation and ritualized defense. Theater ratio is high because much philosophical work consists of defending the fitness criterion against counterexamples rather than performing its core function (determining actual moral status). The tradition persists through institutional inertia and scholarly investment despite accumulating logical challenges. The criterion is increasingly theatrical — maintained because alternatives haven't fully displaced it, not because the fitness test itself works.
constraint_indexing:constraint_classification(fitness_contingent_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXISTING MORAL COMMUNITY (TANGLED ROPE) — Agents already recognized as having full moral standing both benefit from and are constrained by the fitness criterion. They benefit: maintaining a fitness threshold prevents moral obligations from being extended beyond their capacity to recognize and reciprocate (it bounds the community). They are constrained: the criterion that includes them may exclude their infants, disabled relatives, or other dependents, creating internal contradiction and care obligations for the moral community toward its own members. Genuine coordination function (defining community membership) mixed with extraction from those not yet admitted.
constraint_indexing:constraint_classification(fitness_contingent_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some threshold of cognitive or relational capacity is inherent to moral agency itself. Pre-fitness entities lack the structural prerequisites for moral status — the criterion emerges from the nature of personhood and moral relations, not from contingent institutions. This perspective naturalizes the fitness test as reflecting an immutable property of what personhood is. However, the structural data — identifiable adjudicating authorities, beneficiaries, victims, enforced suppression — reveals this as a false summit: the 'inherent to personhood' framing masks a contingent institutional reading of which capacities count and who gets to decide.
constraint_indexing:constraint_classification(fitness_contingent_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fitness_contingent_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fitness_contingent_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fitness_contingent_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fitness_contingent_reading, TR),
    TR >= 0.70.

:- end_tests(fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The fitness criterion creates an asymmetric power structure where an adjudicating authority determines who qualifies for moral standing while pre-fitness entities have no voice in the determination. The beneficiaries (authorities and existing community) have clear interests in maintaining the criterion — it gives them discretionary power to include or exclude. The rising trend reflects increasing sophistication of fitness tests (IQ assessment, economic productivity metrics, cognitive capacity thresholds) which expand the mechanisms for exclusion. Suppression (0.72): Very high. Pre-fitness entities cannot appeal the criterion, cannot exit the evaluation framework, cannot demonstrate fitness retroactively if they never had the opportunity to develop capacity. Caregivers face legal liability for harm to entities with no moral standing, creating suppression of caregiver resistance. The moral community's internalized norms prevent them from fully accepting the implication that their own dependents lack moral standing, creating psychological suppression of the contradiction. Theater ratio (0.55): Moderate and rising. Philosophical traditions generate elaborate defenses of fitness criteria against counterexamples (potential-based loopholes, categories of partial personhood, special cases). The work of maintaining the criterion intellectually is increasingly theatrical as the number of problematic cases (severely disabled persons, non-human animals with surprising capacities, artificial systems with complex behavior) grows. Traditional adjudication bodies become more elaborate in their fitness testing even as the tests become increasingly arbitrary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_criterion_determinacy,
    'What specific capacities or demonstrations constitute ''fitness''? Is the criterion determinate or does it systematically exclude based on adjudicating authority''s interests?',
    'Historical analysis of fitness criteria across traditions (rationality, self-awareness, relational reciprocity, economic productivity); detection of criterion drift when inconvenient entities approach the threshold (moving the goalposts); correlation between who has authority to set criteria and who benefits from their application',
    'If criterion is determinate and stable: the constraint may be a genuine coordination mechanism distinguishing real differences in moral agency. If criterion is systematically vague or drifts to exclude inconvenient cases: the constraint is primarily extractive — the adjudicating authority uses apparent objectivity to mask discretionary exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criterion_determinacy, empirical, 'Whether fitness criteria are determinate or systematically exclude based on authority interests').

omega_variable(
    alternative_personhood_frameworks,
    'What would change in the classification if personhood were grounded in potential (all beings with capacity to develop fitness become members on potential alone), or in birth/conception, or in relational recognition rather than demonstrated individual capacity?',
    'Structural comparison: potential-based framework yields different victim/beneficiary structure (excludes pre-conception but includes all post-conception); birth-based framework moves boundary earlier (different exclusion set); relational recognition framework decentralizes adjudication (different authority structure). Each produces a different ε value and classification pattern.',
    'This omega documents the kernel-level commitment underlying this reading. The choice between fitness-contingent, potential-based, and birth-based readings is not empirically resolvable — it depends on foundational normative commitments about what grounds personhood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_personhood_frameworks, conceptual, 'Kernel-level reading indeterminacy: fitness vs. potential vs. birth as personhood criterion').

omega_variable(
    enforcement_mechanism_asymmetry,
    'Who bears enforcement costs? Does the adjudicating authority enforce the fitness criterion equally on all candidate entities, or are some groups systematically subjected to fitness testing while others are granted presumptive membership?',
    'Historical analysis: which groups have faced formal fitness assessments (cognitive testing for disabilities, ''civilizedness'' tests for colonized populations, economic productivity thresholds for welfare recipients, rationality tests for religious minorities); which groups gained presumptive moral status without demonstration (infants of dominant groups, inherited aristocracy, core community members)',
    'If enforcement is asymmetric: the constraint functions as a disguised extraction mechanism targeting specific groups. The ''objective fitness criterion'' becomes a tool for excluding disfavored populations from moral community. This increases ε and strengthens the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_asymmetry, empirical, 'Whether fitness criterion is enforced equally or targets specific groups').

omega_variable(
    care_paradox_resolution,
    'How does the constraint resolve the paradox that pre-fitness entities (infants, disabled people) require intensive care precisely because they lack demonstrated fitness, yet the moral community cannot deny care without violating its own internalized norms?',
    'Examination of how traditions resolve the paradox: by redefining fitness retrospectively (infants ''have'' fitness potential even if not demonstrated); by splitting personhood into categories (moral status deferred, but care obligations proceed from other grounds); by treating pre-fitness care as supererogatory rather than obligatory (mercy, compassion); by denying the paradox exists (asserting that demonstrated-fitness and care-without-fitness are compatible)',
    'The resolution strategy reveals the constraint''s actual stability. If resolved through rationalization (potential, categories, supererogation), the constraint is maintained through theater and interpretive flexibility — piton signal. If the paradox remains genuinely unresolved, the tradition is under structural pressure to adopt an alternative reading (birth-based, potential-based).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_paradox_resolution, conceptual, 'How fitness-contingency resolves tension with care obligations for pre-fitness entities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fitness_contingent_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fitn_tr_t0, fitness_contingent_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fitn_tr_t2, fitness_contingent_reading, theater_ratio, 2, 0.47).
narrative_ontology:measurement(fitn_tr_t4, fitness_contingent_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(fitn_be_t0, fitness_contingent_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(fitn_be_t2, fitness_contingent_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(fitn_be_t4, fitness_contingent_reading, base_extractiveness, 4, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(fitness_contingent_reading, birth_threshold_reading).
narrative_ontology:affects_constraint(fitness_contingent_reading, potential_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the personhood_boundary kernel. The fitness-contingent reading coexists with birth-threshold and potential-based readings as live positions in ongoing dispute. Each reading has distinct ε values, victim/beneficiary structures, and authority grounding. Decomposition is required because the three readings yield fundamentally different classifications: fitness-contingent classifies as snare (high extraction, total suppression of pre-fitness entities); birth-threshold classifies as rope (coordination with minimal extraction); potential-based classifies as tangled_rope (coordination function mixed with deferred inclusion). The kernel remains contested because no single reading has achieved institutional supremacy across all traditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fitness_contingent_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
