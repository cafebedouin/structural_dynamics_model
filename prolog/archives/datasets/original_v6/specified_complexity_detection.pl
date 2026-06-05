% ============================================================================
% CONSTRAINT STORY: specified_complexity_detection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_specified_complexity_detection, []).

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
 *   constraint_id: specified_complexity_detection
 *   human_readable: Specified Complexity Detection in Origins Science
 *   domain: philosophy_of_science/epistemology
 *
 * SUMMARY:
 *   Specified complexity detection — the concept that biological information
 *   bearing 'specified' patterns combined with 'complex' structure (high
 *   Kolmogorov complexity) signals design — operates as a hybrid constraint
 *   in origins science. It functions simultaneously as a coordination
 *   mechanism (sharpening mechanistic rigor in evolutionary biology), an
 *   extraction mechanism (gatekeeping resources and prestige away from
 *   naturalistic origin-of-life research), a degraded institutional apparatus
 *   (performing scientific status without novel discoveries), a temporary
 *   problem being engineered out (as prebiotic chemistry advances), and an
 *   unchangeable logical asymmetry (from the analytical view). The
 *   constraint's theater_ratio has risen from 0.45 to 0.68 over 15 years as
 *   the institutional apparatus has elaborated without corresponding
 *   empirical breakthroughs, and extractiveness has risen correspondingly as
 *   the concept's use in resource-gatekeeping has intensified. The analysis
 *   reveals specified complexity as a Tangled Rope: genuine coordination
 *   function (improving mechanistic rigor) coupled with asymmetric extraction
 *   (systematic suppression of abiogenic pathways).
 *
 * KEY AGENTS:
 *   - Abiogenic Mechanism Researchers: Primary victim (powerless/trapped) — cannot pursue origin-of-life mechanisms without confronting specified complexity objections; face institutional barriers to funding and publication
 *   - Design Inference Proponents: Primary beneficiary (institutional/arbitrage) — captured multiple institutional positions, sustained research program, generated philosophical apparatus; benefits from the constraint's existence
 *   - Evolutionary Synthesis Community: Secondary victim (moderate/constrained) — constrained by need to defend mechanism against specified complexity critiques; also benefits from refined theoretical precision
 *   - Methodological Naturalism Reformers: Organized agents (organized/constrained) — philosophers and science educators working to engineer out the constraint through better prebiotic chemistry and demarcation criteria
 *   - Design Movement Institutional Apparatus: Institutional actor (institutional/arbitrage) — maintains textbooks, conferences, position statements; theater-heavy with limited novel empirical discoveries since Behe (1996)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent epistemological choices (narrow definition of mechanism, broad definition of design) as logical necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(specified_complexity_detection, 0.58).
domain_priors:suppression_score(specified_complexity_detection, 0.65).
domain_priors:theater_ratio(specified_complexity_detection, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(specified_complexity_detection, extractiveness, 0.58).
narrative_ontology:constraint_metric(specified_complexity_detection, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(specified_complexity_detection, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(specified_complexity_detection, tangled_rope).
narrative_ontology:human_readable(specified_complexity_detection, "Specified Complexity Detection in Origins Science").
narrative_ontology:topic_domain(specified_complexity_detection, "philosophy_of_science/epistemology").

domain_priors:requires_active_enforcement(specified_complexity_detection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(specified_complexity_detection, design_inference_proponents).
narrative_ontology:constraint_beneficiary(specified_complexity_detection, alternative_origin_frameworks).
narrative_ontology:constraint_victim(specified_complexity_detection, abiogenic_mechanism_research).
narrative_ontology:constraint_victim(specified_complexity_detection, evolutionary_synthesis_community).
narrative_ontology:constraint_victim(specified_complexity_detection, methodological_naturalism_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABIOGENIC MECHANISM RESEARCH (SNARE) — Trapped by resource allocation and institutional gatekeeping. Funding for origin-of-life mechanisms is scarce; journal editors apply 'specified complexity' critiques selectively to naturalistic proposals while protecting design-friendly interpretations. Cannot exit this constraint without abandoning the research domain entirely. Bears full extraction cost.
constraint_indexing:constraint_classification(specified_complexity_detection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EVOLUTIONARY SYNTHESIS COMMUNITY (TANGLED ROPE) — Constrained by need to defend mechanistic coherence against specified complexity objections, yet benefits from the existence of a formalized critique that sharpens theoretical precision. Must respond to design arguments but also uses those arguments to refine population genetics and developmental models. Mixed extraction and coordination.
constraint_indexing:constraint_classification(specified_complexity_detection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DESIGN INFERENCE PROPONENTS (ROPE) — Primary beneficiary with arbitrage options. 'Specified complexity' provides a coherent explanatory framework, generates publications, funds research programs, and enables institutional presence (Discovery Institute, academic philosophy positions). Net beneficiary from the constraint. Experiences it as coordination: the concept channels inquiry toward design alternatives.
constraint_indexing:constraint_classification(specified_complexity_detection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: METHODOLOGICAL NATURALISM REFORMERS (SCAFFOLD) — Organized agents (philosophers of science, science educators) see specified complexity as a temporary tension in demarcation methodology. Sunset logic: as origin-of-life chemistry advances and multi-pathway evolvability models mature, the concept loses explanatory force. The constraint is being engineered out through better naturalistic mechanisms, not through philosophical argument. Sunset horizon: 30-50 years as prebiotic chemistry complexity mapping advances.
constraint_indexing:constraint_classification(specified_complexity_detection, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTELLIGENT DESIGN INSTITUTIONAL APPARATUS (PITON) — The formalized apparatus of design inference — textbooks, conferences, position statements — persists largely through institutional inertia. The core research program (detecting design via specified complexity) has generated limited empirical discoveries since Behe (1996). The concept is maintained through rhetorical rehearsal and philosophical refinement rather than novel empirical breakthroughs. Theater ratio high because the movement performs 'scientific status' through institutional mimicry rather than producing falsifiable predictions.
constraint_indexing:constraint_classification(specified_complexity_detection, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DEMARCATION VIEW (MOUNTAIN) — From a logical standpoint, specified complexity reflects an inherent epistemic asymmetry: all mechanistic explanations must explain how low-probability states were reached, and design hypotheses bypass this requirement by positing intentional selection. This asymmetry is seen as an unchangeable feature of the logical structure of origins arguments. However, this perspective risks naturalizing what is actually a contingent framing choice — defining 'mechanism' narrowly (excluding agent-causation) and 'design' broadly (any intentional process). The mountain classification is perspectival and contestable.
constraint_indexing:constraint_classification(specified_complexity_detection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(specified_complexity_detection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(specified_complexity_detection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(specified_complexity_detection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(specified_complexity_detection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(specified_complexity_detection, TR),
    TR >= 0.70.

:- end_tests(specified_complexity_detection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Design inference proponents benefit substantially from the constraint — sustained institutional positions, publication outlets, research funding — while naturalistic origin-of-life research faces systematic barriers. The extraction is not at snare levels (0.66+) because the constraint has genuine coordination content: specified complexity discourse does sharpen mechanistic reasoning and forces defenders to specify evolvability pathways more rigorously. Suppression (0.65): High. Substantial barriers to abiogenic research include publication bias (design-friendly papers receive gentler peer review), funding gatekeeping (design institutes control significant resources), institutional hiring (design-compatible views gain positions at faith-affiliated institutions), and rhetorical supremacy (specified complexity objections are treated as automatically requiring response while naturalistic mechanisms are assessed on internal consistency alone). Theater ratio (0.68): High and rising. The design inference apparatus (Discovery Institute publications, academic philosophy positions, textbook representation) maintains institutional presence and performs scientific status, but empirical productivity has been limited since Behe's irreducible complexity argument. The rhetoric rehearsal of specified complexity objections without novel empirical discoveries drives the theater ratio upward.
 *
 * PERSPECTIVAL GAP:
 *   The specified complexity constraint exhibits maximum perspectival divergence across the six types. Design proponents experience it as Rope — a coordination mechanism enabling their research program and intellectual community. The evolutionary synthesis community experiences it as Tangled Rope — they must respond to legitimate epistemological challenges while bearing extraction costs through resource scarcity and publication bias. Abiogenic researchers experience it as Snare — pure extraction with institutional gatekeeping preventing field development. Methodological naturalism reformers experience it as Scaffold — a temporary institutional problem being solved through better chemistry and demarcation theory. The ID apparatus experiences it as Piton — the concept persists through institutional inertia rather than empirical breakthrough. The analytical observer risks experiencing it as Mountain — an unchangeable logical feature of how origins arguments work — but this classification naturalizes contingent epistemic conventions. The perspectival gap reveals that 'specified complexity' is not a discovered constraint but a constructed frame that different communities experience with radically different extraction profiles.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position. Design proponents occupy a beneficiary position (institutional power, arbitrage exit through think tanks and academic philosophy) — their d is low (~0.15), producing negative or minimal effective extraction. They see and experience Rope. Abiogenic researchers occupy a trapped victim position (limited funding, career risk, publication barriers) — their d is high (~0.92), producing maximum extraction. They experience Snare. Evolutionary biologists occupy a constrained-victim position (they must respond to specified complexity critiques, but retain institutional funding and publication channels) — their d is moderate (~0.68), producing the tangled combination of extraction and coordination. The design apparatus itself occupies an arbitrage position (can maintain institutional presence through think-tank funding and philosophy positions without direct dependence on peer review) — producing the piton experience of low functional extraction disguised as performative science. The Analytical Observer at civilizational scope faces a logical asymmetry (mechanistic explanation requires explaining low-probability states; design explanation bypasses this) but this asymmetry is itself a chosen frame, not a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that specified complexity is both real (as an epistemological challenge to naturalistic mechanisms) and instrumentalized (as an institutional tool for suppressing abiogenic research funding and prestige). The constraint is not 'one true type' but rather a presheaf over different structural positions. For abiogenic researchers, it is a Snare — pure extraction mechanism with institutional gatekeeping. For design proponents, it is Rope — genuine coordination within their community. For evolutionary synthesis, it is Tangled Rope — coordination mixed with extraction. The mandatrophy resolution forbids collapsing these into a single answer ('specified complexity is really just X'), and instead treats the multiperspectival reading as the complete picture. The analytical observer's Mountain classification is a false summit — the constraint naturalizes as logical necessity what is actually a contingent frame choice. The constraint becomes analytically clear not when a single type is judged correct, but when the perspectival decomposition reveals the asymmetric extraction and its institutional mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specified_complexity_mathematical_validity,
    'Is Dembski''s specified complexity metric (probability × conditional specificity) mathematically sound and empirically implementable for biological systems?',
    'Formal mathematical review; attempted application to well-understood biological sequences (known neutral variants, domestication signatures) to assess whether the metric distinguishes designed from evolved patterns',
    'If valid: specified complexity is a robust detection principle (strengthens snare/tangled_rope for victims). If invalid: the constraint''s enforcement mechanism is rhetorical rather than logical (strengthens piton classification and reveals theater_ratio should exceed 0.70).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specified_complexity_mathematical_validity, empirical, 'Mathematical validity of specified complexity detection method').

omega_variable(
    universal_probability_bound_calibration,
    'Is Dembski''s universal probability bound (10^-120, derived from maximum atomic operations in the observable universe) the correct threshold for ''specified complexity'' or an arbitrary convention that smuggles creationist assumptions into mathematics?',
    'Comparison with alternative probability thresholds used in legitimate fields (cryptography, error correction); analysis of whether the bound is justified by physics or is instead reverse-engineered from theological conclusions',
    'If justified: the threshold is a discovered constraint of design-detectability. If arbitrary: the entire metric is question-begging, and specified complexity enforcement is purely institutional suppression (maximizes snare experience for victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_probability_bound_calibration, empirical, 'Whether universal probability bound is empirically justified or arbitrary').

omega_variable(
    abiogenic_complexity_trajectory,
    'Can prebiotic chemistry pathways (ribozyme evolution, autocatalytic networks, metabolic replication) generate sequences of equivalent Kolmogorov complexity to biological DNA through Darwinian mechanisms, and if so, at what timescale?',
    'Laboratory synthesis of RNA replication cycles; computer modeling of chemical-space exploration under selective pressure; comparison of achieved complexity with complexity of early-life candidate sequences',
    'If yes (< 1 billion years): abiogenic mechanisms can generate arbitrary complexity (invalidates snare classification, strengthens scaffold sunset). If no: specified complexity is a genuine bottleneck (maintains snare classification for abiogenic research).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abiogenic_complexity_trajectory, empirical, 'Whether prebiotic mechanisms can generate biological complexity').

omega_variable(
    institutional_resource_allocation_asymmetry,
    'Do funding agencies and academic institutions systematically underweight abiogenic mechanism research and overweight design-friendly research relative to predictive value and empirical productivity of each research program?',
    'Citation analysis, funding database audit (NSF, NIH grants), journal acceptance rate comparison (specified complexity objections to naturalistic claims vs. design-friendly papers), career trajectory analysis for researchers in each camp',
    'If yes: suppression is structural and institutional (confirms snare and explains high suppression score). If no: selection pressure is meritocratic and the snare classification is overstated (weakens victim narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_resource_allocation_asymmetry, empirical, 'Whether institutional funding and publication show systematic bias').

omega_variable(
    identity_lock_mechanism_in_naturalism,
    'Is the commitment to methodological naturalism in the evolutionary synthesis community an identity_locked binding (foundational to scientific self-concept and community boundary maintenance) or a constrained binding (external barriers and career incentives)?',
    'Qualitative analysis of scientific identity narratives; historical study of whether scientists who transition between naturalistic and design frameworks retain community standing; analysis of whether belief in naturalism causally drives institutional gatekeeping or vice versa',
    'If identity_locked: evolutionary community sees specified complexity critique as existentially threatening to scientific identity (increases tangled_rope extraction experience). If constrained: the gatekeeping is instrumental and conditional (reduces tangled_rope to institutional coordination challenge).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_naturalism, conceptual, 'Whether naturalism commitment is identity-locked or externally constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(specified_complexity_detection, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scd_tr_t0, specified_complexity_detection, theater_ratio, 0, 0.45).
narrative_ontology:measurement(scd_tr_t5, specified_complexity_detection, theater_ratio, 5, 0.58).
narrative_ontology:measurement(scd_tr_t10, specified_complexity_detection, theater_ratio, 10, 0.68).
narrative_ontology:measurement(scd_tr_t15, specified_complexity_detection, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(scd_be_t0, specified_complexity_detection, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scd_be_t5, specified_complexity_detection, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(scd_be_t10, specified_complexity_detection, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(scd_be_t15, specified_complexity_detection, base_extractiveness, 15, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(specified_complexity_detection, information_standard).
narrative_ontology:boltzmann_floor_override(specified_complexity_detection, 0.12).
narrative_ontology:affects_constraint(specified_complexity_detection, demarcation_problem_in_philosophy_of_science).
narrative_ontology:affects_constraint(specified_complexity_detection, institutional_funding_for_origins_research).
narrative_ontology:affects_constraint(specified_complexity_detection, methodological_naturalism_commitment).

% DUAL FORMULATION NOTE:
% Specified complexity detection decomposes into two structurally distinct constraints: (1) Specified complexity as mathematical criterion (empirical question: does the metric validly distinguish design from evolution?), and (2) Specified complexity as institutional enforcement mechanism (empirical question: does the concept gate resources and prestige?). These stories have different ε values because they measure different observables. The mathematical story (ε ≈ 0.15, rope from all perspectives) covers the logical structure. The institutional story (ε ≈ 0.58, tangled_rope at aggregate) covers the extraction mechanism. This file addresses the institutional version. The mathematical version would be a separate constraint story focusing on Dembski's formalism and its validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(specified_complexity_detection, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
