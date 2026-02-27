% ============================================================================
% CONSTRAINT STORY: ape_cognition_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ape_cognition_framework, []).

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
 *   constraint_id: ape_cognition_framework
 *   human_readable: The scientific and ethical framework defining the limits of ape cognition
 *   domain: social/scientific
 *
 * SUMMARY:
 *   The scientific and ethical framework defining ape cognition operates as a
 *   dual-function constraint: it coordinates research methodology and enables
 *   welfare policy, while simultaneously suppressing field innovation and
 *   constraining ethical advocacy. The framework establishes boundaries
 *   between genuine cognition (typically requiring controlled laboratory
 *   conditions, explicit task performance, and ruling out 'simpler'
 *   explanations) and sophisticated behavior (field observations, novel
 *   problem-solving in natural contexts, apparent emotional or social
 *   complexity). These boundaries are presented as methodological necessities
 *   but function as institutional gatekeeping. The constraint exhibits all
 *   six DR types depending on the observer's structural position: for the ape
 *   subject, it is pure extraction (snare); for field methodologists, it is
 *   mixed coordination and suppression (tangled rope); for the primatology
 *   establishment, it is coordination (rope); for animal ethics coalitions,
 *   it is constrained protection (tangled rope); for the comparative
 *   psychology canon, it is degraded ritual (piton); and from the analytical
 *   universal view, it appears as immutable methodological necessity
 *   (mountain — though the structural data reveals this as false summit). The
 *   constraint's theater ratio has risen from 0.38 to 0.58 over 30 years,
 *   indicating increasing performative effort to defend framework boundaries
 *   against accumulating anomalous field observations. The extractiveness has
 *   grown from 0.35 to 0.52, reflecting rising suppression of methodological
 *   alternatives and growing mismatch between framework definitions and
 *   observed ape capacities.
 *
 * KEY AGENTS:
 *   - Ape subjects: Powerless/trapped. Bear full cost of definitional exclusion; observed capacities are systematically reinterpreted to fit framework constraints.
 *   - Field methodologists: Moderate/constrained. Face funding and publication bias toward framework-confirming research; constrained innovation capacity.
 *   - Primatology establishment: Institutional/arbitrage. Primary beneficiary. Controls framework definitions; has arbitrage option to revise definitions if evidence demands but low incentive.
 *   - Animal ethics coalitions: Organized/constrained. Benefit from cognitive framework as policy basis but are constrained by framework's conservatism in advocacy for stronger protections.
 *   - Comparative psychology canon: Institutional/arbitrage. Framework persists through institutional inertia despite degraded theoretical foundation.
 *   - Field researchers (implicit secondary victim): Experience constraint as methodological friction; novel observations encounter higher publication barriers than framework-confirming results.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ape_cognition_framework, 0.52).
domain_priors:suppression_score(ape_cognition_framework, 0.65).
domain_priors:theater_ratio(ape_cognition_framework, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ape_cognition_framework, extractiveness, 0.52).
narrative_ontology:constraint_metric(ape_cognition_framework, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ape_cognition_framework, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ape_cognition_framework, tangled_rope).
narrative_ontology:human_readable(ape_cognition_framework, "The scientific and ethical framework defining the limits of ape cognition").
narrative_ontology:topic_domain(ape_cognition_framework, "social/scientific").

domain_priors:requires_active_enforcement(ape_cognition_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ape_cognition_framework, primatology_establishment).
narrative_ontology:constraint_beneficiary(ape_cognition_framework, institutional_funding_bodies).
narrative_ontology:constraint_beneficiary(ape_cognition_framework, conservation_authorities).
narrative_ontology:constraint_victim(ape_cognition_framework, ape_cognitive_agency).
narrative_ontology:constraint_victim(ape_cognition_framework, field_methodological_innovation).
narrative_ontology:constraint_victim(ape_cognition_framework, ethical_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: APE COGNITIVE SUBJECT (SNARE) — Trapped within a framework that declares certain cognitive capacities impossible a priori, regardless of observed behavior. No exit from definitional constraints embedded in scientific protocols. Maximum extraction: the ape's demonstrated capacities are systematically reinterpreted to fit framework constraints rather than framework adapting to evidence. The subject experiences pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(ape_cognition_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD METHODOLOGIST (TANGLED ROPE) — Constrained by funding requirements tied to established frameworks but also benefits from access to primatology infrastructure, collaborative networks, and validated protocols. Can propose methodological innovation but faces publication and grant bias toward framework-confirming results. Mixed extraction and coordination: the framework both enables systematic study and suppresses novel findings.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIMATOLOGY ESTABLISHMENT (ROPE) — Benefits from framework stability: clear definitions enable standardized research, comparative studies across sites, and institutional coherence. Experiences the constraint as coordination mechanism rather than extraction. Arbitrage option: can redefine framework terms if evidence demands, but has low incentive to do so. Net beneficiary.
constraint_indexing:constraint_classification(ape_cognition_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANIMAL ETHICS COALITION (ORGANIZED) (TANGLED ROPE) — Organized actors (ethics boards, welfare advocates, conservation NGOs) benefit from cognitive framework as basis for welfare policies (more cognition = more welfare protections) but are constrained by framework's conservatism: many observed ape behaviors that suggest suffering or complex preferences are excluded from policy consideration by definitional fiat. Mixed: the framework enables some protections but suppresses advocacy for stronger protections based on excluded evidence.
constraint_indexing:constraint_classification(ape_cognition_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPARATIVE PSYCHOLOGY CANON (PITON) — The theoretical framework itself (distinctions between 'true' metacognition vs behavioral signs, 'genuine' theory of mind vs performance mimicry) has degraded from functional tool to performative ritual. Researchers spend substantial effort defending which capacities qualify as 'real' cognition vs mere behavior, but this definitional theater does little cognitive work — most researchers privately acknowledge the categories are fuzzy. The framework persists through institutional inertia: journals still require 'ruling out simpler explanations,' ethics boards still apply the same thresholds, but the underlying theoretical confidence has eroded. Theater ratio high because much research effort goes to framework defense rather than capacity discovery.
constraint_indexing:constraint_classification(ape_cognition_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / METHODOLOGICAL NATURALISM (MOUNTAIN) — From a universal analytical context, some epistemic conservatism is inherent to science: distinguishing genuine cognition from sophisticated behavioral mimicry requires controlled evidence, and that bar is necessarily high to prevent false positives. This view sees the framework as expressing an immutable principle of scientific method itself. However, the structural data (high suppression, active enforcement, extraction from field innovation) contradicts pure naturalism — the engine will identify this as a false summit, revealing that 'scientific conservatism' naturalizes what is actually a contingent institutional arrangement with gatekeeping power.
constraint_indexing:constraint_classification(ape_cognition_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ape_cognition_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ape_cognition_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ape_cognition_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ape_cognition_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ape_cognition_framework, TR),
    TR >= 0.70.

:- end_tests(ape_cognition_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework creates asymmetric cognitive authority: primatology establishment defines what counts as genuine cognition, constrains which methods are legitimate pathways to evidence, and controls publication and funding resources. Field researchers can propose innovation but face suppression through review and funding gatekeeping. This is extraction, not pure coordination, because the beneficiary (establishment) captures resource value and gatekeeping power while the cost-bearer (field innovation, ape agency) receives no reciprocal benefit. Suppression (0.65): High. Multiple barriers prevent alternative framings: (1) definitional barriers — novel capacity claims must overcome the presumption that established categories are exhaustive; (2) methodological barriers — field observations are considered lower-quality evidence than controlled lab tests; (3) career barriers — researchers advancing novel frameworks face publication bias and reputation risk; (4) institutional barriers — funding bodies reward framework-confirming research. Theater ratio (0.58): Moderate-high and rising. Much research effort goes to defending framework boundaries against anomalies rather than discovering new capacities. The 'ruling out simpler explanations' ritual has become performative — researchers and reviewers acknowledge the categories are fuzzy but continue enforcing them, maintaining the theater.
 *
 * PERSPECTIVAL GAP:
 *   The framework creates a fundamental gap between what field researchers observe and what the framework permits to count as evidence. A chimpanzee that solves a novel problem in a natural context using apparent planning, teaching younger individuals, and social coordination would be classified by the framework as 'clever behavior' or 'cultural learning,' but not as genuine 'cognition' in the theoretical sense — that label is reserved for laboratory tasks with explicit controls. This gap grows as field observation methods improve and become more rigorous; the theater ratio increases because researchers must invest effort defending the boundaries. The organized ethics coalition experiences this gap as a welfare policy failure: they observe behavioral indicators of suffering, preference, social attachment, and planning that would justify stronger protections if the framework recognized them as genuine cognition, but the framework's definitional conservatism prevents translation of observation into policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position within the extraction flow. The primatology establishment benefits from framework stability and has arbitrage options (can revise definitions if evidence demands) — derives low d, experiences low chi. Field methodologists bear constraints (publication bias, funding gatekeeping) but also benefit from infrastructure and validation — derives moderate d. Ape subjects have no exit and bear all suppression costs — derives high d, experiences high chi. Ethics coalitions are organized but have constrained exit (framework is institutional, not easily dismissed) and face suppression in advocacy — derives moderate-high d. The comparative psychology canon has arbitrage options but low motivation to revise — derives low d but experiences theater-ratio drag. The analytical observer at the universal scope is attempting to naturalize contingent institutional arrangement — derives false mountain d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the tangled_rope classification is structural, not perspectival confusion. The framework genuinely provides both coordination (standardized comparative methodology, enabling research) and asymmetric extraction (suppresses field innovation, excludes ape agency from policy). The framework cannot be reduced to pure coordination (rope) because suppression is systematic and benefits a specific group. It cannot be reduced to pure extraction (snare) because research infrastructure and validated protocols genuinely enable study. The piton classification is real: the comparative psychology canon has experienced theoretical degradation — most researchers privately acknowledge the cognition/behavior boundary is fuzzy, yet maintain the framework through institutional inertia. The false mountain (analytical observer) is diagnostic: the framework naturalizes its own conservatism as methodological necessity, but the structural data reveals this as institutional choice. The resolving insight is that all perspectives are accurate readings of the same constraint: the framework is simultaneously a coordination mechanism (enabling comparative science), an extraction mechanism (suppressing alternative framings), a degraded ritual (maintaining theater despite theoretical erosion), and a false natural law (claiming necessity it does not possess). Mandatrophy is resolved by recognizing that the apparent type-ambiguity is itself the structural feature — the framework's power derives from conflating coordination with extraction, making it hard to reform without losing the legitimate coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognition_definition_boundary,
    'What constitutes genuine ape cognition vs. sophisticated behavioral response? Where is the principled boundary?',
    'Longitudinal comparison of behavioral predictions under novel conditions; identification of whether framework revisions track evidence or resist it; historical analysis of how boundaries have shifted for other species',
    'If boundary is empirically determined: framework evolves with evidence (Rope/Scaffold). If boundary is definitional-prior: framework suppresses evidence (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognition_definition_boundary, conceptual, 'Definition of cognition boundary and its empirical vs. conceptual status').

omega_variable(
    publication_bias_direction,
    'Does the framework''s enforcement suppress novel cognition claims more than it suppresses false-positive cognition claims?',
    'Meta-analysis of published ape cognition studies: proportion claiming framework-confirming results vs. novel capacity claims; analysis of rejection rates for capacity-expanding vs. capacity-limiting papers; interview data from researchers on submission and review experience',
    'If suppression is symmetric: framework functions as quality control (Rope). If suppression is asymmetric: framework functions as extraction mechanism (Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publication_bias_direction, empirical, 'Directional bias in publication and review of ape cognition claims').

omega_variable(
    welfare_protection_sufficiency,
    'Do current cognitive framework definitions provide adequate ethical protection for ape welfare, or do excluded capacities imply welfare needs that policy ignores?',
    'Correlation analysis: ape behaviors that field researchers observe but framework excludes vs. subsequent welfare harms; expert elicitation from ethicists and field researchers on gap between framework definitions and observed suffering indicators',
    'If framework is sufficient: ethical mechanism works (Rope). If gap is large: framework suppresses welfare advocacy (Snare) and organized actors are constrained (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_protection_sufficiency, preference, 'Adequacy of cognitive framework for ape welfare protection').

omega_variable(
    methodological_conservatism_necessity,
    'Is the high epistemic bar for ape cognition claims a principled necessity (to prevent false positives) or a social institutional choice (status quo bias masked as methodology)?',
    'Comparative analysis: what bar do other animal cognition fields use (octopus, corvid, cetacean cognition)? Historical analysis of how the bar for comparable species (great apes vs. other primates) has evolved; examination of whether bar height correlates with evidence quality or with institutional investment in current framework',
    'If necessity: framework is mountain-adjacent (high resistance). If institutional choice: framework is extractive (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_conservatism_necessity, conceptual, 'Whether methodological conservatism reflects necessity or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ape_cognition_framework, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(apecog_tr_t0, ape_cognition_framework, theater_ratio, 0, 0.38).
narrative_ontology:measurement(apecog_tr_t15, ape_cognition_framework, theater_ratio, 15, 0.5).
narrative_ontology:measurement(apecog_tr_t30, ape_cognition_framework, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(apecog_be_t0, ape_cognition_framework, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(apecog_be_t15, ape_cognition_framework, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(apecog_be_t30, ape_cognition_framework, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ape_cognition_framework, information_standard).
narrative_ontology:affects_constraint(ape_cognition_framework, primate_welfare_policy_framework).
narrative_ontology:affects_constraint(ape_cognition_framework, evolutionary_continuity_hypothesis).
narrative_ontology:affects_constraint(ape_cognition_framework, research_ethics_primate_studies).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific primatology research questions (e.g., ape theory of mind, metacognition, culture) and downstream of broader scientific methodology choices. It decomposes from the colloquial 'ape cognition' into two structurally distinct claims: (1) what empirical capacities apes possess (empirical constraint with variable epsilon depending on the specific capacity), and (2) what definitions and methods legitimately count as evidence for cognition (institutional/definitional constraint with high suppression). The story focuses on the definitional/institutional constraint, which has higher extractiveness. Specific empirical claims about ape theory of mind or metacognition would form separate constraint stories with their own epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ape_cognition_framework, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
