% ============================================================================
% CONSTRAINT STORY: keltner_relationship_evaluation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_keltner_relationship_evaluation, []).

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
 *   constraint_id: keltner_relationship_evaluation
 *   human_readable: The Keltner Relationship List
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The Keltner Relationship List is a 15-question diagnostic framework
 *   designed to evaluate the psychological and social health of romantic
 *   relationships. It has become institutionalized in couples therapy,
 *   relationship counseling, self-help literature, and popular psychology.
 *   The constraint emerges from the structural tension between the
 *   framework's genuine coordination function (providing couples with
 *   structured language for discussing relationship health) and its
 *   extractive function (establishing diagnostic authority that affects power
 *   dynamics within couples and constrains individual agency). The framework
 *   creates asymmetric access to knowledge: therapists and counselors use it
 *   to structure their professional practice, while couples being assessed
 *   have limited ability to contest results or reframe their relational
 *   experience. Over the past 10 years, the theater ratio has increased as
 *   the framework has become more institutionalized without commensurate
 *   validation of its predictive power — it persists through therapeutic
 *   authority and cultural legitimacy rather than demonstrated functional
 *   efficacy.
 *
 * KEY AGENTS:
 *   - Evaluated Individuals: Primary victims (powerless/trapped) — individuals whose relationships are assessed by the framework; cannot exit assessment without relationship costs
 *   - Relationship Couples: Secondary victims/beneficiaries (moderate/constrained) — receive coordination benefit (structured reflection) but face power asymmetry and constraint from diagnostic authority
 *   - Relationship Counselors/Therapists: Primary beneficiaries (institutional/arbitrage) — gain professional authority, structured assessment tool, and diagnostic language with minimal cost; can adopt or discard framework freely
 *   - Diagnostic Authority: Institutional actor (institutional/arbitrage) — maintains authority through therapeutic legitimacy and cultural embedding; benefits from framework's continued use
 *   - Alternative Assessment Movements: Organized critics (organized/constrained) — building alternative frameworks that reject pathologizing diagnostic models; see Keltner List as transitional
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing culturally contingent therapeutic practices as universal relationship laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(keltner_relationship_evaluation, 0.38).
domain_priors:suppression_score(keltner_relationship_evaluation, 0.42).
domain_priors:theater_ratio(keltner_relationship_evaluation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(keltner_relationship_evaluation, extractiveness, 0.38).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(keltner_relationship_evaluation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(keltner_relationship_evaluation, tangled_rope).
narrative_ontology:human_readable(keltner_relationship_evaluation, "The Keltner Relationship List").
narrative_ontology:topic_domain(keltner_relationship_evaluation, "social/psychological").

domain_priors:requires_active_enforcement(keltner_relationship_evaluation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(keltner_relationship_evaluation, diagnostic_authority).
narrative_ontology:constraint_beneficiary(keltner_relationship_evaluation, relationship_counselors).
narrative_ontology:constraint_victim(keltner_relationship_evaluation, evaluated_individuals).
narrative_ontology:constraint_victim(keltner_relationship_evaluation, relational_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EVALUATED PARTNER (SNARE) — Individuals being assessed by the framework have no exit option from the diagnostic apparatus. The 15-question structure creates a binding assessment that affects their lived relationship. They cannot decline evaluation without relationship consequences, cannot reframe their answers retroactively, and cannot escape the framework's authority once invoked. Maximum extraction from powerless position with no alternatives.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: RELATIONSHIP DYAD (TANGLED ROPE) — The couple being assessed receives genuine coordination benefit (structured reflection on their relationship, common language for discussing health) but also faces asymmetric extraction: the framework's authority to declare relationship health or dysfunction creates power asymmetry within the dyad. Partners may use scores as evidence in conflicts. Framework constrains but also enables communication. Mixed benefit and cost.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COUNSELORS/THERAPISTS (ROPE) — Professional practitioners benefit from the framework as a coordination tool: it provides common language, structured assessment, and diagnostic authority. They can use it or discard it with minimal cost. The framework enables their work without constraining them. Genuine beneficiary with high exit capacity.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIAGNOSTIC AUTHORITY (PITON) — The framework persists as an institutional fixture through theatrical legitimacy. Psychological diagnostics claim scientific authority but rest on performative measurement: 15 questions cannot capture the complexity of relational dynamics. The framework maintains credibility through the ritual of assessment rather than through demonstrated predictive power. Theater ratio is high because the authority derives more from the appearance of expertise than from validated functional outcomes.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE ASSESSMENT MOVEMENTS (SCAFFOLD) — Organized actors (relationship researchers, feminist critiques of relationship evaluation, neurodiversity advocates) are building alternative frameworks that reject binary health/dysfunction assessment. These alternatives see the Keltner List as a temporary coordination tool being replaced by more contextual, less pathologizing approaches. The scaffold classifies the framework as transitional — valuable now, but with a sunset as better tools mature.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational frame, some evaluation of relational health is inherent to human bonding: all dyads naturally assess whether they are functioning well. The Keltner List might appear as a natural crystallization of this evaluation instinct. However, this perspective risks naturalizing a specific cultural artifact (15-question diagnostic) as inevitable, when the framework is historically contingent and culturally embedded.
constraint_indexing:constraint_classification(keltner_relationship_evaluation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(keltner_relationship_evaluation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(keltner_relationship_evaluation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(keltner_relationship_evaluation, TR),
    TR >= 0.70.

:- end_tests(keltner_relationship_evaluation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The framework extracts authority and diagnostic power but provides genuine coordination benefit to both therapists and couples. The extraction is not as high as pure Snare (0.70+) because couples do receive value — structured reflection, common language for discussing health. However, the extraction is real: the framework's authority to declare dysfunction affects power within couples and constrains how individuals can frame their experiences. Measurement progression (0.22 → 0.38) reflects increasing institutionalization and authority consolidation over 10 years. Suppression (0.42): Moderate. Couples have some alternatives to the Keltner List (other frameworks, intuitive assessment, therapy without formal diagnostics), but within therapeutic contexts the framework often becomes standard. The gatekeeping by therapists creates suppression — individuals cannot easily contest the framework's authority or use competing assessments. Theater ratio (0.58): Moderate-high. The framework claims scientific authority through 15 quantified dimensions, but the actual predictive validity is contested and the functional value beyond the counseling process itself is unclear. Much of the framework's persistence derives from therapeutic ritual and institutional legitimacy rather than demonstrated superior outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the therapist's experience (Rope) and the evaluated individual's experience (Snare) is the core mandatrophy issue. From the therapist's view, the framework solves a coordination problem: how to structure assessment and provide clients with actionable feedback. From the evaluated individual's view, the framework imposes authority and constrains how they can understand their own relationship. Both perspectives are structurally valid — they are not measuring the same constraint from different angles, they are measuring different extraction flows. The Tangled Rope classification (moderate agent) captures the dyad's mixed experience: couples benefit from structured reflection but lose autonomy over diagnostic framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from structural position relative to diagnostic authority. Therapists benefit (low d) — they gain professional tool with arbitrage option to use it or not. Evaluated couples are constrained by authority (high d) — they cannot exit assessment once invoked and cannot contest results without jeopardizing therapeutic relationship. Individuals in couples experience maximum extraction (highest d) — they have no exit option from being evaluated and no authority to reframe results. Beneficiaries (therapists, diagnostic authority) have arbitrage exits: they can choose to use alternative frameworks. Victims (evaluated individuals) have trapped exits: declining assessment creates relationship consequences. The framework itself has constrained exit (institutional/constrained) — it persists through cultural embedding and cannot be easily replaced despite growing critiques.
 *
 * MANDATROPHY ANALYSIS:
 *   CASE STUDY IN COORDINATION-EXTRACTION CONFUSION: The Keltner List exemplifies how diagnostic frameworks can be simultaneously coordination mechanisms (providing language and structure) and extraction mechanisms (establishing authority that affects power within relationships). The mandatrophy is resolved by recognizing that the framework serves BOTH functions: it coordinates communication between therapist and clients (Rope benefit), but it also extracts diagnostic authority and constrains individual agency (Snare/Tangled Rope cost). The tension is not resolvable by choosing a single classification — the framework IS hybrid, not a misclassified pure type. The rising theater ratio (0.45 → 0.58) indicates that the framework's persistence increasingly relies on institutional legitimacy rather than validated functional outcomes. The scaffold perspective (alternative assessment movements) is the potential resolution path: if competing frameworks can establish better predictive validity or less pathologizing approaches, the Keltner List's authority will be displaced and its extraction mechanism will decay. The framework is not inherently extractive — it becomes extractive when it claims authority beyond what it can justify empirically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictive_validity_threshold,
    'Does the Keltner List''s assessment of relationship health actually predict relationship outcomes (stability, satisfaction, longevity) at rates significantly above chance?',
    'Longitudinal studies tracking couples assessed by the framework; correlation between initial scores and 5-year/10-year relationship outcomes; comparison to simpler predictors (e.g., single-question satisfaction ratings)',
    'If predictive validity < 0.40: framework is pure theater, classification shifts toward pure Piton. If validity > 0.65: coordination value increases, framework becomes stronger Rope/Tangled Rope from professional perspective. If validity 0.40-0.65: extracted value (authority claims) exceeds functional value (real prediction), confirming high theater ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictive_validity_threshold, empirical, 'Whether framework predicts actual relationship outcomes').

omega_variable(
    cultural_universality_of_dimensions,
    'Are the 15 dimensions of relationship health in the Keltner List culturally universal, or do they embed Western/individualist/therapeutic assumptions?',
    'Cross-cultural validation studies; application of framework in non-Western relationship contexts; analysis of which questions show highest item-response variance across cultural groups',
    'If universal: framework has lower extractive component (applies genuinely across contexts). If culturally contingent: framework imposes Western therapeutic norms on all relationships, increasing extraction from non-Western users and making victims broader than initially assessed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_universality_of_dimensions, conceptual, 'Whether relationship dimensions are culturally universal or embedded').

omega_variable(
    therapeutic_benefit_measurement,
    'Does the act of completing and receiving feedback on the Keltner List produce measurable therapeutic benefit for couples, or does the benefit come entirely from the subsequent counseling process?',
    'Randomized controlled trial: couples receiving Keltner assessment plus counseling vs. couples receiving counseling alone; measure relationship satisfaction improvement at endpoint',
    'If framework adds benefit: justifies some institutional authority, coordination benefit is real. If framework adds no benefit beyond placebo: extraction is higher (couples are not actually helped), theater is higher (ritual without function), Piton classification becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_benefit_measurement, empirical, 'Whether framework itself provides therapeutic benefit or only counseling does').

omega_variable(
    consent_and_reflexivity,
    'Can the evaluated individuals achieve genuine consent to assessment when the framework''s authority is imposed by therapeutic or institutional contexts (e.g., couples therapy mandate, court-ordered evaluation)?',
    'Qualitative research: interviews with couples on whether they experienced assessment as chosen vs. coerced; analysis of exit options in different contexts (voluntary therapy vs. mandatory court assessment)',
    'If consent is typically genuine: extraction decreases, suppression decreases. If consent is typically coerced: extraction and suppression both increase, framework appears more Snare-like from evaluated parties'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_and_reflexivity, conceptual, 'Whether consent to assessment is genuine or structurally coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(keltner_relationship_evaluation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kelt_tr_t0, keltner_relationship_evaluation, theater_ratio, 0, 0.45).
narrative_ontology:measurement(kelt_tr_t5, keltner_relationship_evaluation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(kelt_tr_t10, keltner_relationship_evaluation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(kelt_be_t0, keltner_relationship_evaluation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kelt_be_t5, keltner_relationship_evaluation, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(kelt_be_t10, keltner_relationship_evaluation, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(keltner_relationship_evaluation, information_standard).
narrative_ontology:affects_constraint(keltner_relationship_evaluation, therapeutic_authority_asymmetry).
narrative_ontology:affects_constraint(keltner_relationship_evaluation, relationship_pathologization_discourse).

% DUAL FORMULATION NOTE:
% The Keltner List constrains individual agency in relationship self-assessment (this story, extractiveness 0.38). It is downstream of broader therapeutic authority claims in intimate relationships (affect_constraints upstream). It influences specific pathologization discourses that categorize relationships as dysfunctional based on standardized criteria (affects_constraints downstream).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(keltner_relationship_evaluation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
