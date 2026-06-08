% ============================================================================
% CONSTRAINT STORY: hybrid_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_continuity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_continuity_reading
 *   human_readable: Hybrid Continuity Reading of Hebrew Vitality
 *   domain: sociolinguistics/language_revitalization/jewish_studies
 *
 * SUMMARY:
 *   The hybrid continuity reading of Hebrew vitality emerged in
 *   sociolinguistic scholarship as a synthesis of two competing accounts: the
 *   liturgical preservation view (Hebrew survived because it was maintained
 *   in religious practice) and the native daily use view (Hebrew was revived
 *   because Zionist language planners created a community of native
 *   speakers). The hybrid reading claims both components were necessary:
 *   liturgical preservation provided the linguistic substrate and cultural
 *   continuity, while Zionist reconstruction created the conditions for daily
 *   native use. This reading functions as a coordination mechanism among
 *   scholars and language planners by reframing the binary dispute as a false
 *   dichotomy. The constraint's low extractiveness (0.12) reflects that it is
 *   primarily an analytical synthesis rather than an actionable policy
 *   framework — it clarifies interpretation but does not directly allocate
 *   resources or impose costs. The constraint's low suppression (0.08)
 *   reflects that alternative framings remain available and are not
 *   foreclosed by adopting the hybrid view. The modest theater ratio (0.15)
 *   indicates that the reading has minimal performative content — it is a
 *   substantive empirical claim about dual causation rather than a rhetorical
 *   move.
 *
 * KEY AGENTS:
 *   - Linguists Studying Revitalization: Beneficiary (analytical/analytical) — gain interpretive framework that resolves liturgical-vs-native contest
 *   - Language Planning Practitioners: Beneficiary (institutional/mobile) — adopt dual-strategy framework for revival projects
 *   - Communities Attempting Revival: Beneficiary (moderate/constrained) — gain clarity on necessary components for revival efforts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_continuity_reading, 0.12).
domain_priors:suppression_score(hybrid_continuity_reading, 0.08).
domain_priors:theater_ratio(hybrid_continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hybrid_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hybrid_continuity_reading, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_continuity_reading, rope).
narrative_ontology:human_readable(hybrid_continuity_reading, "Hybrid Continuity Reading of Hebrew Vitality").
narrative_ontology:topic_domain(hybrid_continuity_reading, "sociolinguistics/language_revitalization/jewish_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_continuity_reading, '6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6').
narrative_ontology:cs_kernel_codification('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', distributed).
narrative_ontology:cs_authority_grounding('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', distributed).
narrative_ontology:cs_reading_relation('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', hybrid_continuity_reading__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', hybrid_continuity_reading__native_daily_reading, coexists_with).
narrative_ontology:cs_axiom('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', foundational, substrate_reconstruction_complementarity).
narrative_ontology:cs_axiom_status(substrate_reconstruction_complementarity, holdable).
narrative_ontology:cs_axiom_grounding('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', substrate_reconstruction_complementarity, empirically_contingent).
narrative_ontology:cs_axiom('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', secondary, single_factor_insufficiency).
narrative_ontology:cs_axiom_status(single_factor_insufficiency, holdable).
narrative_ontology:cs_axiom_grounding('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', single_factor_insufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', post_revival_scholarly_synthesis).
narrative_ontology:cs_drift_state('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6ffc8e0c-fcec-4b45-9dbd-d25900d94fb6', '').
narrative_ontology:cs_kernel_id(hybrid_continuity_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_continuity_reading, linguists_studying_revitalization).
narrative_ontology:constraint_beneficiary(hybrid_continuity_reading, language_planning_practitioners).
narrative_ontology:constraint_beneficiary(hybrid_continuity_reading, communities_attempting_revival).
narrative_ontology:constraint_vindicates(hybrid_continuity_reading, substrate_necessity_thesis).
narrative_ontology:constraint_vindicates(hybrid_continuity_reading, reconstruction_necessity_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ANALYTICAL OBSERVER (ROPE) — The hybrid continuity reading functions as a coordination mechanism among scholars and practitioners: it resolves the liturgical-vs-native dispute by reframing both as necessary components. Low extraction, minimal suppression. The constraint coordinates interpretation of the Hebrew case without foreclosing alternative framings.
constraint_indexing:constraint_classification(hybrid_continuity_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 2: LANGUAGE PLANNING INSTITUTION (ROPE) — Institutions engaged in language revitalization projects (Hebrew Language Committee successors, other revival movements) adopt this reading as a practical coordination framework: preserve liturgical substrate while building native speaker base. Benefits from the synthesis without being trapped by it.
constraint_indexing:constraint_classification(hybrid_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: REVIVAL COMMUNITY PARTICIPANT (ROPE) — Individual participants in language revival efforts (second-language learners, heritage speakers, educators) experience this reading as a clarifying synthesis: both traditional learning and daily use matter. Modest coordination benefit, low coercion.
constraint_indexing:constraint_classification(hybrid_continuity_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_continuity_reading_tests).
:- end_tests(hybrid_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Low. The hybrid reading coordinates interpretation and planning but does not directly extract resources from any party. Some modest extraction may occur if the reading's adoption channels funding toward dual-strategy revival programs at the expense of single-focus approaches, but this is second-order and reflects genuine coordination rather than rent-seeking. Suppression (0.08): Very low. The hybrid reading does not foreclose alternative interpretations — liturgical-only and native-only readings remain live positions in the literature. Scholars and practitioners can exit the hybrid framing without penalty. Theater ratio (0.15): Low. The reading makes substantive empirical claims (substrate necessity, reconstruction necessity) that are in principle falsifiable through comparative language revival case studies. The small performative component reflects that the synthesis has some rhetorical appeal ('both matter') independent of its empirical accuracy.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as Rope, reflecting genuine coordination with minimal extraction. The analytical observer sees the reading as an interpretive synthesis that coordinates scholarly discourse. Language planning institutions see it as a practical framework for dual-strategy revival efforts. Individual revival participants see it as a clarifying guide to their own efforts. The absence of a perspectival gap is diagnostically significant: the hybrid reading functions uniformly as coordination across observation sites, which supports its claimed status as an analytical synthesis rather than an interest-driven framing. If the reading were naturalizing a contingent institutional arrangement (false summit), we would expect the analytical perspective to classify as Mountain while other perspectives revealed extraction. The uniform Rope classification indicates the constraint is what it claims to be: a coordination mechanism among parties attempting to understand and replicate the Hebrew case.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary structure (linguists, language planners, revival communities) and the absence of victims reflect that the hybrid reading coordinates interpretation without imposing costs. The linguists are primary beneficiaries — they gain a framework that resolves the liturgical-vs-native dispute. The language planning institutions are secondary beneficiaries — they adopt the dual-strategy framework for practical revival efforts. The revival communities are tertiary beneficiaries — they gain clarity without being coerced into either the liturgical or native daily framing. The constraint's low extractiveness derives from the absence of a clear loser: no party's resources or options are constrained by the hybrid reading's adoption. The constraint benefits from being a scholarly synthesis rather than a policy mandate — it coordinates without compelling.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy. The hybrid reading's mandate (synthesize the liturgical and native daily accounts into a dual-causation framework) remains aligned with its function (coordinate interpretation of the Hebrew case and guide language revival planning). The modest increase in theater ratio (0.10 → 0.15) over the interval reflects some rhetorical elaboration ('both matter' becomes a slogan) but does not indicate that the reading's analytical function has been displaced by performance. The reading has not outlived its purpose — it continues to serve as a working synthesis for scholars and practitioners engaging with the Hebrew vitality case and its implications for other revival efforts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_contest,
    'Is the hybrid continuity reading a resolution of the liturgical-vs-native contest, or merely a third position that coexists with the original dispute?',
    'Longitudinal analysis of citation patterns and adoption in language planning discourse: does the hybrid reading replace the binary framings, or do all three persist as competing interpretations?',
    'If resolution: the reading successfully reframes the kernel. If coexistence: the reading adds complexity without resolving the original contest, and the kernel remains distributed across three incompatible framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Whether hybrid reading resolves or extends the kernel contest').

omega_variable(
    substrate_necessity_operationalization,
    'What constitutes ''sufficient'' liturgical substrate for revival to be possible? Can any preserved textual tradition serve, or does Hebrew''s specific religious centrality matter?',
    'Comparative analysis of revival attempts with varying substrate depths (Irish, Cornish, Manx vs Hebrew, Modern Greek). Identify threshold conditions for substrate sufficiency.',
    'If Hebrew-specific factors matter: the reading generalizes poorly to other revival cases. If any substrate suffices: the reading is a transferable framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_necessity_operationalization, empirical, 'Operationalizing substrate necessity for language revival').

omega_variable(
    reconstruction_agency_attribution,
    'Does the ''reconstruction'' component require coordinated institutional action (Zionist language planning, state support), or could distributed grassroots revival produce the same outcome given sufficient time?',
    'Counterfactual analysis of revival trajectories: compare Hebrew (institutional) to Irish (mixed), Cornish (grassroots), Livonian (failed despite some institutional support). Identify whether coordination structure predicts outcome.',
    'If institutional coordination is necessary: the reading vindicates centralized language planning. If grassroots suffices: the Zionist institutional role is overstated, and the reading''s emphasis on reconstruction reflects historiographic bias toward the institutional narrative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstruction_agency_attribution, empirical, 'Whether reconstruction requires institutional coordination or can emerge grassroots').

omega_variable(
    sibling_reading_structural_delta,
    'What structural features distinguish this reading from the liturgical and native daily readings — beyond the analytical synthesis claim?',
    'Cross-reading comparison of beneficiary structures, extractiveness, and institutional adoption patterns. Identify which actors prefer which reading and why.',
    'If beneficiary structures differ: the readings are not merely analytical framings but reflect different institutional interests. If beneficiary structures are identical: the readings are genuinely conceptual alternatives rather than interest-driven positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural differentiation of sibling readings beyond analytical framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_cont_tr_t0, hybrid_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hybrid_cont_tr_t25, hybrid_continuity_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(hybrid_cont_tr_t50, hybrid_continuity_reading, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(hybrid_cont_be_t0, hybrid_continuity_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hybrid_cont_be_t25, hybrid_continuity_reading, base_extractiveness, 25, 0.1).
narrative_ontology:measurement(hybrid_cont_be_t50, hybrid_continuity_reading, base_extractiveness, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_continuity_reading, information_standard).

% DUAL FORMULATION NOTE:
% The hybrid continuity reading is one of three constraint stories modeling the hebrew_vitality kernel. The other two (liturgical_reading and native_daily_reading) are sibling readings linked via cs_structure.reading_relations. All three readings share the same kernel but differ in causal emphasis and beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
