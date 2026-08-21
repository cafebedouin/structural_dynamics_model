% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Zionist Hegemony over Jewish Self-Determination (Diasporist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'diasporist reading' of Jewish
 *   self-determination. From this perspective, the dominant framework of
 *   Zionism, which ties Jewish fate to a militarized state and territorial
 *   sovereignty, has become a hegemonic constraint on Jewish collective
 *   survival and flourishing. It argues that the original mandate of securing
 *   Jewish safety has atrophied, replaced by institutional inertia and the
 *   suppression of alternative, pluralistic diaspora-based approaches. The
 *   constraint is classified as a Piton because its primary function
 *   (securing Jewish survival) is seen as having atrophied, and its
 *   persistence relies on theatrical maintenance of a narrative rather than
 *   genuine coordination, with no concentrated beneficiary meaningfully
 *   profiting from extraction in a way that would make it a Snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.55).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.75).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Zionist Hegemony over Jewish Self-Determination (Diasporist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__diasporist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '57b0b4d8-80ac-46bf-ae57-8156a2c823f8').
narrative_ontology:cs_kernel_codification('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', distributed).
narrative_ontology:cs_authority_grounding('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', distributed).
narrative_ontology:cs_reading_relation('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', jewish_self_determination__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', foundational, diaspora_pluralism_is_optimal_survival_strategy).
narrative_ontology:cs_axiom_status(diaspora_pluralism_is_optimal_survival_strategy, holdable).
narrative_ontology:cs_axiom_grounding('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', diaspora_pluralism_is_optimal_survival_strategy, conventional).
narrative_ontology:cs_axiom('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', foundational, territorial_sovereignty_is_dangerous_for_jews).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_dangerous_for_jews, holdable).
narrative_ontology:cs_axiom_grounding('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', territorial_sovereignty_is_dangerous_for_jews, empirically_contingent).
narrative_ontology:cs_reference_frame('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', diaspora_pluralism_framework).
narrative_ontology:cs_drift_state('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', contemporary_zionist_hegemony, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('57b0b4d8-80ac-46bf-ae57-8156a2c823f8', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_association).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diasporist_activists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the hegemonic narrative and institutional structures that assert Zionism as the primary mode of Jewish self-determination. Benefits from the continued existence of this framework through institutional self-preservation, narrative control, and political influence, rather than direct economic extraction. Actively suppresses alternative visions of Jewish identity and political action.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of suppressed alternatives for Jewish flourishing, including the erosion of distinct diaspora identities and the pressure to align with Zionist political agendas. Experience risk by association with Israeli state actions, which can fuel antisemitism in host countries. Their flourishing is constrained by the hegemonic framework.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, payer,
    moderate, generational, constrained, global).

% Individuals and groups within the diaspora who feel compelled to adopt a Zionist framework for their Jewish identity or political expression, often due to social pressure, institutional funding structures, or fear of being ostracized. Their self-determination is constrained by the dominant narrative.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism, payer,
    powerless, biographical, identity_locked, global).

% Jews in various parts of the world who face increased antisemitism or physical danger due to actions of the Israeli state, with which they are often conflated by non-Jewish populations. They bear the externalized costs of the militarized state without having chosen to be represented by it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_association, payer,
    powerless, biographical, constrained, global).

% Actively resist the Zionist hegemony, advocating for diaspora pluralism and minority rights. They bear the costs of marginalization, funding cuts, and accusations of disloyalty from mainstream Jewish institutions. They observe and analyze the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diasporist_activists, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__diasporist_reading, diasporist_activists, observer).

% Observe the dynamics of Jewish self-determination within their borders, often navigating the complexities of antisemitism, free speech, and foreign policy. They are not directly subject to the constraint but are affected by its global implications and the political demands it places on their Jewish citizens.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_nations, observer,
    institutional, generational, mobile, national).

% Are structurally excluded from the internal Jewish debate on self-determination, despite being central to the practical implications of Zionism. Their indigenous claims and experiences of dispossession are often dismissed or reframed within the hegemonic Zionist narrative, making their voice absent from this specific constraint's internal logic.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, palestinians, excluded,
    powerless, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the diasporist perspective, the constraint (Zionist hegemony) does not solve a genuine coordination problem for global Jewish survival, but rather imposes a singular, militarized solution that suppresses more pluralistic and secure alternatives. The original coordination function of securing Jewish safety has atrophied.
% TRANSFER_FUNCTION: Transfers political loyalty, financial resources, and narrative control from diverse Jewish communities to Zionist institutions and the Israeli state. It also transfers the burden of defending Israeli state actions onto diaspora Jews, and the risk of antisemitism by conflation.
% ABSENT_VOICES: Palestinians, anti-Zionist Jewish groups, and those advocating for genuinely pluralistic diaspora futures are systematically marginalized or excluded from mainstream discourse on Jewish self-determination. Their perspectives are actively suppressed by the hegemonic framework.
% DISAPPEARANCE_RATIONALE: If Zionist hegemony over Jewish self-determination vanished overnight, global Jewish communities would immediately begin to reorganize around diverse political expressions, cultural forms, and relationships with host nations. The pressure to align with a single state would dissipate, leading to a flourishing of pluralistic Jewish identities and political action, and a re-evaluation of historical narratives.
% FOUNDING_PROBLEM: The founding problem was the existential threat of antisemitism and the search for Jewish security and self-determination in a world hostile to Jewish life.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions claim the problem of Jewish insecurity is still live and only Zionism provides a solution. Diasporist scholars, historians, and anti-Zionist Jewish organizations argue that while antisemitism persists, Zionism has exacerbated, rather than solved, the problem of Jewish security, tying Jewish fate to a militarized state and endangering diaspora communities. Independent political analyses and historical studies from outside the benefiting parties corroborate the contested status of the founding problem's resolution.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'piton' classification reflects the diasporist view that Zionism's original mandate has largely atrophied, and its continued dominance is maintained through institutional inertia and narrative performance (high theater_ratio). Extractiveness is moderate (0.55) as it extracts loyalty, resources, and imposes risks on diaspora Jews. Suppression is high (0.75) due to the active marginalization of non-Zionist and anti-Zionist Jewish voices and institutions. Accessibility collapse is moderate (0.6) because while alternatives are suppressed, they are not entirely eliminated. Resistance is moderate (0.4) as diasporist and anti-Zionist movements exist but face significant institutional headwinds. The increasing trends in extractiveness, suppression, and theater ratio over the interval (1948-2023) reflect the growing entrenchment of Zionist hegemony and the increasing divergence from its original stated purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions, the framework is a successful Rope or even a Mountain, providing essential security and identity. From the diasporist perspective, the same structure is a Piton, an atrophied mandate that now extracts from and endangers its supposed beneficiaries, while suppressing more viable alternatives. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutions act as the agenda-setter, benefiting from the maintenance of the hegemonic framework through institutional self-preservation and narrative control. Diaspora Jewish communities, Jews coerced into Zionism, and Jews endangered by association are all payers, bearing the costs of suppressed alternatives, identity pressure, and increased risk. Diasporist activists are also payers, actively resisting the constraint. Host nations and Palestinians are observers or excluded, affected by the constraint's broader implications but not directly within its internal Jewish self-determination logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist reading explicitly frames Zionism as a case of mandatrophy. The original mandate of securing Jewish survival is seen as having been superseded by a dangerous deviation that ties Jewish fate to a militarized state. The constraint persists not because it effectively solves the founding problem, but due to institutional inertia, the suppression of alternatives, and the theatrical maintenance of a narrative of necessity. The high theater_ratio and increasing extractiveness over time are key indicators of this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zionism_as_solution_or_exacerbation,
    'Does Zionism genuinely secure Jewish survival and flourishing, or does it exacerbate threats and endanger diaspora communities?',
    'Longitudinal comparative studies of Jewish community safety and flourishing in diverse geopolitical contexts, analyzing the correlation with Zionist political alignment and Israeli state actions.',
    'If Zionism is empirically shown to exacerbate threats, the constraint''s extractiveness and suppression would be re-evaluated upwards, strengthening the Piton classification and potentially shifting it towards a Snare if concentrated beneficiaries are identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zionism_as_solution_or_exacerbation, empirical, 'Whether Zionism fulfills its stated mandate or creates new dangers.').

omega_variable(
    atrophied_vs_actively_maintained_function,
    'To what extent is the ''atrophied function'' of Zionism genuinely inert, versus actively maintained by Zionist institutions for their own self-preservation?',
    'Analysis of institutional budgets, lobbying efforts, and narrative control mechanisms of Zionist organizations. If significant resources are dedicated to maintaining the hegemony despite its perceived functional atrophy, it points to active maintenance.',
    'If actively maintained for institutional self-preservation, the ''piton'' classification might lean towards a ''snare'' if the benefits to Zionist institutions are re-evaluated as concentrated and extractive, rather than diffuse and inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophied_vs_actively_maintained_function, conceptual, 'Distinguishing genuine atrophy from active, self-serving maintenance of a defunct mandate.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the current Zionist hegemony over Jewish self-determination a natural, inevitable outcome of historical forces, or a constructed constraint maintained by identifiable agents?',
    'Historical and sociological analysis of the contingent choices, political movements, and institutional actions that led to the establishment and maintenance of Zionist hegemony, demonstrating its constructed nature.',
    'If shown to be a constructed constraint, the ''piton'' classification is reinforced, as it highlights the human agency in its persistence despite its atrophied function. If perceived as natural, it would challenge the very premise of the diasporist critique.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Ambiguity between natural outcome and constructed constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1948, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.4).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__diasporist_reading, theater_ratio, 1987, 0.5).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.58).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__diasporist_reading, theater_ratio, 2010, 0.62).
narrative_ontology:measurement(jewi_tr_t2023, jewish_self_determination__diasporist_reading, theater_ratio, 2023, 0.65).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__diasporist_reading, base_extractiveness, 1987, 0.5).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__diasporist_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(jewi_be_t2023, jewish_self_determination__diasporist_reading, base_extractiveness, 2023, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__diasporist_reading, suppression_requirement, 1987, 0.68).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__diasporist_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(jewi_su_t2023, jewish_self_determination__diasporist_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, israeli_state_legitimacy).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, antisemitism_definition).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_identity_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
