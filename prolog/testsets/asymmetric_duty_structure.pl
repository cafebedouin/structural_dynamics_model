% ============================================================================
% CONSTRAINT STORY: asymmetric_duty_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_duty_structure, []).

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
 *   constraint_id: asymmetric_duty_structure
 *   human_readable: Asymmetric Duty Structure in Moral Obligation Systems
 *   domain: moral_psychology/systems_of_obligation/agency_depletion
 *
 * SUMMARY:
 *   Asymmetric duty structures in moral obligation systems create a
 *   structural tension between the coordination function of organizing care
 *   relationships and the extraction mechanism of ego protection for
 *   duty-bearers. The constraint exhibits a 0.64 vs 0.96 chi divergence: the
 *   duty-bearer (moderate power, identity-locked exit) experiences chi ≈
 *   0.64, while the dependent (powerless, trapped exit) experiences chi ≈
 *   0.96. This divergence reveals the constraint's core extraction mechanism:
 *   duty performance (visible care labor that protects the duty-bearer's
 *   identity as 'responsible' or 'good') substitutes for duty fulfillment
 *   (actual satisfaction of the dependent's needs). The theater_ratio (0.68)
 *   reflects that much of the duty structure's activity is performative —
 *   duty-bearers engage in visible care labor that signals moral virtue while
 *   the dependent's actual needs remain unmet. The constraint is downstream
 *   of indexical_relativity_of_extraction (the mountain constraint
 *   establishing that extraction is observer-relative), demonstrating how the
 *   same moral framework appears as coordination from the beneficiary's
 *   position and as extraction from the dependent's position.
 *
 * KEY AGENTS:
 *   - Dependent with Unmet Need: Primary victim (powerless/trapped) — bears full cost of duty structure's theatrical performance; cannot exit relationship or organize resistance
 *   - Duty-Bearer: Mixed position (moderate/identity_locked) — experiences genuine coordination function (duty framework organizes care labor) alongside extraction (ego protection through performative duty that substitutes for need satisfaction); identity-locked by self-concept as 'the responsible one'
 *   - Relational Integrity: Secondary victim (abstract collective good) — the relationship's capacity for genuine mutual care is degraded by the asymmetric power structure
 *   - Moral Framework Beneficiary: Primary beneficiary (institutional/arbitrage) — religious authorities, cultural norm-setters, therapeutic frameworks that benefit from duty structure's coordination function without bearing its costs
 *   - Mutual Aid Network: Organized agents (organized/constrained) — building alternative care structures but constrained by dominant duty framework's influence on resources and legitimacy
 *   - Care Ethics Coalition: Organized agents (organized/mobile) — feminist care ethics, disability rights, relational autonomy frameworks building sunset pathway through cultural transformation
 *   - Traditional Duty Framework: Institutional actor (institutional/arbitrage) — maintains degraded duty codes through inertia; sees own process as theatrical
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees full structural pattern of coordination + extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_duty_structure, 0.64).
domain_priors:suppression_score(asymmetric_duty_structure, 0.72).
domain_priors:theater_ratio(asymmetric_duty_structure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_duty_structure, extractiveness, 0.64).
narrative_ontology:constraint_metric(asymmetric_duty_structure, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(asymmetric_duty_structure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_duty_structure, tangled_rope).
narrative_ontology:human_readable(asymmetric_duty_structure, "Asymmetric Duty Structure in Moral Obligation Systems").
narrative_ontology:topic_domain(asymmetric_duty_structure, "moral_psychology/systems_of_obligation/agency_depletion").

domain_priors:requires_active_enforcement(asymmetric_duty_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_duty_structure, duty_bearer_ego_protection).
narrative_ontology:constraint_victim(asymmetric_duty_structure, dependent_with_unmet_need).
narrative_ontology:constraint_victim(asymmetric_duty_structure, relational_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT (SNARE) — Trapped by material dependency with no exit options. Experiences maximum extraction: duty-bearer controls access to resources while framing non-delivery as the dependent's moral failure. The dependent cannot exit the relationship, cannot organize with other dependents, and bears full cost of the duty structure's theatrical performance.
constraint_indexing:constraint_classification(asymmetric_duty_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DUTY-BEARER (TANGLED ROPE) — Identity-locked by internalized moral framework that constitutes their self-concept as 'the responsible one' or 'the caregiver.' Structurally mobile (could exit the relationship) but psychologically bound by identity fusion with the duty role. Experiences genuine coordination function (the duty structure organizes care labor) alongside extraction (ego protection through performative duty fulfillment that substitutes for actual need satisfaction). The identity lock prevents seeing that duty performance has replaced duty fulfillment.
constraint_indexing:constraint_classification(asymmetric_duty_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: MORAL FRAMEWORK BENEFICIARY (ROPE) — Institutional actors (religious authorities, cultural norm-setters, therapeutic frameworks) that benefit from the duty structure's coordination function. Experience the constraint as pure coordination: the duty framework organizes care relationships and provides moral legibility. Net beneficiary with arbitrage exit — can shift between moral frameworks as needed.
constraint_indexing:constraint_classification(asymmetric_duty_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MUTUAL AID NETWORK (TANGLED ROPE) — Organized agents building alternative care structures (mutual aid, community care, disability justice frameworks) see both coordination and extraction. The duty structure provides a legible framework for organizing care labor (coordination) but also embeds asymmetric power that mutual aid seeks to dissolve (extraction). Constrained exit — can build alternatives but cannot fully escape the dominant duty framework's influence on resource allocation and social legitimacy.
constraint_indexing:constraint_classification(asymmetric_duty_structure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CARE ETHICS COALITION (SCAFFOLD) — Organized agents (feminist care ethics, disability rights, relational autonomy frameworks) see the asymmetric duty structure as a temporary problem with a sunset: as care ethics replaces duty ethics, the extraction mechanism loses force. The coalition has mobile exit (can operate outside traditional duty frameworks) and sees a generational timeline for norm shift. Low effective extraction because the coalition has agency and sees an exit path through cultural transformation.
constraint_indexing:constraint_classification(asymmetric_duty_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL DUTY FRAMEWORK (PITON) — The institutional apparatus of traditional moral obligation (religious duty codes, filial piety norms, gendered care expectations) persists through inertia despite degraded function. Theater ratio is high: duty performance (visible acts of care labor) has replaced duty fulfillment (actual need satisfaction). The framework sees its own process as degraded — maintained because alternatives haven't fully replaced it, not because it effectively organizes care.
constraint_indexing:constraint_classification(asymmetric_duty_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal perspective, the asymmetric duty structure exhibits both genuine coordination (organizing care relationships across power differentials) and systematic extraction (ego protection for duty-bearers at the expense of dependent need satisfaction). The analytical observer sees the full structural pattern: duty performance substitutes for duty fulfillment, creating a theater that protects the duty-bearer's identity while leaving the dependent's needs unmet. This is the constraint's true type — the presheaf over all observation sites.
constraint_indexing:constraint_classification(asymmetric_duty_structure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_duty_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_duty_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_duty_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_duty_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asymmetric_duty_structure, TR),
    TR >= 0.70.

:- end_tests(asymmetric_duty_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.64): High. The duty-bearer captures ego protection and social status through performative care labor, while the dependent's actual needs remain unmet. The extraction is substantial but not maximal — some genuine care does occur, and the coordination function (organizing care relationships) provides real value. The value reflects the structural asymmetry: duty performance substitutes for duty fulfillment, creating a theater that protects the duty-bearer's identity at the dependent's expense. Suppression (0.72): High. The dependent faces material dependency (economic, physical, social), legal barriers (guardianship, conservatorship, family law), publication bias against dependent narratives (care labor is valorized, dependency is stigmatized), and internalized unworthiness (the dependent believes they deserve inadequate care). The duty-bearer faces identity-fusion suppression (cannot exit without abandoning self-concept) and social pressure (duty abandonment is morally condemned). Theater ratio (0.68): High. Much of the duty structure's activity is performative: visible care labor (taking the dependent to appointments, posting about caregiving on social media, performing emotional labor in public) substitutes for actual need satisfaction (listening to the dependent's preferences, respecting autonomy, providing materially adequate support). The theater has increased over the interval as duty performance has become more visible and valorized (social media caregiving narratives, 'good parent' culture, filial piety performance) while dependent outcomes have not improved proportionally.
 *
 * PERSPECTIVAL GAP:
 *   The duty-bearer sees tangled_rope (genuine coordination function + ego protection extraction) because their identity lock prevents distinguishing duty performance from duty fulfillment. The dependent sees snare (pure extraction with no exit) because they bear the full cost of the theater. The moral framework beneficiary sees rope (pure coordination) because they capture the benefit without the cost. The mutual aid network sees tangled_rope (coordination + extraction) because they experience both the value of organized care and the harm of asymmetric power. The care ethics coalition sees scaffold (temporary problem with sunset) because they have agency and see a generational exit path. The traditional duty framework sees piton (degraded ritual) because it recognizes its own theatrical nature. The analytical observer sees tangled_rope as the constraint's true type — the presheaf over all observation sites reveals both coordination and extraction as structural features, not perspectival artifacts.
 *
 * DIRECTIONALITY LOGIC:
 *   The chi divergence (0.64 vs 0.96) is the constraint's diagnostic signature. The duty-bearer is a beneficiary (captures ego protection and status) with moderate power and identity-locked exit, yielding d ≈ 0.20 → f(d) ≈ 0.02 → chi ≈ 0.64 × 0.02 × 1.0 (local scope) ≈ 0.01 base, but the identity lock modulates this — the duty-bearer experiences the constraint as moderately extractive because the identity fusion creates psychological cost. The dependent is a victim (bears cost of unmet needs) with powerless status and trapped exit, yielding d ≈ 0.95 → f(d) ≈ 1.42 → chi ≈ 0.64 × 1.42 × 1.0 ≈ 0.91. The institutional beneficiary (moral framework) has d ≈ 0.05 → f(d) ≈ -0.12 → negative chi (net benefit). The organized agents (mutual aid, care ethics coalition) have d ≈ 0.40-0.55 → f(d) ≈ 0.40-0.75 → moderate chi. The analytical observer computes the structural chi from the base extractiveness and sees the full asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the duty structure is neither pure coordination (rope) nor pure extraction (snare) but a hybrid (tangled_rope) whose type depends on the observer's structural position. The duty-bearer's identity lock is the key mechanism: it prevents the duty-bearer from seeing that duty performance has replaced duty fulfillment, creating a theater that protects the duty-bearer's ego while leaving the dependent's needs unmet. The analytical classification (tangled_rope) is confirmed by the structural data: beneficiaries exist (duty-bearer ego protection, moral framework coordination), victims exist (dependent with unmet need, relational integrity), and active enforcement exists (social pressure, legal frameworks, internalized duty codes). The chi divergence (0.64 vs 0.96) quantifies the extraction asymmetry. The theater ratio (0.68) quantifies the substitution of performance for fulfillment. The constraint is not mislabeled — it genuinely coordinates care relationships (rope function) while simultaneously extracting from the dependent (snare function). The tangled_rope classification captures this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duty_fulfillment_measurement,
    'What constitutes duty fulfillment vs duty performance? Where is the threshold between legitimate care coordination and extractive theater?',
    'Longitudinal tracking of dependent outcomes (need satisfaction, autonomy, wellbeing) vs duty-bearer self-reported fulfillment. Correlation analysis between duty performance visibility and dependent outcome improvement.',
    'If duty performance correlates with dependent outcomes: lower extractiveness, more rope-like. If duty performance inversely correlates (high performance, low outcomes): higher extractiveness, more snare-like. Current data suggests weak or negative correlation, supporting high extractiveness classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duty_fulfillment_measurement, empirical, 'Threshold between duty fulfillment and duty performance theater').

omega_variable(
    identity_lock_reversibility,
    'Is the duty-bearer''s identity lock structurally reversible within biographical time, or does it require generational norm shift?',
    'Clinical data on identity transformation in caregiving relationships; success rates of therapeutic interventions targeting duty-based identity fusion; comparative analysis of cultures with different care ethics.',
    'If reversible at biographical scale: duty-bearer perspective shifts from tangled_rope toward rope (lower experienced extraction, higher agency). If requires generational shift: duty-bearer remains identity-locked, validating current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Timeline for duty-bearer identity lock dissolution').

omega_variable(
    suppression_mechanism_ratio,
    'What proportion of the measured suppression (0.72) is structural (material dependency, legal barriers) vs internalized (dependent''s belief in their own unworthiness, duty-bearer''s identity fusion)?',
    'Post-exit suppression trajectory analysis: if suppression persists after material barriers are removed, reclassify as partially internalized. Comparative analysis of dependents who exit vs those who remain in duty relationships.',
    'If suppression is primarily structural: removing material barriers dissolves the constraint. If suppression is primarily internalized: the constraint persists as cognitive pattern even after exit, requiring therapeutic intervention. Current estimate: 60% structural, 40% internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ratio, empirical, 'Structural vs internalized suppression mechanism ratio').

omega_variable(
    care_ethics_sunset_timeline,
    'Is the care ethics coalition''s scaffold perspective empirically grounded, or is it aspirational? What is the actual timeline for norm shift from duty ethics to care ethics?',
    'Generational cohort analysis of care ethics adoption; policy analysis of care infrastructure investment; cultural norm tracking in media, education, and legal frameworks.',
    'If sunset is real and near (10-20 years): scaffold perspective is structural. If sunset is distant or uncertain (50+ years or indefinite): scaffold perspective is aspirational, and the constraint remains tangled_rope or snare for longer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(care_ethics_sunset_timeline, preference, 'Timeline and likelihood of care ethics norm shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_duty_structure, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duty_asym_tr_t0, asymmetric_duty_structure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(duty_asym_tr_t3, asymmetric_duty_structure, theater_ratio, 3, 0.55).
narrative_ontology:measurement(duty_asym_tr_t6, asymmetric_duty_structure, theater_ratio, 6, 0.62).
narrative_ontology:measurement(duty_asym_tr_t9, asymmetric_duty_structure, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(duty_asym_be_t0, asymmetric_duty_structure, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(duty_asym_be_t3, asymmetric_duty_structure, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(duty_asym_be_t6, asymmetric_duty_structure, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(duty_asym_be_t9, asymmetric_duty_structure, base_extractiveness, 9, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_duty_structure, attachment_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of indexical_relativity_of_extraction (the mountain constraint establishing that extraction is observer-relative). The asymmetric duty structure demonstrates how the same moral framework appears as coordination from the beneficiary's position and as extraction from the dependent's position, instantiating the indexical relativity principle in a concrete interpersonal domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asymmetric_duty_structure, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
