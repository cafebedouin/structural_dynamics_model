% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary: Balanced Contestation Reading
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   The interpretive boundary between judicial and legislative authority
 *   represents a foundational constitutional tension. This story instantiates
 *   ONE reading of that contested kernel: the balanced contestation reading,
 *   in which both institutions hold legitimate but bounded authority. Courts
 *   interpret within jurisdictional domains; legislatures retain ultimate
 *   sovereign power but are constrained by norms of judicial independence and
 *   international constitutional obligations. The constraint exhibits
 *   tangled-rope structure at the institutional level (genuine coordination
 *   in maintaining constitutional legitimacy alongside asymmetric constraint
 *   on executive and policy domains) and varies by policy domain and
 *   historical moment. The three sibling readings — judicial supremacy
 *   (courts as final interpreters) and parliamentary sovereignty
 *   (legislatures as ultimate authority) — represent alternative
 *   institutional configurations that remain live in different constitutional
 *   traditions and historical moments.
 *
 * KEY AGENTS:
 *   - Judiciary: Institutional beneficiary (institutional/arbitrage) — gains interpretive authority within jurisdictional bounds; experiences constraint as coordination framework preserving independence
 *   - Legislature: Institutional beneficiary (institutional/arbitrage) — retains ultimate sovereign power through amendment capacity; experiences constraint as mutual respect arrangement
 *   - Subordinate Executive: Primary victim (powerless/trapped) — trapped between judicial review and legislative oversight; bears extraction from both
 *   - Constitutional Reform Coalition: Organized secondary agent (organized/constrained) — seeks clarification or revision of boundary; constrained by amendment difficulty
 *   - Policy Communities: Moderate secondary agents (moderate/constrained) — experience variable extraction across domains; constrained by dual institutional oversight
 *   - Comparative Constitutional Network: International learning agents (organized/mobile) — see boundary as temporary solution embedded in supranational harmonization
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.48).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary: Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '67a5cb38-bdd0-4622-95ab-e77dd6994439').
narrative_ontology:cs_kernel_codification('67a5cb38-bdd0-4622-95ab-e77dd6994439', formalized).
narrative_ontology:cs_authority_grounding('67a5cb38-bdd0-4622-95ab-e77dd6994439', lineage).
narrative_ontology:cs_interpretation_layer_present('67a5cb38-bdd0-4622-95ab-e77dd6994439').
narrative_ontology:cs_reading_relation('67a5cb38-bdd0-4622-95ab-e77dd6994439', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('67a5cb38-bdd0-4622-95ab-e77dd6994439', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('67a5cb38-bdd0-4622-95ab-e77dd6994439', foundational, mutual_institutional_legitimacy_required).
narrative_ontology:cs_axiom_status(mutual_institutional_legitimacy_required, holdable).
narrative_ontology:cs_axiom_grounding('67a5cb38-bdd0-4622-95ab-e77dd6994439', mutual_institutional_legitimacy_required, deontological).
narrative_ontology:cs_axiom('67a5cb38-bdd0-4622-95ab-e77dd6994439', foundational, sequential_not_concurrent_supremacy).
narrative_ontology:cs_axiom_status(sequential_not_concurrent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('67a5cb38-bdd0-4622-95ab-e77dd6994439', sequential_not_concurrent_supremacy, instrumental).
narrative_ontology:cs_reference_frame('67a5cb38-bdd0-4622-95ab-e77dd6994439', institutional_dialogue_framework).
narrative_ontology:cs_drift_state('67a5cb38-bdd0-4622-95ab-e77dd6994439', contemporary_constitutional_pluralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('67a5cb38-bdd0-4622-95ab-e77dd6994439', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_legitimacy).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, subordinate_executive).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, clarified_policy_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE EXECUTIVE (SNARE) — Trapped between judicial review authority and legislative accountability. Executive cannot exit judicial oversight or legislative constraint; bears extraction from both institutional powers simultaneously. No meaningful escape route. Maximal experienced suppression.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JUDICIARY (ROPE) — Experiences the boundary as coordination mechanism: judicial independence enables legitimate review of state action within jurisdictional bounds. Court benefits from negotiated autonomy; enforcement of interpretive limits is not experienced as extraction but as mutual institutional respect. Low effective extraction due to arbitrage capacity and institutional recognition.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATURE (ROPE) — Retains ultimate sovereign power through legislative amendment and constitutional revision. Legislature experiences the boundary as coordination: recognizing judicial independence maintains legitimacy of both institutions. Can arbitrage by revising basic law (high-cost but available). Net beneficiary from the constraint's stability.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM COALITION (TANGLED ROPE) — Organized institutional actors (civil society, legal scholars, minority parties) see the boundary as generating genuine coordination (mutual institutional respect) alongside extraction (locked out of day-to-day policy adjustment without constitutional consensus). High agency but real constraints on unilateral revision.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICY COMMUNITY / DOMAIN-SPECIFIC (TANGLED ROPE) — Moderately powerful actors (industry, professional associations, advocacy groups) experience the constraint differently across policy domains. Some domains show coordination (shared expertise reduces interpretation disputes), others show extraction (locked out by competing judicial/legislative readings). Effective extraction varies by domain and historical moment.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPARATIVE LEARNING NETWORK (SCAFFOLD) — Internationally organized constitutional scholars and comparative law practitioners see the balanced boundary as a temporary institutional solution undergoing generational refinement. The constraint exhibits sunset logic: as comparative constitutional norms mature and regional (EU, African Union, ASEAN) harmonization deepens, bilateral court-legislature contestation becomes embedded in supranational frameworks. Mobile options increase through legal harmonization.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal/civilizational perspective, the separation of powers is a natural structural feature of governance: institutional differentiation is inherent to rule-of-law systems. The constraint appears immutable — any system claiming constitutional legitimacy must distribute interpretive authority. However, structural data reveals this as a false summit: the specific boundary (where courts stop, where legislatures retain power) is contingent, contested, and institutionally constructed, not natural.
constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(basic_law_interpretive_boundary__balanced_contestation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The balanced reading produces genuine institutional coordination — both courts and legislatures benefit from maintaining each other's legitimate spheres — alongside moderate extraction from the executive and policy communities. The executive cannot escape dual oversight; policy actors must navigate contested boundaries. However, extractiveness is not high (0.46+) because the arrangement is genuinely negotiated, not imposed unilaterally. The trajectory (0.28 → 0.42 over 40 years) reflects gradual boundary intensification as courts expand interpretive scope while legislatures resist encroachment. Suppression (0.48): Moderate. Significant barriers to escaping the boundary include constitutional entrenchment, judicial review doctrine, and international norms of judicial independence. But suppression is not total — legislatures can amend, courts negotiate with political branches, and international pressure creates alternative exit routes. Theater ratio (0.55): Moderate-high. The institutional dialogue contains performative elements — constitutional reasoning that obscures power bargaining, doctrinal language that masks domain-specific negotiation — but the coordination function is genuine enough that theater does not dominate. As trajectory shows (0.42 → 0.55), theater has increased over the interval as doctrinal elaboration has accumulated without proportionate institutional clarity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence across institutional positions. The judiciary sees coordination and legitimate boundary (Rope); the legislature sees mutual respect and sovereignty retention (Rope); the executive sees pure extraction (Snare); the policy communities see mixed outcomes by domain (Tangled Rope); the comparative learning network sees temporary solution with sunset toward harmonization (Scaffold); the civilizational observer risks naturalizing institutional arrangement as immutable separation of powers (Mountain, false summit). The gap reveals that 'the interpretive boundary' is not a single constraint but a family of domain-specific constraints plus an overarching coordination mechanism. The balanced reading's key insight is that the boundary is negotiated rather than fixed — it shifts by domain, historical moment, and constitutional tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary and legislature both occupy institutional power positions but experience the constraint through different structural relationships. Both are net beneficiaries of the boundary arrangement — it legitimates both institutions' roles. Judiciary derives d ≈ 0.15 (beneficiary + arbitrage exit via constitutional review independence), producing low/negative chi. Legislature derives d ≈ 0.20 (beneficiary + arbitrage exit via amendment power), producing low/negative chi. Executive derives d ≈ 0.92 (victim + trapped between dual oversight), producing high chi ≈ 1.28. Policy communities derive d ≈ 0.65 (mixed victim and beneficiary roles across domains), producing moderate chi ≈ 0.95. The analytical observer derives d ≈ 0.72 (external perspective), producing chi ≈ 1.15. No directionality overrides required — structural derivation captures the asymmetry between beneficiary institutions and victim/constrained actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The balanced contestation reading resolves potential mandatrophy by acknowledging that institutional coordination (genuine mutual benefit in maintaining legitimacy and rule of law) coexists with real extraction (executive and policy communities cannot escape the boundary). The constraint is legitimately tangled_rope: it solves the problem of maintaining two potentially rival institutions (courts and legislatures) in the same constitutional system, and this coordination function is real. Simultaneously, the boundary constrains actors who cannot participate in the negotiation (executive, policy domains), producing asymmetric extraction. No single classification is 'correct' — the presheaf over institutional positions captures the full structure. The mandatrophy dissolves when we recognize that both sibling readings (judicial supremacy, parliamentary sovereignty) would produce higher extractiveness at the cost of lower coordination legitimacy. The balanced reading trades off slightly higher extractiveness (0.38) for genuine institutional collaboration (both beneficiaries), whereas supremacy readings would produce lower extractiveness but at the cost of delegitimizing the subordinated institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_specificity_threshold,
    'At what level of policy specificity does the judiciary legitimately defer to legislative judgment, and where does deference become abdication of review authority?',
    'Longitudinal case law analysis: map precedent clusters by policy domain (criminal law, taxation, social welfare, economic regulation); measure variation in deference ratios; identify domains where courts maintain strict review vs. rational basis review; track whether domain-specific patterns are consistent across constitutional traditions or artifact of single jurisdiction',
    'If threshold is jurisdiction-invariant: extractiveness reflects universal separation-of-powers principle (mountain candidate). If threshold varies by jurisdiction and shifts over time: extractiveness reflects negotiated institutional boundary (tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_specificity_threshold, empirical, 'Legitimacy threshold for judicial deference by policy domain').

omega_variable(
    reading_foreclosure_test,
    'Does the balanced contestation reading''s core axiom (mutual institutional respect without supreme adjudicator) logically foreclose the judicial supremacy reading''s core axiom (courts as final interpreter)?',
    'Jurisprudential analysis: identify cases where balanced contestation explicitly rejects supremacy logic; check whether supremacy reading can coexist in same framework by redefining ''final'' to mean ''within jurisdiction'' rather than ''absolute''; determine if the readings differ only in framing or represent genuinely incompatible structural commitments',
    'If foreclosing: balanced contestation and judicial supremacy cannot coexist in single constitutional system. If coexisting: both readings remain live options within different institutional coalitions or historical moments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether balanced contestation forecloses judicial supremacy').

omega_variable(
    supremacy_claim_alternative,
    'Is the judiciary''s practical supremacy over statutory interpretation (final say on what law means) consistent with the legislature''s theoretical supremacy (ultimate power to amend)?',
    'Institutional power mapping: measure frequency of legislative override via constitutional amendment vs. statutory rewrite; assess cost/feasibility of each; analyze whether judicial ''final say'' on interpretation is only final until legislature revises the statute, making supremacy sequential rather than concurrent; check whether agents experience this asymmetry as coordination or extraction',
    'If amendment is infeasible/rare: practical judicial supremacy dominates; supremacy reading is more accurate than balanced contestation. If amendment is available/expected: supremacy is constrained and sequential; balanced reading captures actual negotiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supremacy_claim_alternative, empirical, 'Relationship between judicial interpretation supremacy and legislative amendment power').

omega_variable(
    performance_variance_across_domains,
    'Does the balanced boundary hold equally across constitutional domains (criminal procedure, economic regulation, social rights, electoral law), or does institutional dominance shift by domain?',
    'Comparative case law analysis across domain clusters; measure success rate of judicial overrides, legislative reversals, executive circumvention for each domain; identify domains where courts are consistently deferred to vs. overridden; assess whether domain-specific variance is evidence of stable boundary (domain expertise justifies differential deference) or evidence of unstable negotiation',
    'If variance is high and systematic by domain: the constraint is not a single boundary but multiple domain-specific constraints (decompose into separate stories per ε-invariance). If variance is low: single boundary holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_variance_across_domains, empirical, 'Institutional balance variance across constitutional policy domains').

omega_variable(
    reading_temporal_location,
    'Is the balanced contestation reading the historical norm, or is it a contingent contemporary position emerging from recent rebalancing away from either supremacy pole?',
    'Historical institutional analysis: map constitutional moment(s) when balanced reading became dominant; identify predecessor readings (judicial supremacy, parliamentary sovereignty) and their decline; assess whether contemporary reading reflects stable constitutional equilibrium or transitional moment toward new dominance pole',
    'If historically contingent moment: the reading''s stability is uncertain; omega priority shifts to drift_state projections. If stable equilibrium: the constraint''s ε may be higher than estimated (institutional arrangements are more entrenched).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_temporal_location, empirical, 'Temporal location of balanced contestation as dominant reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blib_theater_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(blib_theater_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(blib_theater_t40, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(blib_extractiveness_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(blib_extractiveness_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(blib_extractiveness_t40, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(blib_suppression_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(blib_suppression_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(blib_suppression_t40, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, executive_accountability_doctrine).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_amendment_mechanics).

% DUAL FORMULATION NOTE:
% The basic_law_interpretive_boundary kernel decomposes into three constraint stories, one per reading: judicial_supremacy_reading (ε ≈ 0.32, Mountain at institutional level), parliamentary_sovereignty_reading (ε ≈ 0.35, Rope at institutional level), and this balanced_contestation_reading (ε = 0.38, Tangled Rope). Each reading has identical base institutional actors but different structural relationships due to different axiom sets. The ε values differ because the readings produce different beneficiary/victim distributions and different suppression mechanisms. The balanced reading produces the highest ε because it distributes costs across multiple actors without achieving full institutional supremacy for either side, reducing coordination efficiency relative to either supremacy reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
