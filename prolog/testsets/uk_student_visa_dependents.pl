% ============================================================================
% CONSTRAINT STORY: uk_student_visa_dependents
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_student_visa_dependents, []).

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
 *   constraint_id: uk_student_visa_dependents
 *   human_readable: UK Policy Restricting Dependents of International Students on Taught Postgraduate Courses
 *   domain: political/economic/immigration
 *
 * SUMMARY:
 *   The UK's 2023 policy restricting international students on taught
 *   postgraduate courses from bringing family dependents represents a
 *   high-extraction constraint justified by migration control objectives. The
 *   policy emerges from electoral pressure to reduce net migration figures
 *   while maintaining the university sector's international recruitment base.
 *   From the structural perspective, the constraint exhibits characteristics
 *   of both pure extraction (snare) and mixed coordination-extraction
 *   (tangled rope), depending on whether the net migration benefit justifies
 *   the family separation costs. International postgraduate students and
 *   their dependents experience maximal suppression: no formal appeals, no
 *   hardship exemptions, no alternative pathways to family unification.
 *   Universities experience extraction through reduced recruitment
 *   competitiveness against peer nations (Australia, Canada, US) that permit
 *   dependent entry. The UK government experiences coordination benefit
 *   through direct reduction in net migration statistics. The analytical
 *   observer risks naturalizing this as a consequence of state sovereignty,
 *   when it is actually a contingent policy choice — one of 45+ developed
 *   nations maintains this restriction.
 *
 * KEY AGENTS:
 *   - International Postgraduate Students: Primary victim (powerless/trapped) — cannot bring dependents, face family separation and dual-household financial burden for 1-2 years
 *   - Dependent Family Members (Spouses, Children, Elderly Parents): Secondary victim (powerless/trapped) — excluded entirely from entry, no appeal process, experience forced separation and financial hardship
 *   - UK Universities: Mixed actor (organized/constrained) — harmed by reduced recruitment competitiveness but benefit from selective filtering of wealthier students; cannot opt out of policy
 *   - UK Government (Home Office): Primary beneficiary (institutional/arbitrage) — captures political benefit of reduced net migration statistics; can modify or eliminate policy
 *   - Student Advocacy Coalition: Organized pressure (organized/constrained) — universities, student unions, family advocacy groups pushing for policy revision; see sunset mechanism through accumulated evidence of economic harm
 *   - Dependent Visa Administration System: Institutional artifact (institutional/constrained) — bureaucratic apparatus persists with reduced function, maintained through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_student_visa_dependents, 0.58).
domain_priors:suppression_score(uk_student_visa_dependents, 0.72).
domain_priors:theater_ratio(uk_student_visa_dependents, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_student_visa_dependents, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_student_visa_dependents, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(uk_student_visa_dependents, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_student_visa_dependents, snare).
narrative_ontology:human_readable(uk_student_visa_dependents, "UK Policy Restricting Dependents of International Students on Taught Postgraduate Courses").
narrative_ontology:topic_domain(uk_student_visa_dependents, "political/economic/immigration").

domain_priors:requires_active_enforcement(uk_student_visa_dependents).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, uk_government_migration_targets).
narrative_ontology:constraint_beneficiary(uk_student_visa_dependents, domestic_university_revenue_protection).
narrative_ontology:constraint_victim(uk_student_visa_dependents, international_postgraduate_students).
narrative_ontology:constraint_victim(uk_student_visa_dependents, student_family_units).
narrative_ontology:constraint_victim(uk_student_visa_dependents, uk_universities_recruitment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERNATIONAL STUDENT WITH FAMILY OBLIGATIONS (SNARE) — Cannot bring spouse or children; cannot exit without abandoning education or family. Trapped by both economic sunk cost (tuition deposits) and relational obligations. Bears full extraction: separated from dependents for 1-2 years, financial burden of maintaining dual households, psychological cost of family separation. No alternative entry pathway exists for dependents accompanying postgraduate students.
constraint_indexing:constraint_classification(uk_student_visa_dependents, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEPENDENT FAMILY MEMBER (SNARE) — Excluded entirely from entry. Experiences maximal extraction: forced separation, financial hardship from lost dual-income household, psychological cost of abandonment. Cannot exit — separation is structurally enforced. Suppression is total: no formal appeals process, no hardship exemptions for dependents under 18 or elderly parents.
constraint_indexing:constraint_classification(uk_student_visa_dependents, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: UK UNIVERSITIES (TANGLED ROPE) — Experience the constraint as mixed: forced to compete without the family-accompanying benefit that rival countries (Australia, Canada, US) offer. This reduces recruitment of international postgraduates, harming university revenue. But universities also benefit from the constraint's selective effect — it filters for wealthier students with extended family support networks, potentially increasing average per-student spending and reducing support service costs. Constrained exit: universities can lobby government but cannot opt out of the policy; they must absorb the recruitment loss. Extraction runs both directions: harmed recruitment but benefited selectivity filtering.
constraint_indexing:constraint_classification(uk_student_visa_dependents, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: UK GOVERNMENT MIGRATION TARGETS (ROPE) — Primary beneficiary. The constraint directly reduces net migration figures by excluding dependents. Experiences coordination benefit: the policy is a tool for meeting electoral pledges on migration control without banning international students entirely (which would harm university revenue). Arbitrage options: can adjust policy, can create exemptions, can modify targets. Net extraction runs toward this actor — they capture the political benefit (lower migration statistics) while universities and students bear costs. But this is classified as Rope rather than pure Snare because the coordination function is genuine: the policy solves the legitimate collective action problem of quantifying and controlling migration.
constraint_indexing:constraint_classification(uk_student_visa_dependents, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEPENDENT VISA ADMINISTRATION SYSTEM (PITON) — The bureaucratic apparatus for assessing dependent visa claims persists despite the dependent ban removing most of its function. Processing centers, trained staff, assessment rubrics remain in place for edge cases (doctoral students, postdoctoral researchers) while the primary category (taught postgraduates) is blocked entirely. Theater ratio high: extensive paperwork and waiting periods exist for non-postgraduate students, creating the appearance of rigorous assessment where the actual gate is categorical exclusion. The administrative system is degraded — maintained through institutional inertia, justified through inherited procedures rather than current functional necessity.
constraint_indexing:constraint_classification(uk_student_visa_dependents, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STUDENT ADVOCACY COALITION (SCAFFOLD) — Organized pressure from universities, international student unions, and family advocacy groups sees the constraint as a temporary policy response to electoral pressure. The sunset mechanism is implicit: demographic shifts (aging UK population increasing demand for skilled migrants), economic pressure from reduced university revenue, and competitive recruitment losses create incentives for policy revision. Constrained exit: advocates cannot unilaterally change policy but can accumulate pressure. Theater low: the advocacy mechanisms are functional (parliamentary petitions, media campaigns, employer evidence) rather than performative. Estimated sunset: 5-10 years as electoral cycles reset and evidence of economic harm accumulates.
constraint_indexing:constraint_classification(uk_student_visa_dependents, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint might appear as a natural limit: nation-states have inherent sovereign authority to control borders and set migration policy. Family separation is an immutable consequence of state sovereignty. However, this naturalizes what is actually a contingent policy choice — 45 other developed nations allow international students to bring dependents. The 'natural law' framing obscures that the constraint is an institutional arrangement, not a law of nature. False summit detection should flag this perspective.
constraint_indexing:constraint_classification(uk_student_visa_dependents, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_student_visa_dependents_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_student_visa_dependents, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_student_visa_dependents, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_student_visa_dependents, TR),
    TR >= 0.70.

:- end_tests(uk_student_visa_dependents_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The policy directly extracts family separation and financial hardship from affected students (estimated 40-70% of postgraduate cohort depending on dependent definition). However, the extraction is justified by a stated coordination objective (net migration control), which prevents classification as pure snare. The value reflects that the harm is severe and affects substantial populations, but the coordination rationale has empirical plausibility (if the net migration benefit is real). Suppression (0.72): Very high. The policy contains no hardship exemptions, no appeals process, and no alternative entry pathways for dependents of taught postgraduates. The categorical exclusion creates maximal suppression — students cannot negotiate, appeal, or escape the constraint through legal channels. Theater ratio (0.48): Moderate-low. The policy is operationally functional (clear categorical exclusion) rather than performative, though the underlying migration-counting methodology has theatrical elements (what counts as 'net migration' involves definitional choices). The theater ratio has increased slightly as universities have responded with rhetorical positioning (framing the restriction as temporary, highlighting competitive losses) without operational change.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between the victim perspective (student/dependent) and the beneficiary perspective (UK government). Students see a snare with no exit and no alternatives. Government sees a coordination mechanism (albeit with real costs to universities and individuals). The secondary gap lies between institutional actors: universities see harm (reduced recruitment, reputational damage) but also selective filtering benefits (wealthier, more self-sufficient student cohorts). The tertiary gap lies in temporal framing: government sees immediate migration control; universities and advocates see long-term competitive erosion and eventual policy revision as evidence accumulates.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to the extraction flow. International students with trapped exit options and victim status occupy d ≈ 0.95 (maximum target). Dependent family members with no entry pathway at all occupy d ≈ 1.0 (full extraction target). UK government with institutional power and arbitrage options (can modify policy) occupies d ≈ 0.10 (beneficiary). Universities with constrained exit (cannot opt out, but can lobby and adapt) occupy d ≈ 0.55 (mixed). Suppression (0.72) applies uniformly across all agents — it is a structural property of the constraint (categorical exclusion, no appeals) rather than context-dependent. The effective extractiveness chi experienced by each agent reflects d-dependent scaling: powerless students experience high chi; institutional government experiences negative chi (benefits); organized universities experience moderate chi (mixed).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing whether the policy's net migration benefit is real and achievable without the family separation extraction. If net migration reduction is substantial: the classification approaches rope (coordination with justifiable cost). If net migration reduction is marginal or offset by recruitment losses: the classification settles at snare (pure extraction justified by ineffective claim). The four omega variables (dependent definition scope, alternative pathways, recruitment impact, net migration counterfactual) are the empirical gates for resolving this. Current evidence suggests the policy's net migration impact is real but modest (~2-3% reduction), and universities are experiencing meaningful recruitment losses (~8-12% application decline by some institutional reports). This evidence supports the snare classification — the extraction magnitude appears to exceed the coordination benefit. However, the rope classification remains live if evidence emerges that alternative pathways effectively reduce experienced extraction, or that the net migration benefit is larger than current estimates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependent_definition_scope,
    'Does ''dependent'' include only spouses and children, or does it extend to elderly parents, adult siblings with special needs, or other extended family caregiving relationships?',
    'Policy documentation review; comparison with dependent definitions in family reunion visa categories; analysis of hardship exemption requests',
    'Narrow definition (spouse + minor children only): affects ~40% of postgraduate students. Broad definition (extended caregivers): affects ~70% of postgraduate students. Scope affects magnitude of extraction but not classification type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dependent_definition_scope, empirical, 'Scope of dependent category definition').

omega_variable(
    alternative_entry_pathways,
    'Do spouse and children of postgraduate students have legitimate alternative entry pathways (visitor visas, student visas of their own, family reunion routes) that reduce the extraction experienced?',
    'Comparative cost-benefit analysis of alternative visa routes; timeline analysis of family reunion processing; income threshold requirements for visitor and family reunion categories',
    'If viable alternatives exist: effective extraction is lower (some dependents exit via alternate routes). If no alternatives: extraction approaches maximum for trapped cohort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_entry_pathways, empirical, 'Availability of alternative dependent entry pathways').

omega_variable(
    recruitment_impact_magnitude,
    'What fraction of prospective international postgraduates decline UK offers due to the dependent restriction? Does this vary by geographic origin or family structure?',
    'University application flow analysis; comparison of UK vs competing countries'' postgraduate application rates; student survey data on decision factors',
    'If < 5% decline due to dependent restriction: policy is minimally extractive in recruitment terms. If > 15% decline: policy significantly harms university revenue and competitive position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recruitment_impact_magnitude, empirical, 'Magnitude of recruitment impact from dependent restriction').

omega_variable(
    net_migration_counterfactual,
    'What is the actual reduction in net migration achieved by the dependent restriction, compared to the counterfactual of allowing dependents? Does the policy reduce net migration by the stated target, or are the gains offset by reduced student recruitment?',
    'Longitudinal net migration statistics pre/post policy; modeling of dependent count per student; comparison to government migration target achievement',
    'If policy achieves stated migration reduction: extraction rationale is defensible as coordination mechanism (rope). If policy fails to reduce net migration or increases it indirectly: classification shifts to pure snare (extraction without functional benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_migration_counterfactual, empirical, 'Net migration counterfactual and policy effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_student_visa_dependents, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uksvd_tr_t0, uk_student_visa_dependents, theater_ratio, 0, 0.35).
narrative_ontology:measurement(uksvd_tr_t2, uk_student_visa_dependents, theater_ratio, 2, 0.42).
narrative_ontology:measurement(uksvd_tr_t4, uk_student_visa_dependents, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(uksvd_be_t0, uk_student_visa_dependents, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(uksvd_be_t2, uk_student_visa_dependents, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(uksvd_be_t4, uk_student_visa_dependents, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_student_visa_dependents, enforcement_mechanism).
narrative_ontology:affects_constraint(uk_student_visa_dependents, uk_postgraduate_visa_points_system).
narrative_ontology:affects_constraint(uk_student_visa_dependents, international_student_economic_impact).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_student_visa_dependents, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
