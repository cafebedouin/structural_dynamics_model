% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection Permits Race as Diversity Factor in University Admissions
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint embodies one reading of the equal protection clause: the
 *   permissibility of race as one factor among many in university admissions
 *   to achieve educational diversity. The diversity reading distinguishes
 *   itself from the colorblind reading (which forbids any racial
 *   classification) and the remedial reading (which frames race-consciousness
 *   as correcting caste subordination). Under the diversity reading,
 *   universities gain discretionary authority to factor race into holistic
 *   review without requirement to show prior discrimination. Individual
 *   applicants bear uncertainty about how race weighting affected their
 *   outcome. The constraint's persistence depends on active judicial
 *   enforcement—courts must continuously approve university frameworks and
 *   reject challenges. The measured extractiveness (0.28) reflects the
 *   procedural rather than substantive character of the constraint: it is
 *   less about direct resource transfer than about discretionary authority
 *   allocation and opacity in individual evaluation.
 *
 * KEY AGENTS:
 *   - Universities: institutional beneficiary, set the diversity-weighted admissions criteria, defend the educational mission rationale
 *   - Applicants individually disadvantaged: payers in expectation, bear opacity about how race affected their individual outcome
 *   - Applicants structurally benefiting: beneficiaries of relaxed evaluation in light of underrepresentation
 *   - Competing applicant groups (Asian American, poor, rural): excluded from formal policy debate despite bearing variable effects
 *   - Courts: observers and enforcers, evaluate whether frameworks satisfy constitutional scrutiny
 *   - Legislatures: observers with structural power to alter the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.22).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Permits Race as Diversity Factor in University Admissions").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional/political").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '36a0f947-75a9-4d8a-9bef-891665d96ce4').
narrative_ontology:cs_kernel_codification('36a0f947-75a9-4d8a-9bef-891665d96ce4', fixed_text).
narrative_ontology:cs_authority_grounding('36a0f947-75a9-4d8a-9bef-891665d96ce4', lineage).
narrative_ontology:cs_interpretation_layer_present('36a0f947-75a9-4d8a-9bef-891665d96ce4').
narrative_ontology:cs_reading_relation('36a0f947-75a9-4d8a-9bef-891665d96ce4', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('36a0f947-75a9-4d8a-9bef-891665d96ce4', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('36a0f947-75a9-4d8a-9bef-891665d96ce4', foundational, institutional_discretion_in_diversity_pursuit).
narrative_ontology:cs_axiom_status(institutional_discretion_in_diversity_pursuit, holdable).
narrative_ontology:cs_axiom_grounding('36a0f947-75a9-4d8a-9bef-891665d96ce4', institutional_discretion_in_diversity_pursuit, deontological).
narrative_ontology:cs_axiom('36a0f947-75a9-4d8a-9bef-891665d96ce4', foundational, race_as_permissible_educational_factor).
narrative_ontology:cs_axiom_status(race_as_permissible_educational_factor, overridden).
narrative_ontology:cs_axiom_grounding('36a0f947-75a9-4d8a-9bef-891665d96ce4', race_as_permissible_educational_factor, empirically_contingent).
narrative_ontology:cs_reference_frame('36a0f947-75a9-4d8a-9bef-891665d96ce4', equal_protection_permits_race_consciousness_for_compelling_interests).
narrative_ontology:cs_drift_state('36a0f947-75a9-4d8a-9bef-891665d96ce4', post_students_for_fair_admissions_2023, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('36a0f947-75a9-4d8a-9bef-891665d96ce4', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, applicants_individually_disadvantaged_by_holistic_review).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).
:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as low-moderate (0.28 endpoint) because the constraint grants discretionary authority rather than direct financial transfer. Universities gain the ability to weight race in admissions; this is extractive in the sense that applicants lose the benefit of race-blind evaluation, but it is not extraction in the sense of capturing rents or resources. The measured opacity (theater_ratio) rises modestly over time (0.08 to 0.15) as universities develop increasingly complex holistic frameworks where the weighting of race becomes harder for applicants to discern. Suppression requirement remains moderate (0.22) because colorblind advocates mounted sustained legal challenges; courts had to actively defend the reading against constitutional objections. Accessibility_collapse is below-median (0.45) because the constraint leaves alternative readings (colorblindness, remedial framing) as live constitutional claims that some institutions or states pursue. Resistance is elevated (0.68) because significant constituencies contest the constraint's legitimacy throughout the interval. The measurement series tracks one shared time grid (1978, 1995, 2010, 2016, 2023) so that every metric is authored at every examined point and drift can be assessed.
 *
 * PERSPECTIVAL GAP:
 *   Universities perceive this constraint as enabling their mission and enhancing educational quality for all students—they are near-beneficiary positions (d ≈ 0.2). Individual applicants who are disadvantaged by race weighting perceive opacity and lost race-neutral evaluation—they are near-target positions (d ≈ 0.75-0.85). Applicants who gain admission through the diversity reading perceive access that would be denied under colorblindness—they are near-beneficiary positions (d ≈ 0.15-0.25). Courts see their role as enforcing constitutional doctrine, neither benefiting nor paying directly, though their decisions materially affect university discretion and applicant outcomes—they are closest to symmetric (d ≈ 0.5). The engine computes these divergences from the structural data: beneficiary/victim declarations plus power level plus exit options. The authored claim (rope: genuine coordination with minimal extraction overhead) and the authored metrics (moderate extractiveness, sustained resistance, measured suppression) do not reconcile to each other—that divergence is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are declared as the sole beneficiary because they gain discretionary authority and the institutional power to implement it without bearing the opacity cost. Applicants individually disadvantaged are declared as victims because they lose the certainty of race-blind evaluation and cannot easily determine how race weighting affected their outcome—their exit is to apply elsewhere, but that does not recover the original decision. The tension here is that the benefiting applicants (those who gain admission through diversity weighting) are also organized around the constraint but benefit from it; they are not declared as victims. From the structural perspective: universities (institutional power, mobile exit—can adjust criteria) are at low d; disadvantaged applicants (powerless, constrained exit—cannot know their outcome or change it) are at high d; benefiting applicants sit between (powerless, constrained exit, but access benefit cancels the disadvantage). No directionality overrides are necessary; the structural derivation captures the real relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical racial segregation, underrepresentation in higher education) is contested in status by the colorblind reading, which argues the problem has been substantially remedied by formal non-discrimination law. The diversity reading asserts the problem remains live because colorblindness alone has not produced diverse student bodies. The constraint persists because universities find it instrumentally valuable (diversity is claimed to improve education) AND because courts have affirmed its constitutionality. Mandatrophy would arise if courts revoked the constitutional permission—which occurred in 2023 with Students for Fair Admissions v. Harvard. That revocation is outside this interval endpoint but visible in the projection: the constraint's founding problem (need for diversity to cure segregation) is live from the diversity reading's perspective, but the constraint itself is now defunct at the institutional level in actual university practice. The measured theater_ratio plateau (0.15 in 2016 and 2023) suggests the constraint's performative element did not grow in the final years—universities continued to justify diversity as real educational benefit, not as theater. The subsequent revocation is a validity event, not a theater-driven degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_as_educational_benefit,
    'Does student body racial diversity produce the educational benefits universities claim—improved critical thinking, reduced stereotype threat, better preparation for a diverse society?',
    'Longitudinal empirical research on student outcomes (learning gains, critical thinking measures, post-graduation civic engagement) comparing cohorts in diverse vs. non-diverse student bodies, controlling for peer effects and selection bias.',
    'If diversity produces measurable educational benefits, the constraint''s coordination function is validated and extractiveness interpretation shifts toward genuine coordination. If no benefit is found, the diversity rationale becomes theater and the constraint reclassifies toward snare (extraction dressed in pedagogical language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_as_educational_benefit, empirical, 'Whether educational diversity produces measurable benefits to justify institutional discretion.').

omega_variable(
    race_factor_vs_proxy,
    'When universities use race as one factor in holistic review, is race operating as a direct marker of lived experience and perspective, or as a proxy for socioeconomic disadvantage that could be measured directly?',
    'Comparative analysis of admissions outcomes under race-conscious vs. socioeconomic-conscious holistic review in jurisdictions where both have been tried; examination of how universities justify race weighting in internal policy documents.',
    'If race is mainly a proxy for disadvantage, the constraint obscures a more transparent alternative (socioeconomic weighting); this supports reclassification as snare (extracting institutional discretion through misdirection). If race captures information about lived experience beyond class (identity-based discrimination, cultural knowledge), the constraint''s informational role is substantive and the extraction interpretation is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_factor_vs_proxy, empirical, 'Whether race operates as direct information or as a proxy for class-based disadvantage.').

omega_variable(
    colorblind_vs_diversity_competing_readings,
    'Are the colorblind and diversity readings genuinely coexistent within equal protection doctrine, or does one logically foreclose the other?',
    'Jurisprudential analysis of how courts frame the competing doctrines; analysis of whether a single institutional framework can coherently hold both. The 2023 Students for Fair Admissions decision empirically resolves this: the Supreme Court held that colorblindness FORECLOSES diversity weighting, treating them as incompatible within equal protection.',
    'This omega is empirically resolved by the 2023 SCOTUS decision: the colorblind reading forecloses the diversity reading within current U.S. constitutional law. The relationship in cs_structure.reading_relations should reflect this foreclosure, not coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_diversity_competing_readings, conceptual, 'Logical relationship between colorblind and diversity readings of equal protection.').

omega_variable(
    institutional_opacity_and_fairness,
    'Does the permissibility of race as one factor among many inherently introduce opacity that violates fair-notice requirements, or can universities transparently communicate race weighting and remain within bounds of the diversity reading?',
    'Empirical audit of university admissions policies: do they disclose race weighting explicitly, or is race-consciousness de facto but not formally stated? If transparency is achievable without triggering legal challenge, opacity is institutional choice, not structural necessity.',
    'If opacity is structural (cannot disclose race weighting without inviting successful challenge), the constraint''s extractiveness rises because unfair opacity is built in. If opacity is institutional choice, the constraint could be reformed toward transparency without abandoning the diversity reading, lowering extractiveness and theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_opacity_and_fairness, empirical, 'Whether race-factor opacity is structural to the diversity reading or an institutional choice.').

omega_variable(
    kernel_reading_foreclosure_status,
    'Does the 2023 Students for Fair Admissions decision establish that colorblindness FORECLOSES the diversity reading (one reading logically rules out the other), or are they still coexistent as competing readings held by different parties?',
    'The 2023 SCOTUS ruling directly addresses this: the majority opinion holds that equal protection requires colorblindness and forbids race-conscious admissions, treating the two readings as incompatible. This resolves the omega from ''open question'' to ''empirically established foreclosure.''',
    'If colorblind reading forecloses diversity reading: the reading_relations entry should change from coexists_with to forecloses, and the diversity reading becomes a historical but no longer operative interpretation. If the decision is overturned or the contest remains live: the relations remain open.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_status, empirical, 'Relationship between colorblind and diversity readings resolved by 2023 constitutional decision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement(equa_tr_t1995, equal_protection_commitment__diversity_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_commitment__diversity_reading, theater_ratio, 2010, 0.13).
narrative_ontology:measurement(equa_tr_t2016, equal_protection_commitment__diversity_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.18).
narrative_ontology:measurement(equa_be_t1995, equal_protection_commitment__diversity_reading, base_extractiveness, 1995, 0.22).
narrative_ontology:measurement(equa_be_t2010, equal_protection_commitment__diversity_reading, base_extractiveness, 2010, 0.26).
narrative_ontology:measurement(equa_be_t2016, equal_protection_commitment__diversity_reading, base_extractiveness, 2016, 0.29).
narrative_ontology:measurement(equa_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.15).
narrative_ontology:measurement(equa_su_t1995, equal_protection_commitment__diversity_reading, suppression_requirement, 1995, 0.18).
narrative_ontology:measurement(equa_su_t2010, equal_protection_commitment__diversity_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(equa_su_t2016, equal_protection_commitment__diversity_reading, suppression_requirement, 2016, 0.23).
narrative_ontology:measurement(equa_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal protection commitment kernel is contested across three readings: colorblind (forbids racial classification), diversity (permits race as one factor for diversity), and remedial (permits race-conscious correction of subordination). Each reading is a separate constraint story with its own ε, beneficiary/victim structure, and type classification. They are linked via network.affects_constraints because they share the same constitutional foundation and compete for judicial endorsement and institutional implementation. The diversity reading (this story) treats race-consciousness as permissible in service of institutional diversity missions; the colorblind reading rejects all racial classification; the remedial reading reframes the constraint as affirmatively corrective rather than merely permissive. The three readings emit different constraint classifications because their structural features differ: diversity is procedural (discretionary authority), remedial is corrective (past-wrongs focused), colorblind is categorical (prohibition). All three inhabit equal protection jurisprudence; they are not the same constraint viewed from different angles—they are structurally distinct claims with different ε values, beneficiary/victim structures, and persistence mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
