% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions 1949 — Security Maximization Reading
 *   domain: political/legal/military
 *
 * SUMMARY:
 *   The security-maximization reading of the 1949 Geneva Conventions treats
 *   international humanitarian law as a peacetime framework whose protections
 *   yield to operational necessity in asymmetric conflict. Under this
 *   reading, states facing irregular combatants (insurgents, terrorists,
 *   unlawful combatants) have unilateral authority to suspend protections:
 *   expanding the 'unlawful combatant' category to deny POW status and habeas
 *   corpus; degrading civilian immunity via 'human shields' doctrine and
 *   collateral damage acceptance; permitting indefinite detention without
 *   trial; and normalizing coercive interrogation. The constraint is CLAIMED
 *   as tangled-rope (coordination function + asymmetric extraction) while
 *   authored metrics describe highly extractive, intensively suppressed
 *   operation. This divergence is intentional: the reading's proponents frame
 *   the Conventions as coordination (mutual restraint framework); the
 *   victimized parties and humanitarian-law advocates frame the same reading
 *   as pure extraction (state violence license). The engine measures this
 *   divergence from the structural data.
 *
 * KEY AGENTS:
 *   - national_security_apparatus: institutional agenda-setter with arbitrage exit; interprets and enforces the security-maximization reading
 *   - executive_military_authority: institutional agenda-setter/beneficiary; administers detention and interrogation under the reading
 *   - detained_irregular_combatants: powerless payers; held indefinitely without status determination or protections
 *   - civilian_populations_in_conflict_zones: powerless payers; experience collateral harm and conditional immunity
 *   - detained_persons_without_status_determination: powerless payers; exist in legal gray zone created by the reading
 *   - humanitarian_organizations: excluded organized actors; cannot enforce competing readings or access detainees
 *   - international_humanitarian_law_scholars: observers; document the structural contest between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.87).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.92).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949 — Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "political/legal/military").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'fcf4f5c2-fc36-46a4-a082-89409b425adc').
narrative_ontology:cs_kernel_codification('fcf4f5c2-fc36-46a4-a082-89409b425adc', fixed_text).
narrative_ontology:cs_authority_grounding('fcf4f5c2-fc36-46a4-a082-89409b425adc', extraction).
narrative_ontology:cs_interpretation_layer_present('fcf4f5c2-fc36-46a4-a082-89409b425adc').
narrative_ontology:cs_reading_relation('fcf4f5c2-fc36-46a4-a082-89409b425adc', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('fcf4f5c2-fc36-46a4-a082-89409b425adc', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('fcf4f5c2-fc36-46a4-a082-89409b425adc', foundational, operational_necessity_overrides_protection_text).
narrative_ontology:cs_axiom_status(operational_necessity_overrides_protection_text, holdable).
narrative_ontology:cs_axiom_grounding('fcf4f5c2-fc36-46a4-a082-89409b425adc', operational_necessity_overrides_protection_text, instrumental).
narrative_ontology:cs_axiom('fcf4f5c2-fc36-46a4-a082-89409b425adc', foundational, irregular_combatants_forfeit_lawful_status).
narrative_ontology:cs_axiom_status(irregular_combatants_forfeit_lawful_status, holdable).
narrative_ontology:cs_axiom_grounding('fcf4f5c2-fc36-46a4-a082-89409b425adc', irregular_combatants_forfeit_lawful_status, conventional).
narrative_ontology:cs_axiom('fcf4f5c2-fc36-46a4-a082-89409b425adc', secondary, state_security_maximization_paramount_in_asymmetric_conflict).
narrative_ontology:cs_axiom_status(state_security_maximization_paramount_in_asymmetric_conflict, holdable).
narrative_ontology:cs_axiom_grounding('fcf4f5c2-fc36-46a4-a082-89409b425adc', state_security_maximization_paramount_in_asymmetric_conflict, instrumental).
narrative_ontology:cs_reference_frame('fcf4f5c2-fc36-46a4-a082-89409b425adc', operational_necessity_subordinates_protection).
narrative_ontology:cs_drift_state('fcf4f5c2-fc36-46a4-a082-89409b425adc', contemporary_global_counterterrorism_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('fcf4f5c2-fc36-46a4-a082-89409b425adc', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, national_security_apparatus).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, executive_military_authority).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_persons_without_status_determination).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.87) and rising through the interval because the reading permits the security apparatus to unilaterally expand detention authority, narrow protected categories, and degrade protections without requiring external justification. Suppression is very high (0.92) because the constraint's persistence depends on active exclusion of competing readings and suppression of detainee resistance (detention without trial, restricted access, no habeas corpus path). Theater ratio rises from 0.25 to 0.48: the initial operational-necessity framing (coordination cover story) degrades over time as the constraint's extractive function becomes visible—indefinite detention reveals itself as permanent, collateral damage normalizes, and the 'non-torture' framing of coercive interrogation becomes increasingly theatrical. The measurements run on one shared time grid (t0=0 through tn=24) with every metric authored at every examined point. The interval represents the post-2001 era of asymmetric conflict (0 = early 2000s post-9/11 security expansion; 24 = contemporary plateau where indefinite detention and enhanced interrogation are normalized practices).
 *
 * PERSPECTIVAL GAP:
 *   The security apparatus seat and the payer seats should compute dramatically differently. From the institutional agenda-setter perspective, the reading adapts the Conventions to operational reality: irregular combatants reject the Conventions, so symmetrical application is impossible; security-maximization interpretation permits necessary flexibility. From the detainee perspective, the reading licenses indefinite detention, coercive interrogation, and deprivation of all legal protection. From the humanitarian seat, the reading suppresses the competing humanitarian-ceiling reading by claiming operational necessity overrides absolute minimums. The engine computes per-seat classification from this structural data without reconciling the seats to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary extraction (security apparatus → d ≈ 0.15): high institutional power, unilateral exit (can reinterpret Conventions without external constraint), global scope, generational time horizon. The power atom 'institutional' normally sits near d = 0.5 (symmetric), but exit options drop the directionality: the apparatus has arbitrage exit (can choose which reading to adopt unilaterally), which shifts d downward toward beneficiary. Victim extraction (detainees → d ≈ 1.0): powerless agents, trapped exit (no legal pathway to release or status determination), local scope, immediate time horizon. The composition of power + exit + scope yields high effective extraction despite the base extractiveness being authored at 0.87—institutional actors sitting in the target position experience d amplification because they are institutional (surprising and notable), and the engine flags this via high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled-rope, not snare, because the security apparatus genuinely believes (and can articulate a defensible reading of the text) that the Conventions permit the security-maximization interpretation. This is not pure cover. However, the asymmetric extraction (detainees bear near-total costs, security apparatus collects near-total benefits) and active enforcement (the reading's persistence requires exclusion of competing humanitarian readings, suppression of detainee resistance, and restriction of humanitarian access) make tangled-rope the proper classification over rope. The mandatrophy analysis: the founding problem (irregular combatants do not comply with the Conventions) is real and contested. The security-maximization reading's answer is that this justifies unilateral suspension of protections. But the 1977 Protocols and the principle of absolute humanitarian minimum provide alternative answers: protections apply regardless of adversary compliance. The constraint persists not because the founding problem is unsolved, but because the security apparatus has authority to choose its own reading and the competing readings lack enforcement power. This is mandatrophy by substitution: the Conventions exist as a live text; the reading that dominates practice (security-maximization) treats them as conditional rather than absolute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_necessity_threshold,
    'What level of adversary non-compliance, or what conflict characteristics, justify application of the security-maximization reading rather than the humanitarian-ceiling reading?',
    'Documented analysis of conflicts with different adversary-compliance profiles and different state responses: do states apply security-maximization readings only when irregular forces systematically violate the Conventions, or do they apply it prophylactically whenever conflict is asymmetric?',
    'If the reading is applied only in response to documented violations, it approaches the conditional-reciprocity reading. If it is applied prophylactically (in anticipation of non-compliance), it becomes unilateral suspension authority not grounded in adversary action. This determines whether the reading is genuinely responsive (bounded) or categorically permissive (unbounded).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_threshold, empirical, 'Whether security-maximization is applied conditionally or categorically.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of detainees'' resistance and humanitarian oversight primarily structural (no legal pathway to release, no habeas corpus, restricted access) or internalized (detainees accept the unlawful-combatant categorization as legitimate)?',
    'Post-release trajectory study: detainees who have been released track whether suppressive patterns persist (internalized) or dissolve (structural only). Interview evidence from detained persons about whether they view deprivation as procedurally illegitimate or substantively justified.',
    'If internalized, the constraint''s effective suppression is higher than the structural 0.92 metric suggests—detainees carry the suppression with them after exit. If structural only, remediation requires only legal pathway restoration. The distinction affects whether classification should include identity-locked exit for affected agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is internalized (identity/narrative-based) or structural (legal/institutional barriers).').

omega_variable(
    competing_reading_foreclosure,
    'Does the security-maximization reading logically foreclose the humanitarian-ceiling reading, or do both remain live interpretive positions held by different institutional seats?',
    'Textual analysis of the 1949 Conventions: does the security-maximization reading''s argument (operational necessity overrides explicit protections) logically require that the humanitarian-ceiling reading (explicit protections are non-negotiable) is false? Or are both coherent readings of the same text, with the choice between them a matter of interpretive authority rather than logical necessity?',
    'If foreclosed, the security-maximization reading wins the kernel contest definitively (only one reading can be true). If both remain live, the contest is one of institutional power (who controls interpretation authority), not logical entailment. This determines whether the constraint''s persistence is grounded in the text''s inherent properties or in power asymmetry between interpretive authorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether the security-maximization and humanitarian-ceiling readings are logically incompatible or merely differently weighted.').

omega_variable(
    false_summit_humanitarian_coordination,
    'Is the ''coordination function'' framed in the six_questions genuinely a coordination problem the reading solves, or is it a false summit—a constructed-necessity framing that masks extraction?',
    'Compare documented instances where states applied the humanitarian-ceiling reading despite facing irregular combatants: did the absence of the security-maximization reading''s interpretive flexibility result in operational failure, or did restraint prove operationally compatible?',
    'If the coordination function is genuine, the tangled-rope classification is correct (asymmetric extraction riding on real coordination). If the coordination function is a false summit, the constraint should be reclassified as snare (pure extraction wearing a coordination costume). Historical evidence from multiple jurisdictions with different readings would clarify whether the reading is necessary or merely convenient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_humanitarian_coordination, empirical, 'Whether the reading''s coordination function is genuine or a false-summit framing of pure extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gene_tr_t3, geneva_conventions_1949__security_maximization_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(gene_tr_t6, geneva_conventions_1949__security_maximization_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement(gene_tr_t12, geneva_conventions_1949__security_maximization_reading, theater_ratio, 12, 0.44).
narrative_ontology:measurement(gene_tr_t18, geneva_conventions_1949__security_maximization_reading, theater_ratio, 18, 0.48).
narrative_ontology:measurement(gene_tr_t24, geneva_conventions_1949__security_maximization_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(gene_be_t3, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 3, 0.72).
narrative_ontology:measurement(gene_be_t6, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 6, 0.77).
narrative_ontology:measurement(gene_be_t12, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 12, 0.84).
narrative_ontology:measurement(gene_be_t18, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 18, 0.87).
narrative_ontology:measurement(gene_be_t24, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 24, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(gene_su_t3, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 3, 0.85).
narrative_ontology:measurement(gene_su_t6, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 6, 0.88).
narrative_ontology:measurement(gene_su_t12, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(gene_su_t18, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 18, 0.92).
narrative_ontology:measurement(gene_su_t24, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 24, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__security_maximization_reading, 0.18).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% The 1949 Geneva Conventions constitute a contested kernel with three structurally distinct readings: the humanitarian-ceiling reading (absolute protections, non-negotiable); the conditional-reciprocity reading (protections contingent on adversary compliance); and this constraint (security-maximization reading, protections yield to operational necessity). These are NOT different observable framings of a single constraint—they have fundamentally different ε values, beneficiary structures, and enforcement mechanisms. The humanitarian-ceiling reading treats protections as natural law (near-mountain ε ≈ 0.1); the security-maximization reading treats protections as discretionary (snare/tangled-rope ε ≈ 0.87). Each reading is a separate constraint story, linked via this network field. The divergence in ε reflects different claims about the kernel's nature, not measurement ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
