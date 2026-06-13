% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Fourteenth Amendment Equal Protection — Formal Equality Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The Fourteenth Amendment's Equal Protection Clause prohibits explicit
 *   state racial and status-based classification absent compelling
 *   justification. In its formal-equality reading, this constraint is framed
 *   as a neutral rule protecting all citizens from group-based state action.
 *   In operation, it functions as a Tangled Rope: it coordinates a principle
 *   (non-discrimination through neutral law) while simultaneously extracting
 *   from subordinated groups by blocking state corrective action targeting
 *   their accumulated disadvantage. The formal-equality reading treats
 *   structural inequality as pre-constitutional background and defines
 *   race-conscious remedy as presumptively unconstitutional, creating
 *   asymmetric enforcement: race-conscious subordination is prohibited, but
 *   race-conscious remedy faces strict scrutiny. This is distinct from and in
 *   tension with the anti-caste reading, which treats Equal Protection as
 *   requiring affirmative dismantling of hierarchy.
 *
 * KEY AGENTS:
 *   - dominant_racial_groups: Institutional beneficiaries protected from race-conscious classification; benefit from blocking corrective action
 *   - status_quo_preserving_institutions (Supreme Court majority): Agenda-setters that frame and enforce the constraint through judicial interpretation; control the operational definition of 'compelling interest'
 *   - structurally_subordinated_groups: Primary victims; identity-locked to their position and barred from state corrective action by the constraint
 *   - race_conscious_remedy_proponents: Constrained payers; must either work within strict-scrutiny doctrine or abandon remedial programs
 *   - civil_rights_lawyers: Constrained payers; career-dependent on working within the doctrinal structure despite its asymmetry
 *   - anti_caste_advocates (excluded): Would restructure the entire constraint if seated; their reading is foreclosed by the formal-equality framework itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.68).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.72).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Fourteenth Amendment Equal Protection — Formal Equality Reading").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, 'bd7c1619-169d-44e8-8a1e-1bff5f5a80b0').
narrative_ontology:cs_kernel_codification('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', fixed_text).
narrative_ontology:cs_authority_grounding('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', lineage).
narrative_ontology:cs_interpretation_layer_present('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0').
narrative_ontology:cs_reading_relation('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', fourteenth_amendment_equal_protection__anti_caste_reading, coexists_with).
narrative_ontology:cs_axiom('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', foundational, classification_neutrality_paramount).
narrative_ontology:cs_axiom_status(classification_neutrality_paramount, holdable).
narrative_ontology:cs_axiom_grounding('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', classification_neutrality_paramount, deontological).
narrative_ontology:cs_axiom('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', secondary, structural_inequality_as_background).
narrative_ontology:cs_axiom_status(structural_inequality_as_background, holdable).
narrative_ontology:cs_axiom_grounding('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', structural_inequality_as_background, conventional).
narrative_ontology:cs_reference_frame('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', post_reconstruction_statutory_neutrality).
narrative_ontology:cs_drift_state('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', contemporary_persistent_stratification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd7c1619-169d-44e8-8a1e-1bff5f5a80b0', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, dominant_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, status_quo_preserving_institutions).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, structurally_subordinated_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, race_conscious_remedy_proponents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval (t0 to t75): the constraint begins as a coordination mechanism (post-Reconstruction remedy against explicit statutory subordination) but increasingly functions to block state corrective action as structural inequality persists unmeasured. Suppression rises sharply from 0.55 to 0.72 as judicial review becomes more aggressive in invalidating affirmative action and majority-minority districts, reducing alternatives for remedial policy-makers. Theater ratio grows from 0.25 to 0.42 because the constraint's operational effect (blocking corrective action) diverges increasingly from its stated rationale (neutrality); the performance of neutrality masks the structural protection it provides to dominants. Accessibility collapse is high (0.78 structural, individual variation 0.76–0.82 at t75) because subordinated groups have no institutional path to corrective remedy within the formal-equality framework — alternatives are closed off by the doctrine itself. Resistance grows at class level (0.62→0.58, modest decline but sustained) because organized opposition persists despite suppression; individual resistance declines (0.55→0.51) as identity-locked agents internalize the constraint's framing. The coercion grid's leveled picture shows structural suppression rising fastest (0.48→0.68), indicating intensified enforcement machinery; class-level stakes inflate (0.65→0.71) as the constraint's cost concentrates on the group; organizational resistance holds stable (0.48→0.52) because institutional actors (legislatures, administrators) mount sustained challenge through continuing remedial attempts. The measurements are observed through judicial opinions, legislative testimony, and empirical studies of remedial program erosion over the interval (1971–2046 proxy frame).
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court majority and dominant institutional actors compute the constraint as Rope (genuine coordination on neutrality, justified by preventing state-sponsored subordination). Structurally subordinated groups and remedy-proponents compute it as Snare (the formal rule blocks corrective action while leaving race-encoding mechanisms untouched). The formal-equality reading produces this divergence by treating structure inequality as pre-constitutional background and neutrality as an end-state, not as a mechanism whose effects must be measured. The anti-caste reading would close the gap by reframing structural inequality as constitutional injury requiring remedy.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant_racial_groups: d ≈ 0.1–0.2 (beneficiary end). Institutional power, arbitrage-level exit options (can relocate, reorganize institutional structures), and direct benefit from non-remedial law. Status_quo_preserving_institutions: d ≈ 0.05 (beneficiary end, theoretical). Institutional power, analytical exit (can reinterpret doctrine), and benefit from controlling the doctrinal frame. Structurally_subordinated_groups: d ≈ 0.85 (target end). Organized power (large-scale group coordination), identity-locked exit (cannot exit the status without denying group identity), generational time horizon, and direct cost from blocked remedies. Race_conscious_remedy_proponents: d ≈ 0.75 (target end). Moderate power, constrained exit (career dependence), and substantial cost from strict-scrutiny gating. Anti_caste_advocates: d ≈ 0.90 (excluded target, trapped). Organized power, trapped exit (excluded from the doctrinal framework itself), and ultimate cost (their core argument is ruled inadmissible).
 *
 * MANDATROPHY ANALYSIS:
 *   This is a Tangled Rope, not a false Rope. It possesses both genuine coordination (the prohibition on state subordination through explicit classification solved a real post-Reconstruction problem) and asymmetric extraction (the same rule blocks remedial classification, preserving subordination). The coordination function is LIVE at t0 (explicit Jim Crow classification is eliminated). By t75, the founding problem (state-sponsored explicit subordination) has been solved, but the constraint persists — transitioned into blocking corrective action. This is mandatrophy: the founding problem is dead (explicit Jim Crow rules are gone), but the constraint lives, protected by the interpretation of neutrality as absolute prohibition rather than as remedy-permitting (the anti-caste reading). The formal-equality reading does NOT resolve mandatrophy; it denies that mandatrophy exists (by treating structural inequality as background, not as constitutional injury). The anti-caste reading would resolve mandatrophy by treating the founding problem as ONGOING (hierarchy persists; remedy required). This constraint-family relationship (formal_equality as mandatrophic denial of the anti_caste reading's mandatrophy-resolution) is the core analytical content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_intentional_subordination,
    'Is the persistent racial stratification in wealth, health, political representation, and education the result of ongoing intentional subordination, or of neutral law operating in a context of historical inequality?',
    'Causal analysis of post-1965 wealth accumulation, health disparities, school funding, and political district construction; comparison of race-neutral and race-conscious policy outcomes; examination of institutions that encode prior inequality without explicit racial language.',
    'If structural subordination is demonstrated and causally linked to policy (facially neutral but inequality-maintaining), the founding problem is live and the constraint blocks necessary remedy, supporting mandatrophy classification. If stratification is primarily historical residue without ongoing institutional causation, the constraint''s blocking of corrective action is less extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_intentional_subordination, empirical, 'Whether racial stratification is ongoing subordination or historical residue.').

omega_variable(
    neutrality_as_false_symmetry,
    'Can a symmetric rule (equal prohibition on race-conscious classification, regardless of intent) be neutral between hierarchy and equality when applied to subordinated and dominant groups?',
    'Theoretical analysis and empirical comparison: do race-neutral rules actually protect subordinated groups, or do they preserve prior inequality by refusing to name it? Do formal-equality and anti-caste readings both admit the same facts about structural inequality, but disagree on remedy?',
    'If neutrality is false symmetry (a symmetric rule protecting hierarchy), the constraint is Snare from the subordinated-group seat and the anti-caste reading''s case for asymmetric remedy is correct. If neutrality protects all groups from state-sponsored caste-making, the constraint is Rope and the anti-caste reading is overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_as_false_symmetry, conceptual, 'Whether formal equality between hierarchy and correction is truly neutral or false symmetry.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the formal-equality reading logically foreclose the anti-caste reading, or do they coexist as competing positions within a single constitutional framework?',
    'Textual analysis: can the phrase ''equal protection of the laws'' support both readings within a single constitutional grammar? Or does commitment to one reading require rejecting the other''s core premise?',
    'If foreclosed: the readings are mutually exclusive; one framework rules out the other. If coexisting: both readings remain live options and the contest is empirical (which actually remedies subordination) and normative (which allocation of remedy-costs is just). This affects whether kernel revision means replacing formal-equality or incorporating anti-caste alongside it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the formal-equality and anti-caste readings are logically foreclosed or coexistent.').

omega_variable(
    internalized_suppression_in_identity_locked,
    'For structurally_subordinated_groups labeled identity_locked, how much of the measured suppression (0.72) is structural (legal barriers, institutional gatekeeping) versus internalized (self-concept fused with subordination, internalized inferiority)?',
    'Post-remedy trajectory: if substantive remedial programs (affirmative action, integration, targeted investment) were implemented despite the constraint, how quickly would internalized suppression dissipate? Would identity-locked agents recover exit options if the structural barriers were removed?',
    'High internalized suppression suggests the constraint''s effect persists even after structural removal (the target carries suppression with them). This would amplify the effective extraction above the structural measure. High structural suppression suggests remedy would restore agency quickly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_in_identity_locked, empirical, 'Structural versus internalized suppression in identity-locked subordinated groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(four_tr_t10, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(four_tr_t25, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(four_tr_t40, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(four_tr_t60, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(four_tr_t75, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 75, 0.42).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(four_be_t10, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(four_be_t25, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(four_be_t40, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(four_be_t60, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(four_be_t75, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(four_su_t10, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(four_su_t25, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(four_su_t40, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(four_su_t60, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(four_su_t75, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fourteenth_amendment_equal_protection__formal_equality_reading, 0.12).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% The formal-equality and anti-caste readings of the Fourteenth Amendment Equal Protection kernel are structurally distinct constraints with incompatible ε-valuations and beneficiary/victim structures. The formal-equality reading treats structural inequality as pre-constitutional background and blocks race-conscious remedy (low ε for remedy, blocking mechanism). The anti-caste reading treats structural inequality as constitutional injury and requires affirmative remedy (high ε for subordination-perpetuation, affirmative obligation). The two readings share the same contested kernel (the Equal Protection clause) but instantiate different constraints with different extracted costs and remedial capacities. Both stories are required for accurate measurement of the kernel's structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
