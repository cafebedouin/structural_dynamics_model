% ============================================================================
% CONSTRAINT STORY: diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_diversity_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: diversity_reading
 *   human_readable: Equal Protection Diversity Reading: Race-Conscious Policies for Educational Benefit
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   The diversity reading of Equal Protection is one of three structurally
 *   distinct interpretations of whether the Fourteenth Amendment permits
 *   race-conscious state action. This reading claims that race-conscious
 *   admissions policies serve compelling educational interests by assembling
 *   diverse student bodies from which all students (including white students)
 *   benefit through enhanced critical thinking, perspective exposure, and
 *   cross-racial understanding. The diversity reading positions minority
 *   students as instrumental to achieving this institutional goal,
 *   distinguishing it from the remedial reading (which frames minority
 *   students as primary beneficiaries of redress for past discrimination) and
 *   the colorblind reading (which forbids race-conscious classification
 *   entirely). The reading has been the dominant doctrine in higher education
 *   admissions for 45 years (Regents v. Bakke through Fisher v. Texas) but
 *   was foreclosed as valid constitutional doctrine by Students for Fair
 *   Admissions v. Harvard/UNC (2023). This constraint story models the
 *   diversity reading at the moment of its legal defeat, capturing both its
 *   structural logic (moderate extractiveness, genuine educational
 *   coordination) and its doctrinal vulnerability (theater ratio rising as
 *   institutions ritualize narrow tailoring procedures for admissions that
 *   are legally no longer authorized post-2023).
 *
 * KEY AGENTS:
 *   - All Students (especially non-minority students): Primary beneficiaries (moderate/constrained) — receive educational goods from diverse learning environment
 *   - Minority Students: Instrumental means (institutional/constrained) — positioned as diversity credentials rather than primary beneficiaries; face identity reduction to race-conscious classification
 *   - University Administrations: Primary extractors and coordinators (institutional/arbitrage) — benefit from authority to make race-conscious decisions; coordinate genuine educational goods; face post-2023 legal constraints
 *   - Excluded Applicants (individuals rejected partly on race): Victims (powerful/constrained) — bear costs of race-conscious selection; experience extraction with suppressed alternatives
 *   - Supreme Court Doctrinal Framework: Institutional authority (institutional/arbitrage) — establishes narrow tailoring test and compelling interest standard; post-2023 this framework becomes piton (performatively maintained despite legal reversal)
 *   - Analytical Observer: Civilizational vantage (analytical/analytical) — risks naturalizing the diversity reading as inevitable response to educational equity challenges rather than as contingent institutional design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(diversity_reading, 0.38).
domain_priors:suppression_score(diversity_reading, 0.35).
domain_priors:theater_ratio(diversity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(diversity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(diversity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(diversity_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(diversity_reading, tangled_rope).
narrative_ontology:human_readable(diversity_reading, "Equal Protection Diversity Reading: Race-Conscious Policies for Educational Benefit").
narrative_ontology:topic_domain(diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(diversity_reading, fixed_text).
narrative_ontology:cs_authority_grounding(diversity_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(diversity_reading).
narrative_ontology:cs_kernel_id(diversity_reading, equal_protection_clause).
narrative_ontology:cs_reading_relation(diversity_reading, remedial_reading, influences).
narrative_ontology:cs_reading_relation(diversity_reading, colorblind_reading, coexists_with).
narrative_ontology:cs_axiom(diversity_reading, foundational, racial_classification_for_educational_benefit_valid).
narrative_ontology:cs_axiom_status(racial_classification_for_educational_benefit_valid, holdable).
narrative_ontology:cs_axiom(diversity_reading, foundational, diverse_cohorts_serve_compelling_institutional_interest).
narrative_ontology:cs_axiom_status(diverse_cohorts_serve_compelling_institutional_interest, holdable).
narrative_ontology:cs_reference_frame(diversity_reading, narrow_tailoring_doctrine).
narrative_ontology:cs_drift_state(diversity_reading, post_students_fair_admissions_2023, gap(authority_erosion, severe, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(diversity_reading, all_students).
narrative_ontology:constraint_beneficiary(diversity_reading, higher_education_institutions).
narrative_ontology:constraint_victim(diversity_reading, non_beneficiary_applicants_excluded_on_race).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIVERSITY-VALUING STUDENTS (ROPE) — Benefit from diverse learning environment; experience constraint as coordination mechanism that solves genuine collective action problem (assembling diverse cohorts requires institutional action). Face modest constraints (time in admission process) but perceive net benefit from educational coordination.
constraint_indexing:constraint_classification(diversity_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: EXCLUDED APPLICANT (SNARE) — Individual applicants rejected partly on race-conscious grounds perceive high extraction with minimal coordination benefit to them personally. Suppressed alternatives: cannot prove discrimination, race-conscious admission is legally authorized, remedial trajectory is unclear. Constrained exit (can reapply, attend alternative schools, but career trajectory is affected). Experiences the constraint as extraction mechanism despite institutional rationale of broader good.
constraint_indexing:constraint_classification(diversity_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSITY ADMINISTRATION (TANGLED ROPE) — Manages dual function: coordinating diverse student bodies (genuine educational coordination function) while extracting authority to make race-conscious decisions (institutional benefit of non-reviewable discretion). Benefits from ambiguity between the two functions. Faces moderate suppression (legal constraints post-2023, public backlash) but retains significant institutional agency through alternative mechanisms (holistic review, socioeconomic sorting). Experiences both coordination and extraction simultaneously.
constraint_indexing:constraint_classification(diversity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MINORITY STUDENTS (INSTRUMENTAL FRAMING) (TANGLED ROPE) — Positioned as means to institutional goal (diversity) rather than beneficiaries in their own right. Benefit from admission to selective institutions and presence of peers, but this reading instrumentalizes their educational purpose as achieving diversity for others. Constrained by the framing itself (identity becomes diversity credential). Also coordinate genuine educational goods (peer mentorship, cultural knowledge exchange). Mixed extraction and coordination from the minority student's structural position within this reading.
constraint_indexing:constraint_classification(diversity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DOCTRINAL FRAMEWORK (PITON) — The narrow tailoring doctrine and compelling interest test create substantial performative content: universities must ritualize narrow tailoring through admissions offices, review committees, and legal compliance structures, but the actual constraint has been foreclosed by legal decision (Students for Fair Admissions v. Harvard/UNC, 2023). The framework persists through institutional inertia in admissions psychology and policy design, but the functional authority has collapsed. Theater ratio high because the procedures continue despite legal reversal.
constraint_indexing:constraint_classification(diversity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From civilizational perspective, racial categorization for social benefit may appear as inherent feature of how societies manage diversity — a structural limit of equal protection itself: equal treatment cannot deliver equal outcomes, requiring race-conscious correction. This perspective naturalizes the diversity reading as inevitable corollary of educational equity. However, this is a false summit: the naturalizing framing masks contingent institutional commitments (selective higher education as primary status allocator, diversity as educational good, race as permanent classification scheme).
constraint_indexing:constraint_classification(diversity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(diversity_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(diversity_reading, TR),
    TR >= 0.70.

:- end_tests(diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The diversity reading combines genuine educational coordination (assembling diverse cohorts benefits learning environment) with institutional extraction (universities gain authority to make race-conscious decisions, excluding some applicants on race-conscious grounds). The moderate value reflects that the coordination benefit is structurally real (peer diversity does enhance critical thinking) and that the extraction is constrained by narrow tailoring doctrine (universities must prove educational necessity and lack of workable race-neutral alternatives). Suppression (0.35): Moderate. Barriers to exiting include: legal authorization of the practice (making individual challenge difficult), institutional inertia (diversity commitments embedded in mission statements), and temporal displacement (applicants rejected cannot rewind their timeline, though alternative admission pathways exist). Suppression is not total because alternatives exist (socioeconomic diversity, holistic review). Theater ratio (0.42): Moderate-low. Pre-2023, the narrow tailoring doctrine required genuine institutional deliberation about race-conscious mechanics and alternatives. Post-2023, the doctrine persists as performative structure despite legal foreclosure, raising theater ratio. The midpoint value (0.42) reflects the reading's interval: genuine functional content pre-2023, theatrical maintenance post-2023.
 *
 * PERSPECTIVAL GAP:
 *   The diversity reading demonstrates maximum structural divergence across the observation site. Institutions see rope (coordination benefit). Excluded applicants see snare (extraction with suppressed alternatives). Benefiting students see rope (coordination). Instrumentalized minority students see tangled rope (mixed benefit and identity reduction). The doctrinal system (piton) sees its own theatricality post-2023. The analytical observer (mountain) risks naturalizing the reading as inevitable response to the equality paradox (that formal equality cannot deliver substantive equity). This perspectival spectrum reveals that the diversity reading is not a coherent single constraint but rather an institutional arrangement with structurally distinct effects on different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by structural position. University administrations, as beneficiaries with arbitrage options (can switch to race-neutral mechanisms post-2023 while maintaining diversity through socioeconomic sorting), derive d ≈ 0.15 (beneficiary with exit). Excluded applicants, as victims with constrained exit (cannot undo rejected timeline, cannot prove racial discrimination in particular case despite systemic race-consciousness), derive d ≈ 0.70 (victim with limited exit). All-student beneficiary group, as moderate-power agents with constrained exit (benefit from diversity but cannot opt out of institutional decisions), derive d ≈ 0.55. The engine applies f(d) to compute effective extractiveness chi for each perspective. Institutional beneficiaries perceive low chi (rope); constrained victims perceive high chi (snare/tangled rope). This derivation captures why the same structural arrangement appears as beneficial coordination to one agent and extraction to another.
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading resolves mandatrophy by establishing that extractiveness includes both institutional benefit (authority to make race-conscious decisions) and educational coordination (genuine learning benefit from diverse cohorts). The constraint is tangled rope at institutional level (both functions present), snare at excluded applicant level (extraction with suppression), and rope at benefiting student level (coordination). The reading avoids mandatrophy collapse by declaring both beneficiaries (all students, institutions) and victims (excluded applicants on race-conscious grounds), which permits the engine to compute asymmetric directionality. The core mandatrophy risk: does the diversity reading disguise institutional preference for race-conscious discretion under the cover story of educational coordination? The omega variables address this directly by questioning whether the educational benefits are empirically substantial (omega_2) and whether alternatives exist that achieve diversity without race-consciousness (omega_3).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumental_vs_intrinsic_beneficiary_status,
    'Are minority students in the diversity reading framed as beneficiaries in their own right, or instrumentalized as means to institutional diversity goals that benefit primarily non-minority students?',
    'Textual analysis of Supreme Court opinions, university mission statements, and admissions materials; comparison with remedial reading framing (minority students as remedying historical wrongs)',
    'If instrumental (current reading): extractiveness includes epistemic harm to minority students (identity reduced to diversity credential). If intrinsic: reading shifts toward pure coordination or rope. This distinction defines whether the diversity reading is structurally distinct from remedial reading or merely relabels it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(instrumental_vs_intrinsic_beneficiary_status, conceptual, 'Whether minority students are beneficiaries or instrumental means in the diversity reading').

omega_variable(
    educational_benefit_empirical_reality,
    'Do diverse student bodies actually produce measurable educational benefits (critical thinking, perspective-taking, academic outcomes) that justify race-conscious admission mechanisms?',
    'Longitudinal studies of diverse vs homogeneous cohorts; controlled comparisons of educational outcomes; peer-reviewed meta-analysis of diversity pedagogy literature',
    'If benefits are substantial and measurable: extractiveness drops toward 0.20 (pure rope coordination). If benefits are hypothetical or marginal: extractiveness rises toward 0.60+ (snare/extraction reclassification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educational_benefit_empirical_reality, empirical, 'Whether educational diversity produces measurable learning benefits').

omega_variable(
    compelling_interest_vs_preference_distinction,
    'Is the diversity interest genuinely compelling (irreplaceable to core institutional function) or merely a strong institutional preference that could be served through alternative mechanisms (socioeconomic diversity, holistic review without racial categorization)?',
    'Comparative institutional analysis: universities with strict race-neutral admissions (post-2023) demonstrating diversity outcomes; legal framing shift from compelling interest to rational basis after Students for Fair Admissions',
    'If compelling: reading retains narrow-tailoring logic and moderate extractiveness. If preference: extractiveness jumps to 0.55+ (institutional preference disguised as constitutional necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compelling_interest_vs_preference_distinction, conceptual, 'Whether diversity interest is genuinely compelling or institutional preference').

omega_variable(
    reading_foreclosure_post_2023,
    'Has the diversity reading been functionally foreclosed as valid constitutional doctrine by the Students for Fair Admissions decision (2023), and if so, does this constraint describe pre-2023 legal reality or post-2023 institutional resistance?',
    'Mapping of constraint interval to decision date; analysis of whether universities continue diversity-reading practices post-decision or pivot to alternative mechanisms; legal status determination',
    'If foreclosed: constraint should shift to piton classification across all perspectives (performative maintenance post-legal-reversal). If merely constrained: narrow tailoring remains operative doctrine. This determines whether the story documents a living reading or a functionally dead one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_post_2023, empirical, 'Functional foreclosure status of diversity reading post-2023 Supreme Court decision').

omega_variable(
    kernel_ambiguity_equal_protection_purpose,
    'Does the Equal Protection Clause primarily serve formal equal treatment (colorblind reading) or substantive equal opportunity (which may require race-conscious correction per diversity/remedial readings)? Is the kernel''s purpose univocal or fundamentally contested?',
    'Textual analysis of 14th Amendment; historical intent studies; examination of whether all three readings can coexist within a single coherent interpretation or whether they represent genuinely incommensurable constitutional theories',
    'If purpose is univocal: one reading forecloses the others. If contested: all three coexist as live positions. If incommensurable: readings should be modeled as separate kernels, not readings of a single kernel. This determines whether the committer frame is apt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_ambiguity_equal_protection_purpose, conceptual, 'Whether Equal Protection clause purpose is univocal or fundamentally contested across readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(diversity_theater_t0, diversity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(diversity_theater_t25, diversity_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(diversity_theater_t50, diversity_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(diversity_extractiveness_t0, diversity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(diversity_extractiveness_t25, diversity_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(diversity_extractiveness_t50, diversity_reading, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(diversity_reading, resource_allocation).
narrative_ontology:affects_constraint(diversity_reading, remedial_reading).
narrative_ontology:affects_constraint(diversity_reading, colorblind_reading).

% DUAL FORMULATION NOTE:
% The diversity reading, remedial reading, and colorblind reading form a kernel family reading from the Equal Protection Clause. Each reading instantiates a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and classification profile. The diversity reading (this file) features moderate extractiveness (0.38) and institutional beneficiaries; the remedial reading would feature victim-centered beneficiaries and different ε; the colorblind reading would feature universal scope and high accessibility_collapse. The three constraints are linked via network.affects_constraints to enable analysis of how constraint choice at the kernel level cascades into institutional practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
