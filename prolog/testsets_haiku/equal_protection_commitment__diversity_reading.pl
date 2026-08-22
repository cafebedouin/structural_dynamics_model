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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection: Diversity as Compelling State Interest (Diversity Reading)
 *   domain: constitutional_law/social_policy
 *
 * SUMMARY:
 *   The diversity reading of equal protection permits universities to
 *   consider race as one factor among many in admissions to achieve the
 *   compelling state interest of educational diversity. This is ONE reading
 *   of a contested constitutional kernel (equal protection's proper scope).
 *   The constraint is CLAIMED as rope (genuine coordination: universities
 *   gain discretion to operationalize missions, underrepresented applicants
 *   gain statistical admission advantage, and education benefits from
 *   diversity). The authored metrics reflect extractiveness at the
 *   low-moderate end (0.28) and moderate suppression (0.42) because the
 *   constraint is procedurally permissive rather than substantively
 *   redistributive—it grants discretion rather than mandates outcomes—but it
 *   does suppress individual applicants' ability to mount precise
 *   constitutional claims about their own rejection (holistic review obscures
 *   causation). Theater is moderate (0.38) because admissions discourse is
 *   genuinely about pedagogy and diversity outcomes, but a growing share of
 *   the institutional performance is devoted to defending the discretion
 *   itself against colorblind and remedial challenges, not to implementing
 *   diversity pedagogy. This story is the diversity reading only; the
 *   colorblind and remedial readings are separate constraints (sibling
 *   stories linked via network.affects_constraints).
 *
 * KEY AGENTS:
 *   - universities_mission_discretion: institutional beneficiary; gain discretion for race-conscious admissions within strict scrutiny
 *   - racially_underrepresented_applicants: moderate beneficiary; statistical likelihood of admission increases
 *   - rejected_applicants_individual_claims_obscured: moderate payer; unable to establish race's role in individual rejection
 *   - applicants_penalized_by_race_neutral_diversity_metrics: moderate payer; no constitutional claim to race-neutral process
 *   - courts_reviewing_admissions: agenda_setter (institutional); interpret strict scrutiny boundaries
 *   - colorblind_advocates: excluded opposition (foundational premise foreclosed by this reading)
 *   - remedial_advocates: excluded opposition (reading does not authorize strong remedial measures)
 *   - legislative_bodies: observer; could revise authorization by statute or amendment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.42).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection: Diversity as Compelling State Interest (Diversity Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'd189bb2f-afaa-4801-a4c8-dffde667b5de').
narrative_ontology:cs_kernel_codification('d189bb2f-afaa-4801-a4c8-dffde667b5de', fixed_text).
narrative_ontology:cs_authority_grounding('d189bb2f-afaa-4801-a4c8-dffde667b5de', lineage).
narrative_ontology:cs_interpretation_layer_present('d189bb2f-afaa-4801-a4c8-dffde667b5de').
narrative_ontology:cs_reading_relation('d189bb2f-afaa-4801-a4c8-dffde667b5de', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('d189bb2f-afaa-4801-a4c8-dffde667b5de', equal_protection_commitment__remedial_reading, influences).
narrative_ontology:cs_axiom('d189bb2f-afaa-4801-a4c8-dffde667b5de', foundational, diversity_serves_compelling_state_interest).
narrative_ontology:cs_axiom_status(diversity_serves_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('d189bb2f-afaa-4801-a4c8-dffde667b5de', diversity_serves_compelling_state_interest, instrumental).
narrative_ontology:cs_axiom('d189bb2f-afaa-4801-a4c8-dffde667b5de', foundational, race_consciousness_compatible_with_equal_protection).
narrative_ontology:cs_axiom_status(race_consciousness_compatible_with_equal_protection, holdable).
narrative_ontology:cs_axiom_grounding('d189bb2f-afaa-4801-a4c8-dffde667b5de', race_consciousness_compatible_with_equal_protection, deontological).
narrative_ontology:cs_axiom('d189bb2f-afaa-4801-a4c8-dffde667b5de', secondary, holistic_review_is_rational_admissions_practice).
narrative_ontology:cs_axiom_status(holistic_review_is_rational_admissions_practice, holdable).
narrative_ontology:cs_axiom_grounding('d189bb2f-afaa-4801-a4c8-dffde667b5de', holistic_review_is_rational_admissions_practice, conventional).
narrative_ontology:cs_reference_frame('d189bb2f-afaa-4801-a4c8-dffde667b5de', equal_protection_neutrality_with_remedial_capacity).
narrative_ontology:cs_drift_state('d189bb2f-afaa-4801-a4c8-dffde667b5de', contemporary_political_contestation, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d189bb2f-afaa-4801-a4c8-dffde667b5de', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities_mission_discretion).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, racially_underrepresented_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, rejected_applicants_individual_claims_obscured).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, applicants_penalized_by_race_neutral_diversity_metrics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains explicit judicial authorization to consider race as one factor in holistic admissions review. This reading permits them to operationalize educational-diversity missions and defend admissions decisions against colorblind constitutional challenge. They set the institutional policy within the constraint's permission boundary; the constraint shields their discretionary authority from stricter scrutiny.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities_mission_discretion, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, universities_mission_discretion, agenda_setter).

% Benefit structurally from the reading's permission that race-conscious admissions improve their statistical likelihood of admission to competitive universities. The constraint treats their presence in a diverse student body as a compelling educational good, not as a departure from neutral merit.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, racially_underrepresented_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Bear the extractive cost: when race is considered in holistic review, rejected applicants cannot definitively establish whether race played a role in their individual case (the 'holistic review' architecture obscures causation). They are unable to mount precise constitutional claims about their own treatment; the individual claim is structurally suppressed by the permissible opacity of multifactorial decisions.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, rejected_applicants_individual_claims_obscured, payer,
    moderate, biographical, constrained, national).

% May experience admission disadvantage when universities operationalize diversity goals through race-conscious preference, even if the policy is formally 'one factor among many.' They have no constitutionally protected individual claim to a race-neutral admissions process under this reading (colorblind doctrine is foreclosed); their remedy is collective political action to overturn the reading, not individual legal redress.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, applicants_penalized_by_race_neutral_diversity_metrics, payer,
    moderate, biographical, constrained, national).

% Enforce the constraint by applying strict scrutiny to race-conscious admissions while accepting that the compelling-interest test can be satisfied by educational-diversity pedagogy. They adjudicate disputes but do not set university policy; they interpret the constitutional boundaries within which universities operate.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, courts_reviewing_admissions, agenda_setter,
    institutional, generational, analytical, national).

% Would challenge any race-conscious admissions as unconstitutional; are structurally excluded from the diversity reading's normative framework (their foundational premise—that the Constitution forbids all state racial classification—is foreclosed by this reading). They operate outside the constraint as organized opposition.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_advocates, excluded,
    organized, generational, trapped, national).

% Would argue that equal protection requires active dismantling of caste-system subordination, not merely permitting diversity-motivated inclusion. They contest the diversity reading from the left, asserting it does not go far enough to remedy structural discrimination; the diversity reading does not authorize the stronger remedial measures they advocate.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, remedial_advocates, excluded,
    organized, generational, trapped, national).

% Could enact statutory restrictions on race-conscious admissions if they mobilize political will; they observe the constraint and can revise it by constitutional amendment or, within limits, by federal statute. They are not parties to the admissions decision but can alter the authorization framework.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, legislative_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, universities_mission_discretion).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how universities can pursue institutionally coherent educational missions (including diversity goals) without being trapped by rigid colorblind rules that treat all race-conscious decisions as inherently irrational. Provides a legal framework within which mission-driven admissions can be defended as rational, not paternalistic.
% TRANSFER_FUNCTION: Moves institutional discretion FROM rigid colorblindness (which constrained admissions to proxy metrics) TO holistic, race-conscious review that universities can operationalize. Also moves statistical advantage toward racially underrepresented applicants and moves (diffusely) the ability to mount individual constitutional challenges away from rejected applicants whose rejection occurs within a permissibly multifactorial decision.
% ABSENT_VOICES: Applicants penalized by the race-conscious discretion have no authorized voice—they cannot mount a constitutional challenge on grounds that race played a role (colorblind standing is foreclosed). Colorblind originalists and remedial-justice advocates are excluded from the diversity reading's normative framework entirely; they contest from outside.
% DISAPPEARANCE_RATIONALE: If this reading vanished and colorblind doctrine took its place, universities would lose discretion to consider race in admissions; educational-diversity initiatives would shift to race-neutral proxies (socioeconomic status, geography, identity-linked experience); the statistical outcome for underrepresented racial groups would change; and universities would operationalize different mission statements. The reading's absence is not a neutral reversion—it is a active legal reclassification that reshapes what admissions policies are constitutionally permissible.
% FOUNDING_PROBLEM: Equal protection doctrine needed a coherent framework for reconciling the constitutional commitment to equal protection with the educational and civic case that student-body diversity improves learning outcomes and serves the state's interest in developing a diverse, educated citizenry. The colorblind reading made this reconciliation impossible; the diversity reading attempts to hold both commitments.
% FOUNDING_PROBLEM_CORROBORATION: Universities and education researchers attest that student-body diversity improves educational outcomes (amicus briefs, commissioned studies). Civil-rights organizations (outside the beneficiary set of universities) attest that colorblind doctrine obscures ongoing racial inequality in educational access. Colorblind advocates counter that the founding problem is illusory—equal protection simply forbids racial classification, period. The contest is live and unresolved by external corroboration.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low-moderate (0.28 at interval end) because the constraint operates procedurally—it permits universities to act, does not mandate substantive outcomes, and the primary extraction is the suppression of individual applicants' ability to mount precise constitutional claims, not wealth or status transfer. Suppression is moderate (0.42) because rejected applicants face a real structural barrier (holistic-review opacity prevents them from establishing causation), but the barrier is architecturally embedded in permissible discretion, not imposed through coercive enforcement machinery. Theater is moderate-stable (~0.38 across interval) because admissions discourse is genuinely about educational mission and diversity pedagogy, but mounting institutional performance goes into defending the discretion itself against constitutional challenge—that defensive theater is intrinsic to the constraint's operation, not anomalous. The measurement series shows extractiveness and suppression rising slightly at the interval's opening (as the reading becomes more established and universities operationalize it), then stabilizing as legal and political equilibrium settles. Accessibility collapse is moderate (0.65): alternatives (colorblind admissions, race-neutral proxies, remedial-justice framing) remain live and contested; they are not foreclosed by the diversity reading's logic, only excluded from its normative framework. Resistance is high (0.72): colorblind advocates mount sustained constitutional and political challenge; remedial advocates contest from the opposite direction; rejected applicants face structural incentive to litigate. The diversity reading does not suppress resistance, only redirects its channels (individual claims are foreclosed; collective political action remains open).
 *
 * PERSPECTIVAL GAP:
 *   Universities experience the constraint as enabling (discretion, mission-driven authority) and protective (judicial approval of race-conscious review). Underrepresented applicants experience it as substantively beneficial (improved admission chances). Rejected applicants experience it as extractive and suppressive (obscured causation in their rejection, no individual constitutional claim). The diversity reading produces this seat divergence by design: it grants discretion to universities while suppressing individual redress for applicants disadvantaged by that discretion. The engine computes per-seat type from the structural data—the university seat should compute closer to 'rope' or 'mountain'; the rejected-applicant seat should compute closer to 'snare' or 'tangled_rope' (moderate extraction with substantial suppression, no exit from the holistic-review process once race is legally permitted as one factor).
 *
 * DIRECTIONALITY LOGIC:
 *   Universities (institutional power, generational horizon, constrained exit to alternative constitutional frameworks) sit as clear beneficiaries (d near 0.0–0.15). Underrepresented applicants (moderate power, biographical horizon, constrained exit) are structural beneficiaries of the diversity permission (d near 0.1–0.2). Rejected applicants whose individual claims are obscured (moderate power, biographical horizon, trapped/constrained exit—they cannot choose to avoid the admission process) sit as clear targets of the suppression mechanism (d near 0.8–0.95). Applicants penalized by race-conscious diversity metrics (moderate power, biographical horizon, constrained exit—they also cannot avoid the process) sit as targets (d near 0.7–0.85). Colorblind and remedial advocates (organized power, generational horizon, analytical exit) are excluded from the constraint's authorization framework entirely—they do not sit within the diversity reading's structural space, only contest it from outside. This directionality structure is computed by the engine from the beneficiary/victim declarations and exit modulation; no override is required.
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading avoids conflating coordination with extraction by explicitly grounding the universities' discretion in a purported educational mission (diversity improves learning; diverse cohorts serve civic interests). The constraint is not merely a permission structure for universities to extract applicant-selection labor; it is justified as enabling a public good (educated diverse citizenry). However, mandatrophy surfaces here through the measurement series: as extractiveness stabilizes around 0.28 and theater rises, institutional performance devoted to defending the reading against constitutional and political challenge grows, while the underlying diversity-pedagogy function remains contested (some universities operationalize genuine diversity missions; others use the permission as cover for class-based or legacy-based selectivity). The founding-problem status is CONTESTED precisely because mandatrophy is live: colorblind advocates argue the founding problem (reconciling equal protection with diversity) is illusory and the reading is post-hoc justification; remedial advocates argue the founding problem is real but the diversity reading does not solve it—it manages the contradiction without resolving it. This reading permits the constraint's persistence without settling whether the constraint serves its stated purpose or merely legitimates it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    holist_review_causation_suppression,
    'Is the opacity of holistic-review decision-making a necessary feature of the diversity commitment, or a contingent institutional practice that could be reformed?',
    'Empirical pilot of transparent race-conscious admissions: if universities published explicit race-conscious criteria and rejected applicants'' claims about their own race-based disadvantage did NOT rise, the opacity is not necessary. If such transparency provoked legal challenges that courts rejected on equal-protection grounds, the opacity would be revealed as contingent on institutional choice, not constitutional requirement.',
    'If opacity is contingent, the diversity reading permits universities to operate more transparently, reducing the suppression score; if opacity is necessary to the reading''s operation (because explicit racial preference triggers stricter scrutiny), the suppression is structurally embedded and the extraction floor rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holist_review_causation_suppression, empirical, 'Whether holistic-review obscuration is necessary to the diversity reading or contingent on institutional practice.').

omega_variable(
    diversity_outcome_vs_permission_structure,
    'Do the universities that invoke the diversity reading actually operationalize diversity pedagogy and achieve meaningful increases in underrepresented-group enrollment, or does the reading primarily function as permission to maintain historical selectivity patterns under new rhetoric?',
    'Comparative analysis of admissions outcomes before and after adoption of explicit diversity-reading frameworks; interview evidence from admissions offices about whether diversity is an operative consideration or a legal compliance cover story.',
    'If universities genuinely operationalize diversity, the coordination function is real and the ε is justified at 0.28. If the reading is primarily rhetorical cover for unchanged selectivity, the constraint slides toward ''piton'' (inertial, performative, low actual coordination benefit) and ε should rise toward 0.40–0.50.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_outcome_vs_permission_structure, empirical, 'Whether the diversity reading''s stated coordination function matches actual institutional practice.').

omega_variable(
    kernel_reading_underdetermination,
    'Is the diversity reading a coherent instantiation of equal protection principle, or does it represent an unprincipled middle ground that holds incompatible commitments (colorblindness and race-consciousness)?',
    'Sustained philosophical and doctrinal analysis of whether the reading''s distinction between individual colorblind rights and collective state-interest permissibility is logically coherent, or whether it smuggles in equivocation about what ''equal'' means.',
    'If the reading is coherent, it is a legitimate node in the kernel''s solution space; if incoherent, it may be unstable (vulnerable to foreclosure by one of the sibling readings'' more consistent premises). This would affect the confidence in the reading''s persistence and the measured accessibility_collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the diversity reading''s logical structure is coherent or represents unprincipled compromise between colorblind and remedial commitments.').

omega_variable(
    remedial_vs_diversity_boundary,
    'Where does the diversity reading''s permission for race-conscious admissions END and a stronger remedial mandate BEGIN? Is there a principled distinction, or is the boundary drawn post-hoc to exclude stronger remedial measures?',
    'Doctrinal analysis of how courts have drawn the line between permissible diversity consideration and impermissible quota or set-aside; examination of whether the line tracks a coherent principle or reflects political contingency.',
    'If a principled boundary exists, the diversity reading is stable relative to the remedial reading (influences but does not foreclose). If the boundary is drawn ad-hoc, the remedial reading may eventually foreclose the diversity reading by establishing that equal protection REQUIRES stronger measures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_diversity_boundary, conceptual, 'Whether the diversity reading''s boundary with remedial justice is principled or politically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(equa_tr_t4, equal_protection_commitment__diversity_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(equa_tr_t8, equal_protection_commitment__diversity_reading, theater_ratio, 8, 0.37).
narrative_ontology:measurement(equa_tr_t12, equal_protection_commitment__diversity_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(equa_tr_t16, equal_protection_commitment__diversity_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__diversity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(equa_tr_t25, equal_protection_commitment__diversity_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(equa_be_t4, equal_protection_commitment__diversity_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(equa_be_t8, equal_protection_commitment__diversity_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(equa_be_t12, equal_protection_commitment__diversity_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(equa_be_t16, equal_protection_commitment__diversity_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__diversity_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement(equa_be_t25, equal_protection_commitment__diversity_reading, base_extractiveness, 25, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__diversity_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(equa_su_t4, equal_protection_commitment__diversity_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(equa_su_t8, equal_protection_commitment__diversity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(equa_su_t12, equal_protection_commitment__diversity_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(equa_su_t16, equal_protection_commitment__diversity_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__diversity_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(equa_su_t25, equal_protection_commitment__diversity_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel decomposes into three structurally distinct constraint stories: (1) diversity_reading (this story)—permits race-conscious admissions for educational diversity; low-moderate ε because procedural rather than substantive. (2) colorblind_reading—forbids any state use of racial classification; lower ε because constraint is permissive-void rather than discretion-granting; higher suppression of race-conscious voices. (3) remedial_reading—requires active dismantling of caste subordination; higher ε because substantive redistribution is authorized; beneficiary set is those subject to subordination, not institutional discretion-holders. Each reading instantiates a different constraint with different beneficiaries, victims, and extracted values. They coexist as live positions held by different parties (constitutional scholars, judges, advocates); neither forecloses the others from within a single framework, though each excludes the others' logic. The ε-invariance principle (DP-001) requires decomposition: a single constraint story cannot coherently author multiple readings with materially different ε values. Network links establish the kernel relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
