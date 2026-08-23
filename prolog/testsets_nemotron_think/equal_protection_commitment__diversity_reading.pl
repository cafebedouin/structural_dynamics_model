% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Diversity Rationale for Race-Conscious Admissions
 *   domain: constitutional_law/education_policy
 *
 * SUMMARY:
 *   The diversity reading of the Equal Protection Clause, articulated in
 *   Justice Powell's Bakke opinion (1978) and affirmed in Grutter v.
 *   Bollinger (2003), holds that universities may consider race as one factor
 *   among many in holistic admissions review to achieve the educational
 *   benefits of a diverse student body. This reading operates as a procedural
 *   permission rather than a substantive mandate: it authorizes but does not
 *   require race-conscious admissions. Universities gain discretionary
 *   authority to pursue mission-driven diversity goals (beneficiaries), while
 *   all applicants enter a holistic review process whose racial weighting is
 *   opaque and whose individual impacts are obscured (victims/payers). The
 *   constraint's extractiveness is low-moderate (ε ≈ 0.25) because it is
 *   procedural — it creates a permission structure whose extraction occurs
 *   only when universities choose to exercise it, and only against applicants
 *   who would have been admitted absent race-consciousness. The constraint
 *   required active judicial enforcement to maintain the permission against
 *   colorblind challenges, and universities actively enforced it through
 *   admissions policies. Theater ratio rose over time as diversity rhetoric
 *   expanded while the mechanical linkage between race-conscious admissions
 *   and educational outcomes weakened. The constraint effectively ended as
 *   binding law with Students for Fair Admissions v. Harvard/UNC (2023),
 *   though the diversity reading persists as a normative position.
 *
 * KEY AGENTS:
 *   - universities: Primary agenda_setter and beneficiary (institutional/biographical/arbitrage/global) — set admissions policy within the constraint, gain discretionary authority for mission-driven diversity
 *   - all_applicants: Primary payer (organized/biographical/constrained/national) — bear costs of opaque holistic review; some advantaged, some disadvantaged by race-conscious weighting
 *   - underrepresented_applicants: Beneficiary (moderate/biographical/constrained/national) — benefit from race-conscious consideration in holistic review
 *   - overrepresented_applicants: Payer (moderate/biographical/constrained/national) — face higher effective barriers due to race-conscious weighting
 *   - courts: Observer/agenda_setter (institutional/generational/analytical/national) — enforce the permission structure, define its boundaries, ultimately foreclose it in SFFA
 *   - colorblind_advocates: Excluded (organized/biographical/trapped/national) — would argue race-consciousness is categorically forbidden; structurally excluded from the diversity framework's internal logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.25).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.3).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Diversity Rationale for Race-Conscious Admissions").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/education_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '5e183d54-702a-4694-afc8-b33deb6ddb69').
narrative_ontology:cs_kernel_codification('5e183d54-702a-4694-afc8-b33deb6ddb69', formalized).
narrative_ontology:cs_authority_grounding('5e183d54-702a-4694-afc8-b33deb6ddb69', lineage).
narrative_ontology:cs_interpretation_layer_present('5e183d54-702a-4694-afc8-b33deb6ddb69').
narrative_ontology:cs_reading_relation('5e183d54-702a-4694-afc8-b33deb6ddb69', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('5e183d54-702a-4694-afc8-b33deb6ddb69', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('5e183d54-702a-4694-afc8-b33deb6ddb69', foundational, diversity_compelling_interest).
narrative_ontology:cs_axiom_status(diversity_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('5e183d54-702a-4694-afc8-b33deb6ddb69', diversity_compelling_interest, empirically_contingent).
narrative_ontology:cs_axiom('5e183d54-702a-4694-afc8-b33deb6ddb69', foundational, holistic_review_permissible).
narrative_ontology:cs_axiom_status(holistic_review_permissible, holdable).
narrative_ontology:cs_axiom_grounding('5e183d54-702a-4694-afc8-b33deb6ddb69', holistic_review_permissible, conventional).
narrative_ontology:cs_reference_frame('5e183d54-702a-4694-afc8-b33deb6ddb69', bakke_powell_opinion).
narrative_ontology:cs_drift_state('5e183d54-702a-4694-afc8-b33deb6ddb69', post_students_for_fair_admissions, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5e183d54-702a-4694-afc8-b33deb6ddb69', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, all_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, overrepresented_applicants).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, diversity_compelling_interest).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, holistic_review_permissible).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, institutional_academic_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set admissions policies within the diversity permission; gain discretionary authority to pursue mission-driven diversity goals; collect tuition, reputation, and mission-fulfillment benefits from diverse student bodies; can choose not to exercise the permission (but face institutional pressure to do so); exit is arbitrage-grade — they could adopt race-neutral policies but would lose status/competitive position
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, universities, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_commitment__diversity_reading, universities, beneficiary).

% Enter a national holistic review system whose racial weighting is opaque; bear costs of uncertainty, inability to challenge adverse decisions on racial grounds, and potential displacement; exit is constrained — can apply to universities in states with bans, or forego selective admissions, but cannot exit the national market for elite credentials
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, all_applicants, payer,
    organized, biographical, constrained, national).

% Receive a race-conscious boost in holistic review that increases admission odds at selective institutions; benefit from the diversity rationale's permission structure; still subject to the same opaque process; exit is constrained like all applicants but with a subsidy
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, underrepresented_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Face a race-conscious penalty in holistic review that decreases admission odds at selective institutions relative to a race-neutral baseline; bear the marginal cost of the diversity rationale's permission; cannot identify whether race was determinative in their rejection; exit is constrained — same national market, same opacity
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, overrepresented_applicants, payer,
    moderate, biographical, constrained, national).

% Define, refine, and ultimately foreclose the diversity permission through a lineage of cases (Bakke → Grutter → Fisher → SFFA); enforce the constraint's boundaries (narrow tailoring, no quotas, holistic review requirement); their exit is analytical — they interpret the constraint but are not subject to it
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, courts, observer,
    institutional, generational, analytical, national).

% Argue that the Equal Protection Clause categorically forbids state racial classification; their objection is structurally excluded from the diversity reading's internal logic (which treats colorblindness as a competing reading, not a valid objection within the framework); they cannot exit the constitutional conversation but are trapped outside the diversity framework's legitimacy
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, colorblind_advocates, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, universities).
narrative_ontology:fixing_cost_class(equal_protection_commitment__diversity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the assembly of educationally diverse student bodies at selective universities without rigid quotas, by permitting race to be one factor in a holistic, individualized review that aims to capture the educational benefits of diversity for all students.
% TRANSFER_FUNCTION: Moves admission opportunities at selective universities from applicants who would be admitted under race-neutral criteria to applicants who receive a race-conscious boost in holistic review, redistributing scarce elite credentials along racial lines to serve the institutional goal of diversity.
% ABSENT_VOICES: Proponents of the colorblind reading (who argue any racial classification is categorically forbidden) and the remedial reading (who argue diversity is a weak substitute for addressing subordination) are structurally excluded from the diversity framework's internal logic. Applicants who would prefer transparent, criteria-based admissions over opaque holistic review have no voice in the constraint's design. The diversity reading treats these positions as external challenges, not internal objections.
% DISAPPEARANCE_RATIONALE: If the diversity permission vanished overnight (as it effectively did with SFFA), university admissions policies would immediately shift to race-neutral alternatives (class-based affirmative action, percentage plans, expanded outreach), admission demographics would change at selective institutions, the holistic review architecture would be reconfigured or dismantled, and the institutional infrastructure built around diversity justification (offices, metrics, reporting) would lose its legal foundation.
% FOUNDING_PROBLEM: After Bakke struck down racial quotas (1978), universities needed a constitutionally permissible way to continue pursuing racial diversity. The diversity reading solved this by reframing race-conscious admissions as serving a compelling educational interest (diverse viewpoints) rather than remedial justice, permitting individualized holistic review instead of mechanical quotas.
% FOUNDING_PROBLEM_CORROBORATION: The diversity reading's proponents (Grutter majority, university amicus briefs, social science research on diversity benefits) attest the problem persists — diversity benefits remain empirically supported and race-neutral alternatives insufficient. Colorblind advocates (SFFA majority, state ban campaigns) attest the problem is solved or was never valid — race-neutral alternatives work and racial classification is categorically wrong. Remedial advocates (Grutter dissent, critical race theorists) attest the wrong problem was addressed — diversity is a weak substitute for dismantling subordination. No consensus outside the beneficiary set (universities).
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low-moderate (0.25) because the constraint is a permission, not a mandate — extraction occurs only when universities exercise the permission and only against the marginal applicants displaced by race-conscious weighting. Suppression is moderate (0.30) because applicants cannot easily challenge holistic review's racial weighting (opacity) and cannot exit the national admissions market, but alternatives (class-based, percentage plans) exist and were litigated. Theater ratio rose from 0.10 to 0.45 as diversity became an institutional imperative decoupled from measurable educational outcomes — universities performed diversity work (statements, offices, metrics) while the mechanical link to the compelling interest attenuated. Accessibility collapse is moderate (0.45) because class-based and percentage-plan alternatives persisted but were treated as insufficient by the diversity reading's proponents. Resistance is high (0.65) reflecting sustained colorblind litigation and state bans. The claimed type is tangled_rope: genuine coordination function (managing diversity in complex institutions) plus asymmetric extraction (some applicants bear costs for collective diversity benefits) plus active enforcement (judicial review, university compliance).
 *
 * PERSPECTIVAL GAP:
 *   From the university seat (agenda_setter/beneficiary), the constraint is coordination: it solves the problem of how to assemble educationally diverse classes without rigid quotas. From the overrepresented applicant seat (payer), the same structure is extraction: they bear a race-based penalty for a collective benefit they do not directly receive. From the underrepresented applicant seat (beneficiary), it is coordination-with-subsidy: they receive a boost that the constraint frames as serving everyone's educational interest. From the court seat (observer), it evolved from tolerated permission (Bakke) to endorsed compelling interest (Grutter) to foreclosed rationale (SFFA). The engine computes these divergences from the structural data — the authored claim (tangled_rope) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Universities are structural beneficiaries (d ≈ 0.15): they collect discretionary authority, control the admissions process, and face no direct cost from the permission. All applicants are structural targets (d ≈ 0.65): they enter a process whose racial weighting they cannot see, challenge, or avoid without exiting higher education nationally. Underrepresented applicants have slightly lower d (≈ 0.45) because they receive a subsidy, but they remain in the holistic review system. Overrepresented applicants have higher d (≈ 0.80) because they bear the full marginal cost. Courts are analytical (d = 0.5 by definition). Colorblind advocates are excluded — their exit is trapped because the constraint's internal logic does not admit their objection as valid.
 *
 * MANDATROPHY ANALYSIS:
 *   The diversity reading avoided mandatrophy for decades by tethering its justification to a measurable educational outcome (diversity benefits) that social science was claimed to support. As the evidence base weakened and the Court's composition shifted, the constraint's founding problem (how to achieve diversity without quotas) became contested: universities argue the problem persists; colorblind advocates argue the problem was solved or never justified race-consciousness; remedial advocates argue the wrong problem was addressed. The constraint did not formally sunset (Grutter's '25 years' expectation was aspirational, not structural), and its persistence after the empirical justification attenuated is exactly the mandatrophy pattern — but the Court's foreclosure in SFFA resolved it by external force rather than internal recognition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the diversity reading a structurally distinct constraint from the remedial and colorblind readings of the equal protection commitment, or do they represent observable-dependent classifications of the same constraint?',
    'Decompose the three readings into separate constraint stories with independent ε values, beneficiary/victim structures, and temporal trajectories. If ε differs materially (diversity: 0.25, remedial: 0.45, colorblind: 0.05), they are distinct constraints linked by network.affects_constraints.',
    'If distinct, each reading gets its own classification (diversity = tangled_rope, remedial = tangled_rope with higher ε, colorblind = mountain/rope). If merged, the ε-invariance principle is violated and classification becomes measurement-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the three equal protection readings are one constraint with three measures or three constraints sharing a kernel').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the diversity rationale''s coordination function (managing educational diversity) genuinely require race-conscious holistic review, or is race-consciousness extractive overhead that could be replaced by class-based or percentage-plan alternatives?',
    'Natural experiment evidence from states that banned race-conscious admissions (CA, MI, WA) and adopted alternatives: if diversity outcomes persist without race-conscious review, the race-conscious component is extractive overhead, not coordination necessity.',
    'If replaceable, the constraint''s extraction is higher than its coordination function justifies, pushing toward snare. If necessary, tangled_rope classification holds with lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether race-consciousness is necessary for the coordination function or extractive overhead').

omega_variable(
    holistic_review_opacity,
    'Does holistic review''s obscuring of individual racial weighting constitute suppression (preventing applicants from challenging adverse decisions) or necessary administrative flexibility?',
    'Litigation discovery revealing how heavily race is weighted in practice vs. stated ''one factor among many''; correlation between race and admission odds controlling for other factors.',
    'If holistic review systematically weights race heavily while claiming minimal consideration, suppression is higher and the constraint operates as a snare for disfavored applicants. If genuinely minimal, coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holistic_review_opacity, empirical, 'Whether holistic review''s opacity is functional or suppressive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_diversity_tr_t1978, equal_protection_commitment__diversity_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(ep_diversity_tr_t1995, equal_protection_commitment__diversity_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(ep_diversity_tr_t2003, equal_protection_commitment__diversity_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(ep_diversity_tr_t2013, equal_protection_commitment__diversity_reading, theater_ratio, 2013, 0.42).
narrative_ontology:measurement(ep_diversity_tr_t2016, equal_protection_commitment__diversity_reading, theater_ratio, 2016, 0.45).
narrative_ontology:measurement(ep_diversity_tr_t2023, equal_protection_commitment__diversity_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(ep_diversity_be_t1978, equal_protection_commitment__diversity_reading, base_extractiveness, 1978, 0.15).
narrative_ontology:measurement(ep_diversity_be_t1995, equal_protection_commitment__diversity_reading, base_extractiveness, 1995, 0.2).
narrative_ontology:measurement(ep_diversity_be_t2003, equal_protection_commitment__diversity_reading, base_extractiveness, 2003, 0.28).
narrative_ontology:measurement(ep_diversity_be_t2013, equal_protection_commitment__diversity_reading, base_extractiveness, 2013, 0.3).
narrative_ontology:measurement(ep_diversity_be_t2016, equal_protection_commitment__diversity_reading, base_extractiveness, 2016, 0.32).
narrative_ontology:measurement(ep_diversity_be_t2023, equal_protection_commitment__diversity_reading, base_extractiveness, 2023, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(ep_diversity_su_t1978, equal_protection_commitment__diversity_reading, suppression_requirement, 1978, 0.2).
narrative_ontology:measurement(ep_diversity_su_t1995, equal_protection_commitment__diversity_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(ep_diversity_su_t2003, equal_protection_commitment__diversity_reading, suppression_requirement, 2003, 0.3).
narrative_ontology:measurement(ep_diversity_su_t2013, equal_protection_commitment__diversity_reading, suppression_requirement, 2013, 0.35).
narrative_ontology:measurement(ep_diversity_su_t2016, equal_protection_commitment__diversity_reading, suppression_requirement, 2016, 0.38).
narrative_ontology:measurement(ep_diversity_su_t2023, equal_protection_commitment__diversity_reading, suppression_requirement, 2023, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__diversity_reading, 0.15).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, university_admissions_holistic_review).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, state_affirmative_action_bans).

% DUAL FORMULATION NOTE:
% This constraint is one member of the equal_protection_commitment constraint family (kernel_id: equal_protection_commitment). The three readings — diversity_reading, remedial_reading, colorblind_reading — are structurally distinct constraints with different ε values, beneficiary/victim sets, and temporal trajectories, linked by shared kernel text and institutional history. The diversity reading's permission structure enables the remedial reading's more aggressive measures (by establishing that race-consciousness is not categorically forbidden) and is the primary target of the colorblind reading's foreclosure project. Decomposed per ε-invariance: the label 'affirmative action' conflates three constraints; this story isolates the diversity rationale's specific permission structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, institutional, 0.15).
constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, organized, 0.65).
constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
