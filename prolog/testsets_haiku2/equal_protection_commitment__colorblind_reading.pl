% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__colorblind_reading, []).

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
 *   constraint_id: equal_protection_commitment__colorblind_reading
 *   human_readable: Constitutional Color-Blindness Requirement (Harlan Dissent Reading)
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   This constraint instantiates the colorblind reading of the 14th
 *   Amendment's equal protection clause: the constitutional claim that the
 *   state must not classify individuals by race, regardless of purpose,
 *   because the Constitution is color-blind. This reading is distinguished
 *   from two sibling readings of the same kernel: the diversity reading
 *   (which permits race as one factor among many toward compelling state
 *   interests like educational diversity) and the remedial reading (which
 *   permits race-conscious measures to dismantle racial subordination and
 *   caste). The colorblind reading reframes applicants denied opportunity
 *   through race-conscious admissions as victims of unconstitutional
 *   classification, and treats institutional administrators of such programs
 *   as constitutional violators. The ε-invariance principle governs: this
 *   constraint has a stable, moderate extractiveness (0.42 at interval end)
 *   because the classification itself is the designated harm, independent of
 *   effect or purpose. The constraint coordinates a boundary-marking function
 *   (race-sorting forbidden) while extracting from beneficiaries of
 *   race-conscious programs, making it a tangled rope from the reading's own
 *   epistemic vantage point.
 *
 * KEY AGENTS:
 *   - applicants_subject_to_race_conscious_classifications (victim, moderate power, constrained exit)
 *   - applicants_not_targeted_by_race_conscious_policy (beneficiary, moderate power, constrained exit)
 *   - educational_institutions_administering_race_conscious_admissions (agenda-setter, institutional power, constrained exit)
 *   - legislative_bodies_authorizing_race_conscious_programs (agenda-setter, institutional power, mobile exit)
 *   - courts_applying_equal_protection (agenda-setter/enforcer, institutional power, analytical exit)
 *   - communities_historically_subject_to_racial_subordination (excluded, organized power, trapped exit)
 *   - constitutional_scholars_remedial_tradition (observer, analytical power)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.38).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Constitutional Color-Blindness Requirement (Harlan Dissent Reading)").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '33f58cb9-3f7d-426b-98c5-e6c1496afa02').
narrative_ontology:cs_kernel_codification('33f58cb9-3f7d-426b-98c5-e6c1496afa02', fixed_text).
narrative_ontology:cs_authority_grounding('33f58cb9-3f7d-426b-98c5-e6c1496afa02', lineage).
narrative_ontology:cs_interpretation_layer_present('33f58cb9-3f7d-426b-98c5-e6c1496afa02').
narrative_ontology:cs_reading_relation('33f58cb9-3f7d-426b-98c5-e6c1496afa02', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_reading_relation('33f58cb9-3f7d-426b-98c5-e6c1496afa02', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('33f58cb9-3f7d-426b-98c5-e6c1496afa02', foundational, constitutional_race_neutrality_absolute).
narrative_ontology:cs_axiom_status(constitutional_race_neutrality_absolute, holdable).
narrative_ontology:cs_axiom_grounding('33f58cb9-3f7d-426b-98c5-e6c1496afa02', constitutional_race_neutrality_absolute, deontological).
narrative_ontology:cs_axiom('33f58cb9-3f7d-426b-98c5-e6c1496afa02', foundational, classification_harm_precedes_effect).
narrative_ontology:cs_axiom_status(classification_harm_precedes_effect, holdable).
narrative_ontology:cs_axiom_grounding('33f58cb9-3f7d-426b-98c5-e6c1496afa02', classification_harm_precedes_effect, deontological).
narrative_ontology:cs_reference_frame('33f58cb9-3f7d-426b-98c5-e6c1496afa02', color_blind_constitutionalism_ideal).
narrative_ontology:cs_drift_state('33f58cb9-3f7d-426b-98c5-e6c1496afa02', contemporary_strict_scrutiny_doctrine, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('33f58cb9-3f7d-426b-98c5-e6c1496afa02', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, applicants_not_targeted_by_race_conscious_policy).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, applicants_subject_to_race_conscious_classifications).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applicants (often Asian-Americans and white applicants) denied admission or other competitive allocations because race-conscious programs explicitly consider their racial/ethnic status as a factor reducing their competitive standing. Their remedial pathway is litigation challenging the classification; exiting the jurisdiction does not change the rule.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, applicants_subject_to_race_conscious_classifications, payer,
    moderate, biographical, constrained, national).

% Applicants who benefit from race-conscious programs receive admission or allocation advantages. Under the colorblind reading, they are incidental beneficiaries of unconstitutional classification rather than intended participants in remediation; the reading reframes their gain as a byproduct of an impermissible state act.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, applicants_not_targeted_by_race_conscious_policy, beneficiary,
    moderate, biographical, constrained, national).

% Universities and competitive-entry institutions design and defend admissions policies that consider race as one factor. They justify race-conscious admissions as advancing diversity and addressing historical inequities. The colorblind reading treats their classifications as constitutional violations regardless of intent.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, educational_institutions_administering_race_conscious_admissions, agenda_setter,
    institutional, generational, constrained, national).

% Federal, state, and local legislatures enact or authorize race-conscious contracting, hiring, and educational programs. They can revise or abandon the programs if ruled unconstitutional; their exit is political feasibility and constituent pressure.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, legislative_bodies_authorizing_race_conscious_programs, agenda_setter,
    institutional, generational, mobile, national).

% Judicial system interprets the 14th Amendment and applies equal protection doctrine. The colorblind reading compels courts to apply strict scrutiny to all racial classifications, invalidating those lacking an extremely narrow tailoring to a compelling state interest. Courts are the enforcement mechanism that renders the colorblind constraint operative.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, courts_applying_equal_protection, agenda_setter,
    institutional, generational, analytical, national).

% Black Americans and other historically subordinated groups whose ancestors experienced legal racial caste are structurally excluded from the colorblind reading's internal logic. The reading does not acknowledge that racial subordination is an ongoing structural fact requiring address; it frames any explicit racial reference as the harm itself, excluding subordination analysis from constitutional deliberation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, communities_historically_subject_to_racial_subordination, excluded,
    organized, generational, trapped, national).

% Scholars, judges, and advocates who read equal protection as requiring address to caste maintenance and historical subordination rather than race-neutrality observe and contest the colorblind reading's constitutional interpretation. They produce alternative framings of the same clause.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, constitutional_scholars_remedial_tradition, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__colorblind_reading, applicants_not_targeted_by_race_conscious_policy).
narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, uniform legal standard for state action on race: the Constitution forbids the state from classifying individuals by race or ethnicity, ensuring formal equality before law.
% TRANSFER_FUNCTION: Removes competitive advantage from applicants benefiting from race-conscious admissions and transfers allocation slots to applicants not subject to racial classification. Transfers authority from admissions officials to courts empowered to invalidate race-conscious programs.
% ABSENT_VOICES: Communities historically subordinated by legal racial caste—particularly Black Americans, Native Americans, and other groups whose constitutional status was defined through explicit racial classification by the state—are structurally excluded from the colorblind reading's deliberation. Their perspective (that constitutional harm is perpetuation of subordination, requiring race-conscious remedy) is not present in institutions applying the constraint. The remedial reading represents their interests; the colorblind reading does not internally acknowledge that voice as relevant to its constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the colorblind constraint disappeared—if courts permitted explicit race-conscious state programs without strict scrutiny—competitive admissions, contracting, hiring, and political representation would reorganize around race-conscious factors across sectors. Institutions would openly weight race in allocation. The constraint's enforcement is what forces programs into their current shape (using proxies, class-based alternatives, or discontinuing programs). Removing it would reshape institutional access and legitimacy.
% FOUNDING_PROBLEM: The founding problem (from the colorblind reading's perspective) is whether the Constitution permits the state to sort individuals by racial classification. The claim is that the 14th Amendment, read in conjunction with the Constitution's commitment to individual liberty and formal equality, forbids race-sorting by government.
% FOUNDING_PROBLEM_CORROBORATION: Justice John Marshall Harlan's Plessy dissent (1896) and contemporary originalist judges including Justice Clarence Thomas and Chief Justice John G. Roberts assert the colorblind reading. Outside the colorblind tradition, constitutional scholars including Richard Delgado, Jean Stefancic, Kimberlé Crenshaw, and economists such as Glenn Loury, along with civil rights organizations, explicitly contest that the Constitution is color-blind, asserting instead that constitutional harm is perpetuation of racial subordination and that addressing it requires race-conscious remedy. The founding problem is genuinely contested among sophisticated constitutional interpreters.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__colorblind_reading_tests).
:- end_tests(equal_protection_commitment__colorblind_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The colorblind reading extracts by treating race-conscious classifications as violations per se, regardless of context, subordination history, or remedial purpose. Extractiveness is moderate (0.42) because the harm is definitional—classification itself—rather than severe economic or coercive injury; applicants denied admission suffer real loss but the extraction mechanism is formal legal disqualification, not physical force or destruction of alternatives. Suppression is moderate (0.38) because institutional resistance to the colorblind constraint is substantial (universities and legislatures defend race-conscious programs; alternative readings command scholarly and political support); the constraint persists but does not fully suppress opposition. Theater is low-to-moderate (0.25) because the constraint's coordination claim (race-neutral boundary) is genuine, but a growing share of the enforcement apparatus is oriented toward invalidating programs rather than clarifying principles. The measurement trajectory shows modest growth: extractiveness rises from 0.28 (when the colorblind reading was dormant in doctrine, ~1950s) to 0.42 (contemporary strict scrutiny regime, 2020s onward), reflecting the reading's ascendance in judicial doctrine and institutional reach. Suppression remains stable at 0.38 because the underlying contest between readings persists; the colorblind reading's institutional dominance does not fully suppress the remedial and diversity readings, which remain live positions in law and scholarship.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (applicants subject to race-conscious classification) and the beneficiary seat (applicants not subject to it) compute entirely differently from the agenda-setter seats (courts and institutions). From the payer perspective, the constraint is enforced extraction: the state is removing opportunity based on race, and courts are validating that removal in the name of color-blindness. From the beneficiary perspective, the constraint is coordination: it removes a burden on their opportunity and ensures neutral treatment. From the court/institutional agenda-setter perspective, the constraint is constitutional fidelity: the state is forbidden from sorting by race, period. The engine computes these seat-specific classifications from the structural data (power, exit options, beneficiary/victim declarations); the authored claim (tangled_rope) reflects the reading's own coherence (it coordinates a boundary while extracting from one applicant set), not a single unified perspective. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Applicants subject to race-conscious classification bear the constraint's designated harm: their directionality is high (near 1.0, target seat). Applicants not subject to it are freed from the burden and thus benefit; their directionality is low (near 0.0, beneficiary seat). Courts enforcing strict scrutiny have high institutional power, generational time horizon, and analytical (rather than trapped or identity-locked) exit; their directionality reflects their role as enforcers of the constraint rather than those harmed by it—they are close to neutral in the constraint's operation (d~0.5), though administratively they are agenda-setters. Communities historically subordinated by racial caste are trapped: they cannot exit the jurisdiction and cannot exit the racial classification system itself (identity_locked); their exclusion from the colorblind reading's internal logic places them outside the beneficiary and victim binary, as the reading does not acknowledge ongoing subordination as the harm.
 *
 * MANDATROPHY ANALYSIS:
 *   The colorblind reading faces a mandatrophy challenge: the founding problem (whether the Constitution permits race-sorting) is highly contested, and the founding_problem_status is coded contested. If courts increasingly treat the colorblind constraint as settled doctrine, but the underlying constitutional question remains live in scholarship, politics, and alternative readings, the constraint risks mandatrophy—persisting as institution and enforcement while its mandate (that color-blindness is the correct reading) loses support among informed constituencies. The remedial reading explicitly challenges the mandate by asserting that constitutional harm is subordination, not classification. The diversity reading challenges it by asserting compelling state interests that override the color-blind ban. Both challenges are live and represent sophisticated constitutional reasoning; neither has been foreclosed or overridden. The constraint's persistence despite contested mandate signals either institutional lock-in or genuine doctrinal settlement; the measurement trajectory (stable extraction and suppression at later time points) suggests neither resolution has solidified—the constraint persists in enforcement while the mandate remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    color_blindness_vs_subordination_referent,
    'Is the relevant constitutional harm racial classification itself, or the perpetuation of racial subordination and caste?',
    'Textual analysis of the 14th Amendment''s original purpose and subsequent constitutional evolution. Investigation of whether color-blindness can coexist with structural subordination without contradiction.',
    'If constitutional harm is classification (colorblind reading), then race-conscious remedies are violations. If constitutional harm is subordination (remedial reading), then color-blindness may perpetuate the harm. The entire victim set and extraction profile depend on this framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(color_blindness_vs_subordination_referent, conceptual, 'Whether the equal protection harm is race classification or caste perpetuation.').

omega_variable(
    constitutionalism_via_exclusion,
    'Is the colorblind reading''s exclusion of subordination-analysis voices from its internal logic a structural feature or a bug?',
    'Comparison with other kernel readings and their internal inclusivity: do diversity and remedial readings include colorblind voices in their deliberation, or do they also exclude? Examination of whether the constitution can accommodate multiple readings simultaneously or whether they are zero-sum.',
    'If the exclusion is structural (colorblind framework cannot accommodate subordination analysis), the reading is confirmed as foreclose-class (logically incompatible with remedial reading). If the exclusion is political (deliberate omission rather than logical necessity), the reading is coexists_with class and the mandatrophy risk is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutionalism_via_exclusion, conceptual, 'Whether the colorblind reading''s exclusion of subordination voices is logically necessary or politically strategic.').

omega_variable(
    institutional_entrenchment_of_colorblind_doctrine,
    'To what extent is the contemporary dominance of colorblind doctrine in U.S. courts attributable to the coherence of the constitutional claim versus institutional path-dependence and personnel change?',
    'Institutional analysis of judicial appointments, doctrinal shifts following personnel changes, and comparative constitutional law (other democracies'' equal protection framings). Counterfactual analysis of how doctrine would evolve under different judicial composition.',
    'If dominance is due to claim coherence, the constraint''s persistence reflects genuine constitutional settlement. If dominance is due to institutional contingency, the constraint''s persistence despite contested mandate is a mandatrophy case—it survives enforcement by institutional lock-in rather than legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_entrenchment_of_colorblind_doctrine, empirical, 'Whether colorblind doctrine''s institutional dominance reflects constitutional truth or contingent personnel effects.').

omega_variable(
    definition_of_victim_under_colorblind_reading,
    'Are applicants denied admission through race-conscious programs correctly characterized as victims of constitutional violation, or are they beneficiaries of a system that violates equal protection against historically subordinated groups?',
    'Examination of counterfactual admissions outcomes under race-neutral alternatives; assessment of whether denial to individual applicants is harm or correction. Investigation of whether harm to individual applicants is comparable to harm of perpetuating caste.',
    'If characterization is correct, the victim set and extraction profile stand as authored. If the characterization reverses (individual applicants are beneficiaries of subordination, not victims of violation), the constraint''s type may shift from tangled_rope to snare or piton (inversion of victim/beneficiary roles).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(definition_of_victim_under_colorblind_reading, preference, 'Whether applicants denied admission via race-conscious policies are victims or beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__colorblind_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__colorblind_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(equa_tr_t25, equal_protection_commitment__colorblind_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__colorblind_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(equa_tr_t60, equal_protection_commitment__colorblind_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(equa_tr_t75, equal_protection_commitment__colorblind_reading, theater_ratio, 75, 0.25).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__colorblind_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__colorblind_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(equa_be_t25, equal_protection_commitment__colorblind_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__colorblind_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(equa_be_t60, equal_protection_commitment__colorblind_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(equa_be_t75, equal_protection_commitment__colorblind_reading, base_extractiveness, 75, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__colorblind_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__colorblind_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(equa_su_t25, equal_protection_commitment__colorblind_reading, suppression_requirement, 25, 0.37).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__colorblind_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(equa_su_t60, equal_protection_commitment__colorblind_reading, suppression_requirement, 60, 0.38).
narrative_ontology:measurement(equa_su_t75, equal_protection_commitment__colorblind_reading, suppression_requirement, 75, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__colorblind_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel decomposes into three reading-based constraints: colorblind_reading (this file), diversity_reading, and remedial_reading. Each reading instantiates a different constraint with distinct victim sets, beneficiary structures, and extraction profiles. The colorblind reading treats race-conscious state programs as violations per se; the diversity reading permits them toward compelling interests; the remedial reading permits them to dismantle subordination. These are not empirical differences resolvable by data—they are normative disputes about constitutional mandate. Each constraint is ε-invariant under its own reading: the colorblind reading's ε (0.42) reflects the harm of classification itself; the remedial reading's ε reflects perpetuation of subordination; the diversity reading's ε reflects restrictions on institutional autonomy. The three constraints form a family linked by network.affects_constraints. Kernel-frame analysis: all three readings operate within the single kernel (equal protection clause); each reading generates a different structural interpretation and therefore a different constraint classification from the engine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__colorblind_reading, organized, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
