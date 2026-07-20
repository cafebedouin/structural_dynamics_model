% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__colorblind_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Equal Protection Colorblind Reading (Harlan's Dissent)
 *   domain: constitutional law / political philosophy / social policy
 *
 * SUMMARY:
 *   The colorblind reading of the Equal Protection Clause, rooted in Harlan's
 *   Plessy dissent, holds that the Constitution forbids any state use of
 *   racial classification. This reading currently enjoys renewed judicial
 *   ascendancy and functions as a legal constraint enforced by federal
 *   courts. It coordinates a formal-equality legal order while asymmetrically
 *   extracting policy discretion from state institutions and race-conscious
 *   remedial capacity from disadvantaged communities.
 *
 * KEY AGENTS:
 *   - Federal judiciary: agenda_setter (institutional/analytical) â enforces the reading and sets constitutional precedent.
 *   - Asian/white applicants: beneficiaries (moderate/constrained) â protected from state racial classification.
 *   - Public universities: payers (institutional/constrained) â lose admissions discretion and must dismantle race-conscious programs.
 *   - Disadvantaged minorities: payers (powerless/constrained) â lose access to remedial race-conscious programs.
 *   - Diversity advocates: excluded (organized/constrained) â their substantive-equality arguments are structurally excluded from constitutional legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__colorblind_reading, 0.42).
domain_priors:suppression_score(equal_protection_commitment__colorblind_reading, 0.65).
domain_priors:theater_ratio(equal_protection_commitment__colorblind_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(equal_protection_commitment__colorblind_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__colorblind_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__colorblind_reading, "Equal Protection Colorblind Reading (Harlan's Dissent)").
narrative_ontology:topic_domain(equal_protection_commitment__colorblind_reading, "constitutional law / political philosophy / social policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__colorblind_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__colorblind_reading, '4f051ada-f2ff-4478-afee-015d32e67d0d').
narrative_ontology:cs_kernel_codification('4f051ada-f2ff-4478-afee-015d32e67d0d', formalized).
narrative_ontology:cs_authority_grounding('4f051ada-f2ff-4478-afee-015d32e67d0d', lineage).
narrative_ontology:cs_interpretation_layer_present('4f051ada-f2ff-4478-afee-015d32e67d0d').
narrative_ontology:cs_reading_relation('4f051ada-f2ff-4478-afee-015d32e67d0d', equal_protection_commitment__diversity_reading, forecloses).
narrative_ontology:cs_reading_relation('4f051ada-f2ff-4478-afee-015d32e67d0d', equal_protection_commitment__remedial_reading, forecloses).
narrative_ontology:cs_axiom('4f051ada-f2ff-4478-afee-015d32e67d0d', foundational, state_racial_classification_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_racial_classification_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('4f051ada-f2ff-4478-afee-015d32e67d0d', state_racial_classification_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('4f051ada-f2ff-4478-afee-015d32e67d0d', foundational, equal_protection_requires_race_neutrality).
narrative_ontology:cs_axiom_status(equal_protection_requires_race_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('4f051ada-f2ff-4478-afee-015d32e67d0d', equal_protection_requires_race_neutrality, deontological).
narrative_ontology:cs_reference_frame('4f051ada-f2ff-4478-afee-015d32e67d0d', colorblind_constitutional_order).
narrative_ontology:cs_drift_state('4f051ada-f2ff-4478-afee-015d32e67d0d', contemporary_post_sffa_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4f051ada-f2ff-4478-afee-015d32e67d0d', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__colorblind_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__colorblind_reading, asian_white_applicants).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, public_universities).
narrative_ontology:constraint_victim(equal_protection_commitment__colorblind_reading, disadvantaged_minorities).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, formal_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_commitment__colorblind_reading, colorblind_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Equal Protection Clause under the colorblind reading, striking down race-conscious state programs and consolidating constitutional authority over racial policy.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, national).

% Seek admission to selective public institutions; the colorblind reading protects them from state racial classification that would disadvantage them in admissions and contracting.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, asian_white_applicants, beneficiary,
    moderate, biographical, constrained, national).

% Operate admissions and employment programs; must dismantle race-conscious practices and rebuild around race-neutral criteria, losing substantial policy discretion and facing compliance costs.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, public_universities, payer,
    institutional, generational, constrained, national).

% Historically excluded groups that relied on race-conscious remedial programs; lose access to affirmative action and other race-aware state interventions under strict colorblind enforcement.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, disadvantaged_minorities, payer,
    powerless, generational, constrained, national).

% Argue that race-conscious measures are necessary for substantive equality; their constitutional arguments are structurally excluded from legitimacy under the colorblind reading's categorical prohibition.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__colorblind_reading, diversity_advocates, excluded,
    organized, generational, constrained, national).

narrative_ontology:fixing_cost_class(equal_protection_commitment__colorblind_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents state-enforced racial caste systems by imposing a uniform rule of formal equality, eliminating the need for individualized review of racial motive through a flat prohibition on classification.
% TRANSFER_FUNCTION: Moves constitutional authority over racial policy from state legislatures and universities to the federal judiciary, and moves protection from racial classification to individual citizens.
% ABSENT_VOICES: Disadvantaged minority communities and diversity advocates who would argue that colorblindness perpetuates substantive inequality; their position is ruled constitutionally illegitimate under this reading.
% DISAPPEARANCE_RATIONALE: If the colorblind reading vanished overnight, public universities and state agencies would reintroduce race-conscious admissions, contracting, and districting within one cycle; the constitutional structure of civil rights law would reorganize around permissive rather than prohibitive rules.
% FOUNDING_PROBLEM: State-enforced racial segregation and caste systems (Jim Crow) that used racial classification to subordinate Black Americans.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights historians and critical race theorists outside the formal-equality tradition attest that de jure segregation is dismantled, though they contest whether the problem is dead or mutated; originalist legal scholars corroborate the founding-problem framing.
narrative_ontology:disappearance_verdict(equal_protection_commitment__colorblind_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__colorblind_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__colorblind_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__colorblind_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__colorblind_reading, 0.42, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.42) is moderate-high because the reading categorically removes policy tools from institutions and remedial pathways from disadvantaged groups, regardless of benign intent. Suppression (0.65) reflects active judicial enforcement striking down democratically enacted programs. Theater ratio (0.25) is relatively low because the judicial enforcement is substantive, though some performative compliance occurs. Accessibility collapse (0.70) is high because once colorblindness is accepted as a constitutional premise, race-conscious alternatives become constitutionally unthinkable. Resistance (0.55) reflects persistent institutional and social pushback against strict formal equality.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial seat, the constraint is a rope of formal equality preventing racial balkanization and state discrimination; from the minority-community and university seats, it operates as a tangled rope that coordinates equality rhetoric while extracting remedial capacity and institutional autonomy. The engine computes this divergence from the structural asymmetry in exit options and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits near the beneficiary end (low d) as the authority that gains interpretive power and agenda control. Protected applicants sit near the beneficiary end. Public universities and disadvantaged minorities sit at the target end (high d) because the constraint removes their policy tools and remedial pathways. Diversity advocates are excluded, receiving no directional subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to dismantle Jim Crow (founding problem is dead). Its persistence as an absolute bar on all race-conscious measures long after de jure segregation ended carries mandatrophy risk. However, the recent judicial revival argues the problem is contested rather than resolved â the reading is being reactivated rather than merely decaying, so piton classification is not yet warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colorblindness_remedial_barrier,
    'Does the colorblind reading foreclose effective constitutional remedies for ongoing racial stratification, or does it prevent only genuinely harmful classification?',
    'Comparative analysis of jurisdictions with and without race-conscious remediation, measuring stratification outcomes over generational time horizons.',
    'If colorblindness perpetuates stratification, the constraint functions as a snare disguised as formal equality; if not, it remains a protective rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblindness_remedial_barrier, empirical, 'Whether colorblindness blocks necessary remedial tools.').

omega_variable(
    classification_intrinsic_harm,
    'Is state racial classification intrinsically harmful, or harmful only when deployed for subordination?',
    'Historical and comparative analysis of benign racial classifications (e.g., census, medical research, tribal coordination).',
    'If benign classification exists, the colorblind reading''s categorical prohibition is overbroad and extractive; if classification is always harmful, the reading is narrowly tailored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_intrinsic_harm, conceptual, 'Whether racial classification is intrinsically or only instrumentally harmful.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the colorblind reading logically foreclose diversity and remedial readings, or can they coexist in a prudential constitutional framework?',
    'Engine computation of cs_axiom_contradiction from sibling axioms and grounding types; legal analysis of whether a single jurisprudence can hold both categorical prohibition and permissive factor-use.',
    'Determines whether the kernel is genuinely split into mutually exclusive constraints or whether readings can be held as prudential variations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between colorblind and sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__colorblind_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_colorblind_tr_t0, equal_protection_commitment__colorblind_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ep_colorblind_tr_t5, equal_protection_commitment__colorblind_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ep_colorblind_tr_t10, equal_protection_commitment__colorblind_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(ep_colorblind_tr_t15, equal_protection_commitment__colorblind_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(ep_colorblind_tr_t20, equal_protection_commitment__colorblind_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(ep_colorblind_tr_t25, equal_protection_commitment__colorblind_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(ep_colorblind_tr_t30, equal_protection_commitment__colorblind_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(ep_colorblind_be_t0, equal_protection_commitment__colorblind_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ep_colorblind_be_t5, equal_protection_commitment__colorblind_reading, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(ep_colorblind_be_t10, equal_protection_commitment__colorblind_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(ep_colorblind_be_t15, equal_protection_commitment__colorblind_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(ep_colorblind_be_t20, equal_protection_commitment__colorblind_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(ep_colorblind_be_t25, equal_protection_commitment__colorblind_reading, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(ep_colorblind_be_t30, equal_protection_commitment__colorblind_reading, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ep_colorblind_su_t0, equal_protection_commitment__colorblind_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ep_colorblind_su_t5, equal_protection_commitment__colorblind_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(ep_colorblind_su_t10, equal_protection_commitment__colorblind_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(ep_colorblind_su_t15, equal_protection_commitment__colorblind_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(ep_colorblind_su_t20, equal_protection_commitment__colorblind_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(ep_colorblind_su_t25, equal_protection_commitment__colorblind_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(ep_colorblind_su_t30, equal_protection_commitment__colorblind_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__colorblind_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__colorblind_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal protection commitment kernel decomposes into three structurally distinct readings: colorblind (forbids all racial classification), diversity (permits race as one factor), and remedial (permits race-conscious dismantling of subordination). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. This reading influences the legal environment in which siblings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
