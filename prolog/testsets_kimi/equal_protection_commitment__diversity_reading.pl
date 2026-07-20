% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Equal Protection Diversity Reading in Selective Admissions
 *   domain: constitutional law / political philosophy / social policy
 *
 * SUMMARY:
 *   This constraint instantiates the diversity reading of the equal
 *   protection commitment kernel: the interpretive claim that the Fourteenth
 *   Amendment permits selective universities to treat race as one factor
 *   among many in holistic admissions to achieve educational diversity as a
 *   compelling state interest. Structurally, it operates as a tangled rope:
 *   it coordinates a genuine policy problem (demographic homogeneity in elite
 *   institutions) while asymmetrically extracting procedural opacity and
 *   competitive displacement from individual applicants. The constraint is
 *   actively enforced by federal judicial review and has generated sustained
 *   resistance from colorblind constitutionalists. The claim/metric
 *   independence is maintained: the claimed type is tangled_rope while the
 *   metrics reflect low-moderate extractiveness consistent with the
 *   constraint's procedural rather than substantive character.
 *
 * KEY AGENTS:
 *   - Elite universities: primary beneficiaries (institutional/constrained exit) â receive discretionary authority over admissions composition.
 *   - Applicants to selective institutions: primary payers (powerless/constrained exit) â bear opacity and potential displacement from holistic race-conscious review.
 *   - Federal judiciary: agenda setter (institutional/analytical exit) â authors and enforces the constitutional standard.
 *   - Civil rights advocacy groups: observers (organized/constrained exit) â monitor and defend the framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.48).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Diversity Reading in Selective Admissions").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional law / political philosophy / social policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, '51694546-eb9a-48da-838c-5fad557a6ef2').
narrative_ontology:cs_kernel_codification('51694546-eb9a-48da-838c-5fad557a6ef2', fixed_text).
narrative_ontology:cs_authority_grounding('51694546-eb9a-48da-838c-5fad557a6ef2', lineage).
narrative_ontology:cs_interpretation_layer_present('51694546-eb9a-48da-838c-5fad557a6ef2').
narrative_ontology:cs_reading_relation('51694546-eb9a-48da-838c-5fad557a6ef2', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('51694546-eb9a-48da-838c-5fad557a6ef2', equal_protection_commitment__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('51694546-eb9a-48da-838c-5fad557a6ef2', foundational, diversity_as_compelling_interest).
narrative_ontology:cs_axiom_status(diversity_as_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('51694546-eb9a-48da-838c-5fad557a6ef2', diversity_as_compelling_interest, conventional).
narrative_ontology:cs_axiom('51694546-eb9a-48da-838c-5fad557a6ef2', foundational, race_as_one_factor_permissible).
narrative_ontology:cs_axiom_status(race_as_one_factor_permissible, holdable).
narrative_ontology:cs_axiom_grounding('51694546-eb9a-48da-838c-5fad557a6ef2', race_as_one_factor_permissible, conventional).
narrative_ontology:cs_reference_frame('51694546-eb9a-48da-838c-5fad557a6ef2', diversity_fostering_equal_protection).
narrative_ontology:cs_drift_state('51694546-eb9a-48da-838c-5fad557a6ef2', post_sffa_decision, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('51694546-eb9a-48da-838c-5fad557a6ef2', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, elite_universities).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, applicants_to_selective_institutions).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, educational_diversity_compelling_interest).
narrative_ontology:constraint_vindicates(equal_protection_commitment__diversity_reading, holistic_review_narrow_tailoring).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive constitutional authorization to consider race as one factor among many in holistic admissions, gaining discretion to shape entering classes toward educational diversity goals without triggering strict scrutiny violations.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, elite_universities, beneficiary,
    institutional, generational, constrained, national).

% Evaluated under holistic review where race may serve as a plus factor; individual academic metrics and achievements are weighed against institutional diversity goals, creating opacity around individual entitlement to admission and potentially displacing otherwise qualified candidates.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, applicants_to_selective_institutions, payer,
    powerless, biographical, constrained, national).

% Sets and enforces the constitutional standard through appellate review of admissions policies, determining whether race-conscious holistic review is narrowly tailored to a compelling diversity interest.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Monitor and litigate to preserve race-conscious admissions frameworks, filing amicus briefs and supporting university defendants in equal protection challenges.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__diversity_reading, civil_rights_advocacy_groups, observer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__diversity_reading, elite_universities).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a constitutionally permissible framework for selective institutions to pursue racial and ethnic diversity in admissions without violating the Equal Protection Clause, replacing a strict colorblind standard with a holistic, individualized review process.
% TRANSFER_FUNCTION: Transfers discretionary authority over admissions composition from a colorblind metric-based standard to university admissions offices, while transferring opacity and competitive displacement onto individual applicants whose candidacies may be tipped by demographic considerations.
% ABSENT_VOICES: Individual applicants who would have been admitted under a strictly race-neutral academic standard but are displaced by diversity considerations lack direct standing to challenge their specific exclusion; colorblind constitutionalists argue their voices are structurally marginalized within the doctrine.
% DISAPPEARANCE_RATIONALE: If the diversity reading vanished overnight, selective universities would lose the constitutional authorization for race-conscious admissions and would be compelled to rearrange toward purely race-neutral holistic or mechanical criteria; the demographic composition and competitive dynamics of elite higher education would shift substantially.
% FOUNDING_PROBLEM: Elite higher education in the United States exhibited severe racial and ethnic homogeneity, and strictly colorblind admissions standards appeared to reproduce demographic stratification despite growing cohort diversity; the problem was how to reconcile the Fourteenth Amendment's equality guarantee with integrative educational goals.
% FOUNDING_PROBLEM_CORROBORATION: Elite universities and civil rights advocates attest the problem remains live and justify the arrangement. Empirical researchers and colorblind advocates contest both the severity of the framing and the efficacy of the remedy; no neutral-party consensus corroborates the founding problem's current status independently of the benefiting parties' interests.
narrative_ontology:disappearance_verdict(equal_protection_commitment__diversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__diversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__diversity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_commitment__diversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__diversity_reading, 0.28, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low-moderate (0.28) because the constraint is procedural: it does not mandate specific racial quotas or outcomes but permits race to serve as a plus factor within individualized review. Suppression is moderate (0.48) because the constraint's persistence depends on judicial enforcement striking down non-compliant admissions schemes, but alternatives (colorblind review) remain structurally available and are actively advocated. Theater ratio rises over the interval (0.20 to 0.48) as the doctrine becomes increasingly defensive and performative under litigation pressure, with universities producing ever-more elaborate holistic justifications that mask a narrowing operational space. Resistance is high (0.72) due to persistent constitutional challenges, culminating in the 2023 SFFA decisions. Accessibility collapse is moderate (0.42) because the colorblind alternative remains intellectually and legally viable.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (federal judiciary) and beneficiary seat (elite universities) experience the constraint as a necessary coordination mechanism preserving institutional autonomy and educational mission. The payer seat (applicants) experiences the same structure as an opaque barrier to individual meritocratic entitlement. The engine will compute divergent per-seat classifications from this structural asymmetry: low directionality for universities, high directionality for applicants.
 *
 * DIRECTIONALITY LOGIC:
 *   Elite universities are declared beneficiaries: they gain discretion and are subsidized by the constraint's authorization (low d). Applicants to selective institutions are declared victims: they bear the costs of opacity and displacement (high d). The federal judiciary sits at symmetric analytical distance, neither collecting nor paying. Civil rights groups observe from a position aligned with the coordination function but do not capture extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â racial homogeneity in elite education â was contested in its severity and diagnosis. The arrangement persisted for decades after its initial justification (Bakke 1978) through Grutter (2003) and Fisher (2013/2016), even as empirical evidence on mismatch effects and alternative diversity mechanisms accumulated. The 2023 SFFA decision represents a mandatrophy resolution: the Court explicitly found the justification exhausted and the constraint's function atrophied, converting what had become a performative piton back into a resolved mandatrophy. However, during the interval modeled here (0â20), the constraint was still actively enforced, justifying the tangled rope classification rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_substantive_extraction,
    'Does the extraction measured here reflect merely procedural opacity in holistic review, or does the constraint constitute substantive redistribution of educational opportunity with material life-course effects?',
    'Longitudinal outcome studies comparing earnings, career trajectories, and social capital of applicants admitted under holistic diversity review versus those displaced by it, controlling for observables.',
    'If extraction is purely procedural, the constraint remains low-moderate Îµ and tangled_rope; if substantive redistribution with measurable victim harm is demonstrated, Îµ rises and the classification edges toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_extraction, empirical, 'Whether the constraint''s extraction is procedural or substantive').

omega_variable(
    kernel_reading_irreducibility,
    'Is the diversity reading of equal protection a structurally distinct constraint from its colorblind and remedial siblings, or do the three readings represent rhetorical framings of a single underlying power arrangement?',
    'Comparative institutional analysis: if the three readings produce different beneficiary/victim sets, different enforcement costs, and different Îµ values when modeled independently, they are structurally distinct constraints per the Îµ-invariance principle.',
    'If the readings are rhetorical masks for a single arrangement, the kernel should be modeled as one constraint with high conceptual omega; if structurally distinct, the decomposition into three linked stories is warranted and the diversity reading''s specific classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_irreducibility, conceptual, 'Whether the three equal protection readings are structurally distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_diversity_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(epc_diversity_tr_t4, equal_protection_commitment__diversity_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(epc_diversity_tr_t8, equal_protection_commitment__diversity_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(epc_diversity_tr_t12, equal_protection_commitment__diversity_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(epc_diversity_tr_t16, equal_protection_commitment__diversity_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(epc_diversity_tr_t20, equal_protection_commitment__diversity_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(epc_diversity_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(epc_diversity_be_t4, equal_protection_commitment__diversity_reading, base_extractiveness, 4, 0.24).
narrative_ontology:measurement(epc_diversity_be_t8, equal_protection_commitment__diversity_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(epc_diversity_be_t12, equal_protection_commitment__diversity_reading, base_extractiveness, 12, 0.28).
narrative_ontology:measurement(epc_diversity_be_t16, equal_protection_commitment__diversity_reading, base_extractiveness, 16, 0.32).
narrative_ontology:measurement(epc_diversity_be_t20, equal_protection_commitment__diversity_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(epc_diversity_su_t0, equal_protection_commitment__diversity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(epc_diversity_su_t4, equal_protection_commitment__diversity_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(epc_diversity_su_t8, equal_protection_commitment__diversity_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(epc_diversity_su_t12, equal_protection_commitment__diversity_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(epc_diversity_su_t16, equal_protection_commitment__diversity_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(epc_diversity_su_t20, equal_protection_commitment__diversity_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% This constraint is the diversity reading of the equal_protection_commitment kernel, decomposed from the colorblind and remedial readings per the Îµ-invariance principle. Each reading instantiates a distinct constraint with different beneficiary/victim structures, different Îµ values, and different axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
