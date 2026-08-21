% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__remedial_reading, []).

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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause: Remedial Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which permits race-conscious state action when narrowly tailored
 *   to remedy documented historical exclusion or achieve a compelling
 *   diversity interest. This reading, primarily articulated through Supreme
 *   Court precedents like Bakke (1978) and Grutter (2003), allowed
 *   universities to consider race as a 'plus factor' in admissions. The
 *   metrics reflect the increasing legal and administrative burden, and the
 *   growing resistance, that characterized this reading's application,
 *   culminating in its effective curtailment in higher education by SFFA v.
 *   Harvard/UNC (2023).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.65).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.75).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause: Remedial Reading").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '97e90ccf-723e-432f-abb6-055d1cf617ac').
narrative_ontology:cs_kernel_codification('97e90ccf-723e-432f-abb6-055d1cf617ac', fixed_text).
narrative_ontology:cs_authority_grounding('97e90ccf-723e-432f-abb6-055d1cf617ac', lineage).
narrative_ontology:cs_interpretation_layer_present('97e90ccf-723e-432f-abb6-055d1cf617ac').
narrative_ontology:cs_reading_relation('97e90ccf-723e-432f-abb6-055d1cf617ac', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('97e90ccf-723e-432f-abb6-055d1cf617ac', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('97e90ccf-723e-432f-abb6-055d1cf617ac', foundational, race_conscious_remedies_permissible).
narrative_ontology:cs_axiom_status(race_conscious_remedies_permissible, holdable).
narrative_ontology:cs_axiom_grounding('97e90ccf-723e-432f-abb6-055d1cf617ac', race_conscious_remedies_permissible, conventional).
narrative_ontology:cs_axiom('97e90ccf-723e-432f-abb6-055d1cf617ac', foundational, diversity_is_compelling_interest).
narrative_ontology:cs_axiom_status(diversity_is_compelling_interest, holdable).
narrative_ontology:cs_axiom_grounding('97e90ccf-723e-432f-abb6-055d1cf617ac', diversity_is_compelling_interest, conventional).
narrative_ontology:cs_reference_frame('97e90ccf-723e-432f-abb6-055d1cf617ac', bakke_grutter_framework).
narrative_ontology:cs_drift_state('97e90ccf-723e-432f-abb6-055d1cf617ac', post_sffa_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('97e90ccf-723e-432f-abb6-055d1cf617ac', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, universities_seeking_diversity).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, state_governments).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, universities_seeking_diversity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of the Equal Protection Clause's meaning, setting the legal tests for race-conscious state action. Its interpretations define the scope and limits of this reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, universal).

% Seek to implement policies that remedy historical exclusion or achieve diversity. They bear the administrative and legal burden of demonstrating 'narrow tailoring' and 'compelling interest' to satisfy this reading's requirements, facing constant legal challenges.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, state_governments, payer).

% Benefit from the ability to consider race as a 'plus factor' in admissions to achieve educational diversity. They also bear significant legal costs and administrative burdens to defend their policies against challenges, and face reputational risks.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, universities_seeking_diversity, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, universities_seeking_diversity, payer).

% Benefit from policies designed to remedy past discrimination and increase their representation in educational and professional institutions. Their identity is often tied to the historical context this reading seeks to address.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Are individuals who might have been admitted under a strictly race-blind process but were not, due to the consideration of race as a 'plus factor' for other applicants. They bear the direct cost of lost opportunity.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_blind_process, payer,
    powerless, immediate, trapped, national).

% Advocate for a strict race-neutral interpretation of the Equal Protection Clause, opposing any race-conscious state action. While active in legal and political discourse, their core premise is structurally excluded by this reading's allowance of race-conscious measures.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% Advocate for the continued use of race-conscious policies to address systemic inequality and promote diversity. They benefit from the legal pathway this reading provides for such efforts, though they also expend resources defending it.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_organizations, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__remedial_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate state action to address the persistent effects of historical racial discrimination and achieve the compelling interest of diversity in public institutions, particularly in education.
% TRANSFER_FUNCTION: Transfers opportunities (e.g., university admissions slots) to members of historically excluded groups, and imposes significant legal and administrative burdens on state actors and institutions seeking to implement race-conscious policies.
% ABSENT_VOICES: Advocates for a strictly 'colorblind' interpretation of the Equal Protection Clause are structurally excluded from the premise of this reading, as their core argument (no race-conscious action ever) is directly contradicted by this reading's allowance of such action under specific conditions.
% DISAPPEARANCE_RATIONALE: If this reading vanished, states and universities would lose a legal pathway to address historical racial exclusion or pursue diversity through race-conscious means. This would lead to a significant reorganization of education policy, civil rights enforcement, and institutional demographics, likely resulting in less diverse institutions and a slower pace of addressing historical inequities.
% FOUNDING_PROBLEM: To reconcile the Equal Protection Clause's promise of equality with the reality of persistent racial inequality and the recognized societal benefits of diversity, particularly in the context of higher education and public contracting.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, social scientists documenting ongoing racial disparities, and educational institutions seeking diverse student bodies corroborate the problem's live status. While the specific legal mechanisms have shifted, the underlying societal problems this reading sought to address remain contested but widely acknowledged by these external parties.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderate-to-high because while it aims for a coordination function (remedying inequality, achieving diversity), it imposes significant costs on rejected applicants and substantial administrative/legal burdens on state actors. `suppression` is high due to the strict legal tests ('narrow tailoring,' 'compelling interest') and constant litigation required to maintain such policies, making their implementation difficult and precarious. `theater_ratio` is moderate, as the process of justifying and defending these policies often involves performative aspects to satisfy judicial scrutiny. `resistance` is very high, reflecting the intense and sustained opposition from advocates of a colorblind approach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically excluded groups and civil rights organizations, this reading functions as a vital, albeit imperfect, rope for achieving social justice. From the perspective of rejected applicants and colorblind advocates, it operates as a snare, creating new forms of discrimination. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and civil rights organizations are clear beneficiaries, as the reading provides a legal avenue to address systemic inequality. Universities seeking diversity are also beneficiaries, but simultaneously payers due to the legal and administrative costs. State governments are agenda-setters and payers, as they must implement and defend these complex policies. Rejected applicants are direct victims. Colorblind advocates are structurally excluded from this reading's premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this reading as a pure Rope (ignoring the victims and enforcement costs) or a pure Snare (ignoring its genuine coordination function in addressing historical inequality and promoting diversity). The increasing extractiveness and suppression over time, coupled with high resistance, indicate a system under stress, where the coordination function is increasingly burdened by the costs of its enforcement and the extraction it generates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_efficacy_of_diversity,
    'To what extent do race-conscious policies, as permitted by this reading, empirically achieve the stated goals of remedying historical exclusion and fostering educational diversity?',
    'Longitudinal studies and meta-analyses of educational outcomes, social mobility, and institutional climate in institutions that implemented such policies, compared to those that did not or could not.',
    'Strong empirical evidence of efficacy would strengthen the coordination narrative; weak or contested evidence would amplify the extraction narrative, suggesting the ''compelling interest'' is more rhetorical than real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_efficacy_of_diversity, empirical, 'Empirical evidence for the effectiveness of race-conscious policies.').

omega_variable(
    legal_status_post_sffa,
    'What is the remaining legal scope for the ''remedial reading'' of the Equal Protection Clause outside of higher education, following the SFFA v. Harvard/UNC (2023) decision?',
    'Future Supreme Court rulings on race-conscious policies in other domains (e.g., public contracting, employment, K-12 education) or legislative action clarifying permissible uses.',
    'If the principles of SFFA are broadly applied, the effective scope of this reading will collapse, shifting its classification towards a Piton or even a dead Snare. If carve-outs or new justifications emerge, it may retain some Tangled Rope characteristics in other domains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_status_post_sffa, conceptual, 'The post-SFFA legal viability of race-conscious state action.').

omega_variable(
    burden_of_proof_for_tailoring,
    'Is the burden of proof for ''narrow tailoring'' and ''compelling interest'' so high that it effectively forecloses all but the most performative attempts at race-conscious action?',
    'Analysis of successful vs. unsuccessful legal challenges to race-conscious policies, focusing on the specific evidence and arguments required by courts. Judicial guidance on what constitutes sufficient documentation.',
    'If the burden is effectively insurmountable, the ''coordination'' aspect becomes largely theatrical, increasing the theater_ratio and pushing the constraint closer to a Snare or Piton, as the enforcement mechanism primarily serves to extract compliance costs rather than facilitate genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_proof_for_tailoring, empirical, 'The practical feasibility of meeting the legal tests for race-conscious action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.25).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__remedial_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__remedial_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__remedial_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(equa_tr_t2020, equal_protection_kernel__remedial_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__remedial_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__remedial_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__remedial_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(equa_be_t2020, equal_protection_kernel__remedial_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__remedial_reading, suppression_requirement, 1990, 0.63).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__remedial_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__remedial_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(equa_su_t2020, equal_protection_kernel__remedial_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Each reading instantiates a different constraint with unique beneficiaries, victims, and structural properties. They are linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
