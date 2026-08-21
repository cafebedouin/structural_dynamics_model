% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection: Remedial Reading (Race-Conscious Remediation)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which mandates race-conscious policies to actively counteract the
 *   effects of historical group subordination and achieve substantive
 *   equality. It is one of three competing interpretations of the Equal
 *   Protection Clause (alongside the 'colorblind' and 'diversity' readings).
 *   This reading is structurally a scaffold, as it implies a temporary
 *   measure with a sunset clause (when remediation is complete), but it
 *   requires active enforcement and involves significant extraction from
 *   non-preferred groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.78).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.65).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection: Remedial Reading (Race-Conscious Remediation)").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '2a148bcd-0681-44cd-8a49-3f6166b3631b').
narrative_ontology:cs_kernel_codification('2a148bcd-0681-44cd-8a49-3f6166b3631b', fixed_text).
narrative_ontology:cs_authority_grounding('2a148bcd-0681-44cd-8a49-3f6166b3631b', lineage).
narrative_ontology:cs_interpretation_layer_present('2a148bcd-0681-44cd-8a49-3f6166b3631b').
narrative_ontology:cs_reading_relation('2a148bcd-0681-44cd-8a49-3f6166b3631b', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('2a148bcd-0681-44cd-8a49-3f6166b3631b', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('2a148bcd-0681-44cd-8a49-3f6166b3631b', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('2a148bcd-0681-44cd-8a49-3f6166b3631b', substantive_equality_mandate, deontological).
narrative_ontology:cs_axiom('2a148bcd-0681-44cd-8a49-3f6166b3631b', foundational, historical_subordination_requires_remediation).
narrative_ontology:cs_axiom_status(historical_subordination_requires_remediation, holdable).
narrative_ontology:cs_axiom_grounding('2a148bcd-0681-44cd-8a49-3f6166b3631b', historical_subordination_requires_remediation, empirically_contingent).
narrative_ontology:cs_reference_frame('2a148bcd-0681-44cd-8a49-3f6166b3631b', post_civil_war_reconstruction_amendments).
narrative_ontology:cs_drift_state('2a148bcd-0681-44cd-8a49-3f6166b3631b', contemporary_judicial_review, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2a148bcd-0681-44cd-8a49-3f6166b3631b', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, educational_institutions).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, reparative_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the primary beneficiaries of policies designed to remediate past and ongoing systemic discrimination. The constraint aims to elevate their social and economic standing, requiring active measures to counteract historical disadvantages. Exit is identity-locked as their status is tied to the very group identity the remediation addresses.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, identity_locked, national).

% Individuals from groups not targeted for remediation may experience adverse impacts, such as being denied admission or opportunities in favor of members of preferred groups, even if they personally have not contributed to historical subordination. Their options are to accept the policy, seek alternative paths, or litigate.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups, payer,
    moderate, biographical, constrained, local).

% These institutions are responsible for interpreting and implementing the Equal Protection Clause, including determining the scope and limits of race-conscious remedial policies. They set the legal framework and enforce compliance, balancing competing claims of equality.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Universities and schools implement race-conscious admissions and hiring policies to achieve remedial goals. They bear the administrative burden and legal risk of these policies, often facing challenges from individuals claiming reverse discrimination. Their exit options are limited by legal mandates and institutional mission.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, educational_institutions, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, educational_institutions, agenda_setter).

% Advocates for a colorblind interpretation of the Equal Protection Clause argue that any racial classification is unconstitutional, regardless of its intent. They are excluded from the direct implementation of remedial policies but actively challenge them through litigation and public discourse.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% Advocates for diversity-based affirmative action support race-conscious policies but ground them in educational benefits for all, rather than remediation. While their goals may overlap, their legal and philosophical justifications differ from the remedial reading, leading to distinct policy arguments.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, diversity_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal efforts to address and overcome the lingering effects of historical racial discrimination and subordination, aiming to achieve a state of substantive equality where all groups have equitable opportunities and outcomes.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and social capital from individuals not belonging to historically subordinated groups to members of those groups, as a means of rectifying past injustices and achieving future equity.
% ABSENT_VOICES: The voices of those who believe that individual merit should be the sole criterion for opportunity, irrespective of group identity or historical context, are often marginalized in the discourse surrounding remedial policies. They would argue that such policies perpetuate racial division and unfairness.
% DISAPPEARANCE_RATIONALE: If the remedial reading of Equal Protection vanished, policies aimed at addressing systemic racial inequality would cease, leading to a re-entrenchment of existing disparities. The legal and social landscape would shift dramatically, likely exacerbating inequalities for historically subordinated groups and altering the balance of power in educational and economic spheres.
% FOUNDING_PROBLEM: The enduring legacy of slavery, Jim Crow laws, and other forms of systemic racial discrimination, which created and perpetuated deep-seated group subordination and inequality in American society.
% FOUNDING_PROBLEM_CORROBORATION: Sociological studies, economic data on wealth and income disparities, and ongoing reports from civil rights organizations consistently corroborate the continued existence of systemic inequalities rooted in historical subordination. This corroboration comes from independent academic research and advocacy groups, not solely from the direct beneficiaries of the policies.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because it mandates significant re-allocation of opportunities and resources to address systemic inequalities. Suppression (0.65) is moderate, reflecting the active legal and institutional enforcement required to implement these policies against resistance, but also the ongoing legal challenges that limit their scope. Theater ratio is low (0.15) as the policies are genuinely intended to achieve their stated remedial goals, though their effectiveness is debated. The temporal measurements show a decrease in extractiveness and suppression from the initial post-Civil Rights era, reflecting a narrowing of the scope of such policies by judicial decisions, followed by a slight increase in extractiveness as advocates push for more robust measures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups, this constraint is a necessary, albeit often insufficient, mechanism for justice and equality. From the perspective of individual members of non-preferred groups, it can be perceived as an unfair imposition that penalizes them for historical wrongs they did not commit. Courts and legislatures navigate these competing perspectives, leading to a contested and evolving legal landscape.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated racial groups are clear beneficiaries (d near 0.0), as the policies are designed to uplift them. Individual members of non-preferred groups are payers/victims (d near 1.0), as they bear the direct costs of these policies. Courts and legislatures act as agenda-setters, defining and enforcing the scope of the constraint. Educational institutions are both payers (implementing policies, facing legal challenges) and agenda-setters (shaping policy within legal bounds).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a scaffold, implying a temporary mandate. Mandatrophy would occur if the historical subordination were fully remediated, but the race-conscious policies persisted due to institutional inertia or rent-seeking by beneficiaries. The sunset clause is critical to prevent it from becoming a snare. The 'live' status of the founding problem indicates that, by this reading's own lights, the mandate is not yet resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_criteria,
    'What objective, measurable criteria would signal the completion of remediation and trigger the sunset clause for race-conscious policies?',
    'Establishment of clear, data-driven metrics for substantive equality (e.g., parity in wealth, education, health outcomes) agreed upon by all stakeholders, with independent verification.',
    'Without clear criteria, the ''scaffold'' risks becoming a ''tangled_rope'' or ''snare'' as the justification for its temporary nature becomes perpetually contested, leading to indefinite enforcement and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_completion_criteria, preference, 'Defines the conditions for the constraint''s termination.').

omega_variable(
    causality_of_disparities,
    'To what extent are current group disparities attributable to historical subordination versus other factors (e.g., cultural, individual choice)?',
    'Longitudinal sociological and economic studies controlling for various factors, with robust statistical analysis and peer review.',
    'If disparities are found to be primarily due to non-remedial factors, the justification for race-conscious remediation weakens, potentially shifting the constraint towards a ''piton'' or ''snare'' if maintained without a valid coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causality_of_disparities, empirical, 'Determines the empirical basis for remedial policies.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the Equal Protection Clause, or a policy preference masquerading as constitutional interpretation?',
    'Legal scholarship and judicial precedent analysis, focusing on the methodology of constitutional interpretation and its consistency with historical and textual context.',
    'If deemed a policy preference, its legitimacy as a constitutional constraint would be undermined, potentially reclassifying it as a ''snare'' (if coercively enforced) or ''piton'' (if maintained by inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Addresses the meta-level question of the reading''s constitutional validity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1960, equal_protection_clause__remedial_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(equa_tr_t1980, equal_protection_clause__remedial_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_clause__remedial_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_clause__remedial_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1960, equal_protection_clause__remedial_reading, base_extractiveness, 1960, 0.85).
narrative_ontology:measurement(equa_be_t1980, equal_protection_clause__remedial_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(equa_be_t2000, equal_protection_clause__remedial_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(equa_be_t2024, equal_protection_clause__remedial_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1960, equal_protection_clause__remedial_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(equa_su_t1980, equal_protection_clause__remedial_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(equa_su_t2000, equal_protection_clause__remedial_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(equa_su_t2024, equal_protection_clause__remedial_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, affirmative_action_policies).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, desegregation_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Each reading instantiates a different constraint with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
