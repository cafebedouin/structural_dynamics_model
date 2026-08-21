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
 *   constraint_id: equal_protection_kernel__remedial_reading
 *   human_readable: Equal Protection Clause: Remedial/Diversity Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'remedial/diversity' reading of the Equal
 *   Protection Clause, which permits race-conscious state action when
 *   narrowly tailored to remedy documented historical exclusion or achieve a
 *   compelling diversity interest. This reading was dominant from the Bakke
 *   decision (1978) through Grutter (2003), but faced increasing legal
 *   challenges, culminating in the Students for Fair Admissions (SFFA)
 *   rulings (2023) which largely dismantled its application in higher
 *   education. The constraint is claimed as a Tangled Rope due to its dual
 *   function of coordinating remedial action/diversity goals while imposing
 *   costs on other applicants and requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__remedial_reading, 0.8).
domain_priors:suppression_score(equal_protection_kernel__remedial_reading, 0.85).
domain_priors:theater_ratio(equal_protection_kernel__remedial_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(equal_protection_kernel__remedial_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__remedial_reading, "Equal Protection Clause: Remedial/Diversity Reading").
narrative_ontology:topic_domain(equal_protection_kernel__remedial_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__remedial_reading, '50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0').
narrative_ontology:cs_kernel_codification('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', fixed_text).
narrative_ontology:cs_authority_grounding('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', lineage).
narrative_ontology:cs_interpretation_layer_present('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0').
narrative_ontology:cs_reading_relation('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', equal_protection_kernel__antisubordination_reading, coexists_with).
narrative_ontology:cs_axiom('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', foundational, race_conscious_action_permissible_for_remedy_or_diversity).
narrative_ontology:cs_axiom_status(race_conscious_action_permissible_for_remedy_or_diversity, holdable).
narrative_ontology:cs_axiom_grounding('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', race_conscious_action_permissible_for_remedy_or_diversity, conventional).
narrative_ontology:cs_axiom('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', secondary, strict_scrutiny_applies_to_racial_classifications).
narrative_ontology:cs_axiom_status(strict_scrutiny_applies_to_racial_classifications, holdable).
narrative_ontology:cs_axiom_grounding('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', strict_scrutiny_applies_to_racial_classifications, conventional).
narrative_ontology:cs_reference_frame('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', bakke_grutter_framework).
narrative_ontology:cs_drift_state('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', post_sfaf_harvard_unc_ruling, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('50fd0e1c-2c97-4f52-96aa-f59ad4aeb6f0', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__remedial_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, institutions_seeking_diversity).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_conscious_policy).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, state_actors_facing_litigation).
narrative_ontology:constraint_victim(equal_protection_kernel__remedial_reading, conservative_legal_foundations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__remedial_reading, state_universities).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, affirmative_action_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_kernel__remedial_reading, diversity_as_compelling_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement race-conscious admissions policies to achieve diversity and remedy past discrimination, navigating complex legal standards. They benefit from a diverse student body but bear the cost of litigation and administrative burden.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, state_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, state_universities, beneficiary).

% Benefit from increased access to higher education and other opportunities through race-conscious policies designed to remedy historical exclusion and promote diversity. Their access is contingent on the policy's legality.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Bear the cost of being denied admission to institutions they might otherwise have entered under race-blind criteria. They often feel unfairly disadvantaged by policies intended to benefit others.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, rejected_applicants_under_race_conscious_policy, payer,
    powerless, biographical, constrained, national).

% Advocate for and defend race-conscious policies, seeing them as essential tools for achieving racial justice and equality. They benefit from the vindication of their policy goals and influence legal and public discourse.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, civil_rights_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, civil_rights_advocates, agenda_setter).

% Actively challenge race-conscious policies through litigation, arguing they violate the principle of colorblindness. They bear the financial and strategic costs of these legal battles but seek to overturn the remedial reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, conservative_legal_foundations, payer,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__remedial_reading, conservative_legal_foundations, agenda_setter).

% The ultimate arbiter of the Equal Protection Clause, interpreting its meaning and setting the legal boundaries for state action. Its rulings define the scope and enforceability of the remedial reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, universal).

% Observes and is affected by the societal outcomes of these policies, including debates over fairness, merit, and diversity in institutions. Public opinion often shifts, influencing political and legal pressures.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__remedial_reading, general_public, observer,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate state action towards remedying historical discrimination and achieving educational diversity, ensuring a pathway for historically excluded groups into institutions while adhering to constitutional limits.
% TRANSFER_FUNCTION: Transfers educational opportunities and social capital to historically excluded groups, while potentially transferring the burden of exclusion to other applicants who would have been admitted under race-blind criteria. It also transfers legal costs to state actors defending policies and to legal foundations challenging them.
% ABSENT_VOICES: Those who believe any race-conscious action is inherently discriminatory, regardless of intent, are often excluded from the policy-making process, only engaging through litigation. Their perspective is often framed as opposition rather than a legitimate alternative within the policy debate.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state actors would likely revert to strictly race-blind policies, potentially reducing diversity in higher education and other sectors, and altering the legal landscape for civil rights enforcement. The legal and social structures around affirmative action would collapse, leading to significant institutional and societal reorganization.
% FOUNDING_PROBLEM: The historical and ongoing exclusion of racial minorities from educational and economic opportunities, leading to systemic inequality and a lack of diversity in key institutions, which the Equal Protection Clause was intended to address.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, sociological studies, and historical analyses consistently document the persistence of systemic inequalities and the benefits of diversity, corroborating the problem's live status from outside the direct beneficiaries of affirmative action policies. However, the *means* to address it are highly contested.
narrative_ontology:disappearance_verdict(equal_protection_kernel__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_kernel__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__remedial_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high and rising because implementing race-conscious policies became increasingly complex and costly, requiring extensive documentation and legal defense, effectively 'extracting' significant institutional resources and creating burdens for those excluded. Suppression is also high and rising, reflecting the intense legal and political efforts required to defend these policies against challenges, actively suppressing purely colorblind alternatives. Theater ratio increased as institutions, facing legal restrictions, resorted to more indirect and performative means to achieve diversity goals. Accessibility collapse decreased as the legal avenues for race-conscious action narrowed, making race-neutral alternatives more accessible. Resistance is very high, reflecting the sustained and successful legal challenges against this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this reading was a necessary tool for justice and equity, a coordination mechanism to address systemic issues. From the perspective of those who bore the costs (e.g., rejected applicants), it was an extractive mechanism that imposed unfair burdens. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically excluded groups and institutions seeking diversity are beneficiaries, gaining access or achieving institutional goals. Rejected applicants under race-conscious policies are payers, bearing the direct cost of exclusion. Civil rights advocates benefit from the policy's existence, while conservative legal foundations bear the cost of challenging it. The Supreme Court acts as the agenda-setter, defining the scope and enforceability of the reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compelling_interest_genuineness,
    'Is the ''compelling interest'' in diversity a genuine coordination problem that requires race-conscious solutions, or a legal fiction used to permit race-conscious action for other (e.g., remedial) purposes?',
    'Empirical studies on the unique benefits of racial diversity that cannot be achieved through race-neutral means, or legal analysis of judicial intent behind the ''compelling interest'' standard.',
    'If a genuine, unique coordination problem, it strengthens the justification for race-conscious action. If a legal fiction, it highlights the performative aspect of the constraint and increases its effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_interest_genuineness, conceptual, 'Whether diversity as a compelling interest is a genuine coordination problem or a legal construct.').

omega_variable(
    narrow_tailoring_effectiveness,
    'To what extent did the ''narrowly tailored'' requirement actually prevent undue burden on non-minority applicants, or was it a performative legal standard that allowed significant burdens?',
    'Quantitative analysis of admissions data comparing outcomes for different racial groups under race-conscious vs. hypothetical race-blind policies, and qualitative studies of applicant experiences.',
    'If ''narrowly tailored'' was largely performative, the effective extraction from rejected applicants was higher than acknowledged, pushing the constraint closer to a Snare. If effective, it supports the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_tailoring_effectiveness, empirical, 'Effectiveness of ''narrowly tailored'' in mitigating burdens on non-minority applicants.').

omega_variable(
    kernel_reading_context,
    'This constraint is one reading of the `equal_protection_kernel`, specifically the `remedial_reading`. Sibling readings include `colorblind_reading` and `antisubordination_reading`. The disagreement is located in the interpretation of the Equal Protection Clause''s core prohibition and permissible state action.',
    'Analysis of judicial opinions, legislative history, and academic discourse surrounding the Equal Protection Clause.',
    'Understanding the contestation between readings is crucial for assessing the stability and legitimacy of this constraint. Resolution would clarify the foundational principles governing racial equality in law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Contextualizes this constraint as one reading within a contested constitutional kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__remedial_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(equa_tr_t1988, equal_protection_kernel__remedial_reading, theater_ratio, 1988, 0.2).
narrative_ontology:measurement(equa_tr_t1998, equal_protection_kernel__remedial_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(equa_tr_t2008, equal_protection_kernel__remedial_reading, theater_ratio, 2008, 0.45).
narrative_ontology:measurement(equa_tr_t2018, equal_protection_kernel__remedial_reading, theater_ratio, 2018, 0.55).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__remedial_reading, theater_ratio, 2023, 0.6).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__remedial_reading, base_extractiveness, 1978, 0.45).
narrative_ontology:measurement(equa_be_t1988, equal_protection_kernel__remedial_reading, base_extractiveness, 1988, 0.55).
narrative_ontology:measurement(equa_be_t1998, equal_protection_kernel__remedial_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(equa_be_t2008, equal_protection_kernel__remedial_reading, base_extractiveness, 2008, 0.72).
narrative_ontology:measurement(equa_be_t2018, equal_protection_kernel__remedial_reading, base_extractiveness, 2018, 0.77).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__remedial_reading, base_extractiveness, 2023, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__remedial_reading, suppression_requirement, 1978, 0.5).
narrative_ontology:measurement(equa_su_t1988, equal_protection_kernel__remedial_reading, suppression_requirement, 1988, 0.6).
narrative_ontology:measurement(equa_su_t1998, equal_protection_kernel__remedial_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(equa_su_t2008, equal_protection_kernel__remedial_reading, suppression_requirement, 2008, 0.78).
narrative_ontology:measurement(equa_su_t2018, equal_protection_kernel__remedial_reading, suppression_requirement, 2018, 0.82).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__remedial_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__remedial_reading, equal_protection_kernel__antisubordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Each reading has a different structural interpretation of permissible state action regarding race, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
