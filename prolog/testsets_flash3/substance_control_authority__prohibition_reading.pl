% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Prohibitionist Drug Control Authority (Third-Party Protection)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.9).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Prohibitionist Drug Control Authority (Third-Party Protection)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '8d49e18f-c68b-4896-aac2-f7610d6c02da').
narrative_ontology:cs_kernel_codification('8d49e18f-c68b-4896-aac2-f7610d6c02da', formalized).
narrative_ontology:cs_authority_grounding('8d49e18f-c68b-4896-aac2-f7610d6c02da', lineage).
narrative_ontology:cs_interpretation_layer_present('8d49e18f-c68b-4896-aac2-f7610d6c02da').
narrative_ontology:cs_reading_relation('8d49e18f-c68b-4896-aac2-f7610d6c02da', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d49e18f-c68b-4896-aac2-f7610d6c02da', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('8d49e18f-c68b-4896-aac2-f7610d6c02da', foundational, drug_use_is_inherently_criminal).
narrative_ontology:cs_axiom_status(drug_use_is_inherently_criminal, holdable).
narrative_ontology:cs_axiom_grounding('8d49e18f-c68b-4896-aac2-f7610d6c02da', drug_use_is_inherently_criminal, deontological).
narrative_ontology:cs_axiom('8d49e18f-c68b-4896-aac2-f7610d6c02da', foundational, deterrence_through_punishment_is_effective).
narrative_ontology:cs_axiom_status(deterrence_through_punishment_is_effective, holdable).
narrative_ontology:cs_axiom_grounding('8d49e18f-c68b-4896-aac2-f7610d6c02da', deterrence_through_punishment_is_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('8d49e18f-c68b-4896-aac2-f7610d6c02da', punitive_deterrence_framework).
narrative_ontology:cs_drift_state('8d49e18f-c68b-4896-aac2-f7610d6c02da', contemporary_evidence_based_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8d49e18f-c68b-4896-aac2-f7610d6c02da', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, private_prison_industry).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_citizens).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, marginalized_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and maintains laws criminalizing drug use and possession, framing them as necessary for public safety and order. Responds to public pressure for 'tough on crime' policies.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Receive funding and expanded powers to enforce drug laws, leading to increased arrests and seizures. Their operational metrics often incentivize continued enforcement regardless of public health outcomes.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter).

% Profits directly from increased incarceration rates driven by drug offenses, lobbying for policies that maintain high prisoner populations. Their business model is directly tied to the persistence of prohibition.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, private_prison_industry, beneficiary,
    organized, generational, arbitrage, national).

% Perceive a reduction in drug-related street crime and disorder, feeling safer in their communities. They support prohibitionist policies based on this perceived benefit, often without considering the costs to other groups.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_citizens, beneficiary,
    organized, biographical, mobile, local).

% Face criminal charges, incarceration, loss of civil liberties, and social stigma. Their health and safety are often worsened by illicit drug markets and lack of access to regulated substances or harm reduction services. Exit from use is made harder by criminalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by drug law enforcement, leading to higher arrest and incarceration rates, family separation, and economic instability. The social fabric of these communities is eroded by the enforcement of prohibition.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, marginalized_communities, payer,
    powerless, generational, identity_locked, local).

% Argue for public health-centered approaches to drug use, focusing on treatment, prevention, and harm reduction. Their voices are often marginalized in policy debates dominated by criminal justice narratives.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social behavior by deterring drug use and associated crime, thereby maintaining public order and safety for non-users.
% TRANSFER_FUNCTION: Transfers resources (taxpayer money) to law enforcement and carceral systems, and transfers social costs (incarceration, stigma, health harms) to drug users and marginalized communities.
% ABSENT_VOICES: Public health experts, drug policy reform advocates, and directly impacted individuals (drug users, families affected by incarceration) are often excluded from the core policy-making process, where they would advocate for evidence-based public health interventions over criminalization.
% DISAPPEARANCE_RATIONALE: If the authority to criminalize drug use vanished, the criminal justice system would undergo massive restructuring, drug markets would shift from illicit to regulated (or unregulated but non-criminalized), and public health systems would need to rapidly scale up services. Social order would be redefined, and the current beneficiaries would lose their rents.
% FOUNDING_PROBLEM: To address perceived social disorder, crime, and public health threats associated with unregulated drug use, particularly in the early 20th century.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement and some citizens attest the problem is still live, citing ongoing crime and disorder. Public health advocates and social scientists, corroborated by empirical studies, attest that the founding problem has either shifted, been exacerbated by prohibition itself, or is better addressed through public health approaches; they argue the current approach is counterproductive.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_effectiveness_empirical,
    'Does criminalizing drug use effectively reduce drug-related crime and social disorder, or does it displace it and create new harms?',
    'Comparative empirical studies of jurisdictions with different drug policies (prohibition vs. harm reduction vs. legalization) on crime rates, public health outcomes, and social disorder metrics.',
    'If prohibition is found to be ineffective or counterproductive, the justification for its high extractiveness and suppression would collapse, reclassifying it closer to a ''snare'' or ''piton'' even from the perspective of third-party protection. If effective, it would strengthen the ''tangled_rope'' or ''rope'' framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prohibition_effectiveness_empirical, empirical, 'Empirical efficacy of prohibition in achieving its stated goals.').

omega_variable(
    racial_disparity_causality,
    'Are the observed racial disparities in drug law enforcement a consequence of the prohibitionist framework itself, or of pre-existing social inequalities?',
    'Causal inference studies controlling for socio-economic factors and comparing enforcement patterns across different policy regimes. Analysis of legislative intent vs. actual impact.',
    'If disparities are inherent to the prohibitionist framework, it strengthens the ''snare'' classification by highlighting systemic victimhood. If primarily due to pre-existing inequalities, the constraint''s role might be seen as amplifying, rather than solely creating, the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_causality, empirical, 'Causal factors behind racial disparities in drug law enforcement.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a distinct reading of ''substance_control_authority'' or merely a policy choice within a broader framework?',
    'Analysis of foundational axioms and their logical compatibility with sibling readings (''harm_reduction_reading'', ''legalization_reading''). If core premises are mutually exclusive, it''s a distinct reading.',
    'If a distinct reading, it highlights the deep conceptual divide in drug policy. If merely a policy choice, it suggests a ''tangled_rope'' where different policy levers are being pulled within a shared, but contested, coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishes this prohibitionist reading as a fundamental interpretation of state authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__prohibition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t10, substance_control_authority__prohibition_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__prohibition_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(subs_tr_t30, substance_control_authority__prohibition_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(subs_tr_t40, substance_control_authority__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(subs_tr_t50, substance_control_authority__prohibition_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__prohibition_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(subs_be_t10, substance_control_authority__prohibition_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__prohibition_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(subs_be_t30, substance_control_authority__prohibition_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(subs_be_t40, substance_control_authority__prohibition_reading, base_extractiveness, 40, 0.85).
narrative_ontology:measurement(subs_be_t50, substance_control_authority__prohibition_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__prohibition_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(subs_su_t10, substance_control_authority__prohibition_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__prohibition_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(subs_su_t30, substance_control_authority__prohibition_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(subs_su_t40, substance_control_authority__prohibition_reading, suppression_requirement, 40, 0.9).
narrative_ontology:measurement(subs_su_t50, substance_control_authority__prohibition_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, prison_industrial_complex_funding).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, public_health_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
