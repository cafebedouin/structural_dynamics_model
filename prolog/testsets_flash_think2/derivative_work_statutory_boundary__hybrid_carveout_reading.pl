% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint defines the legal boundary for derivative works,
 *   distinguishing between non-commercial transformative uses (generally
 *   permitted) and commercial uses (requiring authorization). It represents a
 *   'hybrid carveout' reading of copyright law, aiming to balance creator
 *   incentives with public access and innovation. The constraint is claimed
 *   as a Tangled Rope because it serves a genuine coordination function
 *   (clarifying rights) but also involves asymmetric extraction (licensing
 *   fees from commercial developers to copyright holders).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.5).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Commercial/Non-Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, 'd69cc5c2-9247-4d94-881f-146a811f7c6d').
narrative_ontology:cs_kernel_codification('d69cc5c2-9247-4d94-881f-146a811f7c6d', formalized).
narrative_ontology:cs_authority_grounding('d69cc5c2-9247-4d94-881f-146a811f7c6d', lineage).
narrative_ontology:cs_interpretation_layer_present('d69cc5c2-9247-4d94-881f-146a811f7c6d').
narrative_ontology:cs_reading_relation('d69cc5c2-9247-4d94-881f-146a811f7c6d', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('d69cc5c2-9247-4d94-881f-146a811f7c6d', derivative_work_statutory_boundary__coordination_reading, influences).
narrative_ontology:cs_axiom('d69cc5c2-9247-4d94-881f-146a811f7c6d', foundational, commercial_exploitation_requires_authorization).
narrative_ontology:cs_axiom_status(commercial_exploitation_requires_authorization, holdable).
narrative_ontology:cs_axiom_grounding('d69cc5c2-9247-4d94-881f-146a811f7c6d', commercial_exploitation_requires_authorization, conventional).
narrative_ontology:cs_axiom('d69cc5c2-9247-4d94-881f-146a811f7c6d', foundational, non_commercial_transformative_use_is_permitted).
narrative_ontology:cs_axiom_status(non_commercial_transformative_use_is_permitted, holdable).
narrative_ontology:cs_axiom_grounding('d69cc5c2-9247-4d94-881f-146a811f7c6d', non_commercial_transformative_use_is_permitted, conventional).
narrative_ontology:cs_reference_frame('d69cc5c2-9247-4d94-881f-146a811f7c6d', incentive_balance_framework).
narrative_ontology:cs_drift_state('d69cc5c2-9247-4d94-881f-146a811f7c6d', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d69cc5c2-9247-4d94-881f-146a811f7c6d', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, original_content_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to control and monetize commercial uses of their work, while seeing their work contribute to a broader creative commons through non-commercial transformation. They receive licensing fees from commercial developers.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, original_content_creators, beneficiary,
    powerful, generational, mobile, global).

% Administer and enforce copyright, collecting licensing fees from commercial derivative works. They benefit from the revenue stream and the legal framework that protects their exclusive rights in the commercial sphere, while navigating the public relations aspect of non-commercial use.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, beneficiary).

% Must seek authorization and pay licensing fees to copyright holders for their derivative works. Their ability to innovate is constrained by these costs and the need for permission, but they gain legal certainty for their commercial products.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_derivative_developers, payer,
    powerful, biographical, constrained, global).

% Are permitted to create transformative works without commercial intent, fostering a vibrant culture of remix and commentary. They benefit from legal protection for their creative freedom, without incurring licensing costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_transformative_users, beneficiary,
    moderate, biographical, mobile, global).

% Analyze the legal and economic impacts of this boundary, contributing to ongoing debates about intellectual property policy. They observe the practical application and judicial interpretation of the rules.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Interpret and apply the statutory boundary in specific cases, shaping the evolving definition of 'derivative work' and 'transformative use.' Their rulings provide the practical enforcement of the constraint.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the rights of original creators with the public interest in building upon existing works, by differentiating between commercial exploitation (requiring authorization) and non-commercial transformative use (permitted). This provides a clear, albeit complex, framework for innovation and monetization.
% TRANSFER_FUNCTION: Transfers licensing fees and control over commercial exploitation from commercial derivative developers to original content creators/copyright holders. It transfers creative freedom and public access to non-commercial transformative users.
% ABSENT_VOICES: Advocates for a broader 'fair use' doctrine or 'free culture' movement, who might argue that even some commercial transformative uses should be permitted without authorization, are often marginalized in policy debates dominated by established copyright industries.
% DISAPPEARANCE_RATIONALE: If this specific boundary vanished, the legal landscape for creative works would become chaotic. Commercial developers would either face unlimited liability or exploit works without compensation, while non-commercial users might lose their protected space, leading to a complete reorganization of creative industries and digital culture.
% FOUNDING_PROBLEM: How to balance the economic incentives for creators to produce original works with the societal benefit of allowing new creators to build upon and transform existing works, especially as digital technologies made copying and modification easier.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, technology companies, and artist advocacy groups consistently highlight the ongoing tension between creator rights and transformative use, indicating the problem remains central to intellectual property discourse. Judicial decisions also reflect the continuous effort to refine this balance.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.60) due to the licensing costs imposed on commercial derivative developers. Suppression is also moderate (0.50) as commercial uses are actively suppressed without authorization, but non-commercial uses are explicitly protected. Theater ratio is low (0.15) because the enforcement of this boundary is largely functional, with courts actively adjudicating cases. Accessibility collapse is moderate (0.40) as commercial alternatives are constrained by licensing, while non-commercial ones are open. Resistance is moderate (0.40) from commercial entities seeking to reduce licensing burdens.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this constraint is a necessary Rope, providing a framework for monetizing their work and incentivizing creation. From the perspective of commercial derivative developers, it can feel like a Snare, imposing significant costs and barriers to innovation. Non-commercial users experience it as a Rope, enabling their creative activities. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Original content creators and copyright holders are beneficiaries, gaining control and revenue. Non-commercial transformative users are also beneficiaries, gaining creative freedom. Commercial derivative developers are targets/payers, bearing the costs of licensing. Courts act as agenda-setters, interpreting and enforcing the boundary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_vs_derivative_ambiguity,
    'How consistently is ''transformative use'' distinguished from ''derivative work'' in practice, especially for new technologies (e.g., AI-generated content)?',
    'Analysis of judicial decisions and industry licensing practices over time, particularly in emerging technological fields. Clearer legal guidelines or landmark court cases could reduce ambiguity.',
    'If the distinction becomes less clear, the extractiveness and suppression for non-commercial users could increase, shifting the constraint closer to an ''enclosure_reading'' and potentially a Snare for a broader set of creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_vs_derivative_ambiguity, empirical, 'Ambiguity in applying the ''transformative use'' standard.').

omega_variable(
    incentive_balance_efficacy,
    'Does this hybrid boundary effectively balance creator incentives with public access and innovation, or does it disproportionately favor one side?',
    'Longitudinal economic studies on creator output, licensing revenue, and the volume/quality of derivative works (both commercial and non-commercial).',
    'If the balance is found to be ineffective, it could lead to calls for legislative reform, potentially shifting towards a more extractive (enclosure) or more permissive (coordination) framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_balance_efficacy, empirical, 'Effectiveness of the boundary in achieving its stated balance.').

omega_variable(
    reading_contest_resolution,
    'Will the ''hybrid_carveout_reading'' maintain its dominance, or will it be superseded by the ''enclosure_reading'' or ''coordination_reading'' in future legal interpretations?',
    'Analysis of legislative changes, shifts in judicial philosophy, and the outcomes of major intellectual property lawsuits over the next decade.',
    'If the ''enclosure_reading'' gains ground, the constraint would become more extractive and suppressive, potentially reclassifying as a Snare. If the ''coordination_reading'' gains ground, it would become less extractive and suppressive, potentially reclassifying as a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_resolution, conceptual, 'The ongoing contest between different readings of the derivative work boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 1998, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t1998, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(deri_tr_t2003, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2003, 0.11).
narrative_ontology:measurement(deri_tr_t2008, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(deri_tr_t2013, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2013, 0.13).
narrative_ontology:measurement(deri_tr_t2018, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2018, 0.14).
narrative_ontology:measurement(deri_tr_t2023, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(deri_be_t1998, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(deri_be_t2003, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2003, 0.56).
narrative_ontology:measurement(deri_be_t2008, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2008, 0.57).
narrative_ontology:measurement(deri_be_t2013, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2013, 0.58).
narrative_ontology:measurement(deri_be_t2018, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2018, 0.59).
narrative_ontology:measurement(deri_be_t2023, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t1998, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 1998, 0.45).
narrative_ontology:measurement(deri_su_t2003, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2003, 0.46).
narrative_ontology:measurement(deri_su_t2008, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2008, 0.47).
narrative_ontology:measurement(deri_su_t2013, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2013, 0.48).
narrative_ontology:measurement(deri_su_t2018, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2018, 0.49).
narrative_ontology:measurement(deri_su_t2023, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 2023, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
