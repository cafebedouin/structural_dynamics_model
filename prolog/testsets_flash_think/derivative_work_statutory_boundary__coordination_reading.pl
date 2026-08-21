% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Statutory Boundary: Coordination Reading (Transformative Use Permitted)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'coordination_reading' of the
 *   derivative work statutory boundary. This reading interprets copyright law
 *   to mean that only fixed recastings substantially incorporating original
 *   expression are derivative works, while transformative and intermediate
 *   uses (such as AI training) are generally non-infringing. It functions as
 *   a coordination scaffold, providing temporary legal clarity to foster
 *   innovation in generative technologies and other transformative fields,
 *   pending potential legislative updates. The low extractiveness and
 *   suppression reflect its intent to minimize friction for new creation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.25).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.2).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, scaffold).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Statutory Boundary: Coordination Reading (Transformative Use Permitted)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:has_sunset_clause(derivative_work_statutory_boundary__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '43616354-8ad6-45ac-88bb-d3d05401714f').
narrative_ontology:cs_kernel_codification('43616354-8ad6-45ac-88bb-d3d05401714f', fixed_text).
narrative_ontology:cs_authority_grounding('43616354-8ad6-45ac-88bb-d3d05401714f', lineage).
narrative_ontology:cs_interpretation_layer_present('43616354-8ad6-45ac-88bb-d3d05401714f').
narrative_ontology:cs_reading_relation('43616354-8ad6-45ac-88bb-d3d05401714f', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('43616354-8ad6-45ac-88bb-d3d05401714f', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('43616354-8ad6-45ac-88bb-d3d05401714f', foundational, transformative_use_promotes_progress).
narrative_ontology:cs_axiom_status(transformative_use_promotes_progress, holdable).
narrative_ontology:cs_axiom_grounding('43616354-8ad6-45ac-88bb-d3d05401714f', transformative_use_promotes_progress, instrumental).
narrative_ontology:cs_axiom('43616354-8ad6-45ac-88bb-d3d05401714f', secondary, intermediate_copying_is_functional_not_expressive).
narrative_ontology:cs_axiom_status(intermediate_copying_is_functional_not_expressive, holdable).
narrative_ontology:cs_axiom_grounding('43616354-8ad6-45ac-88bb-d3d05401714f', intermediate_copying_is_functional_not_expressive, empirically_contingent).
narrative_ontology:cs_reference_frame('43616354-8ad6-45ac-88bb-d3d05401714f', fair_use_balancing_framework).
narrative_ontology:cs_drift_state('43616354-8ad6-45ac-88bb-d3d05401714f', contemporary_generative_ai_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('43616354-8ad6-45ac-88bb-d3d05401714f', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, researchers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, public_domain_users).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_content_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, traditional_copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the clarity that their models' training and output are largely non-infringing, reducing legal risk and fostering innovation. This interpretation allows them to build on vast datasets without prohibitive licensing costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers, beneficiary,
    powerful, biographical, mobile, global).

% Relies on the ability to analyze and transform existing works for academic and scientific purposes without facing copyright infringement claims, enabling progress in various fields.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, researchers, beneficiary,
    moderate, biographical, mobile, global).

% Benefits from a broader understanding of non-infringing use, which effectively expands the 'public domain' of ideas and expressions available for creative reuse and transformation, even if the original work is copyrighted.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, public_domain_users, beneficiary,
    powerless, generational, mobile, universal).

% Gains legal protection for creating new works that substantially alter the original's purpose, character, or expression, fostering artistic freedom and cultural commentary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_artists, beneficiary,
    moderate, biographical, mobile, local).

% Perceives a loss of control and potential revenue from uses of their work that are deemed non-infringing under this interpretation, particularly when their work is used in AI training or highly transformative new creations.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_content_creators, payer,
    moderate, biographical, constrained, global).

% Bears the cost of reduced licensing opportunities and the perceived erosion of their exclusive rights, leading to advocacy for stricter derivative work definitions and increased litigation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, traditional_copyright_holders, payer,
    powerful, generational, constrained, global).

% Responsible for interpreting and applying copyright statutes, shaping the legal boundary of derivative works through case law. This reading reflects a judicial tendency to favor transformative use.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Holds the power to codify or alter the statutory definition of derivative works, potentially clarifying or changing the legal landscape for transformative and intermediate uses, especially concerning new technologies like AI.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, legislators, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear legal framework that coordinates innovation by allowing creators and developers to build upon existing works through transformative and intermediate uses without requiring ex-ante licensing, thereby reducing transaction costs and legal uncertainty.
% TRANSFER_FUNCTION: Minimizes the transfer of licensing fees and control from transformative users (e.g., AI developers, artists) to original copyright holders, facilitating the free flow of information and fostering new creative and technological development.
% ABSENT_VOICES: While traditional copyright holders are present in the debate, those advocating for an 'enclosure' reading (maximalist copyright enforcement) are structurally excluded from this interpretation's core premise, as their view is directly contradicted by the allowance of broad transformative use.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal landscape for generative AI, research, and transformative art would become highly uncertain. Innovation would be stifled by fear of litigation, leading to a chilling effect on new technologies and creative expression, and a scramble for new licensing models or legislative intervention.
% FOUNDING_PROBLEM: To balance the exclusive rights of copyright holders with the public interest in promoting new creativity, scholarship, and technological innovation, especially in the face of new technologies that enable novel forms of reuse and transformation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, technology industry associations, and public interest groups consistently corroborate the ongoing nature of this balancing act, often citing the need for this specific interpretation to maintain a dynamic equilibrium between protection and progress. Legislative hearings and amicus briefs from diverse parties also attest to this persistent tension.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.25) reflects that this interpretation minimizes direct financial transfers from transformative users to original copyright holders, treating many uses as non-infringing. Suppression (0.20) is low because it actively enables new forms of creation rather than restricting them. The theater ratio (0.10) is low, indicating that the legal framework is highly functional in its stated purpose of fostering innovation. As a 'scaffold', it is understood to be a transitional legal interpretation, likely to be revisited or codified by new legislation, hence the 'has_sunset_clause: true'. Resistance (0.30) is present from traditional copyright holders who advocate for broader derivative work definitions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this constraint is a vital enabler of innovation and creativity, a true 'scaffold'. From the perspective of payers, it represents an erosion of their rights and a failure to adequately compensate original creators. The engine's classification will highlight this divergence, showing a beneficial outcome for innovators and a cost for traditional rights holders.
 *
 * DIRECTIONALITY LOGIC:
 *   Generative AI developers, researchers, public domain users, and transformative artists are clear beneficiaries, as this reading reduces their legal burden and enables their work. Original content creators and traditional copyright holders are payers, as they perceive a loss of control and potential revenue from uses deemed non-infringing. Courts act as agenda-setters through their interpretations, while legislators have the ultimate power to codify or alter this boundary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_intent_ambiguity,
    'Does the original statutory language for ''derivative work'' genuinely support this coordination-focused interpretation, or is it a judicial adaptation to new technologies?',
    'Historical legal analysis of legislative intent, comparative analysis with other jurisdictions'' copyright statutes, and expert legal consensus on textual interpretation.',
    'If it''s a clear adaptation, it highlights the judiciary''s role in evolving law, but also its potential vulnerability to legislative override. If it''s strongly supported by original intent, it strengthens the reading''s legitimacy against challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_intent_ambiguity, conceptual, 'Ambiguity regarding the original legislative intent behind derivative work definitions.').

omega_variable(
    economic_impact_of_non_infringement,
    'What is the actual economic impact of this interpretation on original content creators and traditional copyright holders, in terms of lost revenue versus new market creation?',
    'Empirical economic studies analyzing revenue streams, licensing markets, and the growth of new industries enabled by transformative use, comparing outcomes across different legal regimes.',
    'If the net economic impact is significantly negative for original creators, it strengthens arguments for legislative intervention or alternative compensation mechanisms. If net positive (due to new markets), it reinforces the coordination reading''s economic justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_of_non_infringement, empirical, 'Uncertainty about the net economic effects of broad transformative use.').

omega_variable(
    sunset_clause_trigger,
    'What specific conditions or legislative actions would trigger the ''sunset'' of this scaffold-like interpretation, leading to a new, more permanent legal framework?',
    'Analysis of legislative proposals, judicial signals, and industry consensus on the need for new, AI-specific copyright law. The sunset is not a fixed date but a functional trigger.',
    'Clarity on the sunset conditions would reduce uncertainty for all stakeholders. Lack of clarity prolongs the ''scaffold'' phase, potentially leading to increased litigation or calls for more drastic legislative action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_trigger, preference, 'Ambiguity regarding the conditions for transitioning from this temporary legal interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t2010, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2010, 0.08).
narrative_ontology:measurement(deri_tr_t2015, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2015, 0.09).
narrative_ontology:measurement(deri_tr_t2020, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(deri_tr_t2025, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(deri_be_t2010, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2010, 0.2).
narrative_ontology:measurement(deri_be_t2015, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2015, 0.22).
narrative_ontology:measurement(deri_be_t2020, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2020, 0.24).
narrative_ontology:measurement(deri_be_t2025, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2025, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t2010, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(deri_su_t2015, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2015, 0.19).
narrative_ontology:measurement(deri_su_t2020, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2020, 0.2).
narrative_ontology:measurement(deri_su_t2025, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, generative_ai_development_funding).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, open_source_licensing_practices).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, copyright_fair_use_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'derivative_work_statutory_boundary' kernel. This 'coordination_reading' emphasizes enabling transformative use, contrasting with the 'enclosure_reading' (maximalist control) and the 'hybrid_carveout_reading' (commercial/non-commercial distinction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
