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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Boundary for Transformative Use (Coordination Reading)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'coordination_reading' of the derivative
 *   work statutory boundary, emphasizing that only fixed recastings
 *   substantially incorporating original expression are derivative works,
 *   thereby making transformative and intermediate uses non-infringing. This
 *   interpretation aims to foster innovation by reducing legal friction for
 *   new technologies like generative AI. It stands in contrast to the
 *   'enclosure_reading' (which advocates for a broader derivative work
 *   definition) and the 'hybrid_carveout_reading' (which distinguishes based
 *   on commercial exploitation). The low extractiveness and suppression
 *   reflect its function as a facilitative legal framework.
 *
 * KEY AGENTS:
 *   - generative_ai_developers: Primary beneficiary (organized/mobile) — benefits from reduced legal risk.
 *   - researchers: Secondary beneficiary (moderate/mobile) — benefits from clear use rights for data analysis.
 *   - copyright_holders: Payer (powerful/constrained) — bears the cost of reduced control over certain uses.
 *   - courts_and_legislatures: Agenda setter (institutional/analytical) — interprets and potentially codifies this boundary.
 *   - enclosure_advocates: Excluded (organized/trapped) — their maximalist view is not adopted here.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.15).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.2).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Boundary for Transformative Use (Coordination Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, 'e2f026bb-1c7c-461a-a0c7-4f150c73a80f').
narrative_ontology:cs_kernel_codification('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', formalized).
narrative_ontology:cs_authority_grounding('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', lineage).
narrative_ontology:cs_interpretation_layer_present('e2f026bb-1c7c-461a-a0c7-4f150c73a80f').
narrative_ontology:cs_reading_relation('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', foundational, transformative_use_is_non_derivative).
narrative_ontology:cs_axiom_status(transformative_use_is_non_derivative, holdable).
narrative_ontology:cs_axiom_grounding('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', transformative_use_is_non_derivative, conventional).
narrative_ontology:cs_axiom('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', foundational, innovation_requires_unfettered_intermediate_use).
narrative_ontology:cs_axiom_status(innovation_requires_unfettered_intermediate_use, holdable).
narrative_ontology:cs_axiom_grounding('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', innovation_requires_unfettered_intermediate_use, instrumental).
narrative_ontology:cs_reference_frame('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', innovation_friendly_copyright_balance).
narrative_ontology:cs_drift_state('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', contemporary_generative_ai_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e2f026bb-1c7c-461a-a0c7-4f150c73a80f', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, researchers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, public_domain_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, copyright_holders).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, innovation_incentive_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from clear legal boundaries that permit the use of copyrighted material for training AI models and other transformative purposes without requiring ex-ante licensing, reducing legal risk and development costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, generative_ai_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefits from the ability to use copyrighted works for data analysis, text and data mining, and other intermediate uses without fear of infringement claims, fostering academic and scientific progress.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, researchers, beneficiary,
    moderate, biographical, mobile, global).

% Supports interpretations that limit the scope of derivative works, thereby expanding the effective public domain and promoting broader access to and reuse of cultural and informational assets.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, public_domain_advocates, beneficiary,
    organized, generational, constrained, global).

% Bears the cost of reduced control over certain uses of their copyrighted works, particularly transformative and intermediate uses, which are deemed non-infringing. This may lead to a perceived loss of potential licensing revenue.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, copyright_holders, payer,
    powerful, biographical, constrained, global).

% Responsible for interpreting and enforcing copyright law, and for potentially codifying this reading into statute. They aim to balance competing interests to foster innovation and creativity.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, courts_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Represents interests that advocate for a broader definition of derivative works, requiring authorization for almost any use of copyrighted expression in new creations. Their perspective is not adopted in this reading.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, enclosure_advocates, excluded,
    organized, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies the legal boundary of derivative works, reducing uncertainty for innovators and creators in fields like generative AI and data analysis, thereby coordinating efforts towards new technological and artistic development by defining permissible uses.
% TRANSFER_FUNCTION: Transfers the burden of ex-ante licensing and potential infringement liability from transformative users (e.g., AI developers, researchers) to copyright holders, who must accept a broader scope of non-infringing uses.
% ABSENT_VOICES: Advocates for maximalist copyright protection and ex-ante licensing for all uses of copyrighted material, particularly those who believe any use of their work in new creations should require permission and compensation. These voices are represented by 'enclosure_advocates' and some 'copyright_holders' who would argue for a more restrictive interpretation.
% DISAPPEARANCE_RATIONALE: If this clear boundary vanished, legal uncertainty would significantly increase, stifling innovation in generative AI, data analysis, and other fields reliant on intermediate and transformative uses. The information economy would become more litigious and fragmented, as creators would face prohibitive legal risks and costs.
% FOUNDING_PROBLEM: The challenge of adapting copyright law to new technologies (like digital sampling, data mining, and generative AI) that create new works by transforming or analyzing existing copyrighted material, without clear guidance on what constitutes a 'derivative work' requiring authorization.
% FOUNDING_PROBLEM_CORROBORATION: Technology ethicists, innovation economists, and open-source legal scholars consistently highlight the ongoing tension between copyright protection and technological progress, corroborating the need for clear, innovation-friendly boundaries. Legislative hearings and academic papers from independent experts also support this view.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15, decreasing) because this reading explicitly limits the scope of copyright, reducing the ability of copyright holders to extract rents from transformative uses. Suppression is low (0.2, decreasing) as it aims to enable, rather than restrict, new forms of creativity and technology. Theater ratio is also low (0.1, decreasing) because the legal framework is functional in clarifying boundaries, with minimal performative aspects. The decreasing trends reflect a growing clarity and acceptance of this interpretation over time, further reducing friction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of generative AI developers and researchers, this constraint is a clear Rope, enabling innovation and reducing legal overhead. From the perspective of some copyright holders and enclosure advocates, it represents a loss of control and potential revenue, though this reading frames that as a necessary rebalancing for overall societal benefit, not as extraction from victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Generative AI developers, researchers, and public domain advocates are clear beneficiaries, as the constraint directly enables their activities and reduces costs (low d). Copyright holders are payers, as they cede some control and potential revenue, but are not 'victims' in this Rope reading, as the rebalancing is considered a legitimate coordination cost (d near symmetric). Courts and legislatures act as agenda setters, shaping the legal environment (d near symmetric/beneficiary for the system as a whole).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine coordination (enabling innovation) as extraction by clearly defining non-infringing uses. It addresses the founding problem of balancing copyright with innovation by prioritizing the latter in specific contexts, ensuring the constraint's mandate remains live and relevant to technological progress. The low extractiveness and suppression indicate it is not a Snare or Tangled Rope in this interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformative_use_definition_ambiguity,
    'How precisely can ''transformative use'' and ''substantially incorporating original expression'' be defined in practice, especially with rapidly evolving generative AI technologies?',
    'Further judicial precedent or legislative clarification providing specific examples and criteria for different technological contexts.',
    'If definitions remain ambiguous, legal uncertainty could persist, increasing transaction costs and potentially leading to a de facto enclosure effect despite the intent of this reading. This would increase effective extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_definition_ambiguity, empirical, 'Ambiguity in key legal terms for derivative works.').

omega_variable(
    kernel_reading_contest,
    'Is this ''coordination_reading'' truly stable, or will it be eroded by pressures from the ''enclosure_reading'' or ''hybrid_carveout_reading'' in future legal challenges?',
    'Analysis of future court decisions and legislative actions regarding derivative works, particularly in high-stakes cases involving major copyright holders and generative AI companies.',
    'If the ''enclosure_reading'' gains traction, the constraint would shift towards higher extractiveness and suppression, potentially reclassifying as a Tangled Rope or Snare. If the ''hybrid_carveout_reading'' prevails, the constraint''s extractiveness would become conditional on commercial intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The ongoing contest between different interpretations of the derivative work boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t2010, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(deri_tr_t2015, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(deri_tr_t2020, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(deri_tr_t2025, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2025, 0.09).
narrative_ontology:measurement(deri_tr_t2030, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2030, 0.08).

% Extraction over time
narrative_ontology:measurement(deri_be_t2010, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(deri_be_t2015, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2015, 0.16).
narrative_ontology:measurement(deri_be_t2020, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2020, 0.15).
narrative_ontology:measurement(deri_be_t2025, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2025, 0.14).
narrative_ontology:measurement(deri_be_t2030, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2030, 0.13).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t2010, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(deri_su_t2015, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2015, 0.22).
narrative_ontology:measurement(deri_su_t2020, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2020, 0.2).
narrative_ontology:measurement(deri_su_t2025, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2025, 0.18).
narrative_ontology:measurement(deri_su_t2030, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2030, 0.17).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, generative_ai_development_funding).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, data_mining_research_ethics).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'derivative_work_statutory_boundary' kernel, which also includes the 'enclosure_reading' and 'hybrid_carveout_reading'. This reading directly influences the operational environment for generative AI and data research.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
