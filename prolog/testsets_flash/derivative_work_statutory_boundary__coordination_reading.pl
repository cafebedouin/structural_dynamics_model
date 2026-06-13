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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Work Statutory Boundary (Coordination Reading)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents a 'coordination reading' of the derivative
 *   work statutory boundary, where only fixed recastings substantially
 *   incorporating original expression are considered derivative works, and
 *   transformative or intermediate uses (like AI training) are
 *   non-infringing. This interpretation aims to foster innovation and
 *   cultural production by minimizing ex-ante licensing requirements for new
 *   creations that build upon existing works. It is a specific reading of the
 *   broader 'derivative_work_statutory_boundary' kernel, emphasizing
 *   coordination over enclosure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.15).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.2).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Work Statutory Boundary (Coordination Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "intellectual_property_law/technology_governance/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, 'ef676143-ebfc-42cb-84fa-d55ab8585532').
narrative_ontology:cs_kernel_codification('ef676143-ebfc-42cb-84fa-d55ab8585532', fixed_text).
narrative_ontology:cs_authority_grounding('ef676143-ebfc-42cb-84fa-d55ab8585532', lineage).
narrative_ontology:cs_interpretation_layer_present('ef676143-ebfc-42cb-84fa-d55ab8585532').
narrative_ontology:cs_reading_relation('ef676143-ebfc-42cb-84fa-d55ab8585532', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef676143-ebfc-42cb-84fa-d55ab8585532', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('ef676143-ebfc-42cb-84fa-d55ab8585532', foundational, transformative_use_is_non_infringing).
narrative_ontology:cs_axiom_status(transformative_use_is_non_infringing, holdable).
narrative_ontology:cs_axiom_grounding('ef676143-ebfc-42cb-84fa-d55ab8585532', transformative_use_is_non_infringing, conventional).
narrative_ontology:cs_axiom('ef676143-ebfc-42cb-84fa-d55ab8585532', secondary, copyright_promotes_progress).
narrative_ontology:cs_axiom_status(copyright_promotes_progress, holdable).
narrative_ontology:cs_axiom_grounding('ef676143-ebfc-42cb-84fa-d55ab8585532', copyright_promotes_progress, instrumental).
narrative_ontology:cs_reference_frame('ef676143-ebfc-42cb-84fa-d55ab8585532', original_copyright_balance).
narrative_ontology:cs_drift_state('ef676143-ebfc-42cb-84fa-d55ab8585532', contemporary_generative_ai_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ef676143-ebfc-42cb-84fa-d55ab8585532', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, public_domain_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Artists, writers, and musicians who build upon existing works in new and transformative ways, without needing to license every prior use. They benefit from a clear boundary that allows creative freedom.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, mobile, global).

% Companies and researchers developing generative AI models. This reading allows them to train models on vast datasets of copyrighted material without incurring ex-ante licensing costs, treating training as an intermediate, non-infringing use.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ai_developers, beneficiary,
    institutional, generational, mobile, global).

% Authors, publishers, and studios who hold copyrights on original works. While they retain rights over direct copies and adaptations, this reading limits their control over transformative uses, potentially reducing their revenue from secondary markets.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, original_copyright_holders, payer,
    powerful, generational, constrained, global).

% The general public and future creators who benefit from a rich public domain and the ability to freely build upon works once their copyright expires or if their use is deemed transformative. This reading expands the effective public domain by narrowing the scope of derivative works.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, public_domain_users, beneficiary,
    powerless, civilizational, arbitrage, universal).

% Academics and legal experts who analyze the impact of copyright law on innovation and cultural production. They observe how this interpretation fosters creativity and balances rights.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates creative and technological innovation by providing clear boundaries for what constitutes a 'derivative work,' reducing legal uncertainty for creators and developers who build upon existing content, and fostering a vibrant ecosystem of new expression.
% TRANSFER_FUNCTION: Facilitates the free flow of information and creative inspiration, allowing transformative uses without requiring direct financial transfer to original copyright holders for such uses. It implicitly transfers value from potential licensing fees for transformative uses to the broader creative and technological commons.
% ABSENT_VOICES: Those who advocate for maximalist copyright protection, viewing any use of copyrighted material as requiring authorization, would object. They are often represented by industry groups whose business models rely on broad control over derivative rights.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal landscape for generative AI, fan fiction, parody, and other transformative works would become highly uncertain. Creators would face increased litigation risk, potentially stifling innovation and leading to a more permission-based culture, fundamentally altering how new works are created and distributed.
% FOUNDING_PROBLEM: The original problem was to balance the rights of creators to profit from their work with the public's interest in accessing and building upon existing knowledge and culture, preventing copyright from becoming a perpetual monopoly on ideas.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and technology policy experts widely corroborate that balancing creator rights with public access and innovation remains a live and evolving problem, especially with the advent of new technologies like generative AI. Courts continue to grapple with these boundaries, indicating the problem is far from settled.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).

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
 *   The extractiveness is low (0.15) because this reading minimizes the 'tax' on transformative creativity, allowing new works to emerge with fewer gatekeepers. Suppression is also low (0.2) as it actively resists attempts to broaden copyright control over non-literal or intermediate uses. Theater ratio is negligible (0.05) as the constraint's function is genuinely to coordinate creative activity, not to mask extraction. The metrics reflect a system designed to facilitate, rather than restrict, new forms of expression.
 *
 * PERSPECTIVAL GAP:
 *   Original copyright holders experience this as a cost (reduced control and potential revenue from secondary uses), while transformative creators and AI developers experience it as a benefit (freedom to innovate). The legal system, as an agenda-setter, aims to balance these interests, but this reading explicitly favors the latter, leading to a divergence in perceived fairness and utility.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative creators, AI developers, and public domain users are clear beneficiaries (d near 0.0) as this reading expands their scope of permissible activity. Original copyright holders are the primary payers (d near 1.0) as their control over certain secondary uses is curtailed. The constraint subsidizes innovation by limiting the reach of existing rights.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy by ensuring the 'founding problem' of balancing access and incentive remains live and responsive to new technologies. It actively resists the drift towards an 'enclosure reading' that would expand copyright beyond its original intent, thus preventing the constraint from becoming a snare for innovation. The low extractiveness and suppression indicate it is not a zombie constraint serving only to extract rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly a ''coordination reading'' of the derivative work boundary, or is it merely a policy preference dressed as a structural interpretation?',
    'Analysis of judicial precedent and legislative intent over time: if the interpretation consistently prioritizes public benefit and innovation over maximalist control, it supports the coordination reading. If it shifts to favor specific industry interests, it leans towards policy preference.',
    'If confirmed as a genuine coordination reading, it strengthens the argument for a Rope classification. If revealed as a policy preference, it might indicate a Tangled Rope or Snare, depending on who benefits from the preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between structural interpretation and policy preference for derivative work boundary.').

omega_variable(
    enclosure_reading_impact,
    'What would be the precise economic and creative impact if the ''enclosure_reading'' of the derivative work boundary were to prevail?',
    'Economic modeling of licensing markets for AI training data and transformative works, combined with qualitative studies of creator behavior under different legal regimes.',
    'If the enclosure reading leads to significant market failures, reduced innovation, and stifled creativity, it would highlight the coordination benefits of this reading. If it leads to a robust, fair licensing market, it would challenge the premise of this reading''s necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enclosure_reading_impact, empirical, 'Impact of alternative ''enclosure'' reading on innovation and markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t1976, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 1976, 0.03).
narrative_ontology:measurement(deri_tr_t1990, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 1990, 0.04).
narrative_ontology:measurement(deri_tr_t2005, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2005, 0.04).
narrative_ontology:measurement(deri_tr_t2015, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(deri_tr_t2024, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(deri_be_t1976, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 1976, 0.1).
narrative_ontology:measurement(deri_be_t1990, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(deri_be_t2005, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2005, 0.13).
narrative_ontology:measurement(deri_be_t2015, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2015, 0.14).
narrative_ontology:measurement(deri_be_t2024, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t1976, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 1976, 0.15).
narrative_ontology:measurement(deri_su_t1990, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 1990, 0.17).
narrative_ontology:measurement(deri_su_t2005, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2005, 0.18).
narrative_ontology:measurement(deri_su_t2015, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2015, 0.19).
narrative_ontology:measurement(deri_su_t2024, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, information_standard).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, copyright_term_extension).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, ai_liability_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'derivative_work_statutory_boundary' kernel. This 'coordination_reading' emphasizes open access for transformative and intermediate uses. The 'enclosure_reading' (a Snare) asserts broad control for original copyright holders. The 'hybrid_carveout_reading' (a Tangled Rope) attempts to differentiate by commercial vs. non-commercial use. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
