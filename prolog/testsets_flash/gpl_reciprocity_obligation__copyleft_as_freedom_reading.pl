% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'copyleft as freedom' reading of the GPL's
 *   reciprocity obligation. From this perspective, the license is a mechanism
 *   to guarantee user freedoms by preventing proprietary capture of
 *   open-source software. It ensures that any derivative work distributed
 *   must also be open, creating a 'viral' effect that expands the realm of
 *   free software. The constraint is actively enforced by copyright holders
 *   and the Free Software Foundation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.3).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.7).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'b5fafab0-eb07-4ef0-9959-6159243b53ae').
narrative_ontology:cs_kernel_codification('b5fafab0-eb07-4ef0-9959-6159243b53ae', fixed_text).
narrative_ontology:cs_authority_grounding('b5fafab0-eb07-4ef0-9959-6159243b53ae', lineage).
narrative_ontology:cs_interpretation_layer_present('b5fafab0-eb07-4ef0-9959-6159243b53ae').
narrative_ontology:cs_reading_relation('b5fafab0-eb07-4ef0-9959-6159243b53ae', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5fafab0-eb07-4ef0-9959-6159243b53ae', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('b5fafab0-eb07-4ef0-9959-6159243b53ae', foundational, software_freedom_is_paramount).
narrative_ontology:cs_axiom_status(software_freedom_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b5fafab0-eb07-4ef0-9959-6159243b53ae', software_freedom_is_paramount, deontological).
narrative_ontology:cs_axiom('b5fafab0-eb07-4ef0-9959-6159243b53ae', foundational, proprietary_capture_is_a_threat_to_freedom).
narrative_ontology:cs_axiom_status(proprietary_capture_is_a_threat_to_freedom, holdable).
narrative_ontology:cs_axiom_grounding('b5fafab0-eb07-4ef0-9959-6159243b53ae', proprietary_capture_is_a_threat_to_freedom, empirically_contingent).
narrative_ontology:cs_reference_frame('b5fafab0-eb07-4ef0-9959-6159243b53ae', free_software_movement_principles).
narrative_ontology:cs_drift_state('b5fafab0-eb07-4ef0-9959-6159243b53ae', contemporary_open_source_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b5fafab0-eb07-4ef0-9959-6159243b53ae', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, user_control_over_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and maintains GPL-licensed software, choosing to apply the copyleft terms to ensure their work remains free for users. They actively defend the license against violations.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_developers, agenda_setter,
    organized, generational, mobile, global).

% Receive software under terms that guarantee their freedom to use, study, modify, and distribute it. They benefit from the open nature of the software and the community contributions it fosters.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Wish to incorporate GPL-licensed code into proprietary products without releasing their modifications under compatible terms. They view the reciprocity obligation as a restriction on their business models and intellectual property strategy.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Avoid GPL-licensed software due to its 'viral' nature, preferring more permissive licenses that allow proprietary derivatives. They would advocate for weaker copyleft or alternative licensing models.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_corporations, excluded,
    institutional, generational, arbitrage, global).

% Analyze the legal enforceability and economic impact of copyleft licenses, contributing to the discourse around intellectual property and open source governance.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of software under terms that ensure user freedom and prevent proprietary enclosure of derivative works, fostering a collaborative ecosystem.
% TRANSFER_FUNCTION: Transfers the obligation to share modifications under compatible terms from original developers to anyone who distributes derivative works, ensuring 'freedom' for all downstream users.
% ABSENT_VOICES: Proprietary software corporations and developers who prioritize maximal commercial flexibility are effectively excluded from the GPL ecosystem; they would argue for less restrictive licensing models.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, much of the open-source software currently under GPL would likely be absorbed into proprietary projects without its modifications being returned to the commons, fundamentally altering the open-source ecosystem and user freedoms.
% FOUNDING_PROBLEM: The problem of software becoming proprietary and users losing control over their computing, leading to a loss of freedom and community collaboration.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many open-source communities continue to attest that proprietary capture remains a live threat to user freedom. Legal scholars and industry analysts outside the immediate beneficiary group corroborate the ongoing tension between open and proprietary models.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).
:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because the 'cost' (the obligation to share modifications) is framed as a necessary condition for 'freedom,' benefiting all users. Suppression is high (0.7) because the license actively prevents alternative proprietary licensing models for derivative works, requiring legal enforcement to maintain its 'viral' effect. Theater ratio is low (0.1) as the enforcement directly serves the stated goal of preserving freedom.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of open-source advocates, the GPL is a pure Rope, a coordination mechanism for freedom. From proprietary integrators, it is a Snare, restricting their ability to monetize software. This story instantiates the 'freedom' reading, acknowledging the 'restriction' reading as a sibling perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Open-source developers and downstream users are beneficiaries (d near 0.0) as the constraint directly enables their freedom and collaborative model. Proprietary integrators are targets (d near 1.0) as they bear the 'cost' of the reciprocity obligation, which restricts their business models. Software corporations that avoid GPL are excluded, their alternative licensing preferences suppressed by the GPL's structural design.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_restriction_framing,
    'Is the GPL''s reciprocity obligation primarily a mechanism for user freedom or a restriction on proprietary business models?',
    'Analysis of economic impact on different business models versus the expansion of user rights and access to source code. This is a conceptual distinction, not purely empirical.',
    'If framed primarily as a restriction, the extractiveness and suppression metrics might be re-evaluated upwards, potentially shifting the classification towards a Tangled Rope or Snare from the perspective of proprietary integrators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_vs_restriction_framing, conceptual, 'Ambiguity in framing the GPL''s core function.').

omega_variable(
    enforcement_cost_vs_benefit,
    'What is the true cost of enforcing GPL compliance, and does it outweigh the benefits of expanded software freedom?',
    'Empirical study of legal costs, developer time spent on compliance, and the economic value generated by the free software ecosystem.',
    'If enforcement costs are disproportionately high relative to the benefits, it could suggest a higher theater ratio or a less efficient coordination mechanism, potentially moving it closer to a Piton if the benefits atrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_vs_benefit, empirical, 'Efficiency of GPL enforcement in achieving its stated goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1999, 0.08).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2009, 0.09).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1989, 0.2).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1999, 0.25).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2009, 0.28).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2019, 0.29).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1989, 0.5).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1999, 0.6).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2009, 0.65).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, information_standard).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'GPL reciprocity obligation' kernel, focusing on user freedom. It is linked to sibling readings that emphasize restriction and commons management.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
