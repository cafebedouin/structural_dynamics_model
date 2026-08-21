% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Scope (Extraction Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint describes the HOA covenant system as primarily a revenue
 *   generation mechanism and a tool for board power consolidation, achieved
 *   through the proliferation of fines and selective enforcement. It is one
 *   reading of the broader 'hoa_covenant_scope' kernel. The high
 *   extractiveness and suppression reflect the punitive nature of
 *   enforcement, targeting financially vulnerable homeowners and generating
 *   revenue for the board and associated firms, rather than primarily
 *   coordinating community benefits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.68).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.75).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Scope (Extraction Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '3c5f7f99-4245-434f-a40c-9122a062d569').
narrative_ontology:cs_kernel_codification('3c5f7f99-4245-434f-a40c-9122a062d569', formalized).
narrative_ontology:cs_authority_grounding('3c5f7f99-4245-434f-a40c-9122a062d569', extraction).
narrative_ontology:cs_interpretation_layer_present('3c5f7f99-4245-434f-a40c-9122a062d569').
narrative_ontology:cs_reading_relation('3c5f7f99-4245-434f-a40c-9122a062d569', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c5f7f99-4245-434f-a40c-9122a062d569', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('3c5f7f99-4245-434f-a40c-9122a062d569', foundational, covenants_as_revenue_source).
narrative_ontology:cs_axiom_status(covenants_as_revenue_source, holdable).
narrative_ontology:cs_axiom_grounding('3c5f7f99-4245-434f-a40c-9122a062d569', covenants_as_revenue_source, empirically_contingent).
narrative_ontology:cs_axiom('3c5f7f99-4245-434f-a40c-9122a062d569', foundational, board_power_consolidation_via_enforcement).
narrative_ontology:cs_axiom_status(board_power_consolidation_via_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('3c5f7f99-4245-434f-a40c-9122a062d569', board_power_consolidation_via_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('3c5f7f99-4245-434f-a40c-9122a062d569', unfettered_board_discretion).
narrative_ontology:cs_drift_state('3c5f7f99-4245-434f-a40c-9122a062d569', contemporary_legal_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3c5f7f99-4245-434f-a40c-9122a062d569', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, all_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and enforce covenants, often selectively, to generate revenue through fines and consolidate their power within the community. They benefit from the fees paid to property management and legal firms, and from the deference of homeowners seeking to avoid fines.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_board_members, agenda_setter,
    institutional, biographical, constrained, local).

% Contracted by HOA boards to enforce covenants, often receiving a percentage of collected fines or flat fees for violation notices. They have a direct financial incentive to identify and process violations, contributing to fine proliferation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, mobile, regional).

% Retained by HOAs to pursue unpaid fines, place liens on properties, and handle foreclosure proceedings. Their fees, often passed directly to the homeowner in violation, represent a significant portion of the extraction mechanism.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_legal_counsel, beneficiary,
    organized, biographical, mobile, local).

% Bear the brunt of fine proliferation and aggressive enforcement. They often lack the resources to contest fines, leading to escalating penalties, liens, and potential foreclosure, making them primary victims of the extraction.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Indirectly pay for HOA fines and fees through increased rent or reduced property maintenance by landlords seeking to avoid their own costs. They have no direct voice in HOA governance.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% All homeowners are subject to the covenants and the potential for fines, though enforcement is often selective. They collectively bear the costs of legal actions and management fees, even if not directly targeted.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, all_homeowners, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The covenant system nominally coordinates community standards for property maintenance and behavior, aiming to preserve property values and shared amenities.
% TRANSFER_FUNCTION: Transfers financial resources (fines, legal fees, management fees) from homeowners (especially those in violation) to the HOA board, property management firms, and legal counsel.
% ABSENT_VOICES: Homeowners who have lost their homes due to escalating HOA fines and liens are absent from the current discourse, as are potential residents who are deterred by the punitive nature of the covenants. Their experiences would highlight the coercive aspects of the system.
% DISAPPEARANCE_RATIONALE: If the covenant's extractive enforcement mechanisms vanished, the revenue streams for the board, management, and legal firms would collapse. Homeowners would no longer face punitive fines, and the power dynamics within the community would shift dramatically, leading to a reorganization of governance and financial flows.
% FOUNDING_PROBLEM: HOA covenants were established to ensure collective maintenance of shared property, resolve neighbor disputes, and maintain community standards to protect property values.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and associated firms claim the founding problems (e.g., property value protection, dispute resolution) are still live. However, financially vulnerable homeowners and housing advocates attest that the original problems are largely solved or have been superseded by the revenue-generation function, with independent legal analyses often supporting this view.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the fines and associated legal/management fees are disproportionate to the actual harm of the violations, serving as a significant revenue stream. Suppression (0.75) is high due to the legal mechanisms (liens, foreclosure) that trap homeowners, especially those with limited financial resources, into compliance or loss of property. The theater ratio (0.40) indicates that while some genuine coordination (e.g., basic maintenance) may occur, a substantial portion of enforcement activity is performative, designed to assert authority and generate revenue rather than solve collective action problems.
 *
 * PERSPECTIVAL GAP:
 *   The HOA board and associated firms would likely frame this as a necessary coordination mechanism for community upkeep and property value protection (the 'coordination_reading'). However, from the perspective of financially vulnerable homeowners, it operates as a highly extractive and suppressive system, leveraging minor infractions for revenue and power consolidation. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   HOA board members, property management firms, and legal counsel are clear beneficiaries, directly profiting from the fine system and its enforcement. Financially vulnerable homeowners and renters (via pass-through costs) are the primary victims, bearing the direct financial burden and facing severe consequences for non-compliance. Other homeowners are payers, but with more options than the vulnerable group.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revenue_vs_cost_justification,
    'To what extent do HOA fine revenues and associated fees genuinely cover the costs of community maintenance and administration, versus generating surplus for the board and associated firms?',
    'Independent forensic audit of HOA finances, comparing fine revenue and legal/management fees against actual maintenance and administrative expenditures.',
    'If a significant surplus is found, it strengthens the ''extraction_reading'' and supports reclassification towards a Snare. If costs are closely matched, it would lend more credence to a ''coordination_reading'' or ''behavioral_control_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_vs_cost_justification, empirical, 'Determines if fine revenue is cost-justified or rent-seeking.').

omega_variable(
    selective_enforcement_bias,
    'Is covenant enforcement applied uniformly across all homeowners, or is it selectively targeted based on socioeconomic status, perceived compliance, or other non-rule-based criteria?',
    'Statistical analysis of violation notices and fines issued, correlated with homeowner demographics and property characteristics, controlling for actual violation rates.',
    'Evidence of selective enforcement would strongly support the ''extraction_reading'' by demonstrating the use of covenants as a tool for power consolidation and targeting, rather than universal rule application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_bias, empirical, 'Assesses fairness and targeting in covenant enforcement.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the HOA covenant system fundamentally a coordination mechanism, a behavioral control mechanism, or an extraction mechanism?',
    'This is a conceptual omega. Resolution depends on which normative framework (e.g., property rights, collective action theory, critical legal studies) is applied to interpret the empirical evidence of its operation.',
    'The choice of framing dictates the primary classification. Adopting the ''coordination_reading'' would emphasize collective benefits; ''behavioral_control_reading'' would emphasize aesthetic and social conformity; ''extraction_reading'' emphasizes rent-seeking and power dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the primary function of HOA covenants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'extraction_reading' of the 'hoa_covenant_scope' kernel, focusing on revenue generation and power consolidation. It coexists with 'coordination_reading' and 'behavioral_control_reading', which emphasize different aspects of the same covenant system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
