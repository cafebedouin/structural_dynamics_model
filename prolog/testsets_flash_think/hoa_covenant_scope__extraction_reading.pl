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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Revenue Generation and Power Consolidation
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint describes the operation of an HOA covenant from the
 *   perspective of it being primarily a revenue generation mechanism and a
 *   tool for board power consolidation. While covenants ostensibly serve to
 *   maintain community standards and property values, this reading focuses on
 *   the proliferation of fines, selective enforcement, and the financial
 *   benefits accruing to the HOA board, property management firms, and legal
 *   counsel, often at the expense of financially vulnerable homeowners. This
 *   is one reading of the 'hoa_covenant_scope' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.65).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.75).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Revenue Generation and Power Consolidation").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '04400b66-8f4c-4629-bfdd-688701364e02').
narrative_ontology:cs_kernel_codification('04400b66-8f4c-4629-bfdd-688701364e02', formalized).
narrative_ontology:cs_authority_grounding('04400b66-8f4c-4629-bfdd-688701364e02', extraction).
narrative_ontology:cs_interpretation_layer_present('04400b66-8f4c-4629-bfdd-688701364e02').
narrative_ontology:cs_reading_relation('04400b66-8f4c-4629-bfdd-688701364e02', hoa_covenant_scope__coordination_reading, influences).
narrative_ontology:cs_reading_relation('04400b66-8f4c-4629-bfdd-688701364e02', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('04400b66-8f4c-4629-bfdd-688701364e02', foundational, covenant_as_revenue_source).
narrative_ontology:cs_axiom_status(covenant_as_revenue_source, holdable).
narrative_ontology:cs_axiom_grounding('04400b66-8f4c-4629-bfdd-688701364e02', covenant_as_revenue_source, conventional).
narrative_ontology:cs_axiom('04400b66-8f4c-4629-bfdd-688701364e02', foundational, board_power_consolidation_legitimate).
narrative_ontology:cs_axiom_status(board_power_consolidation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('04400b66-8f4c-4629-bfdd-688701364e02', board_power_consolidation_legitimate, conventional).
narrative_ontology:cs_reference_frame('04400b66-8f4c-4629-bfdd-688701364e02', uncontested_revenue_generation).
narrative_ontology:cs_drift_state('04400b66-8f4c-4629-bfdd-688701364e02', contemporary_homeowner_advocacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('04400b66-8f4c-4629-bfdd-688701364e02', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, other_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, other_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the covenants, benefiting from the power consolidated through fine proliferation and selective enforcement. They often have close ties to property management and legal firms, creating a self-reinforcing system.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_board_members, agenda_setter,
    institutional, generational, constrained, local).

% Receives fees for managing the HOA, which often increase with the complexity of enforcement, fine collection, and legal actions. They have an incentive to support aggressive covenant enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, arbitrage, local).

% Profits from legal services related to covenant enforcement, lien processes, and litigation against homeowners. Their financial interest aligns with the proliferation of fines and disputes.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    organized, biographical, arbitrage, local).

% Bears the direct costs of fines, late fees, and potential legal expenses, often leading to liens or foreclosure. Their limited financial resources make resistance difficult, and their property is tied to the covenant.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Indirectly pays for the costs of covenant enforcement through increased rents or fees passed on by landlords who are themselves subject to HOA rules and fines.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% While potentially benefiting from perceived property value maintenance, they are also subject to fines and the general atmosphere of strict enforcement. They may resist but face significant collective action problems and legal costs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, other_homeowners, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, other_homeowners, beneficiary).

% Receives complaints from homeowners but often has limited direct regulatory power over private HOAs. They can investigate specific abuses but rarely intervene in the fundamental structure of covenant enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, local_government_officials, observer,
    institutional, biographical, analytical, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly, the covenant coordinates community standards and property maintenance, but this reading emphasizes how that function is leveraged for revenue generation and power consolidation.
% TRANSFER_FUNCTION: Transfers financial resources (fines, fees, legal costs) from homeowners and indirectly from renters to the HOA board, property management firms, and legal counsel.
% ABSENT_VOICES: Financially vulnerable homeowners who cannot afford legal representation or sustained resistance, and potential residents deterred by the punitive nature of the covenants, are effectively excluded from shaping the enforcement regime.
% DISAPPEARANCE_RATIONALE: If the covenant's ability to generate revenue through fines and consolidate board power vanished, the HOA's financial model would collapse, the board's influence would diminish, and the relationships with management and legal firms would fundamentally alter. The community governance structure would need to be entirely re-imagined.
% FOUNDING_PROBLEM: The original problem was to establish and maintain community standards, shared amenities, and property values in a planned development.
% FOUNDING_PROBLEM_CORROBORATION: Homeowner advocacy groups, legal aid organizations, and investigative journalists provide evidence that the original problem of maintaining standards has been superseded by a focus on revenue generation and power consolidation, often through selective and punitive enforcement.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.65) reflects the significant financial transfers from homeowners to the benefiting parties through fines and associated fees. Suppression (0.75) is high due to the board's control over enforcement, the difficulty of challenging decisions, and the severe consequences (liens, foreclosure) for non-compliance. The theater ratio (0.45) indicates that while some enforcement activity genuinely addresses community standards, a substantial portion is performative, designed to justify revenue generation and maintain the board's authority. The increasing trend in all metrics over time reflects a drift towards more aggressive and financially driven enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the HOA board and its associated firms, the covenant is a necessary tool for maintaining community order and financial stability. From the perspective of victimized homeowners, it is an extractive mechanism that leverages property ownership to generate revenue and consolidate power. The engine's per-seat classification will highlight this divergence, showing a 'tangled_rope' or 'snare' for payers and a 'rope' or 'tangled_rope' for beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board, property management firms, and legal counsel are clear beneficiaries, receiving direct financial gains and consolidated power. Financially vulnerable homeowners and renters (via pass-through costs) are the primary targets, bearing the brunt of the extraction with limited exit options. Other homeowners sit in a more complex position, potentially benefiting from perceived community stability but also subject to the extractive regime.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_purpose_ambiguity,
    'Is the primary function of the HOA covenant genuinely to coordinate community standards and maintain property values, or has it drifted to primarily serve as a revenue generation and power consolidation mechanism?',
    'Independent audit of HOA finances comparing fine revenue to actual maintenance costs, analysis of enforcement patterns (e.g., targeting low-cost vs. high-cost violations), and surveys of homeowner satisfaction vs. perceived fairness of enforcement.',
    'If the latter is confirmed, the constraint''s classification shifts more definitively towards ''snare'' for homeowners, and the ''tangled_rope'' aspect (coordination) is further diminished. If the former, the ''tangled_rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_purpose_ambiguity, empirical, 'Ambiguity regarding the covenant''s true operational purpose.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by homeowners primarily structural (legal barriers, financial costs of resistance) or internalized (fear of retaliation, belief in HOA authority)?',
    'Post-exit suppression trajectory: if homeowners who successfully exit the HOA (e.g., through legal challenge or sale) continue to exhibit suppressed behavior in new community contexts, it suggests an internalized component. Otherwise, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as homeowners carry the suppression with them. This would amplify the ''snare'' characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for homeowners.').

omega_variable(
    framing_under_determination,
    'Does framing the HOA covenant as an ''extraction_reading'' adequately capture the full complexity, or would a ''behavioral_control_reading'' or ''coordination_reading'' offer a more complete, albeit different, structural analysis?',
    'Comparative analysis of classification outcomes and stakeholder experiences across all three readings. If a different reading consistently yields a more coherent and less contradictory classification for a majority of stakeholders, it suggests a stronger framing.',
    'Adopting an alternative framing could shift the perceived extractiveness and suppression, potentially leading to a different claimed_type and different policy recommendations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Under-determination of the most appropriate structural framing for the HOA covenant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hoa_covenant_scope' kernel. This 'extraction_reading' focuses on the covenant's role in revenue generation and power consolidation, distinct from its coordination or behavioral control functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
