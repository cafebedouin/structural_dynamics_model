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
 *   This constraint describes the operation of Homeowners' Association (HOA)
 *   covenants as a mechanism for revenue generation and board power
 *   consolidation, primarily through the proliferation of fines and selective
 *   enforcement. While HOAs nominally exist for coordination (e.g., shared
 *   maintenance), this reading focuses on how the enforcement of covenants
 *   has become extractive, targeting financially vulnerable homeowners and
 *   generating income for the board, property management firms, and legal
 *   counsel. The claimed type is 'Tangled Rope' because it still possesses a
 *   nominal coordination function (the cover story) but operates with
 *   significant asymmetric extraction and requires active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.65).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.75).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Scope (Extraction Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'be829c46-6b63-4d91-b0b0-7d3cba9c0187').
narrative_ontology:cs_kernel_codification('be829c46-6b63-4d91-b0b0-7d3cba9c0187', formalized).
narrative_ontology:cs_authority_grounding('be829c46-6b63-4d91-b0b0-7d3cba9c0187', extraction).
narrative_ontology:cs_interpretation_layer_present('be829c46-6b63-4d91-b0b0-7d3cba9c0187').
narrative_ontology:cs_reading_relation('be829c46-6b63-4d91-b0b0-7d3cba9c0187', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('be829c46-6b63-4d91-b0b0-7d3cba9c0187', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('be829c46-6b63-4d91-b0b0-7d3cba9c0187', foundational, covenants_as_revenue_source).
narrative_ontology:cs_axiom_status(covenants_as_revenue_source, holdable).
narrative_ontology:cs_axiom_grounding('be829c46-6b63-4d91-b0b0-7d3cba9c0187', covenants_as_revenue_source, empirically_contingent).
narrative_ontology:cs_axiom('be829c46-6b63-4d91-b0b0-7d3cba9c0187', foundational, board_power_consolidation).
narrative_ontology:cs_axiom_status(board_power_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('be829c46-6b63-4d91-b0b0-7d3cba9c0187', board_power_consolidation, empirically_contingent).
narrative_ontology:cs_reference_frame('be829c46-6b63-4d91-b0b0-7d3cba9c0187', covenant_as_revenue_engine).
narrative_ontology:cs_drift_state('be829c46-6b63-4d91-b0b0-7d3cba9c0187', contemporary_hoa_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be829c46-6b63-4d91-b0b0-7d3cba9c0187', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel).
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

% Elected or appointed members who interpret and enforce covenants, often using fine revenue to fund operations or consolidate power. They benefit from the administrative fees and control over community resources.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    institutional, generational, constrained, local).

% Contracted by the HOA board to administer covenants, issue fines, and manage collections. Their revenue often increases with the volume of enforcement actions and associated fees, creating an incentive for fine proliferation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, arbitrage, regional).

% Retained by the HOA to pursue legal action for covenant violations, fine collection, and lien enforcement. They generate substantial fees from these processes, particularly when targeting vulnerable homeowners who cannot afford to contest.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel, beneficiary,
    powerful, biographical, arbitrage, local).

% Bear the brunt of selective enforcement and fine proliferation. They face disproportionate financial burdens, risk of liens, and potential foreclosure due to minor covenant infractions, with limited means to contest. Their identity is tied to their home, making exit costly.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Indirectly bear the costs of HOA fines and fees if landlords pass these expenses through in rent increases or reduced property maintenance. They have no direct voice in HOA governance and limited recourse.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% All residents are subject to the covenants and pay regular dues. While some may benefit from perceived property value maintenance, they are all exposed to the fine system and potential selective enforcement, contributing to the revenue stream.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, all_homeowners, payer,
    moderate, biographical, constrained, local).

% Monitor HOA practices, document cases of abuse, and advocate for homeowner rights and regulatory oversight. They analyze the patterns of fine proliferation and selective enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, community_advocacy_groups, observer,
    organized, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally, to maintain property values, ensure aesthetic uniformity, and manage shared amenities within the community, providing a stable living environment.
% TRANSFER_FUNCTION: Moves financial resources (fines, legal fees, administrative charges) from homeowners, particularly the financially vulnerable, to the HOA board, property management firms, and legal counsel.
% ABSENT_VOICES: Homeowners who are financially or socially marginalized, unable to attend meetings, or intimidated by the HOA's enforcement power. They would highlight the punitive nature of the system and the disproportionate impact on vulnerable residents.
% DISAPPEARANCE_RATIONALE: If the HOA covenants and their enforcement as a revenue mechanism vanished, the current power structure would collapse. Property management firms and legal counsel would lose a significant revenue stream, and homeowners would either self-organize new governance or face a period of uncoordinated property management and potential decline in shared amenities, leading to a reorganization of local property dynamics.
% FOUNDING_PROBLEM: The original problem was to ensure the collective maintenance of shared property and infrastructure, and to prevent individual actions from negatively impacting neighborhood aesthetics and property values.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and property management firms claim the founding problem (maintaining community standards and property values) is still live and requires active enforcement. However, many homeowners and community advocacy groups argue that the problem is largely solved, and the current enforcement regime has drifted into a rent-seeking mechanism, with independent analyses of fine revenue and legal costs supporting this view.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.65) reflects the substantial financial transfers from homeowners to the benefiting parties through fines and associated legal/administrative fees. Suppression (0.75) is high due to the HOA's legal power to levy liens and pursue foreclosure, coupled with the limited exit options and identity-locked nature of homeownership. The theater ratio (0.30) indicates that while some genuine coordination (e.g., basic maintenance) may occur, a significant portion of enforcement activity is performative, serving to justify revenue generation rather than solely addressing genuine externalities. The increasing extractiveness and suppression over time reflect a 'fine ratchet' where the system becomes more punitive.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the board and management, the covenants are a necessary 'Rope' for maintaining community standards and property values. From the perspective of financially vulnerable homeowners, the same structure operates as a 'Snare' or 'Tangled Rope,' extracting wealth and enforcing compliance through coercive means. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members, property management firms, and legal counsel are clear beneficiaries, receiving direct financial gains and consolidating power. Financially vulnerable homeowners and renters (via pass-through) are the primary targets, bearing the direct costs of fines and legal actions. All homeowners are payers, as they are subject to the system, but the extraction is concentrated on the vulnerable through selective enforcement. The 'trapped' and 'identity_locked' exit options for homeowners amplify their directionality towards being targets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_purpose_ambiguity,
    'Is the primary function of HOA covenant enforcement genuine coordination (e.g., maintaining shared amenities) or primarily revenue generation and power consolidation?',
    'Detailed financial audits comparing fine revenue to actual maintenance costs, analysis of enforcement patterns (e.g., targeting minor vs. major infractions), and surveys of homeowner satisfaction with enforcement outcomes vs. perceived benefits.',
    'If primarily revenue generation, the constraint''s extractiveness is confirmed as structural; if primarily coordination, the extractiveness might be re-evaluated as a necessary cost of coordination, potentially reclassifying it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_purpose_ambiguity, empirical, 'Ambiguity regarding the true underlying purpose of HOA covenant enforcement.').

omega_variable(
    selective_enforcement_impact,
    'To what extent is enforcement truly selective, disproportionately targeting specific homeowner demographics (e.g., based on income, race, or social status)?',
    'Statistical analysis of fine issuance and enforcement actions correlated with homeowner demographics, combined with qualitative interviews and legal case studies.',
    'Strong evidence of selective enforcement would amplify the ''suppression'' and ''extractiveness'' metrics, confirming the coercive nature and potentially shifting the classification further towards a Snare for targeted groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_impact, empirical, 'Impact of selective enforcement on specific homeowner groups.').

omega_variable(
    internalized_suppression_among_homeowners,
    'Is the suppression experienced by homeowners primarily structural (e.g., legal barriers, financial penalties) or internalized (e.g., fear of retaliation, social pressure to conform)?',
    'Post-exit surveys of former homeowners or those who successfully challenged HOA actions, assessing lingering psychological impacts or changes in behavior after the direct structural pressure is removed.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as homeowners carry the suppression with them, making resistance harder even if structural barriers are lowered.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_among_homeowners, empirical, 'Structural vs. internalized suppression mechanism among homeowners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.65).

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

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
