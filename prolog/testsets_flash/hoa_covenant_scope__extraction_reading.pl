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
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Extraction Mechanism (Extraction Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint describes the operation of Homeowners Association (HOA)
 *   covenants primarily as a mechanism for revenue generation and power
 *   consolidation by the HOA board and associated firms, rather than for
 *   genuine community coordination or property value maintenance. Enforcement
 *   is characterized by a proliferation of fines, selective application of
 *   rules, and expedited lien processes, often leading to significant
 *   attorney fees. This is the 'extraction_reading' of the
 *   'hoa_covenant_scope' kernel, focusing on the rent-seeking aspects.
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
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Extraction Mechanism (Extraction Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, '5640d351-1387-4abf-98bd-20d869d2e370').
narrative_ontology:cs_kernel_codification('5640d351-1387-4abf-98bd-20d869d2e370', formalized).
narrative_ontology:cs_authority_grounding('5640d351-1387-4abf-98bd-20d869d2e370', extraction).
narrative_ontology:cs_interpretation_layer_present('5640d351-1387-4abf-98bd-20d869d2e370').
narrative_ontology:cs_reading_relation('5640d351-1387-4abf-98bd-20d869d2e370', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('5640d351-1387-4abf-98bd-20d869d2e370', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('5640d351-1387-4abf-98bd-20d869d2e370', foundational, covenant_as_revenue_source).
narrative_ontology:cs_axiom_status(covenant_as_revenue_source, holdable).
narrative_ontology:cs_axiom_grounding('5640d351-1387-4abf-98bd-20d869d2e370', covenant_as_revenue_source, conventional).
narrative_ontology:cs_axiom('5640d351-1387-4abf-98bd-20d869d2e370', foundational, board_discretion_as_power_tool).
narrative_ontology:cs_axiom_status(board_discretion_as_power_tool, holdable).
narrative_ontology:cs_axiom_grounding('5640d351-1387-4abf-98bd-20d869d2e370', board_discretion_as_power_tool, conventional).
narrative_ontology:cs_reference_frame('5640d351-1387-4abf-98bd-20d869d2e370', unfettered_board_discretion).
narrative_ontology:cs_drift_state('5640d351-1387-4abf-98bd-20d869d2e370', contemporary_consumer_protection_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('5640d351-1387-4abf-98bd-20d869d2e370', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_legal_counsel).
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

% Elected or appointed individuals who set and enforce covenant rules, often with significant discretion. They benefit from increased property values (if they own property in the HOA) and can consolidate power through control over enforcement and financial resources. They approve contracts with property management and legal counsel.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_board_members, agenda_setter,
    institutional, biographical, mobile, local).

% Contracted by the HOA board to administer covenants, collect fees, and manage enforcement. They profit directly from management fees and often from additional charges for fine processing, lien filings, and other administrative tasks, incentivizing fine proliferation.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, generational, arbitrage, regional).

% Law firms retained by the HOA to pursue covenant violations, collect unpaid fines, and handle litigation. They generate substantial revenue from attorney fees, which are often passed directly to homeowners, creating a strong incentive for aggressive enforcement and legal action.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_legal_counsel, beneficiary,
    organized, generational, arbitrage, regional).

% Homeowners with limited financial resources who are disproportionately impacted by fines, late fees, and legal costs. They face the risk of liens, foreclosure, and significant financial distress, with few options to contest enforcement or exit the HOA without losing their home.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Renters in HOA-governed properties who indirectly bear the costs of fines and fees through increased rent or reduced property maintenance by landlords. They have no direct voice in HOA governance and limited recourse against covenant enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Homeowners who may benefit from perceived property value maintenance or amenities, but also face the risk of fines and contribute to HOA fees. Their position is ambiguous, as they are both subject to the extractive mechanisms and potential beneficiaries of the overall HOA structure.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, other_homeowners, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, other_homeowners, beneficiary).

% Local authorities responsible for consumer protection or property rights who may receive complaints about HOA practices. They have the power to investigate and potentially intervene, but often face legal limitations in regulating private HOA covenants.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, local_government_regulators, observer,
    institutional, generational, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The claimed coordination function is to maintain property values and shared amenities through uniform standards and enforcement, ensuring a desirable living environment for all residents.
% TRANSFER_FUNCTION: Transfers financial resources (fines, fees, legal costs) from homeowners (especially financially vulnerable ones) to the HOA's administrative and legal apparatus, primarily benefiting property management firms and legal counsel, and indirectly consolidating power for HOA board members.
% ABSENT_VOICES: Homeowners who have lost their homes due to HOA liens or foreclosures are permanently excluded. Potential residents who avoid HOA communities due to their reputation for aggressive enforcement are also absent, representing a suppressed market for alternative governance models.
% DISAPPEARANCE_RATIONALE: If the covenant's extractive enforcement vanished, the revenue streams for property management and legal counsel would collapse, HOA boards would lose a primary tool for power consolidation, and homeowners would regain autonomy over their property. The market for community governance would likely reorganize, with a shift towards more genuinely cooperative models or direct municipal services.
% FOUNDING_PROBLEM: HOA covenants were originally established to ensure maintenance of common areas, resolve genuine externalities (e.g., noise, waste), and protect property values in planned communities where shared responsibilities were necessary.
% FOUNDING_PROBLEM_CORROBORATION: HOA boards and property management firms assert the founding problems (e.g., maintaining property values, ensuring community standards) are still live. However, financially vulnerable homeowners, housing advocates, and some legal scholars argue that while some problems persist, the enforcement mechanism has drifted to primarily serve extractive interests, with the original coordination function largely superseded by rent-seeking. Independent studies on HOA foreclosures and fine revenue allocation corroborate the shift.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) due to the substantial financial transfers from homeowners to the HOA, property management, and legal counsel through fines and associated fees. Suppression (0.75) is significant because homeowners have limited exit options and face legal and financial penalties for non-compliance, with enforcement often targeting minor infractions to generate revenue. The theater ratio (0.40) reflects that while some covenant enforcement genuinely maintains community standards, a substantial portion is performative or pretextual, serving primarily to justify fines and assert board authority. The increasing trend in extractiveness and suppression over time reflects a drift towards more aggressive fine proliferation and enforcement.
 *
 * PERSPECTIVAL GAP:
 *   HOA board members and property management firms perceive the covenant as a necessary tool for maintaining community standards and financial stability, justifying their fees. Financially vulnerable homeowners, however, experience it as an arbitrary and punitive system designed to extract wealth, with little recourse. This divergence is central to the extraction reading.
 *
 * DIRECTIONALITY LOGIC:
 *   HOA board members, property management firms, and legal counsel are clear beneficiaries (d near 0.0) as they directly profit from fine revenue and associated fees. Financially vulnerable homeowners and renters (via pass-through costs) are the primary victims (d near 1.0), bearing the brunt of fines and enforcement. Other homeowners may experience a mix of benefits (e.g., perceived property value maintenance) and costs (e.g., risk of fines), placing them closer to symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate might have been genuine coordination (e.g., maintaining common areas). However, in this extraction reading, the mandate has atrophied into a cover for rent-seeking. The classification as Tangled Rope (rather than Snare) acknowledges the *claimed* coordination function, but the high extractiveness and active enforcement reveal its true nature as a mechanism for asymmetric wealth transfer. Resolving mandatrophy would require restructuring governance to align incentives, ensuring fine revenue directly benefits the community, and limiting the discretion of enforcement to prevent rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily an extraction mechanism, or does it genuinely serve coordination or behavioral control functions?',
    'Analysis of fine revenue allocation, enforcement patterns (e.g., targeting high-fine violations vs. genuine nuisances), and board member financial interests. If revenue primarily funds management/legal fees and board members benefit from increased property values without direct contribution, the extraction reading is strengthened.',
    'If the extraction reading is confirmed, the constraint is a Snare; if coordination or behavioral control functions are dominant, it would be a Rope or Tangled Rope with lower extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is the ''extraction_reading'' of the ''hoa_covenant_scope'' kernel. Sibling readings (''coordination_reading'', ''behavioral_control_reading'') offer alternative interpretations of the covenant''s primary function.').

omega_variable(
    fine_revenue_allocation_ambiguity,
    'To what extent does fine revenue genuinely fund community improvements or shared amenities, versus primarily covering administrative and legal costs?',
    'Detailed audit of HOA financial records, comparing fine income to expenditures on common area maintenance, amenity upgrades, and legal/management fees.',
    'If fine revenue disproportionately funds administrative overhead and legal fees, it strengthens the extraction reading. If it primarily funds community benefits, it would weaken the extraction claim and shift towards a coordination or behavioral control reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fine_revenue_allocation_ambiguity, empirical, 'Ambiguity in how fine revenue is utilized by the HOA.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hoa_covenant_scope' kernel, focusing on its extractive function. It is linked to 'hoa_covenant_scope__coordination_reading' and 'hoa_covenant_scope__behavioral_control_reading' as sibling interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
