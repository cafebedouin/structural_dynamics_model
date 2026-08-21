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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Extraction and Power Consolidation
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story represents the 'extraction reading' of HOA
 *   covenants, where the covenant primarily functions as a mechanism for
 *   revenue generation and board power consolidation through fine
 *   proliferation and selective enforcement. The original coordination
 *   function (e.g., shared maintenance) is largely superseded by these
 *   extractive dynamics. This reading highlights the beneficiaries (board
 *   members, property management, legal counsel) and victims (financially
 *   vulnerable homeowners, renters) of this system.
 *
 * KEY AGENTS:
 *   - hoa_board_members: Primary agenda-setter (institutional/mobile)
 *   - property_management_firms: Primary beneficiary (organized/arbitrage)
 *   - hoa_legal_counsel: Primary beneficiary (organized/mobile)
 *   - financially_vulnerable_homeowners: Primary target (powerless/trapped)
 *   - renters_via_pass_through: Secondary target (powerless/constrained)
 *   - all_homeowners: Diffuse payer (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.65).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.75).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Extraction and Power Consolidation").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'be3afbd1-f69d-40ae-8e60-f7aa45dc2633').
narrative_ontology:cs_kernel_codification('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', formalized).
narrative_ontology:cs_authority_grounding('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', extraction).
narrative_ontology:cs_interpretation_layer_present('be3afbd1-f69d-40ae-8e60-f7aa45dc2633').
narrative_ontology:cs_reading_relation('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', foundational, covenant_as_revenue_source).
narrative_ontology:cs_axiom_status(covenant_as_revenue_source, holdable).
narrative_ontology:cs_axiom_grounding('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', covenant_as_revenue_source, empirically_contingent).
narrative_ontology:cs_axiom('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', secondary, board_power_consolidation_legitimate).
narrative_ontology:cs_axiom_status(board_power_consolidation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', board_power_consolidation_legitimate, conventional).
narrative_ontology:cs_reference_frame('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', unfettered_board_discretion).
narrative_ontology:cs_drift_state('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', contemporary_homeowner_advocacy_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('be3afbd1-f69d-40ae-8e60-f7aa45dc2633', '').
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

% Elected or appointed members who interpret and enforce covenants, often benefiting from the power and influence, and sometimes from direct or indirect financial arrangements with management or legal firms. They consolidate power through fine proliferation and selective enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_board_members, agenda_setter,
    institutional, biographical, mobile, local).

% Contracted by the HOA board to administer covenants, issue fines, and manage collections. They profit directly from increased enforcement activity, fine collection, and associated administrative fees, often incentivized by the volume of violations.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    organized, biographical, arbitrage, regional).

% Provides legal services to the HOA, including drafting and interpreting covenants, pursuing collections, and litigating disputes. Their revenue increases with enforcement actions, particularly those leading to liens or foreclosures, creating a financial incentive for aggressive enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_legal_counsel, beneficiary,
    organized, biographical, mobile, local).

% Bear the brunt of fine proliferation and aggressive collection tactics. They face disproportionate financial strain, risk of liens, and potential foreclosure due to minor covenant violations, with limited legal or financial recourse. Their identity is often tied to their homeownership.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, immediate, trapped, local).

% Indirectly bear the costs of HOA fines and fees through increased rents or reduced property maintenance by landlords. They have no direct voice in HOA governance and limited ability to influence covenant enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% All homeowners are subject to the covenants and potential fines, contributing to the HOA's revenue stream. While some may benefit from perceived property value maintenance, the overall system can feel extractive due to opaque financial practices and aggressive enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, all_homeowners, payer,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, hoa_board_members).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The covenant nominally coordinates property maintenance and community standards to preserve property values and shared amenities, providing a framework for collective action on common issues.
% TRANSFER_FUNCTION: Transfers financial resources (fines, fees, legal costs) from homeowners (especially financially vulnerable ones) to the HOA board, property management firms, and legal counsel, consolidating power and generating revenue for these entities.
% ABSENT_VOICES: Homeowners who are intimidated by the HOA board or lack the resources to challenge fines are effectively silenced. Potential alternative management or legal service providers are excluded by existing contracts and board preferences, preventing competitive pressure.
% DISAPPEARANCE_RATIONALE: If the covenant as an extraction mechanism vanished, the immediate effect would be a cessation of fine-based revenue generation and power consolidation. Homeowners would regain autonomy over minor property decisions, and the financial incentives for aggressive enforcement would disappear. The market for property management and legal services would shift away from fine-driven models.
% FOUNDING_PROBLEM: HOA covenants were established to ensure collective maintenance of shared property, resolve neighborhood disputes, and maintain property values by enforcing aesthetic and behavioral standards.
% FOUNDING_PROBLEM_CORROBORATION: While the original problems of collective maintenance and dispute resolution may still exist, the current operation of the covenant, as described by this reading, has largely decoupled from these. Independent homeowner advocacy groups and legal aid organizations attest that the primary function has shifted to revenue generation and power consolidation, with the original coordination function serving as a cover story.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.65) is high due to the systematic transfer of wealth from homeowners to the board, management, and legal firms through fines and associated fees. Suppression (0.75) is significant because homeowners, especially vulnerable ones, have limited legal and financial options to resist enforcement, and their 'identity_locked' status as homeowners makes exit difficult. The theater ratio (0.4) reflects that while some genuine maintenance activities occur, a substantial portion of enforcement is performative, designed to justify fines and assert board authority rather than address genuine community problems. The increasing trend in extractiveness and suppression over the interval reflects the observed 'fine proliferation' and hardening of enforcement tactics.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the HOA board, property management, and legal counsel, the covenant is a necessary tool for maintaining order and property values, justifying their fees and authority. From the perspective of financially vulnerable homeowners, it is an arbitrary and punitive system designed to extract wealth and enforce compliance through fear, with little genuine coordination benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   HOA board members, property management firms, and legal counsel are clear beneficiaries, as they directly profit from the enforcement and fine collection mechanisms (low directionality). Financially vulnerable homeowners and renters are clear targets, bearing the costs without significant benefit and facing severe consequences for non-compliance (high directionality). Other homeowners may experience a mix of perceived benefits (e.g., maintained property values) and direct costs (fines, fees), placing them closer to symmetric or moderate target status.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the covenant as a pure coordination mechanism (Rope) or a simple aesthetic control (behavioral_control_reading). It highlights the dual function: a nominal coordination role (maintaining property standards) coexisting with a significant, actively enforced extractive function (revenue generation, power consolidation). The rising extractiveness and suppression over time indicate a drift towards a Snare, where the coordination story increasingly serves as cover for extraction. The 'dead' status of the founding problem further supports this drift, suggesting the original mandate has atrophied while the structure persists for other reasons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_coordination_vs_extraction_ratio,
    'What proportion of HOA covenant enforcement genuinely addresses collective action problems (e.g., shared infrastructure maintenance, severe externalities) versus serving as a revenue generation or power consolidation mechanism?',
    'Detailed audit of HOA budgets, fine allocations, and enforcement actions, correlating them with documented community problems and homeowner complaints, conducted by an independent regulatory body.',
    'A higher proportion of genuine coordination would shift the constraint closer to a Rope or a less extractive Tangled Rope. A lower proportion would confirm its classification as a highly extractive Tangled Rope, potentially drifting towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_coordination_vs_extraction_ratio, empirical, 'Distinguishing genuine coordination from extractive cover stories in HOA covenant enforcement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., legal barriers, financial inability to fight fines) or internalized (e.g., fear of retaliation, belief in HOA authority) for homeowners?',
    'Post-exit suppression trajectory: if homeowners who successfully exit the HOA (e.g., through legal challenge or sale) continue to exhibit suppressed behavior or fear of authority, it suggests internalized suppression. Surveys and qualitative interviews with former and current homeowners.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as homeowners carry the suppression with them. This would amplify the effective extraction (χ) for individual homeowners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for homeowners under HOA covenants.').

omega_variable(
    kernel_reading_difference_extraction,
    'How would the classification change if the ''coordination_reading'' or ''behavioral_control_reading'' of the HOA covenant were adopted instead of this ''extraction_reading''?',
    'Analyze the same empirical data (fine rates, budget allocations, enforcement patterns) through the lens of the alternative readings, noting shifts in identified beneficiaries/victims, extractiveness, and suppression. The engine would then compute different classifications for those alternative readings.',
    'The ''coordination_reading'' would likely yield a Rope or low-extraction Tangled Rope with different beneficiaries (all homeowners) and lower extractiveness. The ''behavioral_control_reading'' would likely yield a Tangled Rope focused on aesthetic conformity, with different beneficiaries (homeowners prioritizing uniformity) and potentially different victims (those with non-conforming preferences). This ''extraction_reading'' emphasizes the financial and power dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_difference_extraction, conceptual, 'This constraint is one reading of the ''hoa_covenant_scope'' kernel. This ''extraction_reading'' focuses on revenue generation and power consolidation, leading to a Tangled Rope classification with high extractiveness. The ''coordination_reading'' would emphasize shared maintenance, likely resulting in a Rope. The ''behavioral_control_reading'' would focus on aesthetic uniformity, likely a different Tangled Rope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t2000, hoa_covenant_scope__extraction_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(hoa__tr_t2005, hoa_covenant_scope__extraction_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(hoa__tr_t2010, hoa_covenant_scope__extraction_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(hoa__tr_t2015, hoa_covenant_scope__extraction_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(hoa__tr_t2020, hoa_covenant_scope__extraction_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(hoa__tr_t2024, hoa_covenant_scope__extraction_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(hoa__be_t2000, hoa_covenant_scope__extraction_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(hoa__be_t2005, hoa_covenant_scope__extraction_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(hoa__be_t2010, hoa_covenant_scope__extraction_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(hoa__be_t2015, hoa_covenant_scope__extraction_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(hoa__be_t2020, hoa_covenant_scope__extraction_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(hoa__be_t2024, hoa_covenant_scope__extraction_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t2000, hoa_covenant_scope__extraction_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(hoa__su_t2005, hoa_covenant_scope__extraction_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(hoa__su_t2010, hoa_covenant_scope__extraction_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(hoa__su_t2015, hoa_covenant_scope__extraction_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(hoa__su_t2020, hoa_covenant_scope__extraction_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(hoa__su_t2024, hoa_covenant_scope__extraction_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, property_value_maximization_norm).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, local_government_revenue_streams).

% DUAL FORMULATION NOTE:
% This constraint is the 'extraction_reading' of the 'hoa_covenant_scope' kernel. It is structurally distinct from the 'coordination_reading' and 'behavioral_control_reading' due to differing ε values and beneficiary/victim structures, but all three are linked as components of the broader HOA covenant system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
