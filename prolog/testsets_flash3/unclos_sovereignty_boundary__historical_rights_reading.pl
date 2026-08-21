% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Override UNCLOS EEZ
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents the 'historical rights' reading of maritime
 *   sovereignty, where claims based on historical usage and occupation are
 *   asserted to predate and override the Exclusive Economic Zone (EEZ)
 *   provisions of the United Nations Convention on the Law of the Sea
 *   (UNCLOS). This reading is actively enforced by claimant states, leading
 *   to significant extraction from EEZ-holding coastal states and
 *   international navigational actors. The claimed type is 'tangled_rope'
 *   because it attempts to coordinate historical claims but does so through
 *   asymmetric extraction and active suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.85).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Override UNCLOS EEZ").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, 'f1b60155-c4fe-4459-949e-fb065b6b9b42').
narrative_ontology:cs_kernel_codification('f1b60155-c4fe-4459-949e-fb065b6b9b42', distributed).
narrative_ontology:cs_authority_grounding('f1b60155-c4fe-4459-949e-fb065b6b9b42', extraction).
narrative_ontology:cs_interpretation_layer_present('f1b60155-c4fe-4459-949e-fb065b6b9b42').
narrative_ontology:cs_reading_relation('f1b60155-c4fe-4459-949e-fb065b6b9b42', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1b60155-c4fe-4459-949e-fb065b6b9b42', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('f1b60155-c4fe-4459-949e-fb065b6b9b42', foundational, historical_usage_creates_sovereignty).
narrative_ontology:cs_axiom_status(historical_usage_creates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f1b60155-c4fe-4459-949e-fb065b6b9b42', historical_usage_creates_sovereignty, conventional).
narrative_ontology:cs_axiom('f1b60155-c4fe-4459-949e-fb065b6b9b42', foundational, unclos_is_subordinate_to_prior_rights).
narrative_ontology:cs_axiom_status(unclos_is_subordinate_to_prior_rights, holdable).
narrative_ontology:cs_axiom_grounding('f1b60155-c4fe-4459-949e-fb065b6b9b42', unclos_is_subordinate_to_prior_rights, conventional).
narrative_ontology:cs_reference_frame('f1b60155-c4fe-4459-949e-fb065b6b9b42', pre_unclos_customary_law).
narrative_ontology:cs_drift_state('f1b60155-c4fe-4459-949e-fb065b6b9b42', contemporary_unclos_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f1b60155-c4fe-4459-949e-fb065b6b9b42', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, fishing_fleets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that assert historical rights (e.e.g., 'nine-dash line' claims) over maritime areas, often based on ancient maps or traditional fishing grounds, overriding UNCLOS EEZ provisions. They actively enforce these claims through naval patrols, artificial island construction, and resource extraction, benefiting from expanded territorial control and resource access.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, constrained, regional).

% States whose UNCLOS-defined Exclusive Economic Zones (EEZs) are encroached upon by historical claims. They bear the cost of lost resource control, increased security risks, and diplomatic friction. Their options are diplomatic protest, legal challenge (often ignored), or military confrontation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    institutional, generational, constrained, regional).

% Companies whose vessels transit disputed waters. They face increased insurance costs, potential harassment, and rerouting, leading to delays and higher operational expenses. Their options are to comply with new demands, seek naval protection, or avoid the areas.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies, payer,
    organized, immediate, constrained, global).

% Local and international fishing vessels operating in areas now claimed under historical rights. They face seizure, fines, or expulsion from traditional fishing grounds, directly impacting their livelihoods. Their options are to risk confrontation, abandon fishing, or relocate to less productive areas.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, fishing_fleets, payer,
    moderate, biographical, trapped, local).

% The broader community of states that ratified UNCLOS. They observe the erosion of the convention's authority and the precedent set by historical claims, impacting the stability of international maritime law. Their options are to issue condemnations, support affected states, or re-evaluate their own maritime strategies.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_signatories, observer,
    institutional, generational, analytical, global).

% Bodies like the Permanent Court of Arbitration or ITLOS, which have ruled against historical claims that contradict UNCLOS. Their rulings are often ignored by claimant states, rendering them structurally excluded from effective enforcement.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_tribunals, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading attempts to coordinate maritime claims by prioritizing long-standing historical presence and traditional use, aiming to provide a basis for sovereignty that predates modern international conventions.
% TRANSFER_FUNCTION: Transfers control over maritime resources (fishing, oil, gas) and strategic waterways from states adhering to UNCLOS EEZ limits to states asserting historical rights, often through unilateral action and military presence.
% ABSENT_VOICES: International tribunals and the broader UNCLOS signatory community, whose legal interpretations and rulings are systematically disregarded by claimant states. They would argue for the primacy of established international law and peaceful dispute resolution.
% DISAPPEARANCE_RATIONALE: If the claim of historical rights overriding UNCLOS EEZ vanished, claimant states would lose their justification for expansive maritime control, leading to a re-assertion of UNCLOS-defined EEZ boundaries by coastal states. Resource allocation and navigational freedoms would revert to international legal norms, fundamentally altering geopolitical dynamics in contested regions.
% FOUNDING_PROBLEM: The problem of reconciling ancient, traditional claims to maritime areas with modern, codified international law, particularly in resource-rich or strategically vital regions.
% FOUNDING_PROBLEM_CORROBORATION: Claimant states assert the problem is live, citing historical maps and cultural heritage. Coastal states and international legal scholars corroborate that the tension between historical claims and UNCLOS remains a live and destabilizing issue, leading to ongoing disputes and enforcement actions.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__historical_rights_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because claimant states gain significant control over resources and strategic areas at the direct expense of other states' UNCLOS-recognized rights. Suppression is also high (0.75) due to the use of naval patrols, coast guard actions, and diplomatic pressure to enforce these claims, often disregarding international legal rulings. The theater ratio is moderate (0.4) as claimant states often frame their actions as defending historical integrity, while the primary function has shifted to resource acquisition and strategic control. Accessibility collapse is 0.6, as alternatives (like UNCLOS dispute resolution) are often ignored, but not entirely foreclosed, leaving room for diplomatic and legal resistance. Resistance is 0.8, reflecting the strong opposition from affected coastal states and the international community.
 *
 * PERSPECTIVAL GAP:
 *   Claimant states perceive this as a legitimate assertion of long-standing sovereign rights, a 'mountain' of historical fact. Affected coastal states and international legal bodies perceive it as a 'snare' of unilateral extraction, undermining established international law. The engine's classification as 'tangled_rope' reflects the hybrid nature: a coordination claim (of historical rights) coupled with asymmetric extraction and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are clear beneficiaries and agenda-setters, actively shaping and enforcing the constraint. EEZ-holding coastal states, international shipping, and fishing fleets are victims, bearing the direct costs of lost access and increased operational risks. UNCLOS signatories act as observers, while international tribunals are excluded, their rulings often rendered ineffective by non-compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by highlighting the active enforcement and extraction inherent in the 'historical rights' claim. While framed as a historical 'mountain,' its persistence relies on active suppression and benefits identifiable actors, making it a 'tangled_rope' rather than a natural law or a pure coordination mechanism. The high extractiveness and suppression, coupled with active enforcement, distinguish it from a 'piton' (which would have atrophied function and lower enforcement) or a 'rope' (which would lack asymmetric extraction and high suppression).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_legitimacy_vs_modern_law,
    'To what extent do historical claims, even if documented, retain legal legitimacy in the face of modern, codified international law like UNCLOS?',
    'A definitive ruling by a universally recognized international court that is accepted and enforced by all parties, or a new international convention that explicitly reconciles or supersedes historical claims.',
    'If historical claims are deemed legally superseded, the constraint''s legitimacy collapses, reducing its extractiveness and suppression. If they are recognized as co-equal or superior, the constraint''s legitimacy is strengthened, potentially increasing its extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_legitimacy_vs_modern_law, conceptual, 'The fundamental legal and conceptual conflict between historical claims and modern international maritime law.').

omega_variable(
    enforcement_sustainability,
    'Is the current level of active enforcement by claimant states sustainable in the long term, given the diplomatic and economic costs?',
    'Analysis of claimant states'' long-term economic and military budgets, coupled with a geopolitical assessment of international willingness to challenge or accommodate these claims.',
    'If enforcement is unsustainable, the constraint''s suppression and extractiveness may decline over time, potentially leading to a re-assertion of UNCLOS norms. If sustainable, the constraint could harden further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'The long-term viability of active enforcement for historical claims.').

omega_variable(
    kernel_reading_divergence,
    'This constraint is one reading of the ''unclos_sovereignty_boundary'' kernel. How would the classification change under the ''strict_eez_reading'' or ''non_ratifier_enforcement_reading''?',
    'Comparative analysis of the structural properties (beneficiaries, victims, enforcement) and metrics (extractiveness, suppression) as authored for each sibling reading.',
    'The ''strict_eez_reading'' would likely classify as a ''rope'' or ''mountain'' for its adherence to codified law, with lower extractiveness. The ''non_ratifier_enforcement_reading'' might be a ''tangled_rope'' or ''snare'' depending on its enforcement mechanisms and beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Analysis of how different readings of maritime sovereignty lead to distinct constraint classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(uncl_tr_t2008, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2008, 0.35).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(uncl_be_t2008, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2008, 0.78).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(uncl_su_t2008, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unclos_sovereignty_boundary' kernel. It directly contests the 'strict_eez_reading' and influences the operational space for 'non_ratifier_enforcement_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
