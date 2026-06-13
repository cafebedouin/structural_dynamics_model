% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: State Territorial Sovereignty as Absolute Border Control
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story models the 'sovereignty primary' reading of border
 *   control legitimacy, where state territorial sovereignty is understood to
 *   entail absolute discretion to exclude non-citizens, and border control is
 *   constitutive of statehood itself. This reading justifies robust
 *   enforcement mechanisms and treats human rights considerations as external
 *   limits rather than internal components of legitimate authority. The
 *   constraint is classified as a Tangled Rope because it provides a
 *   coordination function (defining state boundaries and populations) but
 *   also involves significant, actively enforced extraction from
 *   non-citizens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.7).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.85).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "State Territorial Sovereignty as Absolute Border Control").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, 'bea4f808-49c3-476f-9205-9c9e765f77f1').
narrative_ontology:cs_kernel_codification('bea4f808-49c3-476f-9205-9c9e765f77f1', formalized).
narrative_ontology:cs_authority_grounding('bea4f808-49c3-476f-9205-9c9e765f77f1', lineage).
narrative_ontology:cs_interpretation_layer_present('bea4f808-49c3-476f-9205-9c9e765f77f1').
narrative_ontology:cs_reading_relation('bea4f808-49c3-476f-9205-9c9e765f77f1', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('bea4f808-49c3-476f-9205-9c9e765f77f1', border_control_legitimacy__jurisdictional_sovereignty, forecloses).
narrative_ontology:cs_axiom('bea4f808-49c3-476f-9205-9c9e765f77f1', foundational, territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('bea4f808-49c3-476f-9205-9c9e765f77f1', territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('bea4f808-49c3-476f-9205-9c9e765f77f1', foundational, exclusion_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(exclusion_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('bea4f808-49c3-476f-9205-9c9e765f77f1', exclusion_constitutive_of_statehood, conventional).
narrative_ontology:cs_reference_frame('bea4f808-49c3-476f-9205-9c9e765f77f1', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('bea4f808-49c3-476f-9205-9c9e765f77f1', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bea4f808-49c3-476f-9205-9c9e765f77f1', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, sovereign_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_populations).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, non_citizen_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce the right to control borders as a fundamental aspect of their sovereignty. They benefit from the perceived stability and control over national identity and resources, justifying enforcement as a defense of the state's very existence.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, sovereign_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the perceived security, cultural cohesion, and economic protection offered by strict border controls. They often support policies that limit non-citizen entry, viewing it as a defense of their national interests and social services.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_populations, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of exclusion, including physical danger, economic hardship, and separation from family. Their attempts to cross borders are met with active enforcement, and their legal status is often precarious, leaving them with minimal agency.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, non_citizen_migrants, payer,
    powerless, immediate, trapped, global).

% Face severe restrictions and often detention, despite international legal frameworks for protection. Their claims are often processed slowly or rejected, forcing them into prolonged limbo or dangerous returns. Their identity as asylum seekers is often challenged by the state.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Monitor and challenge state border policies, arguing that they violate international human rights law. They document abuses and advocate for more humane and rights-respecting approaches to migration, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Analyze the historical and theoretical underpinnings of state sovereignty and its relationship to border control and human rights. They often highlight the tension between absolute sovereignty claims and evolving international norms.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the internal and external boundaries of a state, defining who belongs and who does not, thereby enabling the state to manage its population, resources, and security within a defined territory.
% TRANSFER_FUNCTION: Transfers the right to reside and access state resources from non-citizens to citizens, and transfers the costs of exclusion (e.g., enforcement, humanitarian crises) to non-citizens and, indirectly, to international aid organizations.
% ABSENT_VOICES: Non-citizen migrants and asylum seekers, whose voices are systematically marginalized or criminalized in national policy debates, would articulate the human cost of absolute exclusion and advocate for rights-based approaches to mobility.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, states would lose their primary mechanism for defining and defending their territorial integrity and national identity. Global migration patterns would shift dramatically, and the concept of statehood itself would undergo a profound redefinition, leading to a complete rearrangement of international relations and domestic governance.
% FOUNDING_PROBLEM: The need to establish and maintain distinct political communities with defined territories, populations, and governing authorities, particularly in the context of emerging nation-states and the Westphalian system.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and their citizen populations consistently attest that the problem of maintaining distinct political communities and managing national interests is live and ongoing. International relations theory and historical analyses from outside the benefiting parties corroborate the historical necessity of border control for state formation, though they often contest its absolute nature in contemporary contexts.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) due to the severe costs imposed on non-citizen migrants and asylum seekers, who are denied access to territory, resources, and safety. Suppression is very high (0.85) because the state actively employs physical barriers, legal restrictions, and enforcement personnel to prevent entry and deter movement, with few viable alternatives for those seeking entry. Theater ratio is low (0.1) as the enforcement is largely functional in achieving its stated goal of exclusion, with little performative maintenance. Accessibility collapse is high (0.7) as legal and safe pathways for entry are severely restricted or non-existent for many. Resistance is moderate (0.6) from migrants themselves and human rights advocates, but this resistance is often met with increased suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states and citizen populations, this constraint is a legitimate and necessary exercise of self-determination, ensuring security and national identity. From the perspective of non-citizen migrants and human rights advocates, it is an extractive and often violent imposition that disregards fundamental human dignity and rights. The engine's classification as Tangled Rope reflects this inherent tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states are the primary agenda-setters and beneficiaries, directly controlling and benefiting from the exclusion. Citizen populations are also beneficiaries, gaining perceived security and resource protection. Non-citizen migrants and asylum seekers are clear victims, bearing the direct and severe costs of exclusion. Human rights advocates and international law scholars act as observers, analyzing and challenging the constraint without direct control over its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (defining statehood and protecting national interests) is still considered 'live' by its beneficiaries. However, the 'sovereignty primary' reading faces increasing contestation regarding its proportionality and human rights implications. The classification as Tangled Rope, rather than a pure Snare, acknowledges the genuine coordination function of defining state boundaries, while highlighting the asymmetric extraction inherent in its absolute application. This prevents mislabeling it as purely benign coordination or purely malicious extraction, capturing the hybrid nature of its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_to_exclude_ambiguity,
    'Is the state''s right to exclude non-citizens an inherent, ''natural'' right of sovereignty, or a constructed legal norm subject to evolving international human rights law?',
    'International legal consensus shifts towards a ''responsibility to protect'' framework that includes non-citizens, or a landmark international court ruling redefines the scope of sovereign exclusion.',
    'If constructed, the constraint''s legitimacy becomes contingent on its alignment with human rights, potentially reclassifying it towards a Snare if extraction is deemed disproportionate; if inherent, its ''mountain-like'' persistence is reinforced, making challenges more difficult.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_right_to_exclude_ambiguity, conceptual, 'Ambiguity over the foundational nature of the right to exclude.').

omega_variable(
    proportionality_of_enforcement,
    'Is the level of suppression and extractiveness (e.g., border militarization, detention policies) proportional to the actual threats to state security and resources posed by non-citizen migration?',
    'Independent, empirically robust assessments of migration''s impact on security, economy, and social services, compared against the human and financial costs of enforcement.',
    'If disproportionate, the ''coordination'' aspect of the Tangled Rope diminishes, pushing it closer to a Snare, as the enforcement serves primarily to extract from migrants rather than genuinely coordinate state functions. If proportional, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_enforcement, empirical, 'Whether enforcement measures are proportional to actual threats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1648, border_control_legitimacy__sovereignty_primary, theater_ratio, 1648, 0.05).
narrative_ontology:measurement(bord_tr_t1800, border_control_legitimacy__sovereignty_primary, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(bord_tr_t1900, border_control_legitimacy__sovereignty_primary, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(bord_tr_t1950, border_control_legitimacy__sovereignty_primary, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(bord_tr_t2000, border_control_legitimacy__sovereignty_primary, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__sovereignty_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1648, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1648, 0.5).
narrative_ontology:measurement(bord_be_t1800, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1800, 0.55).
narrative_ontology:measurement(bord_be_t1900, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(bord_be_t1950, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1950, 0.65).
narrative_ontology:measurement(bord_be_t2000, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1648, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1648, 0.4).
narrative_ontology:measurement(bord_su_t1800, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1800, 0.5).
narrative_ontology:measurement(bord_su_t1900, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(bord_su_t1950, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(bord_su_t2000, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('sovereignty_primary') of the 'border_control_legitimacy' kernel. It directly influences and is influenced by sibling readings such as 'freedom_of_movement_primary' and 'jurisdictional_sovereignty', as these readings offer competing interpretations of the same underlying concept of state authority over borders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
