% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Authority Derived from Territorial Sovereignty
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint models the 'sovereignty reading' of border legitimacy,
 *   where a state's authority to control its borders and exclude non-citizens
 *   is derived from its territorial sovereignty. It is presented as a
 *   fundamental right of the state, essential for national security, economic
 *   stability, and cultural preservation. This reading places excluded
 *   migrants and asylum seekers in a victim role, as their movement is
 *   directly suppressed by the state's legitimate exercise of this authority.
 *   The high extractiveness reflects the severe costs imposed on those
 *   excluded, while high suppression reflects the active enforcement required
 *   to maintain this exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.85).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.9).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, snare).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Authority Derived from Territorial Sovereignty").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '3725a8d1-d085-49f5-9a05-86b8a554ac0d').
narrative_ontology:cs_kernel_codification('3725a8d1-d085-49f5-9a05-86b8a554ac0d', formalized).
narrative_ontology:cs_authority_grounding('3725a8d1-d085-49f5-9a05-86b8a554ac0d', lineage).
narrative_ontology:cs_interpretation_layer_present('3725a8d1-d085-49f5-9a05-86b8a554ac0d').
narrative_ontology:cs_reading_relation('3725a8d1-d085-49f5-9a05-86b8a554ac0d', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('3725a8d1-d085-49f5-9a05-86b8a554ac0d', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('3725a8d1-d085-49f5-9a05-86b8a554ac0d', foundational, territorial_sovereignty_absolute).
narrative_ontology:cs_axiom_status(territorial_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3725a8d1-d085-49f5-9a05-86b8a554ac0d', territorial_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('3725a8d1-d085-49f5-9a05-86b8a554ac0d', foundational, state_right_to_exclude_unconditional).
narrative_ontology:cs_axiom_status(state_right_to_exclude_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('3725a8d1-d085-49f5-9a05-86b8a554ac0d', state_right_to_exclude_unconditional, deontological).
narrative_ontology:cs_reference_frame('3725a8d1-d085-49f5-9a05-86b8a554ac0d', westphalian_sovereignty_principle).
narrative_ontology:cs_drift_state('3725a8d1-d085-49f5-9a05-86b8a554ac0d', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3725a8d1-d085-49f5-9a05-86b8a554ac0d', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizenry_of_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, state_apparatus).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers_without_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state, through its legislative, executive, and judicial branches, defines and enforces border policy, including who may enter and under what conditions. It claims the right to do so based on territorial sovereignty and the protection of national interests. It benefits from maintaining control over its population and resources.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens benefit from the perceived security, cultural cohesion, and economic stability that border controls are claimed to provide. They are protected from perceived threats of uncontrolled migration and maintain exclusive access to certain state resources and social benefits. Their support legitimizes the state's actions.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizenry_of_state, beneficiary,
    organized, generational, mobile, national).

% Individuals seeking to cross borders without state permission face legal penalties, detention, deportation, and often dangerous journeys. They bear the direct costs of exclusion, including separation from family, loss of economic opportunity, and risk to life and limb. Their movement is directly suppressed by the constraint.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Individuals fleeing persecution or disaster who are denied entry or legal status based on the state's sovereign right to exclude. They are often in a legal limbo, unable to return home and unable to gain legal residency, facing precarious living conditions and exploitation. Their identity as asylum seekers is often tied to their inability to return.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers_without_status, payer,
    powerless, biographical, identity_locked, global).

% Advocate for universal human rights, including freedom of movement and the right to seek asylum, often challenging the absolute nature of state sovereignty. Their arguments are often dismissed by states asserting sovereign control, and their influence is limited to moral suasion or legal challenges within existing state frameworks.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the historical development and contemporary application of international law regarding sovereignty, borders, and human rights. They provide critical commentary on the legal justifications for exclusion and the evolving norms of state responsibility, but do not directly enforce or benefit from the constraint.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear boundary for state jurisdiction, allowing for the coordinated administration of laws, resources, and social services within a defined territory, and enabling the state to act as a unified entity in international relations.
% TRANSFER_FUNCTION: Transfers the right to determine who resides within a territory from individuals (migrants) to the state, and by extension, to its citizenry. It also transfers the costs of exclusion (e.g., detention, enforcement, human suffering) to excluded migrants.
% ABSENT_VOICES: The voices of excluded migrants and those who advocate for a universal right to freedom of movement are largely absent from the formal decision-making processes that define border policy. They would argue that human rights supersede absolute state sovereignty.
% DISAPPEARANCE_RATIONALE: If the state's sovereign right to exclude at its borders vanished overnight, there would be immediate and profound global rearrangements. Migration patterns would shift dramatically, national identities and economies would be fundamentally altered, and the very concept of the nation-state as currently understood would be challenged, leading to a complete reorganization of political and social structures.
% FOUNDING_PROBLEM: The need to define and defend a territory, control population movement, and establish a distinct political community with shared laws and resources, particularly in the context of emerging nation-states and the Westphalian system.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and a significant portion of its citizenry attest that the founding problem of maintaining territorial integrity and national identity remains live, citing ongoing geopolitical tensions, economic pressures, and security concerns. International law scholars acknowledge the historical basis of sovereignty but often contest its absolute application in the modern era, particularly concerning human rights.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the costs imposed on excluded migrants (loss of life, liberty, opportunity) are severe and directly result from the state's exercise of its claimed right. Suppression (0.9) is also high, as states employ significant resources (border patrols, detention, legal frameworks) to actively prevent unauthorized entry and remove those without status. The theater ratio (0.1) is low, indicating that the enforcement is largely functional, directly achieving the goal of exclusion, rather than being performative. Accessibility collapse (0.7) is substantial, as legal alternatives for entry are severely restricted for many, and resistance (0.8) is high, reflecting the desperate attempts of migrants to overcome these barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and its citizenry, this constraint is a legitimate exercise of sovereign power, a 'rope' or even a 'mountain' of international law. From the perspective of excluded migrants, it is a 'snare' that traps them in precarious situations, denying fundamental rights. The engine's classification will reflect the latter due to the high extractiveness and suppression, despite the state's claim of legitimate authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and its citizenry are clear beneficiaries (d near 0.0), as they claim and exercise the right to exclude, benefiting from perceived security and control. Excluded migrants and asylum seekers are direct targets (d near 1.0), bearing the full brunt of the constraint's coercive power. International human rights advocates are excluded from the formal decision-making process, and international law scholars act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights,
    'Is territorial sovereignty an absolute right that precedes or supersedes universal human rights, or is it constrained by them?',
    'Evolution of international customary law and binding judicial precedents from international courts that explicitly balance or prioritize these claims.',
    'If human rights are deemed to constrain sovereignty, the legitimacy of absolute exclusion would diminish, potentially reclassifying the constraint towards a ''tangled_rope'' or ''scaffold'' with more defined obligations. If sovereignty remains absolute, the ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights, conceptual, 'The fundamental conceptual conflict between state sovereignty and universal human rights.').

omega_variable(
    security_vs_rent_seeking,
    'To what extent do border controls genuinely serve national security and economic stability, versus acting as a mechanism for rent-seeking by the citizenry or state apparatus?',
    'Independent, long-term economic and sociological studies on the actual impacts of migration (both authorized and unauthorized) on host countries, disaggregated by sector and social group, compared against the stated justifications for exclusion.',
    'If the security and economic benefits are found to be exaggerated or non-existent, the ''snare'' classification would be further solidified, as the coordination story (national protection) would be revealed as a cover for extraction. If benefits are substantial, it might push towards a ''tangled_rope'' where coordination and extraction are more genuinely intertwined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_rent_seeking, empirical, 'Empirical basis for the claimed benefits of border exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1648, border_legitimacy__sovereignty_reading, theater_ratio, 1648, 0.05).
narrative_ontology:measurement(bord_tr_t1800, border_legitimacy__sovereignty_reading, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(bord_tr_t1900, border_legitimacy__sovereignty_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(bord_tr_t1950, border_legitimacy__sovereignty_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(bord_tr_t2000, border_legitimacy__sovereignty_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(bord_tr_t2024, border_legitimacy__sovereignty_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1648, border_legitimacy__sovereignty_reading, base_extractiveness, 1648, 0.6).
narrative_ontology:measurement(bord_be_t1800, border_legitimacy__sovereignty_reading, base_extractiveness, 1800, 0.7).
narrative_ontology:measurement(bord_be_t1900, border_legitimacy__sovereignty_reading, base_extractiveness, 1900, 0.75).
narrative_ontology:measurement(bord_be_t1950, border_legitimacy__sovereignty_reading, base_extractiveness, 1950, 0.8).
narrative_ontology:measurement(bord_be_t2000, border_legitimacy__sovereignty_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_legitimacy__sovereignty_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1648, border_legitimacy__sovereignty_reading, suppression_requirement, 1648, 0.5).
narrative_ontology:measurement(bord_su_t1800, border_legitimacy__sovereignty_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(bord_su_t1900, border_legitimacy__sovereignty_reading, suppression_requirement, 1900, 0.75).
narrative_ontology:measurement(bord_su_t1950, border_legitimacy__sovereignty_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(bord_su_t2000, border_legitimacy__sovereignty_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(bord_su_t2024, border_legitimacy__sovereignty_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_legitimacy' kernel, focusing on state sovereignty. It is linked to sibling readings that emphasize freedom of movement and humanitarian obligations, as these interpretations are in direct contestation over the same underlying issue of border authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
