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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: State Sovereignty as Primary Border Control Authority
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primary' reading of border
 *   control legitimacy, asserting that state territorial sovereignty
 *   inherently grants absolute discretion to exclude non-citizens, and that
 *   border control is constitutive of statehood itself. This reading places
 *   the state's right to self-determination and territorial integrity above
 *   individual claims to freedom of movement or asylum, treating human rights
 *   obligations as external limits rather than internal components of
 *   legitimate authority. The constraint is claimed as a Rope by its
 *   proponents, but its operational metrics (high extractiveness and
 *   suppression) lead to a computed Tangled Rope classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.7).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.9).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "State Sovereignty as Primary Border Control Authority").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, 'a2ba5e22-c417-4bc8-b44d-9f114009a017').
narrative_ontology:cs_kernel_codification('a2ba5e22-c417-4bc8-b44d-9f114009a017', formalized).
narrative_ontology:cs_authority_grounding('a2ba5e22-c417-4bc8-b44d-9f114009a017', lineage).
narrative_ontology:cs_interpretation_layer_present('a2ba5e22-c417-4bc8-b44d-9f114009a017').
narrative_ontology:cs_reading_relation('a2ba5e22-c417-4bc8-b44d-9f114009a017', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('a2ba5e22-c417-4bc8-b44d-9f114009a017', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('a2ba5e22-c417-4bc8-b44d-9f114009a017', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('a2ba5e22-c417-4bc8-b44d-9f114009a017', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('a2ba5e22-c417-4bc8-b44d-9f114009a017', foundational, border_control_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(border_control_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('a2ba5e22-c417-4bc8-b44d-9f114009a017', border_control_constitutive_of_statehood, conventional).
narrative_ontology:cs_reference_frame('a2ba5e22-c417-4bc8-b44d-9f114009a017', westphalian_sovereignty_model).
narrative_ontology:cs_drift_state('a2ba5e22-c417-4bc8-b44d-9f114009a017', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a2ba5e22-c417-4bc8-b44d-9f114009a017', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, sovereign_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_populations).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, non_citizen_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, national_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce the right to control borders as a fundamental aspect of their sovereignty. They benefit from the ability to regulate who enters and resides within their territory, claiming it is essential for national security, economic stability, and cultural preservation. Exit options are constrained by international norms and treaties, but the core claim of sovereignty remains strong.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, sovereign_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the perceived security, economic protection, and cultural cohesion that border controls are claimed to provide. They are often the primary constituency for policies that prioritize national sovereignty in migration matters. Their direct costs are diffuse (e.g., higher prices for certain goods/services due to labor shortages), but their perceived benefits are concentrated.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_populations, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of exclusion, including denied entry, detention, deportation, and the risks associated with irregular migration. Their options are severely limited, often to remaining in precarious situations or attempting dangerous crossings. They are the primary targets of the enforcement mechanisms.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, non_citizen_migrants, payer,
    powerless, immediate, trapped, global).

% Face significant barriers to entry and often have their claims adjudicated under frameworks that prioritize state sovereignty over individual protection. They are legally distinct from economic migrants but often experience similar forms of exclusion and suppression at borders, despite international legal protections.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Challenge the absolute discretion of states to exclude, arguing for the primacy of human rights and international protection obligations. While they can influence discourse and policy, their arguments are often treated as external constraints on, rather than constitutive elements of, state sovereignty in this reading.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_advocates, excluded,
    moderate, generational, constrained, global).

% Analyze the legal and philosophical underpinnings of state sovereignty and its implications for border control. They observe the tension between state claims of absolute discretion and evolving international human rights law, often critiquing the practical outcomes of sovereignty-first approaches.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the management of national territory, resources, and population by establishing clear boundaries and regulating entry, which is claimed to be essential for maintaining internal order and collective self-determination.
% TRANSFER_FUNCTION: Transfers the right to determine who belongs to a territory from individual non-citizens to the sovereign state, effectively transferring control over access to resources, labor markets, and social benefits to the state and its citizens.
% ABSENT_VOICES: Non-citizen migrants and asylum seekers, whose perspectives on the human cost of exclusion are often marginalized in national policy debates. International human rights bodies and advocates are present but often framed as external critics rather than legitimate participants in defining state authority.
% DISAPPEARANCE_RATIONALE: If the principle of absolute state discretion over borders vanished, the global system of nation-states would fundamentally reorganize. Migration patterns would shift dramatically, national identities would be challenged, and the very concept of territorial sovereignty would need redefinition, leading to profound political and economic restructuring.
% FOUNDING_PROBLEM: The need to define and defend the territorial integrity and political independence of sovereign states in a world of competing powers and mobile populations, ensuring the state's capacity for self-governance.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and their citizen populations consistently attest that the problem of maintaining territorial integrity and national self-determination is live and ongoing, citing geopolitical instability, economic pressures, and perceived threats to national identity. This is corroborated by historical and contemporary international relations theory, which emphasizes the centrality of sovereignty to the state system.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because the constraint enables states to extract significant value from non-citizens (e.g., through labor exploitation of undocumented workers, or by denying access to higher-wage economies). Suppression is very high (0.9) due to the active and often violent enforcement mechanisms (border patrols, detention, deportation) required to maintain exclusion. Theater ratio is low (0.1) because the enforcement is largely functional in achieving its stated goal of exclusion, with minimal performative elements. Accessibility collapse is high (0.8) as alternatives to state-controlled entry are severely limited. Resistance is high (0.75) from migrants and advocates, reflecting the significant costs imposed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states, this is a legitimate exercise of self-determination (a Rope). From the perspective of excluded migrants, it is a coercive system of extraction and denial of fundamental rights (a Snare). The engine's computation of Tangled Rope reflects the hybrid nature: a genuine coordination function (state self-governance) coupled with asymmetric extraction and active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and citizen populations are beneficiaries, as they control access and accrue perceived benefits. Non-citizen migrants and asylum seekers are clear victims, bearing the direct costs of exclusion and lacking exit options. International human rights advocates are excluded, as their arguments are often dismissed or circumvented by states adhering to this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (as proponents claim) by highlighting the high extractiveness and suppression. It also avoids mislabeling it as a pure Snare by acknowledging the genuine, albeit contested, coordination function of state self-governance and territorial integrity. The 'contested' status of the founding problem (is it still about self-preservation or primarily about rent-seeking?) is key to understanding its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_primacy,
    'Does state territorial sovereignty inherently entail absolute discretion to exclude, or is it fundamentally constrained by international human rights law?',
    'Evolution of international customary law and binding judicial precedents from international courts that explicitly limit state discretion in border control based on human rights obligations.',
    'If human rights are deemed primary, the extractiveness and suppression of this constraint would be re-evaluated downward, and its legitimacy as a ''Tangled Rope'' would be challenged, potentially shifting towards a Snare if the coordination function is deemed insufficient to justify the rights infringements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_primacy, conceptual, 'The fundamental conceptual conflict between state sovereignty and individual human rights in border control.').

omega_variable(
    economic_vs_security_justification,
    'To what extent are current border control policies driven by genuine national security concerns versus economic protectionism or labor market control?',
    'Independent audits of border security expenditures and their effectiveness, coupled with economic analyses of the impact of migration on labor markets and public services, disaggregated by skill level and origin.',
    'If economic protectionism is the dominant driver, the ''coordination function'' of this constraint would be seen as a cover for pure extraction, pushing its classification closer to a Snare. If security is genuinely primary, the Tangled Rope classification would be reinforced, albeit with high extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_security_justification, empirical, 'The true motivations behind state border control policies.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (external barriers, legal penalties) or internalized (fear, hopelessness, identity fusion with ''undocumented'' status)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., fear of deportation, inability to integrate) after the immediate physical barrier is removed (e.g., through amnesty programs), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making ''exit'' less meaningful. This would amplify the Snare-like qualities of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for migrants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1648, border_control_legitimacy__sovereignty_primary, theater_ratio, 1648, 0.05).
narrative_ontology:measurement(bord_tr_t1800, border_control_legitimacy__sovereignty_primary, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(bord_tr_t1900, border_control_legitimacy__sovereignty_primary, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(bord_tr_t1950, border_control_legitimacy__sovereignty_primary, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(bord_tr_t2000, border_control_legitimacy__sovereignty_primary, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__sovereignty_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1648, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1648, 0.5).
narrative_ontology:measurement(bord_be_t1800, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1800, 0.6).
narrative_ontology:measurement(bord_be_t1900, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(bord_be_t1950, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(bord_be_t2000, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1648, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1648, 0.4).
narrative_ontology:measurement(bord_su_t1800, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1800, 0.55).
narrative_ontology:measurement(bord_su_t1900, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(bord_su_t1950, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(bord_su_t2000, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, international_refugee_law).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, national_citizenship_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('sovereignty_primary') of the 'border_control_legitimacy' kernel. Its high extractiveness and suppression contrast with other readings that prioritize freedom of movement or jurisdictional balance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
