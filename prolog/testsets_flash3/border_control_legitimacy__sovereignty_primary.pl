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
 *   This constraint represents the 'sovereignty-primary' reading of border
 *   control legitimacy, asserting that state territorial sovereignty
 *   inherently grants absolute discretion to exclude non-citizens, and that
 *   border control is constitutive of statehood itself. This reading places
 *   state autonomy above individual rights in the context of migration,
 *   justifying robust enforcement mechanisms. The claimed type is
 *   'tangled_rope' because it purports to coordinate national
 *   self-determination while demonstrably extracting from non-citizens
 *   through active enforcement.
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
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, 'a1d30f1e-f814-4dc9-854e-fb587db1b70e').
narrative_ontology:cs_kernel_codification('a1d30f1e-f814-4dc9-854e-fb587db1b70e', formalized).
narrative_ontology:cs_authority_grounding('a1d30f1e-f814-4dc9-854e-fb587db1b70e', lineage).
narrative_ontology:cs_interpretation_layer_present('a1d30f1e-f814-4dc9-854e-fb587db1b70e').
narrative_ontology:cs_reading_relation('a1d30f1e-f814-4dc9-854e-fb587db1b70e', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('a1d30f1e-f814-4dc9-854e-fb587db1b70e', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('a1d30f1e-f814-4dc9-854e-fb587db1b70e', foundational, territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('a1d30f1e-f814-4dc9-854e-fb587db1b70e', territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('a1d30f1e-f814-4dc9-854e-fb587db1b70e', foundational, state_membership_discretionary).
narrative_ontology:cs_axiom_status(state_membership_discretionary, holdable).
narrative_ontology:cs_axiom_grounding('a1d30f1e-f814-4dc9-854e-fb587db1b70e', state_membership_discretionary, conventional).
narrative_ontology:cs_reference_frame('a1d30f1e-f814-4dc9-854e-fb587db1b70e', westphalian_state_system).
narrative_ontology:cs_drift_state('a1d30f1e-f814-4dc9-854e-fb587db1b70e', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a1d30f1e-f814-4dc9-854e-fb587db1b70e', '').
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

% Assert and enforce the right to control borders as a fundamental aspect of their sovereignty. They benefit from the ability to regulate entry and exit, maintaining internal order and national identity. Exit options are constrained by international norms and treaties, but the core claim of exclusion remains.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, sovereign_states, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the perceived security, cultural preservation, and economic stability that border control is claimed to provide. They are the primary constituency for state actions on borders, often supporting restrictive policies. Their mobility is largely unaffected by the constraint.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_populations, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of exclusion, including denied entry, detention, deportation, and separation from families. Their options are severely limited, often facing dangerous journeys or prolonged limbo. They are the primary targets of border enforcement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, non_citizen_migrants, payer,
    powerless, immediate, trapped, global).

% Face significant barriers to entry, often being denied the right to claim asylum or subjected to prolonged processing in precarious conditions. While international law grants them specific protections, this reading of sovereignty often prioritizes state discretion over these rights, making their situation highly vulnerable.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Advocate for the rights of migrants and asylum seekers, challenging the absolute discretion of states. Their legal and moral arguments are often treated as external pressures or secondary considerations by states adhering to the sovereignty-primary view, rather than as constitutive elements of legitimate border governance.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the internal population and resources of a state by defining who belongs and who does not, enabling collective self-governance and resource allocation within defined territorial limits.
% TRANSFER_FUNCTION: Transfers the right to reside, work, and access social services from non-citizens to citizens, and transfers the costs of exclusion (e.g., detention, enforcement) to non-citizens and sometimes to the state's own budget.
% ABSENT_VOICES: Non-citizen migrants and asylum seekers are largely excluded from the policy-making process, despite being the primary targets of the constraint. International human rights advocates are often heard but their arguments are frequently dismissed as infringing on state sovereignty.
% DISAPPEARANCE_RATIONALE: If the principle of absolute state discretion over borders vanished, states would lose a foundational claim to control their territory and population. Global migration patterns would shift dramatically, national identities would be challenged, and the very concept of the 'nation-state' would undergo profound redefinition.
% FOUNDING_PROBLEM: The need for political communities to define their membership, control their territory, and protect their resources and cultural identity from external pressures.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and their citizen populations consistently attest to the live status of this problem, citing ongoing challenges to national security, economic stability, and cultural cohesion. International relations scholars and political theorists also corroborate the historical and ongoing salience of these foundational state concerns, even while critiquing the specific solutions.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.7) due to the severe costs imposed on non-citizens, including denial of fundamental rights and often dangerous conditions. Suppression is very high (0.9) as states employ extensive legal, physical, and technological means to prevent unauthorized entry, actively suppressing alternatives for migrants. Theater ratio is low (0.1) because the enforcement is largely functional in achieving its stated goal of exclusion, with minimal performative excess beyond the core function. Resistance is high (0.75) from migrants themselves, human rights organizations, and some international bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states, this is a legitimate exercise of self-determination and a necessary coordination mechanism. From the perspective of non-citizen migrants, it is a highly extractive and suppressive mechanism that denies fundamental freedoms. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and their citizen populations are the primary beneficiaries, gaining control and perceived security (low directionality). Non-citizen migrants and asylum seekers are the clear targets, bearing the full weight of exclusion and enforcement (high directionality). International human rights bodies are excluded from the core decision-making, acting as external critics.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_discretion_vs_human_rights,
    'To what extent does absolute state discretion to exclude conflict with or supersede international human rights obligations, particularly for asylum seekers?',
    'International court rulings on specific cases, or a new global treaty that explicitly redefines the scope of state sovereignty in relation to human mobility.',
    'If human rights obligations are found to limit or supersede absolute discretion, the constraint''s legitimacy would be undermined, and its extractiveness from vulnerable populations would be re-evaluated as unjust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_discretion_vs_human_rights, conceptual, 'The tension between state sovereignty and universal human rights in border control.').

omega_variable(
    constitutive_vs_contingent_statehood,
    'Is border control truly constitutive of statehood, or is it a contingent practice that has evolved alongside the modern state system and could be reconfigured?',
    'Historical and sociological analysis of state formation and evolution, or the emergence of new forms of political organization that do not rely on exclusive territorial control.',
    'If border control is found to be contingent, the ''mountain-like'' justification for its absolute nature would weaken, opening pathways for reclassification towards a more negotiable, policy-driven constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_contingent_statehood, empirical, 'Whether border control is an inherent feature of statehood or a mutable practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_control_legitimacy__sovereignty_primary, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(bord_tr_t1965, border_control_legitimacy__sovereignty_primary, theater_ratio, 1965, 0.07).
narrative_ontology:measurement(bord_tr_t1985, border_control_legitimacy__sovereignty_primary, theater_ratio, 1985, 0.09).
narrative_ontology:measurement(bord_tr_t2005, border_control_legitimacy__sovereignty_primary, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__sovereignty_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(bord_be_t1965, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(bord_be_t1985, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(bord_be_t2005, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(bord_su_t1965, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1965, 0.75).
narrative_ontology:measurement(bord_su_t1985, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(bord_su_t2005, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_control_legitimacy' kernel. This 'sovereignty_primary' reading emphasizes state discretion, while 'freedom_of_movement_primary' prioritizes individual rights and 'jurisdictional_sovereignty' focuses on regulatory authority. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
