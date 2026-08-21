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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: State Territorial Sovereignty Entails Absolute Exclusion (Sovereignty Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty_primary' reading of the
 *   'border_control_legitimacy' kernel. It asserts that state territorial
 *   sovereignty inherently grants absolute discretion to exclude
 *   non-citizens, and that border control is a constitutive element of
 *   statehood itself. This reading prioritizes state autonomy and national
 *   interest above individual claims to freedom of movement or international
 *   human rights obligations. The structural delta for this reading is that
 *   excluded migrants are clearly victims, the enforcement apparatus is
 *   justified as a defense of sovereignty, and human rights are treated as
 *   external limits rather than internal components of legitimate authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.85).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.9).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "State Territorial Sovereignty Entails Absolute Exclusion (Sovereignty Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '726bbecb-3ad2-406e-b065-b1a7a7bfd527').
narrative_ontology:cs_kernel_codification('726bbecb-3ad2-406e-b065-b1a7a7bfd527', formalized).
narrative_ontology:cs_authority_grounding('726bbecb-3ad2-406e-b065-b1a7a7bfd527', lineage).
narrative_ontology:cs_interpretation_layer_present('726bbecb-3ad2-406e-b065-b1a7a7bfd527').
narrative_ontology:cs_reading_relation('726bbecb-3ad2-406e-b065-b1a7a7bfd527', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('726bbecb-3ad2-406e-b065-b1a7a7bfd527', border_control_legitimacy__jurisdictional_sovereignty, forecloses).
narrative_ontology:cs_axiom('726bbecb-3ad2-406e-b065-b1a7a7bfd527', foundational, state_territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(state_territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('726bbecb-3ad2-406e-b065-b1a7a7bfd527', state_territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('726bbecb-3ad2-406e-b065-b1a7a7bfd527', foundational, exclusion_as_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(exclusion_as_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('726bbecb-3ad2-406e-b065-b1a7a7bfd527', exclusion_as_constitutive_of_statehood, conventional).
narrative_ontology:cs_reference_frame('726bbecb-3ad2-406e-b065-b1a7a7bfd527', westphalian_state_model).
narrative_ontology:cs_drift_state('726bbecb-3ad2-406e-b065-b1a7a7bfd527', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('726bbecb-3ad2-406e-b065-b1a7a7bfd527', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_population).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, state_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, non_citizen_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, refugees_asylum_seekers).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, national_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The government and its enforcement agencies (border patrol, immigration services) that define and implement border policies. They claim absolute authority over who enters and resides in the territory, justifying it as essential for national security, economic stability, and cultural preservation. They benefit from maintaining control and the associated power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The citizens of the state, who are granted exclusive rights to residence, employment, and social benefits within the territory. They benefit from the perceived security, stability, and resource control that border enforcement provides, often viewing it as a defense of their national identity and way of life.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_population, beneficiary,
    organized, biographical, constrained, national).

% Individuals seeking to enter or reside in the state who are not citizens. They bear the direct costs of exclusion, including denial of entry, detention, deportation, and often dangerous journeys. Their movement is entirely at the discretion of the state, with few legal avenues for challenge.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, non_citizen_migrants, payer,
    powerless, immediate, trapped, global).

% Individuals fleeing persecution or conflict who seek protection in the state. While international law grants them specific rights, this reading of sovereignty often prioritizes state discretion, leading to their exclusion or prolonged detention. They face severe risks and lack effective exit options from their precarious situation.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, refugees_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Non-governmental organizations and activists who monitor border policies and advocate for the rights of migrants and refugees. They challenge the absolute discretion of states, arguing for human rights as a limit on sovereignty. They can influence public opinion and legal discourse but lack direct power over border enforcement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, human_rights_advocates, observer,
    organized, generational, analytical, global).

% Organizations like the UN and regional human rights courts that interpret and apply international law. They provide normative frameworks and judgments that challenge absolute state discretion, but their enforcement power is limited, often relying on state compliance or political pressure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_law_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, state_apparatus).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains a distinct political community with defined territorial boundaries, allowing for internal order, resource allocation, and the preservation of national identity for its citizens.
% TRANSFER_FUNCTION: Transfers security, stability, and exclusive access to national resources and social benefits to the citizen population and the state apparatus. It transfers the costs of exclusion, displacement, and economic hardship to non-citizen migrants and refugees.
% ABSENT_VOICES: Non-citizen migrants and refugees are structurally excluded from the political processes that determine border policy. If present, they would advocate for open borders, humanitarian access, and recognition of universal human rights that transcend state sovereignty.
% DISAPPEARANCE_RATIONALE: If state territorial sovereignty no longer entailed absolute discretion to exclude non-citizens, the fundamental nature of statehood, citizenship, and national borders would be radically transformed. Global migration patterns would shift dramatically, national identities would be redefined, and the international political order would undergo a profound reorganization.
% FOUNDING_PROBLEM: The problem of establishing and maintaining distinct political communities with self-determination, control over territory, and the ability to define their own membership and collective destiny.
% FOUNDING_PROBLEM_CORROBORATION: State governments, national security experts, and some political theorists attest that the founding problem of maintaining state integrity and control is still live and paramount. Human rights organizations and some international legal scholars contest this, arguing that the problem has evolved to include universal human rights obligations that limit absolute state discretion.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because this reading justifies the state's ability to deny entry and residence, imposing severe costs on non-citizens for the benefit of citizens and the state apparatus. Suppression is very high (0.90) as it involves active, often coercive, enforcement mechanisms to prevent entry and remove those deemed undesirable. Theater ratio is low (0.10) because border control is a highly functional and actively maintained aspect of state power, not primarily performative. Accessibility collapse is near total (0.95) for non-citizens, as the state's discretion is absolute. Resistance is high (0.70) from migrants and advocacy groups, but often ineffective against state power. The claimed type 'tangled_rope' reflects the dual function: it coordinates the state and its citizens while extracting from non-citizens through the same structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state apparatus and citizen population, this constraint is a legitimate and necessary mechanism for self-preservation and order. From the perspective of non-citizen migrants and human rights advocates, it is a highly extractive and suppressive mechanism that denies fundamental rights and imposes immense suffering. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and citizen population are clear beneficiaries (low directionality) as they gain security, control, and exclusive rights. Non-citizen migrants and refugees are clear targets (high directionality) as they bear the full cost of exclusion and have virtually no exit options. International law bodies and human rights advocates act as observers, analyzing and challenging the constraint without directly benefiting or paying its costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_policy,
    'Is absolute border control truly constitutive of statehood, or is it a policy choice that can be altered without dissolving the state?',
    'Comparative analysis of states that have adopted more open border policies or supranational arrangements (e.g., EU Schengen Area) to assess their impact on state integrity and function.',
    'If found to be a policy choice, the constraint''s claim of naturalness would be undermined, potentially reclassifying it closer to a Snare or Tangled Rope, and opening avenues for policy reform based on human rights or economic considerations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_policy, conceptual, 'Ambiguity between a fundamental feature of statehood and a mutable policy decision.').

omega_variable(
    proportionality_of_suppression,
    'Is the level of suppression (e.g., border militarization, detention, deportation) proportional to the actual threats to state security or economic stability posed by non-citizens?',
    'Independent empirical studies on the actual security and economic impacts of migration, compared against the costs and intensity of border enforcement measures.',
    'If suppression is found to be disproportionate, it would indicate excessive extraction beyond what is necessary for the claimed coordination function, strengthening a Snare classification and highlighting potential human rights violations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_suppression, empirical, 'Whether enforcement intensity matches actual threats or serves other extractive purposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_control_legitimacy__sovereignty_primary, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(bord_tr_t1965, border_control_legitimacy__sovereignty_primary, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(bord_tr_t1985, border_control_legitimacy__sovereignty_primary, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(bord_tr_t2005, border_control_legitimacy__sovereignty_primary, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(bord_tr_t2025, border_control_legitimacy__sovereignty_primary, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1945, 0.7).
narrative_ontology:measurement(bord_be_t1965, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(bord_be_t1985, border_control_legitimacy__sovereignty_primary, base_extractiveness, 1985, 0.8).
narrative_ontology:measurement(bord_be_t2005, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2005, 0.83).
narrative_ontology:measurement(bord_be_t2025, border_control_legitimacy__sovereignty_primary, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1945, 0.75).
narrative_ontology:measurement(bord_su_t1965, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(bord_su_t1985, border_control_legitimacy__sovereignty_primary, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(bord_su_t2005, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(bord_su_t2025, border_control_legitimacy__sovereignty_primary, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, national_citizenship_laws).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, international_human_rights_law).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_control_legitimacy' kernel, focusing on absolute state discretion. It is linked to sibling readings that offer alternative interpretations of sovereignty and migration rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
