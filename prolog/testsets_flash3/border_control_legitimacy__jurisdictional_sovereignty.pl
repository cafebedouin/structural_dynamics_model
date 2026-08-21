% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Jurisdictional Sovereignty Reading of Border Control Legitimacy
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'jurisdictional sovereignty' reading of
 *   border control legitimacy, where state sovereignty grants authority to
 *   regulate rights and obligations within territory, but not necessarily
 *   absolute border closure. Legitimacy is derived from balancing protection
 *   obligations (e.g., human rights) with labor needs and public consent. It
 *   acknowledges dual victim sets: both excluded migrants and citizens
 *   negatively impacted by unmanaged migration. The enforcement apparatus is
 *   constrained by proportionality and necessity, and a legitimacy crisis
 *   arises from violations of human rights or erosion of public consent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.45).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.6).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.45).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty Reading of Border Control Legitimacy").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '37c7e05c-af6d-4f99-aaaa-fd63996f9408').
narrative_ontology:cs_kernel_codification('37c7e05c-af6d-4f99-aaaa-fd63996f9408', formalized).
narrative_ontology:cs_authority_grounding('37c7e05c-af6d-4f99-aaaa-fd63996f9408', lineage).
narrative_ontology:cs_interpretation_layer_present('37c7e05c-af6d-4f99-aaaa-fd63996f9408').
narrative_ontology:cs_reading_relation('37c7e05c-af6d-4f99-aaaa-fd63996f9408', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('37c7e05c-af6d-4f99-aaaa-fd63996f9408', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('37c7e05c-af6d-4f99-aaaa-fd63996f9408', foundational, sovereignty_is_jurisdictional_not_absolute_closure).
narrative_ontology:cs_axiom_status(sovereignty_is_jurisdictional_not_absolute_closure, holdable).
narrative_ontology:cs_axiom_grounding('37c7e05c-af6d-4f99-aaaa-fd63996f9408', sovereignty_is_jurisdictional_not_absolute_closure, conventional).
narrative_ontology:cs_axiom('37c7e05c-af6d-4f99-aaaa-fd63996f9408', foundational, legitimacy_requires_balancing_obligations_needs_consent).
narrative_ontology:cs_axiom_status(legitimacy_requires_balancing_obligations_needs_consent, holdable).
narrative_ontology:cs_axiom_grounding('37c7e05c-af6d-4f99-aaaa-fd63996f9408', legitimacy_requires_balancing_obligations_needs_consent, deontological).
narrative_ontology:cs_reference_frame('37c7e05c-af6d-4f99-aaaa-fd63996f9408', post_westphalian_human_rights_framework).
narrative_ontology:cs_drift_state('37c7e05c-af6d-4f99-aaaa-fd63996f9408', contemporary_migration_crises_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('37c7e05c-af6d-4f99-aaaa-fd63996f9408', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, citizens_seeking_labor_protection).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, states_managing_labor_markets).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_by_unmanaged_migration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States assert the right to control who enters their territory to manage labor markets, protect social services, and maintain public order. They benefit from the ability to regulate the flow of labor and ensure public consent for migration policies. Their authority is constrained by international human rights law and the need to maintain internal legitimacy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, states_managing_labor_markets, agenda_setter,
    institutional, generational, constrained, national).

% Citizens benefit from policies that protect domestic labor markets from downward wage pressure and ensure the sustainability of social welfare systems. They exert political pressure on states to manage borders in line with their perceived interests and consent.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, citizens_seeking_labor_protection, beneficiary,
    organized, biographical, constrained, national).

% Individuals seeking entry to a territory for economic opportunity, asylum, or family reunification are denied entry or face significant barriers. They bear the direct costs of exclusion, including loss of opportunity, separation from family, and potential danger in transit or at borders. Their options are limited to illegal entry, seeking asylum elsewhere, or returning to their origin.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Citizens in areas experiencing high levels of unmanaged migration may face increased competition for jobs, strain on local services, or cultural friction. They bear diffuse costs and may experience a sense of displacement or loss of public consent for migration policies, leading to political resistance.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_by_unmanaged_migration, payer,
    moderate, biographical, constrained, local).

% Monitor state border practices against international human rights standards, including non-refoulement, due process, and the right to seek asylum. They provide legal interpretations and recommendations that influence the perceived legitimacy and legality of state actions, but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's need to manage its territory and population with its obligations under international law and the need to maintain public consent for migration policies, aiming for a balanced approach to border governance.
% TRANSFER_FUNCTION: Transfers the right to reside and work within a territory from the state to admitted migrants, while transferring the burden of exclusion and its associated costs to those denied entry. It also transfers political legitimacy from the public to the state in exchange for managed borders.
% ABSENT_VOICES: Migrant advocacy groups and some international legal scholars would argue for a stronger presumption of freedom of movement, challenging the state's broad discretionary power to exclude. They are often excluded from direct policy-making processes.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would lose a key mechanism for managing their populations and economies, leading to uncontrolled migration flows, potential social unrest, and a collapse of the current international legal framework for migration. The world would rapidly reorganize around new, likely more chaotic, forms of territorial control or open borders.
% FOUNDING_PROBLEM: The need to reconcile state sovereignty with the movement of people across borders, balancing national interests (security, economic stability, public consent) with international obligations (human rights, refugee protection).
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and many national governments attest that this problem remains live and highly contested, requiring ongoing negotiation and policy adjustments. Public opinion polls also consistently show a desire for managed, rather than open, borders.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).
:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely attempts to coordinate competing interests (state control, human rights, labor needs, public consent) but involves asymmetric extraction from both excluded migrants and, in some cases, citizens affected by unmanaged migration. Extractiveness (0.45) is moderate, reflecting the ongoing tension and the costs borne by those excluded or negatively impacted. Suppression (0.6) is significant, as states actively enforce border controls. Theater ratio (0.2) is low, indicating that while there's some performative aspect to border security, the core function of control is real. The metrics reflect the dynamic and contested nature of this reading, where the balance is constantly shifting.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states and citizens, this reading offers a legitimate framework for managing national interests. From the perspective of excluded migrants, it is a system of enforced exclusion, even if tempered by human rights considerations. The engine's per-seat classification will highlight this divergence, showing a more extractive experience for migrants despite the overall 'tangled rope' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   States and citizens seeking labor protection are beneficiaries, as the constraint allows for managed borders that serve their interests. Excluded migrants are clear victims, bearing the direct costs of denial. Citizens displaced by unmanaged migration are also victims, experiencing the negative externalities when the balance is not maintained. International human rights bodies act as observers, influencing the legitimacy discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_of_enforcement,
    'Are border enforcement measures genuinely proportional to the stated security and economic threats, or do they disproportionately impact vulnerable populations?',
    'Independent audits of border agency practices, legal challenges to specific enforcement tactics, and empirical studies on the effectiveness and human cost of different measures.',
    'If enforcement is found to be disproportionate, the constraint''s effective suppression and extractiveness would be re-evaluated upwards, potentially shifting its classification towards a Snare for excluded migrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_enforcement, empirical, 'Assesses whether enforcement aligns with the proportionality principle central to this reading.').

omega_variable(
    public_consent_authenticity,
    'Is ''public consent'' for migration policies genuinely informed and deliberative, or is it shaped by political rhetoric and media narratives that suppress nuanced understanding?',
    'Deliberative democracy experiments, long-term studies on public opinion formation regarding migration, and analysis of media framing and political discourse.',
    'If consent is found to be manipulated, the legitimacy grounding of the constraint would be weakened, and the ''beneficiary'' status of citizens seeking labor protection might be re-evaluated as a form of ''extracted consent'' through political theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_consent_authenticity, conceptual, 'Examines the quality and authenticity of public consent as a legitimizing factor.').

omega_variable(
    balance_of_obligations,
    'How is the balance between state protection obligations (e.g., human rights) and national interests (e.g., labor needs, public consent) actually struck in practice, and is it consistent with the stated principles of this reading?',
    'Comparative legal analysis of national migration policies, case studies of policy implementation, and assessments by international human rights bodies.',
    'If the balance consistently favors national interests over protection obligations, the extractiveness for excluded migrants would be higher than currently assessed, and the ''tangled rope'' classification might lean more towards ''snare'' due to a de facto prioritization of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balance_of_obligations, empirical, 'Evaluates the practical application of the balancing act central to this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.18).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.2).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.19).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.59).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_control_legitimacy' kernel. This 'jurisdictional sovereignty' reading attempts to balance state control with human rights and public consent, differing from the 'sovereignty primary' (absolute state discretion) and 'freedom of movement primary' (human right to movement) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
