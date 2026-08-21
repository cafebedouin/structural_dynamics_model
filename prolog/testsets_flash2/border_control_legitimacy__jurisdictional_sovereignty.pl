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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Jurisdictional Sovereignty as Border Control Legitimacy
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents a reading of border control legitimacy that
 *   emphasizes jurisdictional sovereignty – the state's authority to regulate
 *   within its territory – but critically, does not equate this with absolute
 *   border closure. It posits that legitimate border control must balance
 *   protection obligations (e.g., for refugees), labor needs, and public
 *   consent. The constraint is a Tangled Rope because it genuinely
 *   coordinates these competing demands but also involves significant,
 *   actively enforced extraction from excluded migrants and, at times, from
 *   citizens whose interests are displaced.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.65).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.7).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Jurisdictional Sovereignty as Border Control Legitimacy").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, 'e4ed795d-34f6-4764-936c-239b7255ecb6').
narrative_ontology:cs_kernel_codification('e4ed795d-34f6-4764-936c-239b7255ecb6', formalized).
narrative_ontology:cs_authority_grounding('e4ed795d-34f6-4764-936c-239b7255ecb6', lineage).
narrative_ontology:cs_interpretation_layer_present('e4ed795d-34f6-4764-936c-239b7255ecb6').
narrative_ontology:cs_reading_relation('e4ed795d-34f6-4764-936c-239b7255ecb6', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('e4ed795d-34f6-4764-936c-239b7255ecb6', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('e4ed795d-34f6-4764-936c-239b7255ecb6', foundational, territorial_jurisdiction_is_foundational).
narrative_ontology:cs_axiom_status(territorial_jurisdiction_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('e4ed795d-34f6-4764-936c-239b7255ecb6', territorial_jurisdiction_is_foundational, conventional).
narrative_ontology:cs_axiom('e4ed795d-34f6-4764-936c-239b7255ecb6', foundational, border_control_must_balance_obligations_and_consent).
narrative_ontology:cs_axiom_status(border_control_must_balance_obligations_and_consent, holdable).
narrative_ontology:cs_axiom_grounding('e4ed795d-34f6-4764-936c-239b7255ecb6', border_control_must_balance_obligations_and_consent, deontological).
narrative_ontology:cs_reference_frame('e4ed795d-34f6-4764-936c-239b7255ecb6', post_westphalian_state_system_with_human_rights).
narrative_ontology:cs_drift_state('e4ed795d-34f6-4764-936c-239b7255ecb6', contemporary_global_migration_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e4ed795d-34f6-4764-936c-239b7255ecb6', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, host_state_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, host_state_employers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, host_state_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the state's ability to manage public services, maintain social cohesion, and protect national security. They also bear the costs of enforcement and potential social tensions from migration. Their consent is a key component of the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, host_state_citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__jurisdictional_sovereignty, host_state_citizens, payer).

% Benefit from access to a flexible labor supply, often at lower wages, which border controls can regulate. They can lobby for specific migration policies to meet labor needs.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, host_state_employers, beneficiary,
    powerful, immediate, mobile, national).

% Bear the direct costs of exclusion, including loss of opportunity, separation from family, and exposure to dangerous irregular migration routes. Their rights are acknowledged but often overridden by state interests.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Experience negative impacts from uncontrolled migration, such as wage depression, strain on public services, or cultural disruption, leading to a withdrawal of public consent for migration policies. Their concerns are part of the legitimacy balance.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens, payer,
    moderate, biographical, constrained, local).

% Monitor state compliance with international human rights law, including non-refoulement and the rights of migrants. They provide critical analysis and pressure for more humane border policies, acting as a check on state power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Implement and enforce border policies, balancing security, economic needs, and human rights obligations. They operate within the legal and political framework set by the state, but their actions directly shape the constraint's impact.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_border_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's need to manage its population and resources with its obligations under international law and the economic demands for labor, while maintaining public consent for migration policies.
% TRANSFER_FUNCTION: Transfers the right to reside and work within a territory from excluded migrants to host state citizens, while also transferring labor benefits to employers and security/social stability to citizens. It also transfers the costs of exclusion and enforcement.
% ABSENT_VOICES: Migrants who are denied entry and lack legal representation or collective bargaining power are largely absent from the policy-making process, though their plight is represented by human rights advocates. Future generations, who will inherit the long-term demographic and economic consequences of current policies, are also absent.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would lose a key mechanism for managing their populations, economies, and social contracts. Uncontrolled migration flows would lead to rapid demographic shifts, economic disruption, and potentially social unrest, forcing a complete re-evaluation of state functions and international relations.
% FOUNDING_PROBLEM: The problem of managing population flows across territorial boundaries to ensure state security, economic stability, and social cohesion, while also upholding human rights and international obligations.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and economists corroborate that the problem of balancing state interests with individual rights in migration remains a live and complex challenge, requiring ongoing negotiation and policy adjustment. Public opinion polls also reflect ongoing societal debates and concerns regarding migration management.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial due to the severe costs imposed on excluded migrants and the potential for displacement of citizen interests. Suppression (0.70) is high because active enforcement (border patrols, detention, deportation) is required to maintain the system. Theater ratio (0.20) is moderate; while some enforcement is genuinely about security and management, a portion is performative, aimed at signaling control to the domestic populace. The slight dip in extractiveness and suppression towards the end of the interval reflects increased legal challenges and public scrutiny, forcing some policy adjustments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state border agencies, this constraint is a necessary, if complex, coordination mechanism. From the perspective of excluded migrants, it is a highly extractive and suppressive barrier. Host state citizens experience it as a mixed bag, with benefits of order balanced against potential social costs. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Host state citizens and employers are beneficiaries, gaining from managed labor supply and social stability, though citizens also bear some costs. Excluded migrants are clear victims, facing severe restrictions and human rights violations. Displaced citizens are also victims when their interests are undermined by migration policies that lack public consent. International human rights bodies act as observers, while state border agencies are agenda-setters, implementing the policies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_of_enforcement,
    'Are the actual enforcement mechanisms (e.g., detention, deportation, border militarization) proportionate to the stated goals of border management and consistent with human rights obligations?',
    'Independent audits of border agency practices, judicial review of individual cases, and empirical studies on the effectiveness and human cost of specific enforcement measures.',
    'If enforcement is found disproportionate or rights-violating, the constraint''s effective extractiveness and suppression would be higher than currently measured, potentially reclassifying it closer to a Snare. If proportionate, it would reinforce its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_enforcement, empirical, 'Assesses whether border enforcement aligns with its stated legitimate aims and human rights standards.').

omega_variable(
    public_consent_measurement,
    'How is ''public consent'' for migration policies genuinely measured, and to what extent does it reflect informed deliberation versus xenophobic sentiment or economic anxiety?',
    'Deliberative polling, citizen assemblies on migration policy, and longitudinal studies tracking public attitudes alongside economic and social indicators, rather than relying solely on general election results or single-issue referenda.',
    'If public consent is found to be manipulated or based on misinformation, the legitimacy claim of this constraint would be undermined, increasing its effective extractiveness from both migrants and potentially from citizens whose true interests are not served. This could shift its classification towards a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_consent_measurement, conceptual, 'Examines the quality and authenticity of public consent as a legitimizing factor for border control.').

omega_variable(
    labor_needs_vs_migrant_rights,
    'To what extent do ''labor needs'' genuinely drive migration policy, versus serving as a justification for maintaining a vulnerable, exploitable labor pool?',
    'Economic analysis comparing declared labor shortages with actual employment data, wage trends in sectors employing migrants, and the legal protections afforded to migrant workers. Examination of guest worker programs for signs of tied labor or suppressed wages.',
    'If labor needs are primarily a cover for exploitation, the constraint''s extractiveness from migrants would be significantly higher, and the ''coordination'' function would be revealed as a facade for a Snare. If genuine, it supports the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_needs_vs_migrant_rights, empirical, 'Distinguishes genuine labor market coordination from exploitation under the guise of economic necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(bord_tr_t1970, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(bord_tr_t1990, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t2010, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(bord_be_t1970, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(bord_be_t1990, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(bord_be_t2010, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1948, 0.45).
narrative_ontology:measurement(bord_su_t1970, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(bord_su_t1990, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(bord_su_t2010, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, international_refugee_law).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, national_labor_market_regulation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'border_control_legitimacy' kernel. This reading emphasizes jurisdictional sovereignty balanced with obligations and consent, distinct from claims of absolute state discretion or fundamental freedom of movement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
