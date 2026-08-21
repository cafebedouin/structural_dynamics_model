% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Freedom of Movement as Primary Right (Border Control Legitimacy Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'freedom_of_movement_primary' reading of
 *   the 'border_control_legitimacy' kernel. It asserts that freedom of
 *   movement is a fundamental human right, and state territorial sovereignty
 *   does not inherently grant authority for border closure. Instead, border
 *   control is viewed as an extractive mechanism that suppresses human
 *   rights. Sibling readings include 'sovereignty_primary' (absolute state
 *   discretion) and 'jurisdictional_sovereignty' (sovereignty as regulatory
 *   power, not exclusion).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.85).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.92).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Freedom of Movement as Primary Right (Border Control Legitimacy Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, 'df8712bc-cf6d-494f-be6b-66dec856bde6').
narrative_ontology:cs_kernel_codification('df8712bc-cf6d-494f-be6b-66dec856bde6', formalized).
narrative_ontology:cs_authority_grounding('df8712bc-cf6d-494f-be6b-66dec856bde6', extraction).
narrative_ontology:cs_interpretation_layer_present('df8712bc-cf6d-494f-be6b-66dec856bde6').
narrative_ontology:cs_reading_relation('df8712bc-cf6d-494f-be6b-66dec856bde6', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('df8712bc-cf6d-494f-be6b-66dec856bde6', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('df8712bc-cf6d-494f-be6b-66dec856bde6', foundational, freedom_of_movement_is_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('df8712bc-cf6d-494f-be6b-66dec856bde6', freedom_of_movement_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('df8712bc-cf6d-494f-be6b-66dec856bde6', foundational, state_sovereignty_is_limited_by_human_rights).
narrative_ontology:cs_axiom_status(state_sovereignty_is_limited_by_human_rights, holdable).
narrative_ontology:cs_axiom_grounding('df8712bc-cf6d-494f-be6b-66dec856bde6', state_sovereignty_is_limited_by_human_rights, deontological).
narrative_ontology:cs_reference_frame('df8712bc-cf6d-494f-be6b-66dec856bde6', universal_human_rights_framework).
narrative_ontology:cs_drift_state('df8712bc-cf6d-494f-be6b-66dec856bde6', contemporary_global_migration_crisis, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('df8712bc-cf6d-494f-be6b-66dec856bde6', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, citizens_of_wealthier_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, state_security_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, migrants_and_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_of_poorer_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Denied entry to desired territories, facing danger, economic hardship, and separation from family. They bear the direct costs of border closure.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migrants_and_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Unable to seek better economic opportunities or safety in other countries due to border closures, effectively trapped by their nationality and lack of entry rights. Their identity is tied to a state that cannot provide for them, but they cannot leave.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_of_poorer_states, payer,
    powerless, biographical, identity_locked, global).

% Benefit from perceived national security, cultural homogeneity, and potentially higher wages due to restricted labor supply. They also bear the indirect costs of border enforcement and the moral costs of exclusion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, citizens_of_wealthier_states, beneficiary,
    moderate, generational, mobile, national).

% Enforces border closures, maintaining its budget, personnel, and mandate. It justifies its existence by claiming to protect national interests and security from external threats.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue for the primacy of freedom of movement as a human right, documenting abuses and challenging state claims of absolute sovereignty. They operate through legal and advocacy channels but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% Claim the inherent right to control their borders as a fundamental aspect of sovereignty. This reading directly challenges the legitimacy of this claim, arguing it is superseded by human rights obligations.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, sovereign_states, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: States claim to coordinate national security, public services, and labor markets by controlling entry, ensuring orderly migration and protecting existing populations.
% TRANSFER_FUNCTION: Transfers the right to reside and work from non-citizens to citizens, and the costs of exclusion (danger, lost opportunity, human suffering) to migrants, while transferring resources and legitimacy to the state's enforcement apparatus.
% ABSENT_VOICES: Migrants and asylum seekers, who are directly affected but lack political voice in the states enforcing border closures. Also, global civil society organizations advocating for human rights, often excluded from policy-making.
% DISAPPEARANCE_RATIONALE: If border closure authority vanished, global migration patterns would shift dramatically, labor markets would rebalance, and the concept of national citizenship would be fundamentally altered. States would need to re-evaluate their social contracts and resource allocation, leading to a profound reorganization of global society.
% FOUNDING_PROBLEM: The perceived need for states to control their territory, manage populations, and protect national interests, particularly after the rise of the nation-state system and the Westphalian order.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and their security agencies attest to the ongoing necessity of border control for national security and economic stability. International human rights bodies and migration scholars, from outside the benefiting parties, contest this, arguing that the 'problem' is often a pretext for exclusion and rent-seeking, and that the original problem has been superseded by human rights norms.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) because the constraint denies a fundamental human right and imposes severe costs on migrants and displaced persons. Suppression is also very high (0.92) due to the active, often militarized, enforcement of borders and the lack of viable alternatives for those seeking entry. Theater ratio is low (0.1) because the enforcement is genuinely effective in preventing movement, not merely performative. Accessibility collapse is high (0.9) as physical and legal barriers are substantial. Resistance is high (0.7) from migrants, human rights organizations, and some international bodies. The claimed type is 'snare' because the coordination story (state security, resource management) is seen as a cover for pure extraction from vulnerable populations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of migrants and human rights advocates, border control is a clear snare, denying fundamental rights and imposing immense suffering. From the perspective of sovereign states and many citizens, it is presented as a necessary function of statehood and security, often framed as a 'rope' or 'mountain' for national stability. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Migrants and displaced persons are the primary targets (payers), bearing the full cost of exclusion. Citizens of wealthier states and the state security apparatus are the beneficiaries, gaining perceived security, economic advantages, and institutional mandate. Sovereign states, as agenda-setters, benefit from maintaining control, despite the human rights implications.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the mandate for absolute border closure, if it ever existed, has been superseded by international human rights law. The persistence of such control, despite its human cost and contestation, indicates a potential mandatrophy where the original 'problem' (state control) is now a pretext for extraction, rather than a legitimate coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_primacy,
    'Does state territorial sovereignty inherently grant absolute border closure authority, or is it fundamentally limited by international human rights law, particularly the right to freedom of movement?',
    'International legal precedent from courts with universal jurisdiction, or a global consensus shift in the interpretation of sovereignty and human rights.',
    'If human rights primacy is established, the constraint''s legitimacy collapses, leading to reclassification towards a ''snare'' or ''piton'' for states that continue to enforce closure. If absolute sovereignty is affirmed, the constraint might be re-evaluated as a ''mountain'' or ''rope'' from the state''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_primacy, conceptual, 'The fundamental legal and philosophical conflict between state sovereignty and human rights.').

omega_variable(
    economic_impact_of_open_borders,
    'What would be the actual economic and social impacts of a global regime of open borders on both sending and receiving states, considering both short-term disruption and long-term integration?',
    'Comprehensive, long-term empirical studies and economic modeling that account for dynamic effects, rather than static analyses or short-term projections.',
    'If the impacts are demonstrably net positive and manageable, it weakens the ''resource management'' justification for border closure. If demonstrably catastrophic, it could strengthen arguments for some form of managed migration, though not necessarily absolute closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_open_borders, empirical, 'The empirical consequences of open borders on global economies and societies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(bord_tr_t1968, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(bord_tr_t1988, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(bord_tr_t2008, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(bord_tr_t2024, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1968, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1968, 0.75).
narrative_ontology:measurement(bord_be_t1988, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1988, 0.8).
narrative_ontology:measurement(bord_be_t2008, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(bord_su_t1968, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1968, 0.8).
narrative_ontology:measurement(bord_su_t1988, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1988, 0.85).
narrative_ontology:measurement(bord_su_t2008, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2008, 0.9).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, international_refugee_law).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, national_citizenship_laws).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_control_legitimacy' kernel, which also includes 'sovereignty_primary' and 'jurisdictional_sovereignty' readings. Each reading has a distinct ε and stakeholder structure, reflecting different interpretations of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
