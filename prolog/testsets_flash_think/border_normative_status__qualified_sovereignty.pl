% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified State Sovereignty over Borders
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint represents the 'qualified_sovereignty' reading of the
 *   contested 'border_normative_status' kernel. It posits that states retain
 *   authority over their borders but must exercise it proportionately to
 *   legitimate state interests and consistently with human rights
 *   obligations. This reading attempts to balance state control with
 *   individual rights, standing between claims of absolute state sovereignty
 *   and universal freedom of movement. The metrics reflect a system that,
 *   while aiming for coordination, has seen increasing extraction and
 *   suppression over time, often with states performing compliance while
 *   acting otherwise.
 *
 * KEY AGENTS:
 *   - states_exercising_qualified_sovereignty: Agenda-setter (institutional/constrained) — bears adjudication burden
 *   - migrants_and_asylum_seekers: Primary target (powerless/trapped) — bears costs of restriction
 *   - international_human_rights_bodies: Agenda-setter (institutional/analytical) — monitors compliance
 *   - displaced_citizens: Secondary target (powerless/trapped) — bears costs of internal/return restrictions
 *   - human_rights_advocates: Beneficiary (organized/mobile) — uses framework to challenge abuses
 *   - pro_sovereignty_factions: Excluded (powerful/constrained) — advocates for less constraint
 *   - pro_freedom_of_movement_advocates: Excluded (organized/mobile) — advocates for more open borders
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.7).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.8).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.7).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified State Sovereignty over Borders").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '5d85f360-4a54-47be-93df-0fa2efd32d9a').
narrative_ontology:cs_kernel_codification('5d85f360-4a54-47be-93df-0fa2efd32d9a', formalized).
narrative_ontology:cs_authority_grounding('5d85f360-4a54-47be-93df-0fa2efd32d9a', lineage).
narrative_ontology:cs_interpretation_layer_present('5d85f360-4a54-47be-93df-0fa2efd32d9a').
narrative_ontology:cs_reading_relation('5d85f360-4a54-47be-93df-0fa2efd32d9a', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('5d85f360-4a54-47be-93df-0fa2efd32d9a', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('5d85f360-4a54-47be-93df-0fa2efd32d9a', foundational, state_sovereignty_is_qualified).
narrative_ontology:cs_axiom_status(state_sovereignty_is_qualified, holdable).
narrative_ontology:cs_axiom_grounding('5d85f360-4a54-47be-93df-0fa2efd32d9a', state_sovereignty_is_qualified, deontological).
narrative_ontology:cs_axiom('5d85f360-4a54-47be-93df-0fa2efd32d9a', foundational, human_rights_are_universal).
narrative_ontology:cs_axiom_status(human_rights_are_universal, holdable).
narrative_ontology:cs_axiom_grounding('5d85f360-4a54-47be-93df-0fa2efd32d9a', human_rights_are_universal, deontological).
narrative_ontology:cs_reference_frame('5d85f360-4a54-47be-93df-0fa2efd32d9a', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('5d85f360-4a54-47be-93df-0fa2efd32d9a', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5d85f360-4a54-47be-93df-0fa2efd32d9a', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, states_exercising_qualified_sovereignty).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, international_human_rights_bodies).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, migrants_and_asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states claim the right to control their borders but acknowledge obligations under international human rights law. They bear the burden of justifying border policies as necessary and proportionate, and face scrutiny from international bodies and human rights advocates. Their exit options are constrained by international legal frameworks and reputational costs.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, states_exercising_qualified_sovereignty, agenda_setter,
    institutional, generational, constrained, global).

% Individuals seeking entry or asylum, who bear the direct costs of border controls, including detention, deportation, and denial of entry. Their movement is restricted, and their claims are subject to state adjudication processes that may not always adhere to proportionality or human rights standards. They are often trapped between unsafe origins and closed borders.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, migrants_and_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Organizations like the UNHCR, human rights committees, and international courts that interpret, monitor, and enforce states' human rights obligations at borders. They provide guidance, investigate violations, and advocate for compliance, acting as a check on state power within this framework.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Citizens displaced within their own country or seeking to return, whose movement or access to safety may be restricted by state border or internal security measures, even if they hold citizenship. They face internal checkpoints, movement restrictions, or denial of return based on 'legitimate state interests'.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    powerless, immediate, trapped, national).

% NGOs and legal aid groups that use the framework of qualified sovereignty to challenge state actions, provide legal assistance to migrants, and advocate for stronger human rights protections. While they benefit from the existence of this legal framework, their work is an ongoing struggle against state practices.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Political parties, nationalist groups, and segments of the public who advocate for unfettered state control over borders, viewing human rights obligations as an infringement on national self-determination. They are excluded from the direct framing of this constraint but exert political pressure against its full implementation.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, pro_sovereignty_factions, excluded,
    powerful, biographical, constrained, national).

% Academics, activists, and organizations who argue for open borders and view any state control as an infringement on fundamental human rights. They are excluded from the direct framing of this constraint, which still legitimizes state control, but their arguments influence the broader debate.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, pro_freedom_of_movement_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for legitimate border governance that balances state security and economic interests with international human rights law, preventing humanitarian abuses while allowing states to manage their territories.
% TRANSFER_FUNCTION: Transfers the burden of justification, necessity, and proportionality onto states for their border policies, while simultaneously restricting the freedom of movement of non-citizens and, at times, displaced citizens.
% ABSENT_VOICES: Pro-sovereignty factions would object to any limitation on state control, arguing for absolute territorial sovereignty. Pro-freedom of movement advocates would object to any state-imposed border control, arguing for universal human mobility. Both are structurally excluded from the core premise of this 'qualified' approach.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states would either revert to a model of absolute sovereignty (unfettered exclusion, likely leading to severe humanitarian crises) or be forced towards open borders (a radical shift in global governance). The current international legal and political order, and the lives of millions of migrants, would fundamentally reorganize.
% FOUNDING_PROBLEM: Unfettered state sovereignty after World War II led to mass expulsions, denial of refuge, and severe human rights abuses at borders, necessitating a legal framework to balance state interests with individual dignity and international humanitarian principles.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and UN bodies consistently attest to the ongoing relevance of this framework, citing continued violations and humanitarian challenges at borders, particularly during contemporary migration crises. This corroboration comes from outside the immediate benefiting states.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.70) is high because, despite the 'qualified' nature, the system still heavily restricts movement and imposes significant costs on migrants and asylum seekers. Suppression (0.80) is also high, reflecting the active enforcement of borders and the limited alternatives for those seeking entry. The theater ratio (0.45) has risen over time, indicating that while states often articulate adherence to human rights, their actual practices (e.g., pushbacks, detention conditions) frequently diverge, creating a performative gap. Accessibility collapse is 0.70, as alternatives to state-controlled entry are largely foreclosed, but not absolutely, due to legal avenues for asylum. Resistance is moderate (0.50), reflecting ongoing challenges from human rights groups and the persistent efforts of migrants to cross borders.
 *
 * PERSPECTIVAL GAP:
 *   States often perceive this constraint as a necessary and legitimate exercise of their sovereign right, balanced by their obligations. However, from the perspective of migrants and human rights advocates, the 'qualification' often feels insufficient, with state interests frequently overriding human rights in practice. The engine's computation of per-seat types will likely show states as beneficiaries of a coordination mechanism, while migrants experience it as a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would imply symmetric benefits and minimal extraction) or a pure Snare (which would deny the genuine, albeit often unfulfilled, coordination function of balancing state interests with human rights). The rising extractiveness and theater ratio over time suggest a drift towards a more extractive operation, where the coordination story increasingly serves as cover for control.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_ambiguity,
    'How is ''proportionality'' actually measured and applied in state border policies, and to what extent do national courts and international bodies agree on its interpretation?',
    'Comparative legal analysis of national jurisprudence and international court rulings on border cases, identifying consistent vs. divergent application of proportionality tests.',
    'If proportionality is consistently applied and enforced, the constraint''s effective extractiveness would be lower, and its coordination function stronger. If it''s inconsistently or weakly applied, the constraint functions more as a cover for state discretion, increasing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_ambiguity, empirical, 'Ambiguity in the practical application of the proportionality principle.').

omega_variable(
    legitimate_state_interest_definition,
    'What constitutes a ''legitimate state interest'' in the context of border control, and how are these interests balanced against human rights obligations when they conflict?',
    'Analysis of state declarations, international legal commentary, and policy documents to identify the scope and hierarchy of ''legitimate state interests'' and their explicit or implicit prioritization against human rights.',
    'If ''legitimate state interests'' are broadly defined and consistently prioritized over human rights, the constraint leans towards the ''sovereignty_primary'' reading, increasing extraction. If narrowly defined and consistently subordinated to human rights, it leans towards ''freedom_primary'', reducing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_state_interest_definition, conceptual, 'Ambiguity in defining and prioritizing ''legitimate state interests''.').

omega_variable(
    sovereignty_vs_human_rights_priority,
    'Does this ''qualified sovereignty'' framework truly balance state interests and human rights, or does one implicitly take precedence in practice, despite the stated ideal?',
    'Empirical study of border outcomes (e.g., rates of asylum grant, detention, pushbacks) correlated with state justifications, to determine which principle consistently ''wins'' in cases of conflict.',
    'If state interests consistently override human rights, the constraint''s effective extractiveness is higher, functioning more as a Snare. If human rights consistently prevail, it functions more as a Rope. If genuinely balanced, it remains a Tangled Rope with fluctuating outcomes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_priority, empirical, 'Whether the stated balance between sovereignty and human rights is achieved in practice.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the ''qualified_sovereignty'' reading of the ''border_normative_status'' kernel. What specific structural elements differentiate it from its sibling readings?',
    'Comparison of the declared beneficiary/victim sets, enforcement mechanisms, and core axioms across the ''sovereignty_primary'' and ''freedom_primary'' readings.',
    'This reading introduces a burden of justification and proportionality on states, expanding the victim set to include displaced citizens, which is not present in the ''sovereignty_primary'' reading. It also legitimizes state control, unlike ''freedom_primary''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural differences between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__qualified_sovereignty, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(bord_tr_t1967, border_normative_status__qualified_sovereignty, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__qualified_sovereignty, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(bord_tr_t2001, border_normative_status__qualified_sovereignty, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__qualified_sovereignty, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__qualified_sovereignty, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__qualified_sovereignty, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(bord_be_t1967, border_normative_status__qualified_sovereignty, base_extractiveness, 1967, 0.4).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__qualified_sovereignty, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(bord_be_t2001, border_normative_status__qualified_sovereignty, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__qualified_sovereignty, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__qualified_sovereignty, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__qualified_sovereignty, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(bord_su_t1967, border_normative_status__qualified_sovereignty, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__qualified_sovereignty, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(bord_su_t2001, border_normative_status__qualified_sovereignty, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__qualified_sovereignty, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__qualified_sovereignty, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, refugee_status_determination).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, asylum_law_application).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_security_practices).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
