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
 *   human_readable: Border Control Legitimacy (Freedom of Movement Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story analyzes border control legitimacy from the reading
 *   that prioritizes freedom of movement as a fundamental human right,
 *   arguing that territorial sovereignty does not inherently grant states the
 *   authority to close borders. From this perspective, existing border
 *   controls are highly extractive and suppressive, primarily benefiting
 *   state security apparatuses and domestic labor market controllers at the
 *   expense of migrants, asylum seekers, and even displaced citizens. The
 *   constraint is claimed as a 'snare' because its coordination function (if
 *   any) is seen as cover for systematic extraction and suppression of
 *   movement.
 *
 * KEY AGENTS:
 *   - state_security_apparatus: Agenda setter (institutional/constrained) — enforces border closures, benefits from expanded authority.
 *   - domestic_labor_market_controllers: Beneficiary (organized/mobile) — benefits from controlled labor supply.
 *   - migrants_and_asylum_seekers: Payer (powerless/trapped) — bears the direct costs of exclusion.
 *   - displaced_citizens_and_workers: Payer (moderate/constrained) — impacted by restrictions on their own movement.
 *   - international_human_rights_advocates: Observer (organized/analytical) — challenges legitimacy based on human rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.85).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.9).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Control Legitimacy (Freedom of Movement Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '3eb145b4-3c96-49a9-9a39-56e98406e8c8').
narrative_ontology:cs_kernel_codification('3eb145b4-3c96-49a9-9a39-56e98406e8c8', formalized).
narrative_ontology:cs_authority_grounding('3eb145b4-3c96-49a9-9a39-56e98406e8c8', extraction).
narrative_ontology:cs_interpretation_layer_present('3eb145b4-3c96-49a9-9a39-56e98406e8c8').
narrative_ontology:cs_reading_relation('3eb145b4-3c96-49a9-9a39-56e98406e8c8', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('3eb145b4-3c96-49a9-9a39-56e98406e8c8', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('3eb145b4-3c96-49a9-9a39-56e98406e8c8', foundational, freedom_of_movement_is_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('3eb145b4-3c96-49a9-9a39-56e98406e8c8', freedom_of_movement_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('3eb145b4-3c96-49a9-9a39-56e98406e8c8', foundational, territorial_sovereignty_is_conditional).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('3eb145b4-3c96-49a9-9a39-56e98406e8c8', territorial_sovereignty_is_conditional, deontological).
narrative_ontology:cs_reference_frame('3eb145b4-3c96-49a9-9a39-56e98406e8c8', universal_human_rights_framework).
narrative_ontology:cs_drift_state('3eb145b4-3c96-49a9-9a39-56e98406e8c8', contemporary_global_migration_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3eb145b4-3c96-49a9-9a39-56e98406e8c8', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, state_security_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, domestic_labor_market_controllers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, migrants_and_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_and_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces border closures, justifying actions as essential for national security and sovereignty. Benefits from expanded budgets and authority derived from perceived threats. This reading delegitimizes their core function.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, state_security_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the ability to control labor supply and demand by restricting entry, potentially suppressing wages in certain sectors or ensuring a compliant workforce. This reading challenges their ability to use borders for economic leverage.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, domestic_labor_market_controllers, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of border closures, including physical danger, separation from family, economic hardship, and denial of fundamental rights. Their movement is actively suppressed, often with severe consequences.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, migrants_and_asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Citizens or legal residents who are prevented from moving freely across borders due to restrictive policies, impacting their economic opportunities, family reunification, or personal liberty. This reading recognizes them as victims of border control.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_citizens_and_workers, payer,
    moderate, biographical, constrained, national).

% Advocate for the primacy of human rights, including freedom of movement, over absolute state sovereignty. They analyze border regimes through the lens of international law and human dignity, challenging the legitimacy of current practices.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the existing border control system primarily coordinates the exclusion of non-citizens, rather than solving a genuine collective action problem for all humanity. It coordinates state power to maintain territorial integrity and control population flows.
% TRANSFER_FUNCTION: Transfers the right to move and reside freely from individuals (migrants, asylum seekers, displaced persons) to the state, which then allocates this right based on national interest, often extracting economic or security benefits from the exclusion.
% ABSENT_VOICES: The voices of those denied entry, those trapped in transit zones, and those whose lives are directly imperiled by border closures are systematically excluded from the policy-making process. Their perspectives would fundamentally challenge the legitimacy of current border regimes.
% DISAPPEARANCE_RATIONALE: If border closure authority vanished overnight, global migration patterns would fundamentally shift, labor markets would rebalance, and the concept of national citizenship would be profoundly altered. States would need to re-evaluate their foundational principles and develop new mechanisms for managing population flows and resource distribution.
% FOUNDING_PROBLEM: The problem of managing population movements, ensuring national security, and maintaining social cohesion within defined territories.
% FOUNDING_PROBLEM_CORROBORATION: States and their security apparatuses assert the problem is live and requires robust border controls. International human rights bodies and migration scholars, from outside the benefiting parties, argue that the 'problem' is often a pretext for exclusion and that the founding problem of managing movement can be addressed through less restrictive means that prioritize human rights.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.85) and suppression (0.90) reflect the severe costs imposed on individuals whose fundamental right to movement is denied. The 'snare' classification is chosen because the coordination narrative (national security, orderly migration) is, from this reading, primarily a cover for the coercive exclusion of specific populations. The rising extractiveness and suppression over time reflect the increasing securitization of borders and the intensification of enforcement mechanisms since the mid-20th century.
 *
 * PERSPECTIVAL GAP:
 *   The state security apparatus and domestic labor market controllers would experience this as a legitimate 'rope' or even a 'mountain' (sovereignty as natural law), essential for national order. Migrants and human rights advocates, however, experience it as a 'snare' that systematically denies fundamental rights and imposes immense suffering. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The state security apparatus and domestic labor market controllers are beneficiaries (low d) as they gain power and economic leverage. Migrants and asylum seekers are full targets (high d) due to their trapped status and direct extraction. Displaced citizens are also targets, albeit with slightly more exit options than non-citizens. International human rights advocates are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the mandate for absolute border control has outlived its ethical justification, if it ever had one. What was once framed as a necessary function of statehood (a 'mountain' or 'rope') has, through the lens of human rights, become a mechanism for extraction and suppression ('snare'). The classification prevents mislabeling this as coordination by highlighting the identifiable victims and the coercive nature of its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_primacy,
    'Is state territorial sovereignty an absolute right that precedes and overrides individual human rights, or is it a conditional authority constrained by international human rights law?',
    'International legal precedent from the International Court of Justice or a global human rights tribunal explicitly adjudicating the hierarchy of these claims, or a new global treaty establishing a universal right to movement.',
    'If sovereignty is absolute, this constraint would be reclassified closer to a ''mountain'' or ''rope'' from the state''s perspective. If human rights are primary, the ''snare'' classification is reinforced, and state border control becomes a violation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_primacy, conceptual, 'The fundamental conceptual conflict between state sovereignty and individual human rights in international law.').

omega_variable(
    economic_impact_of_open_borders,
    'What would be the net economic impact (GDP, wages, public services) of a global regime of open borders, accounting for both benefits (labor mobility, innovation) and costs (adjustment, infrastructure)?',
    'Comprehensive, long-term empirical studies and economic modeling across diverse national contexts, free from political bias, to quantify the effects of significantly liberalized migration.',
    'If net benefits are overwhelmingly positive, the economic justification for border closures would be undermined, shifting the ''domestic_labor_market_controllers'' from beneficiary to a seat whose ''benefit'' is a net social cost. If costs are prohibitive, it would strengthen arguments for managed migration, though not necessarily for absolute closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_open_borders, empirical, 'The empirical economic consequences of open versus closed borders.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of movement structural (external barriers, legal penalties) or internalized (fear, lack of information, psychological barriers)?',
    'Post-liberalization migration patterns: if movement remains low even after legal and physical barriers are removed, reclassify as partially internalized suppression. If movement surges, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. If purely structural, removing barriers would immediately increase movement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in border control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(bord_be_t1970, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(bord_be_t1990, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(bord_be_t2010, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(bord_be_t2024, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(bord_su_t1970, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(bord_su_t1990, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(bord_su_t2010, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(bord_su_t2024, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'border_control_legitimacy' kernel. It is structurally distinct from the 'sovereignty_primary' and 'jurisdictional_sovereignty' readings, which emphasize state discretion or regulatory authority, respectively. Each reading yields a different epsilon and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
