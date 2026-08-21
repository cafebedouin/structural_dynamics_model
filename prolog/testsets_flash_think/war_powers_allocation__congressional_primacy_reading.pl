% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Primacy in War Powers Authorization
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint represents the 'congressional primacy' reading of the
 *   U.S. Constitution's war powers allocation, asserting that military force
 *   beyond immediate defense requires explicit congressional authorization.
 *   From this perspective, executive unilateral action constitutes an
 *   extraction from congressional war power, and attempts to assert
 *   congressional authority are met with high suppression. The constraint is
 *   claimed as a 'rope' because it is intended to be a coordination mechanism
 *   for war powers, but its actual operation, as described by this reading,
 *   is highly extractive due to frequent executive bypass.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.7).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.75).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, rope).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Primacy in War Powers Authorization").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, '2d8a24be-3e95-447a-8090-7e3120ad580c').
narrative_ontology:cs_kernel_codification('2d8a24be-3e95-447a-8090-7e3120ad580c', fixed_text).
narrative_ontology:cs_authority_grounding('2d8a24be-3e95-447a-8090-7e3120ad580c', lineage).
narrative_ontology:cs_interpretation_layer_present('2d8a24be-3e95-447a-8090-7e3120ad580c').
narrative_ontology:cs_reading_relation('2d8a24be-3e95-447a-8090-7e3120ad580c', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('2d8a24be-3e95-447a-8090-7e3120ad580c', war_powers_allocation__functional_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('2d8a24be-3e95-447a-8090-7e3120ad580c', foundational, congressional_declaration_sole_war_authority).
narrative_ontology:cs_axiom_status(congressional_declaration_sole_war_authority, holdable).
narrative_ontology:cs_axiom_grounding('2d8a24be-3e95-447a-8090-7e3120ad580c', congressional_declaration_sole_war_authority, deontological).
narrative_ontology:cs_axiom('2d8a24be-3e95-447a-8090-7e3120ad580c', foundational, executive_commander_in_chief_limited_to_defense).
narrative_ontology:cs_axiom_status(executive_commander_in_chief_limited_to_defense, holdable).
narrative_ontology:cs_axiom_grounding('2d8a24be-3e95-447a-8090-7e3120ad580c', executive_commander_in_chief_limited_to_defense, deontological).
narrative_ontology:cs_reference_frame('2d8a24be-3e95-447a-8090-7e3120ad580c', founding_era_constitutional_design).
narrative_ontology:cs_drift_state('2d8a24be-3e95-447a-8090-7e3120ad580c', post_cold_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d8a24be-3e95-447a-8090-7e3120ad580c', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, military_command).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, international_allies).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, legislative_branch).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, public_accountability_advocates).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, constitutional_checks_and_balances).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, democratic_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the body constitutionally mandated to declare war and authorize military force, it bears the cost of its power being bypassed or diminished by executive unilateral action. Its ability to assert this power is constrained by political costs and institutional inertia.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, legislative_branch, payer,
    institutional, generational, constrained, national).

% As Commander-in-Chief, the executive often initiates military action, interpreting its authority broadly. From this reading's perspective, when acting unilaterally beyond immediate defense, it extracts power from Congress. Its actions face political and legal challenges but often benefit from first-mover advantage.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, global).

% Benefits from clear, decisive orders and avoids potential political gridlock that could delay military operations. Its actions are bound by civilian control, but it generally prefers clear directives.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military_command, beneficiary,
    organized, immediate, constrained, global).

% Adjudicates constitutional disputes but often defers to the political branches on 'political questions' related to war powers, limiting its direct enforcement role. It observes and interprets the boundaries of power.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Bear the cost of reduced democratic oversight and accountability when war powers are exercised unilaterally. They advocate for congressional authorization and public debate, but their direct power to enforce this is limited.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, public_accountability_advocates, payer,
    organized, biographical, constrained, national).

% Benefit from perceived decisiveness and swift action by the executive, avoiding potential delays from congressional debate. They may prefer a strong, unified executive voice in foreign policy and military matters.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, international_allies, beneficiary,
    institutional, immediate, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__congressional_primacy_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__congressional_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the constitutional powers of the legislative and executive branches in the initiation and conduct of military force, ensuring democratic legitimacy and checks on executive power.
% TRANSFER_FUNCTION: Transfers the authority to initiate military force beyond immediate defense from the executive to the legislative branch, or, when bypassed, transfers power and accountability from Congress to the executive.
% ABSENT_VOICES: Citizens directly impacted by undeclared wars, who would demand greater democratic accountability and adherence to constitutional processes, are often absent from the decision-making process, their voices mediated through advocacy groups or electoral cycles.
% DISAPPEARANCE_RATIONALE: If the constitutional necessity for congressional authorization vanished, the executive branch would gain unchecked power to deploy military force, fundamentally altering the balance of powers, increasing the frequency and scope of military interventions, and diminishing democratic accountability for war.
% FOUNDING_PROBLEM: The framers sought to prevent unilateral executive war-making, vesting the power to declare war in Congress to ensure broad deliberation and democratic consent before committing the nation to conflict, drawing lessons from monarchical abuses of power.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, former members of Congress, and public interest groups consistently attest that the founding problem of preventing unilateral executive war-making remains live, citing numerous instances of executive military action without explicit congressional authorization. This is corroborated by historical analysis of constitutional intent and ongoing debates in legal and political discourse.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the significant erosion of congressional war-making authority due to executive actions without explicit authorization. Suppression (0.75) is high because executive claims of inherent authority and the political difficulty of challenging a sitting president's military decisions effectively suppress congressional attempts to reclaim its power. The theater ratio (0.4) indicates that while some executive consultations with Congress are genuine, a substantial portion serves to legitimize actions already decided, rather than seeking genuine authorization. Resistance (0.65) is moderate-high, reflecting ongoing efforts by some members of Congress, legal scholars, and advocacy groups to reassert congressional authority. The measurements show a trend of increasing extractiveness and suppression, particularly after the Cold War, reflecting a shift towards greater executive unilateralism.
 *
 * PERSPECTIVAL GAP:
 *   From the executive's perspective, the constraint might be seen as an impediment to decisive action, justifying broad interpretations of inherent authority. From the legislative branch's perspective (this reading), it is a vital check on power, whose erosion leads to a loss of democratic accountability. The engine's computation of per-seat classifications will highlight this divergence, showing the executive as a beneficiary of the constraint's practical erosion, and Congress as a victim.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and military command are beneficiaries when the constraint is bypassed, as they gain flexibility and decisiveness. International allies also benefit from perceived swift action. The legislative branch is a primary target/victim, as its constitutional power is diminished. Public accountability advocates are also victims, as the process becomes less transparent and democratically responsive. The judiciary acts as an observer, often deferring to the political branches.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_immediate_defense,
    'What constitutes ''immediate defense'' versus actions requiring authorization, and is this boundary consistently applied?',
    'Analysis of executive branch legal opinions and congressional responses to specific military actions over time, identifying patterns of interpretation and contestation.',
    'A narrow, consistently applied definition of ''immediate defense'' would strengthen congressional primacy; a broad, inconsistently applied definition would further legitimize executive unilateralism, increasing extraction from Congress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_immediate_defense, conceptual, 'Ambiguity in defining the scope of executive''s unilateral defensive power.').

omega_variable(
    judicial_deference_impact,
    'To what extent does judicial deference to ''political questions'' enable executive overreach in war powers, and could a more active judiciary alter the constraint''s operation?',
    'Hypothetical legal analysis of how different judicial doctrines (e.g., non-justiciability vs. robust review) would impact the outcomes of war powers challenges, or comparative analysis with systems where courts play a more active role.',
    'If judicial deference is a key enabler, a shift to more active judicial review could significantly reduce executive extraction and suppression of congressional power; if deference is merely a symptom, judicial action alone would have limited impact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_deference_impact, conceptual, 'The role of judicial deference in the balance of war powers.').

omega_variable(
    congressional_will_to_assert,
    'Is the erosion of congressional war powers primarily due to executive overreach, or to a lack of political will within Congress to assert its constitutional authority?',
    'Historical analysis of congressional voting records, legislative initiatives, and internal debates regarding war powers, alongside executive branch actions, to identify periods of active assertion versus acquiescence.',
    'If lack of will is primary, the constraint''s extractiveness is more a function of internal congressional dynamics than external executive pressure; if executive overreach is primary, then external enforcement mechanisms are more critical for restoring balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_will_to_assert, empirical, 'Internal vs. external factors driving congressional war powers erosion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1965, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(war__tr_t1985, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2001, 0.45).
narrative_ontology:measurement(war__tr_t2010, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(war__be_t1965, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(war__be_t1985, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1985, 0.65).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2001, 0.75).
narrative_ontology:measurement(war__be_t2010, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(war__su_t1965, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(war__su_t1985, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2001, 0.8).
narrative_ontology:measurement(war__su_t2010, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
