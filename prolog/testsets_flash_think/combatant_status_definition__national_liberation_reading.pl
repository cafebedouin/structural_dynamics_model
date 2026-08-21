% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: Combatant Status for National Liberation Movements (AP I Art 1(4) Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the 'national liberation' reading of combatant
 *   status under Additional Protocol I (AP I) Article 1(4) of the Geneva
 *   Conventions. It extends combatant status, and thus protections like
 *   prisoner-of-war (POW) status, to members of non-state armed groups
 *   fighting against colonial domination, alien occupation, and racist
 *   regimes, provided they meet criteria of organization and command control.
 *   This reading is highly contested by states that prefer a narrower,
 *   state-centric definition of combatant status, particularly in the context
 *   of asymmetric conflicts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.8).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.6).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Art 1(4) Reading)").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, 'adb302bf-9180-4b57-b652-c843f4f7ec51').
narrative_ontology:cs_kernel_codification('adb302bf-9180-4b57-b652-c843f4f7ec51', fixed_text).
narrative_ontology:cs_authority_grounding('adb302bf-9180-4b57-b652-c843f4f7ec51', lineage).
narrative_ontology:cs_interpretation_layer_present('adb302bf-9180-4b57-b652-c843f4f7ec51').
narrative_ontology:cs_reading_relation('adb302bf-9180-4b57-b652-c843f4f7ec51', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('adb302bf-9180-4b57-b652-c843f4f7ec51', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('adb302bf-9180-4b57-b652-c843f4f7ec51', foundational, right_to_self_determination_in_conflict).
narrative_ontology:cs_axiom_status(right_to_self_determination_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('adb302bf-9180-4b57-b652-c843f4f7ec51', right_to_self_determination_in_conflict, deontological).
narrative_ontology:cs_axiom('adb302bf-9180-4b57-b652-c843f4f7ec51', foundational, expanded_scope_of_ihl_applicability).
narrative_ontology:cs_axiom_status(expanded_scope_of_ihl_applicability, holdable).
narrative_ontology:cs_axiom_grounding('adb302bf-9180-4b57-b652-c843f4f7ec51', expanded_scope_of_ihl_applicability, conventional).
narrative_ontology:cs_reference_frame('adb302bf-9180-4b57-b652-c843f4f7ec51', api_humanitarian_expansion).
narrative_ontology:cs_drift_state('adb302bf-9180-4b57-b652-c843f4f7ec51', post_cold_war_asymmetric_conflicts, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('adb302bf-9180-4b57-b652-c843f4f7ec51', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, human_rights_advocates).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_racist_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups gain conditional combatant status and associated protections (e.g., POW status if captured) when fighting against colonial domination, alien occupation, or racist regimes, provided they meet organizational and command-control criteria. Their identity is often fused with the struggle for self-determination.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, generational, identity_locked, global).

% These states bear the burden of recognizing and treating qualifying non-state armed groups as combatants, rather than as mere criminals or terrorists. This limits their freedom of action and imposes legal obligations regarding detention, trial, and humane treatment.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, biographical, constrained, global).

% Similar to occupying powers, these regimes are legally constrained by this interpretation, which grants legitimacy and protection to those fighting against them. They face international pressure and legal challenges if they fail to adhere to these provisions.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_racist_regimes, payer,
    institutional, biographical, constrained, global).

% These bodies interpret and enforce international humanitarian law, including AP I, holding individuals accountable for war crimes. Their rulings shape the practical application and legitimacy of this combatant status definition.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_criminal_courts, agenda_setter,
    institutional, civilizational, analytical, global).

% These organizations benefit from the expanded protections offered by this reading, as it aligns with their goals of humanizing conflict and ensuring dignity and legal safeguards for all involved, particularly vulnerable groups.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, human_rights_advocates, beneficiary,
    organized, generational, analytical, global).

% Scholars who adhere strictly to a state-centric view of international law find this reading problematic, as it challenges traditional notions of sovereignty and the exclusive right of states to wage war. They are often excluded from the interpretive framework of this reading, though they participate in broader legal debates.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_centric_legal_scholars, excluded,
    analytical, generational, analytical, global).

% These experts analyze, interpret, and document the application and evolution of IHL, including the nuances and contestations surrounding combatant status for non-state actors. They provide critical commentary and influence legal discourse.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_humanitarian_law_experts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal framework for the conduct of hostilities involving non-state actors in specific contexts (anti-colonial, anti-occupation, anti-racist struggles), aiming to humanize conflict and ensure minimum protections by granting conditional combatant status.
% TRANSFER_FUNCTION: Transfers legal protections (e.g., POW status, immunity from prosecution for lawful acts of war) from the exclusive domain of state militaries to qualifying non-state armed groups, and transfers corresponding legal obligations (e.g., humane treatment, fair trial) to detaining powers.
% ABSENT_VOICES: States and legal scholars who maintain a narrow, strictly state-centric interpretation of combatant status are structurally excluded from the premise of this reading. They would argue against extending protections to non-state actors without strict state control, fearing a blurring of lines and legitimization of 'terrorism'.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, national liberation movements would universally be treated as criminals, escalating violence, removing incentives for adherence to IHL by non-state actors, and undermining the legitimacy of international law in these conflicts. The legal landscape of asymmetric warfare would fundamentally shift, likely leading to increased suffering and impunity.
% FOUNDING_PROBLEM: The need to address the legal status and protection of combatants in conflicts against colonial domination, alien occupation, and racist regimes, which were not adequately covered by traditional state-centric definitions of armed conflict and left combatants in these struggles without clear legal safeguards.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, UN resolutions, and a significant body of academic scholarship from outside the directly benefiting parties (liberation movements) corroborate the ongoing relevance and necessity of this interpretation, citing persistent conflicts of this nature and the continued need for humanitarian protection.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.8) because this reading imposes significant legal burdens and limits on the freedom of action of occupying powers and regimes, forcing them to treat insurgents with greater protections than they might otherwise. `Suppression` is moderate (0.6) because while the constraint aims to reduce the suppression of liberation movements, its application is actively resisted and often circumvented by states, requiring ongoing enforcement efforts. `Theater_ratio` is moderate (0.4) as international law often involves performative adherence, but the legal consequences of non-compliance are real. `Accessibility_collapse` is high (0.7) for states, as it significantly limits their ability to deny combatant status. `Resistance` is high (0.8) due to strong opposition from states that view this interpretation as undermining sovereignty or legitimizing non-state violence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national liberation movements and human rights advocates, this constraint is a vital 'rope' or 'scaffold' for justice and protection. From the perspective of occupying powers and state-centric scholars, it is an 'extraction' or 'snare' that undermines state authority and complicates conflict management. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   National liberation movements are clear beneficiaries, gaining legal protections that would otherwise be denied. Occupying powers and colonial/racist regimes are the primary targets/payers, as the constraint imposes obligations and limits their punitive options. Human rights advocates also benefit from the expanded scope of protection. State-centric legal scholars are 'excluded' in the sense that their core premise is challenged by this reading, though they remain active in the broader discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    criteria_ambiguity,
    'How are ''organized'' and ''command-controlled'' precisely defined and verified for non-state armed groups in practice?',
    'Further jurisprudence from international courts or detailed interpretive guidance from international legal bodies, clarifying the thresholds and evidentiary requirements for these criteria.',
    'Clearer criteria would reduce the ability of states to deny combatant status on technical grounds, potentially increasing the effective protection for liberation movements and raising the effective extraction on detaining powers. Ambiguity allows for continued contestation and selective application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criteria_ambiguity, empirical, 'Ambiguity in the practical application of organizational and command-control criteria for non-state groups.').

omega_variable(
    state_sovereignty_vs_humanitarian_protection,
    'To what extent does the expansion of combatant status to non-state actors genuinely undermine state sovereignty, versus merely re-balancing humanitarian obligations?',
    'Long-term empirical study of state behavior and conflict outcomes in jurisdictions where this interpretation is applied, assessing changes in state authority, conflict duration, and adherence to IHL by all parties.',
    'If the impact on sovereignty is minimal and humanitarian outcomes improve, the ''tangled_rope'' aspect (extraction from states) might be re-evaluated as a necessary cost of a broader ''rope'' (coordination for protection). If sovereignty is demonstrably undermined without clear humanitarian gains, the ''snare'' aspect for states would be amplified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_sovereignty_vs_humanitarian_protection, conceptual, 'The conceptual tension between state sovereignty and the expanded scope of humanitarian protection.').

omega_variable(
    non_recognition_impact,
    'How does the non-recognition of this interpretation by powerful states (e.g., the US, which has not ratified AP I) affect its de facto application and legitimacy?',
    'Analysis of state practice and legal arguments in conflicts involving non-ratifying states, and the extent to which international tribunals still apply the principles of AP I as customary international law.',
    'If non-recognition by powerful states significantly weakens the constraint''s enforcement, its effective suppression might be lower than measured, and its ''theater_ratio'' higher, indicating a more performative rather than binding function in certain contexts. If it''s widely accepted as customary law, the impact is less.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_recognition_impact, empirical, 'The practical impact of non-ratification of AP I by key states on this reading''s effectiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.3).
narrative_ontology:measurement(comb_tr_t1987, combatant_status_definition__national_liberation_reading, theater_ratio, 1987, 0.33).
narrative_ontology:measurement(comb_tr_t1997, combatant_status_definition__national_liberation_reading, theater_ratio, 1997, 0.36).
narrative_ontology:measurement(comb_tr_t2007, combatant_status_definition__national_liberation_reading, theater_ratio, 2007, 0.38).
narrative_ontology:measurement(comb_tr_t2017, combatant_status_definition__national_liberation_reading, theater_ratio, 2017, 0.39).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.6).
narrative_ontology:measurement(comb_be_t1987, combatant_status_definition__national_liberation_reading, base_extractiveness, 1987, 0.68).
narrative_ontology:measurement(comb_be_t1997, combatant_status_definition__national_liberation_reading, base_extractiveness, 1997, 0.73).
narrative_ontology:measurement(comb_be_t2007, combatant_status_definition__national_liberation_reading, base_extractiveness, 2007, 0.77).
narrative_ontology:measurement(comb_be_t2017, combatant_status_definition__national_liberation_reading, base_extractiveness, 2017, 0.79).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement(comb_su_t1987, combatant_status_definition__national_liberation_reading, suppression_requirement, 1987, 0.5).
narrative_ontology:measurement(comb_su_t1997, combatant_status_definition__national_liberation_reading, suppression_requirement, 1997, 0.55).
narrative_ontology:measurement(comb_su_t2007, combatant_status_definition__national_liberation_reading, suppression_requirement, 2007, 0.58).
narrative_ontology:measurement(comb_su_t2017, combatant_status_definition__national_liberation_reading, suppression_requirement, 2017, 0.59).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, common_article_3_protections).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'combatant_status_definition' kernel, focusing on the expansion of combatant status to national liberation movements under AP I Article 1(4). Its ε value differs significantly from the 'state_centric_reading' (which denies such status) and the 'functional_protection_reading' (which focuses on minimum humane treatment regardless of status).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
