% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: State-Centric Combatant Status Definition (Geneva Article 4 Reading)
 *   domain: international_humanitarian_law/political
 *
 * SUMMARY:
 *   This constraint represents the state-centric reading of combatant status,
 *   primarily derived from Geneva Convention III, Article 4. It restricts
 *   prisoner of war (POW) protections to members of state armed forces,
 *   excluding non-state armed groups. This reading is a snare for non-state
 *   fighters, who are denied legal immunity and face criminal prosecution,
 *   while benefiting state parties by granting them legal flexibility. The
 *   high extractiveness and suppression reflect the severe consequences for
 *   those excluded from its protections.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.85).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.92).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, snare).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "State-Centric Combatant Status Definition (Geneva Article 4 Reading)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "international_humanitarian_law/political").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, 'f0212153-0d35-44bc-ba18-c2453cfa2506').
narrative_ontology:cs_kernel_codification('f0212153-0d35-44bc-ba18-c2453cfa2506', fixed_text).
narrative_ontology:cs_authority_grounding('f0212153-0d35-44bc-ba18-c2453cfa2506', lineage).
narrative_ontology:cs_interpretation_layer_present('f0212153-0d35-44bc-ba18-c2453cfa2506').
narrative_ontology:cs_reading_relation('f0212153-0d35-44bc-ba18-c2453cfa2506', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f0212153-0d35-44bc-ba18-c2453cfa2506', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('f0212153-0d35-44bc-ba18-c2453cfa2506', foundational, state_sovereignty_as_sole_source_of_legitimate_force).
narrative_ontology:cs_axiom_status(state_sovereignty_as_sole_source_of_legitimate_force, holdable).
narrative_ontology:cs_axiom_grounding('f0212153-0d35-44bc-ba18-c2453cfa2506', state_sovereignty_as_sole_source_of_legitimate_force, conventional).
narrative_ontology:cs_axiom('f0212153-0d35-44bc-ba18-c2453cfa2506', foundational, formal_organization_as_prerequisite_for_pow_status).
narrative_ontology:cs_axiom_status(formal_organization_as_prerequisite_for_pow_status, holdable).
narrative_ontology:cs_axiom_grounding('f0212153-0d35-44bc-ba18-c2453cfa2506', formal_organization_as_prerequisite_for_pow_status, conventional).
narrative_ontology:cs_reference_frame('f0212153-0d35-44bc-ba18-c2453cfa2506', post_geneva_1949_consensus).
narrative_ontology:cs_drift_state('f0212153-0d35-44bc-ba18-c2453cfa2506', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0212153-0d35-44bc-ba18-c2453cfa2506', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_military_personnel).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, insurgent_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret and enforce the Geneva Conventions, particularly Article 4, to limit combatant status and POW protections exclusively to members of state armed forces. They benefit from the legal flexibility to prosecute non-state fighters under domestic law.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_parties_to_geneva_conventions, agenda_setter,
    institutional, generational, constrained, global).

% As members of formally organized state militaries, they are guaranteed POW status and protections if captured, ensuring humane treatment and repatriation. This reading provides them with a clear legal shield.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_military_personnel, beneficiary,
    organized, biographical, mobile, global).

% Categorically denied POW status under this reading, they face prosecution as criminals under domestic law if captured, often for the mere act of fighting. They bear the full legal and physical costs of this exclusion.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_group_fighters, payer,
    powerless, immediate, trapped, regional).

% These groups, often fighting for self-determination or against oppressive regimes, find their members denied legal protections, complicating their ability to wage war and gain international legitimacy. They are forced to operate outside recognized legal frameworks.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, insurgent_movements, payer,
    moderate, generational, constrained, national).

% Analyze the legal implications and practical consequences of this state-centric interpretation, often highlighting its tension with evolving forms of conflict and the principle of humane treatment.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit narrow, legal framework for distinguishing lawful combatants from criminals in international armed conflicts, aiming to bring some order to warfare and protect state soldiers.
% TRANSFER_FUNCTION: Transfers legal immunity and humane treatment protections from non-state armed group fighters to state military personnel, and transfers the right to prosecute non-state fighters from international law to domestic criminal law.
% ABSENT_VOICES: Representatives of non-state armed groups and national liberation movements are largely excluded from the drafting and interpretation processes of these conventions, where they would advocate for broader recognition of combatant status based on functional criteria or the justice of their cause.
% DISAPPEARANCE_RATIONALE: If this state-centric definition vanished, the legal landscape of armed conflict would be profoundly altered. Non-state fighters might claim POW status, states would lose a key tool for prosecuting them, and the distinction between international and non-international armed conflict would blur, leading to a significant re-evaluation of legal protections in warfare.
% FOUNDING_PROBLEM: To regulate warfare by distinguishing between combatants (who receive protections) and civilians (who are protected from attack), and to ensure humane treatment for captured state soldiers, thereby limiting the brutality of war.
% FOUNDING_PROBLEM_CORROBORATION: State parties argue the problem of regulating warfare and protecting their soldiers remains live. Critics, including many IHL scholars and human rights organizations, argue that while the problem of regulating warfare is live, the state-centric solution has become an instrument of extraction, failing to address contemporary conflicts and creating legal vacuums for non-state actors. Independent legal analyses and reports from organizations like the ICRC highlight the gap between the legal framework and the realities of modern warfare.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__state_centric_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because non-state fighters lose fundamental legal protections, facing severe penalties for acts that would be lawful for state combatants. Suppression (0.92) is also very high, as states actively enforce this distinction through legal systems, military tribunals, and diplomatic pressure, effectively denying legal space for non-state actors. The theater ratio is low (0.1) because the distinction, while contested, is genuinely applied and has severe real-world consequences; it is not merely performative. Resistance is high (0.7) due to ongoing challenges from non-state groups and advocacy from IHL scholars for broader protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state parties, this definition is a necessary coordination mechanism for regulating warfare. From the perspective of non-state fighters, it is a snare designed to criminalize their struggle and deny them basic human rights in conflict. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and their military personnel are clear beneficiaries, gaining legal certainty and protection. Non-state armed group fighters and insurgent movements are the primary victims, facing severe legal and physical risks. IHL scholars act as observers, analyzing the structural implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolving_nature_of_conflict,
    'Does the state-centric definition adequately address the realities of modern armed conflicts, which increasingly involve non-state actors?',
    'Empirical analysis of conflict patterns and casualties in non-international armed conflicts (NIACs) compared to international armed conflicts (IACs), and the effectiveness of existing legal frameworks in protecting combatants and civilians in NIACs.',
    'If the definition is found to be increasingly inadequate, it would strengthen arguments for re-interpreting or amending IHL to extend protections, potentially shifting this constraint towards a Tangled Rope or even Rope for non-state actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(evolving_nature_of_conflict, empirical, 'The fit of the state-centric definition to contemporary warfare.').

omega_variable(
    legitimacy_of_non_state_actors,
    'Is the categorical exclusion of non-state actors from combatant status a legitimate legal distinction, or a political tool to delegitimize certain forms of armed resistance?',
    'Conceptual analysis of the historical development of IHL, the principle of equality of belligerents, and the political motivations behind state interpretations of Article 4. This would involve examining UN resolutions, state practice, and scholarly debates.',
    'If found to be primarily a political tool, it would reinforce the Snare classification and highlight the extractive nature of the constraint; if found to be a genuinely necessary legal distinction for maintaining order, it would push towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_non_state_actors, conceptual, 'The underlying justification for the state-centric exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1949, combatant_status_definition__state_centric_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__state_centric_reading, theater_ratio, 1977, 0.08).
narrative_ontology:measurement(comb_tr_t2001, combatant_status_definition__state_centric_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__state_centric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comb_be_t1949, combatant_status_definition__state_centric_reading, base_extractiveness, 1949, 0.6).
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__state_centric_reading, base_extractiveness, 1977, 0.7).
narrative_ontology:measurement(comb_be_t2001, combatant_status_definition__state_centric_reading, base_extractiveness, 2001, 0.8).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__state_centric_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1949, combatant_status_definition__state_centric_reading, suppression_requirement, 1949, 0.7).
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__state_centric_reading, suppression_requirement, 1977, 0.75).
narrative_ontology:measurement(comb_su_t2001, combatant_status_definition__state_centric_reading, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__state_centric_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'combatant_status_definition' kernel. Its state-centric interpretation directly influences the legal space available for alternative readings, such as those advocating for national liberation movements or broader functional protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
