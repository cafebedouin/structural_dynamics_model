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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: Combatant Status for National Liberation Movements (AP I Art 1(4) Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the 'national liberation' reading of combatant
 *   status, primarily derived from Additional Protocol I (AP I) Article 1(4)
 *   to the Geneva Conventions. It extends combatant status, and thus prisoner
 *   of war (POW) protections, to members of non-state armed groups fighting
 *   against colonial domination, foreign occupation, or racist regimes,
 *   provided they are organized and under responsible command. This reading
 *   is highly contested by states that view it as undermining the traditional
 *   state-centric framework of International Humanitarian Law (IHL).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.7).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.8).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Art 1(4) Reading)").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, 'e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924').
narrative_ontology:cs_kernel_codification('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', fixed_text).
narrative_ontology:cs_authority_grounding('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', lineage).
narrative_ontology:cs_interpretation_layer_present('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924').
narrative_ontology:cs_reading_relation('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', foundational, right_to_self_determination_in_armed_conflict).
narrative_ontology:cs_axiom_status(right_to_self_determination_in_armed_conflict, holdable).
narrative_ontology:cs_axiom_grounding('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', right_to_self_determination_in_armed_conflict, deontological).
narrative_ontology:cs_axiom('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', foundational, organized_non_state_actors_can_be_legitimate_combatants).
narrative_ontology:cs_axiom_status(organized_non_state_actors_can_be_legitimate_combatants, holdable).
narrative_ontology:cs_axiom_grounding('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', organized_non_state_actors_can_be_legitimate_combatants, conventional).
narrative_ontology:cs_reference_frame('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', post_api_protective_framework).
narrative_ontology:cs_drift_state('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', contemporary_counter_terrorism_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e24a2d4b-dc7d-44e7-ab4a-f748ac0b2924', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, human_rights_advocates).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_racist_regimes).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, right_to_self_determination).
narrative_ontology:constraint_vindicates(combatant_status_definition__national_liberation_reading, humanitarian_protection_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups, when organized and under responsible command, gain conditional combatant status and POW protections under this reading, shielding them from being treated as mere criminals. Their struggle is often existential, making exit unthinkable.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    organized, biographical, identity_locked, regional).

% These states are obligated by this reading to grant combatant status and POW protections to members of qualifying national liberation movements, even if they do not recognize the legitimacy of the conflict. This constrains their military and legal options, imposing costs.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, generational, constrained, global).

% Similar to occupying powers, these regimes face legal obligations to treat members of national liberation movements as combatants, which undermines their narrative of internal policing and imposes significant legal and political costs.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, colonial_racist_regimes, payer,
    institutional, generational, constrained, regional).

% Organizations like the ICRC and UN bodies interpret and advocate for the application of AP I Article 1(4), seeking to ensure its protective scope is realized. They set the interpretive agenda and monitor compliance.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_humanitarian_law_bodies, agenda_setter,
    institutional, civilizational, analytical, global).

% These groups benefit from the expansion of protective norms, as it aligns with their mission to ensure humane treatment and legal protections for all individuals in conflict. They leverage this reading in their advocacy.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% This group of scholars and practitioners would argue against extending combatant status to non-state actors, viewing it as an erosion of the traditional state-based framework of IHL. They are often excluded from the interpretive consensus that supports this reading.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_centric_legal_scholars, excluded,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a common legal framework for the conduct of hostilities and the protection of combatants in conflicts involving national liberation movements, thereby reducing brutality and ensuring minimum standards of humane treatment.
% TRANSFER_FUNCTION: Transfers legal protections (e.g., POW status, combatant immunity) to members of qualifying non-state armed groups, and transfers the burden of compliance and restraint onto occupying/colonial/racist regimes.
% ABSENT_VOICES: Military establishments and state-centric legal scholars of occupying powers, who would argue that this reading undermines state sovereignty, creates moral hazard for insurgent groups, and complicates traditional IHL frameworks. Their arguments are often marginalized in forums advocating for this reading.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, national liberation movements would universally be treated as criminals, leading to increased brutality, denial of fair trial rights, and potentially escalating conflicts as protective norms erode. The international legal landscape for self-determination conflicts would fundamentally shift.
% FOUNDING_PROBLEM: The traditional Geneva Conventions (1949) were primarily designed for inter-state conflicts, leaving a legal vacuum for non-state armed groups fighting against colonial domination, foreign occupation, or racist regimes, who were often denied combatant status and subjected to criminal prosecution.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, UN resolutions, and jurisprudence from international tribunals (e.g., ICTY) corroborate the historical context and ongoing relevance of this problem, despite resistance from some states.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.7) because this reading imposes significant obligations on powerful states (occupying/colonial regimes) to grant protections they would otherwise deny, thereby extracting their freedom of action. Suppression is also high (0.8) as these states actively resist and often deny the application of this interpretation, requiring continuous advocacy and enforcement efforts from IHL bodies. The theater ratio (0.4) reflects that while many states formally acknowledge AP I, its application in practice, especially in contemporary conflicts, often involves performative adherence while denying full protections. The metrics show a rise in extractiveness and suppression, particularly in the post-9/11 era, as states increasingly sought to deny combatant status to non-state actors, leading to greater contestation and enforcement pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national liberation movements, this reading is a vital protective measure and an affirmation of their right to self-determination. From the perspective of occupying powers, it is an illegitimate expansion of IHL that grants undue legitimacy to non-state actors and complicates military operations. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National liberation movements are clear beneficiaries (d=0.0-0.2) as they gain crucial legal protections. Occupying powers and colonial/racist regimes are the primary targets (d=0.8-1.0) as the constraint imposes obligations and limits their ability to prosecute opponents as criminals. IHL bodies and human rights advocates act as agenda-setters and secondary beneficiaries, respectively, working to uphold and expand these protections.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by adapting IHL to evolving forms of conflict, ensuring that the protective mandate of the Geneva Conventions remains relevant in non-traditional warfare. It counters the risk of the original mandate becoming obsolete by extending its scope, thus preventing the constraint from degrading into a piton or snare for liberation movements. However, for the occupying powers, it functions as a tangled rope, forcing coordination (adherence to IHL) while extracting their preferred freedom of action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_application_ambiguity,
    'What specific criteria define ''colonial domination, foreign occupation, or racist regimes'' in contemporary conflicts, and how are these criteria applied consistently?',
    'Development of authoritative international jurisprudence or a consensus-based interpretive guide from IHL bodies that provides clear, non-politicized definitions and application guidelines.',
    'Clearer definitions would reduce the ability of states to deny the application of AP I Article 1(4) by disputing the nature of the conflict, potentially increasing the constraint''s effective extractiveness on resistant states and reducing suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_application_ambiguity, conceptual, 'Ambiguity in defining the types of regimes targeted by AP I Article 1(4).').

omega_variable(
    organizational_control_threshold,
    'What constitutes ''organized and under responsible command'' for non-state armed groups in practice, and how is this assessed by neutral parties?',
    'Empirical studies of non-state armed group structures and command chains, combined with IHL expert consensus on minimum thresholds for recognition, to provide objective assessment criteria.',
    'A clearer, verifiable threshold would reduce the ability of states to deny combatant status by claiming insufficient organization, thereby strengthening the protective function of the constraint and increasing its effective extractiveness on denying states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_control_threshold, empirical, 'Uncertainty regarding the practical criteria for non-state group organization and command.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine expansion of IHL''s protective scope, or an overreach that undermines the state-centric foundation of international law?',
    'Long-term observation of state practice and international jurisprudence: if the protective scope is consistently applied without leading to systemic collapse of state-centric IHL, it supports the expansionist view. If it leads to widespread non-compliance and erosion of IHL, it supports the overreach view.',
    'Resolution would either solidify this reading''s legitimacy within IHL (reducing resistance) or lead to its formal or de facto repudiation (increasing suppression and reducing its protective effect).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, preference, 'Fundamental disagreement over the legitimacy and systemic impact of expanding combatant status to non-state actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.2).
narrative_ontology:measurement(comb_tr_t1987, combatant_status_definition__national_liberation_reading, theater_ratio, 1987, 0.25).
narrative_ontology:measurement(comb_tr_t1997, combatant_status_definition__national_liberation_reading, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(comb_tr_t2007, combatant_status_definition__national_liberation_reading, theater_ratio, 2007, 0.4).
narrative_ontology:measurement(comb_tr_t2017, combatant_status_definition__national_liberation_reading, theater_ratio, 2017, 0.45).
narrative_ontology:measurement(comb_tr_t2027, combatant_status_definition__national_liberation_reading, theater_ratio, 2027, 0.4).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.55).
narrative_ontology:measurement(comb_be_t1987, combatant_status_definition__national_liberation_reading, base_extractiveness, 1987, 0.6).
narrative_ontology:measurement(comb_be_t1997, combatant_status_definition__national_liberation_reading, base_extractiveness, 1997, 0.65).
narrative_ontology:measurement(comb_be_t2007, combatant_status_definition__national_liberation_reading, base_extractiveness, 2007, 0.7).
narrative_ontology:measurement(comb_be_t2017, combatant_status_definition__national_liberation_reading, base_extractiveness, 2017, 0.72).
narrative_ontology:measurement(comb_be_t2027, combatant_status_definition__national_liberation_reading, base_extractiveness, 2027, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.6).
narrative_ontology:measurement(comb_su_t1987, combatant_status_definition__national_liberation_reading, suppression_requirement, 1987, 0.65).
narrative_ontology:measurement(comb_su_t1997, combatant_status_definition__national_liberation_reading, suppression_requirement, 1997, 0.7).
narrative_ontology:measurement(comb_su_t2007, combatant_status_definition__national_liberation_reading, suppression_requirement, 2007, 0.8).
narrative_ontology:measurement(comb_su_t2017, combatant_status_definition__national_liberation_reading, suppression_requirement, 2017, 0.85).
narrative_ontology:measurement(comb_su_t2027, combatant_status_definition__national_liberation_reading, suppression_requirement, 2027, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, rules_of_engagement_for_non_state_actors).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, pow_status_determination).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, state_centric_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'combatant_status_definition' kernel, focusing on the extension of combatant status to national liberation movements. It is linked to sibling readings that offer alternative interpretations of combatant status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
