% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: National Liberation Combatant Status (AP I Art. 1(4))
 *   domain: legal/international_humanitarian
 *
 * SUMMARY:
 *   AP I Article 1(4) extends the definition of 'armed conflict' and
 *   combatant status to wars of national liberation against colonial
 *   domination, alien occupation, and racist regimes. This reading treats the
 *   provision as a living, structurally asymmetric coordination mechanism: it
 *   grants conditional combatant status (and thus POW protections) to
 *   non-state armed groups meeting organizational criteria, while imposing a
 *   corresponding obligation on the adverse party (the colonial/occupying
 *   power) to recognize that status. The constraint is not a natural law of
 *   war but a constructed treaty mechanism — a tangled rope with genuine
 *   coordination (reciprocal protections, distinction incentives) and
 *   asymmetric extraction (liberation movements gain status without full
 *   reciprocal compliance capacity; occupying powers bear obligations without
 *   control over the status grant). The metrics reflect the reading's own
 *   lights: moderate extractiveness for liberation movements (conditional
 *   status, organizational costs), high effective extraction for occupying
 *   powers (obligation without reciprocity), low theater (the coordination
 *   function is real and actively used), moderate suppression (enforcement
 *   through diplomatic pressure and ICRC monitoring, not pure coercion).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.38).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.55).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "National Liberation Combatant Status (AP I Art. 1(4))").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "legal/international_humanitarian").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '3fd248ea-fcfc-483f-9360-4fc25e27a824').
narrative_ontology:cs_kernel_codification('3fd248ea-fcfc-483f-9360-4fc25e27a824', formalized).
narrative_ontology:cs_authority_grounding('3fd248ea-fcfc-483f-9360-4fc25e27a824', lineage).
narrative_ontology:cs_interpretation_layer_present('3fd248ea-fcfc-483f-9360-4fc25e27a824').
narrative_ontology:cs_reading_relation('3fd248ea-fcfc-483f-9360-4fc25e27a824', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fd248ea-fcfc-483f-9360-4fc25e27a824', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('3fd248ea-fcfc-483f-9360-4fc25e27a824', foundational, liberation_movements_qualify_for_combatant_status).
narrative_ontology:cs_axiom_status(liberation_movements_qualify_for_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('3fd248ea-fcfc-483f-9360-4fc25e27a824', liberation_movements_qualify_for_combatant_status, conventional).
narrative_ontology:cs_axiom('3fd248ea-fcfc-483f-9360-4fc25e27a824', foundational, self_determination_justifies_status_extension).
narrative_ontology:cs_axiom_status(self_determination_justifies_status_extension, holdable).
narrative_ontology:cs_axiom_grounding('3fd248ea-fcfc-483f-9360-4fc25e27a824', self_determination_justifies_status_extension, deontological).
narrative_ontology:cs_reference_frame('3fd248ea-fcfc-483f-9360-4fc25e27a824', diplomatic_conference_1974_1977_consensus).
narrative_ontology:cs_drift_state('3fd248ea-fcfc-483f-9360-4fc25e27a824', contemporary_post_decolonization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3fd248ea-fcfc-483f-9360-4fc25e27a824', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, national_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, civilian_populations_under_occupation).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_colonial_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, state_armies_denied_reciprocity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, national_liberation_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-state armed groups fighting colonial, alien occupation, or racist regimes. They gain conditional combatant status and POW protections if organized under responsible command and carrying arms openly. Must meet AP I Art. 44 criteria (distinction, command, arms carrying). Their identity is fused to the liberation struggle; exit means abandoning the political cause that constitutes them.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, national_liberation_movements, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, national_liberation_movements, payer).

% Populations under colonial or racist regimes who benefit from the constraint's legitimization of armed resistance and the reciprocal protections it generates. They bear the costs of conflict but gain a legal framework that recognizes their right to self-determination. No meaningful exit from the occupation itself.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, civilian_populations_under_occupation, beneficiary,
    powerless, generational, trapped, local).

% States maintaining colonial, alien occupation, or racist regimes. They bear the obligation to grant combatant immunity and POW status to qualifying insurgents — a cost in operational flexibility and domestic political legitimacy. They set the enforcement agenda through non-ratification, reservations, and narrow interpretation. Can exit the constraint by ending the occupation (mobile at state level).
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_colonial_powers, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__national_liberation_reading, occupying_colonial_powers, agenda_setter).

% Regular armed forces of states facing liberation movements that claim Art. 1(4) status. They must extend POW protections to captured insurgents while often receiving no reciprocal compliance (insurgents may not respect Geneva conventions). Exit is constrained by chain of command and state obligation.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_armies_denied_reciprocity, payer,
    organized, biographical, constrained, national).

% Guardian of IHL treaties; monitors compliance, visits detainees, promotes interpretation. Sees the full structural asymmetry: liberation movements gain status without full reciprocal obligation; occupying powers bear costs without control over the status grant. Analytical seat with no material stake.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_committee_red_cross, observer,
    analytical, civilizational, analytical, universal).

% Major military powers (e.g., US, Israel, Turkey, India, Pakistan) that have not ratified AP I. They reject the Art. 1(4) extension as undermining the state monopoly on legitimate force. Their exclusion is structural — they remain bound by customary law but deny this specific treaty mechanism. Can arbitrate by selective compliance.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, non_ratifying_states, excluded,
    powerful, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the legal recognition of armed resistance against colonial/racist regimes by extending combatant status to non-state actors meeting organizational criteria, creating a reciprocal framework of protections and obligations where none existed under the 1949 Conventions.
% TRANSFER_FUNCTION: Transfers the legal privilege of combatant immunity and POW status from state armies exclusively to qualifying non-state liberation movements, and transfers the burden of granting those protections to the occupying/colonial power.
% ABSENT_VOICES: Insurgent groups fighting non-colonial regimes (e.g., internal secessionists, ideological rebels) are excluded from Art. 1(4) and would object to the colonial/racist threshold; their exclusion is structural. Also absent: civilian victims of liberation movements' tactics who fall outside the coordination function's protective scope.
% DISAPPEARANCE_RATIONALE: If Art. 1(4) combatant status vanished, liberation movements would lose conditional POW protections and revert to 'unprivileged belligerent' status under domestic law — exposing fighters to execution or prosecution for mere participation. Occupying powers would lose the legal obligation to treat captured insurgents as POWs. The legal architecture of self-determination struggles would collapse to pre-1977 rules.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions recognized combatant status only for state armed forces and organized resistance movements in occupied territory (Art. 4 GC III), leaving wars of national liberation against colonial and racist regimes in a legal void where insurgents were treated as criminals and civilians had no protected resistance framework.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC Commentary on AP I (1987) and the 1974-1977 Diplomatic Conference travaux préparatoires corroborate the decolonization mandate. However, major Western delegations (US, UK, France) filed statements at the Conference that the colonial era was ending and the provision was transitional — a contemporaneous corroboration from outside the beneficiary set that the founding problem was seen as time-bound.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).
:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the conditional nature: liberation movements must meet Art. 44 criteria (command, distinction, arms carrying) — organizational costs are real. Suppression (0.55) is moderate because the constraint operates through treaty obligation and customary law pull, not direct coercion; non-ratifying states exit via arbitrage. Theater (0.12) is low because the status grant has been invoked in practice (Namibia, South Africa, Palestine contexts) and ICRC monitoring is substantive. Accessibility collapse (0.42) is moderate — alternatives (domestic criminal law, 'terrorist' designation) remain available to occupying powers but carry diplomatic costs. Resistance (0.68) is high because the provision was and remains contested by major military powers.
 *
 * PERSPECTIVAL GAP:
 *   From the liberation movement's seat, this is a rope: genuine coordination solving the criminalization of resistance. From the occupying power's seat, it is a snare: obligation imposed without consent or reciprocity. From the ICRC's analytical seat, it is a tangled rope — the coordination function (reciprocal protections) is real but the extraction is asymmetric. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberation movements are structural beneficiaries (d ≈ 0.2): they gain combatant immunity conditional on organization, identity-locked to the struggle. Civilian populations are beneficiaries (d ≈ 0.1) but trapped in the occupation. Occupying powers are payers (d ≈ 0.85): they bear the obligation to grant POW status without control over whether the insurgent qualifies. State armies denied reciprocity are payers (d ≈ 0.75): they extend protections without receiving them. ICRC is analytical (d = 0.5). Non-ratifying states are excluded but arbitrage via selective customary compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (decolonization) is contested as live vs. resolved. If dead, the arrangement persists as a piton — theatrical maintenance of a status-grant mechanism whose primary referent has largely vanished (formal colonialism ended). If live (Palestine, Western Sahara, contested occupations), it remains a tangled rope with active coordination function. The mandatrophy tension is structural: the provision's legitimacy depends on the colonial/racist threshold, which is itself contested in contemporary conflicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_racist_threshold,
    'What regimes qualify as ''colonial domination, alien occupation, or racist regimes'' under Art. 1(4) in the post-decolonization era?',
    'State practice, ICJ advisory opinions, and ICRC interpretive guidance on whether contemporary situations (Palestine, Western Sahara, Kashmir, etc.) meet the threshold.',
    'If the threshold is narrow (formal colonialism only), the constraint''s coordination function atrophies → piton. If broad (includes effective control/annexation), the constraint remains a live tangled rope with active beneficiaries and payers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonial_racist_threshold, conceptual, 'Whether the Art. 1(4) trigger condition remains structurally operative or has become a vestigial category.').

omega_variable(
    reciprocity_asymmetry,
    'Does the liberation movement''s conditional combatant status require reciprocal compliance with IHL (distinction, proportionality, humane treatment) as a condition of retaining protections?',
    'ICRC Commentary Art. 44 analysis; state practice in detention and prosecution of captured insurgents; jurisprudence on forfeiture of POW status for IHL violations.',
    'If reciprocal compliance is required, the extraction asymmetry narrows → more rope-like. If status is granted unconditionally once organizational criteria are met, the occupying power''s extraction burden is higher → more snare-like from that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_asymmetry, empirical, 'Whether the coordination function includes a reciprocity mechanism that bounds the asymmetric extraction.').

omega_variable(
    kernel_reading_relations,
    'How does this reading''s structural relationship to the state_centric_reading and functional_protection_reading shape the combatant_status_definition kernel''s drift?',
    'Track treaty ratification patterns, ICJ/ICC jurisprudence citing each reading, and ICRC operational guidance to see which reading gains institutional traction.',
    'If state_centric_reading gains dominance (via non-ratification + customary law narrowing), this reading''s ε for liberation movements rises (protections erode). If functional_protection_reading absorbs the protective function, this reading''s coordination role diminishes → piton drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Commitment-system drift vector for the combatant_status_definition kernel across its three readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.05).
narrative_ontology:measurement(comb_tr_t1985, combatant_status_definition__national_liberation_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(comb_tr_t1995, combatant_status_definition__national_liberation_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement(comb_tr_t2005, combatant_status_definition__national_liberation_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(comb_tr_t2015, combatant_status_definition__national_liberation_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.22).
narrative_ontology:measurement(comb_be_t1985, combatant_status_definition__national_liberation_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(comb_be_t1995, combatant_status_definition__national_liberation_reading, base_extractiveness, 1995, 0.31).
narrative_ontology:measurement(comb_be_t2005, combatant_status_definition__national_liberation_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement(comb_be_t2015, combatant_status_definition__national_liberation_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.4).
narrative_ontology:measurement(comb_su_t1985, combatant_status_definition__national_liberation_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement(comb_su_t1995, combatant_status_definition__national_liberation_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(comb_su_t2005, combatant_status_definition__national_liberation_reading, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(comb_su_t2015, combatant_status_definition__national_liberation_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, protected_person_status__occupation_context).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, pow_treatment_obligations_non_international_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the combatant_status_definition kernel. The state_centric_reading and functional_protection_reading are sibling constraints with distinct ε profiles and stakeholder structures. All three form a constraint family. This reading's ε is moderate for liberation movements (conditional status) and high for occupying powers (obligation without control). The state_centric_reading has near-zero ε for states (maintains status quo) and very high ε for non-state actors (categorical exclusion). The functional_protection_reading has low ε universally (Common Art. 3 as floor).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, moderate, 0.2).
constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, powerless, 0.1).
constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, institutional, 0.85).
constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
