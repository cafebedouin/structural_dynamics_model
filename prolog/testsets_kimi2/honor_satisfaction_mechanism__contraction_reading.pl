% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Honor Satisfaction through Dueling (Contraction Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism â dueling â was a normative system
 *   among European aristocratic and bourgeois males from roughly the
 *   seventeenth through nineteenth centuries. It required men of gentle
 *   status to defend their honor through personal combat upon insult or
 *   challenge, under elaborate procedural rules administered by seconds. The
 *   contraction reading holds that this constraint did not merely decline in
 *   frequency but became cognitively unthinkable: the category of 'satisfying
 *   honor through personal combat' dissolved as bourgeois rationalization and
 *   state monopoly on violence became hegemonic. The mechanism thus evacuated
 *   the possibility space rather than persisting as a fringe practice.
 *
 * KEY AGENTS:
 *   - gentlemanly_class: Primary beneficiary (powerful/constrained) â gains collective status maintenance and boundary policing
 *   - coerced_participants: Primary target (moderate/constrained) â bears mortality risk and social ostracism if refusing
 *   - seconds_and_code_interpreters: Agenda-setter (moderate/constrained) â administers ritual rules and interprets breaches
 *   - state_authorities: Observer (institutional/analytical) â criminalizes but ineffectually until cognitive shift
 *   - bourgeois_reformers: Excluded voice (moderate/mobile) â supplies the alternative cognitive frame that eventually displaces the practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.72).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.78).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Honor Satisfaction through Dueling (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '1a04ca0f-5296-4c40-97b0-50607b733e28').
narrative_ontology:cs_kernel_codification('1a04ca0f-5296-4c40-97b0-50607b733e28', distributed).
narrative_ontology:cs_authority_grounding('1a04ca0f-5296-4c40-97b0-50607b733e28', practice).
narrative_ontology:cs_interpretation_layer_present('1a04ca0f-5296-4c40-97b0-50607b733e28').
narrative_ontology:cs_reading_relation('1a04ca0f-5296-4c40-97b0-50607b733e28', honor_satisfaction_mechanism__decline_reading, forecloses).
narrative_ontology:cs_reading_relation('1a04ca0f-5296-4c40-97b0-50607b733e28', honor_satisfaction_mechanism__composite_reading, coexists_with).
narrative_ontology:cs_axiom('1a04ca0f-5296-4c40-97b0-50607b733e28', foundational, category_collapse_precedes_behavioral_extinction).
narrative_ontology:cs_axiom_status(category_collapse_precedes_behavioral_extinction, holdable).
narrative_ontology:cs_axiom_grounding('1a04ca0f-5296-4c40-97b0-50607b733e28', category_collapse_precedes_behavioral_extinction, empirically_contingent).
narrative_ontology:cs_axiom('1a04ca0f-5296-4c40-97b0-50607b733e28', foundational, honor_satisfaction_is_cognitively_constituted).
narrative_ontology:cs_axiom_status(honor_satisfaction_is_cognitively_constituted, holdable).
narrative_ontology:cs_axiom_grounding('1a04ca0f-5296-4c40-97b0-50607b733e28', honor_satisfaction_is_cognitively_constituted, conventional).
narrative_ontology:cs_reference_frame('1a04ca0f-5296-4c40-97b0-50607b733e28', aristocratic_honor_culture).
narrative_ontology:cs_drift_state('1a04ca0f-5296-4c40-97b0-50607b733e28', modern_bourgeois_hegemony, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('1a04ca0f-5296-4c40-97b0-50607b733e28', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, gentlemanly_class).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__contraction_reading, coerced_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Males of gentle birth whose social standing depended on public willingness to duel; the mechanism preserved their collective status boundaries against bourgeois outsiders and resolved honor disputes among insiders according to an implicit ranked code.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, gentlemanly_class, beneficiary,
    powerful, biographical, constrained, national).

% Men drawn into duels by insult or formal challenge who would prefer to decline but face total social ostracism, dishonor, and effective expulsion from professional and marital markets if they refuse; they bear the physical mortality risk and psychological burden of the ritual.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, coerced_participants, payer,
    moderate, biographical, constrained, national).

% Experienced duelists and military officers who administered the procedural rules, arranged meetings, vouched for weapon equality, and interpreted breaches of etiquette; their authority derived from mastery of the implicit code and their role gave them standing without eliminating their own exposure to the practice.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, seconds_and_code_interpreters, agenda_setter,
    moderate, biographical, constrained, regional).

% Legal and state apparatus that repeatedly criminalized dueling but lacked effective enforcement within the gentlemanly class; their prohibition was systematically circumvented or ignored until the cognitive shift made enforcement eventually congruent with social reality.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_authorities, observer,
    institutional, generational, analytical, national).

% Emerging commercial and professional classes who rejected aristocratic blood-honor codes and promoted legal-rational dispute resolution; initially excluded from the honor discourse but gradually provided the cognitive frame that made dueling unthinkable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, bourgeois_reformers, excluded,
    moderate, generational, mobile, national).

narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized ritual for resolving interpersonal honor disputes among social equals, channeling potentially chaotic violence into regulated form and preserving the internal hierarchy of the gentlemanly class without recourse to state courts.
% TRANSFER_FUNCTION: Transferred physical risk, mortality, and psychological burden from the social collective to the individual duelist; transferred status affirmation and boundary-maintenance from the ritual's performance to the gentlemanly class as a whole.
% ABSENT_VOICES: Women were entirely excluded from the honor discourse and had no voice in its operation; bourgeois merchants and religious reformers who rejected blood honor were present in society but lacked standing within the aristocratic frame; medical professionals who treated wounds were structurally silent on the institution's legitimacy.
% DISAPPEARANCE_RATIONALE: The gentlemanly class's internal status order depended on the dueling mechanism as a boundary marker; its disappearance forced honor to be signaled through wealth, education, legal procedure, and professional credentialing, dissolving a key distinction between aristocratic and bourgeois masculinity and rearranging the upper-class social order.
% FOUNDING_PROBLEM: How to resolve interpersonal insults and status challenges among armed social equals without degenerating into feuds, ambushes, or total social warfare that would destabilize the aristocratic estate.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists including Norbert Elias and subsequent sociological historians attest the founding problem from outside the benefiting parties; the gentlemanly class itself attested the problem as live during the institution's operation, but no independent corroboration exists from non-participant seats at the time.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because participants were compelled to risk death and injury to maintain social standing; the cost was bodily and existential, not merely monetary. Suppression is higher (0.78) because refusal triggered not legal penalty but social death â ostracism from marriage, profession, and identity-community. Theater ratio rises over the lifecycle (endpoint 0.92) because as the practice became rarer it grew more performative and ritualized before final evacuation. Accessibility collapse is very high (0.88): once inside the honor culture, no recognized alternative to dueling existed â state courts were seen as dishonorable and refusal meant expulsion from the class. Resistance is moderate-low (0.35) because while state and church formally opposed dueling, internal peer enforcement consistently neutralized external resistance until the cognitive frame itself dissolved.
 *
 * PERSPECTIVAL GAP:
 *   The gentlemanly_class experienced the constraint as identity-affirming coordination that preserved their collective standing; the coerced_participants experienced the same structure as lethal extraction backed by social annihilation. The engine computes this divergence from the structural data: identical spatial scope and power level produce radically different effective extraction depending on beneficiary versus payer role.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentlemanly_class sits near the beneficiary end (low d) because the constraint subsidizes their status boundaries and collective identity. Coerced_participants sit near the full-target end (high d) because the constraint extracts mortality risk directly from them. Seconds_and_code_interpreters sit near symmetric: they derive standing from administering the constraint but remain personally exposed to its dangers. State authorities are analytically outside the extraction flow. Bourgeois_reformers are outside the constraint entirely until their cognitive reframing dissolves it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â resolving honor disputes among equals without feuds â was genuinely solved by dueling in the aristocratic context. As state monopoly and legal-rational norms advanced, the founding problem became dead. The contraction reading claims the constraint did not persist as a piton (theatrical maintenance of an atrophied function) because the cognitive category itself collapsed, making the practice literally unimaginable rather than merely unfashionable. This prevents mislabeling the terminal phase as inertial performance when it was actually structural evacuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_behavioral_disappearance,
    'Did dueling disappear because the cognitive category dissolved first, or because behavioral opportunity structures (state enforcement, urbanization) removed the material conditions?',
    'Comparative historical analysis of jurisdictions where dueling persisted longer despite similar material conditions; microhistorical study of duelists'' discourse for category-presence vs category-absence.',
    'If cognitive category persisted while behavior declined, the contraction reading weakens toward the decline reading; if category dissolution preceded behavioral extinction, contraction reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_vs_behavioral_disappearance, empirical, 'Whether structural contraction or material opportunity decline drove disappearance').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression within dueling culture structural (external ostracism, legal threat) or internalized (shame, honor-identity fusion)?',
    'Post-exit trajectory of men who refused duels: if they suffered primarily external penalties, suppression is structural; if they reported self-loathing or identity collapse, suppression was substantially internalized.',
    'If internalized, effective extraction exceeds structural measures because targets carry the constraint after nominal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    contraction_vs_decline_foreclosure,
    'Does the contraction reading''s claim of category-level impossibility foreclose the decline reading''s claim of gradual behavioral persistence?',
    'Archaeological and archival evidence of dueling incidents after the supposed cognitive threshold; if incidents persist as fringe practice, decline reading survives and contraction reading must soften.',
    'If decline reading is empirically supported, relation shifts from forecloses to influences or coexists_with, altering kernel dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contraction_vs_decline_foreclosure, empirical, 'Whether category evacuation forecloses gradual decline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hono_tr_t5, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(hono_tr_t10, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(hono_tr_t15, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(hono_tr_t25, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement(hono_tr_t30, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 30, 0.75).
narrative_ontology:measurement(hono_tr_t35, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 35, 0.92).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(hono_be_t5, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(hono_be_t10, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(hono_be_t15, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(hono_be_t25, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(hono_be_t30, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(hono_be_t35, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 35, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hono_su_t5, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hono_su_t10, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(hono_su_t15, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(hono_su_t25, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(hono_su_t30, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(hono_su_t35, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 35, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, bourgeois_status_signaling).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_mechanism kernel decomposes into three readings (contraction, decline, composite) because the natural-language label 'the decline of dueling' conflates structurally distinct claims about cognitive possibility, behavioral frequency, and causal mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
