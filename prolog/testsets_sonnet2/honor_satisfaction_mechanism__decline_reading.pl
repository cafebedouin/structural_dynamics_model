% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor-Satisfaction Mechanism (Gradual Decline Reading)
 *   domain: historical sociology/legal history/normative systems
 *
 * SUMMARY:
 *   This story instantiates the decline_reading of the
 *   honor_satisfaction_mechanism kernel: dueling is treated as a practice
 *   that persisted across the interval at steadily falling frequency, moving
 *   from a routine (if always risky) elite institution toward fringe survival
 *   among diminishing pockets of officer-corps and aristocratic milieu,
 *   rather than becoming categorically unthinkable (contraction_reading) or
 *   explained by a bundle of distinct causal mechanisms (composite_reading).
 *   Under this reading, ε genuinely falls over the interval because both the
 *   social cost of dueling (rising legal risk, declining prestige payoff) and
 *   its actual enforcement as a live obligation both erode — but the practice
 *   itself remains conceptually available and occasionally exercised
 *   throughout, which is exactly what distinguishes decline from contraction.
 *   The claimed type (piton) reflects that by the interval's end the
 *   mechanism is mostly maintained by ritual specialists and incumbent
 *   prestige rather than by any live coordination need — the founding problem
 *   is dead by this reading's own corroborating outside sources, yet the
 *   practice does not vanish; it degrades into theatrical residue.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.42).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, piton).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor-Satisfaction Mechanism (Gradual Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical sociology/legal history/normative systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, '3e20db6c-c377-4501-b06e-eb8fe7cbbd44').
narrative_ontology:cs_kernel_codification('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', distributed).
narrative_ontology:cs_authority_grounding('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', practice).
narrative_ontology:cs_interpretation_layer_present('3e20db6c-c377-4501-b06e-eb8fe7cbbd44').
narrative_ontology:cs_reading_relation('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', foundational, practice_persists_as_available_option_through_decline).
narrative_ontology:cs_axiom_status(practice_persists_as_available_option_through_decline, holdable).
narrative_ontology:cs_axiom_grounding('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', practice_persists_as_available_option_through_decline, empirically_contingent).
narrative_ontology:cs_axiom('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', secondary, single_continuous_causal_trajectory_explains_frequency_fall).
narrative_ontology:cs_axiom_status(single_continuous_causal_trajectory_explains_frequency_fall, holdable).
narrative_ontology:cs_axiom_grounding('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', single_continuous_causal_trajectory_explains_frequency_fall, empirically_contingent).
narrative_ontology:cs_reference_frame('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', code_duello_gentlemanly_honor_norm).
narrative_ontology:cs_drift_state('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', late_nineteenth_century_fringe_status, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3e20db6c-c377-4501-b06e-eb8fe7cbbd44', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_culture_incumbents).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, duel_ritual_specialists).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duel_participants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, families_of_the_slain).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, lower_status_men_excluded_from_satisfaction).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, gentlemanly_status_requires_defensible_honor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold social rank whose defensibility historically ran through willingness to duel; as the frequency of duels declines they retain the residual prestige of the tradition without needing to bear its risks as often, collecting deference from the mechanism's continuing conceptual availability even while rarely invoking it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_culture_incumbents, beneficiary,
    powerful, generational, constrained, national).

% Seconds, code-duello arbiters, and fencing/pistol instructors whose livelihood and social utility depend on the mechanism remaining a recognized, if rarer, option. They administer the practice's remaining instances and have professional interest in its continued legitimacy, however diminished.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duel_ritual_specialists, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, duel_ritual_specialists, agenda_setter).

% Men whose honor is publicly challenged face a binary: duel (risking death or injury) or accept social diminishment. As the practice declines in frequency, the men who still find themselves compelled into it are disproportionately those in social milieus (military officer corps, some professional classes) where refusal remains costlier than the declining base rate would suggest.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duel_participants, payer,
    moderate, immediate, trapped, local).

% Bear the direct cost when a duel proves fatal — loss of income, social disruption, grief — with no recourse against a legally tolerated or lightly punished practice. They had no voice in the challenge and no ability to prevent it.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, families_of_the_slain, payer,
    powerless, biographical, trapped, local).

% Men outside the class recognized as entitled to duel (tradesmen, laborers) could be insulted or struck with no honor-satisfaction recourse at all — the mechanism's declining frequency among elites did nothing to extend or remove the exclusion that always applied to them; their remedy, where any existed, ran through inferior courts or none.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, lower_status_men_excluded_from_satisfaction, payer,
    powerless, biographical, trapped, local).

% Increasingly criminalize dueling over the interval, prosecute survivors with rising but inconsistent severity, and issue proclamations against the practice — their enforcement record tracks and partly drives the declining frequency without eliminating the underlying willingness among incumbents to invoke the mechanism when pressed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, state_legal_authorities, observer,
    institutional, generational, analytical, national).

% Journalists and moralists who campaigned against dueling on grounds of its wastefulness and cruelty had no formal seat in the honor culture's internal codes; their objections registered in public opinion and eventually law but were not part of the negotiated resolution of any individual affair of honor.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, dueling_press_and_commentators, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_mechanism__decline_reading, aristocratic_honor_culture_incumbents).
narrative_ontology:fixing_cost_class(honor_satisfaction_mechanism__decline_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a socially recognized, rule-bound procedure (the code duello) for resolving otherwise unresolvable disputes over reputation among status-equals, substituting a scripted, second-witnessed exchange for unregulated brawling or feud escalation.
% TRANSFER_FUNCTION: Moves physical risk and sometimes life from the party judged to have given or received an insult onto the bodies of the participants, while moving reputational capital toward whichever party is deemed to have behaved with courage and correctness under the code, regardless of who was factually right in the original dispute.
% ABSENT_VOICES: Families of slain participants, women (who could not duel and whose honor disputes were adjudicated entirely by male relatives on their behalf), and lower-status men denied standing under the honor code had no procedural voice in whether a challenge was issued or accepted.
% DISAPPEARANCE_RATIONALE: By the fringe-status endpoint of this reading, the mechanism's disappearance would rearrange very little in most participants' daily lives — legal dueling had already become rare enough that most social disputes were resolved through law, dueling-free social sanction, or simple avoidance. But within the shrinking aristocratic and military-officer milieu where the mechanism persisted longest, its full disappearance would still have removed a recognized, if diminished, currency of honor-repair with no substitute in place, which is why the verdict is contested rather than settled either way.
% FOUNDING_PROBLEM: Elite social orders lacked any legally sanctioned procedure for resolving disputes over personal reputation among status-equals that did not require submission to courts widely seen (in the mechanism's founding era) as beneath a gentleman's dignity, or endless feuding that risked wider violence.
% FOUNDING_PROBLEM_CORROBORATION: State legal authorities and contemporary press commentators — both outside the beneficiary set of aristocratic honor culture — attested by the mid-to-late period of decline that civil and criminal courts, alongside expanding print-mediated reputational remedies (libel actions, public retraction, press exposure), had fully displaced the practical function dueling once served; the mechanism's own remaining defenders (ritual specialists, incumbents) continued to assert the founding problem as live, which is precisely the self-interested assertion this corroboration field exists to flag.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction starts moderate-high (0.62) reflecting the real cost borne by participants and bystander families under a fully live honor code, and falls to 0.38 as declining social stakes and rising legal risk reduce both the frequency of duels and the coercive force compelling participation. Theater ratio rises steadily (0.2 to 0.55) precisely because the decline reading holds that the practice's persistence past its functional need is increasingly performative — surviving affairs of honor late in the period are more about symbolic continuity of an honor culture than genuine dispute-resolution necessity. Suppression_requirement rises through the middle of the interval (state prosecution intensifying) then plateaus rather than falling to zero, consistent with a practice that is legally suppressed but not eliminated — the signature of decline rather than contraction.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent/specialist seats, the declining frequency reads as the honor culture successfully self-regulating toward more measured, occasional use — a coordination mechanism maturing. From the participant and bystander seats, the same declining frequency reads as a coercive obligation that, while rarer, remains just as lethal and just as involuntary whenever it is actually invoked — decline in aggregate frequency does not translate into decline in per-instance stakes for whoever is still caught inside it.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic incumbents and ritual specialists sit near the beneficiary end: they collect prestige and professional relevance from the mechanism's continued (if rarer) availability without bearing its worst risks as often. Duel participants, families of the slain, and excluded lower-status men sit near the target end — they bear risk, grief, or total exclusion from the remedy the code claims to provide, and none of them administers or profits from the code's persistence. State authorities occupy an observer/adjudicator seat whose enforcement record is itself part of the declining-frequency data, not a beneficiary or victim position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status of 'dead' combined with a disappearance_verdict of 'contested' (rather than a clean world_unchanged) is the mandatrophy signature this reading is built to surface: courts and reputational remedies have displaced the mechanism's practical function everywhere except within a shrinking, self-perpetuating honor subculture that keeps the obligation alive through ritual specialists and residual incumbent prestige rather than through any live coordination need. Classifying it as piton (rather than snare or rope) captures that no concentrated beneficiary is extracting rents from a healthy going concern — the mechanism is instead running on inertia and performance, exactly the piton signature, even though its historical operation clearly harmed identifiable victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decline_vs_contraction_locus,
    'Is the observed falling frequency of dueling better modeled as gradual decline of a persistently available option (this reading), or as a category-level shift such that dueling became cognitively unthinkable for later cohorts (the contraction_reading) — i.e., is the disagreement about rate or about categorical availability?',
    'Examine late-period sources for evidence of dueling being treated as a live (if disfavored) option by contemporaries versus evidence that it had become literally inconceivable as a response to insult — private correspondence, memoirs, and legal testimony discussing dueling as a foreclosed category versus a declining-but-real option would discriminate between readings.',
    'If the contraction reading is correct, this story''s ε trajectory understates the true collapse and accessibility_collapse should approach the mountain range (0.85+) by interval''s end rather than plateauing at 0.4; if the decline reading is correct, the contraction reading overstates categorical foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_vs_contraction_locus, conceptual, 'Whether declining frequency reflects gradual decline or categorical unthinkability — the core kernel dispute.').

omega_variable(
    single_vs_composite_causal_mechanism,
    'Does one continuous decline process (declining social payoff, rising legal risk) explain the falling frequency, or did several structurally distinct mechanisms (state legal monopoly, bourgeois respectability norms, insurance/actuarial pressure, redefinition of what counts as insult) operate as separable causal channels that should be authored as separate constraints per the composite_reading?',
    'Disaggregate the historical record by mechanism and test whether each shows independent variation (e.g., insurance actuarial pressure rising while state prosecution intensity is flat) — independent variation across mechanisms would support decomposition into the composite_reading''s separate stories; correlated joint movement would support this single-trajectory reading.',
    'If mechanisms move independently, this single ε trajectory conflates causally distinct processes and the composite_reading''s decomposition is the structurally correct one; if they move together, this reading''s single continuous ε trajectory is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(single_vs_composite_causal_mechanism, empirical, 'Whether the decline is one process or several independently-varying displacing mechanisms.').

omega_variable(
    exclusion_persistence_across_readings,
    'Does the exclusion of lower-status men from honor-satisfaction remedy remain constant across all three kernel readings, or does its salience change depending on which reading is adopted?',
    'Compare victim-group framing across the three sibling stories: if lower_status_men_excluded_from_satisfaction appears with materially different weight or absence in the contraction_reading or composite_reading, the exclusion''s visibility is reading-dependent rather than a fixed historical fact.',
    'If exclusion visibility varies by reading, that is evidence the kernel readings differ not just in mechanism but in whose costs are foregrounded — a finding relevant to R3 corroboration checks across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_persistence_across_readings, conceptual, 'Whether the excluded lower-status victim group is treated consistently across sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_mechanism__decline_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_mechanism__decline_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(hono_tr_t40, honor_satisfaction_mechanism__decline_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(hono_tr_t60, honor_satisfaction_mechanism__decline_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(hono_tr_t80, honor_satisfaction_mechanism__decline_reading, theater_ratio, 80, 0.52).
narrative_ontology:measurement(hono_tr_t100, honor_satisfaction_mechanism__decline_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(hono_be_t40, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(hono_be_t60, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(hono_be_t80, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 80, 0.39).
narrative_ontology:measurement(hono_be_t100, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(hono_su_t40, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(hono_su_t60, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(hono_su_t80, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement(hono_su_t100, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_satisfaction_mechanism kernel. decline_reading (this file) holds a single continuous trajectory where dueling persists as a live, invoked-less-often option through fringe status. contraction_reading holds that dueling crossed a threshold into categorical unthinkability rather than mere rarity, and should author near-total accessibility_collapse. composite_reading decomposes the same history into several separately-authored displacing mechanisms (state monopoly, bourgeois norms, insurance, category-shift) rather than one trajectory. All three share the same underlying historical record but author different ε trajectories, different accessibility_collapse endpoints, and in composite_reading's case, different story counts entirely — exactly the ε-invariance principle's predicted decomposition when one label covers structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
