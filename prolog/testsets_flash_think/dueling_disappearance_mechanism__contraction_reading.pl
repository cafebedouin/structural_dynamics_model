% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Cultural Unthinkability of Dueling (Dignity Culture Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story describes the cultural shift that made dueling
 *   unthinkable, framed as the displacement of honor-culture axioms by
 *   dignity-culture axioms. From the perspective of dignity culture, the
 *   absence of dueling is a fundamental, almost natural, feature of social
 *   order. The constraint is claimed as a Mountain, reflecting this
 *   'irreversible substrate' view. However, the presence of identifiable
 *   beneficiaries (dignity-culture adherents, legal system) and victims
 *   (honor-culture practitioners) means this is a False Summit Mountain
 *   candidate, which the engine will reclassify (by default to Tangled Rope)
 *   to reflect the underlying extraction from the displaced honor culture.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.15).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.25).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Cultural Unthinkability of Dueling (Dignity Culture Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'fa568a03-a358-4c13-8b02-c41b58521e5c').
narrative_ontology:cs_kernel_codification('fa568a03-a358-4c13-8b02-c41b58521e5c', implicit).
narrative_ontology:cs_authority_grounding('fa568a03-a358-4c13-8b02-c41b58521e5c', practice).
narrative_ontology:cs_interpretation_layer_present('fa568a03-a358-4c13-8b02-c41b58521e5c').
narrative_ontology:cs_reading_relation('fa568a03-a358-4c13-8b02-c41b58521e5c', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa568a03-a358-4c13-8b02-c41b58521e5c', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('fa568a03-a358-4c13-8b02-c41b58521e5c', foundational, individual_dignity_is_inalienable).
narrative_ontology:cs_axiom_status(individual_dignity_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('fa568a03-a358-4c13-8b02-c41b58521e5c', individual_dignity_is_inalienable, deontological).
narrative_ontology:cs_axiom('fa568a03-a358-4c13-8b02-c41b58521e5c', foundational, state_monopoly_on_violence).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence, holdable).
narrative_ontology:cs_axiom_grounding('fa568a03-a358-4c13-8b02-c41b58521e5c', state_monopoly_on_violence, conventional).
narrative_ontology:cs_reference_frame('fa568a03-a358-4c13-8b02-c41b58521e5c', dignity_culture_supremacy).
narrative_ontology:cs_drift_state('fa568a03-a358-4c13-8b02-c41b58521e5c', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fa568a03-a358-4c13-8b02-c41b58521e5c', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, legal_system).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, rule_of_law_supremacy).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, individual_autonomy_over_collective_honor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a society where personal dignity is protected by law and civil means, rather than requiring violent self-redress. They are the primary carriers and enforcers of the new cultural norms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    institutional, generational, analytical, national).

% Their traditional framework for dispute resolution and status defense became culturally illegitimate and legally punishable. They bear the cost of their cultural framework becoming illegible and unviable, leading to social marginalization or legal sanction if they attempt to adhere to it.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, excluded).

% The state's monopoly on violence and its authority in dispute resolution are significantly enhanced by the cultural displacement of dueling. It actively codifies and enforces the new norms, benefiting from increased legitimacy and scope.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Study the historical transition from honor to dignity culture and the mechanisms by which dueling became culturally unthinkable. They analyze the structural shifts and their consequences without direct participation.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, analytical_historians, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates social interactions around legal and civil means of dispute resolution, replacing the honor-based system where dueling served to coordinate status disputes and maintain social hierarchy.
% TRANSFER_FUNCTION: Transfers the right and responsibility for resolving grievances from individuals (via private violence) to the state (via law and civil processes), thereby transferring social capital and authority to state institutions and dignity-culture norms.
% ABSENT_VOICES: Advocates for honor culture, who would defend dueling as a necessary mechanism for maintaining personal and collective honor, are structurally excluded. Their framework became unintelligible and illegitimate within the dominant dignity culture.
% DISAPPEARANCE_RATIONALE: If the cultural substrate of dignity culture vanished overnight, the social fabric around dispute resolution, personal reputation, and state authority would be fundamentally altered. Honor culture might re-emerge, making dueling thinkable again, and the legal system's legitimacy would be challenged.
% FOUNDING_PROBLEM: The perceived social instability, violence, and cycles of retribution inherent in honor-based dispute resolution, and the desire for a more ordered, state-mediated system of justice and social interaction.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, sociologists, and contemporary legal codes corroborate the historical shift towards state-mediated justice and the rejection of private violence. The ongoing societal preference for civil dispute resolution over personal combat attests to the problem's continued salience.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the constraint's status *after* the cultural shift has largely occurred. Extractiveness (0.15) and suppression (0.25) are low because dueling is no longer a live option for most, and its absence is maintained by internalized cultural norms rather than active coercion. Accessibility collapse is high (0.9) because the very idea of dueling has become culturally unthinkable. Resistance is low (0.1) as there is no significant movement to revive dueling. The claimed type is 'mountain' because the dignity culture acts as a new, seemingly natural, and unchangeable social substrate. The temporal measurements show a gradual decline in extractiveness and suppression as the dignity culture solidified its dominance over the two-century interval.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity-culture adherents, the constraint is a beneficial, natural evolution towards a more civilized society. From the perspective of honor-culture practitioners, it represents a profound loss and an imposed cultural extraction that renders their identity and values obsolete. The engine's reclassification of this claimed Mountain to a more extractive type (e.g., Tangled Rope) captures this perspectival divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity-culture adherents and the legal system are structural beneficiaries (low directionality), as they gain from a society where disputes are resolved civilly and state authority is paramount. Honor-culture practitioners are the primary targets/victims (high directionality), as their entire framework for social interaction and status defense became illegible and unviable, effectively extracting their cultural capital and agency.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''contraction_reading'' of the ''dueling_disappearance_mechanism'' kernel?',
    'Analysis of historical primary sources and sociological theories to confirm the primacy of cultural axiom displacement in the decline of dueling, distinct from institutional or legal changes.',
    'If misidentified, the analysis of dueling''s disappearance would be structurally flawed, potentially misattributing causality and misclassifying the underlying mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated.').

omega_variable(
    causal_primacy_ambiguity,
    'Was cultural displacement truly the primary cause of dueling''s disappearance, or were institutional changes (e.g., rise of courts, libel law) more significant?',
    'Comparative historical analysis of societies with similar cultural shifts but different institutional developments, or vice versa, to isolate causal pathways.',
    'If institutional displacement was primary, the ''institutional_displacement_reading'' would gain explanatory power, potentially shifting the classification of the underlying constraint from a cultural Mountain to a more actively enforced Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_ambiguity, empirical, 'Addresses the relative causal weight of cultural vs. institutional factors.').

omega_variable(
    overdetermination_ambiguity,
    'Was dueling''s decline a result of a single dominant cause (cultural displacement) or an overdetermined process with multiple independent sufficient conditions?',
    'Detailed historical case studies and counterfactual analysis to determine if any single factor, if removed, would have prevented dueling''s decline.',
    'If overdetermined, the ''overdetermined_composite_reading'' would be more accurate, suggesting a more complex network of linked constraints rather than a single dominant mechanism, potentially altering the classification of the overall phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_ambiguity, conceptual, 'Examines whether dueling''s disappearance was monocausal or multicausal.').

omega_variable(
    cultural_irreversibility,
    'Is the shift to dignity culture truly an ''irreversible substrate'' making dueling unthinkable, or could honor culture re-emerge under different social or political conditions?',
    'Sociological and anthropological studies of cultural resilience and resurgence in contexts of societal stress or regime change, or analysis of historical periods where honor-like codes reasserted themselves.',
    'If the shift is reversible, the ''mountain'' classification would be challenged, suggesting the constraint is more akin to a deeply embedded Rope or even a Snare, requiring ongoing (albeit subtle) maintenance rather than being a fixed feature of the cultural landscape.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_irreversibility, empirical, 'Assesses the permanence of the cultural shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(duel_tr_t1740, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1740, 0.08).
narrative_ontology:measurement(duel_tr_t1780, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1780, 0.07).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1820, 0.06).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1860, 0.05).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.05).

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(duel_be_t1740, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1740, 0.25).
narrative_ontology:measurement(duel_be_t1780, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1780, 0.2).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1820, 0.18).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1860, 0.16).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1700, 0.4).
narrative_ontology:measurement(duel_su_t1740, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1740, 0.35).
narrative_ontology:measurement(duel_su_t1780, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1780, 0.3).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1820, 0.28).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1860, 0.26).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
