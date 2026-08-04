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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Dueling Became Culturally Unthinkable (Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the cultural shift that made dueling
 *   unthinkable, focusing on the displacement of honor-culture axioms by
 *   dignity-culture norms. It argues that the new cultural substrate became a
 *   'mountain' — an unchangeable, fixed reality that fundamentally altered
 *   the social landscape, rendering dueling not just illegal but culturally
 *   illegitimate. The constraint is claimed as a mountain because the
 *   dignity-culture framework, once established, became an irreducible limit
 *   on social behavior, persisting regardless of individual enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.05).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.95).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling Became Culturally Unthinkable (Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'b5557357-c79b-44ea-9872-e22ee595b958').
narrative_ontology:cs_kernel_codification('b5557357-c79b-44ea-9872-e22ee595b958', implicit).
narrative_ontology:cs_authority_grounding('b5557357-c79b-44ea-9872-e22ee595b958', practice).
narrative_ontology:cs_interpretation_layer_present('b5557357-c79b-44ea-9872-e22ee595b958').
narrative_ontology:cs_reading_relation('b5557357-c79b-44ea-9872-e22ee595b958', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_reading_relation('b5557357-c79b-44ea-9872-e22ee595b958', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('b5557357-c79b-44ea-9872-e22ee595b958', foundational, personal_worth_inherent_not_performative).
narrative_ontology:cs_axiom_status(personal_worth_inherent_not_performative, holdable).
narrative_ontology:cs_axiom_grounding('b5557357-c79b-44ea-9872-e22ee595b958', personal_worth_inherent_not_performative, deontological).
narrative_ontology:cs_axiom('b5557357-c79b-44ea-9872-e22ee595b958', foundational, violence_unacceptable_for_reputation_defense).
narrative_ontology:cs_axiom_status(violence_unacceptable_for_reputation_defense, holdable).
narrative_ontology:cs_axiom_grounding('b5557357-c79b-44ea-9872-e22ee595b958', violence_unacceptable_for_reputation_defense, conventional).
narrative_ontology:cs_reference_frame('b5557357-c79b-44ea-9872-e22ee595b958', honor_culture_axioms_dominant).
narrative_ontology:cs_drift_state('b5557357-c79b-44ea-9872-e22ee595b958', post_enlightenment_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('b5557357-c79b-44ea-9872-e22ee595b958', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals and institutions benefit from a social order where personal worth is inherent and not subject to public challenge or violent defense. They actively promote norms of self-restraint and legal recourse, finding dueling antithetical to their worldview.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    institutional, generational, analytical, national).

% Individuals whose social standing and self-concept were historically tied to the public defense of honor. As dignity culture ascendant, their framework for resolving disputes and maintaining status became culturally illegible and socially unacceptable, leading to their marginalization or forced assimilation.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, identity_locked, local).

% The formal legal apparatus that increasingly criminalized dueling and provided alternative mechanisms for dispute resolution. While not the primary driver of cultural shift in this reading, it codified and enforced the new dignity-based norms.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_system, agenda_setter,
    institutional, generational, constrained, national).

% Academics and historians who analyze the mechanisms of cultural change, including the shift from honor to dignity cultures. They seek to understand the underlying axioms and their displacement.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interactions around a shared understanding of personal worth and dispute resolution, shifting from public challenge to inherent dignity and legal recourse.
% TRANSFER_FUNCTION: Transfers the right to define and defend personal worth from individual, public performance (honor) to an inherent, legally protected status (dignity).
% ABSENT_VOICES: Those who continued to adhere to honor-culture axioms, finding the new dignity-based framework inadequate for maintaining their social standing or self-respect. Their voices were increasingly marginalized and dismissed as anachronistic.
% DISAPPEARANCE_RATIONALE: If the cultural shift to dignity-based norms were to reverse overnight, the entire social fabric around personal worth, conflict resolution, and legal standing would fundamentally rearrange, potentially leading to a resurgence of honor-based challenges.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes in a society where personal honor was paramount and often led to violence.
% FOUNDING_PROBLEM_CORROBORATION: The problem of dueling as a widespread social practice is largely dead, as attested by legal historians and sociologists. The cultural framework that made dueling a necessary mechanism for dispute resolution has been fundamentally displaced, as corroborated by historical texts and legal records outside the direct beneficiaries of dignity culture.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.05) because the dignity-culture framework, in this reading, is not primarily about extracting resources but about establishing a new, less violent social order. Suppression is very high (0.95) because the cultural shift effectively eliminated the social space for dueling, making it almost impossible to practice without severe social and legal consequences. Accessibility collapse is high (0.9) as the very idea of dueling became culturally unthinkable. Resistance is low (0.05) because the cultural shift was so profound that active resistance to the new norms was minimal and ineffective. The temporal measurements show a steady increase in suppression as dignity culture solidified its dominance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity-culture adherents, this is a natural and beneficial evolution of social norms. From the perspective of honor-culture practitioners, it is a profound loss of a meaningful framework for life, a form of cultural extraction and suppression. The engine's classification as a mountain from all seats reflects the irreversible nature of this cultural shift, even for those who were its 'victims'.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity-culture adherents are beneficiaries (d near 0.0) as the new norms align with their worldview and reduce social violence. Honor-culture practitioners are victims (d near 1.0) as their identity and social practices are rendered illegitimate and suppressed. The legal system acts as an agenda-setter, codifying and enforcing the emergent cultural mountain.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_causality,
    'To what extent was the decline of dueling primarily a cultural shift (dignity displacing honor) versus an institutional one (legal prohibition, alternative dispute resolution)?',
    'Comparative historical analysis of societies with similar legal changes but different cultural trajectories, or detailed micro-historical studies of individual choices to duel/not duel in transitional periods.',
    'If institutional factors were dominant, the constraint would be closer to a Snare or Tangled Rope (enforced extraction/coordination) rather than a Mountain (natural cultural evolution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_institutional_causality, empirical, 'Distinguishing cultural displacement from institutional substitution as the primary cause of dueling''s decline.').

omega_variable(
    irreversibility_of_dignity_culture,
    'Is the ''mountain'' status of dignity culture truly irreversible, or could a societal crisis lead to a resurgence of honor-based norms?',
    'Longitudinal sociological studies of societies under extreme stress (e.g., state collapse, prolonged civil conflict) to observe if honor-based dispute resolution re-emerges as a dominant cultural force.',
    'If honor-based norms could re-emerge, the ''mountain'' classification would be too strong, suggesting a more contingent, Rope-like or even Snare-like constraint that requires ongoing cultural maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(irreversibility_of_dignity_culture, conceptual, 'Assessing the true irreversibility of the dignity-culture framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1700, 0.1).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.08).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.06).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.05).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.4).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.6).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the 'dueling_disappearance_mechanism' kernel, focusing on cultural displacement. It is linked to sibling readings that emphasize institutional factors or a composite causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
