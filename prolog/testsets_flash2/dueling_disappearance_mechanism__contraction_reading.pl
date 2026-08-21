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
 *   human_readable: Dueling's Disappearance: Dignity Culture Contraction
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story describes the 'contraction reading' of dueling's
 *   disappearance, where the practice became culturally unthinkable due to
 *   the displacement of honor-culture axioms by an ascendant dignity culture.
 *   The constraint is framed as a 'mountain' because the shift in fundamental
 *   cultural axioms is presented as an irreversible, substrate-level change,
 *   making dueling genuinely impossible within the new framework.
 *   Honor-culture practitioners are victims because their entire social
 *   framework became illegible. The metrics reflect a low extractiveness (as
 *   the shift is cultural, not directly economic) but high suppression (as
 *   the new cultural norms actively suppress the old ones) and near-total
 *   accessibility collapse for alternatives.
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
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling's Disappearance: Dignity Culture Contraction").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '94c06a34-b9d4-43b4-b71f-769236ad2e30').
narrative_ontology:cs_kernel_codification('94c06a34-b9d4-43b4-b71f-769236ad2e30', implicit).
narrative_ontology:cs_authority_grounding('94c06a34-b9d4-43b4-b71f-769236ad2e30', practice).
narrative_ontology:cs_interpretation_layer_present('94c06a34-b9d4-43b4-b71f-769236ad2e30').
narrative_ontology:cs_reading_relation('94c06a34-b9d4-43b4-b71f-769236ad2e30', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('94c06a34-b9d4-43b4-b71f-769236ad2e30', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('94c06a34-b9d4-43b4-b71f-769236ad2e30', foundational, individual_worth_is_intrinsic).
narrative_ontology:cs_axiom_status(individual_worth_is_intrinsic, holdable).
narrative_ontology:cs_axiom_grounding('94c06a34-b9d4-43b4-b71f-769236ad2e30', individual_worth_is_intrinsic, deontological).
narrative_ontology:cs_axiom('94c06a34-b9d4-43b4-b71f-769236ad2e30', foundational, violence_is_not_a_legitimate_means_of_honor_defense).
narrative_ontology:cs_axiom_status(violence_is_not_a_legitimate_means_of_honor_defense, holdable).
narrative_ontology:cs_axiom_grounding('94c06a34-b9d4-43b4-b71f-769236ad2e30', violence_is_not_a_legitimate_means_of_honor_defense, deontological).
narrative_ontology:cs_reference_frame('94c06a34-b9d4-43b4-b71f-769236ad2e30', dignity_culture_ascendancy).
narrative_ontology:cs_drift_state('94c06a34-b9d4-43b4-b71f-769236ad2e30', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('94c06a34-b9d4-43b4-b71f-769236ad2e30', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, dignity_culture_supremacy).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, individual_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a social order where individual worth is intrinsic and not subject to public challenge or violent defense. Their worldview is affirmed and propagated by the constraint.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    institutional, generational, analytical, global).

% Are culturally dispossessed; their framework for resolving disputes and maintaining social standing becomes illegitimate and unthinkable. They are forced to adopt a new cultural logic or become social outcasts.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Actively codify and enforce the norms of dignity culture, making dueling illegal and socially unacceptable. They are both an agent of and a beneficiary from the cultural shift.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Analyze the historical and sociological mechanisms by which dueling became unthinkable, identifying the cultural shifts and their consequences for different social groups.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interactions around a shared understanding of individual worth and dispute resolution, shifting from honor-based challenges to legal or social mechanisms.
% TRANSFER_FUNCTION: Transfers the right to define and defend personal honor from individuals (via dueling) to the collective (via legal systems and dignity norms), effectively removing the individual's agency in this domain.
% ABSENT_VOICES: The 'honor-bound' individuals of the past, whose entire social identity and means of redress were tied to the duel, are absent from the contemporary discourse. Their worldview is now largely illegible or dismissed as barbaric.
% DISAPPEARANCE_RATIONALE: If the dignity-culture displacement of honor-culture axioms were to reverse overnight, the fundamental understanding of individual worth and social interaction would be radically altered. Society would have to re-establish mechanisms for dispute resolution and honor defense, potentially leading to a resurgence of practices like dueling or new, analogous forms of violent redress.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes in a way that minimizes violence and respects individual rights, as perceived by the emerging dignity culture.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars, from outside the immediate beneficiaries of dignity culture, corroborate that the shift addressed genuine societal problems related to violence and arbitrary justice, even while acknowledging the costs to honor-culture practitioners.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.05) reflects that the 'extraction' is not economic but cultural — the cost is the loss of a worldview and social practice. The high suppression (0.95) and accessibility collapse (0.98) are due to the fundamental nature of the cultural shift: once dignity culture became dominant, the very concept of dueling became unthinkable, not merely illegal. Resistance is low (0.02) because the shift was so profound that active resistance became futile or impossible. The claimed type is 'mountain' because the cultural substrate itself changed, making the constraint appear as an unchangeable feature of the new social reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity-culture adherents, the disappearance of dueling is a natural and beneficial evolution, a 'mountain' of moral progress. From the perspective of honor-culture practitioners, it was a catastrophic loss of their social framework, a 'snare' that trapped them in an alien cultural logic. The engine's classification will reflect the structural reality of the cultural shift, which this reading frames as a mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity-culture adherents are beneficiaries as their worldview is affirmed and propagated. Honor-culture practitioners are victims, as their identity and social practices are rendered illegitimate. Legal systems act as agenda-setters, codifying and enforcing the new cultural norms. Historical observers provide an analytical perspective on the process.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a profound cultural shift as a mere 'snare' or 'tangled rope' that could be easily undone. The 'mountain' classification highlights the deep, substrate-level change in cultural axioms, suggesting that the 'mandate' of dignity culture is not 'atrophied' but rather fully realized and dominant, making the old mandate of honor culture genuinely obsolete. The challenge is to recognize the 'naturalness' of the new cultural order while acknowledging its profound impact on those whose frameworks were displaced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_causation,
    'Was the decline of dueling primarily a cultural shift (dignity displacing honor) or an institutional one (courts displacing duels as dispute resolution)?',
    'Comparative historical analysis of societies with similar cultural shifts but different institutional developments, or vice versa. Quantitative analysis of legal enforcement vs. cultural norm shifts over time.',
    'If primarily institutional, the constraint might be reclassified as a ''rope'' or ''tangled_rope'' (institutional_displacement_reading), reflecting a more reversible or policy-driven change. If primarily cultural, the ''mountain'' classification holds, emphasizing the deep, substrate-level shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_institutional_causation, empirical, 'Distinguishing the primary causal mechanism for dueling''s disappearance.').

omega_variable(
    irreversibility_of_cultural_shift,
    'Is the dignity-culture displacement of honor-culture axioms truly irreversible, making dueling ''unthinkable'' (mountain), or could honor culture re-emerge under different conditions (snare/rope)?',
    'Anthropological study of cultural reversals or the re-emergence of suppressed cultural forms in response to societal stress or institutional collapse. Theoretical analysis of cultural evolution and path dependence.',
    'If the shift is found to be reversible, the ''mountain'' classification would be challenged, potentially shifting to a ''snare'' or ''tangled_rope'' if active suppression is still required, or a ''piton'' if only inertia maintains it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_of_cultural_shift, conceptual, 'Assessing the true irreversibility of the cultural shift from honor to dignity.').

omega_variable(
    victim_identity_lock_mechanism,
    'To what extent were honor-culture practitioners ''identity_locked'' by their cultural framework, making exit from dueling norms genuinely unthinkable for them, versus merely ''constrained'' by legal prohibition?',
    'Analysis of personal diaries, letters, and contemporary accounts from the period to gauge the internal experience of honor-bound individuals facing the decline of dueling. Psychological studies of identity formation and cultural assimilation.',
    'If identity-lock was the primary mechanism, it reinforces the ''mountain'' classification by highlighting the deep internal constraint. If external legal constraint was dominant, it would support a ''tangled_rope'' or ''snare'' classification, emphasizing coercion over cultural internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_lock_mechanism, empirical, 'Understanding the mechanism of constraint for honor-culture practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1700, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1700, 0.0).
narrative_ontology:measurement(duel_tr_t1750, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1750, 0.0).
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1800, 0.0).
narrative_ontology:measurement(duel_tr_t1850, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__contraction_reading, theater_ratio, 1900, 0.0).

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
% This constraint is one reading of the 'dueling_disappearance_mechanism' kernel, focusing on the cultural displacement of honor-culture axioms by dignity culture. It is linked to sibling readings that emphasize institutional displacement and overdetermined causality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
