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
 *   This constraint story, 'Dueling's Disappearance: Dignity Culture
 *   Contraction,' represents one reading of the broader
 *   'dueling_disappearance_mechanism' kernel. It argues that dueling became
 *   culturally unthinkable due to the fundamental displacement of
 *   honor-culture axioms by the ascendant dignity culture. This shift made
 *   the very concept of dueling, and the honor it sought to defend, illegible
 *   and illegitimate within the new cultural substrate. The constraint is
 *   classified as a Mountain because the dignity culture, once established,
 *   functions as an unchangeable, fixed cultural reality that fundamentally
 *   alters the landscape of social interaction and dispute resolution.
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
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling's Disappearance: Dignity Culture Contraction").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, '0fe5defe-ca7f-4ee9-a5e6-adc87e455365').
narrative_ontology:cs_kernel_codification('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', implicit).
narrative_ontology:cs_authority_grounding('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', practice).
narrative_ontology:cs_interpretation_layer_present('0fe5defe-ca7f-4ee9-a5e6-adc87e455365').
narrative_ontology:cs_reading_relation('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_reading_relation('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', foundational, intrinsic_individual_worth).
narrative_ontology:cs_axiom_status(intrinsic_individual_worth, holdable).
narrative_ontology:cs_axiom_grounding('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', intrinsic_individual_worth, deontological).
narrative_ontology:cs_axiom('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', secondary, state_sole_legitimate_violence).
narrative_ontology:cs_axiom_status(state_sole_legitimate_violence, holdable).
narrative_ontology:cs_axiom_grounding('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', state_sole_legitimate_violence, conventional).
narrative_ontology:cs_reference_frame('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', honor_culture_legitimacy).
narrative_ontology:cs_drift_state('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', post_enlightenment_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('0fe5defe-ca7f-4ee9-a5e6-adc87e455365', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, state_legal_system).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, dignity_culture_supremacy).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a social order where individual worth is intrinsic and not subject to public challenge or violent defense. Their worldview became the dominant cultural substrate, making dueling unthinkable.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents, beneficiary,
    institutional, generational, analytical, national).

% Benefits from the consolidation of its monopoly on legitimate violence. The cultural shift to dignity made legal prohibitions against dueling more effective and less contested, reinforcing state authority.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, state_legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Were culturally dispossessed as their framework for resolving disputes and maintaining social standing became illegitimate and eventually unintelligible. They faced social ostracism or legal sanction for adhering to older norms, with no viable alternative within their identity frame.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, honor_culture_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Analyze the mechanisms of cultural change and the displacement of one normative system by another. They observe the structural shift from honor to dignity culture as a foundational change.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__contraction_reading, historical_sociologists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In honor cultures, dueling coordinated the public defense of reputation and social standing, providing a ritualized mechanism for dispute resolution among elites. Its disappearance meant the coordination of personal worth shifted to internal, intrinsic value.
% TRANSFER_FUNCTION: The constraint transferred the locus of personal worth and dispute resolution from public, violent contestation (honor) to intrinsic, legally protected individual rights (dignity). It transferred the cost of maintaining social order from individual violence to state-enforced legal processes.
% ABSENT_VOICES: The 'voice' of honor culture itself, as a coherent system of values and practices, became absent from mainstream discourse. Its practitioners were marginalized, their claims to legitimate violence rendered unintelligible by the ascendant dignity framework.
% DISAPPEARANCE_RATIONALE: If the cultural shift to dignity (which made dueling unthinkable) were to vanish overnight, it would imply a fundamental reversal of centuries of social evolution. The world as currently constituted, with its emphasis on individual rights and state monopoly on violence, depends on this cultural substrate. Its 'disappearance' would mean a return to a prior, structurally different world.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes in societies where personal honor was paramount and state authority was not absolute.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, legal statutes, and sociological analyses from outside the direct beneficiaries (e.g., contemporary historians, legal scholars) corroborate that the problem of dueling as a legitimate social practice is dead, having been superseded by a different cultural and legal framework. The state legal system and dignity culture adherents, as beneficiaries, also attest to its dead status, but their corroboration is self-serving.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is very low (0.05) because the dignity culture, in this reading, is not primarily an extractive mechanism but a foundational cultural shift that renders prior practices obsolete. Suppression is very high (0.95) because the cultural shift effectively 'suppressed' the very possibility of dueling by making its underlying axioms unthinkable. Accessibility collapse is high (0.9) as alternatives (like dueling) became culturally impossible. Resistance is low (0.05) because the cultural shift was so profound that active resistance to the disappearance of dueling became marginal and ineffective. The claimed type is Mountain because the dignity culture, in this reading, acts as an irreversible, emergent cultural substrate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity_culture_adherents and the state_legal_system, the disappearance of dueling is a natural and beneficial evolution, a 'world_unchanged' scenario where a barbaric practice simply faded away. For honor_culture_practitioners, it was a profound loss and a 'world_rearranges' event, as their entire framework for social standing and dispute resolution was rendered illegitimate. The engine's classification as Mountain reflects the structural reality of the dominant cultural substrate, while the victim set highlights the cost borne by those whose cultural framework was displaced.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity_culture_adherents and the state_legal_system are beneficiaries (d near 0.0) as the cultural shift aligns with their values and strengthens state authority. Honor_culture_practitioners are victims (d near 1.0) because their identity and social practices were directly undermined and made impossible by the new cultural order. Their exit options are 'identity_locked' as their very self-conception was tied to the honor culture.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the cultural shift as a mere 'snare' or 'tangled_rope' designed for extraction. While there are clear beneficiaries and victims, the core mechanism is a fundamental cultural transformation, not an actively maintained extractive structure. The 'mandate' of dueling (to defend honor) became obsolete because the underlying cultural axioms that gave 'honor' meaning were displaced. The constraint's persistence is due to the inertia of the new cultural substrate, not active enforcement of a specific extractive mechanism. The 'dead' status of the founding problem, coupled with the 'world_unchanged' disappearance verdict, reinforces the Mountain classification, indicating a fundamental, irreversible shift rather than a lingering, extractive artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_causation,
    'To what extent was the disappearance of dueling primarily a cultural shift (dignity displacing honor) versus an institutional one (courts, libel law displacing dueling as dispute resolution)?',
    'Comparative historical analysis of societies with similar cultural shifts but different institutional developments, or vice versa. Detailed examination of the timing and causal pathways of legal prohibition versus cultural acceptance.',
    'If institutional displacement was primary, the constraint might be reclassified as a Rope or Tangled Rope (institutional_displacement_reading), reflecting a more active, coordinated substitution of mechanisms rather than a passive cultural contraction. This would shift the focus from cultural substrate to active policy choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_institutional_causation, conceptual, 'Ambiguity between cultural and institutional drivers of dueling''s decline.').

omega_variable(
    irreversibility_of_dignity_culture,
    'Is the ''dignity culture'' truly an irreversible, Mountain-like substrate, or could its axioms be challenged or reversed under different historical conditions?',
    'Longitudinal studies of cultural evolution in other domains, or counterfactual historical analysis exploring scenarios where dignity culture''s foundational axioms were undermined. Examination of contemporary challenges to ''dignity'' as a universal value.',
    'If dignity culture is found to be reversible or contingent, the constraint''s classification might shift from Mountain to a more constructed type (e.g., a Rope or Tangled Rope), implying that its persistence requires ongoing maintenance or defense, rather than being an emergent natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_of_dignity_culture, empirical, 'The degree of irreversibility of the dignity culture as a social substrate.').


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
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.03).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.02).

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
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, state_monopoly_on_violence_legitimacy).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, modern_legal_dispute_resolution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dueling_disappearance_mechanism' kernel, focusing on cultural displacement. Sibling readings include 'institutional_displacement_reading' and 'overdetermined_composite_reading', which emphasize legal/institutional factors and multiple causes, respectively. Each reading offers a distinct causal mechanism for dueling's decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
