% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Discourse Atrophy via Strategic Culture Drift
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   After 1945, total war as a deliberate strategic option was gradually
 *   excised from elite defense discourse, not because it became physically
 *   impossible (the structural_contraction reading) or legally banned (the
 *   normative reading), but because strategic culture shifted: war colleges,
 *   defense intellectuals, and military establishments converged on
 *   limited-war frameworks as the only thinkable form of conflict. The
 *   capacity to conceptualize, plan, and execute unlimited war has atrophied
 *   through institutional forgetting. The constraint is a Piton: a former
 *   coordination mechanism whose primary function (preventing catastrophic
 *   unlimited conflict) has decayed into theatrical maintenance of a
 *   limited-war paradigm that now persists by inertia. Defense intellectuals
 *   benefit incidentally from this paradigm but do not extract concentrated
 *   rents; military establishments pay through lost strategic flexibility.
 *
 * KEY AGENTS:
 *   - defense_intellectuals (agenda_setter / organized / mobile) â administer the discourse and could change it, but are identity-fused to limited-war paradigms
 *   - military_establishments (payer / institutional / identity_locked) â bear the diffuse cost of strategic monoculture and eroded doctrinal memory
 *   - revisionist_strategists (excluded / moderate / constrained) â would reintroduce total-war analytics but are structurally marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.52).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.38).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.52).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Discourse Atrophy via Strategic Culture Drift").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '48280e92-24dd-436f-89a8-2482412a3a39').
narrative_ontology:cs_kernel_codification('48280e92-24dd-436f-89a8-2482412a3a39', distributed).
narrative_ontology:cs_authority_grounding('48280e92-24dd-436f-89a8-2482412a3a39', expertise).
narrative_ontology:cs_interpretation_layer_present('48280e92-24dd-436f-89a8-2482412a3a39').
narrative_ontology:cs_reading_relation('48280e92-24dd-436f-89a8-2482412a3a39', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('48280e92-24dd-436f-89a8-2482412a3a39', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_axiom('48280e92-24dd-436f-89a8-2482412a3a39', foundational, strategic_culture_governs_reachability).
narrative_ontology:cs_axiom_status(strategic_culture_governs_reachability, holdable).
narrative_ontology:cs_axiom_grounding('48280e92-24dd-436f-89a8-2482412a3a39', strategic_culture_governs_reachability, empirically_contingent).
narrative_ontology:cs_axiom('48280e92-24dd-436f-89a8-2482412a3a39', foundational, institutional_memory_atrophies).
narrative_ontology:cs_axiom_status(institutional_memory_atrophies, holdable).
narrative_ontology:cs_axiom_grounding('48280e92-24dd-436f-89a8-2482412a3a39', institutional_memory_atrophies, empirically_contingent).
narrative_ontology:cs_reference_frame('48280e92-24dd-436f-89a8-2482412a3a39', classical_strategic_option_space).
narrative_ontology:cs_drift_state('48280e92-24dd-436f-89a8-2482412a3a39', contemporary_nuclear_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('48280e92-24dd-436f-89a8-2482412a3a39', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, military_establishments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control curricula, journals, and funding streams in strategic studies. They could reintroduce total-war analytics but are professionally invested in limited-war frameworks; their career arcs, publishing incentives, and conceptual vocabularies are built around limited, manageable conflict, making total-war discourse cognitively foreign and institutionally unrewarding.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_intellectuals, agenda_setter,
    organized, generational, mobile, global).

% Bear the operational cost of strategic monoculture: force structures, procurement priorities, and professional military education are optimized for limited war, eroding the doctrinal memory, staff-planning capacity, and organizational imagination required for unlimited conflict even where materially feasible.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_establishments, payer,
    institutional, generational, identity_locked, global).

% Argue that total war remains a reachable and necessary analytical category. They are marginalized in mainstream journals, excluded from war-college curricula, and treated as anachronistic or professionally dangerous within the defense-intellectual community.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, revisionist_strategists, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a professional consensus on the character of modern war, coordinating defense planning around limited, controllable conflict scenarios and preventing strategic surprise through shared analytical frameworks and interoperable doctrine.
% TRANSFER_FUNCTION: Transfers analytical authority and institutional resources from total-war planning cadres to limited-war specialists; moves strategic flexibility away from general-purpose military establishments toward a narrow conceptual framework that treats unlimited conflict as unthinkable.
% ABSENT_VOICES: Classical strategists, total-war historians, and revisionist military theorists who treat unlimited conflict as a persistent systemic possibility are excluded from tenure lines, foundational funding streams, and elite policy access; their absence is treated as natural professional selection rather than structural exclusion.
% DISAPPEARANCE_RATIONALE: If total-war discourse re-entered elite strategic culture overnight, military education curricula would reorient, force-planning assumptions would shift toward existential mobilization scenarios, procurement priorities would broaden, and the professional identity of the officer corps would face a generational rupture.
% FOUNDING_PROBLEM: The industrial-scale catastrophes of the World Wars and the nuclear revolution created an urgent need to prevent unlimited great-power war and to render conflict politically purposeful, controllable, and strategically intelligible.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and critical security scholars outside the limited-war paradigm attest that the total-war option was deliberately buried by the post-1945 strategic community; conventional strategic historians corroborate the post-1945 ideational shift, while the defense-intellectual community itself asserts the problem remains live.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio is high (0.78) because the vast majority of contemporary strategic-studies activity reproduces limited-war assumptions without testing them against total-war scenarios; the discourse has become performative reproduction of a settled paradigm. Base_extractiveness is moderate (0.52) because the atrophy of strategic flexibility is real but diffuse, not captured as concentrated rent. Suppression is moderate-low (0.38) because exclusion operates through professional norms, tenure incentives, and paradigm policing rather than overt coercion. Accessibility_collapse is high (0.72) because once inside the limited-war paradigm, total war becomes cognitively inaccessibleâtreated as madness or anachronism rather than a live option. Resistance is low (0.18) because excluded voices are marginal and the mainstream consensus treats dissent as unprofessional.
 *
 * PERSPECTIVAL GAP:
 *   Defense intellectuals experience the constraint as a well-functioning professional consensus that has tamed strategy; military establishments experience a narrowing of strategic choice that leaves them prepared only for wars they hope never to fight; revisionist strategists experience a dangerous blindness that conflates cultural unthinkability with physical impossibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Defense intellectuals sit near the beneficiary end: the constraint subsidizes their paradigm dominance, career structures, and institutional relevance. Military establishments sit near the target end: they bear the operational and educational costs of a strategically truncated force. Revisionist strategists are excluded rather than coordinated, experiencing the constraint as epistemic closure.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as Piton rather than Snare prevents mislabeling the defense-intellectual community as a concentrated extractor. They benefit from the paradigm, but the constraint persists by inertia and professional autopilot, not by active rent-seeking. Classifying it as Piton rather than Rope captures that the coordination benefitâpreventing total warâhas atrophied; the remaining structure is mostly theatrical reproduction of a once-functional consensus. The Mandatrophy signal is the mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges: the original problem has passed, yet the arrangement persists and the world would rearrange if it vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drift_vs_enforcement_ambiguity,
    'Does the absence of total-war discourse reflect organic institutional forgetting, or active suppression by defense intellectuals invested in limited-war frameworks?',
    'Archival analysis of curriculum gatekeeping, funding allocation, and peer-review decisions in strategic studies over 1945-2020; if documented gatekeeping drives exclusion, the constraint trends snare-ward; if passive atrophy and generational turnover dominate, piton holds.',
    'Reclassification from piton to snare if active beneficiary maintenance is shown to be the primary driver of total-war discourse exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_vs_enforcement_ambiguity, empirical, 'Whether the constraint persists by inertia or by active intellectual gatekeeping.').

omega_variable(
    reading_boundary_ambiguity,
    'Does the strategic_culture_drift reading conflate ideational shift with underlying normative or structural changes better captured by sibling readings?',
    'Comparative historiography measuring independent causal weight of strategic culture versus legal-norm development (Article 2(4), humanitarian law) versus weapons-technology determinism (nuclear revolution).',
    'If normative or structural factors explain most variance, this reading collapses toward its siblings and should be retired or merged; if strategic culture has independent causal force, the reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether this reading is analytically separable from its sibling kernel readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of total-war discourse enforced through structural professional incentives, or through internalized paradigmatic blinders that persist even when incentives are removed?',
    'Natural experiment from defense intellectuals who rotate into policy roles with different incentive structures: if they spontaneously recover total-war analytics when institutional incentives change, suppression is structural; if the blinders persist, suppression is partially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s hold on strategic imagination is deeper than institutional analysis suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of total-war imagination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twsc_tr_t0, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0, 0.2).
narrative_ontology:measurement(twsc_tr_t10, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 10, 0.3).
narrative_ontology:measurement(twsc_tr_t20, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 20, 0.42).
narrative_ontology:measurement(twsc_tr_t30, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 30, 0.52).
narrative_ontology:measurement(twsc_tr_t40, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 40, 0.6).
narrative_ontology:measurement(twsc_tr_t50, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 50, 0.68).
narrative_ontology:measurement(twsc_tr_t60, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 60, 0.72).
narrative_ontology:measurement(twsc_tr_t70, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 70, 0.76).
narrative_ontology:measurement(twsc_tr_t80, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 80, 0.78).

% Extraction over time
narrative_ontology:measurement(twsc_be_t0, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(twsc_be_t10, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(twsc_be_t20, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(twsc_be_t30, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(twsc_be_t40, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(twsc_be_t50, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(twsc_be_t60, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(twsc_be_t70, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 70, 0.51).
narrative_ontology:measurement(twsc_be_t80, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 80, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__strategic_culture_drift, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the total_war_winnability_post1945 kernel. The kernel decomposes into three structurally distinct claims about why total war dropped from the strategic option space: structural impossibility (structural_contraction_reading), normative illegitimacy (normative_reading_drop), and ideational atrophy (strategic_culture_drift). Each reading carries a distinct epsilon, stakeholder structure, and causal story. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
