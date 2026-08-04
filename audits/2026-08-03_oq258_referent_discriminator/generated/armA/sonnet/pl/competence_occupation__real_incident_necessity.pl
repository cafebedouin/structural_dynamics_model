% ============================================================================
% CONSTRAINT STORY: competence_occupation__real_incident_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__real_incident_necessity, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: competence_occupation__real_incident_necessity
 *   human_readable: Real-Incident-Necessity Reading of Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   High-reliability organizations (nuclear power operations, commercial
 *   aviation, surgical teams, air traffic control) face a structural problem:
 *   the skills required to handle catastrophic failure can only be fully
 *   exercised during catastrophic failure, which is by definition rare and by
 *   moral design something the organization works to prevent. This reading of
 *   the competence-occupation kernel holds that only actual catastrophic
 *   incidents provide the authentic conditions necessary to occupy the
 *   competence kernel — simulations, drills, and procedural walkthroughs are
 *   structurally unable to replicate the physiological stress, informational
 *   ambiguity, multi-system cascading failure, and irreversible stakes of a
 *   real event. Under this reading, HROs face an unresolvable
 *   competence-maintenance problem: the organization cannot manufacture the
 *   very conditions that would let it verify or renew the skill it most
 *   needs, and it cannot ethically wish for those conditions to occur. There
 *   is no viable beneficiary structure for this reading's own referent (a
 *   catastrophe benefits no one), which is what distinguishes it sharply from
 *   the sibling readings that posit simulation or hybrid mechanisms as
 *   adequate substitutes. This story treats the real-incident-necessity CLAIM
 *   as the standing arrangement under contest — the epistemic standard some
 *   safety cultures and post-incident investigators actually hold — not the
 *   abolition or resolution of that standard.
 *
 * KEY AGENTS:
 *   - hro_frontline_operators: Primary bearers of the unresolved gap (moderate/constrained) — must perform under a competence standard they can never fully satisfy short of disaster
 *   - incident_investigation_bodies: Structural beneficiary (institutional/mobile) — derive authority, funding, and epistemic standing from treating real incidents as uniquely authoritative evidence
 *   - hro_management: Agenda-setters (institutional/constrained) — administer training regimes knowing the authenticity gap cannot be closed by policy
 *   - regulatory_bodies: Analytical observers (institutional/analytical) — assess HRO readiness against a standard that may be partially unsatisfiable
 *   - the_traveling_public_and_downstream_stakeholders: Excluded (powerless/trapped) — bear the consequences if the competence gap manifests, but are not party to the training-design conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__real_incident_necessity, 0.31).
domain_priors:suppression_score(competence_occupation__real_incident_necessity, 0.22).
domain_priors:theater_ratio(competence_occupation__real_incident_necessity, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, extractiveness, 0.31).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__real_incident_necessity, mountain).
narrative_ontology:human_readable(competence_occupation__real_incident_necessity, "Real-Incident-Necessity Reading of Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__real_incident_necessity, "high_reliability_organizations/safety_training").

domain_priors:emerges_naturally(competence_occupation__real_incident_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__real_incident_necessity, 'e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a').
narrative_ontology:cs_kernel_codification('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', distributed).
narrative_ontology:cs_authority_grounding('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', practice).
narrative_ontology:cs_interpretation_layer_present('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a').
narrative_ontology:cs_reading_relation('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', foundational, authenticity_requires_irreversible_stakes).
narrative_ontology:cs_axiom_status(authenticity_requires_irreversible_stakes, holdable).
narrative_ontology:cs_axiom_grounding('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', authenticity_requires_irreversible_stakes, empirically_contingent).
narrative_ontology:cs_axiom('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', secondary, simulated_conditions_cannot_replicate_cascading_failure_phenomenology).
narrative_ontology:cs_axiom_status(simulated_conditions_cannot_replicate_cascading_failure_phenomenology, holdable).
narrative_ontology:cs_axiom_grounding('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', simulated_conditions_cannot_replicate_cascading_failure_phenomenology, empirically_contingent).
narrative_ontology:cs_reference_frame('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', post_incident_authenticity_standard).
narrative_ontology:cs_drift_state('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', contemporary_hro_training_regimes, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e6678e4d-8da8-4a8d-b2a3-fb79c23eba4a', '').
narrative_ontology:cs_kernel_id(competence_occupation__real_incident_necessity, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__real_incident_necessity, incident_investigation_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_occupation__real_incident_necessity, hro_frontline_operators).
narrative_ontology:constraint_vindicates(competence_occupation__real_incident_necessity, authenticity_criterion_for_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pilots, control room operators, and surgical teams who train continuously under a standard that, by this reading's own logic, can never be fully satisfied short of experiencing an actual catastrophe. They bear the psychological and professional burden of an unresolvable verification gap: no amount of drilling proves they would perform correctly when the stakes become irreversible. Exit from the profession is possible but costly given years of specialized training investment.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, hro_frontline_operators, payer,
    moderate, biographical, constrained, national).

% Bodies such as accident investigation boards that derive their epistemic and institutional authority from being the sole legitimate interpreters of 'real' catastrophic events. Every time the real-incident-necessity standard is invoked, their evidentiary monopoly over what counts as authentic organizational learning is reinforced. They do not manufacture incidents and do not profit financially, but their standing is structurally reinforced by the persistence of this reading.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, incident_investigation_bodies, beneficiary,
    institutional, generational, mobile, national).

% Design and fund training regimes knowing that under this reading's own terms, no training program can close the authenticity gap. They must administer a competence-maintenance system while publicly unable to claim it fully succeeds, creating persistent institutional tension between regulatory compliance narratives and this reading's harder epistemic claim.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, hro_management, agenda_setter,
    institutional, generational, constrained, national).

% Certify HRO readiness against standards that must, if this reading is correct, tacitly accept an irreducible epistemic gap rather than claim it is closed. They take testimony from operators, management, and investigators, and their certification frameworks implicitly negotiate how much of this reading's severity to acknowledge publicly.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% Passengers, patients, and communities near high-hazard facilities who bear the consequences if the unresolved competence gap manifests as failure, but have no voice in how training regimes or certification standards are designed or debated. They would object to any acceptance of irreducible risk if given a seat, but are not part of the professional or regulatory conversation.
narrative_ontology:constraint_stakeholder(competence_occupation__real_incident_necessity, traveling_public_and_downstream_stakeholders, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__real_incident_necessity, diffuse).
narrative_ontology:fixing_cost_class(competence_occupation__real_incident_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared epistemic standard within safety-critical professions for what counts as genuine evidence of catastrophe-response competence, preventing organizations from claiming false confidence based solely on simulated exercises.
% TRANSFER_FUNCTION: Does not primarily move money or resources between parties; it moves epistemic authority — reinforcing incident investigation bodies' standing as arbiters of authentic organizational learning, at the cost of operator and management certainty about their own readiness.
% ABSENT_VOICES: The traveling public and downstream stakeholders who bear the consequences of any residual, unclosable competence gap have no voice in setting or contesting this standard; they would likely object to any acceptance of irreducible risk being treated as an unresolvable fact rather than a problem to keep attacking.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, some HROs would relax into treating simulation or hybrid regimes as fully sufficient, potentially reducing operator anxiety about unverifiable readiness — but incident investigation bodies and safety culture theorists would likely reassert some version of the authenticity concern given the underlying phenomenological difference between drilled and real catastrophic stress remains real regardless of which reading is officially adopted. Parties dispute whether the world meaningfully rearranges or whether the underlying problem simply resurfaces under a different label.
% FOUNDING_PROBLEM: HROs needed a way to know whether their catastrophe-response training was actually adequate before a real catastrophe tested it, given that full-scale testing is both practically rare and morally unacceptable to seek out.
% FOUNDING_PROBLEM_CORROBORATION: Independent human-factors researchers studying near-miss and post-incident debrief literature (outside both HRO management and incident investigation bodies) corroborate that a phenomenological gap between simulated and real catastrophic stress is empirically documented in aviation and nuclear safety literature; this corroboration exists outside the incident investigation bodies who structurally benefit from the standard's persistence.
narrative_ontology:disappearance_verdict(competence_occupation__real_incident_necessity, contested).
narrative_ontology:founding_problem_status(competence_occupation__real_incident_necessity, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__real_incident_necessity, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__real_incident_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__real_incident_necessity, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__real_incident_necessity_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, ExtMetricName, E),
    domain_priors:suppression_score(competence_occupation__real_incident_necessity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(competence_occupation__real_incident_necessity),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(competence_occupation__real_incident_necessity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(competence_occupation__real_incident_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.31 at interval end) because this reading does not describe a rent-extraction mechanism — no one profits from catastrophes occurring, and the mild upward drift reflects incident investigation bodies' institutional standing rising slightly as their evidentiary monopoly on 'authentic' data becomes more entrenched over time, not any direct transfer from operators. Suppression is low (0.22): there is no coercive apparatus forcing operators to accept this reading; it persists because it is a defensible epistemic position, not because alternatives are blocked. Accessibility_collapse is authored high (0.68) because once an organization accepts that only real catastrophic conditions authenticate competence, the alternative epistemic framings (simulation sufficiency, hybrid occupation) become very hard to hold simultaneously in the same safety culture — the standard, once internalized, forecloses casual reliance on drills as 'proof' of readiness. Resistance is authored high (0.72) because this reading is actively contested by simulation-technology vendors, training departments, and operators who find the standard demoralizing or practically unworkable (it implies their preparation can never be verified as sufficient) — real resistance exists, which is consistent with a mountain that describes a genuine hard limit rather than a policy choice nobody objects to.
 *
 * DIRECTIONALITY LOGIC:
 *   Incident investigation bodies are declared as the sole beneficiary because their epistemic authority (as arbiters of what counts as a 'real' test of the system) is reinforced every time the real-incident-necessity standard is invoked — post-incident investigations become the privileged, almost sacred site of organizational learning. This is a modest, indirect benefit (standing, not rent), which is why extractiveness stays low even though a beneficiary is named — the FSM check is satisfied structurally (mountain + declared beneficiary) but the metrics do not inflate to match a captured-extraction story, because none exists at the referent this story is about. Frontline operators and management are not declared as victims: they are burdened by an unresolvable epistemic condition, not extracted from by an identifiable party. This is what differentiates this reading from a tangled_rope — there is coordination-adjacent structure (a shared standard) but no asymmetric extraction flowing from operator to a capturing party.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how does an organization know its catastrophe-response competence is real before catastrophe strikes — remains live; it has not been solved and cannot be solved within this reading's own terms, since satisfying the standard requires the very event the organization exists to prevent. This is why mandatrophy_resolved is not declared: the mandate has not outlived its function, because the function (authentic verification) has structurally never been achievable, not because the constraint decayed from a once-working state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_epistemics,
    'Is the claim that ''only real catastrophic incidents authenticate competence occupation'' a discovered fact about human skill formation under rare-event conditions, or is it a constructed epistemic standard that certain actors benefit from imposing?',
    'Compare organizational safety outcomes and post-incident performance across HROs that treat simulation as sufficient versus those that hold the real-incident-necessity standard, controlling for incident base rate; also examine whether incident investigation bodies gain standing/funding/authority from the standard''s persistence.',
    'If discovered fact: the reading is a genuine mountain describing an unresolvable epistemic gap. If constructed: incident investigation bodies and post-incident inquiry industries who derive institutional authority from privileging ''real'' events as the only authentic evidence would be false-summit beneficiaries, and the reading should reclassify toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_epistemics, conceptual, 'Whether the real-incident-necessity standard is natural epistemic limit or constructed authority claim for incident investigators.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three kernel readings (real_incident_necessity, simulation_sufficiency, hybrid_occupation) disagree — is it about what counts as evidence of competence, or about what competence itself IS?',
    'Trace whether proponents of each reading would accept the same definition of ''competence occupation'' if given a hypothetical omniscient measurement of skill retention; if yes, the dispute is purely evidentiary (what observable counts); if no, the readings disagree about the construct itself.',
    'If purely evidentiary, the readings could in principle be reconciled by better instrumentation and the kernel is less deeply contested than it appears. If constitutive, no instrumentation resolves the dispute and the three readings are permanently coexisting rival constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating whether kernel disagreement is evidentiary or constitutive.').

omega_variable(
    hro_unresolvable_gap_severity,
    'Given that catastrophic incidents are (by design and by moral necessity) rare and unwelcome, does the real-incident-necessity standard describe a genuinely unresolvable competence-maintenance problem for HROs, or does it describe a problem that is merely difficult but tractable through near-miss analysis and partial-authenticity events?',
    'Empirical review of HRO near-miss taxonomies (aviation incident vs. accident distinctions, nuclear near-miss reporting) to determine whether near-misses functionally substitute for full catastrophic incidents in producing the ''authentic conditions'' this reading claims are necessary.',
    'If near-misses substitute, the reading''s core premise weakens substantially and the unresolvable-gap severity should be downgraded; if they do not substitute, the mountain framing (irreducible epistemic limit) is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hro_unresolvable_gap_severity, empirical, 'Whether near-miss events partially resolve the unresolvable competence gap this reading asserts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__real_incident_necessity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__real_incident_necessity, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comp_tr_t8, competence_occupation__real_incident_necessity, theater_ratio, 8, 0.22).
narrative_ontology:measurement(comp_tr_t16, competence_occupation__real_incident_necessity, theater_ratio, 16, 0.25).
narrative_ontology:measurement(comp_tr_t24, competence_occupation__real_incident_necessity, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__real_incident_necessity, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(comp_be_t8, competence_occupation__real_incident_necessity, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(comp_be_t16, competence_occupation__real_incident_necessity, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(comp_be_t24, competence_occupation__real_incident_necessity, base_extractiveness, 24, 0.31).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(competence_occupation__real_incident_necessity, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__real_incident_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__real_incident_necessity, 0.1).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__real_incident_necessity, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the competence_occupation kernel. simulation_sufficiency claims drills adequately substitute for real incidents (low unresolved gap, high tractability). hybrid_occupation claims multi-mechanism continuous exercise is required without settled configuration (moderate gap, procedural contestation). This reading (real_incident_necessity) claims the gap is structurally unresolvable because the authenticating condition is morally foreclosed. Each reading has a distinct epsilon: this reading's epsilon (0.31) reflects the absence of a captured-extraction structure at its own referent, distinguishing it from readings where training-industry incentives might drive higher measured extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
