% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading
 *   human_readable: Simulation-as-Sufficient-Catastrophe-Proxy Doctrine
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   This story authors ONE reading within the contested
 *   catastrophe_proxy_sufficiency kernel: the claim that structured
 *   simulation exercises are themselves sufficient — not merely useful, not a
 *   stopgap, but categorically adequate — to maintain operational competence
 *   in catastrophe-capable systems indefinitely, without any need for
 *   exposure to actual catastrophic events. Under this reading, the
 *   constraint is a coordination mechanism: it solves the real problem of
 *   rare, high-cost, high-destruction failure modes by letting organizations
 *   rehearse them safely and repeatedly, and it grounds a
 *   liability-protective certification regime for regulators. Extraction is
 *   authored low because no identifiable victim class bears a directed cost
 *   under this reading's own lights — frontline operators are trained and
 *   generally protected, not extracted from. The sibling readings
 *   (catastrophe_necessity_reading, hybrid_degradation_reading,
 *   simulation_fidelity_threshold) are NOT part of this constraint; they are
 *   separate constraint files with their own epsilon values, per the
 *   epsilon-invariance principle, and this story does not hedge, average, or
 *   describe their content internally.
 *
 * KEY AGENTS:
 *   - regulatory_bodies: institutional beneficiary and partial agenda-setter — gains liability shield from certifying against documented simulation hours
 *   - simulation_program_administrators: organized agenda-setter — designs and certifies the practice regime, professionally invested in its sufficiency
 *   - frontline_operators: moderate-power beneficiary/payer — trains under the regime, gains procedural fluency and legal cover, bears training time cost
 *   - simulator_technology_vendors: organized beneficiary — commercial basis of the doctrine's persistence
 *   - the_public: powerless beneficiary — relies on the certified system's safety without visibility into whether the sufficiency claim holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.28).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "Simulation-as-Sufficient-Catastrophe-Proxy Doctrine").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, "safety_engineering/organizational_learning/high_reliability_organizations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, '50fadbbc-d9b6-4be7-b526-9adbd026e412').
narrative_ontology:cs_kernel_codification('50fadbbc-d9b6-4be7-b526-9adbd026e412', distributed).
narrative_ontology:cs_authority_grounding('50fadbbc-d9b6-4be7-b526-9adbd026e412', expertise).
narrative_ontology:cs_interpretation_layer_present('50fadbbc-d9b6-4be7-b526-9adbd026e412').
narrative_ontology:cs_reading_relation('50fadbbc-d9b6-4be7-b526-9adbd026e412', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('50fadbbc-d9b6-4be7-b526-9adbd026e412', catastrophe_proxy_sufficiency__hybrid_degradation_reading, coexists_with).
narrative_ontology:cs_reading_relation('50fadbbc-d9b6-4be7-b526-9adbd026e412', catastrophe_proxy_sufficiency__simulation_fidelity_threshold, influences).
narrative_ontology:cs_axiom('50fadbbc-d9b6-4be7-b526-9adbd026e412', foundational, simulation_equivalence_is_categorical).
narrative_ontology:cs_axiom_status(simulation_equivalence_is_categorical, holdable).
narrative_ontology:cs_axiom_grounding('50fadbbc-d9b6-4be7-b526-9adbd026e412', simulation_equivalence_is_categorical, empirically_contingent).
narrative_ontology:cs_axiom('50fadbbc-d9b6-4be7-b526-9adbd026e412', secondary, competence_maintenance_requires_no_real_catastrophic_exposure).
narrative_ontology:cs_axiom_status(competence_maintenance_requires_no_real_catastrophic_exposure, holdable).
narrative_ontology:cs_axiom_grounding('50fadbbc-d9b6-4be7-b526-9adbd026e412', competence_maintenance_requires_no_real_catastrophic_exposure, instrumental).
narrative_ontology:cs_reference_frame('50fadbbc-d9b6-4be7-b526-9adbd026e412', simulation_regime_as_full_competence_substitute).
narrative_ontology:cs_drift_state('50fadbbc-d9b6-4be7-b526-9adbd026e412', contemporary_certification_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('50fadbbc-d9b6-4be7-b526-9adbd026e412', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_program_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulator_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, the_public).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certifies operators as competent on the basis of documented simulation hours and exercise completion records. This certification discharges the regulator's own liability exposure — if a certified operator later fails during a real event, the regulator can point to the simulation regime as due diligence performed. The regulator sets the simulation curriculum and audits compliance but does not itself run the simulations.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, regulatory_bodies, agenda_setter).

% Designs, schedules, and scores the simulation exercises; certifies personnel as proficient based on simulator performance. Their institutional standing depends on the simulation regime being accepted as adequate; they have professional and budgetary incentive to defend sufficiency but also genuinely refine scenarios based on incident review and near-miss data.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_program_administrators, agenda_setter,
    organized, biographical, constrained, national).

% Undergoes recurring simulation training as a condition of continued certification and employment. Benefits from structured, repeatable practice that builds procedural fluency and from a legal/professional shield ('I trained per protocol') if an incident occurs. Bears the time cost of training cycles and the risk that simulator-honed reflexes may not transfer cleanly to a genuinely novel failure mode, but this risk is not concentrated or extracted from them by any identifiable party — it is a shared epistemic limitation of the arrangement, not a directed cost.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, frontline_operators, payer).

% Sells and maintains the simulation platforms that constitute the practice mechanism. Benefits directly from the doctrine's persistence, since it is the commercial basis for the entire training-and-certification industry, but does not administer certification and does not extract from operators beyond ordinary commercial transactions.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulator_technology_vendors, beneficiary,
    organized, biographical, mobile, global).

% Relies on the certified system (aviation, nuclear, maritime, or similar high-reliability domain) operating safely. If the doctrine is correct, the public benefits from continuously maintained competence without needing real catastrophes to occur first. Has no direct role in the simulation regime and cannot verify its sufficiency independently.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, the_public, beneficiary,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that real catastrophic events are too rare, too costly, and too dangerous to serve as the primary vehicle for maintaining operator competence — simulation lets the organization rehearse rare failure modes repeatedly, safely, and on a controllable schedule, coordinating training investment across an entire workforce without waiting for accidents to occur.
% TRANSFER_FUNCTION: Moves liability assurance from an unverifiable claim ('our people would perform well in a real crisis') to a documented, auditable claim ('our people completed N certified simulation hours'), which regulators and organizations can point to. No material resource transfer from a victim class occurs under this reading — the transfer is epistemic/legal (documentation substituting for a currently-unfalsifiable competence claim).
% ABSENT_VOICES: Practitioners of the sibling readings (catastrophe_necessity and hybrid_degradation) would object that this reading conflates procedural fluency with the tacit, stress-tested judgment that only genuine catastrophic uncertainty produces; they are not in this constraint's own room because this story is authored strictly from within the simulation-sufficiency framework, per the kernel-reading discipline.
% DISAPPEARANCE_RATIONALE: If the simulation-as-sufficient-proxy doctrine were abandoned overnight, certification regimes across aviation, nuclear power, and similar domains would lose their evidentiary basis; regulators would need an alternative competence-verification mechanism (which does not currently exist at scale), and simulation vendors, training administrators, and certified operators would all face immediate legitimacy and liability exposure.
% FOUNDING_PROBLEM: Organizations operating catastrophe-capable systems needed a way to build and verify operator competence for failure modes that occur too rarely, too destructively, or too irreversibly to be safely practiced live.
% FOUNDING_PROBLEM_CORROBORATION: Independent safety researchers and post-incident investigation boards (e.g., aviation and nuclear incident review bodies) attest that the scarcity-of-real-catastrophe problem is real and unresolved by any means other than simulation or waiting for accidents; this corroboration comes from investigators outside the regulatory and vendor beneficiary set, though those same investigators are frequently the ones raising the hybrid-degradation and fidelity-threshold objections to this reading's sufficiency claim.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).
:- end_tests(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end) because, within this reading, no party is structurally positioned to extract rents through the simulation regime — the transfer described in transfer_function is epistemic/legal substitution, not resource extraction. Theater ratio is authored moderate and rising (0.18 to 0.30) because as certification volume grows, some portion of simulation activity plausibly drifts toward box-checking documentation rather than genuinely stress-testing competence — this is the seam where the hybrid_degradation and fidelity_threshold siblings would contest this reading's own claim, but from within this reading it remains a manageable, non-dominant theater component. Suppression is moderate (0.28): this reading does not require coercive enforcement to persist, but regulatory mandate does compel participation, which is real if not extractive suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and simulation administrators sit near the beneficiary end: they set the terms of the practice regime and collect the liability/legitimacy benefit of its acceptance. Frontline operators sit closer to symmetric — real costs (time, cognitive load, the risk that simulation-honed skill may not transfer) balanced against real benefits (competence, legal cover, employment continuity) — hence dual beneficiary/payer roles rather than a pure victim declaration. No agent is declared a pure victim under this reading because the reading's own structural claim is that the arrangement produces genuine competence, not that it extracts from anyone while claiming to.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rare catastrophic events cannot be safely used for training) is authored as live and corroborated by investigators outside the regulatory/vendor beneficiary set. This blocks the mandatrophy misreading in the trivial direction: a critic cannot simply assert that simulation-training regimes are pure institutional self-perpetuation with no underlying coordination function — the scarcity problem the regime addresses is real and independently attested. Whether the STATUS QUO SUFFICIENCY of simulation to solve the live problem is itself accurate is exactly the contested claim the kernel's sibling readings dispute; this story does not adjudicate that dispute, it only fixes clean values for the sufficiency reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_categorical_vs_conditional,
    'Is simulation-based competence maintenance categorically sufficient (this reading), or does it hold only above some undetermined simulation-fidelity threshold (simulation_fidelity_threshold reading), or does it degrade over generational timescales despite adequate fidelity (hybrid_degradation_reading)?',
    'Longitudinal comparison of incident-response performance between operators trained purely under high-fidelity simulation regimes versus operators with historical exposure to real catastrophic events within the same domain, tracked across multiple career generations without an intervening real catastrophe.',
    'If sufficiency turns out to be conditional on fidelity or to degrade generationally, this reading''s zero-victim, low-extraction structure is itself a symptom of unfalsifiability rather than genuine adequacy — the regulatory liability shield would then be certifying competence that is not actually being maintained, converting the beneficiary structure (regulatory_bodies, simulation_program_administrators) into something closer to a tangled rope with the public as an undeclared, diffuse victim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sufficiency_categorical_vs_conditional, conceptual, 'Whether simulation-catastrophe equivalence is categorical, threshold-dependent, or time-degrading — the core disagreement among the kernel''s four readings.').

omega_variable(
    regulatory_liability_shield_incentive_bias,
    'Does the regulatory body''s own liability-protection interest in accepting the sufficiency claim bias its willingness to certify simulation regimes as adequate, independent of whether they actually are?',
    'Compare certification standards and audit rigor across regulatory bodies with differing liability exposure structures (e.g., state-indemnified vs. directly-liable regulators) for the same class of catastrophe-capable system.',
    'If liability-protection incentive dominates, the regulatory_bodies beneficiary role is doing more structural work than this reading''s low-extraction, rope-leaning claim acknowledges, and the constraint would compute closer to a false-summit or tangled-rope pattern despite the doctrine''s genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_liability_shield_incentive_bias, empirical, 'Whether regulatory self-interest in liability protection distorts sufficiency certification independent of actual competence outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(cata_tr_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(cata_tr_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(cata_tr_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(cata_tr_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t8, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(cata_be_t16, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(cata_be_t24, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(cata_be_t32, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(cata_be_t40, catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, simulation_fidelity_threshold).

% DUAL FORMULATION NOTE:
% Four constraint files decompose the catastrophe_proxy_sufficiency kernel: this file (simulation_as_proxy_catastrophe_reading, the strong/categorical sufficiency claim), catastrophe_necessity_reading (the direct denial — only real catastrophe suffices), hybrid_degradation_reading (procedural competence holds, tacit/stress competence erodes generationally), and simulation_fidelity_threshold (sufficiency is real but technology/fidelity-conditional, not categorical). Each carries its own epsilon, beneficiary/victim structure, and claimed_type per the epsilon-invariance principle; they are linked via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
