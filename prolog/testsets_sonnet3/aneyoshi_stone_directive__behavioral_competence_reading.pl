% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive__behavioral_competence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: aneyoshi_stone_directive__behavioral_competence_reading
 *   human_readable: Aneyoshi Tsunami Stone — Behavioral Land-Use Line (Competence Reading)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * KEY AGENTS:
 *   - aneyoshi_households_above_the_marker: primary beneficiary of compliance (survived 2011) — powerless/constrained
 *   - aneyoshi_households_below_the_marker: excluded from the marker's practical force during the calm decades — powerless/trapped
 *   - local_and_national_land_use_planners: institutional observer who could encode the line into binding zoning but does not administer the stone itself
 *   - future_coastal_residents: excluded, civilizational time horizon, bear the consequence of whether the line is respected going forward
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, 0.08).
domain_priors:theater_ratio(aneyoshi_stone_directive__behavioral_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive__behavioral_competence_reading, mountain).
narrative_ontology:human_readable(aneyoshi_stone_directive__behavioral_competence_reading, "Aneyoshi Tsunami Stone — Behavioral Land-Use Line (Competence Reading)").
narrative_ontology:topic_domain(aneyoshi_stone_directive__behavioral_competence_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_directive__behavioral_competence_reading, '55849b66-83b6-46a5-97bd-56a06d7184a9').
narrative_ontology:cs_kernel_codification('55849b66-83b6-46a5-97bd-56a06d7184a9', fixed_text).
narrative_ontology:cs_authority_grounding('55849b66-83b6-46a5-97bd-56a06d7184a9', practice).
narrative_ontology:cs_reading_relation('55849b66-83b6-46a5-97bd-56a06d7184a9', aneyoshi_stone_directive__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('55849b66-83b6-46a5-97bd-56a06d7184a9', foundational, physical_hazard_line_retains_force_absent_validation).
narrative_ontology:cs_axiom_status(physical_hazard_line_retains_force_absent_validation, holdable).
narrative_ontology:cs_axiom_grounding('55849b66-83b6-46a5-97bd-56a06d7184a9', physical_hazard_line_retains_force_absent_validation, empirically_contingent).
narrative_ontology:cs_axiom('55849b66-83b6-46a5-97bd-56a06d7184a9', secondary, dormancy_is_not_lapse_for_geological_facts).
narrative_ontology:cs_axiom_status(dormancy_is_not_lapse_for_geological_facts, holdable).
narrative_ontology:cs_axiom_grounding('55849b66-83b6-46a5-97bd-56a06d7184a9', dormancy_is_not_lapse_for_geological_facts, empirically_contingent).
narrative_ontology:cs_reference_frame('55849b66-83b6-46a5-97bd-56a06d7184a9', id_1933_inscribed_inundation_line_as_binding_boundary).
narrative_ontology:cs_drift_state('55849b66-83b6-46a5-97bd-56a06d7184a9', pre_2011_calm_period, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('55849b66-83b6-46a5-97bd-56a06d7184a9', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households_above_the_marker).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households that built and continue to live above the stone's inscribed line on the ridge above Aneyoshi. They did not choose the line for economic advantage; they inherited a physical fact (the historically observed inundation limit) encoded as a boundary. In the 2011 Tōhoku tsunami, structures above the line survived while the village below was destroyed. Their 'benefit' is survival, not extraction from anyone else — no rent is collected, no one pays them.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households_above_the_marker, beneficiary,
    powerless, generational, constrained, local).

% Households and businesses that, across the 78-year gap between the 1933 Shōwa Sanriku tsunami (when the stone was erected) and the 2011 event, resettled below the marked line for convenience, access to the harbor, and economic activity. They are not consulted by the stone; it makes no argument, only states a line. They bore the fatal cost of disregarding it in 2011. Their exclusion is not enforced by any authority — it is simply that the geological fact does not negotiate.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_households_below_the_marker, excluded,
    powerless, biographical, trapped, local).

% Municipal and national disaster-planning bodies that, after 2011, examined the stone's line and other tsunami markers across the Sanriku coast as calibration data for hazard maps and rebuilding codes. They administer zoning decisions but do not administer the stone itself — they can choose to encode its line into binding regulation or not; the stone's physical referent (historical maximum run-up) does not change based on their decision.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, local_and_national_land_use_planners, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive__behavioral_competence_reading, local_and_national_land_use_planners, agenda_setter).

% Not-yet-born or not-yet-arrived residents of the Sanriku coast whose safety will depend on whether the physical inundation line the stone records is respected in future settlement, regardless of whether they know the stone's history or ever read its inscription.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive__behavioral_competence_reading, future_coastal_residents, excluded,
    powerless, civilizational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone encodes a single, verifiable physical fact — the historical maximum tsunami run-up line observed at this location — into a durable, low-maintenance, illiterate-proof marker that requires no institution, no enforcement budget, and no periodic recalibration to remain informative across generations.
% TRANSFER_FUNCTION: Nothing is transferred between parties through this reading of the constraint. No payment, labor, or status moves from one group to another as a function of the line's existence. Households above the line are not subsidized by households below it; the line simply reports where water reached before.
% ABSENT_VOICES: The 1933 survivors who set the stone are gone and cannot testify to their intent directly; their intent is inferred from the inscription itself ('do not build below this point'). Descendants who built below the line in the decades of calm were not overruled by any authority — they simply did not treat the marker as binding, and no one intervened.
% DISAPPEARANCE_RATIONALE: Under this reading the stone is a durable record of a physical fact, not an active social arrangement. If the stone itself were destroyed, the underlying inundation line it recorded would remain exactly where it was — the tsunami hazard boundary is a property of the coastline's geometry and historical wave run-up, not of the stone's continued presence. The world does not rearrange because nothing about it was ever contingent on the marker existing; the marker only ever reported a fact that predates and outlives it.
% FOUNDING_PROBLEM: In the aftermath of the 1933 Shōwa Sanriku tsunami, survivors needed a way to transmit the location of the safe/unsafe boundary to descendants who would not have witnessed the disaster themselves and who might, after enough calm decades, be tempted back toward the harbor.
% FOUNDING_PROBLEM_CORROBORATION: Independent post-2011 engineering surveys (Japanese national and international tsunami-engineering teams) measured the 2011 inundation extent against the 1933 marker lines across multiple Sanriku villages, including Aneyoshi, and found the historical lines to be accurate predictors of the 2011 run-up at many sites — corroboration from geoscientists and disaster-engineering researchers outside the households the stone concerns, not from the beneficiary households themselves.
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive__behavioral_competence_reading, world_unchanged).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_directive__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(aneyoshi_stone_directive__behavioral_competence_reading, 0.05, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, ExtMetricName, E),
    domain_priors:suppression_score(aneyoshi_stone_directive__behavioral_competence_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(aneyoshi_stone_directive__behavioral_competence_reading),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(aneyoshi_stone_directive__behavioral_competence_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(aneyoshi_stone_directive__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is scored near-zero and essentially flat (0.03-0.05) across the full 78-year interval because, under this reading, the stone asserts a physical fact rather than administering a transfer between parties — no group's compliance funds another group's benefit. Suppression is low (0.08) because nothing coerces households to build above the line; the 'directive' has no enforcement mechanism, only a durable inscription. Theater ratio rises slowly (0.02 to 0.10) reflecting the stone's gradual shift from active safety instruction toward partial folklore status in the decades of calm, but stays low overall because the reading holds that its behavioral force, while weakening in practice for some residents, never fully lapsed as a structural fact about the hazard. Accessibility collapse is high (0.82): once the tsunami run-up geometry at this location is understood, there genuinely isn't an alternative safe elevation to substitute for it — the physical constraint does not negotiate. Resistance is low (0.06): no organized party actively contests the stone's claim; the households who built below it were not defying an authority, they were discounting a low-perceived-probability warning under ordinary settlement pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, aneyoshi_households_above_the_marker are declared beneficiaries not because the constraint transfers anything to them from someone else, but because compliance with the physical fact benefited them directly (survival) with no corresponding cost imposed on any other party. This is the FSM (false-summit) trigger condition the schema flags for mountains with declared beneficiaries — hence the omega below interrogating whether 'benefit from compliance with a physical fact' should even be modeled as a beneficiary relation, or whether it collapses the mountain/beneficiary distinction. Households below the marker are 'excluded' rather than 'payer' because no transfer runs from them to anyone; they simply did not benefit from a fact they discounted. No agent is a payer in this reading because nothing is extracted — this is precisely why the sibling reading (commemorative_husk) is needed to carry any story about social cost of non-compliance, cultural memory decay, or institutional failure to re-validate the marker's authority, none of which belong in this reading's ε.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in the opposite direction from the usual case: the risk here is not that a dead coordination function is being defended as if still needed, but that a genuinely still-live physical constraint might be dismissed as inert institutional residue merely because it went unvalidated (untested by an actual tsunami) for 78 years. The founding_problem_status is declared 'live' precisely because the underlying hazard geometry does not expire with disuse — unlike a bureaucratic rule whose function can genuinely die, a hazard-line marker's function is dormant, not dead, during any interval without a triggering event. The 2011 event is the validation that discriminates between this reading (directive stayed behaviorally live under the surface) and the commemorative_husk reading (directive had lost behavioral force and needed revalidation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_relation_on_physical_fact,
    'Is ''benefiting from having correctly heeded a physical fact'' the same structural relation as ''benefiting from a constructed constraint,'' or does treating households above the marker as beneficiaries improperly imports a constructed-benefit frame onto what is actually a description of correct risk assessment?',
    'Compare against other mountain-with-beneficiary cases in the corpus (e.g., gravity-aware engineering codes) to see whether the FSM signature reliably distinguishes ''benefits from compliance with a real hazard'' from ''benefits from a constructed rule dressed as natural law.'' If the distinguishing feature is the absence of any victim/payer group whatsoever (present here), FSM should not fire despite the declared beneficiary.',
    'If the FSM override fires here, this reading would be reclassified toward tangled_rope despite having no victim and no active enforcement, which would misrepresent a genuine physical-hazard marker as an extractive arrangement. Resolving this in favor of ''no extraction because no payer'' would confirm the mountain classification should hold even with a declared beneficiary in this specific structural pattern (beneficiary but zero victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_relation_on_physical_fact, conceptual, 'Whether declaring survivors as beneficiaries of a hazard-line marker triggers FSM inappropriately given the total absence of any victim group.').

omega_variable(
    kernel_reading_locus_of_disagreement,
    'This constraint is one reading (behavioral_competence_reading) of the aneyoshi_stone_directive kernel; the sibling reading (commemorative_husk_reading) holds that the directive''s behavioral force lapsed during 1933-2011 and was only retroactively revalidated by the 2011 event. Where exactly does the disagreement between readings live — is it about the stone''s factual accuracy (both readings likely agree it was accurate), or specifically about whether unvalidated compliance-relevant force can be said to persist through a multi-generational gap with no triggering event and partial disregard by some residents?',
    'A resolution would require a criterion for when a dormant physical-hazard marker''s ''binding force'' is considered continuously live versus lapsed-and-later-revived: e.g., whether any household during the gap treated it as binding (partial continuity), versus requiring near-universal compliance to count as ''live.'' Ethnographic or municipal-record evidence of how many households actually treated the marker as binding during the calm decades would bear directly on this.',
    'If the behavioral_competence_reading is correct, this constraint remains a stable low-epsilon mountain throughout the interval. If the commemorative_husk_reading is correct instead, the 1933-2011 portion of this same physical marker''s history should properly be scored as a much higher-theater, lower-behavioral-force arrangement (a different constraint, per ε-invariance) rather than as a flat continuation of the founding directive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_disagreement, conceptual, 'Locates the substantive disagreement between the two sibling kernel readings in the criterion for continuity of behavioral force, not in disputed facts about the hazard itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1933, 0.02).
narrative_ontology:measurement(aney_tr_t1948, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1948, 0.04).
narrative_ontology:measurement(aney_tr_t1963, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1963, 0.06).
narrative_ontology:measurement(aney_tr_t1978, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1978, 0.08).
narrative_ontology:measurement(aney_tr_t1995, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 1995, 0.09).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_directive__behavioral_competence_reading, theater_ratio, 2011, 0.1).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1933, 0.03).
narrative_ontology:measurement(aney_be_t1948, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1948, 0.04).
narrative_ontology:measurement(aney_be_t1963, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1963, 0.04).
narrative_ontology:measurement(aney_be_t1978, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1978, 0.05).
narrative_ontology:measurement(aney_be_t1995, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_directive__behavioral_competence_reading, base_extractiveness, 2011, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(aneyoshi_stone_directive__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_directive__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(aneyoshi_stone_directive__behavioral_competence_reading, aneyoshi_stone_directive__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This story and aneyoshi_stone_directive__commemorative_husk_reading decompose the natural-language concept 'the Aneyoshi tsunami stone's directive' per the ε-invariance principle. Both share the same physical artifact (the inscribed stone) and the same underlying hazard geometry, but differ on whether the directive's BEHAVIORAL/BINDING force persisted continuously through the 1933-2011 gap (this reading, ε ~0.05, mountain) or lapsed into memorial status and was only retroactively revalidated (sibling reading, expected higher theater_ratio and a piton or scaffold-adjacent structure during the gap period). They are linked here rather than merged because measuring 'the directive' by factual accuracy versus by continuous behavioral force yields different ε trajectories over the interval — exactly the signal the framework treats as evidence of two constraints, not one measured two ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
