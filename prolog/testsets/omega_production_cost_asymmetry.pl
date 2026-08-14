% ============================================================================
% CONSTRAINT STORY: omega_production_cost_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_omega_production_cost_asymmetry, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: omega_production_cost_asymmetry
 *   human_readable: Asymmetry Between Falsifier-Generation Cost and Belief-Revision Cost
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This constraint names a structural asymmetry that emerges from a genuine
 *   natural/economic fact (the collapse of LLM inference pricing, an
 *   empirically documented ~99% decline 2023-2026) crossed with a fact that
 *   is NOT a mountain at all: the cost of actually changing one's mind,
 *   absorbing a disconfirming result, or paying a social price for a failed
 *   precommitment. The mountain here is narrowly the cost-curve divergence
 *   itself — it is a structural feature of any system where
 *   evidence-production and evidence-absorption sit on different economic
 *   footings, and it would hold regardless of who is looking at it or what
 *   institution enforces it. What is NOT a mountain, and must not be
 *   conflated with it, is the institutional practice of treating cheap
 *   falsifier generation as if it constituted rigor — that practice has
 *   identifiable beneficiaries and is where the FSM (false-summit) signature
 *   is deliberately invited: the underlying cost-curve fact is natural, but
 *   institutions and analysts benefit from treating the resulting
 *   register-production as equivalent to real epistemic discipline, which it
 *   structurally is not.
 *
 * KEY AGENTS:
 *   - llm_assisted_analysts_with_institutional_slack: benefit from cheap generation without bearing revision costs
 *   - institutions_that_can_point_to_omega_registers_as_rigor: adopt registers as governance theater
 *   - analysts_without_time_or_tooling_to_curate_omega_menus: structurally disadvantaged despite equal API access
 *   - parties_bound_by_real_precommitments_who_absorb_disconfirmation: bear the unmoved cost
 *   - model_providers: drive the cost collapse but are excluded from the epistemic-norm conversation
 *   - philosophy_of_science_observers: document the gap analytically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(omega_production_cost_asymmetry, 0.61).
domain_priors:suppression_score(omega_production_cost_asymmetry, 0.42).
domain_priors:theater_ratio(omega_production_cost_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, extractiveness, 0.61).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(omega_production_cost_asymmetry, mountain).
narrative_ontology:human_readable(omega_production_cost_asymmetry, "Asymmetry Between Falsifier-Generation Cost and Belief-Revision Cost").
narrative_ontology:topic_domain(omega_production_cost_asymmetry, "epistemology/philosophy_of_technology/institutional_analysis").

domain_priors:emerges_naturally(omega_production_cost_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(omega_production_cost_asymmetry, 'cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3').
narrative_ontology:cs_kernel_codification('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', distributed).
narrative_ontology:cs_authority_grounding('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', distributed).
narrative_ontology:cs_reading_relation('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', omega_production_cost_asymmetry__positional_disagreement_standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', omega_production_cost_asymmetry__positional_disagreement_pragmatist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', omega_production_cost_asymmetry__positional_disagreement_proceduralist_reading, forecloses).
narrative_ontology:cs_axiom('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', foundational, cheap_generation_relocates_but_does_not_eliminate_cost).
narrative_ontology:cs_axiom_status(cheap_generation_relocates_but_does_not_eliminate_cost, holdable).
narrative_ontology:cs_axiom_grounding('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', cheap_generation_relocates_but_does_not_eliminate_cost, empirically_contingent).
narrative_ontology:cs_axiom('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', foundational, evidentiary_force_does_not_require_production_cost_expense).
narrative_ontology:cs_axiom_status(evidentiary_force_does_not_require_production_cost_expense, holdable).
narrative_ontology:cs_axiom_grounding('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', evidentiary_force_does_not_require_production_cost_expense, instrumental).
narrative_ontology:cs_reference_frame('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', expensive_falsifier_production_as_the_bottleneck).
narrative_ontology:cs_drift_state('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', post_llm_cost_collapse_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cfbef53c-2a6e-4fcd-b5f1-ea5dffbf81f3', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, llm_assisted_analysts_with_institutional_slack).
narrative_ontology:constraint_beneficiary(omega_production_cost_asymmetry, institutions_that_can_point_to_omega_registers_as_rigor).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, analysts_without_time_or_tooling_to_curate_omega_menus).
narrative_ontology:constraint_victim(omega_production_cost_asymmetry, parties_bound_by_real_precommitments_who_absorb_disconfirmation).
narrative_ontology:constraint_vindicates(omega_production_cost_asymmetry, cheap_generation_does_not_entail_cheap_revision).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have the time, tooling budget, and survivable public-error tolerance to run generate-omega-candidates loops at near-zero marginal cost, curate the outputs, and present a polished register of alternative positions and kill conditions as evidence of epistemic rigor. Because generation is now cheap for them, they can produce the APPEARANCE of having stress-tested a claim without ever being the one who has to pay if the claim is falsified.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, llm_assisted_analysts_with_institutional_slack, beneficiary,
    organized, biographical, arbitrage, global).

% Adopt the practice of requiring or displaying omega/falsifier registers as a governance or publication norm. Benefit from the appearance of epistemic humility and self-audit at essentially zero added cost, since generating the register is now cheap. Set the norm that a well-populated register substitutes for evidence of actual belief revision, without themselves bearing the cost of retracting a position once committed.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, institutions_that_can_point_to_omega_registers_as_rigor, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(omega_production_cost_asymmetry, institutions_that_can_point_to_omega_registers_as_rigor, agenda_setter).

% Lack the discretionary time, compute budget, or professional slack to generate and curate a competitive menu of falsifiers and alternative-position samples, even though the raw API cost has collapsed for everyone. Their comparative disadvantage is not access to the tool but the surrounding labor of selection, framing, and publication risk-bearing. Their positions get read as less rigorous for lacking a register they could not afford to produce, regardless of underlying merit.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, analysts_without_time_or_tooling_to_curate_omega_menus, payer,
    moderate, biographical, constrained, national).

% Are the ones who actually declared a kill condition, staked a career claim, or made a policy commitment, and who must now absorb the social, professional, or material cost when the disconfirming result arrives. Cheap falsifier generation does nothing for them: the falsifiers were never the bottleneck. They pay in reputation, resources, or relationships regardless of how cheap it became to produce the evidence that proved them wrong.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, parties_bound_by_real_precommitments_who_absorb_disconfirmation, payer,
    powerless, biographical, trapped, local).

% Drive the token-cost collapse through infrastructure and competition, but are not party to the epistemic norms that use their output as a rigor signal. They have no stake in whether cheap generation translates into actual belief revision and are not consulted on how their tooling gets repurposed as evidence of institutional humility.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, model_providers, excluded,
    institutional, generational, arbitrage, global).

% Study the gap between the falsifiability of a claim (now trivially cheap to demonstrate) and the actual practice of belief revision under disconfirmation (unchanged by any tooling improvement). Document that this gap is a structural feature of any epistemic system where evidence-production and evidence-absorption sit on different cost curves.
narrative_ontology:constraint_stakeholder(omega_production_cost_asymmetry, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(omega_production_cost_asymmetry, diffuse).
narrative_ontology:fixing_cost_class(omega_production_cost_asymmetry, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine coordination problem the falling cost of generation solves: it used to be expensive to even articulate a serious alternative hypothesis or falsifier, so committing to test one's own claims against a real alternative was rare simply for want of the labor to produce the alternative. Cheap generation removes that particular bottleneck and makes broader, more thorough self-audit affordable in principle.
% TRANSFER_FUNCTION: The arrangement moves credibility and reputational capital from analysts and institutions that cannot produce a curated falsifier/omega register toward those that can, without moving any actual cost of belief revision. It converts a display of methodological virtue (a populated register) into evidence of rigor, while leaving the real cost of retracting a position, absorbing a disconfirming result, or paying a social price for having been wrong exactly where it was.
% ABSENT_VOICES: Model providers, who created the cost collapse, have no voice in how it is used as an epistemic signal. Parties who are actually bound by precommitments and who bear disconfirmation costs are rarely the ones curating the omega registers that get cited as evidence of rigor — their voice is structurally distinct from the voice that produces the falsifier menu.
% DISAPPEARANCE_RATIONALE: If cheap generation vanished overnight, institutions that had come to treat populated omega registers as a rigor signal would lose that display mechanism and would have to find another way to signal self-audit, or admit they had substituted a cheap artifact for a real one. Parties who actually pay for belief revision would be entirely unaffected either way, since the constraint never touched their cost. Whether the 'world rearranges' therefore depends on which population you ask: for the display economy it rearranges considerably; for the actual practice of changing one's mind under disconfirmation, nothing changes at all — which is exactly the asymmetry the constraint names.
% FOUNDING_PROBLEM: Historically, generating a serious falsifier, an alternative-position sample, or a taxonomy of possible objections required significant human labor — reading, drafting, imagining a genuinely different framing — so most claims went untested against real alternatives simply because producing them was expensive. LLM inference made that production step nearly free.
% FOUNDING_PROBLEM_CORROBORATION: Philosophy-of-science observers and the documented empirical cost-per-token trend (a public, non-partisan dataset) corroborate that generation cost collapsed roughly 99% between 2023 and 2026 — independent of any party's interest. No comparable outside corroboration exists for a matching decline in belief-revision or retraction rates; the absence of such a metric is itself part of what the constraint asserts, and no beneficiary group has produced one.
narrative_ontology:disappearance_verdict(omega_production_cost_asymmetry, contested).
narrative_ontology:founding_problem_status(omega_production_cost_asymmetry, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(omega_production_cost_asymmetry, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(omega_production_cost_asymmetry, 'none', 1).
narrative_ontology:epsilon_provenance(omega_production_cost_asymmetry, 0.61, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(omega_production_cost_asymmetry_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(omega_production_cost_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(omega_production_cost_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(omega_production_cost_asymmetry),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(omega_production_cost_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(omega_production_cost_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   extractiveness (0.61) and theater_ratio (0.58) are both authored as substantial and rising, because the divergence between generation cost and revision cost creates a growing incentive to substitute the former for evidence of the latter — a textbook Goodhart-style metric substitution as institutions adopt 'has an omega register' as a rigor proxy. accessibility_collapse is authored LOW (0.35) deliberately: unlike a genuine mountain, alternatives to this practice have NOT collapsed — one can still insist on tracking actual retraction rates, and some observers do. resistance is moderate (0.55): philosophers of science and rigorous adversarial-collaboration practitioners actively push back against treating cheap generation as evidence of rigor. This combination (moderate accessibility_collapse, moderate resistance) is exactly what should make a reader suspicious of the mountain claim for the DOWNSTREAM institutional practice, even though the underlying cost-curve fact is genuinely natural.
 *
 * DIRECTIONALITY LOGIC:
 *   The underlying economic fact (falling API cost) benefits no one in particular and is close to a true positional invariant — it is the same fact from every seat. But the SOCIAL USE of that fact is highly directional: institutions and well-resourced analysts (low d, beneficiary end) can convert cheap generation into a credibility signal, while analysts without curation slack and parties actually bound by precommitments (high d, target end) see no corresponding relief. The directionality split tracks slack (time, tooling, survivable error), not standpoint in the Fricker sense — this is the instrumentalist reading's key departure from the standpoint reading: the exploited party here is defined by resource asymmetry in curation capacity, not by social marginalization per se, though the two often correlate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (expensive falsifier production suppressing real self-audit) is genuinely partially solved by the cost collapse — this is not a pure zombie mandate. But the founding_problem_status is authored 'contested' because the mandate has partially drifted: an arrangement built to make self-audit affordable has been repurposed by some institutions into a compliance-theater signal that substitutes for the self-audit it was meant to enable. The mismatch between founding_problem_status=contested and a partial disappearance_verdict=contested is the diagnostic signal: a clean mountain would show status=live/dead with a clean world_unchanged verdict; the contested/contested pairing here flags that a natural fact is doing partial cover-story work for an institutional practice riding on top of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_practice_boundary,
    'Is the cost-curve divergence itself (a natural economic/computational fact) cleanly separable from the institutional practice of treating cheap falsifier generation as evidence of epistemic rigor (a constructed practice with beneficiaries), or does declaring beneficiaries on this story mean the ''mountain'' framing has already leaked into describing the practice rather than the underlying fact?',
    'Track whether institutions that adopt omega-register requirements show measurably different actual retraction/policy-change rates than institutions that do not, controlling for domain. If no difference is found, the register requirement is pure theater riding on a real natural fact; if a difference is found, the practice has genuine coordination value beyond signaling.',
    'If the practice is cleanly separable and inert, this story should decompose into two: a genuine mountain (the cost-curve fact) and a tangled_rope or snare (the institutional register-as-rigor practice). If entangled, the FSM flag on this single story is the correct representation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_practice_boundary, conceptual, 'Whether the natural cost-curve fact and the institutional practice built on it require decomposition into separate constraint stories.').

omega_variable(
    revision_cost_immovability,
    'Is the cost of abiding a precommitment (changing one''s mind, absorbing disconfirmation, paying a social price) genuinely untouched by any tooling improvement, or are there emerging mechanisms (reputation systems, prediction markets, structured retraction norms) that are beginning to lower it?',
    'Longitudinal tracking of retraction rates, public correction norms, and career-cost data for stated forecasters/analysts pre- and post- the LLM cost collapse, compared across fields with and without emerging retraction-support tooling.',
    'If revision cost is beginning to move (even slowly), the asymmetry is narrowing and the constraint''s severity should be revised downward over time; if genuinely immovable, the asymmetry is a stable structural feature and the mountain claim for it strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revision_cost_immovability, empirical, 'Whether belief-revision cost is truly fixed or merely slower-moving than generation cost.').

omega_variable(
    curation_slack_vs_standpoint,
    'Is the beneficiary/victim split in this story better explained by curation slack (time, tooling, survivable error — the instrumentalist account) or does it substantially correlate with, and partly reduce to, standpoint-theoretic social marginalization (the standpoint reading''s account)?',
    'Empirical survey of who actually produces and curates omega/falsifier registers in practice, cross-tabulated against institutional position, social location, and available discretionary time/budget.',
    'If curation slack correlates strongly with social marginalization, the instrumentalist and standpoint readings converge empirically even though they diverge axiomatically; if they diverge empirically, the two readings pick out genuinely different victim populations and should remain separately authored constraints per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(curation_slack_vs_standpoint, empirical, 'Whether curation-slack disadvantage and standpoint-theoretic marginalization pick out the same or different populations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(omega_production_cost_asymmetry, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(omeg_tr_t0, omega_production_cost_asymmetry, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(omeg_tr_t0, observed).
narrative_ontology:measurement(omeg_tr_t6, omega_production_cost_asymmetry, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(omeg_tr_t6, observed).
narrative_ontology:measurement(omeg_tr_t12, omega_production_cost_asymmetry, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(omeg_tr_t12, observed).
narrative_ontology:measurement(omeg_tr_t18, omega_production_cost_asymmetry, theater_ratio, 18, 0.44).
narrative_ontology:measurement_basis(omeg_tr_t18, observed).
narrative_ontology:measurement(omeg_tr_t24, omega_production_cost_asymmetry, theater_ratio, 24, 0.51).
narrative_ontology:measurement_basis(omeg_tr_t24, observed).
narrative_ontology:measurement(omeg_tr_t30, omega_production_cost_asymmetry, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(omeg_tr_t30, projected).
narrative_ontology:measurement(omeg_tr_t36, omega_production_cost_asymmetry, theater_ratio, 36, 0.58).
narrative_ontology:measurement_basis(omeg_tr_t36, projected).

% Extraction over time
narrative_ontology:measurement(omeg_be_t0, omega_production_cost_asymmetry, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(omeg_be_t0, observed).
narrative_ontology:measurement(omeg_be_t6, omega_production_cost_asymmetry, base_extractiveness, 6, 0.4).
narrative_ontology:measurement_basis(omeg_be_t6, observed).
narrative_ontology:measurement(omeg_be_t12, omega_production_cost_asymmetry, base_extractiveness, 12, 0.47).
narrative_ontology:measurement_basis(omeg_be_t12, observed).
narrative_ontology:measurement(omeg_be_t18, omega_production_cost_asymmetry, base_extractiveness, 18, 0.52).
narrative_ontology:measurement_basis(omeg_be_t18, observed).
narrative_ontology:measurement(omeg_be_t24, omega_production_cost_asymmetry, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(omeg_be_t24, observed).
narrative_ontology:measurement(omeg_be_t30, omega_production_cost_asymmetry, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(omeg_be_t30, projected).
narrative_ontology:measurement(omeg_be_t36, omega_production_cost_asymmetry, base_extractiveness, 36, 0.61).
narrative_ontology:measurement_basis(omeg_be_t36, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(omega_production_cost_asymmetry, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(omega_production_cost_asymmetry, information_standard).
narrative_ontology:boltzmann_floor_override(omega_production_cost_asymmetry, 0.05).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, positional_disagreement_standpoint_reading).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, positional_disagreement_proceduralist_reading).
narrative_ontology:affects_constraint(omega_production_cost_asymmetry, positional_disagreement_pragmatist_reading).

% DUAL FORMULATION NOTE:
% This story is the instrumentalist_reading member of the positional_disagreement_as_evidence kernel family (kernel_id: positional_disagreement_as_evidence). Sibling readings (standpoint, pragmatist, proceduralist) are separate constraint stories with different beneficiary/victim structures and different ε values, per the ε-invariance principle: the standpoint reading's ε is authored around credibility-injustice asymmetry, the pragmatist reading's ε is authored near-zero (a rope-like ongoing-inquiry coordination problem with no fixed victim set), and the proceduralist reading's ε is authored around whoever evades a designed precommitment procedure. This story's axiom directly contradicts the proceduralist reading's cost-location claim (see axiom_contradictions of record) — the two cannot be merged into one constraint without erasing that contradiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
