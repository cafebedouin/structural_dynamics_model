% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment Arms Right — Civic Republican (Citizen-Militia) Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the civic-republican reading of the Second
 *   Amendment kernel: the right protects armed citizenship as a structural
 *   prerequisite for republican self-governance, not a purely individual
 *   liberty pre-existing government (the individual_right_reading) and not a
 *   right belonging only to organized state militia structures (the
 *   collective_right_reading). Under this reading, the arms right and the
 *   duty of civic militia readiness are two faces of one arrangement —
 *   citizens are protected in keeping arms because, and to the extent that,
 *   they are expected to be part of the republic's collective defense
 *   capacity. This generates moderate, non-zero extraction: legislatures may
 *   condition the right on training and qualification (a coordination
 *   function absent from the individual-right reading), but that same
 *   conditioning burdens gun owners who reject or fall outside the
 *   civic-militia framing, and it inherited historical membership exclusions
 *   from the founding-era militia category.
 *
 * KEY AGENTS:
 *   - civic_militia_eligible_citizens: dual beneficiary/payer (moderate/constrained) — right and duty fused
 *   - state_and_federal_legislatures: agenda_setter (institutional/constrained) — regulatory authority bounded by civic-participation norm
 *   - unorganized_gun_owners_outside_militia_norm: payer (powerless/trapped) — weaker claim under this reading
 *   - populations_historically_excluded_from_militia_membership: payer (powerless/trapped) — inherited exclusion
 *   - judiciary_interpreting_the_amendment: observer (institutional/analytical) — adjudicates which reading controls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.38).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.32).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment Arms Right — Civic Republican (Citizen-Militia) Reading").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'be3577e6-1a45-46e4-ab01-a159f16b74ba').
narrative_ontology:cs_kernel_codification('be3577e6-1a45-46e4-ab01-a159f16b74ba', fixed_text).
narrative_ontology:cs_authority_grounding('be3577e6-1a45-46e4-ab01-a159f16b74ba', lineage).
narrative_ontology:cs_interpretation_layer_present('be3577e6-1a45-46e4-ab01-a159f16b74ba').
narrative_ontology:cs_reading_relation('be3577e6-1a45-46e4-ab01-a159f16b74ba', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('be3577e6-1a45-46e4-ab01-a159f16b74ba', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('be3577e6-1a45-46e4-ab01-a159f16b74ba', foundational, arms_right_conditioned_on_civic_readiness).
narrative_ontology:cs_axiom_status(arms_right_conditioned_on_civic_readiness, holdable).
narrative_ontology:cs_axiom_grounding('be3577e6-1a45-46e4-ab01-a159f16b74ba', arms_right_conditioned_on_civic_readiness, conventional).
narrative_ontology:cs_axiom('be3577e6-1a45-46e4-ab01-a159f16b74ba', foundational, self_governance_requires_armed_citizen_capacity).
narrative_ontology:cs_axiom_status(self_governance_requires_armed_citizen_capacity, holdable).
narrative_ontology:cs_axiom_grounding('be3577e6-1a45-46e4-ab01-a159f16b74ba', self_governance_requires_armed_citizen_capacity, instrumental).
narrative_ontology:cs_reference_frame('be3577e6-1a45-46e4-ab01-a159f16b74ba', founding_era_militia_readiness_norm).
narrative_ontology:cs_drift_state('be3577e6-1a45-46e4-ab01-a159f16b74ba', post_professional_military_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be3577e6-1a45-46e4-ab01-a159f16b74ba', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_self_governance_project).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unorganized_gun_owners_outside_militia_norm).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, populations_historically_excluded_from_militia_membership).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_virtue_requires_armed_capacity).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, self_governance_depends_on_citizen_readiness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the arms right as a dual right-and-duty: eligible to keep and bear arms because they are expected to be ready to serve the republic's defense. They benefit from the right's protection but also bear training, qualification, and readiness burdens that a purely individual-liberty reading would not impose. Exit from the civic obligation is not really available if they wish to retain the right's full civic legitimacy.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, civic_militia_eligible_citizens, payer).

% Set and enforce training, registration, and qualification standards tied to civic-militia readiness. Their regulatory authority is constrained by the civic-participation norm — they cannot ban arms outright without undermining the republican structure the right protects — but they are not bound by a libertarian non-infringement standard either, and may condition the right on demonstrated civic fitness.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_and_federal_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Own or wish to own arms for self-defense, sport, or purely personal reasons unconnected to militia service or civic readiness. Under this reading their claim to protection is weaker and contingent on being read into the civic-militia framework; if courts or legislatures do not recognize them as part of the militia-eligible citizenry, their arms use is subject to greater regulatory reach than an individual-right framework would allow.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unorganized_gun_owners_outside_militia_norm, payer,
    powerless, biographical, trapped, national).

% Groups historically barred from organized militia service (by race, gender, or other exclusion) find that a right conditioned on civic-militia membership inherited the exclusions of that membership category. Their claim to the same right is derivative of whether they are retroactively or prospectively counted as full civic participants — a status they did not control at the right's founding.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, populations_historically_excluded_from_militia_membership, payer,
    powerless, civilizational, trapped, national).

% The abstract structural interest in a citizenry capable of collective self-defense and resistant to both external conquest and internal tyranny. Not an actor itself, but the reading's stated justification — the coordination good the arrangement claims to secure. Listed for completeness, not as an agent whose interests can be separately weighed.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_self_governance_project, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(second_amendment_arms_right__civic_republican_reading, republican_self_governance_project).

% Adjudicates which reading of the amendment controls in a given case, drawing on militia-clause text, founding-era practice, and precedent. Its rulings determine whether the civic-republican reading, the individual-right reading, or the collective-right reading governs a specific regulation's constitutionality.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, judiciary_interpreting_the_amendment, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__civic_republican_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__civic_republican_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a citizenry capable of collective armed defense of the republic and resistant to both foreign conquest and domestic tyranny, by tying the arms right to expectations of civic readiness rather than treating it as either pure personal liberty or pure state prerogative.
% TRANSFER_FUNCTION: Moves regulatory latitude toward legislatures conditioning the right on training, qualification, and civic-participation markers, and moves protective force away from arms possession disconnected from that civic framework — shifting legitimacy (and litigation outcomes) from purely personal gun ownership claims toward militia-readiness-framed claims.
% ABSENT_VOICES: Individuals who want the right recognized purely as personal liberty (no civic-duty condition) and groups historically excluded from militia membership who never consented to having their claim to the right made contingent on a membership category they were shut out of — neither group is party to the founding-era civic-militia consensus this reading reconstructs.
% DISAPPEARANCE_RATIONALE: If the civic-republican reading vanished, some jurisdictions would default to a pure individual-right framework (loosening civic-fitness conditions) while others might default to a collective/state-authority framework (tightening regulation to organized militia contexts only) — which direction the world rearranges toward is itself contested among the reading's own proponents and critics, not resolved by removing the reading.
% FOUNDING_PROBLEM: At the founding, standing armies were distrusted as tools of tyranny, and a armed, trained citizenry organized into militias was seen as the republic's primary check against both external invasion and domestic despotism — the right was meant to secure THAT capacity, not personal self-defense as such.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era and some constitutional scholars outside the gun-rights advocacy space attest the militia-readiness problem was real at ratification but has been largely superseded by professional standing militaries and organized National Guard structures; civic-republican reading proponents (often originalist scholars and militia-movement adjacent commentators) attest the underlying civic-virtue problem remains live regardless of military professionalization. No consensus corroboration exists outside these interested camps.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the civic-republican reading imposes real costs — training and qualification requirements, contingent recognition for those outside the militia-readiness frame — but these costs are bounded by the coordination function (a genuinely defensible collective-defense rationale), unlike a pure extraction story. Suppression (0.32) reflects that alternatives (pure individual-right framing, pure collective/state framing) are not eliminated, only contested in courts and legislatures; accessibility_collapse (0.4) is moderate because the reading has not foreclosed the sibling readings, which remain live and litigated. Resistance (0.55) is substantial because gun-rights advocates favoring the individual-right reading actively contest the civic-conditioning implications in litigation and legislative advocacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens sit near symmetric-to-beneficiary: they receive the right's protection but carry the readiness duty, which the engine should read as a coordination cost rather than pure extraction. Legislatures are the agenda-setting seat with constrained exit — bound by the civic-participation norm even as they administer it. Unorganized gun owners and historically excluded populations are structural targets: the civic-conditioning apparatus is applied against their claims without their having agreed to the underlying militia-membership premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a militia-based check against standing armies and tyranny) is largely superseded by professional militaries and National Guard structures, yet the civic-republican reading persists as a live interpretive framework because it still performs real present-day work: bounding legislative reach on both a pure-libertarian expansion and a pure-collective-authority contraction. This is not simple mandatrophy — the interpretive function (constraining regulatory extremes in either direction) is not dead even though the literal 18th-century militia-readiness problem is largely dead. Classifying this as tangled_rope rather than piton preserves that distinction: real coordination function (bounding regulatory extremes) persists even as the founding factual predicate erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indeterminacy_founding_intent,
    'Does the founding-era historical record support a reading where the arms right is conditioned on civic-militia readiness (this reading), a reading where it is an unconditional pre-political individual liberty, or a reading where it belongs only to organized state militia structures?',
    'Comparative originalist historical analysis across founding-era militia statutes, ratification debates, and contemporaneous commentary; no single resolution is likely given genuine textual and historical ambiguity in the militia clause''s relationship to the operative clause.',
    'If the civic-conditioning premise is historically unsupported, this reading''s coordination-function claim weakens and its extraction on non-militia-framed gun owners becomes harder to justify as coordination rather than pure imposition — pushing the classification toward snare for that subset of stakeholders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_indeterminacy_founding_intent, conceptual, 'Kernel-level indeterminacy: which reading the founding-era record actually supports.').

omega_variable(
    sibling_reading_structural_delta,
    'What would change structurally if the individual_right_reading or collective_right_reading controlled instead of this reading?',
    'Track actual case outcomes and regulatory latitude under each reading where courts have applied them (e.g., post-Heller individual-right jurisprudence vs. pre-Heller collective-right circuit precedent) as a natural experiment.',
    'Under individual_right_reading, civic-conditioning regulations (training/qualification mandates) would face strict scrutiny and likely fail, sharply lowering ε for currently-burdened gun owners but eliminating this reading''s coordination claim entirely. Under collective_right_reading, personal arms claims outside organized militia service would receive minimal protection, raising ε dramatically for the same population. This reading sits structurally between those two poles by design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'What the sibling readings would change if they controlled instead.').

omega_variable(
    historical_exclusion_correction,
    'Should the civic-militia membership category be read as including populations historically excluded from actual militia service (by race, gender, etc.), retroactively extending the right''s protection to them on equal terms?',
    'Judicial and legislative determination of whether the civic-republican reading is applied with or without correction for historical exclusion; track whether courts extend militia-readiness-framed protection to previously excluded groups.',
    'If uncorrected, the reading perpetuates founding-era exclusions as a structural feature rather than a historical accident, sharpening the victim classification for historically excluded populations. If corrected, the coordination/extraction balance shifts favorably for that stakeholder group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_exclusion_correction, preference, 'Whether historical militia-membership exclusions are inherited or corrected under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement(seco_tr_t1860, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1860, 0.14).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1900, 0.16).
narrative_ontology:measurement(seco_tr_t1940, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1940, 0.18).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2008, 0.21).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1791, 0.2).
narrative_ontology:measurement(seco_be_t1860, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1860, 0.28).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(seco_be_t1940, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1940, 0.33).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2008, 0.37).
narrative_ontology:measurement(seco_be_t2026, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2026, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_arms_right__civic_republican_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the second_amendment_arms_right kernel. The individual_right_reading treats the arms right as an unconditional pre-political liberty (near-zero civic conditioning, lower ε for personal ownership, higher ε for regulatory intervention). The collective_right_reading treats the right as belonging to organized state militia structures only (near-zero protection for personal ownership outside militia service, higher ε for unorganized owners). This civic_republican_reading sits structurally between them: moderate ε reflecting genuine but bounded coordination function (civic-readiness conditioning) layered with real extraction (burdens on non-militia-framed owners and inherited historical exclusions). Each story carries its own stable ε and stakeholder set per the ε-invariance principle; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
