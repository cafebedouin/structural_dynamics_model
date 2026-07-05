% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: State Authority to Compel Medical Intervention for Collective Harm Prevention
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the
 *   coercion-legitimacy kernel: the state's authority to compel medical
 *   intervention is legitimate whenever collective harm-prevention outweighs
 *   individual bodily autonomy, without further scaling by disease severity
 *   (that scaling belongs to the sibling proportionality_reading) and without
 *   a categorical autonomy veto (that belongs to the sibling
 *   bodily_autonomy_primary). Under this reading, unvaccinated individuals
 *   and objectors are structurally recast as coerced subjects whose autonomy
 *   claim has been outweighed, while immunocompromised populations who cannot
 *   self-protect exit any victim framing entirely and become the doctrine's
 *   primary beneficiaries. The enforcement apparatus required to sustain
 *   compulsion — exclusion regimes, exemption gatekeeping, court orders — is
 *   itself a major source of the constraint's high extractiveness and
 *   suppression, independent of whether any given mandate is medically
 *   proportionate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.68).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.79).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "State Authority to Compel Medical Intervention for Collective Harm Prevention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '861eb8c4-e929-4207-aabc-ab3f54f1ddd8').
narrative_ontology:cs_kernel_codification('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', distributed).
narrative_ontology:cs_authority_grounding('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', distributed).
narrative_ontology:cs_reading_relation('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', foundational, collective_harm_can_always_outweigh_individual_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_can_always_outweigh_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', collective_harm_can_always_outweigh_individual_autonomy, instrumental).
narrative_ontology:cs_axiom('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', secondary, third_party_harm_generates_categorical_not_scaled_authority).
narrative_ontology:cs_axiom_status(third_party_harm_generates_categorical_not_scaled_authority, holdable).
narrative_ontology:cs_axiom_grounding('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', third_party_harm_generates_categorical_not_scaled_authority, conventional).
narrative_ontology:cs_reference_frame('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', police_power_harm_prevention_doctrine).
narrative_ontology:cs_drift_state('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', post_pandemic_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('861eb8c4-e929-4207-aabc-ab3f54f1ddd8', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, vaccinated_majority).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, medically_borderline_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, pharmaceutical_manufacturers).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, collective_harm_prevention_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, state_police_power_over_bodily_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and enforce mandate policy — school entry requirements, employment conditions, quarantine orders, sometimes direct compulsion via court order. Justify the arrangement through epidemiological modeling of herd-immunity thresholds and outbreak prevention. Do not bear the physical or bodily cost of the intervention themselves; they administer it.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Cannot receive certain vaccines themselves or mount adequate immune response, and depend entirely on herd immunity maintained by others' compliance for protection from disease exposure. Have no independent means of achieving the protection the mandate produces; their safety is a direct downstream product of others being compelled.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Comply voluntarily or under mild social pressure, bear minimal marginal cost, and receive both personal immunity and community-level protection. Can exit the mandate's coercive apparatus more easily than objectors because compliance itself is the exit — they experience the constraint as background policy, not compulsion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccinated_majority, beneficiary,
    organized, biographical, mobile, national).

% Face school exclusion, employment termination, denial of services, fines, or in extreme outbreak scenarios court-ordered intervention. Bear the direct bodily cost the constraint compels. Exit requires either compliance (surrendering the autonomy claim) or accepting exclusion from major social and economic institutions — there is no low-cost third option.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    powerless, immediate, trapped, local).

% Hold sincere non-medical objections that many jurisdictions have narrowed or eliminated as valid exemption grounds during outbreak response. Their objection is treated as a cost to be overridden rather than a claim to be weighed, and legislative venues for restoring exemptions are slow and frequently unsuccessful against public health lobbying.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, religious_and_philosophical_objectors, excluded).

% Have contested or poorly-documented medical contraindications that fall short of formal exemption criteria set by health authorities. Caught between a bureaucratic exemption process calibrated for administrability and their individual physiological uncertainty, with the burden of proof placed entirely on them.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, medically_borderline_refusers, payer,
    powerless, immediate, trapped, local).

% Supply the compelled intervention under liability-shielded procurement contracts and guaranteed demand created by mandate policy. Benefit from the enforcement apparatus without bearing any of its coercive costs; can exit any single jurisdiction's political controversy while retaining global market access.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, pharmaceutical_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Adjudicate the boundary between police power and individual rights case by case, drawing on precedent from historical compulsory vaccination rulings. Can narrow or expand the mandate's legitimate scope through subsequent rulings, shifting the constraint's future enforcement without being subject to it themselves.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieving and maintaining herd immunity thresholds requires near-universal participation; below the threshold, transmission chains persist and both compliant and non-compliant individuals remain at risk, including those medically unable to be vaccinated at all.
% TRANSFER_FUNCTION: Moves bodily autonomy and decision-making control from the individual to the state, and moves epidemiological risk from immunocompromised and other vulnerable populations onto individuals who would otherwise decline the intervention.
% ABSENT_VOICES: Unvaccinated individuals with sincere but non-medical objections are frequently excluded from the policymaking process itself — exemption criteria are set by health authorities and legislatures without binding input from objector communities, whose testimony is treated as noise in outbreak-response deliberation rather than a competing claim to be weighed.
% DISAPPEARANCE_RATIONALE: If state compulsion authority vanished, herd immunity thresholds would depend entirely on voluntary uptake, immunocompromised populations would lose their primary protective mechanism, school and workplace exclusion policies would need to be renegotiated on a purely private basis, and outbreak response would shift from mandate enforcement to persuasion and incentive design — a substantial institutional rearrangement, not a null change.
% FOUNDING_PROBLEM: Contagious disease outbreaks (smallpox historically, then measles, pertussis, and others) impose costs on third parties who cannot protect themselves through their own choices alone, creating a collective-action problem that voluntary compliance alone does not reliably solve.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and immunocompromised advocacy groups attest the founding problem remains fully live, citing measurable outbreak resurgence tied to declining vaccination rates. Independent legal scholars and civil liberties organizations outside the beneficiary set corroborate that the underlying transmission dynamics are real, but contest whether the CURRENT scope and severity of compulsion (workplace mandates, broad exemption narrowing) is proportionate to the founding problem or has expanded past it — the disease-severity-scaling proportionality reading disputes this reading's breadth directly.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.79) are both high because this reading authorizes compulsion as a categorical response to a harm-prevention calculus rather than a severity-scaled one, which produces broader enforcement scope than the proportionality reading would. Accessibility collapse (0.62) is moderate-high: once a jurisdiction adopts this doctrine, alternative arrangements (voluntary incentive systems, narrower severity-gated mandates) become politically and legally harder to argue for, though not impossible — courts retain some ability to narrow scope. Resistance (0.71) is high because objector communities and civil liberties organizations actively contest the doctrine's breadth in courts and legislatures. Theater ratio is comparatively low (0.28) because the enforcement machinery is doing real epidemiological work, not merely performing it — though its rising trajectory suggests exemption-gatekeeping bureaucracy is growing faster than the underlying outbreak risk in some periods.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health authorities), the arrangement is coordination: solving a genuine collective-action problem that voluntary action alone does not reliably solve. From the payer seats (unvaccinated individuals, objectors), the identical structure is compulsion imposed through school exclusion, employment loss, and legal order — a categorical override of their claim with no severity threshold to appeal to under this reading. The engine's per-seat computation should reflect this asymmetry: agenda-setter and protected-beneficiary seats likely compute closer to rope/tangled_rope, while payer seats compute closer to snare, given trapped exit options and high suppression exposure.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and pharmaceutical manufacturers sit near the beneficiary end: they administer or supply the compelled intervention without bearing its bodily cost. Immunocompromised populations and the vaccinated majority are also beneficiaries but through different mechanisms — the former through protection they cannot secure independently, the latter through low-cost compliance plus community protection. Unvaccinated individuals, religious/philosophical objectors, and medically borderline refusers sit at the target end: trapped or constrained exit options, direct bodily and social costs, and — under this reading specifically — no doctrinal recognition that their autonomy claim could outweigh the collective-harm calculus regardless of how minor the disease threat is.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (outbreak transmission harming third parties who cannot self-protect) remains partly live — corroborated by outbreak resurgence data cited independently of the beneficiary set. But the founding_problem_status is marked contested because the doctrine's current SCOPE (workplace mandates, narrowed exemptions) is argued by outside legal scholars to have expanded past what the founding problem strictly requires, which is precisely the structural delta the proportionality_reading sibling exists to capture. This reading does not resolve that scope question; it structurally forecloses only the possibility that autonomy alone, absent any harm-prevention showing, could ever outweigh compulsion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_scaled_legitimacy,
    'Should the legitimacy of compelled intervention scale with disease severity and transmission dynamics (as the proportionality_reading holds), or is a categorical collective-harm-outweighs-autonomy test sufficient regardless of the specific disease''s severity?',
    'Comparative analysis of case law and public health outcomes across jurisdictions that adopt scaled versus categorical mandate authority — does scaled authority produce meaningfully different outbreak outcomes or exemption abuse rates?',
    'If scaled legitimacy produces materially better-calibrated outcomes without loss of herd-immunity protection, this reading''s categorical framing would be shown to authorize broader-than-necessary coercion, strengthening the sibling proportionality_reading''s claim over this one''s domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_scaled_legitimacy, conceptual, 'Whether coercion legitimacy is a categorical threshold or a severity-scaled gradient — the core axis distinguishing this reading from proportionality_reading.').

omega_variable(
    autonomy_veto_existence,
    'Does bodily autonomy retain any categorical veto power against state-compelled intervention, or can it always in principle be outweighed by sufficient collective harm?',
    'Doctrinal and philosophical analysis of whether any documented case exists where a court or framework recognized an absolute bodily-integrity veto regardless of collective harm magnitude, versus cases treating autonomy as always weighable.',
    'If a genuine categorical veto exists and is doctrinally stable, this reading (public_health_primary) and the bodily_autonomy_primary reading cannot both be held within a single coherent legal framework — this is the forecloses relationship. If autonomy is always weighable in practice, the readings coexist as competing but non-foreclosing framings applied by different courts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_veto_existence, conceptual, 'Whether this reading and bodily_autonomy_primary can coexist in one legal framework or structurally exclude each other.').

omega_variable(
    enforcement_apparatus_proportionate_to_threat,
    'Is the observed rise in suppression_requirement (0.48 to 0.79 over the interval) proportionate to actual outbreak risk, or does it reflect enforcement-apparatus entrenchment (bureaucratic exemption-gatekeeping growth) independent of epidemiological need?',
    'Compare suppression/enforcement intensity trajectories against independently-measured outbreak incidence and herd-immunity threshold proximity over the same interval; divergence between rising enforcement and stable or declining actual risk would indicate apparatus entrenchment.',
    'If enforcement has outpaced epidemiological need, the constraint''s classification drifts from a defensible tangled_rope toward snare for the payer seats specifically, even though the coordination function (herd immunity) remains genuinely valuable for beneficiary seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_apparatus_proportionate_to_threat, empirical, 'Whether rising enforcement intensity tracks actual disease risk or reflects institutional entrenchment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 8, 0.15).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 16, 0.19).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement(coer_tr_t32, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 32, 0.25).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(coer_be_t32, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(coer_su_t32, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the coercion_legitimacy_boundary kernel. public_health_primary authorizes compulsion whenever collective harm-prevention outweighs autonomy, with no severity-scaling requirement. bodily_autonomy_primary holds intervention without consent categorically impermissible regardless of collective benefit. proportionality_reading scales legitimacy with disease severity and transmission dynamics. Each reading produces a different victim/beneficiary structure and a different epsilon: this reading's epsilon (0.68) reflects the enforcement apparatus required to sustain a categorical (non-scaled) compulsion doctrine, which is structurally broader than what proportionality_reading would authorize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
