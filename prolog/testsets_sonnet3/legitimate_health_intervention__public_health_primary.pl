% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public-Health-Primary Reading: Vaccination/Intervention Mandates as Population Legitimacy Standard
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the
 *   legitimate_health_intervention kernel: legitimacy is grounded exclusively
 *   in measurable population-level morbidity/mortality reduction, and
 *   individual refusal is reframed as an externality imposed on others rather
 *   than a private medical decision. Under this reading, the unvaccinated
 *   (and religious exemption seekers, treated identically once the
 *   externality framing is accepted) enter the victim set as disease vectors
 *   whose refusal justifies coercive consequence — termination, access
 *   restriction — while the immunocompromised, who cannot generate their own
 *   protection, are structural beneficiaries of aggregate compliance. This is
 *   a distinct constraint from the sibling readings: the
 *   bodily_autonomy_primary reading would place ε near zero for any
 *   individual mandate absent consent (the coercion itself is the violation,
 *   regardless of population benefit), and the proportionality_reading would
 *   scale ε to disease severity and weight autonomy explicitly. Here ε is
 *   authored high because the reading's own legitimacy standard treats
 *   population statistics as fully sufficient justification for enforcement
 *   mechanisms that this reading does not require to be proportionate to
 *   individual risk.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter/beneficiary (institutional/analytical) — sets and justifies mandate via population statistics
 *   - immunocompromised_populations: beneficiary (powerless/trapped) — gains herd protection, cannot individually secure it
 *   - employers_seeking_liability_shield: beneficiary (powerful/mobile) — enforces at low cost to itself, shifts cost to workers
 *   - unvaccinated_workers: payer (powerless/constrained) — bears termination/exclusion, recategorized as vector
 *   - vaccine_injured_minority: payer (powerless/trapped) — bears uncompensated harm treated as acceptable statistical remainder
 *   - civil_liberties_organizations: excluded (organized/analytical) — objection structurally answered in advance by externality framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.71).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public-Health-Primary Reading: Vaccination/Intervention Mandates as Population Legitimacy Standard").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, 'b6b4be2e-6acb-420f-8766-b720c8664b53').
narrative_ontology:cs_kernel_codification('b6b4be2e-6acb-420f-8766-b720c8664b53', distributed).
narrative_ontology:cs_authority_grounding('b6b4be2e-6acb-420f-8766-b720c8664b53', expertise).
narrative_ontology:cs_interpretation_layer_present('b6b4be2e-6acb-420f-8766-b720c8664b53').
narrative_ontology:cs_reading_relation('b6b4be2e-6acb-420f-8766-b720c8664b53', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b6b4be2e-6acb-420f-8766-b720c8664b53', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('b6b4be2e-6acb-420f-8766-b720c8664b53', foundational, population_aggregate_is_sufficient_legitimacy_unit).
narrative_ontology:cs_axiom_status(population_aggregate_is_sufficient_legitimacy_unit, holdable).
narrative_ontology:cs_axiom_grounding('b6b4be2e-6acb-420f-8766-b720c8664b53', population_aggregate_is_sufficient_legitimacy_unit, instrumental).
narrative_ontology:cs_axiom('b6b4be2e-6acb-420f-8766-b720c8664b53', foundational, individual_refusal_constitutes_externality_not_private_choice).
narrative_ontology:cs_axiom_status(individual_refusal_constitutes_externality_not_private_choice, holdable).
narrative_ontology:cs_axiom_grounding('b6b4be2e-6acb-420f-8766-b720c8664b53', individual_refusal_constitutes_externality_not_private_choice, empirically_contingent).
narrative_ontology:cs_reference_frame('b6b4be2e-6acb-420f-8766-b720c8664b53', acute_outbreak_emergency_justification).
narrative_ontology:cs_drift_state('b6b4be2e-6acb-420f-8766-b720c8664b53', routine_administrative_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6b4be2e-6acb-420f-8766-b720c8664b53', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, employers_seeking_liability_shield).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, religious_exemption_seekers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_injured_minority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandate policy by translating population-level morbidity/mortality statistics into binding intervention requirements. Justifies coercive enforcement (employment conditions, access restrictions) as the necessary correction for the externality that individual refusal imposes on others. Gains institutional authority and legitimacy from measurable reduction in aggregate harm; bears none of the individual enforcement cost directly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, public_health_agencies, beneficiary).

% Cannot generate adequate immune response themselves and depend entirely on high population-level compliance (herd protection) for reduced exposure risk. Have no individual lever to compel others' compliance; their protection is a pure function of the mandate's population coverage. Cannot exit the risk exposure by any individual action of their own.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Adopt and enforce the mandate as a condition of employment, gaining regulatory liability protection and alignment with public-health-agency guidance. Terminates non-compliant employees rather than absorbing legal or reputational exposure; their compliance costs are largely administrative and shifted onto workers.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employers_seeking_liability_shield, beneficiary,
    powerful, biographical, mobile, national).

% Face employment termination, access restriction to public spaces, or exclusion from services for declining the intervention. Under this reading they are recategorized as disease vectors whose refusal is an externality imposition on others, which justifies coercive consequence rather than treating refusal as a private medical choice. Exit means loss of employment, mobility, or social participation — not a meaningful alternative.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_workers, payer,
    powerless, biographical, constrained, national).

% Request exemption on doctrinal grounds; under the public-health-primary reading, sincerely held objection does not override the externality claim, so exemptions are narrowly granted or denied outright. Bear the same enforcement consequences as other refusers unless a narrow carve-out is politically tolerated.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, religious_exemption_seekers, payer,
    powerless, biographical, constrained, national).

% Suffer rare adverse events from the mandated intervention itself. Under a population-morbidity-reduction legitimacy standard, their individual harm is treated as an acceptable statistical remainder against aggregate benefit; compensation mechanisms are typically capped, slow, or evidentially difficult to access. Cannot exit their own injury and have limited institutional recourse because the legitimacy standard is defined at the population level, not the individual level.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_injured_minority, payer,
    powerless, biographical, trapped, national).

% Would argue that population-level statistical justification cannot license individualized coercive consequence without proportionality review, but this reading's legitimacy standard treats their bodily-autonomy framing as already answered by the externality argument — their objections are heard in litigation but do not enter the mandate's own justificatory structure.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, civil_liberties_organizations, excluded,
    organized, generational, analytical, national).

% Adjudicate challenges to mandate enforcement, weighing the population-health justification against individual rights claims. Their rulings can validate, narrow, or dismantle the enforcement mechanism, but under this reading their proper role is to defer to public health agencies' measurable population outcomes.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, courts_reviewing_mandates, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level disease suppression: individual compliance decisions aggregate into herd-level protection that no individual can produce alone, and the mandate solves the collective-action problem of under-vaccination by making refusal costly.
% TRANSFER_FUNCTION: Moves health risk and enforcement cost from the immunocompromised and the general population onto individual refusers, who bear employment loss, access restriction, and (for the vaccine-injured minority) uncompensated physical harm, in exchange for population-level morbidity/mortality reduction credited to public health agencies and cost-shielded employers.
% ABSENT_VOICES: Civil liberties organizations and bodily-autonomy advocates would object that population statistics cannot justify individualized coercion without case-by-case proportionality, but this reading's own legitimacy standard treats that objection as already settled by the externality framing — they are heard in courts, not in the mandate's justificatory logic.
% DISAPPEARANCE_RATIONALE: If the public-health-primary legitimacy standard vanished, mandates would lose their justificatory basis, enforcement mechanisms (termination, access restriction) would need new grounding or would lapse, immunocompromised populations would lose herd-protection coverage, and courts would revert to case-by-case proportionality or autonomy-based review — the entire enforcement apparatus depends on this standard being accepted as authoritative.
% FOUNDING_PROBLEM: Infectious disease outbreaks impose costs on people who did not choose their exposure risk; voluntary compliance alone was empirically insufficient to reach the population coverage needed for herd protection, so authorities sought a legitimacy standard that could justify compulsory measures.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and epidemiologists attest the founding problem remains live wherever coverage gaps persist, citing outbreak data. Civil liberties organizations, some constitutional scholars, and the vaccine-injured minority's advocacy groups attest that the standard has drifted from addressing genuine outbreak emergencies to routine administrative enforcement against low-marginal-risk individual cases, and that this drift is not corroborated by anyone outside the enforcing institutions themselves.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) and suppression (0.78) are both high because this reading's own justificatory logic removes proportionality as a constraint on enforcement severity: once refusal is classified as externality imposition, the enforcement mechanism (termination, access denial) is legitimated without a case-by-case severity check. Theater ratio stays low (0.22) because the enforcement mechanisms are functionally real, not performative — they materially change employment and access status. Accessibility collapse (0.62) and resistance (0.72) reflect that legal and social alternatives to compliance (exemption routes, litigation) exist and are actively used, but progressively narrow as the reading hardens into administrative practice.
 *
 * PERSPECTIVAL GAP:
 *   From the public-health-agency seat, this is coordination: a genuine collective-action solution to a real externality problem, validated by measurable outcome reduction. From the unvaccinated-worker or vaccine-injured seat, the same structure computes as extraction backed by coercion: legitimacy is claimed from statistics that never individually examine their case. The engine's per-seat computation should diverge sharply here — this divergence is exactly what the tangled_rope classification is meant to hold, not resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and employers sit near the beneficiary end: they set or administer the standard and bear little of the enforcement cost themselves. Immunocompromised populations are also beneficiaries but via a different mechanism — passive recipients of herd protection with no lever over compliance, hence trapped exit options despite beneficiary role. Unvaccinated workers, religious exemption seekers, and the vaccine-injured minority sit near the target end: constrained or trapped exit, bearing the constraint's costs directly. This is the structural delta this reading demands relative to its siblings — the unvaccinated are recoded as vectors (victims of nothing, source of externality) rather than as autonomy-holders, which is precisely what distinguishes this reading from bodily_autonomy_primary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (outbreak-driving under-vaccination) is contested as still-live: agencies point to ongoing coverage gaps, while critics point to routine administrative enforcement outliving acute emergency conditions. Classifying this as tangled_rope rather than snare preserves that there IS a genuine coordination function (herd protection for the immunocompromised is real and depends on aggregate compliance) alongside the asymmetric extraction (uncompensated harm concentrated on refusers and the vaccine-injured) — collapsing it to pure snare would erase the immunocompromised beneficiary's real stake; collapsing it to pure rope would erase the coercion and uncompensated injury.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    population_standard_vs_individual_proportionality,
    'Should legitimacy for coercive health intervention be assessed purely at the population level (aggregate morbidity/mortality reduction), or does legitimacy require individualized proportionality between the specific intervention''s severity and the specific individual''s marginal risk contribution?',
    'This is the central committer disagreement between this reading and its siblings (proportionality_reading, bodily_autonomy_primary) — it is not resolvable by additional epidemiological data because it is a normative question about which unit of analysis (population vs. individual) legitimacy attaches to. Constitutional courts adjudicating mandate challenges are the live site where this gets provisionally settled.',
    'If individualized proportionality is required, this reading''s core premise (externality imposition alone justifies coercion regardless of individual risk profile) collapses, and enforcement mechanisms authored here as legitimate become illegitimate under the proportionality_reading''s standard — this is exactly the sibling relationship documented in cs_structure.reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(population_standard_vs_individual_proportionality, conceptual, 'The kernel-level disagreement this reading resolves one way and its siblings resolve differently.').

omega_variable(
    externality_framing_scope,
    'Does classifying individual refusal as ''externality imposition'' extend legitimately to all transmissible-disease interventions, or only to a narrow class of high-transmissibility, high-severity diseases — and who decides which class a given disease falls into?',
    'Track whether the externality classification, once established for one intervention, is invoked for progressively lower-severity conditions (scope creep) versus remaining bounded to the original high-severity justification.',
    'Unbounded scope creep would mean this reading''s legitimacy standard has no natural stopping point short of universal medical mandate authority — bounded application would keep the reading closer to the proportionality_reading in practice even while differing from it in stated principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_framing_scope, empirical, 'Whether the externality classification has a natural limiting principle or expands indefinitely.').

omega_variable(
    vaccine_injury_compensation_adequacy,
    'Is the compensation available to the vaccine-injured minority adequate relative to the harm, such that their inclusion in the victim set is offset, or is compensation structurally inadequate (capped, slow, evidentially inaccessible)?',
    'Empirical audit of compensation program payout rates, claim denial rates, and time-to-resolution compared to documented injury severity and cost of care.',
    'If compensation is adequate, the extraction on this specific victim group is substantially mitigated; if structurally inadequate, it strengthens the case that this reading''s population-level legitimacy standard systematically underweights identifiable individual harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_injury_compensation_adequacy, empirical, 'Whether the compensation mechanism offsets the harm this reading concedes exists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(legi_tr_t4, legitimate_health_intervention__public_health_primary, theater_ratio, 4, 0.1).
narrative_ontology:measurement(legi_tr_t8, legitimate_health_intervention__public_health_primary, theater_ratio, 8, 0.13).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.17).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__public_health_primary, theater_ratio, 16, 0.19).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__public_health_primary, theater_ratio, 20, 0.21).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__public_health_primary, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__public_health_primary, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__public_health_primary, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__public_health_primary, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__public_health_primary, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__public_health_primary, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__public_health_primary, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__public_health_primary, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__public_health_primary, 0.1).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the legitimate_health_intervention kernel, decomposed per the ε-invariance principle: the natural-language concept of 'mandate legitimacy' resolves differently depending on whether legitimacy attaches to population statistics, individual consent, or severity-weighted proportionality. Each reading is authored as its own constraint with its own ε, beneficiary/victim structure, and classification; this file authors the highest-ε reading because its own legitimacy standard removes individualized proportionality as an enforcement constraint. See cs_structure.reading_relations for the typed structural relationship to each sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__public_health_primary, powerless, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
