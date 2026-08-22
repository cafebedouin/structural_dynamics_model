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
 *   human_readable: Public-Health-Primary Reading: Mandate Legitimacy via Population Morbidity Reduction
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the public-health-primary reading of the
 *   legitimate_health_intervention kernel: legitimacy for medical mandates is
 *   grounded exclusively in measurable population-level morbidity/mortality
 *   reduction, and individual refusal is reframed structurally as an
 *   externality imposition on others rather than as an exercise of bodily
 *   autonomy. Under this reading, unvaccinated or non-compliant individuals
 *   enter the victim set not as rights-holders being coerced but as vectors
 *   whose refusal transfers risk onto the immunocompromised and risk-averse
 *   third parties. This is a different constraint from the
 *   bodily_autonomy_primary reading (where the same refusal is the exercise
 *   of a protected right and the mandate is the extraction) and from the
 *   proportionality_reading (where severity-weighting moderates both
 *   directions). Each reading is authored as its own file with its own
 *   epsilon; this file's epsilon (0.68) reflects the enforcement apparatus
 *   (termination, exclusion, licensure conditions) that this reading's own
 *   legitimacy standard requires and licenses, not an average across
 *   readings.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter/beneficiary (institutional/analytical) — defines and administers the metric that grounds mandate legitimacy
 *   - immunocompromised_population: beneficiary (powerless/trapped) — depends wholly on population compliance for protection
 *   - vaccine_refusing_workers: payer (moderate/constrained) — reclassified from autonomous decision-maker to externality-imposer
 *   - religious_exemption_seekers: payer (powerless/trapped) — sincerity subordinated to population math
 *   - employers_seeking_liability_shield: beneficiary (organized/arbitrage) — rides the legitimacy claim for liability reduction
 *   - bioethics_review_boards: observer (institutional/analytical) — sees the full structural contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.72).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public-Health-Primary Reading: Mandate Legitimacy via Population Morbidity Reduction").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '9f0c8d4c-4dad-4363-9841-7c3991842fc3').
narrative_ontology:cs_kernel_codification('9f0c8d4c-4dad-4363-9841-7c3991842fc3', distributed).
narrative_ontology:cs_authority_grounding('9f0c8d4c-4dad-4363-9841-7c3991842fc3', expertise).
narrative_ontology:cs_interpretation_layer_present('9f0c8d4c-4dad-4363-9841-7c3991842fc3').
narrative_ontology:cs_reading_relation('9f0c8d4c-4dad-4363-9841-7c3991842fc3', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('9f0c8d4c-4dad-4363-9841-7c3991842fc3', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('9f0c8d4c-4dad-4363-9841-7c3991842fc3', foundational, population_metric_sufficiency).
narrative_ontology:cs_axiom_status(population_metric_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('9f0c8d4c-4dad-4363-9841-7c3991842fc3', population_metric_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('9f0c8d4c-4dad-4363-9841-7c3991842fc3', foundational, refusal_as_externality).
narrative_ontology:cs_axiom_status(refusal_as_externality, holdable).
narrative_ontology:cs_axiom_grounding('9f0c8d4c-4dad-4363-9841-7c3991842fc3', refusal_as_externality, instrumental).
narrative_ontology:cs_reference_frame('9f0c8d4c-4dad-4363-9841-7c3991842fc3', epidemiological_threshold_legitimacy).
narrative_ontology:cs_drift_state('9f0c8d4c-4dad-4363-9841-7c3991842fc3', post_pandemic_mandate_litigation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9f0c8d4c-4dad-4363-9841-7c3991842fc3', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, employers_seeking_liability_shield).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, vaccine_refusing_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, religious_exemption_seekers).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, informal_caregivers_of_unvaccinated_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandate policy, defines the morbidity/mortality metrics that justify intervention, and administers enforcement through licensing bodies, employers, and access gatekeepers. Its legitimacy claim and its institutional survival are bound together: the agency that measures the externality is also the agency that benefits from the mandate's continuation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__public_health_primary, public_health_agencies, beneficiary).

% Cannot generate adequate immune response themselves and depend entirely on population-level uptake (herd protection) for reduced exposure risk. Under this reading, every unvaccinated person in their radius is a live increment to their mortality risk. They have no independent exit — their protection is wholly a function of others' compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, local).

% Refuse a specific intervention on medical, religious, or philosophical grounds. Under this reading they are reclassified as disease vectors imposing an externality rather than as autonomous decision-makers, and face termination, exclusion from public accommodations, or loss of licensure. Their only exits are conversion or exit from the labor market/geography entirely — neither is cost-free.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, vaccine_refusing_workers, payer,
    moderate, biographical, constrained, national).

% Seek exemption on sincerely held religious grounds; under the public-health-primary reading, sincerity is subordinated to population math — an exemption request is read as an externality claim regardless of its constitutional grounding elsewhere. Denial rates for exemptions rise as mandate enforcement intensifies.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, religious_exemption_seekers, payer,
    powerless, biographical, trapped, regional).

% Parents or guardians of minors or dependents who refuse vaccination on the dependents' behalf are treated as the responsible externality-imposing party, facing custody disputes, school exclusion of dependents, and in some jurisdictions civil liability framing — despite bearing no direct medical decision themselves in the more contested cases.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, informal_caregivers_of_unvaccinated_dependents, payer,
    powerless, biographical, trapped, local).

% Adopt mandates in part to reduce workplace transmission liability and insurance exposure, riding the public-health-primary legitimacy claim to justify termination of non-compliant employees without needing to independently litigate the underlying medical judgment.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employers_seeking_liability_shield, beneficiary,
    organized, biographical, arbitrage, national).

% Review mandate policy for proportionality and consent standards, holding testimony from both the population-health case and the autonomy case. Their findings feed litigation and legislative revision but do not themselves enforce or exempt anyone.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, bioethics_review_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces population-level disease transmission by raising vaccination or compliance thresholds toward levels needed for herd protection, which genuinely lowers morbidity and mortality for those who cannot independently protect themselves.
% TRANSFER_FUNCTION: Moves employment security, bodily decision authority, and access to public and workplace spaces away from individual refusers and toward the immunocompromised and risk-averse institutional actors who benefit from reduced ambient transmission risk.
% ABSENT_VOICES: Individuals who refuse for reasons the population framing treats as noise rather than signal — sincere religious objectors, people with atypical adverse-reaction histories not captured by aggregate risk models — are present in enforcement proceedings but not in the metric-setting process that defines what counts as a legitimate exemption.
% DISAPPEARANCE_RATIONALE: If the public-health-primary legitimacy standard were withdrawn, mandate enforcement mechanisms (termination, access exclusion, licensure conditions) would lose their justifying framework; employers and agencies would need to rebuild policy on a different legitimacy basis (consent-based or proportionality-based), and immunocompromised populations would lose the strongest available legal lever for demanding population-level compliance.
% FOUNDING_PROBLEM: Communicable disease outbreaks historically produced excess mortality unpreventable by individual action alone; population-level thresholds (herd protection) were identified as the only mechanism capable of protecting those who cannot be protected individually.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and immunocompromised-advocacy organizations outside the enforcing agencies attest the population-threshold problem remains live for specific pathogens with high R0 and severe outcomes for vulnerable subgroups. Independent bioethics scholarship and civil-liberties litigation record attest that, for many currently-mandated interventions, population risk has fallen enough that the same legitimacy standard is now used to justify continuing enforcement past the point the original threat calculus would support — corroboration for 'dead' or at least materially reduced status comes from outside the enforcing agencies themselves, which have institutional incentive to declare the problem perpetually live.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) tracks the enforcement mechanisms this reading licenses: employment termination, access restriction, and licensure conditioning against a specific population subset defined by refusal status. Suppression (0.72) is set high because the reading's own logic treats accommodation of refusal as tolerating an externality, which structurally justifies escalating coercive response rather than negotiation. Theater ratio is comparatively low (0.22) because the underlying coordination function — herd-protection threshold maintenance — is empirically real for high-R0, high-severity pathogens; the enforcement is not primarily performative even where it is severe. Accessibility collapse (0.6) reflects that once the population-metric legitimacy standard is accepted, individual exemption pathways narrow sharply — sincerity and medical nuance are compressed into a small number of centrally defined exemption categories. Resistance (0.75) is high because this reading directly overrides a competing rights claim (bodily autonomy) that a substantial minority holds as foundational, producing sustained legal and civil resistance rather than passive acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised individuals and public health agencies sit near the beneficiary end of directionality: the former collect protection they cannot generate themselves, the latter collect institutional legitimacy and continued mandate authority. Vaccine-refusing workers, religious exemption seekers, and caregivers of unvaccinated dependents sit near the target end: the same structure that produces herd protection for one group produces termination, exclusion, or custody risk for another, and their exit options are structurally constrained (leaving employment, litigating, or complying under duress) rather than freely available. Employers sit closer to the beneficiary end via arbitrage exit (they can adjust mandate policy by jurisdiction) while still deriving direct liability-shield benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — population thresholds needed to protect those who cannot protect themselves — remains genuinely live for specific high-severity, high-transmissibility pathogens, which is why this reading is authored as tangled_rope rather than snare: there IS a real coordination function alongside the extraction. But the reading's own legitimacy standard has no internal mechanism for recognizing when a given intervention's population risk has fallen enough that continued enforcement under the SAME legitimacy standard becomes rent-seeking on institutional authority rather than active disease control — this is exactly the founding_problem_status: contested finding, and it is why corroboration is sought outside the enforcing agencies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_reframing_validity,
    'Does reclassifying individual medical refusal as ''externality imposition'' rather than as protected bodily autonomy correctly describe the causal structure, or does it smuggle a contested normative premise (that population aggregate risk overrides individual consent) into what is presented as a purely empirical measurement?',
    'Compare this reading''s legitimacy standard against the bodily_autonomy_primary and proportionality_reading constraint files: where all three would classify the same intervention differently despite identical epidemiological facts, the divergence is evidence the ''externality'' framing carries normative weight beyond the measurable morbidity data.',
    'If the externality framing is doing normative work beyond its empirical content, the high extractiveness measured here reflects contested value commitments smuggled into a claimed-objective standard, not a pure function of disease severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_reframing_validity, conceptual, 'Whether population-externality framing is empirically grounded or normatively loaded.').

omega_variable(
    threshold_persistence_after_risk_decline,
    'For interventions where population-level severity has substantially declined since the mandate''s founding, does the public-health-primary legitimacy standard have any internal mechanism for recognizing mandate obsolescence, or does the standard''s own logic (any refusal is an externality) make it structurally resistant to sunset regardless of updated risk data?',
    'Track whether mandate policy under this legitimacy standard has ever been voluntarily withdrawn by the administering agency in response to declining population risk, absent external litigation or legislative override.',
    'If withdrawal never occurs absent external pressure, this reading functions as a one-way ratchet independent of the underlying epidemiology, supporting the tangled_rope classification''s extraction component as structurally locked-in rather than contingent on threat level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_persistence_after_risk_decline, empirical, 'Whether the standard can self-correct toward sunset as risk declines.').

omega_variable(
    immunocompromised_beneficiary_capture,
    'Is the immunocompromised population''s benefit from this reading genuine and load-bearing, or is their vulnerability being invoked by institutional actors (agencies, employers) whose primary motive is liability and authority preservation rather than protection of that specific group?',
    'Examine whether mandate policy changes track immunocompromised advocacy input directly, or whether policy is set independently by agencies/employers and immunocompromised benefit is cited post hoc as justification.',
    'If benefit to the immunocompromised is substantially rhetorical rather than policy-driving, the true beneficiary structure is narrower (agencies and employers primarily) and the tangled_rope''s coordination-function claim weakens toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_beneficiary_capture, empirical, 'Whether immunocompromised benefit is genuine or instrumentalized justification.').


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
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.16).
narrative_ontology:measurement(legi_tr_t16, legitimate_health_intervention__public_health_primary, theater_ratio, 16, 0.19).
narrative_ontology:measurement(legi_tr_t20, legitimate_health_intervention__public_health_primary, theater_ratio, 20, 0.21).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t4, legitimate_health_intervention__public_health_primary, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(legi_be_t8, legitimate_health_intervention__public_health_primary, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(legi_be_t16, legitimate_health_intervention__public_health_primary, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__public_health_primary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t4, legitimate_health_intervention__public_health_primary, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(legi_su_t8, legitimate_health_intervention__public_health_primary, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(legi_su_t16, legitimate_health_intervention__public_health_primary, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__public_health_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the legitimate_health_intervention kernel, decomposed per the ε-invariance principle because the same natural-language claim ('vaccine mandates are legitimate') yields structurally distinct constraints depending on which legitimacy standard is applied. public_health_primary treats population morbidity/mortality reduction as sufficient legitimacy grounds, with refusal reframed as externality-imposition (this file). bodily_autonomy_primary treats informed consent as the legitimacy floor regardless of population benefit, with the mandate itself as the extraction. proportionality_reading weights both population harm and individual autonomy by disease-specific severity. The three files share no averaged epsilon; each is authored from within its own reading's premises. This file forecloses bodily_autonomy_primary within a single framework (a legitimacy standard cannot simultaneously hold that population metrics are sufficient AND that consent is an independent floor that population benefit cannot override) while influencing proportionality_reading (population-metric legitimacy creates downstream pressure on what counts as a proportionate threshold, without foreclosing severity-weighting as an independent axis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
