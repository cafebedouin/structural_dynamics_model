% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Public-Health-Primary Reading: State Vaccination Mandate Authority to Protect the Vulnerable
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint instantiates the public-health-primary reading of the
 *   mandate_legitimacy_scope kernel: the claim that state authority to compel
 *   vaccination is legitimate specifically because, and to the extent that,
 *   it is necessary to protect populations who cannot protect themselves.
 *   Under this reading the immunocompromised and medically vulnerable are the
 *   moral center of the arrangement — their exposure risk absent a mandate is
 *   treated as the harm the state authority exists to prevent, and the
 *   unvaccinated (whether by choice, religious objection, or medical caution
 *   not rising to exemption) are recast as bearing an affirmative duty to
 *   protect third parties rather than as autonomous agents making a purely
 *   self-regarding choice. This reading generates a structurally different
 *   victim set than the bodily_autonomy_primary reading (which would name the
 *   vaccinated-under-compulsion as the primary victims and would not treat
 *   the immunocompromised's absent-mandate exposure as extraction at all) and
 *   a different legitimacy test than the proportionality_reading (which
 *   conditions legitimacy on disease severity and
 *   less-restrictive-alternative analysis rather than treating protective
 *   necessity as a freestanding warrant). ε here reflects the standing
 *   mandate arrangement as this reading's own lights assess it: real
 *   coordination function (herd protection), but substantial extraction from
 *   objectors and workers whose exit options are foreclosed by the
 *   enforcement machinery.
 *
 * KEY AGENTS:
 *   - immunocompromised_and_medically_vulnerable: primary beneficiary (powerless/trapped) — cannot protect themselves and depend on others' compliance
 *   - public_health_departments: agenda_setter (institutional/arbitrage) — designs and enforces mandate policy
 *   - hospital_systems: secondary beneficiary (organized/constrained) — bears outbreak surge cost, benefits from compliance
 *   - unvaccinated_individuals_by_choice: primary payer (moderate/constrained) — bears the compelled-duty framing
 *   - religious_and_philosophical_objectors: payer (powerless/trapped) — loses accommodation as exemptions narrow
 *   - workers_facing_mandate_conditioned_employment: payer (powerless/trapped) — livelihood conditioned on compliance
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates the police-power/liberty balance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.62).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.58).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Public-Health-Primary Reading: State Vaccination Mandate Authority to Protect the Vulnerable").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '50cdbd2b-d9bf-4f02-815e-7488f02dd664').
narrative_ontology:cs_kernel_codification('50cdbd2b-d9bf-4f02-815e-7488f02dd664', distributed).
narrative_ontology:cs_authority_grounding('50cdbd2b-d9bf-4f02-815e-7488f02dd664', distributed).
narrative_ontology:cs_reading_relation('50cdbd2b-d9bf-4f02-815e-7488f02dd664', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('50cdbd2b-d9bf-4f02-815e-7488f02dd664', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('50cdbd2b-d9bf-4f02-815e-7488f02dd664', foundational, protective_necessity_grounds_compulsion).
narrative_ontology:cs_axiom_status(protective_necessity_grounds_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('50cdbd2b-d9bf-4f02-815e-7488f02dd664', protective_necessity_grounds_compulsion, deontological).
narrative_ontology:cs_axiom('50cdbd2b-d9bf-4f02-815e-7488f02dd664', foundational, vulnerable_third_party_harm_overrides_individual_consent).
narrative_ontology:cs_axiom_status(vulnerable_third_party_harm_overrides_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('50cdbd2b-d9bf-4f02-815e-7488f02dd664', vulnerable_third_party_harm_overrides_individual_consent, deontological).
narrative_ontology:cs_reference_frame('50cdbd2b-d9bf-4f02-815e-7488f02dd664', historic_police_power_precedent).
narrative_ontology:cs_drift_state('50cdbd2b-d9bf-4f02-815e-7488f02dd664', contemporary_exemption_contraction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50cdbd2b-d9bf-4f02-815e-7488f02dd664', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_and_medically_vulnerable).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_departments).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, hospital_systems).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals_by_choice).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, workers_facing_mandate_conditioned_employment).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, communitarian_duty_to_protect_thesis).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__public_health_primary, herd_immunity_threshold_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot be vaccinated themselves or mount full immune response, and depend entirely on the vaccination status of everyone around them for protection from serious illness or death. In this reading, the absence of a mandate directly enters their harm profile: every unvaccinated contact is a live risk they cannot exit from by any individual action. They have no direct enforcement power and must rely on the state to compel others.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, immunocompromised_and_medically_vulnerable, beneficiary,
    powerless, biographical, trapped, national).

% Design and administer mandate policy, set exemption criteria, and enforce compliance through school entry rules, employment conditions, and licensing requirements. They frame the mandate as the necessary instrument for protecting those who cannot protect themselves, and bear no personal cost from the compulsion they administer.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_departments, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear the surge capacity burden of vaccine-preventable outbreaks among the vulnerable, and benefit directly from reduced admissions when mandate compliance is high. They lobby for mandates as a load-bearing mechanism for their own capacity planning.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, hospital_systems, beneficiary,
    organized, biographical, constrained, regional).

% Object to vaccination on medical caution, distrust, or personal risk-benefit calculation but are not exempt on religious or philosophical grounds. Under this reading they are recast as bearers of an affirmative duty to protect the vulnerable, and face exclusion from school, workplace, and public accommodations as the enforcement mechanism. Their individual risk calculus is treated as subordinate to the collective protective function.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals_by_choice, payer,
    moderate, biographical, constrained, national).

% Hold sincere objections rooted in belief systems predating the mandate. Where states narrow or eliminate non-medical exemptions to protect herd immunity thresholds, they lose the legal accommodation entirely and face the same exclusions as those objecting for no stated reason — the reading treats the protective function as overriding the accommodation.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, trapped, national).

% Employed in healthcare, education, or other settings serving vulnerable populations; face termination or unpaid leave for non-compliance. Their exit option is loss of livelihood, which for many is not a real exit at all given labor market constraints and sector-specific skills.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, workers_facing_mandate_conditioned_employment, payer,
    powerless, biographical, trapped, national).

% Adjudicate challenges to mandate scope, weighing state police power against individual liberty claims. Their rulings under this reading tend to defer to public health necessity findings, following a jurisprudential line from historic precedent that established state authority to compel vaccination during epidemic threat.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity so that people who cannot be protected by their own vaccination (infants too young, immunocompromised, allergic) are protected indirectly through the vaccination of everyone able to be vaccinated — a genuine collective-action problem where individual non-participation imposes risk on non-consenting third parties.
% TRANSFER_FUNCTION: Moves bodily autonomy and individual risk-tolerance from the unvaccinated and objecting population to the state's enforcement apparatus, and moves epidemiological risk away from the immunocompromised and medically vulnerable onto those compelled to vaccinate against their preference.
% ABSENT_VOICES: The immunocompromised and medically fragile rarely appear directly in mandate litigation or public debate — they are invoked as the justifying class but are not typically party to the legal challenges, which are brought by objectors. Their actual harm rates under varying mandate regimes are underrepresented relative to the volume of objector testimony.
% DISAPPEARANCE_RATIONALE: If public-health-primary mandate authority were struck down entirely, exemption rates would rise, herd immunity thresholds would erode in specific communities, and outbreak risk would concentrate onto the medically vulnerable who cannot vaccinate — hospital systems would face intermittent surge capacity strain from vaccine-preventable disease clusters. This is the reading's own account of what changes; siblings weight this differently.
% FOUNDING_PROBLEM: Contagious disease outbreaks in un-immunized populations historically caused disproportionate death and disability among those least able to protect themselves — infants, immunocompromised patients, and the elderly — creating a collective-action failure where individually rational non-vaccination produces population-level harm concentrated on the most vulnerable.
% FOUNDING_PROBLEM_CORROBORATION: Public health departments and hospital systems (the beneficiary/agenda-setter seats) attest the problem remains live, citing periodic outbreak data. Independent epidemiological surveillance data and peer-reviewed outbreak case studies from academic institutions with no mandate-enforcement role corroborate that unvaccinated clusters do produce measurably elevated transmission to medically vulnerable contacts — this corroboration exists outside the beneficiary set, though its interpretation (how much mandate authority the finding justifies) is exactly what the sibling readings contest.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects a genuine coordination function (herd protection of the medically vulnerable) layered with real cost imposed on objectors and conditioned workers whose exit options are structurally foreclosed once employment, schooling, or licensure is conditioned on compliance. Suppression (0.58) captures the active enforcement machinery — exemption narrowing, employment conditions, exclusion from public accommodations — required to hold the arrangement together; it is not scaled by scope or power, only extractiveness is. Theater ratio is low (0.2) because the protective function is substantively real under this reading, not primarily performative — this is a defensible characteristic of the public-health-primary reading specifically, and would differ under a reading skeptical of mandate necessity.
 *
 * PERSPECTIVAL GAP:
 *   From the immunocompromised beneficiary seat, the mandate looks like the state finally taking seriously a harm that individual choice architecture cannot solve. From the unvaccinated payer seat, the same structure looks like compelled medical intervention justified by an externality claim they may dispute on the facts. The engine computes these divergent seat classifications from the structural power/exit data; this reading does not adjudicate which seat is 'right' — it fixes ONE coherent account (protective necessity as the legitimating premise) and lets the seats diverge from there.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (immunocompromised, hospital systems, public health departments) sit near the subsidized end because the arrangement transfers protective benefit or administrative capacity to them without imposing the compelled-choice cost. Payers (unvaccinated by choice, religious objectors, conditioned workers) sit near the full-target end because the enforcement mechanism operates directly and coercively on their bodily and employment choices, and their exit options are trapped or merely constrained — a worker facing job loss or an objector facing exclusion does not have a meaningfully mobile alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (outbreak risk concentrated on those who cannot self-protect) remains genuinely live in this reading's own account, corroborated by outside epidemiological surveillance — this blocks a straightforward mandatrophy verdict of pure zombie persistence. But the widening enforcement mechanism (narrowing exemptions, employment conditioning) that has grown over the measured interval raises the live question of whether the mandate machinery has scaled beyond what the founding problem's current severity would justify — that scope question is exactly what the proportionality_reading exists to test, and this reading deliberately does not resolve it, per the ε-invariance discipline for kernel readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protective_necessity_vs_proportionality_test,
    'Does state authority to compel vaccination derive from a freestanding duty to protect the vulnerable (this reading), or is it conditional on a severity/efficacy/least-restrictive-means balancing test (proportionality_reading) that could find the same mandate illegitimate at a different point in disease severity or vaccine safety data?',
    'This is not empirically resolvable — it is a jurisprudential and moral framework choice about what grounds state police power. Courts applying different constitutional traditions resolve it differently; no data settles which premise is correct.',
    'Under this reading''s premise, mandate legitimacy is largely insensitive to marginal changes in disease severity once a protective necessity threshold is crossed. Under the proportionality reading, legitimacy would fluctuate continuously with severity and efficacy data, producing a very different persistence profile for the same mandate over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_necessity_vs_proportionality_test, conceptual, 'Structural fork between freestanding-protective-duty and conditional-proportionality framings of mandate legitimacy.').

omega_variable(
    immunocompromised_harm_absent_mandate_magnitude,
    'How large is the actual increase in serious harm to immunocompromised and medically vulnerable populations specifically attributable to mandate absence, versus other factors (natural immunity waning, treatment advances, exposure behavior)?',
    'Longitudinal epidemiological studies comparing outbreak severity and vulnerable-population harm rates across jurisdictions with materially different mandate regimes and exemption rates, controlling for confounders.',
    'If the harm delta attributable to mandate absence specifically is small, this reading''s ε for ''mandate absence'' status would be overstated relative to what the coordination function actually buys; if large, it corroborates the protective-necessity premise more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_harm_absent_mandate_magnitude, empirical, 'Whether the reading''s central causal claim (mandate absence -> vulnerable harm) is quantitatively as strong as the reading assumes.').

omega_variable(
    exemption_narrowing_scope_creep,
    'Has the enforcement apparatus built to serve the protective-necessity function expanded (narrowing religious/philosophical exemptions, widening employment conditioning) beyond what the founding problem''s current severity would justify, constituting scope creep independent of the reading''s core legitimacy premise?',
    'Track exemption-denial rates and employment-conditioning breadth over time against contemporaneous disease incidence and severity data; a widening enforcement scope during a period of stable or declining incidence would indicate creep.',
    'If enforcement has outpaced the underlying severity data, the tangled_rope classification''s active-enforcement element may itself be drifting toward snare-like extraction on objectors even while the core coordination premise (protecting the vulnerable) remains valid — this would not resolve the kernel contest but would flag internal drift within this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exemption_narrowing_scope_creep, empirical, 'Whether enforcement scope has grown independent of the founding problem''s actual current severity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__public_health_primary, theater_ratio, 4, 0.14).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__public_health_primary, theater_ratio, 8, 0.16).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__public_health_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__public_health_primary, theater_ratio, 16, 0.19).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 4, 0.47).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 12, 0.57).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 12, 0.54).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the mandate_legitimacy_scope kernel, each authored as a separate ε-invariant story per the ε-invariance principle. public_health_primary treats protective necessity as a freestanding legitimating warrant (high ε on mandate absence, victim set centered on the medically vulnerable). bodily_autonomy_primary treats non-consensual medical intervention as categorically impermissible (victim set centered on the compelled individual). proportionality_reading makes legitimacy conditional on a severity/efficacy/alternatives balancing test rather than granting or denying authority categorically. The three are linked via affects_constraints rather than merged, because their ε values, victim sets, and legitimacy tests are not the same measurement taken three ways — they are three distinct constraints sharing contested kernel text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
