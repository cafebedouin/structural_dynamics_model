% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__public_health_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__public_health_primary
 *   human_readable: Public-Health-Primary Reading of Vaccine Mandate Authority
 *   domain: public_health/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the public-health-primary reading of the vaccine
 *   mandate balance kernel: once voluntary compliance provably fails to reach
 *   herd-immunity thresholds and vulnerable populations face lethal exposure,
 *   collective protection is held to supersede individual consent. This is
 *   one of three structurally distinct constraints sharing a kernel —
 *   bodily_autonomy_primary holds consent inviolable regardless of collective
 *   benefit, and proportionality_reading conditions mandate legitimacy on
 *   strict severity/transmission/safety thresholds with robust exemptions.
 *   Each reading has its own beneficiary/victim structure and its own
 *   epsilon; they are not the same constraint measured differently. Under
 *   this reading specifically, the immunocompromised and unvaccinatable
 *   infants enter the victim set of an ABSENT mandate (they are exposed and
 *   die without one), while the coerced unvaccinated are explicitly NOT
 *   counted as victims of the mandate itself — their consent claim is treated
 *   as subordinated by necessity, not violated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, 0.62).
domain_priors:suppression_score(vaccine_mandate_balance__public_health_primary, 0.71).
domain_priors:theater_ratio(vaccine_mandate_balance__public_health_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_balance__public_health_primary, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__public_health_primary, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__public_health_primary, "Public-Health-Primary Reading of Vaccine Mandate Authority").
narrative_ontology:topic_domain(vaccine_mandate_balance__public_health_primary, "public_health/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__public_health_primary, '21abe38e-56d1-4ebb-9b89-61c58ffefde0').
narrative_ontology:cs_kernel_codification('21abe38e-56d1-4ebb-9b89-61c58ffefde0', distributed).
narrative_ontology:cs_authority_grounding('21abe38e-56d1-4ebb-9b89-61c58ffefde0', expertise).
narrative_ontology:cs_interpretation_layer_present('21abe38e-56d1-4ebb-9b89-61c58ffefde0').
narrative_ontology:cs_reading_relation('21abe38e-56d1-4ebb-9b89-61c58ffefde0', vaccine_mandate_balance__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('21abe38e-56d1-4ebb-9b89-61c58ffefde0', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('21abe38e-56d1-4ebb-9b89-61c58ffefde0', foundational, collective_lethal_exposure_overrides_individual_consent).
narrative_ontology:cs_axiom_status(collective_lethal_exposure_overrides_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('21abe38e-56d1-4ebb-9b89-61c58ffefde0', collective_lethal_exposure_overrides_individual_consent, instrumental).
narrative_ontology:cs_axiom('21abe38e-56d1-4ebb-9b89-61c58ffefde0', secondary, population_threshold_failure_is_sufficient_trigger).
narrative_ontology:cs_axiom_status(population_threshold_failure_is_sufficient_trigger, holdable).
narrative_ontology:cs_axiom_grounding('21abe38e-56d1-4ebb-9b89-61c58ffefde0', population_threshold_failure_is_sufficient_trigger, empirically_contingent).
narrative_ontology:cs_reference_frame('21abe38e-56d1-4ebb-9b89-61c58ffefde0', collective_immunity_necessity_doctrine).
narrative_ontology:cs_drift_state('21abe38e-56d1-4ebb-9b89-61c58ffefde0', post_pandemic_polarization_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('21abe38e-56d1-4ebb-9b89-61c58ffefde0', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, unvaccinatable_infants).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__public_health_primary, public_health_agencies).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, conscientious_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, religious_exemption_seekers).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, vaccine_injury_susceptible_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_balance__public_health_primary, employers_and_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets vaccination thresholds, determines when voluntary uptake has failed to reach herd-immunity levels, and issues mandates with enforcement mechanisms (employment conditions, school exclusion, fines, in some jurisdictions civil detention for outbreak control). Justifies the mandate as the only remaining lever once persuasion and access campaigns are exhausted. Gains institutional legitimacy and reduced outbreak caseload from successful mandates; bears reputational cost if mandates are seen as coercive overreach.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, public_health_agencies, beneficiary).

% Cannot be vaccinated themselves or mount adequate immune response even if vaccinated; their survival depends entirely on the vaccination rate of the surrounding population reaching herd-immunity thresholds. Have no personal exit from exposure risk other than the compliance of others; a single unvaccinated contact can be lethal. Under this reading, when mandates are absent or under-enforced, they are the direct victims of the resulting under-immunization.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Too young to receive certain vaccines and rely entirely on cocooning — the vaccination of caregivers, siblings, and the surrounding community — for protection against lethal childhood diseases. Have no voice in the mandate debate and no independent capacity to reduce their own exposure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, unvaccinatable_infants, beneficiary,
    powerless, biographical, trapped, local).

% Object to vaccination on grounds of bodily autonomy, medical distrust, or personal risk assessment. Under this reading their objection is subordinated once voluntary compliance provably fails to reach the immunity threshold; they face employment loss, school exclusion, or fines for noncompliance. Exit is nominally available (refuse and accept consequences) but the consequences are structured to be severe enough that compliance is the only livable option for most. This reading holds they are not victims in the morally relevant sense — necessity overrides the consent claim — though they bear the material cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, conscientious_objectors, payer,
    moderate, biographical, constrained, national).

% Seek exemption on doctrinal grounds. Under public-health-primary reasoning, religious exemptions are narrowed or eliminated once they measurably suppress herd immunity below threshold in a community, because the exemption itself becomes a vector for the lethal exposure the mandate exists to prevent. They experience this as their claimed right being overridden by an epidemiological calculation they had no part in setting.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, religious_exemption_seekers, payer,
    moderate, biographical, constrained, regional).

% A small population with genuine elevated risk of adverse vaccine reaction who do not qualify for a formal medical exemption because their risk factor is not on the recognized exemption list, or the recognition process is slow and bureaucratic. Under strict public-health-primary enforcement they may be compelled or structurally coerced despite elevated personal risk, because the threshold calculation is population-level, not individual.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, vaccine_injury_susceptible_individuals, payer,
    powerless, biographical, trapped, local).

% Administer the mandate at the point of contact — checking vaccination status, excluding noncompliant employees or students. Absorb administrative and legal cost of enforcement and occasional litigation from objectors, while gaining reduced outbreak liability and continuity of operation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, employers_and_schools, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__public_health_primary, employers_and_schools, payer).

% Adjudicate challenges to mandate authority, weighing compelling state interest against individual rights claims. Their rulings determine how far this reading's premise (necessity overrides consent) can be operationalized before it collides with constitutional limits on bodily autonomy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__public_health_primary, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level immunity so that individuals who cannot be protected by their own vaccination (the immunocompromised, unvaccinatable infants, vaccine non-responders) are protected by the vaccination of those around them — a genuine collective-action problem where individual non-participation imposes lethal externalities on non-consenting third parties.
% TRANSFER_FUNCTION: Moves bodily autonomy and individual risk-assessment authority from the objecting individual to the public health agency, and moves exposure risk away from the immunocompromised and infant populations onto the compelled or penalized objector.
% ABSENT_VOICES: Vaccine-injury-susceptible individuals without a recognized exemption category are structurally absent from the exemption-design process; their elevated individual risk is invisible to a population-threshold calculation. Conscientious objectors are present in litigation but largely absent from the epidemiological threshold-setting process itself, which is technocratic and not subject to their consent.
% DISAPPEARANCE_RATIONALE: If mandate authority disappeared overnight, public health agencies and immunocompromised advocates argue outbreak rates would rise and vulnerable populations would face measurably higher mortality within one to two disease cycles — the world rearranges toward higher lethal exposure. Objecting populations argue voluntary uptake, information campaigns, and access improvements would substitute adequately and the world would look largely unchanged in aggregate outcomes; this is exactly the empirical dispute the kernel contest turns on.
% FOUNDING_PROBLEM: Voluntary vaccination campaigns, even when well-resourced and widely accessible, plateau below the threshold needed for herd immunity in some communities, leaving individuals who cannot be vaccinated (immunocompromised, infants, allergic) exposed to preventable, sometimes fatal, disease transmission from their unvaccinated neighbors.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and epidemiologists (via peer-reviewed outbreak modeling and post-mandate seroprevalence studies) attest the coverage gap and resulting exposure risk are real and measurable in specific documented outbreaks (e.g., measles clusters in under-vaccinated communities). Civil liberties organizations and some independent bioethicists, outside the public-health-agency beneficiary set, corroborate that the gap exists but dispute that mandate-with-coercion is the only or best remedy, arguing the founding problem could be substantially addressed by non-coercive means — this is the live axis of the kernel contest, not a settled genealogy.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__public_health_primary, contested).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__public_health_primary, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial but not extreme: the mandate genuinely solves a coordination problem (herd immunity) that voluntary action failed to solve, but the enforcement machinery (exclusion, fines, employment conditions) extracts real cost from objectors whose individual risk calculus this reading does not fully credit. Suppression (0.71) is high because the reading's own logic requires narrowing exemptions once they measurably erode the threshold — the suppression is not incidental, it is structurally necessary to the reading's operation. Theater ratio is low (0.22) because the enforcement mechanisms are functionally tied to the immunity threshold, not merely performative, though some compliance-checking infrastructure persists past acute outbreak periods. Accessibility collapse (0.58) reflects that objectors retain some formal exit (accept material consequences, relocate to non-mandate jurisdictions) but the practical alternative space narrows sharply once threshold-based mandates take effect. Resistance (0.69) is high and expected: this is precisely the reading most contested by objecting populations and civil liberties advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the public health agency and immunocompromised-population seats, this reading is the only reading that takes their exposure risk seriously — bodily_autonomy_primary and even proportionality_reading are read from here as under-protecting them. From the conscientious objector and religious exemption seats, this reading is the one that most fully overrides their standing, since it explicitly treats their consent claim as subordinated by necessity rather than merely balanced against it (as proportionality_reading would). The engine computes these as different seat-level classifications from the same structural data; the reading itself does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Immunocompromised populations and unvaccinatable infants are structural beneficiaries with d near the full-beneficiary end — they cannot protect themselves and depend entirely on population-level compliance; under this reading, mandate absence is what victimizes them, so the mandate's operation subsidizes their survival. Conscientious objectors and religious exemption seekers sit near the full-target end: they bear the enforcement cost directly, with constrained exit. Vaccine-injury-susceptible individuals without recognized exemptions are a harder case — powerless, trapped, bearing individual elevated risk that the population-threshold calculation does not register; this reading's own logic subordinates their claim in the same way it subordinates the general objector's claim, which is the reading's most exposed structural weakness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (voluntary uptake plateauing below herd-immunity threshold, exposing the vulnerable) remains live in specific documented contexts (measles, pertussis outbreaks in under-vaccinated communities), which argues against pure mandatrophy. But the reading's classification as tangled_rope rather than rope is deliberate: the coordination function (protecting the unvaccinatable) is real and genuine, and the extraction (coercive enforcement against objectors whose individual risk is not credited) is also real and asymmetric. Calling this a pure rope would erase the objectors' material cost; calling it a pure snare would erase the immunocompromised population's genuine and lethal dependency. Tangled rope holds both facts simultaneously, which is the point of the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_failure_determination_authority,
    'Who determines, and by what evidentiary standard, that voluntary compliance has ''failed'' to achieve herd immunity such that this reading''s mandate-override condition is triggered?',
    'Examine whether the threshold-failure determination is made by an independent epidemiological body with published, falsifiable criteria, versus by the same agency that then administers and enforces the resulting mandate (a self-referential authority structure).',
    'If the same institution both declares failure and administers the remedy, the reading''s necessity claim is less independently verifiable and the classification would shift toward higher extraction (self-dealing threshold-setting); if an independent body makes the determination, the coordination claim is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_failure_determination_authority, conceptual, 'Whether the threshold-failure trigger is independently verified or self-administered.').

omega_variable(
    vaccine_injury_susceptible_exemption_gap,
    'Does this reading''s population-level threshold calculation adequately account for individuals with genuine elevated personal risk who fall outside recognized exemption categories?',
    'Audit the exemption-recognition process for false-negative rate: how many individuals with documented elevated adverse-reaction risk are denied exemption due to bureaucratic category limits rather than absence of actual risk.',
    'A high false-negative rate would mean this reading''s victim set is under-declared — vaccine_injury_susceptible_individuals may be a larger and more clearly victimized group than currently modeled, pushing the classification toward snare for that subgroup specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccine_injury_susceptible_exemption_gap, empirical, 'Whether the exemption process adequately protects genuinely high-risk individuals.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the public-health-primary reading, versus the proportionality reading, the one actually operative in a given jurisdiction''s mandate design, or does the jurisdiction claim public-health-primary language while functionally implementing proportionality-reading exemption structures?',
    'Compare the jurisdiction''s stated legal doctrine against its actual exemption breadth and enforcement severity; a jurisdiction with robust exemptions and proportionate enforcement is functionally closer to proportionality_reading regardless of its stated justification.',
    'Misidentifying the operative reading would misattribute this story''s high-suppression, low-exemption profile to a jurisdiction that is actually operating under the more conditional proportionality reading, inflating apparent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether stated doctrine and operative structure match across jurisdictions claiming this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__public_health_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__public_health_primary, theater_ratio, 4, 0.12).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__public_health_primary, theater_ratio, 8, 0.15).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__public_health_primary, theater_ratio, 12, 0.18).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__public_health_primary, theater_ratio, 16, 0.2).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__public_health_primary, theater_ratio, 20, 0.21).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__public_health_primary, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__public_health_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__public_health_primary, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__public_health_primary, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__public_health_primary, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__public_health_primary, base_extractiveness, 16, 0.59).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__public_health_primary, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__public_health_primary, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__public_health_primary, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__public_health_primary, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__public_health_primary, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__public_health_primary, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__public_health_primary, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__public_health_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__public_health_primary, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__bodily_autonomy_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__public_health_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vaccine_mandate_balance kernel, decomposed per the epsilon-invariance principle rather than represented as a single constraint with an observable-dependent classification. bodily_autonomy_primary treats consent as inviolable and would classify near snare from the objector's seat with no coordination credit. proportionality_reading occupies a middle position, gating mandate legitimacy on strict severity/transmission/safety thresholds with preserved robust exemptions, which structurally narrows both its beneficiary set (fewer immunocompromised protected under a higher threshold) and its extraction (fewer objectors coerced). This reading (public_health_primary) has the widest beneficiary set (immunocompromised and infants fully counted as victims of mandate absence) and the highest extraction (broadest subordination of consent, narrowest exemptions) of the three. The three stories are linked bidirectionally in intent; each should list the other two in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
