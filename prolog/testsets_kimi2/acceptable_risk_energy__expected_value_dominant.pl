% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__expected_value_dominant, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: acceptable_risk_energy__expected_value_dominant
 *   human_readable: Expected-Value Dominant Energy Risk Framework
 *   domain: risk_assessment/energy_policy
 *
 * SUMMARY:
 *   The expected-value-dominant reading of acceptable energy risk
 *   institutionalizes mortality-per-TWh metrics as the decisive policy
 *   criterion. Under this constraint, fossil fuel pathways are heavily
 *   suppressed because their mortality footprint enters the calculation with
 *   full weight, while nuclear accident risks are discounted by estimated
 *   probabilities, making nuclear appear superior. The constraint coordinates
 *   investment toward allegedly lower-harm energy portfolios but
 *   asymmetrically externalizes catastrophic tail risk onto nuclear-proximate
 *   populations and suppresses fossil-dependent livelihoods. As a kernel
 *   reading, it is contested by catastrophic-tail-dominant readings (which
 *   reject probability discounting for nuclear) and option-value readings
 *   (which reject single-metric optimization under deep uncertainty).
 *
 * KEY AGENTS:
 *   - Energy policy agencies (agenda_setter, institutional/analytical): Set the EV methodological rules for permitting and subsidy.
 *   - Nuclear energy sector (beneficiary, powerful/constrained): Gains regulatory and social license because tail risks are discounted by probability.
 *   - Renewable energy sector (beneficiary, organized/mobile): Gains market share from fossil suppression.
 *   - Fossil fuel labor (payer, powerless/constrained): Bears job loss and devaluation from pathway suppression.
 *   - Fossil dependent communities (payer, moderate/constrained): Bear regional economic collapse.
 *   - Nuclear proximate populations (payer, powerless/trapped): Bear catastrophic tail risk treated as negligible.
 *   - Precautionary advocates (excluded, moderate/constrained): Excluded from policy tables because their framework is ruled unquantifiable.
 *   - Independent risk analysts (observer, analytical): Evaluate whether mortality metrics capture all relevant harms.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, 0.62).
domain_priors:suppression_score(acceptable_risk_energy__expected_value_dominant, 0.76).
domain_priors:theater_ratio(acceptable_risk_energy__expected_value_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, extractiveness, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__expected_value_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__expected_value_dominant, "Expected-Value Dominant Energy Risk Framework").
narrative_ontology:topic_domain(acceptable_risk_energy__expected_value_dominant, "risk_assessment/energy_policy").

domain_priors:requires_active_enforcement(acceptable_risk_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__expected_value_dominant, 'fa435046-91fa-4ddd-a3cc-af2793dbba31').
narrative_ontology:cs_kernel_codification('fa435046-91fa-4ddd-a3cc-af2793dbba31', formalized).
narrative_ontology:cs_authority_grounding('fa435046-91fa-4ddd-a3cc-af2793dbba31', expertise).
narrative_ontology:cs_interpretation_layer_present('fa435046-91fa-4ddd-a3cc-af2793dbba31').
narrative_ontology:cs_reading_relation('fa435046-91fa-4ddd-a3cc-af2793dbba31', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('fa435046-91fa-4ddd-a3cc-af2793dbba31', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('fa435046-91fa-4ddd-a3cc-af2793dbba31', foundational, commensurable_mortality).
narrative_ontology:cs_axiom_status(commensurable_mortality, holdable).
narrative_ontology:cs_axiom_grounding('fa435046-91fa-4ddd-a3cc-af2793dbba31', commensurable_mortality, empirically_contingent).
narrative_ontology:cs_axiom('fa435046-91fa-4ddd-a3cc-af2793dbba31', foundational, probability_discounting_legitimate).
narrative_ontology:cs_axiom_status(probability_discounting_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('fa435046-91fa-4ddd-a3cc-af2793dbba31', probability_discounting_legitimate, instrumental).
narrative_ontology:cs_reference_frame('fa435046-91fa-4ddd-a3cc-af2793dbba31', aggregate_mortality_minimization).
narrative_ontology:cs_drift_state('fa435046-91fa-4ddd-a3cc-af2793dbba31', contemporary_climate_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa435046-91fa-4ddd-a3cc-af2793dbba31', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__expected_value_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, nuclear_energy_sector).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__expected_value_dominant, renewable_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_fuel_labor).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, fossil_dependent_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__expected_value_dominant, nuclear_proximate_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the methodological rules for national and international energy risk assessment, determining which mortality metrics count and how probabilities are assigned to catastrophic scenarios. They justify permitting decisions and subsidy allocation by reference to aggregate expected harm minimization.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, energy_policy_agencies, agenda_setter,
    institutional, generational, analytical, global).

% Owns and operates nuclear generation assets. Gains regulatory licenses and investment flows because the expected-value framework discounts the probability of catastrophic accidents, making nuclear appear safer than fossil alternatives on the central metric.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_energy_sector, beneficiary,
    powerful, generational, constrained, global).

% Develops and deploys wind, solar, and other non-fossil generation. Benefits from the suppression of fossil competitors and from policy frameworks that favor low-mortality-per-TWh technologies.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Works in coal mining, oil and gas extraction, and fossil-fuel power generation. Faces job losses, wage suppression, and devaluation of trade skills as the framework justifies regulatory denial of new fossil projects and early retirement of existing plants.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_fuel_labor, payer,
    powerless, biographical, constrained, regional).

% Live in regions where tax bases, supply chains, and local services depend on fossil fuel extraction and combustion. Experience regional economic decline as investment is diverted to nuclear and renewable pathways under the mortality metric.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, fossil_dependent_communities, payer,
    moderate, generational, constrained, regional).

% Reside near nuclear power plants, waste storage facilities, or downstream of potential fallout paths. Bear the full consequences of any catastrophic accident while the policy framework treats that risk as negligible because it is multiplied by a low estimated probability.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, nuclear_proximate_populations, payer,
    powerless, biographical, trapped, local).

% Argue that catastrophic nuclear harms should not be discounted by probability and that deep uncertainty requires preserving multiple options. Are systematically excluded from regulatory hearings and policy consultations because their framework is treated as unquantifiable and therefore outside the decision logic.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, precautionary_advocates, excluded,
    moderate, biographical, constrained, national).

% Evaluate energy risk methodologies from outside the benefiting institutions. They can question the mortality metric's completeness, probability assignments, and discounting assumptions without facing the economic or physical consequences borne by payer seats.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__expected_value_dominant, independent_risk_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables collective comparison of energy technologies by converting disparate harm profiles into a single mortality-per-TWh metric, solving the coordination problem of how to choose between technologies with different risks.
% TRANSFER_FUNCTION: Moves policy support, investment capital, and regulatory approval from fossil fuel pathways to nuclear and renewable pathways; moves catastrophic tail-risk exposure from institutional decision-makers to nuclear-proximate populations via probability discounting.
% ABSENT_VOICES: Precautionary advocates who reject probability discounting for irreversible catastrophes; fossil fuel communities whose economic harm is treated as external to the energy-risk calculation; future generations exposed to long-duration nuclear waste harms that standard discounting renders negligible.
% DISAPPEARANCE_RATIONALE: If the expected-value minimization rule vanished, fossil fuel investment would regain regulatory footing, nuclear expansion would stall under tail-risk scrutiny, and policy discourse would shift toward precaution or portfolio diversification â the global energy mix would reorganize around different decision criteria.
% FOUNDING_PROBLEM: How to make rational energy policy choices when technologies impose different types and scales of harm under uncertainty; how to prevent paralysis by indecision in the face of competing risk claims.
% FOUNDING_PROBLEM_CORROBORATION: Energy economists and public health statisticians outside the precautionary tradition attest to the need for comparable metrics; however, much of this corroboration comes from institutions aligned with the policy agencies that benefit from the framework's dominance. No fully independent corroboration exists â the problem definition is itself contested by the excluded precautionary seat.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__expected_value_dominant, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the asymmetric imposition of costs: fossil livelihoods are stranded and nuclear tail risk is borne by localized populations who did not consent to the probability discounting. Suppression (0.76) is high because the constraint's dominance requires actively excluding precautionary frameworks and suppressing fossil investment through regulatory denial. Theater ratio (0.42) captures the performative objectivity of mortality-per-TWh claims â the metrics are empirically derived but the value choices (what counts as a death, which probabilities to use, intergenerational discounting) are hidden beneath a veneer of actuarial neutrality. Accessibility collapse (0.68) is high because once the EV frame is institutionalized, alternative criteria appear irrational or anti-science. Resistance (0.58) comes from fossil interests, affected regions, and precautionary movements.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (policy agencies, nuclear and renewable sectors) experience the constraint as neutral coordination â a rational solution to energy comparison. The payer seats (fossil labor, nuclear-proximate communities) experience it as imposed risk and economic harm. The excluded seat (precautionary advocates) experiences it as epistemic closure. The engine will compute these seats differently: low directionality for beneficiaries, high for trapped proximate populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (nuclear and renewable sectors) have low directionality because the constraint subsidizes their market position and regulatory standing. Agenda-setters have low-to-moderate directionality because the framework legitimates their authority. Payers have high directionality: fossil labor is geographically and skill-constrained, amplifying effective extraction; nuclear-proximate populations are trapped by property and livelihood, bearing risk they cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure rope (neutral coordination) by requiring victim identification and active enforcement, which the fossil suppression and tail-risk externalization satisfy. It also prevents mislabeling as pure snare by acknowledging the genuine coordination function: mortality-per-TWh does solve a real information problem in comparing energy harms. The tangled_rope classification captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_probability_empirical_basis,
    'Are the probability estimates for catastrophic nuclear accidents empirically grounded on sufficient historical data, or are they structurally underestimated by methodological choices?',
    'Independent retrospective audit of probabilistic risk assessments against actual incident frequencies and near-miss disclosures; comparison with actuarial standards in other industries.',
    'If probabilities are systematically underestimated, the EV frame extracts more risk from proximate populations than disclosed, shifting classification toward snare; if accurate, the extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_probability_empirical_basis, empirical, 'Whether nuclear tail risk probabilities are empirically sound or manipulated.').

omega_variable(
    fossil_suppression_independence,
    'Is the suppression of fossil pathways driven primarily by the mortality-per-TWh metric, or by independent climate-policy goals that use the metric as retrospective justification?',
    'Historical policy-trace analysis examining whether EV dominance preceded or followed the political decision to suppress fossils; compare stated justifications in regulatory preambles.',
    'If independent, the EV coordination function is a cover story for extraction (snare-leaning); if metric-driven, the coordination is genuine with extraction side-effects (tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_suppression_independence, conceptual, 'Whether fossil suppression is caused by the EV metric or uses it as cover.').

omega_variable(
    intergenerational_scope,
    'Does the framework''s typical use of standard discounting exclude intergenerational harms in a way that makes the mortality metric incomplete?',
    'Compare undiscounted and discounted harm inventories for each technology across 100+ year horizons; evaluate whether the discount rate is a normative choice disguised as technical parameter.',
    'If intergenerational harms are systematically excluded, the framework extracts from future populations and the effective scope is larger than acknowledged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_scope, conceptual, 'Whether intergenerational harm exclusion structurally biases the EV comparison.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__expected_value_dominant, theater_ratio, 0, 0.15).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__expected_value_dominant, theater_ratio, 8, 0.22).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__expected_value_dominant, theater_ratio, 16, 0.3).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__expected_value_dominant, theater_ratio, 24, 0.36).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_energy__expected_value_dominant, theater_ratio, 32, 0.4).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__expected_value_dominant, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__expected_value_dominant, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__expected_value_dominant, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__expected_value_dominant, option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is the expected_value_dominant reading of the acceptable_risk_energy kernel, decomposed from the catastrophic_tail_dominant and option_value_preserving readings per the epsilon-invariance principle. Each reading carries a distinct epsilon and structural profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
