% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Substance Control: Harm Reduction Reading
 *   domain: public health policy / criminal justice / political economy
 *
 * SUMMARY:
 *   This constraint instantiates the harm_reduction_reading of the
 *   substance_control_kernel. It treats substance use as a health condition
 *   requiring pragmatic intervention to reduce harm, independent of use
 *   cessation. Users exit the criminal victim set but remain subject to
 *   paternalistic health intervention and surveillance. The supply chain
 *   remains criminalized, preserving black-market violence and enforcement
 *   against suppliers while the state shifts to a service-provider role.
 *   Sibling readings include prohibition_reading (moral
 *   transgression/punishment) and legalization_reading (individual liberty
 *   with externality control).
 *
 * KEY AGENTS:
 *   - public_health_apparatus (agenda_setter/institutional): Administers harm reduction and retains coercive health authority.
 *   - substance_users (payer/powerless): Subject to health-system paternalism and criminalized supply chains.
 *   - criminalized_suppliers (payer/moderate): Bear concentrated enforcement under persistent supply prohibition.
 *   - affected_communities (beneficiary/organized): Receive public health externalities from harm reduction services.
 *   - prohibitionist_advocates (excluded/organized): Moral-framing advocates structurally absent from policy consensus.
 *   - epidemiological_observers (observer/institutional): Analytical seat tracking population-level outcomes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.62).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.58).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Substance Control: Harm Reduction Reading").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public health policy / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, 'debd03ab-b2cb-4aae-a675-f08a0076abbf').
narrative_ontology:cs_kernel_codification('debd03ab-b2cb-4aae-a675-f08a0076abbf', distributed).
narrative_ontology:cs_authority_grounding('debd03ab-b2cb-4aae-a675-f08a0076abbf', expertise).
narrative_ontology:cs_interpretation_layer_present('debd03ab-b2cb-4aae-a675-f08a0076abbf').
narrative_ontology:cs_reading_relation('debd03ab-b2cb-4aae-a675-f08a0076abbf', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('debd03ab-b2cb-4aae-a675-f08a0076abbf', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('debd03ab-b2cb-4aae-a675-f08a0076abbf', foundational, substance_use_as_health_condition).
narrative_ontology:cs_axiom_status(substance_use_as_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('debd03ab-b2cb-4aae-a675-f08a0076abbf', substance_use_as_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('debd03ab-b2cb-4aae-a675-f08a0076abbf', foundational, intervention_without_abstinence_mandate).
narrative_ontology:cs_axiom_status(intervention_without_abstinence_mandate, holdable).
narrative_ontology:cs_axiom_grounding('debd03ab-b2cb-4aae-a675-f08a0076abbf', intervention_without_abstinence_mandate, instrumental).
narrative_ontology:cs_reference_frame('debd03ab-b2cb-4aae-a675-f08a0076abbf', population_health_pragmatism).
narrative_ontology:cs_drift_state('debd03ab-b2cb-4aae-a675-f08a0076abbf', contemporary_opioid_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('debd03ab-b2cb-4aae-a675-f08a0076abbf', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_apparatus).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, affected_communities).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, criminalized_suppliers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers harm reduction programs including needle exchanges, supervised consumption sites, opioid substitution therapy, and overdose prevention. Controls public health funding and sets intervention protocols. Retains coercive tools such as mandatory treatment orders and civil commitment in many jurisdictions. Gains institutional authority and budget by framing substance use as a health condition requiring ongoing management.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Reclassified from criminals to patients under this reading, but remain subject to paternalistic health intervention, surveillance through service enrollment, and potential coerced treatment. Continue to rely on a criminalized supply chain for non-prescribed substances, exposing them to overdose risk and market violence. Exit is constrained by addiction, dependency on services, and the persistence of illicit supply.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, substance_users, payer,
    powerless, biographical, constrained, national).

% Bear the continued full force of drug law enforcement while user-targeting enforcement recedes. Face imprisonment, asset forfeiture, and violence inherent to black markets. Their continued criminalization is the structural condition that allows the harm reduction reading to maintain supply-side prohibition while shifting user-facing policy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, criminalized_suppliers, payer,
    moderate, biographical, trapped, regional).

% Experience reduced infectious disease transmission and overdose mortality due to publicly funded needle exchanges, supervised consumption, and naloxone distribution. Receive public health externalities without direct individual cost under this framework.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, affected_communities, beneficiary,
    organized, biographical, mobile, local).

% Advocate for abstinence-based and punitive drug policies. Structurally excluded from the harm reduction policy consensus, their moral framing is delegitimized in the health-apparatus discourse, though they continue to lobby for reversal.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibitionist_advocates, excluded,
    organized, generational, mobile, national).

% Monitor overdose rates, infectious disease incidence, and program efficacy through epidemiological frameworks. Provide analytical validation for the health-condition reading of substance use without direct stake in enforcement or service revenue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, epidemiological_observers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates population-level reduction in overdose mortality and infectious disease transmission by deploying supervised consumption sites, needle exchanges, opioid substitution therapy, and naloxone distribution without conditioning access on immediate abstinence.
% TRANSFER_FUNCTION: Transfers autonomy and compliance from substance users to the public health apparatus through paternalistic intervention and treatment mandates; transfers enforcement pressure from users to criminalized suppliers; transfers public funds to harm reduction service providers.
% ABSENT_VOICES: Prohibitionist constituencies who frame substance use as moral transgression are excluded from policy design. Libertarian legalization advocates who reject state paternalism and health-system management are also absent from the consensus.
% DISAPPEARANCE_RATIONALE: If the harm reduction framework vanished, overdose and disease transmission would rise rapidly where services currently operate; users would lose supervised consumption and substitution access; enforcement would revert to user-targeting or unstructured prohibition; public health institutions would lose the service-provider role and associated funding.
% FOUNDING_PROBLEM: The punitive War on Drugs produced mass incarceration, overdose epidemics, and infectious disease outbreaks without reducing substance use prevalence, creating simultaneous crises in public health and criminal justice.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data on overdose mortality and blood-borne pathogen incidence corroborates the failure of punitive approaches. International health organizations (WHO, UNODC) corroborate the health framing from outside the direct state beneficiary set, though their mandates are partially funded by member states. Prohibitionist criminal justice institutions contest the founding problem status, arguing the issue is substance use itself rather than the policy response.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the genuine reduction in mortality and disease (coordination) alongside the extraction of autonomy from users via paternalistic intervention and the criminalization of suppliers. Suppression (0.58) is moderate-high because supply-chain prohibition requires active enforcement and treatment mandates retain coercive capacity. Theater ratio rises to 0.47 as the health-framing discourse increasingly masks the persistence of carceral supply-side dynamics. Accessibility collapse (0.45) is moderate: alternatives (legalization, prohibition) are politically suppressed but intellectually available. Resistance (0.50) reflects pushback from prohibitionists, some user communities, and suppliers.
 *
 * PERSPECTIVAL GAP:
 *   The public health apparatus experiences the constraint as genuine coordination with necessary oversight; substance users experience it as continued state control through a medicalized lens; suppliers experience pure extraction via unmodified prohibition. The engine computes this divergence from structural data rather than authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   public_health_apparatus sits near the beneficiary end (low d) because it receives funding, authority, and institutional scope from the constraint. affected_communities also sit near the beneficiary end via positive externalities. substance_users and criminalized_suppliers sit near the target end (high d) because they bear the costs of paternalistic intervention and enforcement, respectively. The derivation reflects declared beneficiary/victim status and exit asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure extraction (snare) by acknowledging the measurable coordination benefit (reduced overdose, reduced disease transmission). It also prevents mislabeling as pure coordination (rope) by requiring victim declaration for the asymmetric costs borne by users (autonomy loss, coerced treatment) and suppliers (criminalization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_reduction_kernel_contestation,
    'This constraint is the harm_reduction_reading of the substance_control_kernel. Does its persistence structurally require the coexistence of supply-chain prohibition, or is it a transitional scaffold toward the legalization_reading?',
    'Jurisdictional comparison where harm reduction operates alongside regulated supply or full decriminalization versus jurisdictions retaining supply-side criminalization.',
    'If inseparable from supply-chain prohibition, the reading is a stable tangled_rope with sustained supplier extraction. If separable, it may function as a scaffold toward a less extractive regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_kernel_contestation, conceptual, 'Whether harm reduction can be sustained without the criminalized supply chain that accompanies it.').

omega_variable(
    paternalism_mechanism,
    'Is the extraction from substance users structurally coercive (mandatory treatment laws, civil commitment, service conditionalities) or internalized (self-surveillance under the disease model)?',
    'Comparative study of jurisdictions with voluntary versus compulsory harm reduction frameworks, measuring retention of user autonomy and rates of administrative coercion.',
    'If internalized, effective extraction exceeds the structural measure because users carry the constraint after exiting formal systems. If purely structural, non-compliant exit remains possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_mechanism, empirical, 'Structural versus internalized suppression mechanism in health-system paternalism.').

omega_variable(
    funding_dependence_extraction,
    'Does the public health apparatus''s funding and authority depend on maintaining substance use as a permanent managed population, creating perverse incentives against resolving the condition?',
    'Budget analysis tracking whether harm reduction funding scales with prevalence, mortality, or program outcomes; institutional survival analysis.',
    'If funding is prevalence-dependent, the agenda-setter is also a concentrated beneficiary of extraction, deepening the asymmetric capture and moving the constraint toward snare dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(funding_dependence_extraction, empirical, 'Whether institutional funding incentives sustain the problem the constraint claims to solve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(substance_harm_red_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(substance_harm_red_tr_t4, substance_control_kernel__harm_reduction_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(substance_harm_red_tr_t8, substance_control_kernel__harm_reduction_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(substance_harm_red_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(substance_harm_red_tr_t18, substance_control_kernel__harm_reduction_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(substance_harm_red_tr_t24, substance_control_kernel__harm_reduction_reading, theater_ratio, 24, 0.47).

% Extraction over time
narrative_ontology:measurement(substance_harm_red_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(substance_harm_red_be_t4, substance_control_kernel__harm_reduction_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(substance_harm_red_be_t8, substance_control_kernel__harm_reduction_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(substance_harm_red_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.56).
narrative_ontology:measurement(substance_harm_red_be_t18, substance_control_kernel__harm_reduction_reading, base_extractiveness, 18, 0.59).
narrative_ontology:measurement(substance_harm_red_be_t24, substance_control_kernel__harm_reduction_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(substance_harm_red_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(substance_harm_red_su_t4, substance_control_kernel__harm_reduction_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(substance_harm_red_su_t8, substance_control_kernel__harm_reduction_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(substance_harm_red_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(substance_harm_red_su_t18, substance_control_kernel__harm_reduction_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(substance_harm_red_su_t24, substance_control_kernel__harm_reduction_reading, suppression_requirement, 24, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_kernel. It is structurally distinct from prohibition_reading (which extracts via criminal punishment and moral stigmatization) and legalization_reading (which minimizes state paternalism and permits regulated markets). The epsilon values differ because harm reduction retains state paternalism and supply-chain criminalization while shifting user-facing enforcement to health services.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
