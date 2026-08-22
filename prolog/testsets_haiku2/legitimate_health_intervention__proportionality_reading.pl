% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__proportionality_reading, []).

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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Legitimate Health Intervention—Proportionality Reading
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the proportionality reading of legitimate
 *   health intervention: a framework that permits medical autonomy constraint
 *   (vaccination mandate, quarantine, contact tracing) ONLY when measurable
 *   disease threat (transmissibility R0, case-fatality rate IFR, or current
 *   case burden in a specific population) objectively justifies the severity
 *   of intervention. Both population harm and individual autonomy matter, but
 *   their weighting is conditional on disease characteristics. High-threat
 *   diseases (measles, pandemic influenza variants) justify substantial
 *   autonomy constraint; endemic low-threat diseases do not. The constraint
 *   is CLAIMED as tangled_rope (coordination between disease control and
 *   autonomy preservation) while the measurements show substantially
 *   extractive operation—the engine measures this divergence. The victim set
 *   and extraction level are disease-conditional: a measles outbreak in an
 *   unvaccinated community activates the constraint at high severity (high ε,
 *   high suppression of refusal); seasonal flu does not. This conditional
 *   structure is central to the reading and explains why the same agent may
 *   be classified differently under different disease scenarios.
 *
 * KEY AGENTS:
 *   - Public health authorities: institutional power, set threat-level thresholds, enforce mandates conditional on meeting proportionality test. Authority is conditional.
 *   - Medical autonomy bearers: moderate power, identity-locked exit (refusal means sanctions), bear the autonomy cost—victims under high-threat conditions, less-burdened under low-threat conditions.
 *   - Vaccine-hesitant groups: powerless, trapped exit, excluded from proportionality deliberation despite bearing the intervention cost—structural victims regardless of threat level.
 *   - Constitutional courts: institutional observers, adjudicate whether specific mandates meet proportionality standard, serve as external validators.
 *   - Disease characteristics (R0, IFR): the measurable parameters the constraint is indexed to—the proportionality scale. Yet these parameters are themselves contested and politicized.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.58).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.62).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Legitimate Health Intervention—Proportionality Reading").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, '803bf0ea-89d3-461a-b180-9db84cd5d37e').
narrative_ontology:cs_kernel_codification('803bf0ea-89d3-461a-b180-9db84cd5d37e', fixed_text).
narrative_ontology:cs_authority_grounding('803bf0ea-89d3-461a-b180-9db84cd5d37e', lineage).
narrative_ontology:cs_interpretation_layer_present('803bf0ea-89d3-461a-b180-9db84cd5d37e').
narrative_ontology:cs_reading_relation('803bf0ea-89d3-461a-b180-9db84cd5d37e', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('803bf0ea-89d3-461a-b180-9db84cd5d37e', legitimate_health_intervention__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('803bf0ea-89d3-461a-b180-9db84cd5d37e', foundational, intervention_severity_requires_proportionate_threat).
narrative_ontology:cs_axiom_status(intervention_severity_requires_proportionate_threat, holdable).
narrative_ontology:cs_axiom_grounding('803bf0ea-89d3-461a-b180-9db84cd5d37e', intervention_severity_requires_proportionate_threat, deontological).
narrative_ontology:cs_axiom('803bf0ea-89d3-461a-b180-9db84cd5d37e', foundational, autonomy_weighting_scales_with_threat_level).
narrative_ontology:cs_axiom_status(autonomy_weighting_scales_with_threat_level, holdable).
narrative_ontology:cs_axiom_grounding('803bf0ea-89d3-461a-b180-9db84cd5d37e', autonomy_weighting_scales_with_threat_level, instrumental).
narrative_ontology:cs_reference_frame('803bf0ea-89d3-461a-b180-9db84cd5d37e', medical_autonomy_proportionality_doctrine).
narrative_ontology:cs_drift_state('803bf0ea-89d3-461a-b180-9db84cd5d37e', contemporary_pandemic_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('803bf0ea-89d3-461a-b180-9db84cd5d37e', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, population_disease_risk_reduction).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, medical_autonomy_bearers).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The public health outcome—reduced disease transmission, prevented cases, lives saved—that the constraint is designed to achieve. Non-excludable: all citizens benefit from lower disease prevalence regardless of whether they accepted intervention. This benefit is real but diffuse; no single seat captures the gain.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, population_disease_risk_reduction, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(legitimate_health_intervention__proportionality_reading, population_disease_risk_reduction).

% Set disease-threat thresholds, declare emergencies, issue and enforce vaccination or quarantine mandates, and adjudicate proportionality claims. They operate under the proportionality reading's constraint: intervention severity must be justified by measurable threat (R0, IFR, or case burden). Their authority is conditional on meeting this test. In practice, they control threat-level assessment and can invoke proportionality as legitimacy cover. They benefit from the constraint because it provides legal/constitutional basis for action without unlimited challenge.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Individuals whose bodily autonomy and medical self-determination are constrained by vaccination mandates, quarantine orders, or contact-tracing requirements. They may object to the intervention (distrust vaccines, prefer natural immunity, have prior adverse-event experience) or simply assert their right to refuse. Identity-locked exit: refusing the intervention means accepting social sanctions, employment loss, denial of public-space access, or incarceration. They bear the autonomy cost of the constraint. Under high-threat disease scenarios (measles, pandemic flu), they are clearly victims. Under low-threat scenarios (endemic flu), the proportionality threshold is not met and the constraint's burden is less severe.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, medical_autonomy_bearers, payer,
    moderate, biographical, identity_locked, national).

% Populations with stated concerns about vaccines—cultural/religious objections, prior adverse-event experience, distributional-justice concerns about vaccine access or testing—or more fundamental doubt about the threat-level and proportionality claims. They bear the full cost of intervention mandates and have no structural power to participate in proportionality assessment. Trapped exit: they cannot exit the jurisdiction, their children are subject to school-based mandates, their employment is contingent on compliance. They are consistently victimized regardless of the disease threat level because they are excluded from the deliberation frame. The constraint treats their concerns as outside the legitimate epistemic space, even though proportionality doctrine nominally includes autonomy as a weighting factor.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_groups, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, vaccine_hesitant_groups, observer).

% The measurable disease characteristics—basic reproduction number (R0), infection fatality rate (IFR), case severity distribution, transmissibility by age, variant-specific parameters—that the proportionality reading is indexed to. Measles has R0~12, IFR ~0.1-0.2%; seasonal influenza has R0~1-2, IFR~0.1%; COVID-19 has R0~2-8 (variant-dependent), IFR~0.3-1% (age-dependent). These parameters are the referent for the proportionality constraint: they justify high autonomy constraint (measles) vs. low autonomy constraint (flu). Yet the parameters themselves are estimated from incomplete data, change with variants, and become politically contested. Excluded because the parameters are contested and not resolved through the constraint's deliberation process.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, disease_transmissibility_and_severity, excluded,
    analytical, immediate, analytical, universal).
narrative_ontology:stakeholder_non_agent(legitimate_health_intervention__proportionality_reading, disease_transmissibility_and_severity).

% Adjudicate whether specific public health mandates meet the proportionality test—whether the threat level justifies the intervention severity, whether less-restrictive alternatives exist, whether the victim-set classification is defensible. They serve as external validators of the constraint's structural integrity. In practice, courts often defer to public health authority expertise during active crises, so proportionality review may be more symbolic than functional during peak threat (corresponding to the theater-ratio spike in the measurement series).
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% The evolving empirical understanding of disease characteristics—case counts, transmission routes, intervention efficacy, adverse-event rates—that proportionality assessment depends on. Delays in evidence synthesis, suppression of adverse-event reports, or exclusive reliance on single-source evidence undermine the constraint's legitimacy by making threat-level assessment unreliable. The constraint's function depends on timely, transparent, peer-reviewed evidence, but the evidence base is often contested and politicized.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, epidemiological_evidence_base, excluded,
    analytical, biographical, analytical, universal).
narrative_ontology:stakeholder_non_agent(legitimate_health_intervention__proportionality_reading, epidemiological_evidence_base).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for balancing population disease control (collective interest) against individual medical autonomy (individual interest) by requiring that intervention severity be proportionate to measurable disease threat. Solves the collective-action problem of disease transmission—all citizens have incentive to free-ride on others' vaccination—while preserving individual bodily integrity by preventing indiscriminate coercion. The coordination problem is genuine: absent intervention, high-transmissibility diseases spread through the population; absent autonomy protection, authorities can impose unlimited coercion under the guise of health emergency. Proportionality bridges: intervention is permitted only when threat-level assessment justifies it, weighed against the cost to autonomy.
% TRANSFER_FUNCTION: Moves the bodily-autonomy burden from the collective level (all citizens at risk of disease transmission) to individual level (specific individuals undergo vaccination, quarantine, or contact tracing). The constraint is designed to ensure this transfer is necessary and scale-appropriate—measles outbreak with susceptible children justifies substantial autonomy constraint; seasonal flu does not. The transfer is from the unvaccinated or unquarantined individuals to the authority that enforces the mandate. The gains (reduced disease transmission, prevented cases) accrue diffusely to the population; the losses (bodily autonomy, medical self-determination) are concentrated on the payers (autonomy bearers and vaccine-hesitant groups).
% ABSENT_VOICES: Vaccine-hesitant populations and communities bearing concentrated intervention burden (lower baseline disease risk but high intervention cost) are structurally excluded from threat-level and proportionality assessment. Their epistemic contributions—lived experience with prior adverse events, alternative health frameworks, distributional-justice concerns—are treated as outside the legitimate deliberative frame. Dissenting epidemiologists and public health ethicists face institutional pressure to defer to consensus, narrowing the evidence base. Patients with comorbidities or specific medical reasons to refuse intervention have minimal standing in mandate policy. The constraint nominally includes autonomy as a weighting factor, but the deliberation process excludes the voices that bear the autonomy costs most acutely.
% DISAPPEARANCE_RATIONALE: If the proportionality constraint disappeared, public health authorities would operate under either the bodily_autonomy_primary reading (no medical coercion ever permitted) or the public_health_primary reading (coercion permitted whenever population benefit exists, regardless of threat level). Removal of proportionality would shift health governance toward one of the sibling readings. If governance shifted toward public_health_primary, authorities could impose vaccination mandates for endemic diseases, routine quarantine for seasonal flu, surveillance for low-risk infections—a substantially more coercive public health state. If governance shifted toward bodily_autonomy_primary, authorities could not intervene even during severe pandemics without explicit consent—a substantially less coordinated disease control system. The constraint prevents both extreme positions by anchoring intervention to measurable threat.
% FOUNDING_PROBLEM: Early pandemic response, historical public health crises, and abusive medical authority revealed that public health institutions, absent external constraint, could impose interventions wildly disproportionate to threat—forced sterilizations, indefinite quarantines on suspicion, population surveillance deployed under health authority. Post-WWII medical ethics (Nuremberg Code) and post-pandemic constitutional review codified proportionality as the corrective: intervention severity must be justified by threat level, must be necessary (no less-restrictive alternatives), and must be subject to external (judicial) review. The proportionality reading was forged to prevent both indiscriminate coercion masquerading as health emergency and medical paternalism justified by abstract collective benefit. It requires visible, justified, externally-reviewable threat assessment.
% FOUNDING_PROBLEM_CORROBORATION: Public health ethics literature (Beauchamp & Childress on the principle of justice; Gostin and Guttman on proportionality in public health law) documents both the history of abuse and the principle as corrective. Constitutional courts in Germany (Federal Constitutional Court precedent on proportionality in health mandates), Canada (Oakes test applied to Charter-protected liberty), and EU jurisdictions have formally adopted proportionality as a judicially enforceable requirement. Medical ethicists outside public health authorities attest that external proportionality review is necessary to prevent authority drift—institutional incentives for escalation are real. Public health historians document that absent proportionality review, intervention creep is systematic (mandates expand beyond initial justification, thresholds shift downward over time). Vaccine-hesitant populations and civil liberties organizations corroborate that without proportionality enforcement, mandates are imposed on weak evidentiary grounds and maintained despite threat decline (mandatrophy risk). The constraint remains contested (public_health_primary advocates argue proportionality delays lifesaving intervention; bodily_autonomy_primary advocates argue it is merely legitimacy theater), but it is broadly acknowledged as necessary by medical ethicists, constitutional lawyers, and health-law scholars outside the benefiting authorities.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.58 because the constraint permits substantial autonomy constraint under high-threat disease scenarios (measles, pandemic flu), but that constraint is conditionally legitimate—it is not arbitrary rent extraction, it is justified by the threat level. The measurement trajectory shows extractiveness rising (0.38→0.58) as a disease threat escalates through early detection→spike→peak→decline, then declining again as the threat recedes. This cyclical pattern reflects the conditional logic: the same constraint is less extractive under low-threat conditions and more extractive when threat is genuine. Suppression requirement follows the same pattern: higher during acute outbreak (authorities must actively suppress refusal-based resistance), lower during endemic periods. Theater ratio rises sharply (0.12→0.32) during peak threat as social enforcement of mandates becomes performative (visible compliance checking, public health theater), then declines as threat recedes and authorities can afford to relax visibility. This is not piton-level theater—the core function (disease control through intervention) remains real—but the trajectory shows how the constraint's legitimacy framing becomes increasingly hollow at peak threat, even as the threat is real. Suppression is high (0.62) because maintaining the constraint requires active resistance suppression: vaccine-hesitant groups must be prevented from organizing refusal campaigns, from accessing unrestricted spaces, from spreading alternative risk-assessment narratives. This structural suppression is necessary for the constraint to persist, which raises the Tangled Rope classification question: is this coordination or enforced extraction?
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority seat, this is genuine coordination: the constraint protects both population health AND individual autonomy by limiting intervention to proportionate responses. From the vaccine-hesitant or autonomy-maximalist seat, the same structure is enforced extraction: the threat level is politically asserted, not objectively established; the proportionality threshold is set by authorities with incentive to escalate; and identity-locked refusal makes exit impossible. From the constitutional court seat, the constraint is an incomplete governance mechanism—it requires external validation at each disease event. The engine computes these divergent readings from the structural data: high suppression + high extractiveness = snare or tangled rope at the payer seat; genuine coordination benefit + victim set = tangled rope at the beneficiary seat. The claim of tangled rope reflects the designer's intent (genuine coordination with necessary but asymmetric costs); the metrics reflect the actual operation (high suppression, conditional extraction, resistance). The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities benefit from the constraint because it legitimates their intervention authority conditional on threat assessment—they can act decisively in genuine crises without legal challenge, yet are constrained in low-threat periods. Their directionality is near-symmetric: they benefit from legitimacy, bear costs of proportionality review. Medical autonomy bearers are the primary victims under high-threat conditions (lose bodily autonomy, face sanctions), less-burdened under low-threat conditions. Their directionality is disease-dependent: d rises toward target (high autonomy cost) under high-threat conditions, falls toward beneficiary (low autonomy cost) under low-threat conditions. Vaccine-hesitant groups are consistently victimized because they are trapped (no exit) and excluded (no voice in proportionality assessment)—their d is consistently near 1.0 (full target). The constraint's population-health benefit is real but non-excludable: all citizens benefit from reduced disease transmission regardless of whether they accepted the intervention. This non-excludable benefit diffuses the beneficiary position across the entire population, not concentrated in any single seat. The authority's beneficiary position comes from legitimacy/authority, not from health benefits. This asymmetry (diffuse health benefit + concentrated authority benefit + concentrated victim burden for hesitant groups) is the extractive structure underlying the tangled-rope claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem was real—historical public health authorities did impose indiscriminate interventions (forced sterilization, indefinite quarantine) without proportionality review. The constraint was designed to prevent this. Yet the measurement series (particularly the theater-ratio spike at time 18, peak threat) shows that proportionality review itself becomes performative during crisis: authorities assert threat level, courts defer, and the proportionality check becomes a legitimacy cover rather than a functional gate. This is not yet full mandatrophy (the constraint's core function—preventing unlimited intervention—remains operative and was not bypassed during the measured period), but the theater trajectory suggests mandatrophy risk: as threat perception escalates and public fear rises, the proportionality requirement increasingly becomes theater—asserted but not enforced, invoked for legitimacy but not applied to prevent intervention escalation. The measurement series should be read as early-stage mandatrophy risk, not resolved mandatrophy. If the theater ratio continues to rise while suppression remains high and extractiveness does not decline, the constraint may be transitioning from tangled rope (genuine coordination with necessary costs) to snare (extraction cover). This is the reading the corpus measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_level_epistemic_asymmetry,
    'Who determines the threat level—transmissibility and case-fatality rate—and how are disputes about those parameters resolved?',
    'Real-world: variant emergence (COVID-19 Omicron presented as ''milder'' but higher transmissibility, changing the proportionality calculus) and expert disagreement on IFR estimates reveal that threat level is not a simple observable. Structural resolution: establish independent epidemiological review boards with mandatory representation from dissenting expert voices and communities bearing intervention burden.',
    'If threat-level assessment is controlled by authorities with incentive to escalate (more severe threat justifies more authority), the proportionality constraint becomes a cover for arbitrary coercion. The constraint''s legitimacy depends on treating threat-level assessment itself as contestable and externally reviewable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_level_epistemic_asymmetry, empirical, 'Epistemic authority over disease threat parameters.').

omega_variable(
    identity_lock_suppression_mechanism,
    'Is the measured suppression (0.62) structural (legal/economic barriers) or internalized (vaccine-hesitant groups have been socialized to accept refusal as identity suicide)?',
    'Post-intervention trajectory: if vaccine-hesitant populations'' suppression persists after mandates end (they remain isolated, continue to distrust authorities, maintain parallel information networks), the suppression is partially internalized and the true exit cost is higher than the legal/structural barriers.',
    'If suppression is internalized, the constraint''s effective suppression is higher than authored, the victim set is more severely trapped, and the extraction is more complete (carried with them even after exit). The constraint would recompute toward snare from the victim seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Suppression mechanism: structural vs. internalized in vaccine-hesitant communities.').

omega_variable(
    proportionality_as_asymptotic_legitimacy,
    'Is proportionality a meaningful constraint on authority (the legitimacy gate is real and enforced), or is it an asymptotic legitimacy claim (any intervention can be post-hoc justified as proportionate to some re-assessed threat)?',
    'Case-law analysis: count instances where courts rejected public health authorities'' proportionality claims and where authorities were forced to modify or abandon mandates based on proportionality review. If rejection rate is near-zero, proportionality is cover; if substantial, proportionality is functional.',
    'If proportionality is cover, the constraint''s legitimacy is theater and mandatrophy is advanced—the constraint persists (authorities cite it) but no longer functions (it prevents nothing). If proportionality is functional, the constraint is a genuine check on authority and the tangled-rope reading is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_asymptotic_legitimacy, empirical, 'Whether proportionality review is a meaningful gate or post-hoc legitimation.').

omega_variable(
    conditional_constraint_structure_measurement,
    'The proportionality constraint is disease-conditional: high-threat diseases (measles, pandemic flu) activate it at high severity; low-threat diseases do not. Can a single ε value capture this conditional structure, or does the constraint require parameterization by threat level?',
    'Decomposition per the ε-invariance principle: author separate constraints for high-threat and low-threat disease scenarios, each with its own ε, victim set, and measurement series. Link them via network.affects_constraints.',
    'The authored ε=0.58 is a time-averaged value across a typical year including multiple disease events. If the goal is to model the constraint''s structure accurately, decomposing into threat-level-conditional variants would be more precise. Alternatively, author a wider range of measurements spanning multiple disease events to show the constraint''s actual operation across different threat scenarios.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conditional_constraint_structure_measurement, conceptual, 'Single vs. parameterized constraint modeling for disease-conditional interventions.').

omega_variable(
    reading_coexistence_legitimacy,
    'Can the three readings (autonomy_primary, proportionality, public_health_primary) coexist in a single jurisdiction''s constitutional and health-law framework, or does adopting one reading foreclose the others?',
    'Comparative constitutional law: jurisdictions (Germany, Canada, U.S.) have adopted proportionality doctrine; others (classical autonomy framings) maintain inviolable bodily integrity; still others (Singapore, China) adopt public-health-primary approaches. The fact that all three are live in different jurisdictions suggests coexistence across the global system, but foreclosure within jurisdictions (adoption of one reading often de-emphasizes the others institutionally).',
    'If readings coexist, the sibling relationships are coexists_with (not forecloses). If one reading forecloses others within jurisdictions, relationships may include forecloses edges. The net assessment is that readings coexist globally but influence local institutional emphasis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_legitimacy, conceptual, 'Logical and institutional coexistence of the three health-intervention readings.').

omega_variable(
    ground_truth_disease_parameters,
    'What is the true case-fatality rate and transmissibility for specific diseases, given that these parameters are estimated from incomplete data, change with variants and host factors, and are themselves politically contested?',
    'Epidemiological consensus building: long-term follow-up, meta-analysis across studies, post-hoc comparison of predictions vs. outcomes. Yet by definition, true ground truth emerges only after the crisis decisions are made—proportionality review must operate under uncertainty.',
    'Uncertainty in threat-level parameters undermines the proportionality constraint''s ability to distinguish justified from unjustified intervention. Authorities can always cite worst-case threat estimates; the constraint can prevent this only if it requires best-available evidence and peer review, not strategic threat-level assertion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ground_truth_disease_parameters, empirical, 'Epistemic uncertainty in disease threat parameters underlying proportionality assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__proportionality_reading, theater_ratio, 6, 0.16).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__proportionality_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__proportionality_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__proportionality_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__proportionality_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__proportionality_reading, theater_ratio, 36, 0.28).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__proportionality_reading, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__proportionality_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__proportionality_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__proportionality_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__proportionality_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__proportionality_reading, base_extractiveness, 36, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__proportionality_reading, suppression_requirement, 6, 0.51).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__proportionality_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__proportionality_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__proportionality_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__proportionality_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__proportionality_reading, suppression_requirement, 36, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__proportionality_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).

% DUAL FORMULATION NOTE:
% The legitimate_health_intervention kernel decomposes into three structurally distinct constraints, each a reading with different ε, victim sets, and legitimacy criteria. The proportionality reading (this file) indexes intervention severity to measurable disease threat and weights both autonomy and public health. The bodily_autonomy_primary reading treats autonomy as inviolable regardless of threat. The public_health_primary reading prioritizes population morbidity/mortality reduction and treats refusal as externality. All three readings can coexist across different jurisdictions; within jurisdictions, the choice of reading structures the entire public health law framework. The sibling constraints are linked here as family members, not as alternatives to choose between—the corpus documents how different readings operate and where they diverge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__proportionality_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
