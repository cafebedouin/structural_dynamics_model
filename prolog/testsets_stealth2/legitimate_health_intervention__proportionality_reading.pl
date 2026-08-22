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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: legitimate_health_intervention__proportionality_reading
 *   human_readable: Threat-Proportioned Legitimacy Standard for Coercive Health Intervention (Proportionality Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the proportionality_reading of the contested
 *   kernel legitimate_health_intervention: the claim that a health
 *   intervention is legitimate when its severity is proportioned to measured
 *   threat, with population harm and individual autonomy both entering the
 *   calculation, weighted by disease characteristics such as transmissibility
 *   and case fatality. The referent of the authored extractiveness is the
 *   standing arrangement this reading is about — proportionality-governed
 *   intervention legitimacy as actually administered and reviewed (isolation,
 *   quarantine, mandate, and closure decisions passing through agency threat
 *   assessment and judicial proportionality review) — assessed by this
 *   reading's own lights, which concede real rotating burdens rather than
 *   crediting the reading's endorsed ideal. The claim and the metrics are
 *   independent authored facts: the constraint is CLAIMED as tangled_rope
 *   because it possesses a genuine coordination function (a shared,
 *   adjudicable calibration of coercive power) while the same structure
 *   imposes asymmetric, rotating costs — refusing minorities bear coercion in
 *   high-threat regimes, the immunocompromised and elderly bear residual risk
 *   in low-threat ones — sustained by active enforcement. The sibling
 *   readings (public_health_primary, bodily_autonomy_primary) are separate
 *   stories with their own epsilon values and victim sets, not described
 *   here; see the kernel_reading_contestation omega and the network links.
 *   KEY AGENTS (by structural relationship): - public_health_agencies:
 *   Agenda-setting administrator and conditional beneficiary
 *   (institutional/constrained) — converts threat assessments into
 *   enforceable authority - constitutional_courts: Agenda-setting interpreter
 *   of the standard (institutional/constrained) -
 *   mandate_refusing_individuals: Primary target in high-threat regimes
 *   (powerless/constrained) — bears coercion - immunocompromised_and_elderly:
 *   Primary target in low-threat regimes (powerless/trapped) — bears residual
 *   risk - general_population: Principal beneficiary (moderate/constrained) -
 *   civil_liberties_organizations: Secondary beneficiary and litigant
 *   (organized/mobile) - epidemiological_advisory_bodies: Threat-input
 *   supplier, analytical seat with partial capture exposure
 *   (institutional/analytical) - residents_of_discretionary_jurisdictions:
 *   Excluded voice — lacks the standard's protection entirely
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setting administrator and conditional beneficiary (institutional/constrained)
 *   - constitutional_courts: agenda-setting interpreter of the standard (institutional/constrained)
 *   - mandate_refusing_individuals: primary target in high-threat regimes (powerless/constrained)
 *   - immunocompromised_and_elderly: primary target in low-threat regimes (powerless/trapped)
 *   - general_population: principal beneficiary (moderate/constrained)
 *   - civil_liberties_organizations: secondary beneficiary and litigant (organized/mobile)
 *   - epidemiological_advisory_bodies: threat-input supplier, analytical seat with partial capture exposure (institutional/analytical)
 *   - residents_of_discretionary_jurisdictions: excluded voice lacking the standard's protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__proportionality_reading, 0.42).
domain_priors:suppression_score(legitimate_health_intervention__proportionality_reading, 0.55).
domain_priors:theater_ratio(legitimate_health_intervention__proportionality_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(legitimate_health_intervention__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__proportionality_reading, "Threat-Proportioned Legitimacy Standard for Coercive Health Intervention (Proportionality Reading)").
narrative_ontology:topic_domain(legitimate_health_intervention__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__proportionality_reading, 'b588eaea-9823-404f-b11e-0a71df4db3ca').
narrative_ontology:cs_kernel_codification('b588eaea-9823-404f-b11e-0a71df4db3ca', formalized).
narrative_ontology:cs_authority_grounding('b588eaea-9823-404f-b11e-0a71df4db3ca', lineage).
narrative_ontology:cs_interpretation_layer_present('b588eaea-9823-404f-b11e-0a71df4db3ca').
narrative_ontology:cs_reading_relation('b588eaea-9823-404f-b11e-0a71df4db3ca', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('b588eaea-9823-404f-b11e-0a71df4db3ca', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_axiom('b588eaea-9823-404f-b11e-0a71df4db3ca', foundational, coercion_requires_threat_proportionality).
narrative_ontology:cs_axiom_status(coercion_requires_threat_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('b588eaea-9823-404f-b11e-0a71df4db3ca', coercion_requires_threat_proportionality, instrumental).
narrative_ontology:cs_axiom('b588eaea-9823-404f-b11e-0a71df4db3ca', foundational, autonomy_and_population_harm_jointly_weighted).
narrative_ontology:cs_axiom_status(autonomy_and_population_harm_jointly_weighted, holdable).
narrative_ontology:cs_axiom_grounding('b588eaea-9823-404f-b11e-0a71df4db3ca', autonomy_and_population_harm_jointly_weighted, deontological).
narrative_ontology:cs_axiom('b588eaea-9823-404f-b11e-0a71df4db3ca', secondary, disease_characteristics_set_the_weights).
narrative_ontology:cs_axiom_status(disease_characteristics_set_the_weights, holdable).
narrative_ontology:cs_axiom_grounding('b588eaea-9823-404f-b11e-0a71df4db3ca', disease_characteristics_set_the_weights, empirically_contingent).
narrative_ontology:cs_reference_frame('b588eaea-9823-404f-b11e-0a71df4db3ca', threat_proportioned_coercive_authority).
narrative_ontology:cs_drift_state('b588eaea-9823-404f-b11e-0a71df4db3ca', post_acute_emergency_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b588eaea-9823-404f-b11e-0a71df4db3ca', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, general_population).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, mandate_refusing_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__proportionality_reading, immunocompromised_and_elderly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__proportionality_reading, immunocompromised_and_elderly).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, disease_severity_gradient_legitimacy).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, least_restrictive_means_doctrine).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__proportionality_reading, necessity_principle_in_public_health).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run surveillance, declare outbreaks, and impose or lift isolation, quarantine, vaccination mandates, and closure orders. Under this standard each imposition must be justified by measured threat characteristics such as transmissibility and severity, and each imposition hands the agency enforceable authority that tends to persist in legal frameworks after the threat recedes. Exit from this role is not available; the agency is bound to justify its actions by the very standard that empowers it.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, public_health_agencies, beneficiary).

% Review challenges to health interventions and decide whether the severity of a measure matched the threat that justified it. Their interpretations set precedents that recalibrate what agencies may do. They cannot decline the role when challenged cases arrive, and their authority depends on doctrinal continuity they did not choose.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Decline vaccination or accept only limited quarantine compliance on conscientious, religious, or medical-grounds objections. When threat assessments cross the threshold that licenses mandates, they face job loss, venue exclusion, fines, or detention. Individual exit options are narrow: relocate, change employment, or litigate at personal cost; an intervention imposed on the body cannot be exited once delivered.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, mandate_refusing_individuals, payer,
    powerless, biographical, constrained, national).

% Bear the residual infection risk left when interventions are judged disproportionate to a circulating threat, as in ordinary respiratory-virus seasons. In high-threat episodes the same standard protects them first, mandating the precautions they cannot procure privately. They cannot exit their susceptibility; shielding measures are partial and depend on other people's compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, immunocompromised_and_elderly, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__proportionality_reading, immunocompromised_and_elderly, beneficiary).

% Receive calibrated protection: coercion when threat justifies it, liberty when it does not. They fund and staff the enforcement apparatus through taxes and compliance, and vote on the governments that appoint agency leadership. Moving between jurisdictions with differing standards is possible but costly.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, general_population, beneficiary,
    moderate, biographical, constrained, national).

% Litigate against overbroad interventions and publish assessments of emergency powers. They gain standing and membership when the standard blocks overreach, and lose relevance when interventions are few. They can redirect attention to other issues when health coercion subsides.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, civil_liberties_organizations, beneficiary,
    organized, biographical, mobile, national).

% Supply the transmissibility and case-fatality estimates that drive the weighting. Their committees sit formally inside health ministries; their estimates are cited by agencies seeking to justify impositions and by litigants challenging them. Analytical distance is partial: funding and access run through the same ministries they advise.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, epidemiological_advisory_bodies, observer,
    institutional, generational, analytical, global).

% Live under health authorities that impose isolation or treatment with no proportionality review at all, so detention can be indefinite and unreviewable. They would benefit from adoption of a calibrated standard but have no seat in the jurisdictions where the standard is contested; their situation surfaces mainly in human-rights reporting.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__proportionality_reading, residents_of_discretionary_jurisdictions, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__proportionality_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the calibration problem for coercive public-health power: gives agencies, courts, and publics a shared, adjudicable standard for when isolation, quarantine, mandate, or closure is permissible, so that each episode does not renegotiate the limits of state medical power from scratch.
% TRANSFER_FUNCTION: Moves coercive burden and risk along the threat gradient: in high-threat regimes it moves compliance burdens (vaccination, isolation, restricted movement) from the general population onto refusing minorities; in low-threat regimes it leaves infection risk parked on the immunocompromised and elderly; across both it converts threat assessments into enforceable authority held by health agencies.
% ABSENT_VOICES: Residents of jurisdictions whose health authorities impose isolation or treatment with no proportionality review at all — they would object that the contest presupposes a protection they lack. Also minority communities with generational reasons to distrust state medicine: their consent interests enter this reading only as one weighted factor among several, never as a veto, and they are seldom seated on the advisory bodies that set the weights.
% DISAPPEARANCE_RATIONALE: If the proportionality standard vanished overnight, intervention legitimacy would collapse to whichever sibling filled the vacuum: pure official discretion where agencies dominate, with the detention abuses the standard was built to prevent, or near-total consent requirements where autonomy advocates dominate, with preventable spread in high-threat episodes. Courts would lose their health-powers docket, agencies would lose the authorization basis for emergency action, and every novel pathogen would restart the fight from zero.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century health law paired indispensable quarantine powers with arbitrary detention: carriers confined indefinitely without review, interventions imposed by fiat, and later a counter-wave of consent absolutism that left populations exposed. The arrangement was built to make necessary coercion reviewable and proportioned instead of discretionary or forbidden.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Siracusa Principles (1984, drafted by human-rights jurists rather than health agencies) codify threat-proportioned limitation of movement rights; post-war constitutional and bioethics scholarship documents both the detention abuses and the consent backlash; court opinions in multiple jurisdictions independently articulate the proportionality requirement; and historical commissions document the discretionary-era abuses the standard responds to. Health agencies also attest liveness, but the attestation above does not rest on them.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__proportionality_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__proportionality_reading_tests).
:- end_tests(legitimate_health_intervention__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42: the standard's burdens are real but conditional and rotating, and the post-emergency period retains ratchet residue (expanded frameworks, normalized review deferral) that keeps the figure above its pre-episode baseline near 0.3. Suppression is authored at 0.55 as a raw structural property, unscaled by scope: enforcement capacity is legal and logistical (employment conditionality, travel rules, isolation orders) rather than internalized belief, and it was partially rolled back after the acute phase while statutory frameworks remained. Theater ratio 0.38: proportionality assessments contain genuine analytic work, but emergency-period practice drifted toward justificatory boilerplate written to ratify decisions already taken. Accessibility collapse 0.55: inside the reading's framework the alternatives (pure official discretion, unconditional consent requirements) collapse as serious contenders, yet they persist as live sibling positions and in jurisdictions without review, so collapse is partial. Resistance 0.6: litigation waves, exemption movements, and scholarly contestation are sustained and occasionally successful. Coalition potential is real for the nominally powerless: refusing individuals organized into movements that moved several jurisdictions to soften mandates, which is why their seat is authored powerless rather than organized — the coalition is episodic, not standing. Identity fusion operates on the administrative seat: professional identity constituted around population protection biases threat characterization toward the coercive-license side, which is flagged in the threat_assessment_capture omega rather than corrected by override. The measurement series share one time grid (t indexes years since 1990; t=31 is the acute emergency year, t=34 the post-rollback present) so no metric borrows another's endpoint; the burden rotation across disease regimes is a slower cycle than the grid samples and is documented structurally rather than temporally.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent verdicts from identical structure. From the mandate_refusing seat the standard is an authorization machine: it converts an epidemiological threshold into a warrant operating on their bodies, with constrained exit, so the arrangement reads as enforced extraction. From the immunocompromised seat the same standard reads as licensed abandonment in low-threat seasons — their extraction phase is the mirror image, and their secondary beneficiary role in high-threat phases does not cancel it because the phases alternate rather than compensate. From the general_population seat the arrangement is net protection bought at diffuse tax and compliance cost. The administrative seat experiences the standard as both empowerment and fetter: it receives calibrated authority and accumulates it across episodes while being formally bound to justify each exercise. Courts and advisory bodies sit near the analytical middle, though advisory bodies carry partial capture exposure through ministerial funding and access. Inter-institutionally, agencies and courts hold similar nominal institutional power but different exits: courts cannot refuse cases, agencies cannot refuse missions, but agencies control the threat inputs courts consume — an asymmetry the derivation chain sees only through the capture omega.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: general_population and civil_liberties_organizations derive near-beneficiary positions, and public_health_agencies derive a low-moderate position from their beneficiary role — tempered by their own subjection to the standard, which is why no directionality override is authored; the residual distortion (the capture incentive) is routed to an omega instead of an override because the derivation is broadly right and only the incentive gradient is in doubt. Victim declarations drive the two payer seats to the target end: mandate_refusing_individuals with constrained exit sit near full-target in high-threat regimes; immunocompromised_and_elderly sit near full-target in low-threat regimes despite their secondary_role beneficiary entry, because their exit from susceptibility is trapped. The rotation between these two target seats across disease regimes is the structural signature the expected delta predicted: the victim set varies with severity, and the scalar epsilon averages across the rotation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating necessary health coercion while preventing arbitrary detention and overreach — remains live: every novel pathogen reopens the calibration question, so no mandatrophy is declared. The tangled_rope claim is what prevents mislabeling in both directions: calling the arrangement pure coordination would erase the rotating victims that the bodily_autonomy sibling exists to name; calling it pure extraction would erase the genuine function (the standard demonstrably prevents both arbitrary detention and abandonment-style overreaction, and its absence in discretionary jurisdictions is visibly worse for the unprotected). The zombie pathway is monitored rather than asserted: if emergency-expanded frameworks persist unexercised across episode-free decades while the founding problem fades, the founding_problem_status x disappearance_verdict mismatch fires and the arrangement drifts toward performance-maintained territory. The emergency_authority_ratchet omega is the tripwire.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the proportionality_reading of the legitimate_health_intervention kernel; how would classification shift under the sibling readings, public_health_primary and bodily_autonomy_primary?',
    'Generate the sibling stories and compare computed classifications. The disagreement is located in the weighting premise: whether individual autonomy enters legitimacy independently of aggregate outcomes, and whether threat can ever override consent.',
    'Under bodily_autonomy_primary the victim set expands to every coerced individual regardless of disease severity and the arrangement computes as far more extractive; under public_health_primary refusal is redefined as externality and measured extraction drops toward coordination cost. Cross-reading comparison, not within-story metrics, resolves the contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings instantiate different victim sets and epsilon bases.').

omega_variable(
    epsilon_conditionality_decomposition,
    'Extractiveness scales with disease characteristics and the victim set differs between a measles-scale threat and a seasonal-flu threat, so is the authored scalar epsilon a stable property of one constraint or an average over several condition-specific constraints?',
    'Per the epsilon-invariance principle, decompose into per-regime stories (high-transmissibility/high-fatality regime versus low-severity endemic regime) if engine evaluation shows the scalar unstable across observable regimes, linking the fragments via network.affects_constraints.',
    'Decomposed, the high-threat regime carries markedly higher extractiveness concentrated on refusing minorities and the low-threat regime carries extractiveness shifted onto the vulnerable; the unified scalar currently averages these apart.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_conditionality_decomposition, conceptual, 'Conditional constraint structure: whether one story or a family of regime-specific stories is the right unit.').

omega_variable(
    threat_assessment_capture,
    'Do agency threat characterizations that trigger coercive licensing track independent epidemiological estimates, or does the threshold structure reward inflating severity to unlock authority?',
    'Retrospective comparison of agency threat declarations against independent serological and excess-mortality reconstructions across multiple episodes.',
    'Systematic inflation converts the standard''s licensing gate into an authority-rent mechanism and pushes the arrangement toward pure-extraction dynamics; accurate characterization supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_assessment_capture, empirical, 'Whether the threat inputs driving the weighting are captured by the seat that gains authority from them.').

omega_variable(
    residual_risk_allocation_fairness,
    'In low-threat phases the standard leaves immunocompromised and elderly people bearing residual infection risk: is that burden an unavoidable cost of respecting majority autonomy, or extractive because feasible shielding and support measures went unprovided?',
    'Audit what ventilation, sick-leave, antiviral-access, and targeted-shielding measures were feasible and offered during low-threat phases versus what vulnerable people actually received.',
    'If feasible protections were skipped, the vulnerable seat''s burden is extraction rather than tragedy, raising effective extractiveness and strengthening the case for regime decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_risk_allocation_fairness, preference, 'Whether the low-threat residual risk borne by the vulnerable is legitimate trade-off or dumped cost.').

omega_variable(
    emergency_authority_ratchet,
    'Legal frameworks expanded during acute emergencies tend to persist after the threat recedes: does post-emergency retention reflect functional precaution or inertial accumulation of unused coercive capacity?',
    'Track statutory sunset and renewal of emergency health powers across successive episode-free intervals; measure how much retained authority is exercised or reviewed.',
    'Persistent unexercised authority raises the theater ratio and signals drift toward performance-maintained arrangements; clean sunsetting supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_authority_ratchet, empirical, 'Whether emergency-expanded frameworks decay back or ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__proportionality_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lhi_proportionality_tr_t0, legitimate_health_intervention__proportionality_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(lhi_proportionality_tr_t0, observed).
narrative_ontology:measurement(lhi_proportionality_tr_t7, legitimate_health_intervention__proportionality_reading, theater_ratio, 7, 0.21).
narrative_ontology:measurement_basis(lhi_proportionality_tr_t7, observed).
narrative_ontology:measurement(lhi_proportionality_tr_t14, legitimate_health_intervention__proportionality_reading, theater_ratio, 14, 0.23).
narrative_ontology:measurement_basis(lhi_proportionality_tr_t14, observed).
narrative_ontology:measurement(lhi_proportionality_tr_t21, legitimate_health_intervention__proportionality_reading, theater_ratio, 21, 0.25).
narrative_ontology:measurement_basis(lhi_proportionality_tr_t21, observed).
narrative_ontology:measurement(lhi_proportionality_tr_t28, legitimate_health_intervention__proportionality_reading, theater_ratio, 28, 0.26).
narrative_ontology:measurement_basis(lhi_proportionality_tr_t28, observed).
narrative_ontology:measurement(lhi_proportionality_tr_t31, legitimate_health_intervention__proportionality_reading, theater_ratio, 31, 0.45).
narrative_ontology:measurement_basis(lhi_proportionality_tr_t31, observed).
narrative_ontology:measurement(lhi_proportionality_tr_t34, legitimate_health_intervention__proportionality_reading, theater_ratio, 34, 0.38).
narrative_ontology:measurement_basis(lhi_proportionality_tr_t34, observed).

% Extraction over time
narrative_ontology:measurement(lhi_proportionality_be_t0, legitimate_health_intervention__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(lhi_proportionality_be_t0, observed).
narrative_ontology:measurement(lhi_proportionality_be_t7, legitimate_health_intervention__proportionality_reading, base_extractiveness, 7, 0.29).
narrative_ontology:measurement_basis(lhi_proportionality_be_t7, observed).
narrative_ontology:measurement(lhi_proportionality_be_t14, legitimate_health_intervention__proportionality_reading, base_extractiveness, 14, 0.31).
narrative_ontology:measurement_basis(lhi_proportionality_be_t14, observed).
narrative_ontology:measurement(lhi_proportionality_be_t21, legitimate_health_intervention__proportionality_reading, base_extractiveness, 21, 0.33).
narrative_ontology:measurement_basis(lhi_proportionality_be_t21, observed).
narrative_ontology:measurement(lhi_proportionality_be_t28, legitimate_health_intervention__proportionality_reading, base_extractiveness, 28, 0.34).
narrative_ontology:measurement_basis(lhi_proportionality_be_t28, observed).
narrative_ontology:measurement(lhi_proportionality_be_t31, legitimate_health_intervention__proportionality_reading, base_extractiveness, 31, 0.52).
narrative_ontology:measurement_basis(lhi_proportionality_be_t31, observed).
narrative_ontology:measurement(lhi_proportionality_be_t34, legitimate_health_intervention__proportionality_reading, base_extractiveness, 34, 0.42).
narrative_ontology:measurement_basis(lhi_proportionality_be_t34, observed).

% Suppression requirement over time
narrative_ontology:measurement(lhi_proportionality_su_t0, legitimate_health_intervention__proportionality_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(lhi_proportionality_su_t0, observed).
narrative_ontology:measurement(lhi_proportionality_su_t7, legitimate_health_intervention__proportionality_reading, suppression_requirement, 7, 0.33).
narrative_ontology:measurement_basis(lhi_proportionality_su_t7, observed).
narrative_ontology:measurement(lhi_proportionality_su_t14, legitimate_health_intervention__proportionality_reading, suppression_requirement, 14, 0.36).
narrative_ontology:measurement_basis(lhi_proportionality_su_t14, observed).
narrative_ontology:measurement(lhi_proportionality_su_t21, legitimate_health_intervention__proportionality_reading, suppression_requirement, 21, 0.38).
narrative_ontology:measurement_basis(lhi_proportionality_su_t21, observed).
narrative_ontology:measurement(lhi_proportionality_su_t28, legitimate_health_intervention__proportionality_reading, suppression_requirement, 28, 0.4).
narrative_ontology:measurement_basis(lhi_proportionality_su_t28, observed).
narrative_ontology:measurement(lhi_proportionality_su_t31, legitimate_health_intervention__proportionality_reading, suppression_requirement, 31, 0.74).
narrative_ontology:measurement_basis(lhi_proportionality_su_t31, observed).
narrative_ontology:measurement(lhi_proportionality_su_t34, legitimate_health_intervention__proportionality_reading, suppression_requirement, 34, 0.55).
narrative_ontology:measurement_basis(lhi_proportionality_su_t34, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__public_health_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__proportionality_reading, legitimate_health_intervention__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate health intervention' decomposes into three readings of one kernel with materially different epsilon bases and victim sets. This (proportionality) story authors epsilon for the standing proportionality-governed arrangement as this reading's own lights assess it; the public_health_primary story authors epsilon for the same standing arrangement from the outcomes-only seat (lower, since refusal counts as externality), and the bodily_autonomy_primary story authors it from the consent-absolutist seat (higher, since every coerced individual counts as a victim regardless of severity). Upstream/downstream: the proportionality reading draws its threat evidence base from the same epidemiology the public_health reading relies on, while borrowing its autonomy weighting from the bodily_autonomy tradition; each sibling story should link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
