% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Medical Mandate Without Informed Consent (Bodily Autonomy Reading)
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the bodily_autonomy_primary reading of the
 *   mandate_legitimacy_scope kernel. The reading asserts that informed
 *   consent to medical intervention is a foundational right that cannot be
 *   overridden by public health benefit, no matter how significant. When
 *   medical mandates are enforced without genuine informed consent (where
 *   refusal carries severe legal, employment, or social penalties), the state
 *   becomes a rights violator and the constraint functions as pure extraction
 *   of bodily autonomy from those coerced. The unvaccinated-coerced enter the
 *   victim set; disease-vulnerable populations who benefit indirectly from
 *   coerced vaccination enter the beneficiary set. The constraint's
 *   persistence depends on active enforcement — legal penalties, employment
 *   termination, educational exclusion, healthcare access denial — making it
 *   a snare: a pure extraction mechanism using public health framing as
 *   cover, from this reading's perspective.
 *
 * KEY AGENTS:
 *   - unvaccinated_individuals_under_coercion: targets bearing the bodily integrity violation
 *   - public_health_authorities: agenda-setters enforcing the mandate
 *   - disease_vulnerable_populations: indirect beneficiaries of coerced vaccination
 *   - autonomy_rights_bearers_generally: systemic victims of the precedent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.88).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.91).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.88).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Medical Mandate Without Informed Consent (Bodily Autonomy Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'ddf7bb38-ab9e-417d-8827-0686e8d77346').
narrative_ontology:cs_kernel_codification('ddf7bb38-ab9e-417d-8827-0686e8d77346', formalized).
narrative_ontology:cs_authority_grounding('ddf7bb38-ab9e-417d-8827-0686e8d77346', extraction).
narrative_ontology:cs_interpretation_layer_present('ddf7bb38-ab9e-417d-8827-0686e8d77346').
narrative_ontology:cs_reading_relation('ddf7bb38-ab9e-417d-8827-0686e8d77346', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_reading_relation('ddf7bb38-ab9e-417d-8827-0686e8d77346', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('ddf7bb38-ab9e-417d-8827-0686e8d77346', foundational, bodily_integrity_inviolable_deontological).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable_deontological, holdable).
narrative_ontology:cs_axiom_grounding('ddf7bb38-ab9e-417d-8827-0686e8d77346', bodily_integrity_inviolable_deontological, deontological).
narrative_ontology:cs_axiom('ddf7bb38-ab9e-417d-8827-0686e8d77346', foundational, state_coercion_vitiation_of_consent).
narrative_ontology:cs_axiom_status(state_coercion_vitiation_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('ddf7bb38-ab9e-417d-8827-0686e8d77346', state_coercion_vitiation_of_consent, deontological).
narrative_ontology:cs_reference_frame('ddf7bb38-ab9e-417d-8827-0686e8d77346', autonomous_medical_refusal_capacity).
narrative_ontology:cs_drift_state('ddf7bb38-ab9e-417d-8827-0686e8d77346', contemporary_pandemic_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ddf7bb38-ab9e-417d-8827-0686e8d77346', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, disease_vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_individuals_under_coercion).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, autonomy_rights_bearers_generally).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face coercive legal and social pressure to accept medical intervention (vaccination) without informed refusal being treated as a legitimate choice. Their bodily integrity — the foundational right to determine what enters their body — is overridden by state mandate. Exit options are severely constrained: refusing vaccination triggers employment loss, educational exclusion, healthcare access denial, and legal penalties. The coercion operates through structural levers (employment, education, social participation) that make genuine consent impossible.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_individuals_under_coercion, payer,
    powerless, biographical, trapped, national).

% Experience a precedent-setting breach of the principle that bodily integrity is inviolable regardless of collective benefit. Even those who accept vaccination face degradation of the foundational right that no state may compel medical intervention without genuine informed consent. The constraint establishes state authority to override the most fundamental bodily autonomy claim, affecting all future medical mandate contexts.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, autonomy_rights_bearers_generally, payer,
    organized, generational, constrained, national).

% Set and enforce the mandate, treating disease transmission prevention as justification for overriding individual consent requirements. They frame the constraint as necessary emergency power during public health crises. They determine who is exempt, what counts as valid medical reason for refusal, and what penalties attach to non-compliance. They have the power to modify or revoke mandates but treat that power as contingent on epidemiological conditions, not on the foundational autonomy claim itself.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain reduced disease transmission risk when vaccination rates rise. They are framed as the primary beneficiary class whose protection justifies the mandate — immunocompromised individuals, elderly populations, those for whom certain vaccines are medically contraindicated. They benefit from coerced vaccination of others, though they themselves often cannot be vaccinated. This reading treats their benefit as insufficient to override the autonomy violation for those coerced.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, disease_vulnerable_populations, beneficiary,
    powerless, biographical, constrained, national).

% Monitor whether the core principles of medical ethics — that informed consent is non-waivable, that bodily integrity is foundational to all other rights — are preserved or eroded. This includes medical licensing boards, ethics committees, and courts. They see the mandate as a potential precedent that weakens the consent requirement across all medical contexts.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, informed_consent_doctrine_guardians, observer,
    institutional, generational, analytical, national).

% Would argue that mandate legitimacy depends on proportionality testing — weighing disease severity, vaccine efficacy, safety data, and availability of less restrictive alternatives. This reading excludes proportionality from the core autonomy claim, treating bodily integrity as prior to and independent of such balancing. Proportionality adjudicators are systematically excluded from this reading's framework: their voice would reframe the constraint as needing empirical/contextual justification rather than absolute prohibition.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, proportionality_adjudicators, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading treats the mandate as pure extraction, not coordination. A coordination reading would frame vaccination as a collective-action solution to disease transmission; this reading rejects that frame as illegitimate when it requires coerced bodily intervention.
% TRANSFER_FUNCTION: Bodily integrity — the right to refuse medical intervention — is transferred from individuals under coercion to the state (exercised as mandate enforcement) and indirectly to disease-vulnerable populations (who benefit from higher vaccination rates). The transfer is enforced through employment exclusion, educational access denial, healthcare system barriers, and legal penalties.
% ABSENT_VOICES: Proportionality adjudicators are systematically excluded — they would argue for empirical testing of the mandate's necessity and restrictiveness. Individuals with prior infection or natural immunity are often excluded from legitimate refusal categories. Communities with historical medical trauma and distrust are excluded from the consent framework, their absence used to support higher mandates rather than questioned as a design flaw.
% DISAPPEARANCE_RATIONALE: If medical mandates without informed consent disappeared, the foundational principle that bodily integrity cannot be overridden regardless of collective benefit would be restored as a binding legal/ethical constraint. Medical interventions would require genuine informed consent with no-penalty refusal as a live option. Public health authorities would operate under the autonomy constraint rather than treating it as contingent on epidemiological judgment.
% FOUNDING_PROBLEM: Infectious disease transmission poses collective harm; vulnerable populations face elevated risk when vaccination rates fall below critical thresholds. The mandate was framed as necessary to protect those who cannot be vaccinated.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and epidemiologists attest the transmission-prevention problem is live. Medical ethicists, autonomy-rights scholars, and courts in autonomy-protective jurisdictions attest that the founding problem does not override the bodily integrity principle — that there are alternative approaches (voluntary uptake, targeted protection, risk-stratified strategies) that solve the transmission problem without coercion. The corroboration is split across constituencies with incompatible readings.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88 terminal) because the constraint forcibly takes bodily autonomy from those coerced, without their genuine informed consent. Suppression is higher still (0.91) because the constraint's persistence depends on active enforcement machinery — employment law, healthcare access rules, educational policies — that makes refusal prohibitively costly. Theater is low (0.22) because while public health rationales are offered, the core enforcement action is transparently coercive: accept the intervention or lose employment/education/healthcare access. The measurement series tracks extraction and suppression rising over the interval as enforcement infrastructure matures and cultural acceptance of mandates consolidates, while theatrical justification remains relatively constant. The leveled coercion grid shows suppression intensity is highest at the individual level (0.91 terminal) — where the bodily autonomy violation is most direct — and lowest at the structural level (0.70), reflecting the constraint's direct operation on individual medical choice rather than systemic resource allocation. Resistance falls over the interval as collective action capacity erodes from cumulative enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (public health authorities) experiences this as legitimate emergency coordination solving a collective-action problem — vaccination as a public good requiring collective enforcement. The payer seats (individuals coerced, autonomy rights defenders) experience it as a rights violation, extraction of the most fundamental bodily autonomy. The disease-vulnerable populations experience it as protective benefit without themselves bearing the coercion cost. These divergences should compute per-seat: the engine derives directionality from the structural data (beneficiary/victim declarations, exit options, power levels). Public health authorities should compute as beneficiaries with high arbitrage exit (they can modify mandates); unvaccinated-coerced should compute as full targets with trapped exit (employment/education loss makes refusal impossible); disease-vulnerable populations as partial beneficiaries with constrained exit (they gain benefit but cannot opt out of being beneficiaries).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for unvaccinated-coerced: d near 1.0 (full target). They bear the bodily integrity violation, face trapped exit (employment/education/healthcare access loss), operate at powerless level. The constraint extracts from them maximally. Directionality for public_health_authorities: d near 0.0 (beneficiary). They set and administer the constraint, have arbitrage exit (can modify mandates), operate at institutional power level. They collect the effective function — disease reduction — and face minimal direct cost. Directionality for disease-vulnerable_populations: d near 0.5 (partial symmetric). They benefit from coerced vaccination but do not administer it; they have constrained rather than trapped exit. The constraint helps but does not coerce them directly. Directionality for autonomy_rights_bearers_generally: d near 0.8 (high target). They bear the precedent cost of eroded bodily integrity protection across all future medical contexts; they have constrained exit (no-penalty refusal is not a live option); they operate at organized level. Override directionality for public_health_authorities from the derived 0.0 to 0.15 (mild target, not true beneficiary): the constraint's persistence depends on active enforcement, which creates ongoing cost for the authorities; they cannot simply walk away without loss of institutional legitimacy. This override captures the structural reality that maintaining a snare requires continuous defensive work.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading rejects mandatrophy resolution by rejecting the 'proportionality' frame entirely. A mandatrophy analysis would ask: has the founding problem (vulnerable populations face disease risk) persisted or evolved? Are there less restrictive alternatives that solve the transmission problem without bodily coercion? This reading answers: those questions are irrelevant. Bodily integrity cannot be overridden by collective benefit, period. The founding problem's status (live, dead, or contested) does not change the autonomy principle. This reading instantiates what pure extraction looks like when public health framing is applied to bodily intervention: the constraint persists because beneficiary institutions (public health authorities) have power to enforce it and benefit from disease reduction, not because the constraint solves a problem that could not be solved less restrictively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_collective_benefit_foundational_irreducibility,
    'Is the conflict between bodily autonomy and collective disease-prevention benefit a genuine logical contradiction that precludes both readings within a single framework, or a values conflict that permits framework coexistence?',
    'The resolution is not empirical but conceptual. It depends on whether bodily integrity is treated as a deontological right that cannot be overridden (forecloses proportionality) or as a prima facie right that can be weighed against other rights (permits proportionality). Different legal traditions and constitutional framings answer differently. The United States treats bodily integrity as foundational (Cruzan, Roe implied); Canada treats it as weighty but balanceable (Oakes test). No data settles which framing is correct.',
    'If bodily autonomy is foundational (deontological, overrides collective benefit), this reading forecloses proportionality and the constraint is a pure snare. If bodily autonomy is weighty but balanceable, this reading coexists with proportionality, and the constraint''s classification depends on empirical proportionality testing (disease severity, vaccine efficacy, alternatives availability). The classification outcome diverges radically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_vs_collective_benefit_foundational_irreducibility, conceptual, 'Whether bodily autonomy is deontologically foundational or empirically balanceable against collective benefit.').

omega_variable(
    informed_consent_realizability_under_coercion,
    'Can genuine informed consent exist when the cost of refusal includes employment loss, educational exclusion, and healthcare access denial? Or does the coercive context make any ''consent'' nominal?',
    'A philosophical and empirical question: does consent require absence of penalty, or only absence of direct physical force? Legal doctrine varies (Common Law requires merely that choice be voluntary in the absence of duress; some autonomy theorists argue all structural coercion vitiates consent). Post-exit trajectory analysis: if individuals who refuse mandates and exit employment report ex-post regret at having refused, consent may have been inadequately informed; if they report vindication, the coercion was structural but consent was real.',
    'If coercive penalties vitiate consent categorically, the mandate is rights violation (snare). If consent can be informed despite penalties, the mandate might be justified proportionality case (tangled_rope or scaffold). The classification pivots on this axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_realizability_under_coercion, conceptual, 'Whether structural coercion (employment/education/healthcare loss) precludes genuine informed consent.').

omega_variable(
    vulnerable_population_protection_necessity,
    'Are there epidemiologically adequate alternatives to coerced vaccination for protecting disease-vulnerable populations (targeted vaccination, environmental controls, therapeutic options, risk-stratified policies)?',
    'Epidemiological analysis and modeling comparing transmission outcomes under alternative protection strategies: targeted vaccination of vulnerable populations, vaccination of healthcare workers, environmental/hygienic controls, therapeutic development, risk-stratified isolation policies. Data from jurisdictions using different strategies.',
    'If adequate alternatives exist, the mandate fails proportionality even under a balancing reading (not foreclosed, but empirically unjustified). This reading treats alternatives as categorically irrelevant (bodily integrity forecloses the balancing test), but sibling readings depend on demonstrating lack of alternatives. This omega bridges the readings'' different empirical requirements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerable_population_protection_necessity, empirical, 'Whether coerced-population-wide vaccination is empirically necessary or whether alternatives can adequately protect vulnerables.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of refusal capacity in this context primarily structural (legal penalties, employment termination, healthcare access rules are external barriers) or internalized (individuals have fused their identity with vaccination acceptance, fear of social judgment, or internalized the public health framing)?',
    'Post-exit trajectory analysis: if suppression persists after the coercive mechanism is removed (individuals who fled mandatory vaccination jurisdictions still carry vaccine compliance internalized), reclassify toward internalized. If suppression collapses when coercive barriers are lifted, the suppression is structural. Community cohort studies comparing refusal rates and post-decision trajectory between coercive and non-coercive jurisdictions.',
    'If internalized, the effective suppression is higher than the structural measure suggests — the constraint operates through both external coercion and internal capture. If structural, the constraint operates purely through legal/employment/healthcare penalties. The distinction affects exit-option classification and directionality computation for different power atoms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of refusal capacity is structural coercion or internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(mand_tr_t0, observed).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(mand_tr_t6, observed).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.2).
narrative_ontology:measurement_basis(mand_tr_t12, observed).
narrative_ontology:measurement(mand_tr_t18, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 18, 0.21).
narrative_ontology:measurement_basis(mand_tr_t18, observed).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(mand_tr_t24, observed).
narrative_ontology:measurement(mand_tr_t30, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(mand_tr_t30, observed).
narrative_ontology:measurement(mand_tr_t36, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 36, 0.22).
narrative_ontology:measurement_basis(mand_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(mand_be_t0, observed).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 6, 0.76).
narrative_ontology:measurement_basis(mand_be_t6, observed).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.82).
narrative_ontology:measurement_basis(mand_be_t12, observed).
narrative_ontology:measurement(mand_be_t18, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 18, 0.85).
narrative_ontology:measurement_basis(mand_be_t18, observed).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.87).
narrative_ontology:measurement_basis(mand_be_t24, observed).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 30, 0.88).
narrative_ontology:measurement_basis(mand_be_t30, observed).
narrative_ontology:measurement(mand_be_t36, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 36, 0.88).
narrative_ontology:measurement_basis(mand_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.81).
narrative_ontology:measurement_basis(mand_su_t0, observed).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 6, 0.84).
narrative_ontology:measurement_basis(mand_su_t6, observed).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.88).
narrative_ontology:measurement_basis(mand_su_t12, observed).
narrative_ontology:measurement(mand_su_t18, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 18, 0.89).
narrative_ontology:measurement_basis(mand_su_t18, observed).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.9).
narrative_ontology:measurement_basis(mand_su_t24, observed).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 30, 0.91).
narrative_ontology:measurement_basis(mand_su_t30, observed).
narrative_ontology:measurement(mand_su_t36, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 36, 0.91).
narrative_ontology:measurement_basis(mand_su_t36, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=36
narrative_ontology:measurement(mand_grid_01, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(class), 0, 0.74).
narrative_ontology:measurement(mand_grid_02, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(class), 36, 0.78).
narrative_ontology:measurement(mand_grid_03, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(individual), 0, 0.82).
narrative_ontology:measurement(mand_grid_04, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(individual), 36, 0.84).
narrative_ontology:measurement(mand_grid_05, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(mand_grid_06, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(organizational), 36, 0.76).
narrative_ontology:measurement(mand_grid_07, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(mand_grid_08, mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse(structural), 36, 0.72).
narrative_ontology:measurement(mand_grid_09, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(class), 0, 0.74).
narrative_ontology:measurement(mand_grid_10, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(class), 36, 0.71).
narrative_ontology:measurement(mand_grid_11, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(individual), 0, 0.68).
narrative_ontology:measurement(mand_grid_12, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(individual), 36, 0.64).
narrative_ontology:measurement(mand_grid_13, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(organizational), 0, 0.76).
narrative_ontology:measurement(mand_grid_14, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(organizational), 36, 0.72).
narrative_ontology:measurement(mand_grid_15, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(structural), 0, 0.62).
narrative_ontology:measurement(mand_grid_16, mandate_legitimacy_scope__bodily_autonomy_primary, resistance(structural), 36, 0.59).
narrative_ontology:measurement(mand_grid_17, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(mand_grid_18, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(class), 36, 0.74).
narrative_ontology:measurement(mand_grid_19, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(individual), 0, 0.79).
narrative_ontology:measurement(mand_grid_20, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(individual), 36, 0.82).
narrative_ontology:measurement(mand_grid_21, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(organizational), 0, 0.64).
narrative_ontology:measurement(mand_grid_22, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(organizational), 36, 0.68).
narrative_ontology:measurement(mand_grid_23, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(mand_grid_24, mandate_legitimacy_scope__bodily_autonomy_primary, stakes_inflation(structural), 36, 0.61).
narrative_ontology:measurement(mand_grid_25, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(class), 0, 0.81).
narrative_ontology:measurement(mand_grid_26, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(class), 36, 0.84).
narrative_ontology:measurement(mand_grid_27, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(individual), 0, 0.88).
narrative_ontology:measurement(mand_grid_28, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(individual), 36, 0.91).
narrative_ontology:measurement(mand_grid_29, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(organizational), 0, 0.76).
narrative_ontology:measurement(mand_grid_30, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(organizational), 36, 0.79).
narrative_ontology:measurement(mand_grid_31, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(structural), 0, 0.68).
narrative_ontology:measurement(mand_grid_32, mandate_legitimacy_scope__bodily_autonomy_primary, suppression(structural), 36, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).

% DUAL FORMULATION NOTE:
% mandate_legitimacy_scope kernel has three sibling readings: bodily_autonomy_primary (this constraint) asserts bodily integrity cannot be overridden; proportionality_reading applies balancing test to mandate necessity; public_health_primary asserts state authority is legitimate when needed to protect vulnerables. These are THREE SEPARATE CONSTRAINTS with different epsilon values, beneficiary/victim structures, and classifications. The bodily_autonomy_primary reading has highest epsilon (0.88 terminal) because it treats all coerced vaccination as rights violation. The proportionality_reading would have lower epsilon when alternatives adequately protect vulnerables. The public_health_primary reading would have lowest epsilon when disease severity and vaccine efficacy are high and alternatives are unavailable. Each reading instantiates one constraint; the kernel_id links them as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
