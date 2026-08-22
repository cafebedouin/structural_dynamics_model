% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy: Bodily Autonomy Primacy Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint models ONE READING of the contested kernel
 *   'vaccine_mandate_legitimacy': the bodily autonomy primacy reading, which
 *   asserts that medical self-sovereignty is categorically prior to
 *   collective-harm justifications, and that state coercion in medical
 *   contexts is fundamentally illegitimate regardless of epidemiological
 *   outcomes. The constraint is authored from this reading's structural
 *   position: it characterizes what mandate coercion looks like when a party
 *   rejects the public health authority's coordinating-function framing and
 *   reads the mandates instead as pure extraction. This reading coexists with
 *   competing readings (public_health_primacy and risk_stratification) held
 *   by different parties — none rules the others out within their own
 *   frameworks; the contest is empirical and political, not logically
 *   resolved. The authored metrics describe mandate operation under this
 *   reading's interpretive lens: extractiveness is high because coercion
 *   overrides individual choice; suppression is higher because enforcement
 *   depends on institutional gatekeeping and restricting alternatives;
 *   theater is lower (more functional than performative) because mandate
 *   enforcement serves a real coordination objective from the public health
 *   seat, even if that objective is categorically rejected by this reading.
 *
 * KEY AGENTS:
 *   - mandate_coerced_medical_subjects: individuals facing employment loss, educational exclusion, or medical access restrictions conditional on vaccination acceptance; trapped with no legitimate exit
 *   - immunocompromised_vulnerable_populations: bearing residual transmission risk in a framework that rejects collective protection as legitimate; excluded from the negotiation space the autonomy-primacy reading sustains
 *   - medical_liberty_advocacy_movements: beneficiaries of the constraint's existence — mandates generate the grievance constituency and fundraising base they mobilize around
 *   - public_health_authorities: agenda-setters deploying state power; read by this constraint as the coercive seat, not as coordinators solving a collective-action problem
 *   - constitutional_court_system: observer seat; decides whether autonomy rights categorically bar mandate authority or whether collective harm permits state intervention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.88).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.91).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, snare).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Vaccine Mandate Legitimacy: Bodily Autonomy Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'ab5f9b1d-410f-46bb-a06b-4fbe293e9d19').
narrative_ontology:cs_kernel_codification('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', distributed).
narrative_ontology:cs_authority_grounding('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', extraction).
narrative_ontology:cs_interpretation_layer_present('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19').
narrative_ontology:cs_reading_relation('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', foundational, bodily_integrity_categorically_prior).
narrative_ontology:cs_axiom_status(bodily_integrity_categorically_prior, holdable).
narrative_ontology:cs_axiom_grounding('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', bodily_integrity_categorically_prior, deontological).
narrative_ontology:cs_axiom('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', foundational, state_coercion_medical_context_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_coercion_medical_context_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', state_coercion_medical_context_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', individual_medical_autonomy_primacy).
narrative_ontology:cs_drift_state('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', contemporary_mandate_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab5f9b1d-410f-46bb-a06b-4fbe293e9d19', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_individuals_by_choice).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mandate_coerced_medical_subjects).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, those_bearing_residual_transmission_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_profession).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_integrity_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, categorical_autonomy_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face loss of employment, educational access, medical care, or freedom of movement conditional on accepting a medical intervention they have declined. The coercion operates through institutional gatekeeping: employers require proof, schools deny enrollment, hospitals restrict visitation. Refusal results in economic isolation or forced separation from essential services. No legitimate exit exists from the constraint itself — only submission or loss of livelihood/access.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mandate_coerced_medical_subjects, payer,
    powerless, biographical, trapped, national).

% Bear disproportionate residual transmission risk and severity outcomes from COVID-19. Under this reading, they are positioned as victims because mandate framers who reject collective health as a legitimate coercive basis render these vulnerable populations without protective coordination — they cannot be protected through mandate legitimacy in the autonomy-primacy frame, yet benefit less from voluntary vaccination campaigns. Their biological vulnerability becomes a burden without institutional remedy within this constraint's logic.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_vulnerable_populations, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_vulnerable_populations, excluded).

% Face ongoing transmission risk in situations where unvaccinated individuals remain present. Under this reading, their risk is authored as a cost they must bear individually rather than addressed through collective action, because the reading rejects collective-harm mitigation as a legitimate mandate basis. They carry the health externality of the unvaccinated choice but cannot invoke collective duty to address it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, those_bearing_residual_transmission_risk, payer,
    powerless, biographical, constrained, national).

% Gain institutional legitimacy, fundraising, and political mobilization capacity from the constraint's enforcement. Mandates create the grievance constituency and the rhetorical platform for these movements. They benefit from the coercion's existence — it generates the narrative of infringement they organize around. They do not run the mandate system, but they benefit from its persistence by maintaining its status as a contested domain.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_liberty_advocacy_movements, beneficiary,
    moderate, biographical, mobile, national).

% Receive rhetorical and legal support from the autonomy-primacy reading, which frames their choice as a fundamental right rather than epidemiological risk. Some benefit from legal challenges that prevent or delay mandate enforcement. However, they also bear the direct costs of the constraint (loss of access, employment) — their beneficiary status comes through the reading's legitimacy claim, not through actual protection by the constraint. Their benefit is vindication of their choice-right, not escape from enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_individuals_by_choice, beneficiary,
    moderate, biographical, constrained, national).

% Set and enforce vaccine requirements through licensing, employment, institutional gatekeeping, and regulatory authority. They justify mandates through collective-harm prevention and disease elimination rationales. Under this reading, they are the coercive seat — they deploy state power to impose a medical intervention on unwilling subjects. Their authority to set these terms is precisely what the autonomy-primacy reading contests as categorically illegitimate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Adjudicates whether mandates survive challenge under constitutional protections of bodily integrity, freedom from forced medical intervention, and personal liberty. Takes testimony from public health authorities (collective-harm prevention), mandate-coerced subjects (autonomy infringement), and affected vulnerable populations. Interprets whether state police power for health includes mandate authority or whether bodily autonomy is a categorical bar to coercive medical intervention.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_court_system, observer,
    institutional, generational, analytical, national).

% Occupies a divided position: medical ethics doctrine (autonomy + informed consent) conflicts with epidemiological rationales for mandates. Under this reading, clinicians are bound to respect patient autonomy as a foundational ethical principle; mandates create a structurally incoherent position where they are agents of coercion. They benefit from institutional backup for mandate enforcement but face professional-identity conflict between their autonomy-respecting oath and their enforcement role.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_profession, observer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medical_profession, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading rejects the framing of mandates as a coordination mechanism solving a collective-action problem. The autonomy-primacy reading asserts that even if vaccination created positive externalities, those externalities cannot justify overriding individual medical self-determination. There is no coordination to solve — only an attempted coercive redistribution of medical choice.
% TRANSFER_FUNCTION: Transfers bodily integrity and medical autonomy from individuals to state-backed public health authorities; transfers risk and protective infrastructure decisions from vulnerable populations to mandate enforcement regimes; transfers legitimacy and mobilization capacity to liberty advocacy movements that contest the mandate; transfers enforcement power to institutional gatekeepers (employers, schools, hospitals) that become proxy agents of the mandate.
% ABSENT_VOICES: Immunocompromised and high-risk individuals who would benefit from collective disease prevention but are absent from the autonomy-primacy reading's negotiation space — the reading's logic does not include them as parties whose interests justify collective action. Vaccine-injured individuals whose harms fall outside mandate justification narratives. Healthcare workers and teachers facing dual pressure (workplace mandates + occupational exposure risk) without a framework that addresses both. Jurisdictions in low-vaccination regions where the mandate's enforcement mechanisms fail and transmission risk remains despite the coercion.
% DISAPPEARANCE_RATIONALE: If the mandate constraint disappeared — if state-backed medical coercion ended and vaccination became entirely voluntary — the world reorganizes around individual choice: vaccination rates fall among those coerced into compliance, transmission patterns shift, vulnerable populations face unmitigated risk exposure, healthcare systems adjust surge capacity expectations, and medical liberty advocacy movements lose their central grievance. The constraint's disappearance is simultaneously a victory for autonomy advocates and a reconfiguration of epidemiological risk distribution toward the vulnerable.
% FOUNDING_PROBLEM: The state's attempt to deploy collective-harm prevention as justification for overriding individual bodily autonomy — the subordination of medical self-determination to epidemiological utility calculations. The founding problem is not disease transmission; it is the legitimacy of state power to mandate medical intervention.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and medical ethicists grounded in autonomy doctrine (outside public health authority) attest the founding problem is live: every vaccine mandate raises the structural question of whether collective harm justifies individual coercion. Public health authorities and epidemiologists contest this framing — they assert the founding problem is misidentified (the real problem is disease transmission, not mandate legitimacy). Liberty advocacy movements and vaccine-injured advocacy groups attest the founding problem is live and urgent. No corroboration from public health beneficiaries — the authorities whose mandates this reading contests cannot credibly attest to a problem whose existence they deny.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.68 → 0.88) as mandate enforcement shifts from initial guidance to widespread institutional gatekeeping: employment verification becomes routine, institutional exclusion deepens, and the coercive apparatus matures. Suppression follows the same trajectory (0.73 → 0.91) because persistence depends on sustained institutional enforcement — alternatives (working unvaccinated, attending unvaccinated, traveling unvaccinated) become progressively unavailable. Theater remains low (~0.22) because mandate enforcement is functionally organized around disease control objectives, even when those objectives are rejected as illegitimate by this reading. The constraint's metrics are descriptively true of mandate operation; the claimed_type (snare) reflects the reading's structural assessment: what began as public health coordination becomes enforced extraction when the coercive mechanism is stripped of legitimacy by the autonomy-primacy frame. The divergence between claimed type and any public health seat's assessment is NOT an error — it is the measurement the corpus exists to take: the same institutional practice is read as coordination from one seat and as extraction from another.
 *
 * PERSPECTIVAL GAP:
 *   The autonomy-primacy reading and the public_health_primacy reading are incommensurable from within each framework: they disagree about what 'harm' means and whether collective welfare is a legitimate coercive basis. From the public health seat, mandates are coordination (solving the externality problem of disease transmission); from the autonomy-primacy seat, they are extraction (overriding choice for collective benefit). The engine computes per-seat classifications from structural data — these two seats will classify the same constraint as rope/tangled_rope (from public health authority's position) and snare (from mandate-coerced subjects' position under the autonomy-primacy reading). That divergence is structural, not an error in authoring or in the engine's computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Mandate-coerced subjects face d ≈ 1.0 (full targets): they bear the direct coercive cost (employment loss, medical access denial) with no offsetting benefit in this reading's frame. Their exit options are trapped by institutional gatekeeping — alternatives are not merely constrained, they are actively foreclosed by the enforcement machinery. Immunocompromised populations are victims (d ≈ 0.9) who receive neither protection nor consent: they cannot invoke collective duty as a legitimate mandate basis under this reading, so they are simultaneously exposed to residual risk and excluded from the framework's negotiation space. Medical liberty advocacy movements are beneficiaries (d ≈ 0.1): they collect organizational resources and political power from the constraint's existence without bearing the enforcement costs directly. Public health authorities sit at the asymmetric enforcement seat: they are the agenda-setters (collectors of mandate authority, deployers of coercion), but their d is complex — they may genuinely believe collective harm justifies action, yet from this reading's structural position they are extractors. Constitutional courts are analysts (d ≈ 0.5): they bear the burden of interpreting whether autonomy is categorical or conditional, but they do not collect extraction rents. The directionality overrides below correct for institutional positions that the automatic derivation misses.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy-primacy reading rejects the entire public health justification for mandates, so it denies that a founding problem (disease transmission) ever legitimated the constraint. From this reading's perspective, the founding problem is state coercion itself, not epidemiology. The founding_problem_status is 'live' because the legitimacy question remains contested and unresolved: no court has ruled that state medical coercion is categorically impermissible, and public health authorities continue to assert mandate authority. The disappearance_verdict is 'world_rearranges' because mandate removal would reorganize both epidemiological risk distribution (toward vulnerable populations) and political alignment (liberty advocacy movements lose their central grievance). This is not a case of mandatrophy in the classical sense (constraint persists despite founding problem being solved); it is a constraint whose founding problem — the legitimacy of state medical coercion — is itself one of the two contested positions within this kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_autonomy_coherence_limit,
    'Is the categorical autonomy claim (state coercion always impermissible in medical contexts) logically sustainable when collective harm reaches extreme severity, or do coherence pressures force admission of exceptions?',
    'Logical analysis under limiting cases: 99%-mortality pandemic scenario, mandatory vaccination for smallpox eradication campaign, airborne Ebola containment. Does the reading''s axiom remain coherent without exceptions, or does it require caveating to ''state coercion presumptively impermissible, except when...''?',
    'If categorical claim remains coherent without exceptions, the axiom is foundational and forecloses public_health_primacy_reading (they cannot both be true in the same framework). If exceptions emerge under limiting cases, the readings coexist_with one another, and the dispute is empirical (how severe must harm be?) rather than logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_autonomy_coherence_limit, conceptual, 'Whether bodily autonomy supremacy is unconditional or admits coherence-limiting exceptions.').

omega_variable(
    institutional_beneficiary_identity_ambiguity,
    'Do medical liberty advocacy movements genuinely represent mandate-affected populations'' interests, or do they benefit from the constraint''s existence in ways that may diverge from the people''s actual preferences?',
    'Survey data on alignment between mandate-affected persons'' stated priorities (keep job while declining mandate, access healthcare without proof, etc.) and advocacy movement leadership priorities (defeat mandate authority, establish medical freedom doctrine, etc.). Resource flow analysis during high-enforcement periods.',
    'High divergence suggests the identified beneficiaries may be extracting organizational/political gain from the coercion without solving the constraint for those coerced — pointing toward a more complex constraint structure where multiple parties benefit from mandate persistence for different reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_identity_ambiguity, empirical, 'Whether advocacy movement beneficiaries align with affected populations'' actual needs.').

omega_variable(
    vulnerable_population_protection_gap,
    'In a mandate-free autonomy-primacy regime, what non-coercive mechanisms protect immunocompromised and high-risk individuals from disease harm?',
    'Design and costability analysis of voluntary high-risk support systems (targeted outreach, insurance mechanisms, community care agreements, voluntary collective vaccination agreements). Comparative health outcome projections under mandate vs. voluntary regimes for vulnerable populations.',
    'If robust non-coercive alternatives exist and achieve comparable outcomes, the reading''s framework is internally coherent — autonomy can be maintained while protecting the vulnerable. If no adequate alternatives exist, the reading systematically disadvantages the most vulnerable, clarifying the cost distribution of the autonomy-primacy position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_protection_gap, empirical, 'Availability of protective mechanisms for vulnerable populations under autonomy-primacy regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t4, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement_basis(vacc_tr_t16, observed).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(vacc_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 4, 0.75).
narrative_ontology:measurement_basis(vacc_be_t4, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 8, 0.82).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.86).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 16, 0.87).
narrative_ontology:measurement_basis(vacc_be_t16, observed).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement_basis(vacc_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.73).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 4, 0.79).
narrative_ontology:measurement_basis(vacc_su_t4, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 8, 0.85).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.88).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 16, 0.9).
narrative_ontology:measurement_basis(vacc_su_t16, observed).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.91).
narrative_ontology:measurement_basis(vacc_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel vaccine_mandate_legitimacy. The bodily_autonomy_primacy_reading (this file) characterizes mandate coercion from the standpoint of those who reject collective-harm justification as legitimate. The public_health_primacy_reading characterizes the same institutional practice as coordination solving a genuine collective-action problem. The risk_stratification_reading characterizes mandates as legitimate only when grounded in actuarial risk assessment. These are not different views of the same constraint — they are three structurally distinct constraints instantiated from the same kernel. Each has its own ε, its own victims/beneficiaries, its own type classification. The network links document the kernel structure; each reading's ε-invariance is preserved within its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
