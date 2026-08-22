% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Authority (Public Health Primacy Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   A state authority mandates vaccination for access to employment,
 *   education, healthcare, and public gathering. This constraint story
 *   instantiates the PUBLIC HEALTH PRIMACY READING of the vaccine mandate
 *   kernel: the reading that justifies the mandate by treating unvaccinated
 *   status as a negative externality and asserting the state's duty to
 *   prevent collective harm overrides individual medical self-determination.
 *   Under this reading, unvaccinated people are victims (targets of
 *   suppression) and the public health bureaucracy is the
 *   agenda-setter/beneficiary. The constraint is authored as tangled_rope
 *   because it possesses both coordination (high-coverage vaccination) and
 *   asymmetric extraction (suppression costs borne by one party). This is
 *   deliberately NOT a claim that the mandate is correct or incorrect — it is
 *   a structural analysis of what this particular reading instantiates.
 *
 * KEY AGENTS:
 *   - Public health bureaucracy: sets and enforces mandate; expands authority over medical decision-making in the name of collective safety.
 *   - Vaccine refusers: classified as externality under this reading; subject to employment, educational, healthcare, and social exclusion.
 *   - Vaccinated population: receives coordination benefit (reduced transmission) and political cover; pays diffuse enforcement cost.
 *   - Medical professionals: excluded from individualized risk assessment; their judgment is subordinated to epidemiological aggregates.
 *   - Constitutional courts: observe and adjudicate whether state duty overrides bodily autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.72).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.81).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Authority (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'aa669f64-4b61-4812-a3e5-8c46922ff9b8').
narrative_ontology:cs_kernel_codification('aa669f64-4b61-4812-a3e5-8c46922ff9b8', formalized).
narrative_ontology:cs_authority_grounding('aa669f64-4b61-4812-a3e5-8c46922ff9b8', extraction).
narrative_ontology:cs_interpretation_layer_present('aa669f64-4b61-4812-a3e5-8c46922ff9b8').
narrative_ontology:cs_reading_relation('aa669f64-4b61-4812-a3e5-8c46922ff9b8', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('aa669f64-4b61-4812-a3e5-8c46922ff9b8', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('aa669f64-4b61-4812-a3e5-8c46922ff9b8', foundational, state_duty_collective_harm_prevention).
narrative_ontology:cs_axiom_status(state_duty_collective_harm_prevention, holdable).
narrative_ontology:cs_axiom_grounding('aa669f64-4b61-4812-a3e5-8c46922ff9b8', state_duty_collective_harm_prevention, deontological).
narrative_ontology:cs_axiom('aa669f64-4b61-4812-a3e5-8c46922ff9b8', foundational, individual_autonomy_overridable_by_externality).
narrative_ontology:cs_axiom_status(individual_autonomy_overridable_by_externality, holdable).
narrative_ontology:cs_axiom_grounding('aa669f64-4b61-4812-a3e5-8c46922ff9b8', individual_autonomy_overridable_by_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('aa669f64-4b61-4812-a3e5-8c46922ff9b8', state_collective_safety_authority).
narrative_ontology:cs_drift_state('aa669f64-4b61-4812-a3e5-8c46922ff9b8', post_endemic_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa669f64-4b61-4812-a3e5-8c46922ff9b8', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces vaccine mandate policy, justifying it as prevention of externality-driven collective harm. Possesses authority to exclude refusers from employment, education, healthcare access, and public gathering spaces. Frames unvaccinated status as an epidemiological threat to others and expands its jurisdiction over medical decision-making in the name of public safety.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from reduced infection transmission and restored access to social/economic activity. Carries diffuse indirect cost through enforcement machinery and social friction. Majority opinion supports mandate under this reading, providing political cover for enforcement.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_population, payer).

% Subject to employment loss, educational exclusion, healthcare access restrictions, and social stigmatization. Under this reading, their refusal is reframed as a negative externality, placing suppression costs entirely on them. Exit from the constraint (maintaining bodily autonomy) requires abandoning access to employment, education, and healthcare — costs they bear alone. Resistance is characterized as selfish disregard for public health.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_refusers, payer,
    powerless, biographical, identity_locked, national).

% Whose professional autonomy to assess individual risk and recommend individualized treatment is constrained by mandate enforcement. Under this reading, their clinical judgment is subordinated to epidemiological aggregate statistics. They are excluded from the policy conversation about whether mandates constitute the appropriate medical intervention given individual patient variation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, medical_professionals, excluded,
    powerful, biographical, constrained, national).

% Argue for age-stratified, comorbidity-adjusted, prior-infection-adjusted mandate thresholds rather than blanket requirements. Under this reading, their arguments are treated as obstacles to public health coordination and are systematically excluded from policy design. They would argue mandates must track actuarial risk to be legitimate; that argument is foreclosed by the reading.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, risk_stratification_advocates, excluded,
    moderate, biographical, constrained, national).

% Receive challenges to mandate authority grounded in bodily autonomy and proportionality doctrines. Under this reading, they adjudicate whether state duty to prevent collective harm overrides individual medical self-determination. Their decisions either affirm or constrain the bureaucracy's authority.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_bureaucracy).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Achieves high population vaccination coverage through legal mandate, coordination on a single epidemiological standard applied uniformly, preventing fragmentation into individual-choice-driven immunity gaps that create sustained transmission reservoirs.
% TRANSFER_FUNCTION: Moves medical self-determination authority from individuals to the state, transfers the cost of non-compliance (employment loss, educational exclusion, healthcare access restriction) from the public health system to vaccine refusers, and transfers legitimacy authority to epidemiological aggregate risk rather than individual patient risk assessment.
% ABSENT_VOICES: Medical professionals trained in individualized risk assessment; patients with prior infection or comorbidity-specific contraindications; constitutional lawyers trained in bodily autonomy doctrine; epidemiologists who argue risk stratification would achieve equivalent public health outcomes at lower autonomy cost. Under this reading, their objections are characterized as externality-ignoring and are kept out of mandate design.
% DISAPPEARANCE_RATIONALE: If mandate authority and its enforcement infrastructure vanished overnight, vaccination coverage would drop to voluntary-uptake levels (estimated 60–75% depending on risk group). Transmission patterns would reorganize; high-risk populations would face renewed incentive to vaccinate individually; healthcare systems would shift to treating infection rather than preventing it through mandate. The state's exercise of expansive medical authority would terminate.
% FOUNDING_PROBLEM: A novel pathogen threatened mass mortality; vaccines offered protection but required high population coverage to prevent variant emergence and healthcare system collapse. Individual choice led to coverage gaps below collective-safety thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities attest the founding problem is live and mandates are the only solution that achieves required coverage. Epidemiological models from outside the benefiting parties show that risk-stratified mandates or voluntary+targeted-incentive campaigns could achieve equivalent outcomes; constitutional scholars and medical ethicists from outside the bureaucracy attest that the founding problem does not justify the breadth of authority claimed. Legislative records from multiple jurisdictions show no debate on alternative approaches meeting the same epidemiological threshold — the founding problem is invoked but the range of legitimate solutions was pre-narrowed.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.72 because the mandate transfers medical decision-making authority to the state without compensating refusers for the loss, and because the authority structure expands beyond what the founding epidemiological problem requires. Suppression is 0.81 because persistence depends on active enforcement (employment screening, education exclusion, healthcare access barriers, social stigmatization) — refusers cannot exit without catastrophic cost. Theater ratio is 0.28: the coordination function (high-coverage vaccination) is real, but enforcement includes substantial performative elements (mandates applied uniformly regardless of individual risk, public vilification of refusers, maintenance of authority rather than problem-solving). The temporal series shows extractiveness rising from 0.45 to peak at 0.72 by timepoint 18 as enforcement machinery solidifies and authority claims expand, then stabilizing — indicating that the constraint reaches a plateau where the coordination need is met but extraction remains because exit costs are now locked in. Suppression requirement rises sharply from 0.58 to 0.81 by timepoint 12, showing the bureaucracy must invest increasing enforcement effort to maintain refusal rates below politically destabilizing levels. Theater ratio peaks at 0.29 around timepoint 18, when visible enforcement intensity is highest, then drifts slightly lower as enforcement becomes routinized.
 *
 * PERSPECTIVAL GAP:
 *   The public health bureaucracy seat and the vaccine refuser seat compute radically different types from the same constraint. From the bureaucracy's seat: genuine coordination against a collective-action problem (high beneficiary status, legitimate authority expansion, d~0.15). From the refuser seat: coerced extraction using epidemiology as cover (high target status, suppression locked in by identity/employment fusion, d~0.95). The engine computes both seats from the structural data; the authored claim does not resolve the gap. This gap IS the mandatrophy signal: a constraint whose claim is coordination but whose metrics describe asymmetric extraction with substantial suppression cost borne by one party.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health bureaucracy: benefits directly (gains authority, expands jurisdiction), low exit options (captures the policy apparatus), institutional power = d~0.10 (full beneficiary). Vaccine refusers: pay the suppression cost (employment, education, healthcare, social access), identity-locked exit (refusing vaccination is identity-constituted; exit means abandoning self-concept), powerless position = d~0.92 (near-full target). Vaccinated population: genuine coordination benefit (reduced transmission), but also diffuse cost bearer (enforcement taxation, social friction) = d~0.48 (near symmetric). The asymmetry is structural: beneficiary and target are at opposite poles; vaccination status becomes a binary that sorts agents into beneficiary and victim categories with no middle ground.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading instantiates mandatrophy between founding problem and current enforcement. The founding problem is real: a novel pathogen threatened mass mortality and vaccines offered protection at scale. The founding solution was mandate authority to achieve high coverage when individual choice would produce coverage gaps. But by timepoint 18 of the measurement interval, the founding problem (whether voluntary + incentive approaches can meet epidemiological thresholds) is essentially resolved — modeling and real-world experience from multiple jurisdictions show risk-stratified or voluntary+targeted-incentive approaches achieve 75–80% coverage, meeting equivalent public health endpoints. Yet mandate enforcement persists and extracts increasingly from refusers (employment loss, educational exclusion, healthcare access denial) beyond what the epidemiological problem requires. The authority initially built to solve a collective-action problem now persists to maintain bureaucratic scope. Theater ratio traces this: early enforcement is functionally directed at coverage (theater is low), but by interval end, much enforcement is performative (maintaining authority visibility, punishing refusal as defiance rather than problem-solving). The constraint exhibits the piton/mandatrophy signature: founding problem dead or substantially solved, but enforcement persists because the cost to fix (bureaucratic authority reduction, social reconciliation) exceeds what any single seat bears from maintaining it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_vs_autonomy_framing,
    'Is unvaccinated status structurally an externality (a harm imposed on others) or a difference in risk preference that coexists in a pluralist regime?',
    'Epidemiological data on transmission risk from vaccinated vs. unvaccinated in different risk strata; comparison of outcome trajectories in jurisdictions using risk-stratified mandates vs. blanket mandates; counterfactual modeling of what coverage rate would achieve equivalent public health endpoints through voluntary+incentive rather than coercive pathways.',
    'If externality framing is empirically sustained, the mandate is coordination against a genuine collective-action problem and the suppression cost is the legitimate price of internalization. If the externality is artificially broad (classifying all refusal as threat regardless of individual risk), the constraint shifts toward snare — extraction justified by mislabeled externality. If outcomes are achievable through non-coercive means, the mandate is performative authority expansion rather than necessary coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_vs_autonomy_framing, empirical, 'Whether unvaccinated status is a genuine externality or a preference heterogeneity misclassified as externality to justify authority expansion.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.81) structural (legal barriers, employment loss, healthcare access restriction) or internalized (vaccine refusers have internalized shame, medical mistrust, social isolation as permanent states)?',
    'Post-mandate-lift trajectory: if refusers retain vaccine hesitancy and social isolation after legal enforcement ends, the suppression is internalized; if hesitancy drops and social reintegration occurs, suppression was primarily structural. Qualitative interviews with refusers in different enforcement contexts (high vs. low enforcement intensity, high vs. low social stigma).',
    'If suppression is primarily structural, removal of the mandate removes the constraint. If suppression is internalized, the constraint persists through psychological and social mechanisms even after legal enforcement ceases — the target carries the suppression with them. This changes the classification trajectory and the forecasting model for mandate drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural enforcement or internalized identity/shame.').

omega_variable(
    alternative_coordination_designs_foreclosed,
    'Under this reading, are risk-stratified mandates (age/comorbidity/prior-infection adjusted) logically foreclosed, or merely politically deferred?',
    'Review of policy design literature and epidemiological modeling: do risk-stratified approaches achieve equivalent public health endpoints? Are they rejected on principled grounds or convenience? Analysis of how this reading''s core axiom (state duty to prevent harm overrides individual autonomy) would apply to stratified vs. blanket mandates.',
    'If risk stratification is foreclosed by the axiom itself (broad state authority overrides individual choice regardless of risk), the reading is stable. If risk stratification meets the axiom''s criteria but was never considered, the reading reflects authority expansion beyond the founding problem. If risk stratification is epidemiologically adequate but rejected for political reasons, the mandate is tangled_rope with substantial extractive overlay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_designs_foreclosed, conceptual, 'Whether the reading logically requires blanket mandates or permits stratified approaches.').

omega_variable(
    kernel_reading_contest,
    'This is one reading of the vaccine mandate legitimacy kernel; the sibling readings (bodily autonomy primacy, risk stratification) are structurally incompatible claims about what justifies the mandate. How should the framework adjudicate between readings when both are coherent but mutually exclusive?',
    'The engine does not adjudicate between readings — it models each as a clean constraint under its own epistemic commitments. The corpus of three readings (this one, autonomy, stratification) creates a triplet: same kernel, three ε values, three beneficiary/victim structures. The manifest of which readings exist in a jurisdiction, which are enforced, and how the enforced reading''s outputs diverge from the measured reality is the data the framework collects.',
    'No impact on this reading''s classification — each reading is generated independently. The triplet''s power is in showing HOW the same institutional arrangement (vaccine mandate law) instantiates different constraints depending on which reading the authority structure endorses. This reading''s extracted classification is independent of the siblings''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading indeterminacy: the same mandate text instantiates different constraints under different readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(vacc_tr_t4, observed).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(vacc_tr_t8, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t18, observed).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(vacc_tr_t24, observed).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(vacc_tr_t30, observed).
narrative_ontology:measurement(vacc_tr_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 36, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(vacc_be_t4, observed).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(vacc_be_t8, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement_basis(vacc_be_t18, observed).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement_basis(vacc_be_t24, observed).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(vacc_be_t30, observed).
narrative_ontology:measurement(vacc_be_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 36, 0.72).
narrative_ontology:measurement_basis(vacc_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 4, 0.68).
narrative_ontology:measurement_basis(vacc_su_t4, observed).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement_basis(vacc_su_t8, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 12, 0.81).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 18, 0.82).
narrative_ontology:measurement_basis(vacc_su_t18, observed).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement_basis(vacc_su_t24, observed).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(vacc_su_t30, observed).
narrative_ontology:measurement(vacc_su_t36, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 36, 0.81).
narrative_ontology:measurement_basis(vacc_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The vaccine mandate legitimacy kernel decomposes into three structurally distinct readings with different ε values and beneficiary/victim configurations. The public_health_primacy_reading treats unvaccinated status as externality; the bodily_autonomy_primacy_reading treats mandate authority itself as the harm; the risk_stratification_reading separates legitimate (targeted) from illegitimate (blanket) mandates. All three readings interpret the same kernel text but derive different constraints. Upstream: this reading is downstream of epidemiological consensus on vaccine efficacy and transmission reduction. Downstream: this reading constrains the risk_stratification_reading by establishing that blanket authority is the legitimate baseline; the autonomy reading coexists by rejecting the premise entirely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__public_health_primacy_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
