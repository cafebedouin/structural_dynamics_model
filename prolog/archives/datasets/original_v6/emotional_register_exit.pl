% ============================================================================
% CONSTRAINT STORY: emotional_register_exit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emotional_register_exit, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emotional_register_exit
 *   human_readable: Emotional Register Exit in Institutional Communication
 *   domain: healthcare_systems/organizational_behavior/institutional_communication
 *
 * SUMMARY:
 *   The emotional register exit is a communication pattern in institutional
 *   settings (healthcare, customer service, educational administration) where
 *   front-line workers shift from falsifiable technical claims to
 *   unfalsifiable emotional framing when losing a dispute on technical
 *   merits. A patient objects that their medication dosage is incorrect based
 *   on lab values; the provider responds 'I hear that you're feeling anxious
 *   about your care.' A customer points out that a billing error violates the
 *   service contract; the representative responds 'I understand this has been
 *   frustrating for you.' The register shift performs empathy while
 *   terminating the technical dispute. This constraint exhibits the full
 *   indexical range: pure extraction from the trapped patient's perspective
 *   (Snare), mixed coordination-extraction from the constrained advocate's
 *   perspective (Tangled Rope), coordination from the institutional worker's
 *   perspective (Rope — it protects them from disputes they cannot resolve
 *   due to systemic failures), and degraded coordination from the
 *   civilizational perspective (Piton — patient-centered care training has
 *   atrophied into a deflection script). The constraint is downstream of
 *   physician_call_reluctance (mountain) — the emotional register exit is one
 *   institutional adaptation to the structural fact that physicians cannot be
 *   compelled to engage with technical disputes, so front-line workers are
 *   trained to convert technical disputes into emotional interactions that do
 *   not require physician involvement.
 *
 * KEY AGENTS:
 *   - Patient with Legitimate Technical Claim: Primary victim (powerless/trapped) — cannot force return to technical register; bears full cost of unresolved technical failure plus emotional labor burden of being framed as 'difficult'
 *   - Patient Advocate with Resources: Secondary victim (moderate/constrained) — can escalate or exit at cost; experiences both coordination (some emotional needs are real) and extraction (register shift often deflects from accountability)
 *   - Institutional Front-Line Worker: Primary beneficiary (institutional/arbitrage) — the register shift provides a trained escape route from technical disputes the institution has not equipped them to resolve; protects their emotional resources
 *   - Institutional Management: Secondary beneficiary (institutional/arbitrage) — the register shift converts technical failures (legal/regulatory exposure) into emotional conflicts (unfalsifiable, unactionable); functions as liability shield
 *   - Healthcare Quality Improvement Coalition: Organized observer (organized/mobile) — sees both coordination (genuine emotional care) and extraction (suppression of technical accountability signals needed for quality improvement)
 *   - Patient-Centered Care Framework: Institutional framework (institutional/analytical) — originally designed to address empathy deficits, now degraded into performative deflection script (Piton)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees mixed coordination-extraction structure; confirms Tangled Rope classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotional_register_exit, 0.48).
domain_priors:suppression_score(emotional_register_exit, 0.62).
domain_priors:theater_ratio(emotional_register_exit, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotional_register_exit, extractiveness, 0.48).
narrative_ontology:constraint_metric(emotional_register_exit, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(emotional_register_exit, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(emotional_register_exit, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(emotional_register_exit, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotional_register_exit, tangled_rope).
narrative_ontology:human_readable(emotional_register_exit, "Emotional Register Exit in Institutional Communication").
narrative_ontology:topic_domain(emotional_register_exit, "healthcare_systems/organizational_behavior/institutional_communication").

domain_priors:requires_active_enforcement(emotional_register_exit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotional_register_exit, institutional_front_line_workers).
narrative_ontology:constraint_beneficiary(emotional_register_exit, institutional_management).
narrative_ontology:constraint_victim(emotional_register_exit, patients_with_legitimate_technical_claims).
narrative_ontology:constraint_victim(emotional_register_exit, customers_with_legitimate_technical_claims).
narrative_ontology:constraint_victim(emotional_register_exit, institutional_accountability_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT WITH LEGITIMATE TECHNICAL CLAIM (SNARE) — Trapped in immediate context with no exit. When the institution shifts to emotional register after technical objection, the patient cannot force return to technical discussion. The register shift functions as pure extraction: it terminates the technical dispute without resolution, transfers emotional labor burden to the patient, and creates a permanent record framing the patient as difficult rather than correct. Maximum experienced extraction from trapped position.
constraint_indexing:constraint_classification(emotional_register_exit, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PATIENT ADVOCATE WITH RESOURCES (TANGLED ROPE) — Constrained but not trapped. Can escalate through formal complaint channels, request different providers, or exit to alternative institutions at significant cost. Experiences both coordination (the emotional register sometimes reveals genuine empathy gaps that need addressing) and extraction (the register shift often functions as deflection from accountability). The constraint coordinates legitimate emotional care needs while enabling evasion of technical failures.
constraint_indexing:constraint_classification(emotional_register_exit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL FRONT-LINE WORKER (ROPE) — Primary beneficiary with arbitrage exit options. The emotional register exit provides a trained, institutionally-sanctioned escape route from technical disputes the worker cannot resolve due to systemic constraints (inadequate staffing, broken systems, contradictory policies). Experiences the constraint as coordination: it gives them a tool to de-escalate conflict and protect their own emotional resources when the institution has failed to provide technical solutions. Net beneficiary — the register shift runs toward this agent as protection, not away from them as extraction.
constraint_indexing:constraint_classification(emotional_register_exit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL MANAGEMENT (ROPE) — Secondary beneficiary with arbitrage exit. The emotional register exit functions as a liability shield: it converts technical failures (which create legal and regulatory exposure) into emotional conflicts (which are unfalsifiable and unactionable). Management experiences this as coordination: it protects the institution from accountability while maintaining the appearance of patient-centered care. The constraint enables management to avoid costly technical fixes by reframing technical failures as empathy gaps.
constraint_indexing:constraint_classification(emotional_register_exit, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTHCARE QUALITY IMPROVEMENT COALITION (TANGLED ROPE) — Organized agents (patient safety organizations, quality improvement networks, regulatory bodies) see both coordination and extraction. The emotional register genuinely coordinates care for patients whose needs are primarily emotional, but it also suppresses technical accountability signals that quality improvement depends on. The coalition has mobility (can shift focus to institutions with better accountability) but faces extraction through the constraint's interference with error detection and correction mechanisms.
constraint_indexing:constraint_classification(emotional_register_exit, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: PATIENT-CENTERED CARE FRAMEWORK (PITON) — The institutional framework that originally justified emotional register training has degraded into theater. Patient-centered care was designed to address genuine empathy deficits in technical medicine, but the emotional register exit has become a performative script deployed to evade accountability rather than to provide care. The framework persists through institutional inertia (training programs, accreditation requirements, mission statements) despite its functional atrophy. High theater ratio — the empathy script is performed, not practiced.
constraint_indexing:constraint_classification(emotional_register_exit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From analytical distance, the constraint exhibits both genuine coordination function (some patients do need emotional validation, and some technical disputes mask emotional needs) and asymmetric extraction (the register shift systematically suppresses technical accountability). The constraint coordinates emotional care while extracting from technical dispute resolution. The analytical classification matches the claimed type, confirming this is a structurally mixed coordination-extraction mechanism, not a pure snare misclassified as coordination or vice versa.
constraint_indexing:constraint_classification(emotional_register_exit, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emotional_register_exit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emotional_register_exit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emotional_register_exit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emotional_register_exit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emotional_register_exit, TR),
    TR >= 0.70.

:- end_tests(emotional_register_exit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The register shift extracts from patients by terminating technical disputes without resolution, transferring emotional labor burden, and creating institutional records that frame the patient as emotionally dysregulated rather than technically correct. However, extraction is not maximal because some proportion of register shifts do address genuine emotional needs (omega variable: legitimate_emotional_need_proportion). The value reflects that the constraint has real coordination function alongside its extractive mechanism. Suppression (0.62): High. Patients face significant barriers to forcing return to technical register: the institutional script treats requests to return to technical discussion as further evidence of emotional dysregulation; formal complaint channels are resource-intensive and often ineffective; exiting to alternative institutions is costly and may not be possible (insurance networks, geographic constraints, specialized care needs). The register shift is functionally irreversible within most interactions (omega variable: register_shift_reversibility). Theater ratio (0.68): High and rising. The emotional register is increasingly performative: workers are trained in empathy scripts ('I hear you,' 'I understand your frustration,' 'your feelings are valid') that are deployed as ritual rather than as genuine emotional engagement. The theater has increased over the measurement interval as institutions have formalized and standardized the register shift through training programs, creating a reproducible deflection technique rather than authentic patient-centered care.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same communication pattern appears as pure extraction, mixed coordination-extraction, or pure coordination depending on structural position. The trapped patient sees a Snare: the register shift is an escape hatch for the institution that leaves the patient's technical claim unresolved and imposes emotional labor burden. The institutional worker sees a Rope: the register shift is a coordination tool that protects them from disputes the institution has not equipped them to resolve. Management sees a Rope: the register shift coordinates liability management by converting technical failures into emotional conflicts. The patient advocate sees a Tangled Rope: the register shift sometimes addresses genuine emotional needs (coordination) but often deflects from technical accountability (extraction). The quality improvement coalition sees a Tangled Rope: the register shift coordinates emotional care but suppresses the technical accountability signals that quality improvement depends on. The patient-centered care framework sees a Piton: the original coordination function (addressing empathy deficits) has degraded into performative deflection. The analytical observer sees a Tangled Rope: the constraint genuinely coordinates emotional care while genuinely extracting from technical accountability. The perspectival gap is not a measurement error — it is the constraint's indexical structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits clear structural asymmetry in who benefits and who bears costs. Institutional actors (front-line workers and management) are beneficiaries: the register shift protects workers from unresolvable disputes and shields management from accountability. Patients with legitimate technical claims are victims: the register shift terminates their dispute without resolution and imposes emotional labor burden. The directionality computation derives from these structural relationships plus exit options. Trapped patients with no exit experience maximum extraction (high d → high f(d) → high χ). Institutional actors with arbitrage exit experience the constraint as coordination (low d → low/negative f(d) → low/negative χ). The patient advocate with constrained exit experiences mixed extraction-coordination (mid d → mid f(d) → mid χ). The organized quality improvement coalition experiences moderate extraction despite mobility because the constraint interferes with their core function (error detection and correction). The analytical observer sees the structural mix directly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope classification requires BOTH genuine coordination function AND asymmetric extraction, and that both must be structurally present, not just perceptually claimed. The coordination function is real: some patients do have primarily emotional needs that the technical register cannot address, and the emotional register does provide a communication pathway for those needs. The extraction is also real: the register shift systematically terminates technical disputes without resolution, transfers emotional labor burden to patients, and suppresses accountability signals. The constraint is not a Snare misclassified as coordination (the coordination function is genuine, not cover story) and not a Rope misclassified as extraction (the extraction is structural, not perceptual). The Tangled Rope classification is confirmed by the analytical perspective matching the claimed type, by the presence of both beneficiaries (institutional actors) and victims (patients with technical claims), and by the requirement for active enforcement (institutional training programs that teach the register shift as a technique). The mandatrophy is resolved by showing that the coordination and extraction are structurally inseparable: you cannot remove the extraction (terminate the register shift) without also removing the coordination (lose the pathway for genuine emotional needs), and you cannot remove the coordination without the extraction becoming visible as pure deflection (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_emotional_need_proportion,
    'What proportion of emotional register deployments address genuine emotional needs vs deflect from technical failures?',
    'Longitudinal outcome tracking: resolution rate and patient satisfaction for technical claims that trigger emotional register vs those that remain in technical register; correlation between register shift and subsequent technical resolution',
    'If >70% legitimate: constraint is primarily coordination with extractive edge cases (closer to Rope from more perspectives). If <30% legitimate: constraint is primarily extraction with coordination cover story (closer to Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_emotional_need_proportion, empirical, 'Proportion of emotional register deployments addressing genuine emotional needs').

omega_variable(
    register_shift_reversibility,
    'Can patients successfully return to technical register after emotional register deployment, or is the shift functionally irreversible within the interaction?',
    'Conversation analysis of patient-provider interactions: frequency of successful return to technical discussion after empathy script deployment; institutional response to patients who explicitly request return to technical register',
    'If reversible: suppression is lower than measured (patients have more agency than structural position suggests). If irreversible: suppression is accurate or understated (register shift is a one-way trap).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(register_shift_reversibility, empirical, 'Whether emotional register shift is reversible within interactions').

omega_variable(
    front_line_worker_complicity_vs_constraint,
    'Do front-line workers deploy emotional register as strategic deflection (complicity) or as trained response to impossible systemic constraints (structural constraint)?',
    'Worker testimony and institutional ethnography: do workers recognize the register shift as deflection? Do they have technical solutions they are withholding, or are they genuinely unable to resolve the technical claim due to systemic failures? Correlation between register shift frequency and institutional resource adequacy.',
    'If strategic deflection: workers are beneficiaries of extraction mechanism (current model). If structural constraint: workers are also victims, and the true beneficiary is institutional management that has failed to provide adequate resources. This would shift the directionality model significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(front_line_worker_complicity_vs_constraint, conceptual, 'Whether front-line workers are complicit beneficiaries or constrained actors').

omega_variable(
    emotional_register_training_origin,
    'Was emotional register training originally designed as accountability evasion, or has it been repurposed from legitimate patient-centered care pedagogy?',
    'Historical analysis of patient-centered care training materials and institutional implementation: original intent vs current deployment patterns; timeline of training emphasis shift relative to liability and regulatory pressure',
    'If originally evasive: constraint is a designed extraction mechanism (Snare from more perspectives). If repurposed: constraint is a degraded coordination mechanism (Piton from more perspectives, with higher theater ratio).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emotional_register_training_origin, empirical, 'Original intent of emotional register training vs current function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotional_register_exit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emreg_theater_initial, emotional_register_exit, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emreg_tr_t3, emotional_register_exit, theater_ratio, 3, 0.48).
narrative_ontology:measurement(emreg_tr_t6, emotional_register_exit, theater_ratio, 6, 0.58).
narrative_ontology:measurement(emreg_theater_current, emotional_register_exit, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(emreg_be_t0, emotional_register_exit, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(emreg_be_t3, emotional_register_exit, base_extractiveness, 3, 0.36).
narrative_ontology:measurement(emreg_be_t6, emotional_register_exit, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(emreg_be_t10, emotional_register_exit, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotional_register_exit, identity_coordination).
narrative_ontology:boltzmann_floor_override(emotional_register_exit, 0.12).

% DUAL FORMULATION NOTE:
% This constraint is downstream of physician_call_reluctance (mountain). The emotional register exit is one institutional adaptation to the structural fact that physicians cannot be compelled to engage with technical disputes. Front-line workers are trained to convert technical disputes into emotional interactions that do not require physician involvement, because physician involvement is not available as a resolution pathway. The upstream mountain constraint (physician autonomy as immutable professional norm) creates the conditions for the downstream tangled rope (emotional register exit as mixed coordination-extraction mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emotional_register_exit, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
