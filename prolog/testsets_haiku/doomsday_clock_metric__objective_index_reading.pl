% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index (Expert Authority Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists,
 *   presents itself as an objective index of existential risk levels,
 *   updating annually based on expert synthesis of empirical indicators
 *   (nuclear stockpile size, weapons deployment posture, climate metrics,
 *   biosecurity development, AI capabilities). This reading instantiates the
 *   objective-index framing: the Clock measures what it claims to
 *   measure—global existential threat—through verifiable expert assessment.
 *   The constraint operates as a tangled rope: it coordinates expert judgment
 *   on fragmented threat domains (real coordination function), but in doing
 *   so it transfers interpretive authority over existential-risk baselines
 *   from democratic deliberation to expert monopoly, suppressing alternative
 *   framings by claiming objectivity. The measurement series runs across 77
 *   years, capturing rising theater_ratio (performative maintenance
 *   increasing as the founding problem becomes less acute) and rising
 *   suppression_requirement (the effort to exclude alternative risk framings
 *   has intensified as the constraint's normative character became more
 *   visible).
 *
 * KEY AGENTS:
 *   - Atomic Scientists' Collective: agenda-setter, maintains Clock mechanism and annual update process
 *   - Scientific Authority (institutional): beneficiary, derives legitimacy and policy influence from Clock's objective-index authority
 *   - Democratic Accountability Structures: payer, constrained from redefining risk baselines
 *   - Non-Expert Public: payer, trapped—absorbs Clock readings as fact, cannot contest measurement claims
 *   - Alternative Risk Framings: excluded, identity-locked—philosophical and social-science perspectives structurally outside the measurement process
 *   - Policy-Makers (state-level): observer/secondary payer, benefit from outsourced risk authority but are constrained by Clock's readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.79).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index (Expert Authority Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, 'd7c9d67d-133b-450a-8710-042f4a4ca177').
narrative_ontology:cs_kernel_codification('d7c9d67d-133b-450a-8710-042f4a4ca177', formalized).
narrative_ontology:cs_authority_grounding('d7c9d67d-133b-450a-8710-042f4a4ca177', expertise).
narrative_ontology:cs_interpretation_layer_present('d7c9d67d-133b-450a-8710-042f4a4ca177').
narrative_ontology:cs_reading_relation('d7c9d67d-133b-450a-8710-042f4a4ca177', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d7c9d67d-133b-450a-8710-042f4a4ca177', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_axiom('d7c9d67d-133b-450a-8710-042f4a4ca177', foundational, existential_risk_empirically_measurable).
narrative_ontology:cs_axiom_status(existential_risk_empirically_measurable, holdable).
narrative_ontology:cs_axiom_grounding('d7c9d67d-133b-450a-8710-042f4a4ca177', existential_risk_empirically_measurable, empirically_contingent).
narrative_ontology:cs_axiom('d7c9d67d-133b-450a-8710-042f4a4ca177', foundational, measurement_normatively_neutral).
narrative_ontology:cs_axiom_status(measurement_normatively_neutral, holdable).
narrative_ontology:cs_axiom_grounding('d7c9d67d-133b-450a-8710-042f4a4ca177', measurement_normatively_neutral, deontological).
narrative_ontology:cs_reference_frame('d7c9d67d-133b-450a-8710-042f4a4ca177', expert_measurement_of_physical_threat_parameters).
narrative_ontology:cs_drift_state('d7c9d67d-133b-450a-8710-042f4a4ca177', contemporary_visibility_of_normativity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d7c9d67d-133b-450a-8710-042f4a4ca177', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, atomic_scientists_collective).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, scientific_authority_institutional).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_accountability_structures).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, non_expert_public_deliberation).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, expert_quantification_objective).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, existential_risk_measurable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively maintains and updates the Clock setting through an annual expert deliberation process. Authors the specific minute reading as a synthesis of measurable indicators (nuclear weapons stockpiles, climate parameters, biosecurity development, AI capabilities). Justifies the reading as objective risk assessment grounded in empirical data. Maintains exclusive interpretive authority over what the Clock measures and how updates should be read.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, atomic_scientists_collective, agenda_setter,
    organized, generational, constrained, global).

% Derives legitimacy, funding, and policy standing from the Clock's authority as an objective risk measure. The institutional endorsement of expert judgment on existential threat enables science-based policy influence that would be unavailable through transparent value assertion. Scientists' expertise translates directly to decision-setting power over global risk management narratives.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, scientific_authority_institutional, beneficiary,
    institutional, generational, mobile, global).

% Democratic legislatures, parliaments, and publics depend on the Clock's framing to understand existential risk but cannot directly contest or co-author the setting. Policy decisions (weapons treaties, climate action, biosafety regulation) are justified by reference to the Clock's objective reading, removing them from direct democratic deliberation. The constraint narrows democratic authority to ratifying expert-set risk baselines.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_accountability_structures, payer,
    moderate, biographical, constrained, national).

% The public receives the Clock setting as an empirical fact and is expected to adjust threat perception and political urgency accordingly. Non-expert citizens cannot meaningfully evaluate the measurement claims or contest the selection of indicators, methodology, or weighting. The objective-index framing precludes public participation in setting existential-threat baselines, even where outcomes depend on collective action (climate transitions, weapons disarmament).
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, non_expert_public_deliberation, payer,
    powerless, immediate, trapped, global).

% Philosophers, risk economists, and social movements that frame existential risk through different value premises (justice impacts, intergenerational equity, precautionary vs. expected-value reasoning) are structurally outside the Clock-setting process. These framings could reshape which indicators count and how they are weighted, but the objective-index reading defines them as outside the legitimate space of empirical measurement. Their exclusion is maintained by the claim of objectivity itself.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, alternative_risk_framings, excluded,
    moderate, biographical, identity_locked, global).

% Governments cite the Clock's objective reading to justify existential-risk policies to their publics. The constraint provides cover for decisions (weapons programs, climate spending, biosafety regulation) that might be more contested if framed as value choices rather than responses to measured threat. They benefit from outsourcing risk-baseline authority, reducing democratic friction, but are also constrained by the Clock's reading—if the Bulletin adjusts the setting, policy justifications shift.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers_state_level, observer,
    institutional, generational, mobile, national).

% Other expert organizations (IPCC, WHO, international arms-control institutes) produce risk assessments but lack the Doomsday Clock's cultural authority and unified measure. They could offer competing syntheses of existential risk with different weightings or methodologies, but the Clock's establishment and media amplification have made it the canonical index. Their competitive framings are marginalized by the constraint's institutional dominance.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, competing_expert_bodies, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, scientific_authority_institutional).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synthesizes dispersed expert assessments of existential risk (nuclear, climate, biological, technological) into a single unified metric, enabling coordinated communication of threat level to policy and public audiences who could not integrate such data independently.
% TRANSFER_FUNCTION: Transfers interpretive authority over existential-risk baselines from distributed democratic deliberation and diverse expert frameworks to the centralized judgment of the Atomic Scientists' collective, enforced by the constraint that the Clock reading is presented as objective empirical measurement rather than normative choice.
% ABSENT_VOICES: Democratic publics, non-expert stakeholders, philosophers and social scientists who frame existential risk through justice or equity lenses, and alternative expert bodies with competing risk syntheses. These parties would argue that risk baselines are value-laden choices, not measurements; that indicator selection reflects normative priorities; and that democratic legitimacy requires plural framings. They are kept out by the claim of objectivity itself.
% DISAPPEARANCE_RATIONALE: If the Clock constraint—the claim that it measures objective risk levels—were removed, existential-risk discourse would fragment across multiple expert framings and democratic deliberation would re-open around which risks merit which policy responses. Policy justifications would become transparently value-laden, requiring public deliberation rather than deference to expert consensus. Risk governance would reorganize around plural frames rather than a single canonical index.
% FOUNDING_PROBLEM: In the post-WWII atomic era, the public and policy-makers lacked a unified way to gauge existential threat across multiple domains and were unprepared for nuclear escalation risk. Expert physicists created the Clock as a communication tool to make the threat comprehensible and motivate collective action on disarmament.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin of the Atomic Scientists attests the founding problem persists and justifies continued Clock-setting as response to ongoing existential threats. Independent scholars, risk economists, and science communicators attest that the founding communication problem (unprepared audiences, fragmented expertise) has been substantially resolved by modern scientific literacy, historical understanding of nuclear weapons, and professional risk-communication infrastructure. The Clock persists despite problem resolution, indicating mandatrophy. Policy-makers cite Clock readings not because they lack independent risk assessment capability, but because the Clock provides institutional cover for decisions that would be more contested if framed as value choices.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steadily (0.35→0.68) over the interval because the Clock's institutional dominance and media authority have grown, allowing it to suppress competing risk framings more completely. Early Clock-setting (1947–1962) faced genuine epistemic uncertainty and scientific contestation; modern Clock-setting (2015–2024) operates in a context where alternative risk framings (justice-centered, precautionary, value-explicit) have been marginalized and the objective-index reading is treated as canonical. Theater_ratio rises (0.15→0.42) because the original coordination function (communicating novel nuclear threat to unprepared audiences) is no longer the binding constraint—modern audiences understand nuclear risk—yet the Clock persists, increasingly through performative rituals (annual updates, media coverage, policy citation) rather than novel information. Suppression_requirement rises (0.45→0.79) because maintaining the objective-index reading against growing recognition of its normative character requires active exclusion: scientists must suppress their own awareness that indicator selection, weighting, and interpretation reflect value choices; policy-makers must suppress acknowledgment that risk baselines are normative; excluded parties (philosophers, social scientists, democratically-minded critics) must be kept out of the deliberation. The measurements are authored on a single shared time grid; every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the objective-index reading, the Clock is a factual measurement system and alternative framings (justice-centered, value-explicit, precautionary) are ideological overlays on empirical assessment—they should be excluded. From the hybrid-legitimacy reading, the Clock is irreducibly entangled in normative choice and the suppression of that entanglement is itself the constraint's extractive character. From the performative-tool reading, the Clock is strategically designed for policy impact, and its apparent objectivity is part of the strategy. These are three genuinely different readings of the same kernel (the Clock's canonical status), not three angles on the same fact. The objective-index reading treats the other two as logical errors; the hybrid-legitimacy reading treats the objective-index reading as a naturalizing cover story; the performative-tool reading treats both as missing the point—the Clock's social function, not its epistemic character.
 *
 * DIRECTIONALITY LOGIC:
 *   The Atomic Scientists' Collective and scientific authority are beneficiaries (d ≈ 0.2): they set the agenda, collect the legitimacy and policy influence, and face no exit pressure—their power is institutional and their options are mobile (they could step back, but that would cost them influence, so the exit is not real). Democratic accountability structures are victims (d ≈ 0.85): they bear the cost of having risk-baseline authority stripped from deliberation, their power is eroded by the constraint itself, their time horizon is constrained (policy urgency is set by the Clock, not by democratic process), and their exit is trapped (they cannot move to a different risk-communication system without losing legitimacy-by-default). The non-expert public is the deepest target (d ≈ 0.95): powerless, immediate time horizon, trapped exit (they depend on expert mediation to understand existential risk), and their only exit is epistemic—reject the framing entirely—which is both identity-locked (citizens in modern democracies are expected to defer to experts) and materially inaccessible (without experts, they cannot assess existential threats). Alternative risk framings are excluded (d ≈ 0.75): they have organized power, but the constraint's objectivity claim precludes them from legitimate entry into the deliberation. Suppression is the mechanism: not external coercion, but the framing itself that defines objective measurement and subjective normativity as incommensurable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communication of novel nuclear threat to unprepared audiences) was acute in 1947–1962 and has been substantially solved by modern scientific literacy, nuclear-weapons history, and climate science communication infrastructure. The Clock persists despite the founding problem's decline because the scientific authority it confers on the Bulletin's collective and the policy influence it enables are now the primary benefits flowing through the constraint. Rising theater_ratio (0.42 at interval end) captures this atrophy: the Clock still performs its communication function, but increasingly through ritualized media spectacle rather than novel information transfer. The constraint shows classic mandatrophy: a real coordination solution to a historical problem has become a vehicle for expert authority extraction, maintained through institutional inertia and the impossibility of stepping back (stepping back would mean surrendering the authority-capture mechanism). The suppression of alternative framings—philosophers, social scientists, democratic voices—is the work that keeps the constraint from collapsing, because those framings would expose the normative structure underlying the 'objective' index.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_vs_normativity_boundary,
    'Is the Clock''s setting fundamentally a measurement of risk levels, or is indicator selection and weighting inherently normative, with measurement as downstream application?',
    'Audit the Clock-setting process: document what data sources scientists consider, what weightings they use, and what explicit normative principles (if any) guide indicator selection. Compare across cycles to see if weightings change with scientific consensus or with framing shifts.',
    'If measurement is primary and normativity is incidental, the objective-index reading holds and suppression of alternative framings is justified. If normativity is primary (different framings would weight indicators differently and produce different settings), the hybrid-legitimacy reading gains support and suppression becomes epistemically indefensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_vs_normativity_boundary, conceptual, 'Whether risk measurement is separable from normative framing.').

omega_variable(
    expert_monopoly_necessity,
    'Is centralized expert authority structurally necessary for coherent existential-risk communication, or could distributed deliberative processes (including non-experts, affected communities, alternative framings) produce equally useful syntheses?',
    'Natural experiment: societies or organizations that open Clock-setting deliberation to broader participation and track whether policy responsiveness improves or degrades; comparison of jurisdictions with centralized vs. distributed risk-governance structures.',
    'If centralized authority is necessary, the suppression of democratic voice is a coordination cost. If distributed deliberation produces comparable results, the suppression is pure extraction and the constraint should be reclassified as snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expert_monopoly_necessity, empirical, 'Whether expert monopoly is necessary or extractive.').

omega_variable(
    normative_suppression_internalization,
    'Is the suppression of alternative normative framings structural (external barriers: excluded from deliberation, lack of institutional platform) or internalized (scientists themselves believe objective measurement is possible and normativity should be excluded)?',
    'Post-constraint deliberation: if the Clock-setting process were explicitly opened to normative deliberation, would scientists'' own framings shift? Do they privately acknowledge normativity? Interviews with exiting scientists.',
    'If internalized, the constraint''s suppression is higher than the structural measure suggests—the target carries the suppression with them after exit. The constraint''s persistence depends partly on self-suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_suppression_internalization, empirical, 'Whether suppression of normativity is structural or internalized in expert community.').

omega_variable(
    kernel_reading_foreclosure,
    'Can the objective-index reading and the hybrid-legitimacy reading both be held within the same institutional framework, or does commitment to objective measurement logically foreclose the possibility of irreducible normative entanglement?',
    'Logical analysis of the readings'' core premises: objective-index asserts measurement can be decoupled from value; hybrid-legitimacy asserts measurement cannot be decoupled. These are logical contradictions. Within a single institutional framework (the Clock''s authority structure), can both coexist?',
    'If they logically foreclose each other, the reading-relation is forecloses. If different institutional actors can hold different readings (scientists commit to objective, philosophers commit to hybrid, and no unified framework reconciles them), the relation is coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether objective-index and hybrid-legitimacy readings logically foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__objective_index_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement_basis(doom_tr_t1947, observed).
narrative_ontology:measurement(doom_tr_t1962, doomsday_clock_metric__objective_index_reading, theater_ratio, 1962, 0.22).
narrative_ontology:measurement_basis(doom_tr_t1962, observed).
narrative_ontology:measurement(doom_tr_t1980, doomsday_clock_metric__objective_index_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement_basis(doom_tr_t1980, observed).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__objective_index_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(doom_tr_t2000, observed).
narrative_ontology:measurement(doom_tr_t2015, doomsday_clock_metric__objective_index_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(doom_tr_t2015, observed).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__objective_index_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(doom_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement_basis(doom_be_t1947, observed).
narrative_ontology:measurement(doom_be_t1962, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1962, 0.48).
narrative_ontology:measurement_basis(doom_be_t1962, observed).
narrative_ontology:measurement(doom_be_t1980, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement_basis(doom_be_t1980, observed).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement_basis(doom_be_t2000, observed).
narrative_ontology:measurement(doom_be_t2015, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement_basis(doom_be_t2015, observed).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(doom_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1947, 0.45).
narrative_ontology:measurement_basis(doom_su_t1947, observed).
narrative_ontology:measurement(doom_su_t1962, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement_basis(doom_su_t1962, observed).
narrative_ontology:measurement(doom_su_t1980, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1980, 0.66).
narrative_ontology:measurement_basis(doom_su_t1980, observed).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(doom_su_t2000, observed).
narrative_ontology:measurement(doom_su_t2015, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement_basis(doom_su_t2015, observed).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2024, 0.79).
narrative_ontology:measurement_basis(doom_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.06).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% The doomsday_clock_metric kernel admits three structurally distinct constraint readings: objective_index (empirical measurement), hybrid_legitimacy (value-entangled), and performative_tool (strategic communication). Each reading has different ε, beneficiary/victim structures, and suppression mechanisms. They are linked as sibling readings of the same kernel, not as three angles on one constraint. The objective-index reading treats measurement as primary and normative framing as secondary (or erroneous); the hybrid-legitimacy reading reverses that priority; the performative-tool reading sidesteps both in favor of functional analysis. The readings coexist across different parties' commitments and influence each other through institutional dynamics, but they do not converge on a single constraint definition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
