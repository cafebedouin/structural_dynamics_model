% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Hybrid Legitimacy Reading
 *   domain: science communication/normative epistemology/risk governance
 *
 * SUMMARY:
 *   The Bulletin of the Atomic Scientists' Doomsday Clock is a symbolic
 *   device that condenses nuclear, climate, and AI existential risk into a
 *   single minutes-to-midnight figure. This constraint story captures the
 *   hybrid_legitimacy_reading: the clock does not merely track empirical risk
 *   (objective_index_reading) nor merely perform strategic alarm
 *   (performative_tool_reading), but sustains its authority through a
 *   deliberately maintained ambiguity between scientific judgment and
 *   normative prescription. The Bulletin convenes scientists to render a
 *   judgment that is presented as technical yet is inextricable from policy
 *   mobilization. This reading treats the entanglement as structurally
 *   productiveâneither a bug nor a strategy, but the core legitimacy
 *   mechanism.
 *
 * KEY AGENTS:
 *   - bulletin_of_atomic_scientists: Agenda setter (institutional/constrained) â administers the clock, sustains the ambiguity, derives platform and funding
 *   - existential_risk_mobilizers: Primary beneficiaries (powerful/mobile) â leverage the symbol for policy traction
 *   - physical_scientists: Primary payers (organized/constrained) â expertise enrolled, credibility entangled with normative claims
 *   - global_public: Secondary payers (powerless/constrained) â absorbs ambiguous signal without accountability mechanism
 *   - science_communication_observers: Analytical observers â map the entanglement without direct stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.63).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.73).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.73).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science communication/normative epistemology/risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__hybrid_legitimacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '95710257-712c-4359-af36-b120d55753e9').
narrative_ontology:cs_kernel_codification('95710257-712c-4359-af36-b120d55753e9', fixed_text).
narrative_ontology:cs_authority_grounding('95710257-712c-4359-af36-b120d55753e9', lineage).
narrative_ontology:cs_interpretation_layer_present('95710257-712c-4359-af36-b120d55753e9').
narrative_ontology:cs_reading_relation('95710257-712c-4359-af36-b120d55753e9', doomsday_clock_metric__objective_index_reading, coexists_with).
narrative_ontology:cs_reading_relation('95710257-712c-4359-af36-b120d55753e9', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('95710257-712c-4359-af36-b120d55753e9', foundational, existential_risk_entanglement_irreducible).
narrative_ontology:cs_axiom_status(existential_risk_entanglement_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('95710257-712c-4359-af36-b120d55753e9', existential_risk_entanglement_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('95710257-712c-4359-af36-b120d55753e9', foundational, symbolic_ambiguity_generates_legitimacy).
narrative_ontology:cs_axiom_status(symbolic_ambiguity_generates_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('95710257-712c-4359-af36-b120d55753e9', symbolic_ambiguity_generates_legitimacy, conventional).
narrative_ontology:cs_reference_frame('95710257-712c-4359-af36-b120d55753e9', entangled_authority_framework).
narrative_ontology:cs_drift_state('95710257-712c-4359-af36-b120d55753e9', contemporary_accountability_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('95710257-712c-4359-af36-b120d55753e9', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_mobilizers).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, physical_scientists).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, global_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the Science and Security Board annually to set the clock. Maintains that scientific expertise must be fused with normative judgment to address existential risk. Derives institutional relevance, media platform, and fundraising capacity from the clock's symbolic authority. The clock is its defining public voice; abandoning or disambiguating it would dissolve its global salience.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_of_atomic_scientists, agenda_setter,
    institutional, generational, constrained, global).

% Policy advocates, philanthropic funders, and movement leaders who cite the clock to mobilize action on nuclear weapons, climate, and AI. Benefit from a symbol that carries scientific authority while delivering normative urgency. Can shift to other symbols if the clock loses salience, but currently it provides unmatched annual media penetration.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_mobilizers, beneficiary,
    powerful, biographical, mobile, global).

% Expert communities in nuclear physics, climatology, and AI whose technical judgments are enlisted to justify the clock setting. Their collective credibility becomes entangled with policy prescriptions they may not individually endorse. Difficult to exit because the Bulletin claims to speak for scientific consensus broadly, and public dissent risks appearing indifferent to existential threats.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, physical_scientists, payer,
    organized, generational, constrained, global).

% Receives the annual clock announcement as a blended signal of scientific fact and normative alarm. Cannot easily disentangle the empirical basis from the policy prescription, and lacks a clear accountability mechanismâneither electoral nor scientific peer review governs the board's judgment. Bears the epistemic cost of ambiguity without recourse.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, global_public, payer,
    powerless, biographical, constrained, global).

% Scholars of science and technology studies, risk communication researchers, and critical analysts who study how the clock entangles descriptive and prescriptive claims. Neither benefit nor pay directly; they map the mechanism and its effects on public understanding of science.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, science_communication_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__hybrid_legitimacy_reading, diffuse).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__hybrid_legitimacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a globally legible, media-transmissible symbol that translates complex existential risk assessments into public consciousness and policy agendas, coordinating attention across scientific, policy, and public spheres around a shared annual ritual of synthetic judgment.
% TRANSFER_FUNCTION: Moves scientific credibility and symbolic urgency from participating scientists and the Bulletin's institutional history toward policy mobilization and organizational platform maintenance, while depositing an accountability deficit into the public sphere.
% ABSENT_VOICES: Scientists who reject the normative loading of their expertise; publics in the Global South who bear differential existential risk but hold no seat on the board; methodological purists who would demand transparent, auditable risk metrics with explicit confidence intervals.
% DISAPPEARANCE_RATIONALE: If the clock vanished, the Bulletin would lose its primary public platform and likely face institutional contraction; existential risk advocates would lose their most powerful symbolic condensation point; media coverage of nuclear, climate, and AI risk would reorganize around event-driven reporting rather than an annual ceremony of synthetic judgment.
% FOUNDING_PROBLEM: Post-World War II need to communicate nuclear danger to a broad democratic public in a way that conveyed both scientific severity and moral urgency, bridging the gap between technical arms-control discourse and civic engagement.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Manhattan Project and early nuclear age attest the communication gap was real. However, independent science communication scholars and critical risk analysts contest whether the current hybrid form still solves that problem or has become a self-perpetuating institution whose ambiguity now obscures rather than clarifies risk; these corroborating sources sit outside the benefiting parties.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the clock's ambiguity allows institutional and mobilizing actors to borrow scientific credibility without transparent accountability; suppression (0.63) reflects the active discursive work required to marginalize alternative framings that would separate empirical risk assessment from policy prescription. Theater_ratio (0.73) is high because the clock is inherently a theatrical deviceâits power lies in symbolic condensation rather than methodological transparencyâand this theatricality has intensified with media spectacle. Accessibility_collapse (0.48) is moderate: alternatives (pure risk indices, pure advocacy campaigns) remain conceptually available but are crowded out by the clock's institutionalized centrality. Resistance (0.52) is moderate: a significant fraction of scientists and commentators contest the normative loading, but the Bulletin actively manages the narrative. Measurements trace monotonic drift across the interval as mediatization and political contestation have intensified.
 *
 * PERSPECTIVAL GAP:
 *   The Bulletin experiences the constraint as necessary coordinationâits seat sees the entanglement as the only way to communicate existential stakes to a broad public. The mobilizer seat experiences it as a uniquely effective symbolic resource. By contrast, the physical_scientist seat experiences co-optation: their credibility is borrowed to underwrite prescriptions they do not control. The global_public seat experiences epistemic captureâreceiving a signal that cannot be evaluated because its scientific and normative components are deliberately inseparable. These divergences are structural, not perspectival illusions; the engine computes them from the beneficiary/victim declarations and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin and existential risk mobilizers are declared beneficiaries; their structural relationship to the constraint is subsidizing (low d). The Bulletin controls the agenda and captures institutional rents (platform, fundraising, media access); mobilizers gain a discursive tool with scientific aura. Physical scientists and the global public are declared victims/payers. Scientists bear the cost of credibility entanglement and constrained exit (the Bulletin speaks in their name); the public bears the accountability void. The observers are analytical and sit near neutral. No directionality overrides are needed because the structural derivation captures the relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the clock as pure coordination (rope) by naming the identifiable payersâscientists whose credibility is enlisted and a public left without accountability channels. It prevents mislabeling as pure extraction (snare) by acknowledging the genuine coordination function: the clock does concentrate global attention on existential risk in a way no purely empirical index has replicated. The tangled_rope classification captures that the same structure coordinates and extracts simultaneously. The founding problemâcommunicating nuclear risk post-Hiroshimaâis contested in status; if dead, the constraint persists without a sunset clause, reinforcing the rope/tangled boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Doomsday Clock structurally an objective index, a performative tool, or an irreducibly hybrid legitimacy mechanism?',
    'Comparative discourse analysis of Bulletin statements, board deliberation records, and media reception across decades to determine whether the clock''s operation stabilizes around one reading or cycles between them.',
    'Resolution would reclassify the constraint: objective index approximates rope or mountain; performative tool approximates snare; hybrid legitimacy sustains tangled rope classification and determines whether ambiguity is treated as cost or feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the doomsday clock kernel is structurally dominant.').

omega_variable(
    ambiguity_accountability_void,
    'Does the clock''s deliberate ambiguity generate a diffuse public good (coordinated risk attention) or primarily shield institutional actors from accountability?',
    'Accountability mapping: trace whether clock-setting decisions can be meaningfully challenged, by whom, and through what institutional channels; compare to analogous hybrid science-policy institutions.',
    'If ambiguity primarily shields institutional actors, effective extraction is higher than coordination framing suggests and directionality shifts toward the Bulletin as capturer rather than diffuse benefit; if diffuse public good, extraction is lower and the constraint tilts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_accountability_void, empirical, 'Whether ambiguity serves coordination or extraction.').

omega_variable(
    entanglement_necessity,
    'Can scientific judgment and normative stakes in existential risk communication be practically disentangled, or is the hybrid form structurally necessary?',
    'Design and evaluate alternative risk communication formats that separate empirical assessment from policy prescription, measuring public comprehension, policy uptake, and scientific credibility effects.',
    'If disentanglement is viable, the hybrid reading loses its necessity claim and the constraint''s coordination function can be decoupled from its extractive ambiguity; if impossible, the tangled rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entanglement_necessity, conceptual, 'Whether science-policy entanglement is necessary or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomsday_hybrid_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(doomsday_hybrid_tr_t7, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 7, 0.58).
narrative_ontology:measurement(doomsday_hybrid_tr_t14, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 14, 0.62).
narrative_ontology:measurement(doomsday_hybrid_tr_t21, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 21, 0.66).
narrative_ontology:measurement(doomsday_hybrid_tr_t28, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 28, 0.7).
narrative_ontology:measurement(doomsday_hybrid_tr_t35, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 35, 0.73).

% Extraction over time
narrative_ontology:measurement(doomsday_hybrid_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(doomsday_hybrid_be_t7, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 7, 0.46).
narrative_ontology:measurement(doomsday_hybrid_be_t14, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(doomsday_hybrid_be_t21, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 21, 0.58).
narrative_ontology:measurement(doomsday_hybrid_be_t28, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 28, 0.64).
narrative_ontology:measurement(doomsday_hybrid_be_t35, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(doomsday_hybrid_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(doomsday_hybrid_su_t7, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 7, 0.4).
narrative_ontology:measurement(doomsday_hybrid_su_t14, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 14, 0.46).
narrative_ontology:measurement(doomsday_hybrid_su_t21, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 21, 0.53).
narrative_ontology:measurement(doomsday_hybrid_su_t28, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 28, 0.59).
narrative_ontology:measurement(doomsday_hybrid_su_t35, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 35, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
