% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__capture_substrate_reading, []).

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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Openness Commitment â Capture Substrate Reading
 *   domain: technology governance / institutional economics
 *
 * SUMMARY:
 *   This constraint story instantiates the capture_substrate_reading of the
 *   ietf_openness_commitment kernel. The IETF's formal commitment to open,
 *   meritocratic standard-setting is read here not as a rope of pure
 *   coordination nor as a snare of overt extraction, but as a tangled rope: a
 *   genuine coordination substrate (internet protocols do need shared
 *   standards) that has been structurally repurposed by resource-advantaged
 *   platform operators into a gatekeeping mechanism. Large operators can
 *   afford the engineering time, travel, and implementation-scale
 *   demonstrations required to steer 'open' standards toward architectures
 *   that embed their existing advantages, while small implementers and
 *   end-users bear the costs of complexity and de facto proprietary lock-in
 *   disguised as technical consensus. The sibling readings are
 *   commons_stewardship_reading (rope) and legitimacy_erosion_reading
 *   (piton/snare boundary). This reading is Îµ-invariant: it does not fold
 *   the contest into the constraint but treats capture as a structurally
 *   stable claim with moderate extractiveness.
 *
 * KEY AGENTS:
 *   - large_platform_operators: Primary beneficiary (institutional/arbitrage) â captures standard-setting power through resource-intensive participation
 *   - small_implementers: Primary target (moderate/constrained) â bears cost of implementing standards that favor scaled operators
 *   - end_users: Secondary target (powerless/constrained) â locked into consolidated ecosystems masked as open
 *   - ietf_working_groups: Agenda setter (institutional/analytical) â administers rough consensus process that serves as coordination shell
 *   - open_source_alternatives: Excluded voice (moderate/constrained) â structurally absent from working groups due to resource barriers
 *   - regulatory_observers: Analytical observer (institutional/analytical) â evaluates whether open-process rhetoric masks market concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.48).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Openness Commitment â Capture Substrate Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology governance / institutional economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, '3d1c2e87-663b-4dce-8830-6975dd87c84a').
narrative_ontology:cs_kernel_codification('3d1c2e87-663b-4dce-8830-6975dd87c84a', formalized).
narrative_ontology:cs_authority_grounding('3d1c2e87-663b-4dce-8830-6975dd87c84a', expertise).
narrative_ontology:cs_interpretation_layer_present('3d1c2e87-663b-4dce-8830-6975dd87c84a').
narrative_ontology:cs_reading_relation('3d1c2e87-663b-4dce-8830-6975dd87c84a', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d1c2e87-663b-4dce-8830-6975dd87c84a', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('3d1c2e87-663b-4dce-8830-6975dd87c84a', foundational, resource_advantage_encodes_gatekeeping).
narrative_ontology:cs_axiom_status(resource_advantage_encodes_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('3d1c2e87-663b-4dce-8830-6975dd87c84a', resource_advantage_encodes_gatekeeping, empirically_contingent).
narrative_ontology:cs_axiom('3d1c2e87-663b-4dce-8830-6975dd87c84a', foundational, open_process_launders_concentration).
narrative_ontology:cs_axiom_status(open_process_launders_concentration, holdable).
narrative_ontology:cs_axiom_grounding('3d1c2e87-663b-4dce-8830-6975dd87c84a', open_process_launders_concentration, conventional).
narrative_ontology:cs_reference_frame('3d1c2e87-663b-4dce-8830-6975dd87c84a', open_meritocratic_coordination).
narrative_ontology:cs_drift_state('3d1c2e87-663b-4dce-8830-6975dd87c84a', platform_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d1c2e87-663b-4dce-8830-6975dd87c84a', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Facilitate technical discussion, draft review, and consensus calls to produce Request for Comments documents. They apply open participation rules and rough consensus norms, but the time and travel costs of meaningful participation create a structural filter on who is in the room when decisions are made.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_working_groups, agenda_setter,
    institutional, generational, analytical, global).

% Deploy full-time engineering teams to working groups, sponsor draft authorship, and operate implementation-scale testbeds. Their scale allows them to shape standards toward architectures that integrate with their existing infrastructure, making interoperability technically open but effectively dependent on resources they control.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Attempt to implement published standards but encounter undocumented assumptions, rapid revision cycles, and compliance test suites that require scaled infrastructure to pass. They lack the travel budgets and full-time staff to participate in working groups, so their operational reality is shaped by standards they did not help write.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_implementers, payer,
    moderate, biographical, constrained, global).

% Rely on devices and services built atop internet standards marketed as open. Their choice of services consolidates toward a few platforms because the protocols' effective implementation complexity prevents diverse competing implementations from reaching market.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users, payer,
    powerless, immediate, constrained, global).

% Develop independent protocol implementations and alternative tools but lack resources to attend meetings, run implementation-scale interoperability demonstrations, or employ dedicated standards liaison staff. Their technical objections rarely reach the working group floor.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, open_source_alternatives, excluded,
    moderate, biographical, constrained, global).

% Monitor standardization outcomes for evidence of market concentration disguised as technical coordination. They collect testimony from excluded implementers and analyze whether ostensibly open standards create barriers to entry.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, regulatory_observers, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared technical grammar for internet interoperability, reducing fragmentation and enabling multi-vendor communication across a global packet-switched network.
% TRANSFER_FUNCTION: Moves de facto standard-setting power and interoperability control from dispersed implementers to resource-concentrated platform operators, by encoding resource-intensive participation barriers into an otherwise open process.
% ABSENT_VOICES: Individual end-users, small SaaS operators, and non-commercial implementers who lack travel budgets, implementation-scale testbeds, or full-time standards participation capacity are structurally absent from working group consensus despite being affected by outcomes.
% DISAPPEARANCE_RATIONALE: If the IETF openness commitment and its rough-consensus process vanished, platform operators would lose a key legitimacy mechanism for proprietary extension; alternative governance models such as regulatory standards, proprietary consortia, or forked standards bodies would compete, and the current concentration of effective standard-setting power would become explicit rather than laundered through open process.
% FOUNDING_PROBLEM: Preventing protocol fragmentation and vendor lock-in in early internet development by establishing a vendor-neutral, meritocratic technical coordination forum where running code and rough consensus determine outcomes.
% FOUNDING_PROBLEM_CORROBORATION: Original internet pioneers and some academic participants attest the founding problem was real and that the process still solves it. Critics from smaller vendor communities and digital-rights advocates outside the benefiting platform operators attest the process has been repurposed as a legitimacy mechanism for incumbent control; competition authority filings and independent ethnographic studies support the shifted-function reading.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__capture_substrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__capture_substrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint does provide genuine interoperability benefits that would not exist without coordination; the extraction lies in the asymmetry of influence and the encoding of resource barriers. Suppression is moderate (0.48) because the process does not overtly block dissent but suppresses alternatives through procedural cost (participation requires full-time engineering commitment). Theater ratio is moderate (0.40) reflecting the growing performative dimension of 'running code' demonstrations and rough consensus rituals that obscure capture. Accessibility collapse is moderate (0.50): alternatives (proprietary consortia, regulatory standards) exist but carry their own lock-in costs. Resistance is moderate-low (0.35) because victims are fragmented and the process retains legitimacy from its historical coordination successes. The claim/metric independence is maintained: the story claims tangled_rope structurally, while metrics describe actual operation.
 *
 * PERSPECTIVAL GAP:
 *   The large_platform_operators seat experiences the constraint as a legitimate coordination mechanism in which their substantial contributions earn commensurate influence; the small_implementers seat experiences the same structure as extraction because the standard's complexity and revision pace exceed their resources. The ietf_working_groups seat sees meritocracy; the excluded open_source_alternatives see a resource filter. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (large_platform_operators) have low directionality: the constraint subsidizes their market position by making their architectural preferences the default technical grammar. Victims (small_implementers, end_users) have high directionality: they pay compliance costs and suffer reduced choice. The ietf_working_groups, as agenda setter, sit near symmetric (0.5) because they neither collect rents nor bear extraction directly, though their institutional identity is bound to the process.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the false binary of rope (pure coordination) versus snare (pure extraction). The IETF process was built to solve a genuine coordination problem (protocol fragmentation) and still partially solves it; therefore it cannot be a pure snare. However, the asymmetric extraction is structural and active, not incidental: operators capture the agenda-setting function through resource advantage. This rules out rope. There is no sunset clause, ruling out scaffold. The constraint is not merely inertial theater (piton) because beneficiaries actively maintain and profit from it. Therefore tangled_rope is the structurally accurate classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_empirical_basis,
    'Does resource advantage in IETF working groups empirically translate to measurable standard-setting influence disproportionate to technical merit?',
    'Bibliometric and participation analysis of draft authorship, working group chair appointments, and consensus calls correlated with employer size and revenue.',
    'If empirically confirmed, the tangled_rope classification is reinforced; if refuted, the constraint reverts toward rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_empirical_basis, empirical, 'Empirical test of resource-advantage gatekeeping claim').

omega_variable(
    kernel_reading_underdetermination,
    'Which of the three readings (capture substrate, commons stewardship, legitimacy erosion) correctly describes the IETF openness commitment, and can they be adjudicated by the same evidence?',
    'Cross-reading comparison of predictive claims: the capture reading predicts concentration of implementation around large operators; the commons reading predicts broad, level implementation field; the legitimacy erosion reading predicts procedural crisis or fork.',
    'Determines whether the kernel is fundamentally ambiguous (multiple valid constraints) or whether one reading is structurally superior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Indeterminacy among sibling kernel readings').

omega_variable(
    openness_ritual_theater,
    'To what extent does the IETF''s rough consensus process function as performative theater that masks capture rather than as a genuine deliberative mechanism?',
    'Ethnographic and documentary analysis of working group meetings measuring actual consensus formation versus prefabricated operator positions.',
    'High theater ratio would push classification toward piton; low theater ratio would keep it as tangled_rope with genuine coordination remainder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(openness_ritual_theater, empirical, 'Theater vs genuine deliberation in open standards process').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ietf_tr_t4, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(ietf_tr_t8, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(ietf_tr_t12, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(ietf_tr_t16, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ietf_be_t4, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(ietf_be_t8, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(ietf_be_t12, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(ietf_be_t16, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ietf_su_t4, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 4, 0.31).
narrative_ontology:measurement(ietf_su_t8, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 8, 0.37).
narrative_ontology:measurement(ietf_su_t12, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(ietf_su_t16, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
