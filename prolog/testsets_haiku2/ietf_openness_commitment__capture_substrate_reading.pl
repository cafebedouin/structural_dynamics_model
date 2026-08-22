% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__capture_substrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: ietf_openness_commitment__capture_substrate_reading
 *   human_readable: IETF Standards Process as Proprietary Extension Substrate
 *   domain: technology/governance/institutional_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the CAPTURE SUBSTRATE reading of the
 *   IETF openness commitment kernel. The reading holds that the standards
 *   process, despite its formal commitment to rough consensus and openness,
 *   operates as a substrate enabling large platform operators to encode
 *   proprietary extensions into ostensibly open specifications. The mechanism
 *   is simple: resource advantage in participation translates directly to
 *   agenda-setting power, and the openness commitment itself prevents
 *   resistance from coalescing (dissent is framed as a violation of
 *   collaborative norms). Small implementers and open-source projects must
 *   implement these extensions to remain interoperable, creating a transfer
 *   of implementation cost from well-resourced operators to
 *   resource-constrained actors. The sibling readings (commons stewardship
 *   and legitimacy erosion) emphasize different aspects of the same
 *   institution — this reading emphasizes the extraction mechanism that the
 *   commitment's procedural guarantees enable rather than prevent.
 *
 * KEY AGENTS:
 *   - large_platform_operators: institutional power, arbitrage exit, direct capture of extension requirements
 *   - small_device_implementers: moderate power, constrained exit, forced implementation of vendor requirements
 *   - open_source_projects: moderate power, identity-locked exit, moral hazard between mission and interoperability necessity
 *   - IETF working group chairs: powerful, mobile, structural position to amplify or resist large-operator agenda
 *   - end users: powerless, trapped, bearing cost of fragmented interoperability through device constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__capture_substrate_reading, 0.62).
domain_priors:suppression_score(ietf_openness_commitment__capture_substrate_reading, 0.58).
domain_priors:theater_ratio(ietf_openness_commitment__capture_substrate_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(ietf_openness_commitment__capture_substrate_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__capture_substrate_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__capture_substrate_reading, "IETF Standards Process as Proprietary Extension Substrate").
narrative_ontology:topic_domain(ietf_openness_commitment__capture_substrate_reading, "technology/governance/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__capture_substrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__capture_substrate_reading, 'a6a51767-1335-4fac-a8f1-214366e81b65').
narrative_ontology:cs_kernel_codification('a6a51767-1335-4fac-a8f1-214366e81b65', formalized).
narrative_ontology:cs_authority_grounding('a6a51767-1335-4fac-a8f1-214366e81b65', lineage).
narrative_ontology:cs_interpretation_layer_present('a6a51767-1335-4fac-a8f1-214366e81b65').
narrative_ontology:cs_reading_relation('a6a51767-1335-4fac-a8f1-214366e81b65', ietf_openness_commitment__commons_stewardship_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6a51767-1335-4fac-a8f1-214366e81b65', ietf_openness_commitment__legitimacy_erosion_reading, influences).
narrative_ontology:cs_axiom('a6a51767-1335-4fac-a8f1-214366e81b65', foundational, resource_advantage_converts_to_agenda_power).
narrative_ontology:cs_axiom_status(resource_advantage_converts_to_agenda_power, holdable).
narrative_ontology:cs_axiom_grounding('a6a51767-1335-4fac-a8f1-214366e81b65', resource_advantage_converts_to_agenda_power, empirically_contingent).
narrative_ontology:cs_axiom('a6a51767-1335-4fac-a8f1-214366e81b65', secondary, openness_commitment_prevents_coordinated_resistance).
narrative_ontology:cs_axiom_status(openness_commitment_prevents_coordinated_resistance, holdable).
narrative_ontology:cs_axiom_grounding('a6a51767-1335-4fac-a8f1-214366e81b65', openness_commitment_prevents_coordinated_resistance, deontological).
narrative_ontology:cs_reference_frame('a6a51767-1335-4fac-a8f1-214366e81b65', rough_consensus_open_participation_meritocracy).
narrative_ontology:cs_drift_state('a6a51767-1335-4fac-a8f1-214366e81b65', contemporary_large_operator_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6a51767-1335-4fac-a8f1-214366e81b65', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, small_device_implementers).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, open_source_projects).
narrative_ontology:constraint_victim(ietf_openness_commitment__capture_substrate_reading, end_users_interoperability_dependent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__capture_substrate_reading, end_users_interoperability_dependent).
narrative_ontology:constraint_vindicates(ietf_openness_commitment__capture_substrate_reading, rough_consensus_meritocracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major technology companies (Apple, Google, Amazon, Meta, Microsoft) participate heavily in IETF working groups, contribute substantial engineering resources, and propose extensions that lock interoperability features to their platforms. They can embed proprietary requirements into standards drafts and justify them as security, performance, or ecosystem necessity. Their market position means implementers must support their extensions or lose access to large user populations.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, large_platform_operators, beneficiary).

% Makers of IoT devices, routers, embedded systems, and smaller consumer electronics must implement not only the published standard but also the vendor-specific extensions to interoperate meaningfully in deployed networks. They lack the resources to propose competing extensions or influence working group consensus. Implementing proprietary extensions adds engineering cost, testing burden, and technical debt.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, small_device_implementers, payer,
    moderate, biographical, constrained, global).

% Community-driven implementations (libcurl, OpenSSL, Chromium ecosystem) must support published standards plus proprietary extensions to remain viable in practice. Their contributors are ideologically committed to open development; they cannot refuse the extensions without losing interoperability, but implementing them contradicts their open-source mission. This creates moral hazard and contributor burnout as the project becomes a translation layer for proprietary constraints.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, open_source_projects, payer,
    moderate, biographical, identity_locked, global).

% Users depend on the internet standards to work across devices. When extensions fragment the standard, users are trapped in platform silos or forced to adopt devices supporting the dominant extensions. They benefit from the coordinated baseline standard but bear the cost of its fragmentation through device choice restrictions and higher switching costs.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, end_users_interoperability_dependent, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ietf_openness_commitment__capture_substrate_reading, end_users_interoperability_dependent, beneficiary).

% Volunteer and employed chairs of standards working groups are responsible for driving consensus and advancing specifications. They face pressure from large implementers to accommodate their requirements, pressure from smaller players to resist lock-in, and incentive structures that reward productivity (specifications shipped) over interoperability robustness. Many chairs are employed by or maintain consulting relationships with large platform operators.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_working_group_chairs, agenda_setter,
    powerful, biographical, mobile, global).

% Internet researchers who study interoperability, standards processes, and their social/economic effects are largely absent from working group participation. They produce analyses showing how proprietary extensions undermine interoperability goals, but this voice is not seated at the consensus table and cannot directly influence standards drafts. Their insights are retrospective, not prospective.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, academic_researchers_internet_standards, excluded,
    moderate, generational, mobile, global).

% The IETF's formal commitment to rough consensus, openness, and meritocracy operates as the legitimacy frame for the process. As a non-agent, it carries no interest of its own; it is the institutional form that claims to prevent the capture being analyzed.
narrative_ontology:constraint_stakeholder(ietf_openness_commitment__capture_substrate_reading, ietf_governance_structure, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(ietf_openness_commitment__capture_substrate_reading, ietf_governance_structure).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ietf_openness_commitment__capture_substrate_reading, large_platform_operators).
narrative_ontology:fixing_cost_class(ietf_openness_commitment__capture_substrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A volunteer-driven, open-participation standards process creates a single baseline protocol specification that millions of implementations can align on, avoiding Balkanization of the internet into incompatible networks. The process coordinates around technical merit and consensus-seeking rather than proprietary advantage.
% TRANSFER_FUNCTION: Large platform operators convert their resource advantage (dedicated engineering staff, deployed infrastructure, user populations) into the ability to encode platform-specific requirements into ostensibly open standards, creating de facto mandatory proprietary extensions. The transfer is from small implementers and open-source projects (who must incur implementation costs) to large operators (who gain lock-in leverage and interoperability control).
% ABSENT_VOICES: Researchers studying the sociological and economic effects of standards capture, representatives of developing-world device makers, users themselves, and small implementers who lack IETF membership or working group participation are systematically absent or marginalized. Their testimony would argue that extension proliferation contradicts the openness principle.
% DISAPPEARANCE_RATIONALE: If the IETF process and its open-standards commitment were to collapse, large platform operators would impose their own interconnection protocols on smaller implementers directly through bilateral agreements and market power. The coordination substrate would fragment into managed oligopolistic silos. The disappearance would shift costs from distributed implementation burden to explicit gatekeeping licensing.
% FOUNDING_PROBLEM: In the early internet, incompatible proprietary protocols fragmented networks. A formal, open standards process was instituted to ensure anyone could implement interoperable systems without proprietary licensing, enabling decentralized growth and competition.
% FOUNDING_PROBLEM_CORROBORATION: Large platform operators acknowledge the founding problem motivated their participation, but argue proprietary extensions are necessary for security and performance in modern deployments. Small implementers, open-source project maintainers, and interoperability researchers attest that the founding problem is being re-introduced within the standards process itself — the problem is being solved by the same institution that now instantiates it.
narrative_ontology:disappearance_verdict(ietf_openness_commitment__capture_substrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(ietf_openness_commitment__capture_substrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__capture_substrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ietf_openness_commitment__capture_substrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ietf_openness_commitment__capture_substrate_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62 at interval end) because the transfer is real but mediated through technical requirements rather than explicit fees; small implementers incur measurable engineering burden, but the extraction is distributed and obscured by the openness frame. Suppression is moderate (0.58) because the mechanism operates through consensus norms and technical merit arguments rather than coercive exclusion; dissenters can participate, but their dissent is framed as rejecting openness itself. Theater ratio is high-moderate (0.48) because the standards working group maintains genuine technical discussion and peer review, but an increasing proportion of that discussion is dedicated to accommodating vendor extensions rather than solving coordination problems. The measurement trajectory shows extraction and theater ratio both rising: as proprietary extensions accumulate, more working-group time is spent on compatibility translation than on baseline interoperability. Resistance is moderate (0.59) because small implementers and open-source maintainers actively object to extension proliferation, but lack the structural power to block adoption. The temporal gradient shows extraction rising from 0.38 to 0.62: the capture is accelerating as large operators consolidate influence and smaller players exhaust their negotiating capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the large-operator seat, the constraint is legitimate coordination: they argue their extensions solve real problems (security, performance, new capabilities) that the baseline standard cannot address, and their participation and resource contribution justify influence. From the small-implementer and open-source seat, the same structure is extractive capture: they see the extensions as bundling proprietary lock-in with technical necessity. The engine computes this divergence: the large-operator seat should compute near symmetric to beneficiary (they set the requirements and benefit from them), while the small-implementer seat should compute target (they must implement but do not set). The IETF governance structure, as a non-agent, cannot hold a seat — it is the institutional form the different seats use to justify their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Large platform operators are the structural beneficiaries: they propose extensions aligned with their platforms, engineer implementations in their deployed infrastructure, and shift the implementation burden to others. Exit for them is arbitrage — they can support open standards while maintaining proprietary extensions without contradiction, because their user base and market power create interoperability even without full standard compliance. Small implementers are the targets: they must implement both the open standard and the vendor extensions to reach the large-operator platforms' users, incurring implementation cost and technical debt. Exit for them is constrained — leaving the standard means isolation; refusing extensions means invisibility in deployed networks. Open-source projects face identity_locked exit: they are ideologically committed to open development, and refusing proprietary extensions is internally coherent, but doing so fragments the ecosystem they depend on for relevance and user base. End users are trapped — they bear the interoperability cost through device choice restrictions without making the architectural choices that generate the cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is NOT in mandatrophy — the founding problem (incompatible proprietary protocols) is actively being re-instantiated within the standards process, which means the process's original function is being overrun by the extraction it enables. A mandatrophy reading would require the founding problem to be dead (solved) while the arrangement persists for performance alone. Here the founding problem is contested: large operators claim the problem is solved and extensions are necessary for new capabilities; implementers claim the problem is being reborn as extension fragmentation. The mandatrophy analysis in this case is whether the process will eventually fail (small implementers abandon it for bilateral agreements with large operators) or be reformed (the capture substrate becomes explicit, and proprietary extensions are decoupled from standards status). The theater_ratio rise shows the process is spending increasing effort on capture-accommodation rather than baseline coordination, which is a drift signal but not mandatrophy per se.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extension_necessity_vs_lock_in,
    'To what extent are vendor-specific extensions technically necessary (solving real security/performance gaps) versus strategically motivated (encoding platform lock-in)?',
    'Empirical analysis of extension adoption patterns: do extensions address problems that smaller implementers also solve through open mechanisms, or do they solve problems unique to large-operator requirements? Do alternatives to proprietary extensions exist but are deprioritized in favor of vendor proposals?',
    'If extensions are primarily lock-in, the extraction is transparent and the mandatrophy clock starts ticking (the founding problem is being reborn). If extensions are primarily necessary, part of the measured extractiveness is legitimate coordination cost, not pure transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extension_necessity_vs_lock_in, empirical, 'Whether extensions serve genuine technical gaps or are mechanisms for platform lock-in.').

omega_variable(
    capture_mechanism_vs_norm_infection,
    'Is the large-operator influence exerted primarily through formal participation (working group contributions) or through normative capture (the procedural commitment to openness prevents organized resistance)?',
    'Discourse analysis of rejected proposals and dissenting positions: when small implementers or researchers propose competing approaches, are they rejected on technical merit or on grounds of violating openness norms (e.g., being too divisive)? Do larger operators face different enforcement of consensus norms?',
    'If normative capture is primary, the suppression score understates the constraint''s coercive force — dissent is suppressed by reframing it as norm violation rather than by explicit exclusion. If formal participation is primary, the suppression score is more accurate and reform is possible through process change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capture_mechanism_vs_norm_infection, conceptual, 'Whether large-operator influence operates through explicit participation or through normative redefinition of legitimate dissent.').

omega_variable(
    commons_stewardship_coexistence,
    'Is the capture_substrate reading compatible with the commons_stewardship reading, or do they foreclose each other?',
    'Logical examination: the commons_stewardship reading holds that standards are public infrastructure and should serve interoperability for all. The capture_substrate reading holds that the process enables large operators to encode private requirements. Can both be true simultaneously — i.e., is the constraint capturing public infrastructure or substituting for it?',
    'If coexistent, both readings are live and the kernel exhibits institutional hybridity: a public-infrastructure commitment that is simultaneously being used for private extraction. If they foreclose, only one reading''s structural analysis is valid, and the kernel is internally contradictory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commons_stewardship_coexistence, conceptual, 'Compatibility of the capture reading with the public-infrastructure reading of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__capture_substrate_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t0, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(ietf_tr_t0, observed).
narrative_ontology:measurement(ietf_tr_t5, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(ietf_tr_t5, observed).
narrative_ontology:measurement(ietf_tr_t10, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(ietf_tr_t10, observed).
narrative_ontology:measurement(ietf_tr_t15, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(ietf_tr_t15, observed).
narrative_ontology:measurement(ietf_tr_t20, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement_basis(ietf_tr_t20, observed).
narrative_ontology:measurement(ietf_tr_t25, ietf_openness_commitment__capture_substrate_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(ietf_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ietf_be_t0, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(ietf_be_t0, observed).
narrative_ontology:measurement(ietf_be_t5, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(ietf_be_t5, observed).
narrative_ontology:measurement(ietf_be_t10, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(ietf_be_t10, observed).
narrative_ontology:measurement(ietf_be_t15, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(ietf_be_t15, observed).
narrative_ontology:measurement(ietf_be_t20, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(ietf_be_t20, observed).
narrative_ontology:measurement(ietf_be_t25, ietf_openness_commitment__capture_substrate_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(ietf_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t0, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(ietf_su_t0, observed).
narrative_ontology:measurement(ietf_su_t5, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(ietf_su_t5, observed).
narrative_ontology:measurement(ietf_su_t10, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(ietf_su_t10, observed).
narrative_ontology:measurement(ietf_su_t15, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(ietf_su_t15, observed).
narrative_ontology:measurement(ietf_su_t20, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(ietf_su_t20, observed).
narrative_ontology:measurement(ietf_su_t25, ietf_openness_commitment__capture_substrate_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(ietf_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__capture_substrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__capture_substrate_reading, 0.12).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, ietf_openness_commitment__legitimacy_erosion_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, browser_vendor_interoperability_lock_in).
narrative_ontology:affects_constraint(ietf_openness_commitment__capture_substrate_reading, tls_extension_fragmentation).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the ietf_openness_commitment kernel. The capture_substrate reading emphasizes how resource advantage converts to agenda power; the commons_stewardship reading emphasizes interoperability as public good; the legitimacy_erosion reading emphasizes vulnerability of the consensus mechanism itself. All three share the same referent (the IETF process and its openness commitment) but instantiate different structural analyses and ε values. Each reading is a separate constraint story with its own beneficiary/victim structure and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ietf_openness_commitment__capture_substrate_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
