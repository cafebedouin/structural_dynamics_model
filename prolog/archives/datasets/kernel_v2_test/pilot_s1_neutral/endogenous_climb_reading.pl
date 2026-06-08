% ============================================================================
% CONSTRAINT STORY: endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_endogenous_climb_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: endogenous_climb_reading
 *   human_readable: Endogenous Climb: Bottom-Up Norm Adoption Preceding State Mandate
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the structural dynamics of norm adoption when
 *   legitimacy flows bottom-up: decentralized populations voluntarily adopt a
 *   new norm through social learning, network effects, and coordination
 *   around shared understandings of value or necessity. The norm stabilizes
 *   through widespread practice before the state formalizes it into law. The
 *   key structural claim is that the state enters as a recorder and
 *   coordinator of already-popular practice, not as an imposer of alien
 *   requirements. The constraint exhibits low extractiveness (0.15) because
 *   no agent is using coercion or suppressing alternatives to capture rents —
 *   the norm spreads through persuasion and voluntary adoption. Theater ratio
 *   is low (0.25) because the adoption process is substantively about
 *   changing practice, not about the appearance of legitimacy covering
 *   coercive imposition. This reading instantiates the committer axis's
 *   'endogenous climb' framework: norms emerge from the ground and climb
 *   toward state legitimation, not descend from state authority.
 *
 * KEY AGENTS:
 *   - Decentralized Adopter Coalitions: Primary beneficiary (powerless/mobile) — gain coordination benefits from norm stabilization with zero enforcement cost; can exit costlessly
 *   - Norm Entrepreneurs: Secondary beneficiary (moderate/mobile) — gain status and network effects from leading adoption; bear communication costs but no coercive enforcement
 *   - State Institutional Apparatus: Coordinating actor (organized/constrained) — formalize and enforce norm after adoption is complete; gain legitimacy by aligning with popular practice; constrained by commitment to follow rather than lead
 *   - Analytical Observer: Civilizational analytical perspective (institutional/arbitrage) — sees constraint as pure coordination with distributed benefits and low coercive overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endogenous_climb_reading, 0.15).
domain_priors:suppression_score(endogenous_climb_reading, 0.1).
domain_priors:theater_ratio(endogenous_climb_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endogenous_climb_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(endogenous_climb_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(endogenous_climb_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endogenous_climb_reading, rope).
narrative_ontology:human_readable(endogenous_climb_reading, "Endogenous Climb: Bottom-Up Norm Adoption Preceding State Mandate").
narrative_ontology:topic_domain(endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(endogenous_climb_reading, 'f0ec83f1-f339-4265-b135-b70a5346f12e').
narrative_ontology:cs_kernel_codification('f0ec83f1-f339-4265-b135-b70a5346f12e', formalized).
narrative_ontology:cs_authority_grounding('f0ec83f1-f339-4265-b135-b70a5346f12e', lineage).
narrative_ontology:cs_interpretation_layer_present('f0ec83f1-f339-4265-b135-b70a5346f12e').
narrative_ontology:cs_reading_relation('f0ec83f1-f339-4265-b135-b70a5346f12e', endogenous_climb_reading__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('f0ec83f1-f339-4265-b135-b70a5346f12e', endogenous_climb_reading__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('f0ec83f1-f339-4265-b135-b70a5346f12e', foundational, popular_adoption_precedes_state_authority).
narrative_ontology:cs_axiom_status(popular_adoption_precedes_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('f0ec83f1-f339-4265-b135-b70a5346f12e', popular_adoption_precedes_state_authority, empirically_contingent).
narrative_ontology:cs_axiom('f0ec83f1-f339-4265-b135-b70a5346f12e', foundational, state_follows_rather_than_leads).
narrative_ontology:cs_axiom_status(state_follows_rather_than_leads, holdable).
narrative_ontology:cs_axiom_grounding('f0ec83f1-f339-4265-b135-b70a5346f12e', state_follows_rather_than_leads, deontological).
narrative_ontology:cs_reference_frame('f0ec83f1-f339-4265-b135-b70a5346f12e', bottom_up_coordination_equilibrium).
narrative_ontology:cs_drift_state('f0ec83f1-f339-4265-b135-b70a5346f12e', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('f0ec83f1-f339-4265-b135-b70a5346f12e', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, decentralized_adopter_coalitions).
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, norm_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, adopter_coalitions).
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, state_institutional_apparatus).
narrative_ontology:constraint_vindicates(endogenous_climb_reading, bottom_up_legitimacy_thesis).
narrative_ontology:constraint_vindicates(endogenous_climb_reading, popular_coordination_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decentralized groups voluntarily adopt the norm through social learning and network effects. They coordinate their practice around a shared understanding without requiring centralized authority. They benefit from norm stabilization (reduced coordination uncertainty, social alignment). Exit is trivial — they can decline adoption or abandon practice without legal penalty during the pre-formalization period. After state formalization, exit carries modest social cost but no legal penalty in the endogenous climb reading (the state is recording popular will, not imposing alien rule).
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, adopter_coalitions, beneficiary,
    powerless, generational, mobile, regional).

% Individuals and networks who propagate the norm through persuasion, modeling, and social demonstration. They set the adoption agenda by making the norm salient and desirable. They benefit from the norm's spread through status gains, network expansion, and institutional access (e.g., becoming known as an expert or leader in the new practice). Exit is available — they can stop promoting the norm; most continue because their identity and career are invested in leading adoption.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, norm_entrepreneurs, beneficiary,
    moderate, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(endogenous_climb_reading, norm_entrepreneurs, agenda_setter).

% The state enters after popular adoption is substantially complete. Its role is to formalize the norm into law, coordinate enforcement, and provide the legal authority that clarifies obligations and procedures. The state benefits from legitimacy (by aligning with popular practice, it claims to represent the people's will, not impose alien rule). The state is constrained by its commitment to follow rather than lead — once it formalizes a norm, it is bound to enforce it, and it cannot easily reverse course without losing the legitimacy it gained by formalizing popular will.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, state_institutional_apparatus, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(endogenous_climb_reading, state_institutional_apparatus, agenda_setter).

% Individuals and groups who reject or do not adopt the emerging norm. In the endogenous climb reading, they face social pressure (ridicule, exclusion from networks, reduced status) but no legal penalty until state formalization. After formalization, they may face modest legal consequences (fines, license restrictions) but not imprisonment or coercion to actively perform the new norm. In this reading, they are not victims — they retain agency and can choose rejection; they bear costs but not extraction through suppression.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, alternative_norm_holders, excluded,
    powerless, generational, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: How can decentralized populations synchronize their practice around a new norm without centralized authority imposing it? The constraint solves this by enabling voluntary adoption through social learning, network effects, and demonstration effects. Norm entrepreneurs make the norm salient and desirable; adopters learn by observing and imitating peers; coordinated practice emerges from distributed decision-making.
% TRANSFER_FUNCTION: What moves from whom to whom? Status and network access move from late adopters to early adopters and norm entrepreneurs (early adopters gain prestige as pioneers). Behavioral alignment moves from all adopters to the collective (heterogeneous practice converges to homogeneous norm). Legitimacy moves from popular practice to the state (the state gains authority by formalizing what populations already do).
% ABSENT_VOICES: Beneficiaries of the old norm (those whose status or advantage depended on the previous practice) are excluded from the adoption conversation. Conservative voices that prefer stability over change are underrepresented in the adoption narrative (framed as resistance rather than valid caution). Structural beneficiaries of the norm's imposition (e.g., firms that profit from enforcement) are not present as advocates during the bottom-up phase because the norm's legitimacy derives from grassroots adoption, not top-down imposition. These voices appear only after state formalization, and then they are portrayed as implementing the popular will rather than capturing state authority.
% DISAPPEARANCE_RATIONALE: If the endogenous adoption mechanism disappeared overnight (populations stopped adopting, norm entrepreneurs stopped promoting), the world would partially rearrange: the new norm would not be formalized, the state would not have the legitimacy it gained through alignment with popular practice, and coordination would break down. However, the material conditions that made the norm valuable (environmental pressures, technological changes, social problems) would remain, so some alternative coordination mechanism might emerge. The disappearance would be consequential but not catastrophic — the constraint is coordination, not an absolute necessity.
% FOUNDING_PROBLEM: How can new norms (e.g., environmental practices, digital literacy, workplace safety beyond legal minima) spread and stabilize in populations without centralized state mandate? The founding problem is one of distributed coordination: populations need to synchronize around shared understandings without coercive enforcement. Market incentives and social learning are the solving mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary examples corroborate: environmental norm adoption (recycling, conservation) spread widely before state regulation formalized them; digital literacy practices (password management, cybersecurity) spread through peer networks before government training programs; workplace safety beyond legal minima (safety culture, reporting norms) often emerge from worker coalitions before regulatory capture. The adopter coalitions and norm entrepreneurs themselves attest that bottom-up adoption was the mechanism. State officials often acknowledge they are formalizing already-popular norms (e.g., environmental officials noting that public demand drove legislation). However, no neutral third-party empirical study of causal ordering exists for most cases — corroboration is self-reported and retrospective.
narrative_ontology:disappearance_verdict(endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(endogenous_climb_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADOPTER COALITION (ROPE) — Decentralized groups voluntarily adopt the norm through social coordination. No coercion required; exit is trivial (they simply stop adopting). Benefits from norm stabilization without bearing enforcement costs. Pure coordination game from their structural seat.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: NORM ENTREPRENEURS (ROPE) — Individuals and networks who propagate the norm through persuasion, modeling, and social influence. Benefit from the norm's adoption without requiring coercive authority. Exit available (they can stop promoting); most remain because they are persuaded of the norm's value or because their status depends on leading adoption.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE AS LEGITIMACY-FOLLOWER (ROPE) — The state enters after popular adoption is substantially complete, formalizing and coordinating an already-stable norm. The state faces constraint (codifying a popular norm as law makes the state legible and accountable for enforcement); it also benefits (gains legitimacy by aligning with popular practice). The state is not extracting — it is a latecomer to coordination that it did not initiate.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical seat, the constraint is pure coordination with minimal extraction: multiple agents solve a collective action problem through distributed adoption and social learning. Low theater (the adoption process is genuine, not performative). Low enforcement cost (state acts as registrar, not imposer). Low suppression (alternatives remain available — agents can refuse adoption without legal penalty). The constraint succeeds through alignment of incentives, not through coercion. The engine should classify this as Rope across all perspectives.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(endogenous_climb_reading_tests).
:- end_tests(endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. In the endogenous climb reading, no agent uses coercion or suppresses alternatives to extract rents. Norm entrepreneurs benefit from status/network effects, but these accrue through genuine persuasion, not force. The state benefits from legitimacy, but enters after adoption is stable. The adopters benefit from coordination and gain alignment with social practice. The slight non-zero value reflects that early adopters may gain status advantages and norm entrepreneurs may extract some attention rents, but these are coordination byproducts, not the constraint's primary function. Suppression (0.10): Low. Alternatives remain available throughout the adoption phase — agents who reject the norm face social pressure but not legal penalty until state formalization. Even after formalization, suppression is modest because the norm is already popular and enforcement is light (state acts as coordinator, not coercer). Theater ratio (0.25): Low. The adoption process is substantively about changing behavior and belief, not about performative legitimacy theater. Agents genuinely shift their practice because they are persuaded or coordinated, not because they are forced to perform compliance while maintaining hidden resistance.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on Rope classification. There is minimal perspectival gap because the constraint exhibits the same structural properties (low extraction, low suppression, voluntary adoption, distributed benefits) from every observation point. The adopters see coordination. The entrepreneurs see status-building. The state sees legitimacy through alignment with popular will. The analytical observer sees a pure coordination game. The absence of gap is itself the signal: where all perspectives classify identically as Rope, the constraint is not hiding extraction in one perspective while claiming coordination in another. This is diagnostic of genuine coordination, not false-summit naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from power/exit combinations and beneficiary/victim status. Adopter coalitions have low power but mobile exit — they can refuse adoption without penalty. They benefit from coordination (low d toward beneficiary end). Norm entrepreneurs have moderate power and mobile exit — they benefit from promotion (low d). The state has organized power but constrained exit — once formalized, the state is committed to enforcement; however, the state is not victimized by the constraint (constrained exit + beneficiary status → moderate d, not high extraction). The analytical observer at civilizational scope sees all participants as net beneficiaries with minimal coercive overhead (d cluster near beneficiary end, low effective extraction chi). If the constraint were genuinely extractive, we would expect high-power agents to use suppression to prevent exit and concentrated rents — neither appears in the endogenous climb reading.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE PERSISTENCE: The founding mandate of the constraint is to solve a coordination problem — how should adopters synchronize their practice around a shared norm without centralized authority? This mandate remains live: bottom-up norm adoption is still the primary mechanism by which many social norms (environmental practices, digital literacy, workplace safety beyond legal minima) spread. The state's formal codification extends the mandate by adding enforcement and clarity, but does not replace the bottom-up coordination function. The constraint does not exhibit mandatrophy because its founding purpose (decentralized coordination) and its current operation (distributed adoption followed by state formalization) remain aligned. No gap exists between what the constraint was built to do and what it actually does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contingency_endogenous,
    'Is the bottom-up adoption genuinely endogenous (emergent from distributed decision-making), or does it reflect prior state capacity that made adoption salient or valuable?',
    'Historical tracing of adoption chronology: identify whether state institutional presence or prior state-sponsored actors preceded norm spread. Distinguish adoption driven by material conditions / social needs from adoption driven by state-prepared institutional scaffolding.',
    'If truly endogenous: Rope classification holds (pure coordination). If prior state machinery shaped adoption incentives: constraint is Tangled Rope (state-directed coordination masked as voluntary). This is the core ambiguity between this reading and exogenous_override_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_endogenous, empirical, 'Whether bottom-up adoption is truly endogenous or shaped by prior state capacity').

omega_variable(
    state_mandate_causal_timing,
    'Does state formalization of the norm actually follow adoption, or does the appearance of following mask simultaneous causal influence?',
    'Archival analysis of state decision-making timelines: when did legislators/officials begin drafting formalizations, and relative to what adoption milestone? Cross-check against contemporaneous advocacy and lobbying records. Distinguish formal codification date from effective implementation date.',
    'If state genuinely follows adoption: Rope classification (state as recorder of popular will). If state is contemporaneous agent influencing adoption to make formal codification inevitable: Tangled Rope or Snare (state as hidden architect). This determines whether the reading accurately describes the constraint or misidentifies causal order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_mandate_causal_timing, empirical, 'Causal ordering and timing of state mandate relative to adoption').

omega_variable(
    asymmetry_among_adopters,
    'Does the bottom-up adoption distribute its benefits and costs uniformly across adopter groups, or does early adoption by some groups create asymmetric advantage that later groups must overcome?',
    'Comparative analysis of early vs late adopter groups: track status gains, market share, institutional access, and social position before and after adoption. Identify whether early adopters extract rents from latecomers or whether benefits are genuinely collective.',
    'If uniform: pure Rope (collective coordination). If asymmetric: constraint is Tangled Rope at minimum (early adopters benefit, later adopters pay conformity costs). This resolves whether ''bottom-up'' genuinely means non-extractive or masks intra-coalition extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_among_adopters, empirical, 'Distribution of benefits and costs across early and late adopters').

omega_variable(
    sibling_reading_ambiguity,
    'This reading (endogenous_climb) claims state mandate followed bottom-up adoption. The sibling reading (exogenous_override) claims state imposed a norm against initial resistance. What observable distinguishes a genuinely bottom-up case from a framing that reinterprets top-down imposition as grassroots?',
    'This is the committer-axis ambiguity: the same historical case can be read either way depending on which actors'' stated preferences are treated as authoritative and which are dismissed as false consciousness or coercion. Resolution requires explicit choice of reference frame: Do adopters'' stated reasons for adoption count as evidence? If so, exogenous_override is foreclosed. If stated reasons are treated as rationalization of imposed necessity, endogenous_climb is undermined. Frame choice is not empirical — it is normative.',
    'This omega resolves to the cs_structure.reading_relations and cs_structure.axioms blocks: exogenous_override FORECLOSES endogenous_climb IF the reading commits to valuing state-imposed enforcement as the definitive causal factor. Endogenous_climb FORECLOSES exogenous_override IF the reading commits to bottom-up adoption as sufficient causal explanation. The two cannot coexist in a single framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_ambiguity, conceptual, 'Frame ambiguity between bottom-up and top-down readings of the same historical case').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endogenous_climb_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endoclimb_theater_t0, endogenous_climb_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(endoclimb_theater_t5, endogenous_climb_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(endoclimb_theater_t10, endogenous_climb_reading, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(endoclimb_extract_t0, endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(endoclimb_extract_t5, endogenous_climb_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(endoclimb_extract_t10, endogenous_climb_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(endoclimb_suppress_t0, endogenous_climb_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(endoclimb_suppress_t5, endogenous_climb_reading, suppression_requirement, 5, 0.08).
narrative_ontology:measurement(endoclimb_suppress_t10, endogenous_climb_reading, suppression_requirement, 10, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endogenous_climb_reading, information_standard).
narrative_ontology:affects_constraint(endogenous_climb_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(endogenous_climb_reading, hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel decomposes into three structurally distinct constraints, each instantiating a different causal ordering of state authority and popular adoption. Endogenous_climb_reading (this story) models bottom-up adoption preceding state mandate. Exogenous_override_reading models state imposition preceding popular acceptance. Hybrid_legitimation_reading models simultaneous bidirectional influence. Each reading has its own ε value, its own beneficiary/victim structure, and its own classification across perspectives. The three are linked via reading_relations in cs_structure (one reading may foreclose others or coexist with them depending on the choice of authority framework). Network affects_constraints links identify which sibling readings this story structurally influences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
