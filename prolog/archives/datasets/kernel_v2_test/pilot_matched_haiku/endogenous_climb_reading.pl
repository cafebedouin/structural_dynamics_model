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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   This constraint models the endogenous-climb reading of the
 *   imposition-mechanism kernel: norms achieve legitimacy through bottom-up
 *   adoption by communities solving genuine coordination problems, and the
 *   state's mandate follows rather than precedes popular acceptance. The
 *   reading asserts that legitimacy flows from consent and functional
 *   benefit, not from coercive authority. The state acts as a coordinator and
 *   formalizer of norms already embedded in social practice, rather than as
 *   an imposer of external rules. This reading contrasts with the
 *   exogenous-override reading (state imposes norms that communities resist
 *   or adopt under coercion) and the hybrid-legitimation reading (state
 *   signals influence adoption from the start). The endogenous-climb reading
 *   predicts low enforcement costs, rapid adoption timelines, minimal
 *   resistance, and state action as coordination rather than coercion. The
 *   constraint exhibits pure Rope classification across all perspectives:
 *   communities benefit from coordination, the state benefits from legitimacy
 *   alignment, norm entrepreneurs benefit from social coordination, and the
 *   analytical observer sees no extraction mechanism.
 *
 * KEY AGENTS:
 *   - Adopting Communities: Primary beneficiary (powerless/mobile) — solve genuine coordination problems through norm adoption; experience low extraction because the norm is functionally beneficial
 *   - State Legitimacy Apparatus: Secondary beneficiary (institutional/arbitrage) — formalizes norms already adopted; benefits from legitimacy alignment without coercive overhead
 *   - Norm Entrepreneurs: Organized agents (organized/constrained) — champion norm adoption within networks; benefit from social coordination and reputation gains
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees pure coordination mechanism with minimal extraction or suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endogenous_climb_reading, 0.15).
domain_priors:suppression_score(endogenous_climb_reading, 0.08).
domain_priors:theater_ratio(endogenous_climb_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endogenous_climb_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(endogenous_climb_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(endogenous_climb_reading, theater_ratio, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endogenous_climb_reading, rope).
narrative_ontology:human_readable(endogenous_climb_reading, "Endogenous Climb: Bottom-Up Norm Adoption Preceding State Mandate").
narrative_ontology:topic_domain(endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(endogenous_climb_reading, '2e743207-c109-44a9-b9ac-8f0d8c7a1288').
narrative_ontology:cs_kernel_codification('2e743207-c109-44a9-b9ac-8f0d8c7a1288', distributed).
narrative_ontology:cs_authority_grounding('2e743207-c109-44a9-b9ac-8f0d8c7a1288', practice).
narrative_ontology:cs_interpretation_layer_present('2e743207-c109-44a9-b9ac-8f0d8c7a1288').
narrative_ontology:cs_reading_relation('2e743207-c109-44a9-b9ac-8f0d8c7a1288', endogenous_climb_reading__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e743207-c109-44a9-b9ac-8f0d8c7a1288', endogenous_climb_reading__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('2e743207-c109-44a9-b9ac-8f0d8c7a1288', foundational, organic_coordination_legitimacy).
narrative_ontology:cs_axiom_status(organic_coordination_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2e743207-c109-44a9-b9ac-8f0d8c7a1288', organic_coordination_legitimacy, conventional).
narrative_ontology:cs_axiom('2e743207-c109-44a9-b9ac-8f0d8c7a1288', foundational, state_as_coordinator_not_coercer).
narrative_ontology:cs_axiom_status(state_as_coordinator_not_coercer, holdable).
narrative_ontology:cs_axiom_grounding('2e743207-c109-44a9-b9ac-8f0d8c7a1288', state_as_coordinator_not_coercer, instrumental).
narrative_ontology:cs_reference_frame('2e743207-c109-44a9-b9ac-8f0d8c7a1288', organic_community_coordination).
narrative_ontology:cs_drift_state('2e743207-c109-44a9-b9ac-8f0d8c7a1288', post_state_formalization, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2e743207-c109-44a9-b9ac-8f0d8c7a1288', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, adopting_communities).
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, state_legitimacy_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, norm_entrepreneurs).
narrative_ontology:constraint_vindicates(endogenous_climb_reading, organic_social_coordination_hypothesis).
narrative_ontology:constraint_vindicates(endogenous_climb_reading, legitimacy_through_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities face genuine coordination problems (resource allocation, dispute resolution, collective action) that norms solve. They adopt norms voluntarily because the norms work — they reduce transaction costs, enable cooperation, and provide shared frameworks for action. Communities can exit by abandoning the norm if it stops working, but they choose to maintain it because it remains functionally beneficial. The norm spreads through social proof and imitation as other communities observe its success.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, adopting_communities, beneficiary,
    powerless, generational, mobile, regional).

% The state observes norms already adopted and embedded in community practice. It formalizes these norms through law, policy, or official recognition. The state's role is coordination and legitimacy alignment — by codifying what communities already do, the state aligns its authority with popular practice, strengthening legitimacy without requiring coercion. The state benefits from this alignment: it appears responsive to popular will, it reduces enforcement costs, and it consolidates authority through consent rather than force.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, state_legitimacy_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(endogenous_climb_reading, state_legitimacy_apparatus, beneficiary).

% Community leaders, cultural figures, and religious authorities champion norm adoption within their networks. They benefit from the coordination function (their communities function better with the norm) and from reputation gains (they are seen as wise leaders who guide their communities toward beneficial practices). They are constrained by social capital and reputation — if they champion a norm that fails, they lose standing — but they benefit from successful norm adoption.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, norm_entrepreneurs, beneficiary,
    organized, biographical, constrained, regional).

% The analytical observer sees the constraint as a pure coordination mechanism: communities solve collective action problems through norm adoption, the state formalizes what communities already want, and legitimacy flows from alignment between authority and popular practice. No extraction mechanism is visible; suppression is minimal; the state acts as a coordinator rather than a coercer. This perspective is the analytical baseline for the endogenous-climb reading.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(endogenous_climb_reading, analytical_observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Communities face genuine coordination problems (resource allocation, dispute resolution, collective action, identity maintenance) that norms solve by providing shared frameworks for action and reducing transaction costs. The constraint solves the problem of how to achieve coordination without centralized authority.
% TRANSFER_FUNCTION: The constraint transfers legitimacy from communities to the state: communities adopt norms and embed them in practice; the state formalizes these norms and claims authority to coordinate them. The transfer is bidirectional — communities gain the state's enforcement capacity; the state gains legitimacy from alignment with popular practice.
% ABSENT_VOICES: Voices absent from the endogenous-climb reading include those who would argue that state formalization corrupts organic norms, that the state's role is always coercive even when disguised as coordination, or that communities never truly consent but are manipulated by norm entrepreneurs. These voices would push toward the exogenous-override or hybrid-legitimation readings. They are not represented in this reading's stakeholder set.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the state ceased to formalize norms and communities ceased to adopt them — the world would rearrange significantly. Communities would lose the coordination benefits of shared norms; the state would lose legitimacy from alignment with popular practice; social cooperation would become more costly and less stable. The constraint is not a natural law but a contingent institutional arrangement that communities and states depend on.
% FOUNDING_PROBLEM: The founding problem is the coordination challenge: how do communities achieve cooperation and collective action without centralized authority? How does the state gain legitimacy without coercion? The endogenous-climb reading asserts that norms solve this problem organically — communities adopt norms because they work, and the state formalizes them to align authority with popular practice.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical sociologists (Weber on legitimacy, Ostrom on polycentric governance), anthropologists (studying norm emergence in stateless societies), and institutional economists (studying coordination mechanisms). The endogenous-climb mechanism is corroborated by cases of norm adoption preceding state formalization (e.g., common-law development, customary law in colonial contexts, professional norms in academic communities). However, the exogenous-override reading is corroborated by cases of state-imposed norms with community resistance (e.g., colonial law, authoritarian mandates). The founding problem remains live and contested across different historical cases.
narrative_ontology:disappearance_verdict(endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(endogenous_climb_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADOPTING COMMUNITIES (ROPE) — Powerless agents with mobile exit options who voluntarily adopt norms because they solve genuine coordination problems within their communities. No coercion required; adoption spreads through social proof and functional benefit. Low extraction because the communities are net beneficiaries of the coordination mechanism itself.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: STATE LEGITIMACY APPARATUS (ROPE) — Institutional actor with arbitrage options that benefits from codifying norms already adopted bottom-up. The state's role is coordination and formalization, not coercion. Extraction is minimal because the state is formalizing what communities already want; the state's benefit is legitimacy through alignment with popular practice, not rent extraction.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: NORM ENTREPRENEURS (ROPE) — Organized agents (community leaders, cultural figures, religious authorities) who champion the norm's adoption within their networks. Constrained by social capital and reputation but benefit from the coordination function. Experience the constraint as pure coordination with minimal extraction.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, this constraint exemplifies pure coordination: a norm solves a collective action problem, spreads through voluntary adoption, and is later formalized by the state. No extraction mechanism is visible; suppression is minimal; the state acts as a coordinator rather than a coercer. The constraint is a canonical rope.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
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
 *   Extractiveness (0.15): Low. The constraint exhibits minimal extraction because the state is formalizing norms communities already want. The small non-zero value reflects the state's modest benefit from legitimacy alignment and the slight asymmetry in who gets to formalize (state) versus who originated (communities). But this is coordination benefit, not extraction. Suppression (0.08): Very low. Communities adopt voluntarily; no coercion is required. The small non-zero value reflects minor barriers to adoption (communication costs, coordination challenges) rather than active suppression. Theater ratio (0.12): Very low. The constraint is functionally transparent — norms work because they solve real problems, not because they are performed. The small non-zero value reflects the inevitable performative element in any formalization (ceremonies, announcements) but the core mechanism is functional, not theatrical. The trajectory shows slight increases over the interval as the state's formalization role becomes more visible, but all values remain low, consistent with a pure Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap because all perspectives classify as Rope. The adopting communities, the state, norm entrepreneurs, and the analytical observer all see the same structural reality: a coordination mechanism with low extraction and minimal suppression. The absence of perspectival divergence is itself diagnostic — it indicates a genuine coordination constraint rather than a mixed or extractive one. If perspectives diverged significantly (some seeing Snare, others seeing Rope), the constraint would be Tangled Rope or a false-summit candidate. The uniformity of classification here is evidence for the endogenous-climb reading's structural claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is uniformly low across all perspectives because all agents are beneficiaries of the coordination mechanism. Communities with mobile exit options benefit from functional norms and experience d ≈ 0.2 (low target, high beneficiary). The state with arbitrage options benefits from legitimacy alignment and experiences d ≈ 0.15 (beneficiary). Norm entrepreneurs with constrained exit benefit from social coordination and experience d ≈ 0.25 (moderate beneficiary). The analytical observer with analytical exit sees the constraint as pure coordination and experiences d ≈ 0.1 (analytical beneficiary). All d values feed into low effective extraction (chi) because the constraint is genuinely coordinating rather than extracting.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy. The state's mandate (formalizing the norm) aligns with its function (coordinating and legitimizing). The state's authority is grounded in its role as coordinator of already-adopted norms, not in coercive capacity. The constraint's function (coordination) persists as long as the norm solves the underlying coordination problem. If the problem disappears (communities no longer need the coordination), the norm may fade, but this is functional obsolescence, not mandatrophy. The constraint is structurally sound: mandate and function are aligned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_exogenous_override,
    'Is this constraint genuinely endogenous (bottom-up adoption preceding state mandate) or does it represent a case where the state''s anticipated future mandate shaped community adoption from the start?',
    'Historical analysis of adoption timeline and state signaling: did communities adopt before state announcement? Did state signals precede adoption? Archival evidence of community deliberation and state communication timing.',
    'If genuinely endogenous: this reading holds; classification remains Rope. If state signals preceded adoption: the constraint is hybrid (state influence on adoption) or exogenous (state-driven with community theater); reclassifies toward Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_exogenous_override, empirical, 'Whether adoption was genuinely endogenous or state-influenced from the start').

omega_variable(
    coordination_vs_legitimacy_capture,
    'Does the state''s formalization of bottom-up norms represent genuine coordination, or does it represent the state capturing legitimacy from organic movements to consolidate authority?',
    'Post-formalization analysis: does the state''s codification preserve the norm''s original function, or does it reframe/restrict/weaponize it? Do communities experience the formalization as alignment or as appropriation? Longitudinal tracking of norm function before and after state mandate.',
    'If genuine coordination: Rope classification holds. If legitimacy capture: the constraint is Tangled Rope (coordination + extraction) or Snare (extraction masked as coordination); extraction value rises to 0.35-0.50.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_legitimacy_capture, empirical, 'Whether state formalization represents coordination or legitimacy capture').

omega_variable(
    alternative_reading_foreclosure,
    'Does the endogenous-climb reading logically foreclose the exogenous-override reading, or do both readings remain live positions for different historical cases?',
    'Comparative historical analysis: identify cases where the same norm type shows endogenous adoption in one context and exogenous imposition in another. If both patterns exist, readings coexist; if one pattern is universal, one reading forecloses the other.',
    'If readings coexist: both constraints are live (separate stories, linked via network). If one forecloses: the foreclosed reading is a false historical claim; only the foreclosing reading is structurally real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether endogenous and exogenous readings are mutually exclusive or coexisting').

omega_variable(
    norm_type_specificity,
    'Does the endogenous-climb mechanism apply to all norm types (legal, cultural, religious, economic) or only to specific categories?',
    'Comparative analysis across norm domains: track adoption patterns for legal norms, cultural practices, religious doctrines, and economic conventions. Identify which types show endogenous adoption and which require exogenous imposition.',
    'If universal: the reading applies broadly. If domain-specific: the reading is valid only for certain norm types; other types may require different readings (exogenous, hybrid). Affects generalizability of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_type_specificity, empirical, 'Whether endogenous adoption applies across all norm types').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endogenous_climb_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endoclimb_tr_t0, endogenous_climb_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(endoclimb_tr_t10, endogenous_climb_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(endoclimb_tr_t20, endogenous_climb_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(endoclimb_be_t0, endogenous_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(endoclimb_be_t10, endogenous_climb_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(endoclimb_be_t20, endogenous_climb_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(endoclimb_su_t0, endogenous_climb_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(endoclimb_su_t10, endogenous_climb_reading, suppression_requirement, 10, 0.05).
narrative_ontology:measurement(endoclimb_su_t20, endogenous_climb_reading, suppression_requirement, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(endogenous_climb_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(endogenous_climb_reading, hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The endogenous-climb reading is one of three structurally distinct readings of the imposition-mechanism kernel. The exogenous-override reading models state-imposed norms with community resistance; the hybrid-legitimation reading models state-influenced adoption. Each reading has its own epsilon value, beneficiary/victim structure, and classification. They are linked via network.affects_constraints to indicate they are sibling readings of the same kernel, not separate constraints in different domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
