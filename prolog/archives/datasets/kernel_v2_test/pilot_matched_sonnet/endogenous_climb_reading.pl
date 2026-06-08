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
 *   human_readable: Endogenous Climb: Bottom-Up Norm Legitimation
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   The endogenous climb reading describes historical episodes where new
 *   social norms achieved legitimacy through bottom-up adoption before state
 *   codification. In this reading, communities voluntarily adopt practices
 *   through peer observation, perceived benefit, and cultural diffusion. The
 *   state's role is to coordinate and formalize already-legitimated norms,
 *   not to impose them coercively. This reading emphasizes low enforcement
 *   costs, rapid adoption timelines, minimal resistance, and the state acting
 *   as coordinator rather than coercer. Examples might include: adoption of
 *   standardized weights and measures in merchant communities before state
 *   metrication; spread of literacy norms before compulsory education laws;
 *   adoption of hygiene practices before public health mandates. The
 *   constraint's low extractiveness (0.15) and suppression (0.20) reflect
 *   that compliance is voluntary and enforcement is minimal. The modest
 *   theater ratio (0.25) reflects that state codification involves some
 *   performative legitimation ritual (legislative debate, ceremonial signing)
 *   but the primary legitimation work happens at the community level before
 *   codification.
 *
 * KEY AGENTS:
 *   - Adopting Communities: Primary beneficiaries (moderate/mobile) — voluntarily adopt norms that serve their interests; state codification provides coordination benefit without coercion
 *   - State Coordination Apparatus: Secondary beneficiary (institutional/constrained) — gains legitimacy by codifying popular norms; low enforcement costs because compliance is voluntary
 *   - Cultural Innovators: Organized agents (organized/mobile) — norm entrepreneurs and early adopters who demonstrate new practices; state recognition follows their success
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine bottom-up legitimation with state following rather than leading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endogenous_climb_reading, 0.15).
domain_priors:suppression_score(endogenous_climb_reading, 0.2).
domain_priors:theater_ratio(endogenous_climb_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endogenous_climb_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(endogenous_climb_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(endogenous_climb_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endogenous_climb_reading, rope).
narrative_ontology:human_readable(endogenous_climb_reading, "Endogenous Climb: Bottom-Up Norm Legitimation").
narrative_ontology:topic_domain(endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(endogenous_climb_reading, '47b29190-80ba-475b-b779-e1f854704ea2').
narrative_ontology:cs_kernel_codification('47b29190-80ba-475b-b779-e1f854704ea2', distributed).
narrative_ontology:cs_authority_grounding('47b29190-80ba-475b-b779-e1f854704ea2', practice).
narrative_ontology:cs_interpretation_layer_present('47b29190-80ba-475b-b779-e1f854704ea2').
narrative_ontology:cs_reading_relation('47b29190-80ba-475b-b779-e1f854704ea2', endogenous_climb_reading__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('47b29190-80ba-475b-b779-e1f854704ea2', endogenous_climb_reading__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('47b29190-80ba-475b-b779-e1f854704ea2', foundational, legitimacy_follows_adoption).
narrative_ontology:cs_axiom_status(legitimacy_follows_adoption, holdable).
narrative_ontology:cs_axiom_grounding('47b29190-80ba-475b-b779-e1f854704ea2', legitimacy_follows_adoption, empirically_contingent).
narrative_ontology:cs_axiom('47b29190-80ba-475b-b779-e1f854704ea2', secondary, state_coordination_primacy).
narrative_ontology:cs_axiom_status(state_coordination_primacy, holdable).
narrative_ontology:cs_axiom_grounding('47b29190-80ba-475b-b779-e1f854704ea2', state_coordination_primacy, instrumental).
narrative_ontology:cs_reference_frame('47b29190-80ba-475b-b779-e1f854704ea2', popular_sovereignty_baseline).
narrative_ontology:cs_drift_state('47b29190-80ba-475b-b779-e1f854704ea2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('47b29190-80ba-475b-b779-e1f854704ea2', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, adopting_communities).
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, state_coordination_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, cultural_innovators).
narrative_ontology:constraint_vindicates(endogenous_climb_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(endogenous_climb_reading, cultural_evolution_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities that voluntarily adopt new norms through peer observation and perceived benefit. State codification provides coordination benefit (standardization, legal recognition) without imposing costs. Exit is mobile: communities can reject norms that don't serve them, and the state follows rather than leads.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, adopting_communities, beneficiary,
    moderate, biographical, mobile, regional).

% State institutions that codify and formalize already-legitimated norms. They set the agenda for codification (decide which norms to formalize, when, and how) and benefit from legitimacy gains and low enforcement costs. Exit is constrained: once norms are codified, the state cannot easily abandon its coordination role without losing legitimacy.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, state_coordination_apparatus, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(endogenous_climb_reading, state_coordination_apparatus, beneficiary).

% Norm entrepreneurs and early adopters who demonstrate new practices. They benefit when the state recognizes and formalizes their innovations, providing legal protection and broader legitimacy. Exit is mobile: they can abandon unsuccessful innovations and try new ones.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, cultural_innovators, beneficiary,
    organized, biographical, mobile, regional).

% Groups whose traditional practices are displaced by new norms. They are excluded from the adoption process (not consulted, not represented in state codification) but are not coercively suppressed in this reading — the new norms spread through voluntary adoption, and traditional practices fade through obsolescence rather than prohibition. Exit is constrained: they can maintain traditional practices locally but face increasing coordination costs as the broader society adopts new norms.
narrative_ontology:constraint_stakeholder(endogenous_climb_reading, traditional_practice_holders, excluded,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Formalizing and standardizing norms that have already achieved popular legitimacy through bottom-up adoption. The state provides legal recognition, dispute resolution mechanisms, and coordination infrastructure for practices that communities have voluntarily adopted.
% TRANSFER_FUNCTION: Minimal transfer. The state captures some coordination benefit (standardization reduces transaction costs, which can be taxed; legal formalization generates administrative fees). Communities transfer some autonomy (once norms are codified, they become subject to state interpretation and enforcement). But the transfer is small because the norms are already legitimated — the state is not extracting compliance from unwilling subjects.
% ABSENT_VOICES: Traditional practice holders whose customs are displaced by new norms. They are not coercively suppressed in this reading, but they are excluded from the adoption process and the state codification process. Their absence is structural: the endogenous climb mechanism privileges early adopters and majority preferences, leaving traditional minorities without voice in the formalization process.
% DISAPPEARANCE_RATIONALE: If state codification disappeared, communities would still practice the norms (they adopted them voluntarily before codification), but coordination costs would rise. Disputes would lack formal resolution mechanisms, standardization would be incomplete, and transaction costs would increase. The world would rearrange toward more localized, less standardized practice.
% FOUNDING_PROBLEM: Coordination failure in the absence of formal standardization. Before state codification, communities that had voluntarily adopted new norms faced high transaction costs when interacting with other communities (incompatible standards, unclear legal status, no dispute resolution mechanism). The founding problem was genuine: how to coordinate already-legitimated practices across communities and provide legal infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of merchant guilds, professional associations, and community organizations petitioning for state recognition and standardization of practices they had already adopted. Corroborated by adopting communities themselves (not just state beneficiaries), indicating the coordination problem was real and the state response was requested rather than imposed.
narrative_ontology:disappearance_verdict(endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(endogenous_climb_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADOPTING COMMUNITY (ROPE) — Communities adopt norms voluntarily through peer observation and perceived benefit. State codification follows as coordination mechanism, not coercion. Low extraction, high coordination function. Mobile exit: communities can reject norms that don't serve them.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: STATE COORDINATION APPARATUS (ROPE) — State acts as coordinator and codifier of already-legitimated norms. Enforcement costs are low because compliance is voluntary. The state benefits from coordination function but does not extract rents. Constrained exit: state cannot easily abandon coordination role once norms are codified.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CULTURAL INNOVATORS (ROPE) — Norm entrepreneurs and early adopters who demonstrate new practices. State recognition follows their success. They experience the constraint as pure coordination: their innovations spread through demonstration effects, not coercion. Mobile exit: can abandon unsuccessful innovations.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, this reading describes genuine bottom-up legitimation where state authority follows rather than creates popular acceptance. Low extraction, minimal suppression, rapid adoption timelines. The coordination function is real and the coercive overhead is negligible.
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
 *   Extractiveness (0.15): Low. The state captures some coordination benefit (standardization reduces transaction costs, which the state can tax), but the primary beneficiaries are the adopting communities. The extraction is minimal because the state is not imposing costs on unwilling subjects — it is formalizing arrangements that communities already prefer. Suppression (0.20): Low. Enforcement costs are minimal because compliance is voluntary. The state does not need extensive enforcement apparatus because norms are already legitimated at the community level. Some suppression exists (legal penalties for non-compliance, bureaucratic barriers to alternative practices) but it is low relative to coercive imposition. Theater ratio (0.25): Low-moderate. State codification involves some performative legitimation (legislative process, public ceremonies, official declarations) but the primary legitimation work happens through community adoption before codification. The theater is real but not dominant — most of the constraint's function is genuine coordination, not performance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap because all agents experience it as coordination rather than extraction. Adopting communities see Rope (voluntary adoption with coordination benefit). The state sees Rope (low-cost coordination function). Cultural innovators see Rope (their innovations spread through demonstration, not coercion). The analytical observer sees Rope (genuine bottom-up legitimation). The uniform classification across perspectives reflects the structural reality of this reading: when state authority follows popular acceptance, all agents experience the constraint as coordination. The perspectival gap emerges only when comparing this reading to its siblings: the exogenous_override_reading would show high extraction and suppression (state imposes norms coercively), and the hybrid_legitimation_reading would show mixed perspectives (some agents experience coordination, others experience coercion).
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this reading are net beneficiaries or neutral. Adopting communities benefit from coordination (d ≈ 0.2, low extraction). The state benefits from legitimacy and low enforcement costs (d ≈ 0.1, minimal extraction). Cultural innovators benefit from state recognition (d ≈ 0.15, low extraction). No agent is a clear victim because the constraint solves a genuine coordination problem with minimal coercive overhead. The directionality values are derived from beneficiary status + mobile or constrained exit options, producing low effective extraction across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Rope classification is appropriate when state authority follows rather than precedes popular acceptance. The mandate (state codification) does not create the norm — it formalizes an already-legitimated practice. The constraint's function is coordination, not extraction, because compliance is voluntary and enforcement costs are low. The mandatrophy question 'is this coordination or extraction?' is answered by the structural data: low extractiveness, low suppression, minimal theater, and beneficiaries across all perspectives. The constraint is not a false summit (no naturalization of contingent arrangements) and not a degraded ritual (theater is low and function is real).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of the imposition_mechanism_kernel, where different parties dispute whether state authority precedes or follows popular acceptance?',
    'Historical case analysis comparing adoption timelines, enforcement costs, and resistance patterns across different norm-imposition episodes. If enforcement costs are consistently low and adoption precedes mandate, endogenous_climb_reading is structurally accurate. If enforcement costs are high and mandate precedes adoption, exogenous_override_reading is accurate.',
    'If endogenous: state acts as coordinator (Rope). If exogenous: state acts as coercer (Snare or Tangled Rope). If hybrid: both mechanisms operate in different domains or time periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this is one reading of a contested kernel about imposition mechanisms').

omega_variable(
    adoption_timeline_threshold,
    'What timeline threshold distinguishes genuine bottom-up adoption from state-led imposition with manufactured consent?',
    'Quantitative analysis of norm diffusion curves: bottom-up adoption shows S-curve with early adopters preceding state action; top-down imposition shows step-function at mandate date with slow subsequent diffusion. Measure time lag between first community adoption and state codification.',
    'If lag > 10 years: strong evidence for endogenous climb. If lag < 2 years: suggests state-led coordination or exogenous override. If lag is negative (mandate precedes adoption): definitively exogenous.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adoption_timeline_threshold, empirical, 'Timeline threshold for distinguishing bottom-up from top-down norm imposition').

omega_variable(
    enforcement_cost_measurement,
    'How do we measure enforcement costs to distinguish coordination from coercion?',
    'Historical records of enforcement apparatus: size of enforcement bureaucracy, frequency of sanctions, resistance incidents, compliance rates before and after codification. Low enforcement costs + high pre-codification compliance = coordination. High enforcement costs + low pre-codification compliance = coercion.',
    'If enforcement costs < 5% of state budget and compliance > 80% pre-codification: Rope classification confirmed. If enforcement costs > 20% and compliance < 50% pre-codification: reclassify as Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_measurement, empirical, 'Operationalizing enforcement cost measurement').

omega_variable(
    sibling_reading_coexistence,
    'Do the exogenous_override_reading and hybrid_legitimation_reading describe structurally different historical episodes, or competing interpretations of the same episodes?',
    'Case-by-case historical analysis: if different scholars assign different readings to the same episode (e.g., Prohibition in the US, metric system adoption in France), the readings are competing interpretations. If different readings apply cleanly to different episodes, they describe different structural patterns.',
    'If competing interpretations: the kernel is genuinely contested and omega variables must document the interpretive ambiguity. If different episodes: the readings are not in conflict, just domain-specific.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether sibling readings compete over the same cases or describe different cases').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endogenous_climb_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endogen_theater_t0, endogenous_climb_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(endogen_theater_t3, endogenous_climb_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(endogen_theater_t6, endogenous_climb_reading, theater_ratio, 6, 0.25).

% Extraction over time
narrative_ontology:measurement(endogen_extract_t0, endogenous_climb_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(endogen_extract_t3, endogenous_climb_reading, base_extractiveness, 3, 0.12).
narrative_ontology:measurement(endogen_extract_t6, endogenous_climb_reading, base_extractiveness, 6, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(endogen_suppress_t0, endogenous_climb_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(endogen_suppress_t3, endogenous_climb_reading, suppression_requirement, 3, 0.18).
narrative_ontology:measurement(endogen_suppress_t6, endogenous_climb_reading, suppression_requirement, 6, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endogenous_climb_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imposition_mechanism_kernel. The other readings (exogenous_override_reading, hybrid_legitimation_reading) are separate constraint stories with different extractiveness and suppression values. They should be linked via network.affects_constraints once those stories are generated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
