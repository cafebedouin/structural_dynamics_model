% ============================================================================
% CONSTRAINT STORY: ai_risk_governance_priority__bridge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_risk_governance_priority__bridge_reading, []).

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
 *   constraint_id: ai_risk_governance_priority__bridge_reading
 *   human_readable: Unified AI Risk Governance Framework (Bridge Reading)
 *   domain: technology/governance/ethics
 *
 * SUMMARY:
 *   The unified AI risk governance framework claims to address present
 *   documented harms (algorithmic bias, surveillance, labor displacement
 *   affecting marginalized populations) and existential risks
 *   (superintelligence misalignment, capability concentration, irreversible
 *   lock-in) as non-separable, structurally entangled concerns requiring
 *   integrated safety-ethics research and policy. This reading instantiates
 *   the bridge position: neither timescale is subordinate, both victim sets
 *   deserve inclusion, and governance frameworks must reflect both. The
 *   empirical observation is that institutional bridging actors
 *   (approximately 5% of published work, 85% of cross-field citations)
 *   benefit from mediation prestige and resource capture while marginalized
 *   populations experience theoretical inclusion but practical subordination.
 *
 * KEY AGENTS:
 *   - Bridging institutions: Research centers and consortia explicitly connecting existential and near-term AI risk work; benefit from mediation and institutional prestige.
 *   - Present marginalized populations: Communities experiencing current AI harms; structurally excluded from governance despite being primary victims; represented through institutional intermediaries.
 *   - Future humanity: Represented only through proxy institutions; unable to participate in governance directly; bear civilizational-scale risk.
 *   - Existential-risk specialists: Researchers prioritizing superintelligence and long-horizon catastrophic scenarios; benefit from governance frameworks that elevate existential concerns.
 *   - Near-term harm researchers: Researchers focused on fairness, accountability, bias, labor displacement; benefit from unified framework validation but risk resource subordination.
 *   - AI capability developers: Powerful but excluded from governance; affected by unified frameworks but not included in their design.
 *   - Policy regulators: Institutional agenda-setters tasked with managing both present and existential AI risks; navigate competing urgency claims.
 *   - Funders: Control resource allocation; unified framework provides legitimacy for funding bridging work but creates bottleneck in the 5% of bridging actors.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, 0.52).
domain_priors:suppression_score(ai_risk_governance_priority__bridge_reading, 0.48).
domain_priors:theater_ratio(ai_risk_governance_priority__bridge_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ai_risk_governance_priority__bridge_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_risk_governance_priority__bridge_reading, tangled_rope).
narrative_ontology:human_readable(ai_risk_governance_priority__bridge_reading, "Unified AI Risk Governance Framework (Bridge Reading)").
narrative_ontology:topic_domain(ai_risk_governance_priority__bridge_reading, "technology/governance/ethics").

domain_priors:requires_active_enforcement(ai_risk_governance_priority__bridge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_risk_governance_priority__bridge_reading, 'e002a280-1952-4ca4-8ff4-4c6aa74270c8').
narrative_ontology:cs_kernel_codification('e002a280-1952-4ca4-8ff4-4c6aa74270c8', distributed).
narrative_ontology:cs_authority_grounding('e002a280-1952-4ca4-8ff4-4c6aa74270c8', extraction).
narrative_ontology:cs_interpretation_layer_present('e002a280-1952-4ca4-8ff4-4c6aa74270c8').
narrative_ontology:cs_reading_relation('e002a280-1952-4ca4-8ff4-4c6aa74270c8', ai_risk_governance_priority__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e002a280-1952-4ca4-8ff4-4c6aa74270c8', ai_risk_governance_priority__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('e002a280-1952-4ca4-8ff4-4c6aa74270c8', foundational, temporal_entanglement_irreducibility).
narrative_ontology:cs_axiom_status(temporal_entanglement_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('e002a280-1952-4ca4-8ff4-4c6aa74270c8', temporal_entanglement_irreducibility, instrumental).
narrative_ontology:cs_axiom('e002a280-1952-4ca4-8ff4-4c6aa74270c8', foundational, integrated_governance_necessity).
narrative_ontology:cs_axiom_status(integrated_governance_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e002a280-1952-4ca4-8ff4-4c6aa74270c8', integrated_governance_necessity, deontological).
narrative_ontology:cs_reference_frame('e002a280-1952-4ca4-8ff4-4c6aa74270c8', bifurcated_research_communities).
narrative_ontology:cs_drift_state('e002a280-1952-4ca4-8ff4-4c6aa74270c8', contemporary_institutional_bridging_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e002a280-1952-4ca4-8ff4-4c6aa74270c8', '').
narrative_ontology:cs_kernel_id(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, present_marginalized_populations).
narrative_ontology:constraint_victim(ai_risk_governance_priority__bridge_reading, future_humanity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, existential_risk_specialists).
narrative_ontology:constraint_beneficiary(ai_risk_governance_priority__bridge_reading, near_term_harm_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Research centers, policy organizations, and academic consortia that explicitly work to connect existential-risk research with near-term harm mitigation (approximately 5% of published work, accounting for 85% of cross-field citations). They set research agendas that span both timescales and benefit from institutional prestige, grant capture, and the mediation role itself. Their frameworks legitimize addressing both victims simultaneously and capture resources allocated to the problem space.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, bridging_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, bridging_institutions, beneficiary).

% Communities experiencing current harms from AI systems: algorithmic bias in hiring and lending, discriminatory facial recognition, misinformation targeting, labor displacement, and surveillance. They bear demonstrable costs now. A unified framework that treats their harms as one component within a larger existential-risk conversation can dilute urgency and resources directed at remediation. Their seat in policy discussions is weak; institutional bridging actors often speak for them without their authority.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(ai_risk_governance_priority__bridge_reading, present_marginalized_populations, excluded).

% Potential victims of existential or permanently-constraining AI scenarios (superintelligence misalignment, lock-in of value systems, irreversible capability concentration). They cannot participate in governance; their interests are represented through proxy institutions. A unified framework can either prioritize their interests adequately or subordinate them to near-term resource pressures.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, future_humanity, payer,
    powerless, civilizational, trapped, universal).

% Researchers, organizations, and funders focused narrowly on existential and catastrophic AI risks (alignment, power-seeking, deceptive behavior, capability barriers). A unified framework validates their research agenda as foundational to safety governance but can also diffuse funding and reduce their research priority if near-term concerns dominate implementation.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, existential_risk_specialists, beneficiary,
    moderate, civilizational, mobile, global).

% Researchers, practitioners, and advocates focused on documented present harms (fairness, accountability, transparency, labor displacement, bias). A unified framework acknowledges their work's legitimacy but also risks subordinating their urgency to longer-term existential scenarios. Their resource base is typically smaller than existential-risk funding.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, near_term_harm_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Organizations building large-scale AI systems. They are affected by governance frameworks but typically excluded from governance decision-making tables despite their capacity to implement or circumvent controls. A unified framework that bridges harms and risks could require more comprehensive safety practices, or could be weakened if institutional brokers prioritize preserving development momentum.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, ai_capability_developers, excluded,
    powerful, biographical, constrained, global).

% Government agencies tasked with AI governance and regulation. They must respond to present harms (documented injury to constituents) while also managing existential-risk uncertainty. A unified framework provides intellectual coherence for regulation but also creates competing urgency claims that can lead to policy fragmentation or inaction on both fronts.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, policy_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Philanthropic, government, and corporate entities that allocate research funding and resources. A unified framework provides a legitimate justification for funding work that bridges both concerns, but it also creates a bottleneck: resources flow through the handful of institutional actors (the 5%) who can credibly make the bridging argument. Fragmentation into separate tracks might expand funding availability but loses the integrated theoretical perspective.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, funders_and_resource_allocators, agenda_setter,
    institutional, generational, arbitrage, global).

% External evaluation of whether the unified framework achieves genuine integration or functions as institutional rent-capture that subordinates present harms to existential risk narratives. This seat monitors whether resource flows actually reach marginalized populations and whether governance decisions reflect both timescales.
narrative_ontology:constraint_stakeholder(ai_risk_governance_priority__bridge_reading, observer_analytical_seat, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_risk_governance_priority__bridge_reading, bridging_institutions).
narrative_ontology:fixing_cost_class(ai_risk_governance_priority__bridge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates governance across two temporal and institutional domains (near-term harm mitigation and existential risk prevention) that have historically operated in separation with different methodologies, funding sources, and beneficiaries. A unified framework enables shared standards, cross-learning, and resource pooling rather than fragmented siloed efforts.
% TRANSFER_FUNCTION: Moves research legitimacy, funding allocation, policy attention, and institutional prestige toward bridging actors and toward research that claims to address both present harms and existential risks. Present marginalized populations receive theoretical inclusion in a unified framework but often experience subordination of immediate remediation in practice; future humanity receives proxy representation through the same institutional brokers.
% ABSENT_VOICES: AI capability developers are structurally excluded from governance decision-making despite their control over implementation. Marginalized populations most directly harmed by current AI systems participate primarily through institutional intermediation rather than direct advocacy. Longtermist-focused researchers outside the bridging consensus are partly marginalized if the unified framework is enforced as the canonical approach.
% DISAPPEARANCE_RATIONALE: If the unified framework dissolved and governance splintered into separate near-term and existential-risk tracks, resources would fragment into two channels with less cross-learning, policy coordination would weaken, institutional prestige would shift to whoever controls each track, and the two victim sets would compete for legitimacy rather than being theoretically integrated. The 5% of bridging institutions would lose their mediation role.
% FOUNDING_PROBLEM: Early AI safety discourse separated into two isolated communities: existential-risk researchers focused on long-horizon superintelligence scenarios, and fairness/accountability researchers focused on documented present harms. This separation led to duplicated conceptual work, mutual dismissal across communities, misaligned funding, and governance frameworks that addressed one risk domain while leaving the other unintegrated.
% FOUNDING_PROBLEM_CORROBORATION: Existential-risk researchers and near-term harm researchers acknowledge the historical separation. The bridging institutions attesting to the founding problem's resolution cite their own work. External observers (outside bridging consensus) contest whether the problem is actually solved or whether unified-framework adoption simply privileges the bridging actors' narrative and resource position; independent bibliometric and funding-flow analyses show continued separation despite bridging rhetoric.
narrative_ontology:disappearance_verdict(ai_risk_governance_priority__bridge_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_risk_governance_priority__bridge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_risk_governance_priority__bridge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_risk_governance_priority__bridge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_risk_governance_priority__bridge_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_risk_governance_priority__bridge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_risk_governance_priority__bridge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_risk_governance_priority__bridge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the constraint does genuine coordination work—integrating two research communities and governance timescales that historically operated separately—but does so through institutional capture by bridging actors. The measurement series shows extraction rising from 0.38 to 0.52 over the interval: as the unified framework consolidates, institutional prestige and resource concentration in bridging institutions increases. Theater ratio (0.41) reflects that while real integration work happens, a portion of the constraint's functioning is institutional self-justification and legitimacy claims. Suppression requirement (0.48) indicates that the unified framework's persistence depends on active suppression of competing framings (pure existential-risk prioritization, pure near-term-harm prioritization) and on institutional gatekeeping that controls who can make bridging claims credibly. Accessibility collapse (0.62) is moderate: alternatives (separate governance tracks) remain theoretically available but accessing them requires institutional defection and loss of prestige. Resistance (0.71) is substantial because existential-risk and near-term-harm researchers both contest the unified framework's resource implications for their own priorities.
 *
 * PERSPECTIVAL GAP:
 *   From the bridging-institution seat, the unified framework is a genuine coordination success: it integrated two siloed research communities, created shared standards and vocabulary, and improved research coherence. From the present-marginalized-populations seat, the unified framework is extraction that subordinates their immediate suffering to long-horizon institutional narratives. From the future-humanity seat (represented only through proxy), the framework may or may not adequately prioritize their interests depending on whose proxy controls it. From the existential-risk-specialist seat, unification risks diluting existential urgency; from the near-term-harm seat, it risks subordinating remediation. The engine computes these divergences from the structural data (power, exit, time horizon, victim/beneficiary declarations); the claimed type (tangled rope) asserts that coordination and extraction are both real features of the same constraint, not that one is right and the other is cover story.
 *
 * DIRECTIONALITY LOGIC:
 *   Bridging institutions sit at d near beneficiary (d ≈ 0.2): they collects institutional prestige, research legitimacy, grant capture, and mediation power. Present marginalized populations sit at d near target (d ≈ 0.85): they bear the cost of having their harms theoretically integrated but practically subordinated, and their voice in governance is mediated rather than direct. Future humanity sits at d = 1.0 (pure target): they have no agency in governance, no voice, only proxy representation that may or may not protect their interests. Existential-risk and near-term-harm researchers sit at d ≈ 0.5 (symmetric): the unified framework provides legitimacy and inclusion for their work but also introduces resource competition and constraint on research agenda-setting. Policy regulators sit at d ≈ 0.5 (symmetric): they get intellectual coherence and unified direction but also inherit the constraint's actual asymmetries. AI developers sit excluded: they have no formal role but high structural power outside the governance framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem is real: isolated AI safety research communities did face fragmentation, missed synergies, and governance complexity from uncoordinated approaches. The bridging movement addressed that. However, mandatrophy is at risk: the founding problem (community fragmentation) has been substantially solved at the institutional level, but the constraint persists because bridging institutions now have institutional prestige and resource incentive to maintain the narrative that they are irreplaceable. The theater ratio rising from 0.28 to 0.41 suggests this dynamic: increasingly, institutional bridging actors are performing the integration rather than managing novel coherence problems. A genuine mandatrophy resolution would be distributed bridging capacity across more institutions (reducing dependency on the 5%), making the bridging work more routine and less dependent on institutional prestige capture. The measurement series' plateau at t=16-20 suggests the constraint has found its equilibrium—extraction and theater reach a stable point rather than continuing to accumulate—which is consistent with a piton trajectory (function attrophied, constraint maintained by institutional inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_coexistence,
    'Do the existential-risk reading and near-term-harms reading coexist as live positions held simultaneously by different governance coalitions, or does the bridge reading attempt to foreclose one or both through its unified framework claim?',
    'Survey of researchers and policy-makers in each tradition: can existential-risk specialists maintain coherence while accepting some bridge-reading premises? Can near-term-harm advocates do the same? If yes on both, coexistence; if either group experiences the bridge reading as incompatible with their core commitments, foreclosure is operating.',
    'Coexistence means three legitimate readings of the kernel remain live; foreclosure means the bridge reading is attempting to eliminate structural alternatives, which would reframe it as extractive (snare) rather than coordinative (tangled rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether sibling readings coexist with or are foreclosed by the bridge reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_risk_governance_priority__bridge_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_r_tr_t0, ai_risk_governance_priority__bridge_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ai_r_tr_t4, ai_risk_governance_priority__bridge_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(ai_r_tr_t8, ai_risk_governance_priority__bridge_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(ai_r_tr_t12, ai_risk_governance_priority__bridge_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(ai_r_tr_t16, ai_risk_governance_priority__bridge_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(ai_r_tr_t20, ai_risk_governance_priority__bridge_reading, theater_ratio, 20, 0.41).

% Extraction over time
narrative_ontology:measurement(ai_r_be_t0, ai_risk_governance_priority__bridge_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ai_r_be_t4, ai_risk_governance_priority__bridge_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(ai_r_be_t8, ai_risk_governance_priority__bridge_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(ai_r_be_t12, ai_risk_governance_priority__bridge_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(ai_r_be_t16, ai_risk_governance_priority__bridge_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(ai_r_be_t20, ai_risk_governance_priority__bridge_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ai_r_su_t0, ai_risk_governance_priority__bridge_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ai_r_su_t4, ai_risk_governance_priority__bridge_reading, suppression_requirement, 4, 0.39).
narrative_ontology:measurement(ai_r_su_t8, ai_risk_governance_priority__bridge_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(ai_r_su_t12, ai_risk_governance_priority__bridge_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(ai_r_su_t16, ai_risk_governance_priority__bridge_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(ai_r_su_t20, ai_risk_governance_priority__bridge_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_risk_governance_priority__bridge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(ai_risk_governance_priority__bridge_reading, 0.12).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__existential_risk_reading).
narrative_ontology:affects_constraint(ai_risk_governance_priority__bridge_reading, ai_risk_governance_priority__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel decomposition. The unified framework (bridge reading) claims to integrate existential and near-term risk governance. The sibling readings are: (1) existential-risk-only, treating present harms as secondary; (2) near-term-harms-only, treating existential scenarios as speculative. Each reading has a different ε, victim set, and beneficiary structure. The bridge reading has moderate ε (~0.52) with both victim sets (present marginalized + future humanity) and institutional bridging beneficiaries. The existential reading would have higher ε on future humanity and lower on present populations; the near-term reading would reverse. Network edges represent that all three readings compete for the same governance space and resource pool.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_risk_governance_priority__bridge_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
