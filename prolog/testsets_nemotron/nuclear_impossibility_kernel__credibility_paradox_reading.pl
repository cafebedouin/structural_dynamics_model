% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__credibility_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__credibility_paradox_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__credibility_paradox_reading
 *   human_readable: Nuclear Deterrence Credibility Paradox — Escalation Ladder Reading
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   This reading of the nuclear impossibility kernel treats the credibility
 *   paradox not as a logical refutation of deterrence but as a permanent
 *   engine of doctrinal elaboration. The paradox — deterrence requires a
 *   threat that is incredible because its execution is suicidal — is
 *   structurally generative: it produces an endless demand for
 *   'credibility-enhancing' capabilities (counterforce, limited options,
 *   escalation control) that are themselves destabilizing. The constraint is
 *   a tangled rope: it coordinates great power behavior away from direct war
 *   (genuine coordination function) while extracting existential risk onto
 *   non-participants and resources into the nuclear complex (asymmetric
 *   extraction), and requires active enforcement through doctrine, posture,
 *   and alliance management. The theater ratio is high because much of the
 *   credibility machinery (war plans, limited options, escalation ladders) is
 *   performative — exercised but never used, maintained to sustain the
 *   threat's perceived credibility rather than its actual usability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__credibility_paradox_reading, 0.42).
domain_priors:suppression_score(nuclear_impossibility_kernel__credibility_paradox_reading, 0.15).
domain_priors:theater_ratio(nuclear_impossibility_kernel__credibility_paradox_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__credibility_paradox_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__credibility_paradox_reading, tangled_rope).
narrative_ontology:human_readable(nuclear_impossibility_kernel__credibility_paradox_reading, "Nuclear Deterrence Credibility Paradox — Escalation Ladder Reading").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__credibility_paradox_reading, "strategic/international_relations").

domain_priors:requires_active_enforcement(nuclear_impossibility_kernel__credibility_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__credibility_paradox_reading, 'e3637b0f-b252-48b8-be1d-bc4e2f157fb4').
narrative_ontology:cs_kernel_codification('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', distributed).
narrative_ontology:cs_authority_grounding('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', practice).
narrative_ontology:cs_interpretation_layer_present('e3637b0f-b252-48b8-be1d-bc4e2f157fb4').
narrative_ontology:cs_reading_relation('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', nuclear_impossibility_kernel__structural_contraction_reading, influences).
narrative_ontology:cs_reading_relation('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', nuclear_impossibility_kernel__rational_dropout_reading, coexists_with).
narrative_ontology:cs_axiom('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', foundational, deterrence_requires_credible_use_threat).
narrative_ontology:cs_axiom_status(deterrence_requires_credible_use_threat, holdable).
narrative_ontology:cs_axiom_grounding('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', deterrence_requires_credible_use_threat, instrumental).
narrative_ontology:cs_axiom('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', foundational, credibility_requires_usable_limited_options).
narrative_ontology:cs_axiom_status(credibility_requires_usable_limited_options, holdable).
narrative_ontology:cs_axiom_grounding('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', credibility_requires_usable_limited_options, instrumental).
narrative_ontology:cs_reference_frame('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', mutual_vulnerability_standoff).
narrative_ontology:cs_drift_state('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', contemporary_multi_polar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3637b0f-b252-48b8-be1d-bc4e2f157fb4', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_strategic_establishments).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_complexes).
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_doctrine_planners).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, populations_under_extended_deterrence).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_institutions).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, limited_nuclear_war_is_feasible).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, counterforce_capability_stabilizes_deterrence).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_control_is_achievable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce deterrence doctrine; invest in counterforce capabilities, limited nuclear options, and escalation management frameworks. Benefit from sustained budgets, institutional missions, and strategic relevance. Can pivot doctrine but face career and bureaucratic lock-in to nuclear deterrence as organizing logic.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_strategic_establishments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, great_power_strategic_establishments, beneficiary).

% Laboratories, production facilities, and procurement chains that design, build, and maintain arsenals. Receive sustained funding justified by credibility requirements. Exit means industrial conversion or closure — constrained by specialized workforce and political protection.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_weapons_complexes, beneficiary,
    organized, biographical, constrained, national).

% Operationalize deterrence through war plans, targeting doctrines, and escalation ladders. Professional identity fused to the credibility project — their expertise is the credibility machinery. Exit means leaving the field; constrained by career investment and clearance structures.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_doctrine_planners, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(nuclear_impossibility_kernel__credibility_paradox_reading, escalation_doctrine_planners, agenda_setter).

% Bear existential risk from deterrence failures they cannot influence. No nuclear option of their own; security depends on great power restraint. Trapped by geography and the global architecture — cannot exit the risk envelope.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, non_nuclear_states, payer,
    powerless, generational, trapped, global).

% Allied populations covered by nuclear umbrellas (NATO, US-Japan, US-Korea, etc.). Bear the risk of being targets in escalation ladders they do not control. Some political voice through democratic channels, but constrained by alliance commitments and security dependencies.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, populations_under_extended_deterrence, payer,
    moderate, biographical, constrained, regional).

% Treaty regimes (NPT, CTBT, New START, TPNW) and verification bodies. Their mandate is to constrain the very capabilities this reading treats as necessary for credibility. Structurally excluded from credibility doctrine formation; constrained by great power vetoes and non-participation.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, arms_control_institutions, excluded,
    organized, generational, constrained, global).

% Academic and think-tank analysts outside official establishments. Map the credibility paradox, model escalation dynamics, track doctrine evolution. No operational role; exit is analytical freedom. See the full structure across all readings.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__credibility_paradox_reading, strategic_analysts_independent, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates great power behavior by establishing a shared framework where mutual vulnerability creates a floor under conflict — prevents direct great power war through the threat of unacceptable damage, even if the threat's credibility is paradoxical.
% TRANSFER_FUNCTION: Transfers existential risk from great power strategic establishments (who control the threat) to non-nuclear states and allied populations (who bear the consequences if deterrence fails or escalation occurs). Also transfers resources to nuclear weapons complexes and doctrine planners via credibility requirements.
% ABSENT_VOICES: Future generations who inherit the risk architecture; populations in the Global South excluded from nuclear decision-making but subject to fallout and climatic effects; civil society movements for disarmament (TPNW advocates) structurally excluded from deterrence doctrine rooms.
% DISAPPEARANCE_RATIONALE: If the credibility paradox reading vanished — i.e., if great powers abandoned counterforce/limited-war pursuits and accepted mutual vulnerability as the only stable basis — arsenals would shift to minimal survivable second-strike postures, escalation ladders would collapse, arms control would become tractable, and the nuclear weapons complex would face existential mission contraction.
% FOUNDING_PROBLEM: 1945-1960: How to prevent Soviet conventional superiority in Europe from forcing Western concession or general war, given US monopoly/primacy was ending. The credibility problem was immediate: how to make a threat credible that, if executed, destroys the threatener.
% FOUNDING_PROBLEM_CORROBORATION: Strategic establishments attest the founding problem persists in new forms (multi-polar deterrence, conventional-nuclear integration). Arms control institutions and independent analysts attest the original problem (Soviet conventional threat to Europe) is dead — the arrangement persists because it created self-justifying institutions. No single outside corroborator; the contest is the signal.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__credibility_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__credibility_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nuclear_impossibility_kernel__credibility_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__credibility_paradox_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).
:- end_tests(nuclear_impossibility_kernel__credibility_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the sustained resource transfer to nuclear complexes and the existential risk transfer to excluded populations — substantial but not total because the coordination function (preventing great power war) delivers real value. Suppression (0.15) is low because the constraint operates through doctrine and posture, not overt coercion of participants; the 'suppression' is structural (alternative security architectures are marginalized by the credibility logic). Theater ratio (0.58) is high: the credibility project generates elaborate performative structures (war plans, exercises, declaratory policy) that far exceed operational necessity. Accessibility collapse (0.25) is low because alternatives (minimal deterrence, no-first-use, disarmament) remain intellectually and politically available — they are suppressed by the credibility logic, not collapsed by physics. Resistance (0.48) is moderate: arms control movements, TPNW, and internal dissent persist but have not shifted the great power consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (great power establishments), the constraint is a necessary coordination mechanism — the credibility paradox is a management problem, not a structural flaw. From the payer seats (non-nuclear states, allied populations), the same structure is an extraction mechanism that imposes existential risk without consent. The engine computes this divergence from power/exit/role data. The high theater ratio means the agenda-setters' own machinery is partly performative — they maintain credibility theater they may not fully believe.
 *
 * DIRECTIONALITY LOGIC:
 *   Great power establishments and nuclear complexes are structural beneficiaries (d ~ 0.15-0.25): they collect budgets, missions, and strategic centrality from the credibility project. Their exit is arbitrage/constrained — they can pivot doctrine but not leave the nuclear order. Non-nuclear states are full targets (d ~ 0.95): they bear existential risk with zero influence — trapped. Allied populations are constrained targets (d ~ 0.7): they bear risk with limited voice. Arms control institutions are excluded (not on the d-axis as participants) — their role is to challenge the constraint, not inhabit it. Independent analysts sit at analytical (d=0.5): symmetric observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing Soviet conventional conquest of Europe) is historically dead, but the arrangement persists and has elaborated new missions (counterforce, limited war, tailored deterrence). This is classic mandatrophy: the constraint's mandate has outlived its function, but the institutions it created (nuclear complexes, doctrine planners, alliance structures) now justify the constraint. The credibility paradox reading treats this as a feature — the paradox generates the mission — while the structural contraction reading treats it as proof the whole edifice is a piton. The tangled rope classification captures that both coordination and extraction are real and ongoing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credibility_paradox_naturalness,
    'Is the credibility paradox a genuine structural feature of the nuclear condition (emerges naturally) or a constructed problem sustained by institutions that benefit from its persistence?',
    'Historical counterfactual: if great powers had adopted minimal deterrence postures in 1960, would the paradox have dissolved or mutated? Compare doctrinal trajectories of states with different resource constraints (e.g., China''s minimal posture vs US/URSS elaborate ladders).',
    'If natural, the tangled rope classification holds — the coordination function is real and inescapable. If constructed, the constraint is a false summit mountain (claims naturalness but has beneficiaries) or a snare (coordination is cover for extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_paradox_naturalness, conceptual, 'Natural-law vs. constructed status of the credibility paradox').

omega_variable(
    escalation_ladder_operationality,
    'Are escalation ladders and limited nuclear options genuinely usable warfighting tools, or are they performative credibility theater that would collapse if tested?',
    'Wargaming and historical near-use analysis (Cuban Missile Crisis, Able Archer, Kargil): when leaders confronted actual escalation decisions, did ladder logic operate or did political inhibition dominate?',
    'If performative, theater ratio is underestimated and the constraint leans toward piton/snare. If operational, the coordination function includes genuine warfighting capability — extraction is the price of a real (if terrible) coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_ladder_operationality, empirical, 'Whether the credibility machinery is functional or theatrical').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does the nuclear_impossibility_kernel instantiate one constraint with three observable facets, or three genuinely distinct constraints with different ε values?',
    'Apply the ε-invariance test: would measuring the constraint via ''deterrence stability'' yield a different ε than measuring via ''arms racing intensity'' or ''crisis escalation outcomes''? If yes, the kernel decomposes into multiple constraints — which this story set models.',
    'If the kernel is one constraint, the three readings are perspectival frames on a single ε. If three constraints, each reading has its own ε and the network edges model causal influence between them. The current authoring treats them as three constraints in a family (network.affects_constraints links them).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__credibility_paradox_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(nucl_tr_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1955, 0.25).
narrative_ontology:measurement(nucl_tr_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1965, 0.42).
narrative_ontology:measurement(nucl_tr_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1975, 0.55).
narrative_ontology:measurement(nucl_tr_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1985, 0.6).
narrative_ontology:measurement(nucl_tr_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(nucl_tr_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2005, 0.56).
narrative_ontology:measurement(nucl_tr_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2015, 0.59).
narrative_ontology:measurement(nucl_tr_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1945, 0.12).
narrative_ontology:measurement(nucl_be_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1955, 0.28).
narrative_ontology:measurement(nucl_be_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(nucl_be_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1975, 0.41).
narrative_ontology:measurement(nucl_be_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1985, 0.38).
narrative_ontology:measurement(nucl_be_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement(nucl_be_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement(nucl_be_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2015, 0.43).
narrative_ontology:measurement(nucl_be_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(nucl_su_t1955, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1955, 0.12).
narrative_ontology:measurement(nucl_su_t1965, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1965, 0.18).
narrative_ontology:measurement(nucl_su_t1975, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1975, 0.15).
narrative_ontology:measurement(nucl_su_t1985, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1985, 0.12).
narrative_ontology:measurement(nucl_su_t1995, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 1995, 0.08).
narrative_ontology:measurement(nucl_su_t2005, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2005, 0.14).
narrative_ontology:measurement(nucl_su_t2015, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2015, 0.18).
narrative_ontology:measurement(nucl_su_t2025, nuclear_impossibility_kernel__credibility_paradox_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__credibility_paradox_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__credibility_paradox_reading, 0.1).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__structural_contraction_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__credibility_paradox_reading, nuclear_impossibility_kernel__rational_dropout_reading).

% DUAL FORMULATION NOTE:
% This reading treats the credibility paradox as a generative engine of doctrinal elaboration (tangled rope). The structural_contraction_reading treats mutual vulnerability as a physical impossibility of rational victory (mountain). The rational_dropout_reading treats it as a rational-choice constraint where costs exceed benefits (rope/scaffold). The three readings form a constraint family: this reading's doctrinal elaboration (counterforce, limited war) is what the structural_contraction_reading identifies as the impossible structure, and what the rational_dropout_reading identifies as the cost-exceeding-benefit structure. This reading influences both siblings by generating the capabilities and doctrines they analyze.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, organized, 0.25).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, powerful, 0.35).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, powerless, 0.95).
constraint_indexing:directionality_override(nuclear_impossibility_kernel__credibility_paradox_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
