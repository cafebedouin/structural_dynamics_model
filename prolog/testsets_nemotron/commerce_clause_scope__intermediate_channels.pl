% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause — Intermediate Channels Reading
 *   domain: constitutional_law/federalism/commerce_power
 *
 * SUMMARY:
 *   The intermediate channels reading of the Commerce Clause (channels,
 *   instrumentalities, substantial effects with limiting principles)
 *   represents the Supreme Court's doctrinal compromise from 1937-present,
 *   with a punctuation in 1995-2012 (Lopez, Morrison, NFIB) reasserting
 *   categorical limits. It claims to balance national economic integration
 *   against state autonomy through three categories of federal power subject
 *   to two limiting principles (non-economic activity requires jurisdictional
 *   element; aggregation only for economic activity; no attenuated causal
 *   chains). The constraint operates as a tangled rope: it genuinely
 *   coordinates federal-state authority in a national economy (rope function)
 *   but extracts conceptual coherence and state regulatory autonomy through
 *   manipulable boundaries (snare function). The limiting principles do real
 *   work in some cases but collapse in others, creating a doctrinal
 *   instability that benefits federal authority when it chooses to push and
 *   state authority when it can successfully invoke categories.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.45).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.55).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.45).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause — Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism/commerce_power").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'a743030e-5621-4434-b11f-436d7cbcb36b').
narrative_ontology:cs_kernel_codification('a743030e-5621-4434-b11f-436d7cbcb36b', fixed_text).
narrative_ontology:cs_authority_grounding('a743030e-5621-4434-b11f-436d7cbcb36b', lineage).
narrative_ontology:cs_interpretation_layer_present('a743030e-5621-4434-b11f-436d7cbcb36b').
narrative_ontology:cs_reading_relation('a743030e-5621-4434-b11f-436d7cbcb36b', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('a743030e-5621-4434-b11f-436d7cbcb36b', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('a743030e-5621-4434-b11f-436d7cbcb36b', foundational, commerce_power_has_categorical_limits).
narrative_ontology:cs_axiom_status(commerce_power_has_categorical_limits, holdable).
narrative_ontology:cs_axiom_grounding('a743030e-5621-4434-b11f-436d7cbcb36b', commerce_power_has_categorical_limits, conventional).
narrative_ontology:cs_axiom('a743030e-5621-4434-b11f-436d7cbcb36b', foundational, economic_non_economic_distinction_is_judicially_enforceable).
narrative_ontology:cs_axiom_status(economic_non_economic_distinction_is_judicially_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('a743030e-5621-4434-b11f-436d7cbcb36b', economic_non_economic_distinction_is_judicially_enforceable, conventional).
narrative_ontology:cs_reference_frame('a743030e-5621-4434-b11f-436d7cbcb36b', post_new_deal_doctrinal_settlement).
narrative_ontology:cs_drift_state('a743030e-5621-4434-b11f-436d7cbcb36b', post_lopez_morrison_nfib, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a743030e-5621-4434-b11f-436d7cbcb36b', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_economic_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_police_powers).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, conceptual_coherence).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_economic_regulation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_economic_regulation).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, dual_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, limited_enumerated_powers_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces federal commerce power across channels, instrumentalities, and substantially affecting activities. Defines the scope of 'economic activity' and 'substantial effects' through legislation, regulation, and litigation. Benefits from expansive but bounded authority to address national economic problems.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_economic_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Retains exclusive authority over non-economic local conduct (family law, criminal law, education) unless a jurisdictional element connects it to interstate commerce. Uses limiting principles as shield against federal encroachment on traditional state domains. Benefits from categorical boundaries but must constantly defend them.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_police_powers, beneficiary,
    institutional, generational, constrained, national).

% State economic regulations are subject to dormant commerce clause scrutiny and federal preemption when they burden interstate commerce. Bears the cost of compliance with federal standards and loses regulatory autonomy in economic sphere. Gains uniformity and market access from federal floor but pays in lost policy experimentation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_economic_regulation, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_economic_regulation, beneficiary).

% The economic/non-economic distinction is analytically unstable — most human activity has economic dimensions, making the boundary manipulable. Jurisdictional elements become formalistic hooks. Aggregation doctrine allows federal reach into local activity through cumulative effect reasoning. The constraint pays in doctrinal incoherence what it gains in practical compromise.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, conceptual_coherence, payer,
    moderate, civilizational, trapped, universal).

% Advocate for a commerce power limited to cross-border trade and removal of state barriers. Their reading is excluded from governing doctrine but persists in academic discourse and judicial dissents. Would object that the intermediate reading abandons the Constitution's structural design for a pragmatic compromise with no stable stopping point.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, narrow_originalist_scholars, excluded,
    moderate, generational, analytical, national).

% Argue that substantial effects + aggregation give Congress plenary power over national economic problems. Their reading is partially instantiated in current doctrine (Wickard, Raich) but constrained by Lopez/Morrison categorical limits. Would object that the intermediate reading's limiting principles are judicially invented and incoherent.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, broad_effects_pragmatists, excluded,
    moderate, biographical, analytical, national).

% Observes the constraint as a doctrinal compromise that manages but does not resolve the tension between national economic integration and state autonomy. Sees the limiting principles as doing real work in some cases (Lopez, Morrison, NFIB v. Sebelius commerce clause holding) but collapsing in others (Raich, Wickard).
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable division of authority between federal and state governments in a national economy: federal power reaches economic activity with interstate spillovers; state power remains plenary over truly local non-economic conduct. Solves the coordination problem of governance scale in a federal system with integrated markets.
% TRANSFER_FUNCTION: Transfers regulatory authority over economic activity with interstate dimensions from states to federal government. Transfers the costs of regulatory fragmentation and race-to-the-bottom dynamics from national markets to federal oversight. States pay in lost economic regulatory autonomy; federal government pays in enforcement complexity and legitimacy maintenance.
% ABSENT_VOICES: Local communities whose economic activity is regulated by distant federal standards without direct representation in the rulemaking. Individuals whose non-economic conduct gets pulled into federal jurisdiction through jurisdictional elements (e.g., possession of a gun that once moved in interstate commerce). Future generations who inherit a doctrinal framework with unstable boundaries.
% DISAPPEARANCE_RATIONALE: If the intermediate channels reading vanished overnight, either the broad effects test would expand federal power to near-plenary over economic activity (eliminating meaningful state economic regulation), or the narrow originalist reading would collapse federal authority to only cross-border trade barriers (eliminating federal response to national economic crises). The constitutional order would fundamentally rearrange.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states could burden interstate commerce and the national government could not respond to economic crises. The Commerce Clause was designed to prevent state protectionism and enable federal regulation of genuinely national economic problems, while preserving state police powers over local non-economic life.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Constitutional Convention records (Madison's notes on the commercial motive), The Federalist Nos. 42, 44, and 22, and the ratification debates. However, the scope of 'commerce' and 'regulate' was contested even among founders (Hamilton vs. Jefferson on the Bank). Modern originalist scholars (e.g., Barnett, Rappaport) argue the founding problem was narrower; progressive scholars (e.g., Ackerman, Balkin) argue it was broader. No consensus outside the benefiting institutional actors.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).
:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that federal power under this reading is extensive but not plenary — states retain meaningful economic regulatory space and complete non-economic authority. Suppression (0.55) is moderate: the constraint is enforced through judicial review (not police power), but the categorical limits require active judicial maintenance against congressional drift. Theater ratio (0.30) captures that the limiting principles perform real doctrinal work in Lopez/Morrison/NFIB but are widely seen as manipulable formalisms. Accessibility collapse (0.40) and resistance (0.50) reflect that alternatives (narrow originalism, broad effects) remain live and contested — the constraint has not naturalized. Measurements show the New Deal pivot (1937-1942) as the major extraction event, the Rehnquist Court revival (1995-2000) as a partial rollback, and the post-Raich stabilization at a new equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the federal authority seat, the constraint is coordination infrastructure enabling national problem-solving. From the state economic regulation seat, it is a ceiling on policy experimentation. From the conceptual coherence seat, it is a manufactured instability that serves power. The engine computes these divergences from the declared structural positions — the claimed_type (tangled_rope) acknowledges the hybrid nature without resolving which seat's experience is 'real.'
 *
 * DIRECTIONALITY LOGIC:
 *   Federal economic authority is the structural agenda-setter (d ~0.2): it defines the categories, initiates legislation, and litigates the boundaries. State police powers are beneficiaries (d ~0.3) — they gain protected domains but must constantly litigate to maintain them. State economic regulation is a payer (d ~0.7) — it loses autonomy to federal preemption and dormant commerce clause but gains market integration benefits. Conceptual coherence is the trapped victim (d ~0.95) — it bears the incoherence of the economic/non-economic distinction with no exit. The excluded originalist and pragmatist scholars have analytical exit but no structural power to change the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state protectionism + national economic impotence) is contested as either solved (integrated national economy exists) or transformed (new problems: climate, digital markets, supply chains require federal coordination). The constraint persists because neither the broad nor narrow reading commands a stable majority. Mandatrophy is unresolved: the arrangement's original coordination function has mutated into a permanent doctrinal management system where the limiting principles' instability is a feature, not a bug — it allows contextual calibration without constitutional amendment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_boundary_stability,
    'Is the economic/non-economic distinction a stable doctrinal boundary or an inherently manipulable line that collapses under pressure?',
    'Track Supreme Court cases applying the distinction over 20+ years: if the boundary holds against diverse factual challenges, it is stable; if it requires constant ad hoc exceptions or formalistic jurisdictional elements, it is manipulable.',
    'If manipulable, the constraint''s suppression is higher than measured (the limiting principles are theater) and effective extraction on state autonomy approaches the broad effects test. If stable, the tangled rope classification holds — genuine coordination with bounded extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_stability, conceptual, 'Whether the core limiting principle has structural integrity or is a cover for plenary federal economic power.').

omega_variable(
    aggregation_doctrine_scope,
    'Does ''aggregation applies only to economic activity'' meaningfully limit Wickard/Raich, or is ''economic activity'' defined broadly enough to swallow the limitation?',
    'Analyze post-Lopez cases: if Congress can characterize virtually any regulated activity as ''economic'' (including non-commercial possession, cultivation, inactivity), the limitation is formal. If courts police the boundary (as in Lopez, Morrison, NFIB commerce clause holding), it has teeth.',
    'If the limitation is formal, extraction on state autonomy is higher (near broad effects test). If it has teeth, the tangled rope''s coordination function is genuine — states retain a meaningful sphere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_scope, empirical, 'Whether the aggregation limitation operates as a real constraint or a semantic gesture.').

omega_variable(
    kernels_readings_framing_underdetermination,
    'Does the commerce_clause_scope kernel admit only these three readings, or are there additional structurally distinct framings (e.g., a ''political safeguards'' reading where structural protection replaces judicial limits)?',
    'Map the full space of scholarly and judicial positions: if positions cluster cleanly into three, the kernel is well-described. If significant positions fall outside (e.g., process-based federalism, anti-commandeering as commerce clause limit), the kernel decomposition is incomplete.',
    'If additional framings exist, this story''s claimed_type and metrics describe only one slice of a richer constraint family. The network.affects_constraints links would need expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernels_readings_framing_underdetermination, conceptual, 'Whether the declared kernel-reading decomposition captures the full structural space of the dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 1789, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_scope__intermediate_channels, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(comm_tr_t1824, commerce_clause_scope__intermediate_channels, theater_ratio, 1824, 0.08).
narrative_ontology:measurement(comm_tr_t1895, commerce_clause_scope__intermediate_channels, theater_ratio, 1895, 0.25).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__intermediate_channels, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(comm_tr_t1942, commerce_clause_scope__intermediate_channels, theater_ratio, 1942, 0.1).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__intermediate_channels, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__intermediate_channels, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__intermediate_channels, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__intermediate_channels, theater_ratio, 2012, 0.32).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__intermediate_channels, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_scope__intermediate_channels, base_extractiveness, 1789, 0.1).
narrative_ontology:measurement(comm_be_t1824, commerce_clause_scope__intermediate_channels, base_extractiveness, 1824, 0.15).
narrative_ontology:measurement(comm_be_t1895, commerce_clause_scope__intermediate_channels, base_extractiveness, 1895, 0.2).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__intermediate_channels, base_extractiveness, 1937, 0.55).
narrative_ontology:measurement(comm_be_t1942, commerce_clause_scope__intermediate_channels, base_extractiveness, 1942, 0.7).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__intermediate_channels, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__intermediate_channels, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__intermediate_channels, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_scope__intermediate_channels, base_extractiveness, 2012, 0.42).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__intermediate_channels, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_scope__intermediate_channels, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(comm_su_t1824, commerce_clause_scope__intermediate_channels, suppression_requirement, 1824, 0.25).
narrative_ontology:measurement(comm_su_t1895, commerce_clause_scope__intermediate_channels, suppression_requirement, 1895, 0.6).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__intermediate_channels, suppression_requirement, 1937, 0.3).
narrative_ontology:measurement(comm_su_t1942, commerce_clause_scope__intermediate_channels, suppression_requirement, 1942, 0.2).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__intermediate_channels, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__intermediate_channels, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__intermediate_channels, suppression_requirement, 2005, 0.55).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_scope__intermediate_channels, suppression_requirement, 2012, 0.52).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_scope__intermediate_channels, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, dormant_commerce_clause).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, necessary_and_proper_clause_scope).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, anti_commandeering_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, spending_clause_conditionality).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the commerce_clause_scope kernel. The narrow_originalist reading (commerce = cross-border trade) claims Mountain status (natural law of the Constitution's text). The broad_effects_test reading claims Tangled Rope with higher extractiveness (plenary economic power). This intermediate_channels reading claims Tangled Rope with moderate extractiveness and active limiting principles. The three form a constraint family linked by mutual structural influence: each reading's viability depends on the others' perceived failures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, institutional, 0.2).
constraint_indexing:directionality_override(commerce_clause_scope__intermediate_channels, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
