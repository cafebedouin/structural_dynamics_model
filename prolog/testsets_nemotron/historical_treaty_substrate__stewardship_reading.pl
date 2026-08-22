% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Treaties as Relational Pacts for Shared Territorial Stewardship
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the stewardship_reading of the
 *   historical_treaty_substrate kernel. Treaties are read as relational pacts
 *   establishing ongoing mutual obligations for shared territorial
 *   stewardship — no cession of sovereignty occurred; instead, Indigenous
 *   nations and the settler state entered a persistent coordination
 *   arrangement for coexistence. The standing arrangement under contest is
 *   the unilateral extractive regime that replaced treaty relations; this
 *   reading measures extraction against that regime from the perspective of
 *   the treaty's own logic. The constraint claims tangled_rope: genuine
 *   coordination (shared ecosystem management, mutual consent governance)
 *   coexists with asymmetric extraction (the settler state's unilateral
 *   resource claims violate the mutual obligation structure).
 *
 * KEY AGENTS:
 *   - indigenous_nations: Primary beneficiaries (moderate/identity_locked) — retain territorial jurisdiction and stewardship authority under treaty; extraction occurs when unilateral state action violates consent
 *   - settler_state: Agenda setter with obligation burden (institutional/biographical) — administers treaty implementation but bears obligation to obtain consent and share governance; extraction flows from its unilateral claims
 *   - shared_ecosystems: Non-agent beneficiary — territorial lands, waters, and species that depend on coordinated stewardship rather than fragmented extraction
 *   - settler_state_unilateral_extractive_claims: Victim (extracted-from position) — the state's own extractive practices are constrained by treaty obligations; the constraint suppresses the state's ability to act unilaterally
 *   - legal_observers: Analytical observers (analytical/analytical) — courts, scholars, international bodies interpreting treaty obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.38).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.22).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Treaties as Relational Pacts for Shared Territorial Stewardship").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'a4f8efa9-2ae4-4aec-8c21-e946a7dd866d').
narrative_ontology:cs_kernel_codification('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', fixed_text).
narrative_ontology:cs_authority_grounding('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', lineage).
narrative_ontology:cs_interpretation_layer_present('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d').
narrative_ontology:cs_reading_relation('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', foundational, sovereignty_non_cession).
narrative_ontology:cs_axiom_status(sovereignty_non_cession, holdable).
narrative_ontology:cs_axiom_grounding('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', sovereignty_non_cession, deontological).
narrative_ontology:cs_axiom('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', foundational, shared_stewardship_obligation).
narrative_ontology:cs_axiom_status(shared_stewardship_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', shared_stewardship_obligation, conventional).
narrative_ontology:cs_reference_frame('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', treaty_relational_formation).
narrative_ontology:cs_drift_state('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', contemporary_unilateral_extraction_regime, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a4f8efa9-2ae4-4aec-8c21-e946a7dd866d', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, shared_ecosystems).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_state_unilateral_extractive_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_state).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, shared_stewardship_obligation).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, sovereignty_non_cession_principle).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, mutual_consent_governance).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, territorial_relationality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain territorial jurisdiction and stewardship authority under treaty; their nationhood is constituted through the territorial relationship. Exit from the treaty relationship would mean exit from the territorial identity itself — not a feasible option. They bear the cost of enforcing treaty obligations through courts and political action, but receive the coordination benefit of recognized stewardship authority.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    moderate, generational, identity_locked, national).

% Administers treaty implementation and bears the obligation to obtain consent and share governance. Its unilateral extractive claims (resource development, land use decisions without consent) are suppressed by the constraint. The state cannot exit the treaty without constitutional crisis, but its exit options are constrained rather than trapped — it could theoretically pursue amendment or renunciation at high political cost.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_state, payer).

% Territorial lands, waters, and species that depend on coordinated stewardship rather than fragmented extraction. They have no agency and no exit — they are the substrate the constraint coordinates. Their inclusion as beneficiary reflects the treaty's ecological logic: stewardship obligations run to the territory itself, not only to human parties.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, shared_ecosystems, beneficiary,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__stewardship_reading, shared_ecosystems).

% The structural position of the settler state's extractive practices (resource development, infrastructure, land allocation) that operate without Indigenous consent. These claims are the extraction target of the stewardship constraint — they are suppressed by the requirement for joint management and consent. This is not an agent but a structural position: the constraint extracts from unilateralism by requiring coordination.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_unilateral_extractive_claims, payer,
    institutional, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(historical_treaty_substrate__stewardship_reading, settler_state_unilateral_extractive_claims).

% Courts, scholars, and international bodies interpreting treaty obligations. They observe the constraint's operation from outside the beneficiary/payer structure. Their analytical seat has no extraction exposure — they assess whether the constraint functions as coordination or extraction, and their rulings shape enforcement.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, legal_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of governing ecologically interconnected territories across jurisdictional boundaries — Indigenous nations and the settler state share watersheds, wildlife corridors, and resource systems that no single jurisdiction can manage alone. The treaty creates a persistent coordination structure for joint decision-making.
% TRANSFER_FUNCTION: Moves decision-making authority over territorial resources from unilateral state control to shared governance requiring mutual consent. The settler state gives up exclusive extraction rights; Indigenous nations contribute stewardship knowledge and jurisdictional authority. The transfer is ongoing, not a one-time cession.
% ABSENT_VOICES: Indigenous nations that did not sign treaties (unceded territories) — they would object to a framework that treats treaty relationships as the only legitimate basis for Indigenous jurisdiction. Also absent: future generations of both Indigenous and settler populations who will inherit the ecological consequences of current stewardship decisions or failures.
% DISAPPEARANCE_RATIONALE: If the stewardship constraint vanished overnight, the settler state would revert to unilateral resource extraction across treaty territories without consent requirements. Indigenous nations would lose the legal basis for co-management authority and consent rights. The territorial governance regime would reorganize entirely around state sovereignty — the world rearranges because the constraint is the only structure preventing unilateral extraction.
% FOUNDING_PROBLEM: Establishing peaceful coexistence between Indigenous nations and incoming settlers without either party ceding their fundamental territorial jurisdiction — creating a relational framework for shared stewardship of lands and waters that both peoples depend on, where sovereignty remains with each nation and governance is coordinated through mutual consent.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal traditions and oral histories corroborate the non-cession, shared-stewardship understanding (e.g., Anishinaabe gakina-awiiya, Haudenosaunee Guswenta / Two Row Wampum). Settler-state courts have increasingly acknowledged this interpretation (e.g., Tsilhqot'in Nation v. British Columbia 2014; Haida Nation v. British Columbia 2004). The corroboration comes from outside the settler-state beneficiary set — from Indigenous legal orders themselves and from judicial recognition that the founding problem (coexistence without cession) persists.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).
:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the ongoing gap between the treaty's shared stewardship logic and the realized unilateral extractive regime — the constraint extracts from the settler state's extractive claims by requiring consent and joint management. Suppression (0.22) is moderate: the constraint requires active enforcement (court decisions, co-management bodies, consent protocols) but does not rely on overwhelming coercion; its persistence depends on legal and political recognition. Theater ratio (0.15) is low but rising: early treaty implementation had genuine coordination; recent decades show increasing performative consultation without binding consent. Accessibility collapse (0.58) is moderate — alternatives (unilateral state management) remain structurally available but are legally contested. Resistance (0.31) comes primarily from state actors resisting consent requirements and resource-sharing obligations.
 *
 * PERSPECTIVAL GAP:
 *   From the Indigenous nation seat: the constraint is a rope (genuine coordination for stewardship, they are beneficiaries). From the settler state seat: the constraint is a tangled_rope (it administers but its extractive interests are suppressed — coordination for others, extraction from its claims). From the analytical observer seat: the constraint computes as tangled_rope overall — the mutual obligation structure creates real coordination while actively suppressing unilateral extraction. The engine will compute per-seat classifications from the structural data; this divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are beneficiaries with identity_locked exit (territorial jurisdiction is constitutive of nationhood; exit is not an option). The settler state is the agenda_setter but bears obligation costs — its directionality is complex: it administers the constraint (low d as administrator) but is also the primary target of extraction-suppression (high d as the party whose unilateral claims are constrained). Shared ecosystems are non-agent beneficiaries — they receive coordination benefits without agency. The settler_state_unilateral_extractive_claims is a structural victim position: the constraint suppresses these claims, which from the extraction perspective means the extractive arrangement is the victim of the constraint's coordination function. This inversion is why the constraint is tangled_rope: the same structure coordinates stewardship AND suppresses extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (peaceful coexistence and shared territorial stewardship without sovereignty cession) remains live — the ecological and jurisdictional interdependencies that motivated treaties persist. However, the constraint's mandate has partially atrophied: the settler state's unilateral extraction replaced shared governance for decades. The constraint persists because Indigenous nations continue to enforce treaty obligations through courts and political action, not because the settler state finds it useful. This is not mandatrophy in the classic sense (function gone, form remains) — the function is live for one party and suppressed by the other. The tension is structural, not obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the structural disagreement between stewardship_reading, extinguishment_reading, and nation_to_nation_reading locate — in the beneficiary/victim assignment, in the extraction referent, or in the constraint''s temporal horizon?',
    'Structural comparison of the three readings'' ε values, beneficiary/victim arrays, and directionality derivations — the disagreement must manifest as differing seat-level classifications from identical kernel text.',
    'If the disagreement is in beneficiary/victim assignment, the three readings are structurally distinct constraints (separate files). If in extraction referent, the kernel text itself is ambiguous on what arrangement is under contest. If in temporal horizon, the readings differ on whether the constraint''s function is founding (extinguishment), ongoing (stewardship), or forward-looking (nation-to-nation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural locus of the three-way reading disagreement in the historical_treaty_substrate kernel').

omega_variable(
    stewardship_extraction_referent_ambiguity,
    'For the stewardship reading, is ε measured against the standing arrangement of unilateral settler extraction (high ε) or the realized shared-governance regime (lower ε, higher coordination)?',
    'Compare ε authored in this reading against ε in the extinguishment_reading and nation_to_nation_reading. The ε-invariance principle requires the referent to be the standing arrangement under contest — for stewardship_reading this is the current unilateral extractive regime, assessed by stewardship''s lights.',
    'If ε is measured against the extractive status quo, stewardship_reading shows high extraction (the gap between unilateralism and shared stewardship). If measured against an idealized shared regime, ε would be near-zero — but that would violate the ε-invariance rule that the reading''s endorsed alternative is not the referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stewardship_extraction_referent_ambiguity, conceptual, 'Whether ε for stewardship_reading refers to the unilateral extractive status quo or the shared-governance ideal').

omega_variable(
    shared_stewardship_coordination_genuineness,
    'Is the shared stewardship obligation a genuine coordination function (solving the collective-action problem of territorial management across jurisdictional boundaries) or does the coordination framing mask continued settler extraction?',
    'Empirical observation of whether joint management bodies have binding authority over resource decisions or merely advisory roles; whether Indigenous nations'' consent is structurally required or merely consulted.',
    'If coordination is genuine, the constraint is a tangled_rope with real mutual benefit. If the coordination is performative, the constraint reclassifies toward snare — the stewardship language becomes cover for continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shared_stewardship_coordination_genuineness, empirical, 'Whether shared stewardship functions as genuine coordination or extractive theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__stewardship_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__stewardship_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__stewardship_reading, base_extractiveness, 25, 0.22).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__stewardship_reading, base_extractiveness, 75, 0.34).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__stewardship_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.16).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__stewardship_reading, suppression_requirement, 75, 0.2).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__stewardship_reading, 0.12).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the historical_treaty_substrate kernel. The three readings form a constraint family linked by network.affects_constraints. The stewardship_reading assigns ε=0.38 against the unilateral extractive status quo; extinguishment_reading likely assigns low ε (completed transaction, no ongoing extraction); nation_to_nation_reading likely assigns moderate ε (ongoing consent requirements constrain both parties symmetrically). The ε values differ because each reading identifies a different standing arrangement as the referent and different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, institutional, 0.65).
constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
