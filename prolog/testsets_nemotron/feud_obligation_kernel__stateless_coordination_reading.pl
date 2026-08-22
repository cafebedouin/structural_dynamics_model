% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination Mechanism
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the stateless_coordination_reading of
 *   the feud_obligation_kernel: blood-feud obligations function as a
 *   self-enforcing coordination mechanism delivering justice and deterrence
 *   where centralized enforcement is absent. Feud participants (kinship group
 *   members, local community participants) enter the beneficiary set — they
 *   receive dispute resolution, deterrence against predation, and collective
 *   defense. Defectors and honor transgressors enter the victim set — they
 *   bear honor loss, kinship expulsion, and retaliatory violence. Alternative
 *   dispute mechanisms (wergild, mediation assemblies) coexist with low
 *   suppression; they are formally available and materially utilized in many
 *   cases. The constraint is claimed as a rope: genuine coordination, net
 *   beneficiary participants, minimal coercive overhead. Metrics reflect
 *   this: low base extractiveness (0.25), very low suppression (0.18), low
 *   theater ratio (0.12). The time series models a 250-year interval from
 *   early tribal formation through early state contact: extractiveness and
 *   suppression rise modestly as feudal/state structures encroach and feud
 *   obligation is pressed into service for territorial claims, then recede
 *   slightly as state law absorbs the coordination function. This reading
 *   does NOT describe the contest — the christianized_pacification_reading
 *   and extraction_cycle_reading are separate constraints (other files)
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - kinship_group_members: Primary beneficiary (organized/identity_locked) — receive justice, deterrence, collective defense
 *   - local_community_participants: Secondary beneficiary (organized/constrained) — benefit from dispute resolution and order
 *   - feud_defectors: Primary victim (powerless/trapped) — bear honor loss, kinship expulsion, retaliatory violence
 *   - honor_transgressors: Primary victim (powerless/identity_locked) — bear full feud sanctions for norm violation
 *   - wergild_mediators: Observer (organized/arbitrage) — administer compensation alternative
 *   - early_state_actors: Observer (institutional/analytical) — attempt to supplant or co-opt feud mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.25).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.18).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination Mechanism").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '90e58145-00e5-4c36-86b4-b5b444fc0fa0').
narrative_ontology:cs_kernel_codification('90e58145-00e5-4c36-86b4-b5b444fc0fa0', implicit).
narrative_ontology:cs_authority_grounding('90e58145-00e5-4c36-86b4-b5b444fc0fa0', practice).
narrative_ontology:cs_interpretation_layer_present('90e58145-00e5-4c36-86b4-b5b444fc0fa0').
narrative_ontology:cs_reading_relation('90e58145-00e5-4c36-86b4-b5b444fc0fa0', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_reading_relation('90e58145-00e5-4c36-86b4-b5b444fc0fa0', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('90e58145-00e5-4c36-86b4-b5b444fc0fa0', foundational, distributed_justice_legitimate_without_state).
narrative_ontology:cs_axiom_status(distributed_justice_legitimate_without_state, holdable).
narrative_ontology:cs_axiom_grounding('90e58145-00e5-4c36-86b4-b5b444fc0fa0', distributed_justice_legitimate_without_state, empirically_contingent).
narrative_ontology:cs_axiom('90e58145-00e5-4c36-86b4-b5b444fc0fa0', foundational, honor_based_deterrence_functionally_sufficient).
narrative_ontology:cs_axiom_status(honor_based_deterrence_functionally_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('90e58145-00e5-4c36-86b4-b5b444fc0fa0', honor_based_deterrence_functionally_sufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('90e58145-00e5-4c36-86b4-b5b444fc0fa0', stateless_customary_order).
narrative_ontology:cs_drift_state('90e58145-00e5-4c36-86b4-b5b444fc0fa0', early_state_contact, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('90e58145-00e5-4c36-86b4-b5b444fc0fa0', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kinship_group_members).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, local_community_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, honor_transgressors).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, customary_law_legitimacy).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, distributed_justice_viability).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, honor_based_deterrence_effectiveness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the kinship group bound by feud obligation. They receive collective defense, dispute resolution, and deterrence against external predation. Their identity is fused with the group — exit means loss of kinship identity, social world, and protection. They pay the symmetric cost of reciprocal vengeance liability (if their kin kills, they must avenge or pay wergild). This is the coordination cost, not extraction.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kinship_group_members, beneficiary,
    organized, generational, identity_locked, local).

% Non-kin community members who participate in the feud system's dispute resolution and benefit from its order-maintenance function. They can access mediation assemblies and wergild processes. Exit is constrained — they could relocate to another valley or submit to a nearby lord, but doing so abandons land, social network, and familiar institutions. They pay less than kin (no reciprocal vengeance liability) but receive less protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, local_community_participants, beneficiary,
    organized, biographical, constrained, local).

% Individuals who refuse feud obligation — they will not avenge kin killings, will not contribute to wergild, or actively undermine the group's deterrence. They bear the full cost: honor loss (social death), kinship expulsion (loss of all protection and identity), and often retaliatory violence from their own kin. They are trapped — the feud system defines the social world; there is no 'outside' within the stateless zone. Their extraction is maximal.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    powerless, immediate, trapped, local).

% Individuals who violate honor norms (killing a kinsman, betraying the group, violating sanctuary). Their identity is bound to the honor system — transgression destroys their standing within it. They bear feud sanctions (blood vengeance or exorbitant wergild) because the system's deterrence credibility requires it. Exit is identity_locked: they cannot 'leave' the honor frame without ceasing to be who they are in this world. The constraint extracts from them to maintain credibility for beneficiaries.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, honor_transgressors, payer,
    powerless, immediate, identity_locked, local).

% Respected elders, priests, or neutral kin who administer wergild compensation processes. They operate the alternative dispute mechanism that coexists with feud. They have arbitrage-grade exit — they can serve multiple communities, shift to ecclesiastical courts, or join early state administration. They neither collect from nor pay into the feud obligation; they observe and facilitate the coordination-extraction boundary.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_mediators, observer,
    organized, biographical, arbitrage, regional).

% Emerging royal/imperial authorities who view feud obligation as a rival jurisdiction. They attempt to supplant it with state law (king's peace, royal courts) or co-opt it (ennobling feud leaders as royal officials). They are analytical observers of the constraint's structure — their interest is in measuring its extractiveness and coordination value for replacement or absorption. They do not participate in the feud system.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, early_state_actors, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of deterrence and dispute resolution in stateless zones: reciprocal vengeance liability makes predation unprofitable, and the kinship group structure provides the enforcement capacity that a state would otherwise supply.
% TRANSFER_FUNCTION: Moves the cost of deterrence (reciprocal vengeance risk, wergild contributions) from the group as a whole onto defectors and transgressors who free-ride on the group's protection while violating its norms. The transfer is from defectors → group stability; from transgressors → deterrence credibility.
% ABSENT_VOICES: Women and children within kinship groups (often excluded from feud decision-making but bearing disproportionate mortality and displacement); slaves and unfree persons (no honor standing, no feud rights, pure victims); merchants and itinerants (outside the kinship system entirely, no access to its justice). They would object to the system's gendered, status-bound, and exclusionary character but are structurally silenced.
% DISAPPEARANCE_RATIONALE: If feud obligation vanished overnight in a stateless zone, predation would increase immediately (no deterrence), dispute resolution would collapse (no trusted third party), and kinship groups would either fragment or rapidly form substitute protection pacts (warlordism, early state formation). The world rearranges because the constraint IS the order-maintenance infrastructure.
% FOUNDING_PROBLEM: Stateless zones face a Hobbesian security dilemma: no centralized enforcement, predation is profitable, and no individual can credibly commit to deterrence alone. Feud obligation was built to solve this by making deterrence a collective, identity-bound obligation — the group avenges, so the individual need not.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by anthropological consensus (Evans-Pritchard on Nuer, Gluckman on Lozi, Boehm on Montenegrin clans) and historical sources (Icelandic sagas, Germanic law codes, Albanian Kanun) from outside any single feud system's beneficiaries. The founding problem remains live in contemporary stateless zones (Somalia, Yemen, tribal Pakistan/Afghanistan) where feud-like mechanisms persist.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The coordination function is genuine: in stateless zones, feud obligation solves the collective action problem of deterrence against predation without a state. Participants are net beneficiaries — they gain security and dispute resolution they could not individually produce. Extraction is low because the obligation's operation (reciprocal vengeance liability) is symmetric within the group; the cost falls on defectors who free-ride on the group's deterrence while violating its norms. Suppression is low because wergild and mediation are structurally available alternatives — the constraint does not actively suppress them, though honor norms create social friction against their use (captured in accessibility_collapse = 0.68). Resistance is moderate (0.45) because defectors and transgressors do resist, and external actors (early states, church) contest the mechanism. Theater is low because the mechanism's performative and functional components align — public vengeance displays ARE the deterrence signal.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different effective extractions per seat: kinship_group_members (beneficiary, d ≈ 0.15) experience near-zero χ; feud_defectors (victim, d ≈ 0.9) experience high χ; local_community_participants (beneficiary, d ≈ 0.3) experience low positive χ; wergild_mediators (observer, d = 0.5) experience symmetric χ ≈ ε. This seat divergence is the measurement — the claimed rope type holds for beneficiaries but computes toward tangled_rope for victims. The constraint is a rope from the participant seat and a snare from the defector seat. This divergence is structurally correct and should not be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: kinship_group_members and local_community_participants receive the coordination product (justice, deterrence, order) and pay only the symmetric cost of reciprocal liability — which is the coordination cost, not extraction. Victims: feud_defectors and honor_transgressors bear the asymmetric cost (expulsion, violence) because they defect from the coordination compact. The directionality derivation from beneficiary/victim declarations + exit options (identity_locked for kinship members, trapped for defectors) produces the correct d-values. No overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless deterrence and dispute resolution) remains live in the zones where this constraint operates — the constraint has not outlived its function. Mandatrophy is not resolved; the constraint persists because the problem persists. If state enforcement arrives, the constraint becomes a scaffold with a sunset clause (state law absorption) or a piton (theatrical maintenance of feud forms after functional obsolescence). This reading captures the functional phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the stateless coordination reading a genuine structural description of feud obligations, or does it encode a romanticization of pre-state violence that obscures extraction?',
    'Comparative analysis of feud mortality rates, productive capacity depletion, and alternative dispute resolution uptake across societies with identical ecological conditions but different cultural framings of feud obligation.',
    'If feud obligation is structurally extractive despite its coordination function, the reading is a false rope masking a tangled_rope or snare; if coordination is genuine and extraction minimal, the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the coordination framing is structurally accurate or a cover story.').

omega_variable(
    wergild_coexistence_depth,
    'Does wergild (compensation payment) coexist as a genuine alternative that parties can freely choose, or is it a formal option blocked by honor norms that make non-feud settlement socially impossible?',
    'Ethnographic and historical record of settlement rates vs. feud continuation rates when wergild is formally offered and materially adequate.',
    'If wergild is nominal but honor norms prevent uptake, suppression is structurally higher than measured; the constraint becomes tangled_rope or snare rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_coexistence_depth, empirical, 'Depth of alternative dispute mechanism availability.').

omega_variable(
    christianized_pacification_interface,
    'Does the christianized_pacification_reading foreclose this reading within any single authority framework, or do they coexist across different parties?',
    'Historical analysis of societies undergoing Christianization: did ecclesiastical authorities treat feud obligation as logically incompatible with divine law (foreclosure) or as a customary practice to be redirected (coexistence with pressure)?',
    'If foreclosure, the two readings cannot both be holdable in one framework; if coexistence, both remain live across different institutional seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(christianized_pacification_interface, conceptual, 'Structural relationship to the christianized_pacification_reading sibling.').

omega_variable(
    extraction_cycle_emergence,
    'Does the extraction_cycle_reading describe a distinct constraint (a different arrangement) or the same constraint viewed from a different seat?',
    'Measure whether feud obligation''s productive depletion and territorial consolidation effects are structurally inseparable from its coordination function (same constraint, different χ) or whether extraction cycles emerge only when specific power configurations activate (distinct constraint, decomposition required per ε-invariance).',
    'If same constraint, the readings are seats of one story; if distinct, they are separate stories linked by network.affects_constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_cycle_emergence, conceptual, 'Constraint identity boundary with the extraction_cycle_reading sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_stateless_coord_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(feud_stateless_coord_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.08).
narrative_ontology:measurement(feud_stateless_coord_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.1).
narrative_ontology:measurement(feud_stateless_coord_tr_t150, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 150, 0.12).
narrative_ontology:measurement(feud_stateless_coord_tr_t200, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(feud_stateless_coord_tr_t250, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 250, 0.12).

% Extraction over time
narrative_ontology:measurement(feud_stateless_coord_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(feud_stateless_coord_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(feud_stateless_coord_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.22).
narrative_ontology:measurement(feud_stateless_coord_be_t150, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 150, 0.25).
narrative_ontology:measurement(feud_stateless_coord_be_t200, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement(feud_stateless_coord_be_t250, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 250, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(feud_stateless_coord_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(feud_stateless_coord_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement(feud_stateless_coord_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement(feud_stateless_coord_su_t150, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 150, 0.18).
narrative_ontology:measurement(feud_stateless_coord_su_t200, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 200, 0.2).
narrative_ontology:measurement(feud_stateless_coord_su_t250, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 250, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% Feud_obligation_kernel decomposes into three constraint stories by ε-invariance: this reading (rope, ε=0.25), christianized_pacification_reading (snare/tangled_rope from ecclesiastical seat, ε higher due to suppression of customary law), extraction_cycle_reading (tangled_rope/snare from state-building seat, ε higher due to productive depletion). The readings differ in what they treat as the constraint's referent and which agents they place in beneficiary/victim sets. All three share the kernel_id feud_obligation_kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
