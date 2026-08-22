% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story models the blood-feud obligation as a destructive
 *   extraction cycle (the 'extraction_cycle_reading' of the
 *   feud_obligation_kernel). In medieval Western Europe (c. 900-1300),
 *   kinship-based vengeance obligations created a self-sustaining cycle of
 *   killing and counter-killing that depleted the productive capacity of
 *   feuding lineages and their dependents while preventing territorial
 *   consolidation under royal authority. The royal authority benefited from
 *   the disorder — it justified expanded taxation, judicial monopolies, and
 *   military capacity as the 'pacifier.' The feud participants (lineage
 *   heads, members, and their tenant producers) were the primary victims,
 *   locked in by identity-fused exit barriers. Ecclesiastical authorities and
 *   merchants were excluded voices. The constraint is claimed as a snare: its
 *   coordination function (deterrence in stateless zones) is real but
 *   subordinated to extraction that serves royal state-building.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.78).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.85).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '19db236d-2b19-4793-a29c-9495f78fe3bd').
narrative_ontology:cs_kernel_codification('19db236d-2b19-4793-a29c-9495f78fe3bd', implicit).
narrative_ontology:cs_authority_grounding('19db236d-2b19-4793-a29c-9495f78fe3bd', practice).
narrative_ontology:cs_interpretation_layer_present('19db236d-2b19-4793-a29c-9495f78fe3bd').
narrative_ontology:cs_reading_relation('19db236d-2b19-4793-a29c-9495f78fe3bd', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('19db236d-2b19-4793-a29c-9495f78fe3bd', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('19db236d-2b19-4793-a29c-9495f78fe3bd', foundational, kinship_vengeance_depletes_productive_capacity).
narrative_ontology:cs_axiom_status(kinship_vengeance_depletes_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('19db236d-2b19-4793-a29c-9495f78fe3bd', kinship_vengeance_depletes_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('19db236d-2b19-4793-a29c-9495f78fe3bd', foundational, royal_authority_legitimizes_taxation_through_pacification_claim).
narrative_ontology:cs_axiom_status(royal_authority_legitimizes_taxation_through_pacification_claim, holdable).
narrative_ontology:cs_axiom_grounding('19db236d-2b19-4793-a29c-9495f78fe3bd', royal_authority_legitimizes_taxation_through_pacification_claim, conventional).
narrative_ontology:cs_reference_frame('19db236d-2b19-4793-a29c-9495f78fe3bd', stateless_kinship_vengeance_equilibrium).
narrative_ontology:cs_drift_state('19db236d-2b19-4793-a29c-9495f78fe3bd', high_medieval_state_formation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('19db236d-2b19-4793-a29c-9495f78fe3bd', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feuding_lineage_heads).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, lineage_members).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, tenant_producers).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__extraction_cycle_reading, monopoly_on_violence_justifies_taxation).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__extraction_cycle_reading, kinship_violence_undermines_state_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims monopoly on legitimate violence; uses feud persistence as justification for expanding royal courts, taxation, and military capacity. Gains legitimacy and revenue from the disorder the feuds create. Can offer pardons, impose fines, or deploy force to suppress feuds but often allows them to persist at manageable levels to extract political capital.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary).

% Obligated by kinship honor to pursue vengeance for slain kin. Each killing obligates the next; refusal means loss of status, authority within the lineage, and social death. They administer the feud, allocate resources to it, and bear direct mortality risk. Some negotiate truces or accept compensation (blood money) but face internal challengers who denounce compromise as cowardice.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feuding_lineage_heads, payer,
    organized, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, feuding_lineage_heads, agenda_setter).

% Provide labor, food, weapons, shelter, and fighters for the feud. Their productive capacity (farming, herding, craft) is diverted to feud support. Men face conscription into raids; women manage depleted households. Children inherit the obligation at birth. Exit means abandoning kin, land, and identity — social death in a stateless world.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, lineage_members, payer,
    moderate, biographical, identity_locked, local).

% Work lands claimed by feuding lineages. Subject to raids, requisition, and retaliatory destruction of crops and infrastructure. Pay rents to lineages that fund feuds. Have no voice in feud decisions but bear disproportionate material loss. Can sometimes flee to royal towns or ecclesiastical lands but lose tenure and community.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, tenant_producers, payer,
    powerless, immediate, constrained, local).

% Preach against vengeance as sin; administer peace oaths (Treuga Dei), penance, and sanctuary. Their authority is moral, not coercive — they cannot stop feuds but can delegitimize them. Some bishops hold temporal power and act as local peace enforcers, creating tension between spiritual and temporal roles.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authorities, excluded,
    institutional, generational, analytical, national).

% Move goods across feud territories. Pay tolls, ransoms, and protection fees to multiple lineages. Suffer unpredictable predation. Their mobility is an exit option unavailable to sedentary producers, but it makes them visible targets. They lobby royal authority for safe conducts and unified toll systems.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, merchant_travelers, excluded,
    moderate, immediate, mobile, regional).

% Analyzes feud systems across societies (Montenegrin, Albanian, Corsican, Scottish Highland, Somali, Yemeni). Sees the structural pattern: kinship-enforced vengeance as a coordination mechanism that becomes extractive when it blocks state formation and locks participants into negative-sum cycles. No stake in any particular feud outcome.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, comparative_legal_anthropologist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides deterrence and dispute resolution in the absence of a state monopoly on violence: the threat of kinship retaliation substitutes for police and courts, making aggression costly and enabling interpersonal trust within the lineage.
% TRANSFER_FUNCTION: Moves productive labor, agricultural surplus, and lives from feuding lineages and their dependents to the feud apparatus itself (weapons, raids, fortifications, blood money payments) and indirectly to royal authority (taxes justified by disorder, fines, court fees, military service commutations).
% ABSENT_VOICES: Women and children within feuding lineages — they bear reproductive and care burdens amplified by male mortality and resource diversion but have no formal voice in feud decisions. Also absent: future generations who inherit depleted territories and locked-in obligations; their interests are represented by no one in the current negotiation.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight, lineages would lose their primary coercive lever; royal courts would become the sole dispute forum; taxation would lose its 'pacification' justification; tenant producers would retain more surplus; ecclesiastical peace institutions would lose their central moral mission. The political economy of the region would reorganize around state law rather than kinship vengeance.
% FOUNDING_PROBLEM: In stateless or weak-state societies, how to deter predation and resolve lethal disputes without centralized enforcement? The feud obligation answered: make the kin group the enforcement unit — an attack on one is an attack on all, creating deterrence through collective liability.
% FOUNDING_PROBLEM_CORROBORATION: Medieval chroniclers (e.g., Gregory of Tours, Nithard) attest the founding problem was real: royal courts were distant, slow, and corrupt; kin vengeance was the only reliable deterrent. Modern legal anthropologists (Boehm, Durkheim, Evans-Pritchard) corroborate the coordination function in stateless societies. Royal charters and capitularies from the 10th-13th centuries document the crown's deliberate substitution of royal justice for private vengeance — corroborating that the founding problem was live but the feud solution became an obstacle to state consolidation.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.78) reflects the massive diversion of labor, surplus, and lives from productive activity into the feud apparatus. High suppression (0.85) reflects the active enforcement of the obligation: refusal means social death, loss of kin protection, and often physical elimination by rival lineages or internal challengers. Theater ratio (0.42) captures the growing performative aspect: as royal courts expand, feud rituals (formal challenges, truces, blood money negotiations) become increasingly ceremonial — the 'honor' performance masks the material extraction. Accessibility collapse (0.35) is moderate: alternatives (royal courts, ecclesiastical mediation, money compensation) existed but were structurally inaccessible to identity-locked participants. Resistance (0.68) is high: lineages resisted royal encroachment, peasants fled, merchants lobbied, and the Church preached peace — but the cycle persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the royal seat, the feud is a justification — a problem the crown solves by expanding its courts and tax base. From the lineage head seat, the feud is a duty — honor requires vengeance, and the alternative is social death. From the lineage member seat, the feud is a tax — labor and lives extracted for a cause they did not choose. From the tenant producer seat, the feud is predation — they pay rents to fund raids that destroy their crops. The engine computes these divergences from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Royal authority is the structural beneficiary (d near 0.0): it collects taxes, fines, and legitimacy from the disorder. Feuding lineage heads are dual-positioned: they administer the extraction (agenda_setter) but also bear its costs (payer) — their d is mid-range (~0.4-0.5) because they gain status/power within the kinship system while losing productive capacity and kin. Lineage members and tenant producers are full targets (d near 1.0): identity-locked and constrained exit respectively, they bear extraction without administrative control. Ecclesiastical authorities and merchants are excluded: they experience the constraint's externalities but are not governed by its internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless deterrence) was live c. 900 but became increasingly dead as royal courts expanded. The feud obligation persisted because it was repurposed: royal authority extracted legitimacy from suppressing it; lineage heads extracted status from performing it. The constraint did not atrophy — it was captured. Mandatrophy is resolved: the original coordination function is vestigial; the current function is extraction for state-building and kinship-status maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'At what point does the feud''s deterrence function become subordinate to its extraction function? Is there a measurable threshold where the marginal deterrence value of another killing is negative?',
    'Comparative analysis of feud intensity vs. raid frequency across regions with varying royal court penetration; econometric estimation of deterrence elasticity.',
    'If deterrence value turns negative early, the feud is a snare from inception; if it remains positive until late, the snare classification applies only to the terminal phase. Changes the temporal scope of the extraction claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'The coordination-extraction boundary in kinship vengeance systems.').

omega_variable(
    royal_complicity_intentionality,
    'Did royal authorities deliberately allow feuds to persist to extract political capital, or did they genuinely lack capacity to suppress them?',
    'Analysis of royal charters, fiscal records, and judicial activity: did crown revenue from feud-related fines/taxes exceed the cost of suppression? Did suppression efforts correlate with feud intensity or fiscal need?',
    'If deliberate, royal authority is an active architect of the snare (beneficiary with agency); if capacity-constrained, it is an opportunistic beneficiary. Changes the beneficiary role from passive to active extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_complicity_intentionality, empirical, 'Intentionality of royal beneficiary position in the feud extraction cycle.').

omega_variable(
    identity_lock_mechanism,
    'Is the lineage member''s identity lock primarily professional (career path dependence), relational (self constituted through kinship role), ideological (honor code makes exit unthinkable), or institutional (the lineage has become its feud function)?',
    'Ethnographic comparison with contemporary honor cultures; analysis of lineage internal discourse (sagas, chronicles, oral traditions) for exit narratives; demographic data on lineage fission/fusion rates.',
    'If professional/relational, exit becomes possible when alternative status paths open (e.g., royal service, monastic life). If ideological/institutional, the lock persists until the honor code or lineage institution itself collapses. Determines whether identity_lock is a hard or soft barrier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Mechanism of identity lock in kinship vengeance obligations.').

omega_variable(
    kernel_reading_structure,
    'Does the extraction_cycle_reading structurally foreclose the stateless_coordination_reading, or do they coexist as competing framings held by different historical actors?',
    'Trace whether any single historical actor (chronicler, legislator, lineage head) invoked both framings simultaneously, or whether they map to distinct social positions (royal vs. lineage, clerical vs. lay).',
    'If forecloses, the kernel has a logical contradiction at its core — one reading''s truth entails the other''s falsity within a single framework. If coexists_with, the kernel sustains multiple stable equilibria. Determines the cs_structure.reading_relations assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationship between extraction_cycle_reading and stateless_coordination_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 900, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t900, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement(feud_tr_t950, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 950, 0.18).
narrative_ontology:measurement(feud_tr_t1000, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1000, 0.24).
narrative_ontology:measurement(feud_tr_t1050, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1050, 0.29).
narrative_ontology:measurement(feud_tr_t1100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1100, 0.33).
narrative_ontology:measurement(feud_tr_t1150, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1150, 0.37).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1200, 0.4).
narrative_ontology:measurement(feud_tr_t1250, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1250, 0.41).
narrative_ontology:measurement(feud_tr_t1300, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1300, 0.42).

% Extraction over time
narrative_ontology:measurement(feud_be_t900, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 900, 0.45).
narrative_ontology:measurement(feud_be_t950, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 950, 0.52).
narrative_ontology:measurement(feud_be_t1000, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(feud_be_t1050, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1050, 0.65).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1100, 0.71).
narrative_ontology:measurement(feud_be_t1150, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1150, 0.74).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1200, 0.76).
narrative_ontology:measurement(feud_be_t1250, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1250, 0.77).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1300, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t900, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement(feud_su_t950, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 950, 0.62).
narrative_ontology:measurement(feud_su_t1000, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1000, 0.68).
narrative_ontology:measurement(feud_su_t1050, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1050, 0.73).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1100, 0.78).
narrative_ontology:measurement(feud_su_t1150, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1150, 0.81).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1200, 0.83).
narrative_ontology:measurement(feud_su_t1250, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1250, 0.84).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1300, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.08).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, royal_judicial_monopoly).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_peace_institutions).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, tenant_tenure_systems).

% DUAL FORMULATION NOTE:
% Part of the feud_obligation_kernel constraint family. The stateless_coordination_reading emphasizes the feud's deterrence function in stateless zones (rope/tangled_rope). The christianized_pacification_reading frames the feud as sin requiring ecclesiastical/royal suppression (scaffold/tangled_rope). This reading (extraction_cycle_reading) models the feud as a snare that extracts from participants while legitimizing royal state-building. All three share the same referent (kinship vengeance obligation) but disagree on ε and beneficiary/victim structure — per ε-invariance, they are three constraints linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, organized, 0.45).
constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, moderate, 0.85).
constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
