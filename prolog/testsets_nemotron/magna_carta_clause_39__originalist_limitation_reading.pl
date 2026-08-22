% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__originalist_limitation_reading, []).

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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 — Originalist Limitation Reading (1215-Specific Royal Abuses Only)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story models the originalist limitation reading of Magna
 *   Carta clause 39: the clause limits only the specific royal abuses King
 *   John actually committed and documented in the 1215 charter context. It
 *   does not establish a universal principle of due process, nor does it
 *   preserve a general feudal prerogative structure. It is a historically
 *   bounded peace treaty clause that names John's specific overreaches
 *   (arbitrary disseisin, outlawry without judgment, denial of court access,
 *   seizure without process) and gives the barons a legalized enforcement
 *   trigger against those specific acts. The reading's ε (0.28) reflects
 *   moderate extraction — the king loses specific prerogatives he abused, but
 *   retains all other royal powers. The victim is John personally (and only
 *   insofar as he commits the documented abuses); the beneficiaries are the
 *   barons who negotiated the charter and subsequent monarchs who gain
 *   legitimacy from a defined baronial relationship. This reading stands in a
 *   constraint family with the liberal due process reading (which
 *   universalizes the clause) and the feudal prerogative reading (which
 *   treats it as a conservative affirmation of existing hierarchy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.15).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 — Originalist Limitation Reading (1215-Specific Royal Abuses Only)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, 'b8090082-53bb-492e-b733-ee6ae72cbee9').
narrative_ontology:cs_kernel_codification('b8090082-53bb-492e-b733-ee6ae72cbee9', fixed_text).
narrative_ontology:cs_authority_grounding('b8090082-53bb-492e-b733-ee6ae72cbee9', lineage).
narrative_ontology:cs_interpretation_layer_present('b8090082-53bb-492e-b733-ee6ae72cbee9').
narrative_ontology:cs_reading_relation('b8090082-53bb-492e-b733-ee6ae72cbee9', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8090082-53bb-492e-b733-ee6ae72cbee9', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('b8090082-53bb-492e-b733-ee6ae72cbee9', foundational, clause_39_referent_is_johns_1215_abuses_only).
narrative_ontology:cs_axiom_status(clause_39_referent_is_johns_1215_abuses_only, holdable).
narrative_ontology:cs_axiom_grounding('b8090082-53bb-492e-b733-ee6ae72cbee9', clause_39_referent_is_johns_1215_abuses_only, empirically_contingent).
narrative_ontology:cs_axiom('b8090082-53bb-492e-b733-ee6ae72cbee9', foundational, lex_terrae_references_fixed_1215_customary_law).
narrative_ontology:cs_axiom_status(lex_terrae_references_fixed_1215_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('b8090082-53bb-492e-b733-ee6ae72cbee9', lex_terrae_references_fixed_1215_customary_law, empirically_contingent).
narrative_ontology:cs_reference_frame('b8090082-53bb-492e-b733-ee6ae72cbee9', runnymede_1215_settlement).
narrative_ontology:cs_drift_state('b8090082-53bb-492e-b733-ee6ae72cbee9', henry_iii_1225_reissue, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('b8090082-53bb-492e-b733-ee6ae72cbee9', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, english_barons_1215).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, king_john_successors_constrained).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john_person).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john_successors_constrained).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, royal_power_bounded_by_negotiated_charter).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, due_process_as_historical_remedy_not_universal_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The baronial coalition that forced King John to seal the charter. They gained specific, enumerated protections against the abuses John actually committed: arbitrary disseisin, outlawry without judgment, seizure of lands without process. They hold the charter as a negotiated contract they can enforce through their collective military power (clause 61 security clause). Their exit is credible — they can and did renounce fealty and wage war.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, english_barons_1215, beneficiary,
    powerful, generational, arbitrage, national).

% The specific monarch whose documented abuses (arbitrary fines, hostage-taking, denial of justice, dispossession without judgment) are the referent of clause 39. He bears the constraint as a personal concession extracted under duress. He cannot exit the constraint without losing his throne; his compliance is enforced by the barons' swords. The constraint extracts his prerogative to act outside documented custom — but only the specific prerogatives he actually abused.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john_person, payer,
    powerful, immediate, trapped, national).

% Subsequent English monarchs who reissued the charter (Henry III, Edward I). They benefit from the charter's legitimating function — it transforms baronial opposition into a defined legal relationship rather than perpetual civil war. But they also pay: the constraint limits their ability to govern by personal will in the specific ways John overreached. Their exit is constrained by the charter's entrenchment in coronation oaths and the political cost of repudiation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john_successors_constrained, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, king_john_successors_constrained, payer).

% The "free men" (liberi homines) referenced in the clause's text but not party to its negotiation. They include knights, merchants, and substantial tenants. The originalist reading holds they receive no direct enforceable right from clause 39 — the charter's security clause (61) empowers only the 25 barons. Their situation is unchanged by this reading: they remain subject to royal courts without the charter's specific procedural guarantees.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, free_men_non_barons_1215, excluded,
    moderate, biographical, trapped, national).

% The majority of the population, legally unfree, bound to manors. They are not "free men" in the 1215 legal sense and the originalist reading grants them nothing. Their lords' courts remain their only forum. The constraint's existence does not alter their structural position.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, villeins_unfree_1215, excluded,
    powerless, biographical, trapped, national).

% The judges and administrators who must implement the charter's procedures. They gain a defined procedural framework replacing the king's ad hoc commands — a coordination benefit. But they are also constrained: they cannot innovate procedures to serve royal convenience in ways that mirror John's abuses. Their exit is constrained by office and oath.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, thirteenth_century_royal_justices, agenda_setter,
    institutional, biographical, constrained, national).

% Legal historians and constitutional theorists who read clause 39 as a historically bounded settlement of a specific 1215 conflict. They see the clause's "law of the land" (lex terrae) and "judgment of peers" as referencing existing feudal custom, not creating new universal rights. Their analytical seat has no material stake in the constraint's operation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, modern_originalist_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a specific, enforceable procedural checkpoint against the exact royal abuses King John committed: arbitrary disseisin, outlawry without judgment, denial of access to courts, seizure of property without process. It coordinates the barons' collective enforcement (clause 61) against a defined set of prohibited royal actions, replacing private war with a legal trigger.
% TRANSFER_FUNCTION: Transfers the power to define and execute "justice" in the specific modes John abused from the king's unilateral will to a negotiated procedure requiring judgment by peers or law of the land. The king loses the specific prerogatives he misused; the barons gain a legalized enforcement right against those specific misuses.
% ABSENT_VOICES: The "free men" of England who are textually referenced but structurally excluded from the enforcement mechanism (clause 61 names only the 25 barons). The villein majority who are not "free men" at all. Both would object to a reading that limits the clause's protections to the negotiating parties, but neither was in the room at Runnymede.
% DISAPPEARANCE_RATIONALE: If clause 39 vanished in 1215, the barons lose their legalized trigger for the security clause — the charter becomes a mere promise without an enforcement condition. Civil war resumes immediately. The specific procedural protections against John's documented abuses disappear. The king regains unilateral power over disseisin, outlawry, and judicial access. The negotiated settlement collapses.
% FOUNDING_PROBLEM: King John's specific pattern of abuse: using royal courts and administrative machinery to punish political opponents, extract money from barons and merchants, deny justice to those who resisted his demands, and dispose of property rights without any process. The barons needed a legally defined trigger for their collective enforcement right (clause 61) that targeted exactly these abuses.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — King John's specific person and his documented abuses — is historically dead. No subsequent monarch is John; the specific abuses (hostage-taking of barons' children, arbitrary amercements of 1207-1215, the specific disseisins of 1214-1215) are past. This is attested by the charter's own reissue history: Henry III's 1216/1217/1225 reissues drop the security clause and modify clause 39's language, confirming the original problem was John-specific. Thirteenth-century chroniclers (Roger of Wendover, Matthew Paris) document the abuses as John's personal tyranny, not a structural feature of monarchy.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).
:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.28) because the constraint targets only John's documented abuses, not royal power generally. Suppression is low (0.15) because enforcement depends on the barons' pre-existing military power (clause 61), not new coercive machinery. Theater ratio is moderate (0.22) — the charter performs legal ceremony but the security clause is a genuine enforcement mechanism. Accessibility collapse is moderate (0.45) — alternatives (civil war, royal prerogative) remain thinkable but costly. Resistance is moderate (0.35) — John resisted violently but the constraint was imposed by superior force. The metrics describe the constraint as it operated 1215-1225: a specific settlement of a specific conflict.
 *
 * PERSPECTIVAL GAP:
 *   The originalist reading computes differently for each seat: for John, it is a snare-like extraction (high d, trapped); for the barons, it is a rope (coordination with credible exit); for successors, a tangled rope (genuine coordination + asymmetric constraint on prerogative); for free men, a mountain (the constraint simply does not apply — it is a historical fact they cannot use); for villeins, also a mountain (the constraint is irrelevant to their structural position). The engine computes this divergence from the structural data; the authored claim (rope) reflects the constraint's dominant character as a negotiated coordination mechanism among the parties who made it.
 *
 * DIRECTIONALITY LOGIC:
 *   King John is the structural target (d ≈ 0.85): the constraint extracts the specific prerogatives he abused, he is trapped (cannot exit without losing throne), and the constraint's referent is his personal conduct. The 1215 barons are structural beneficiaries (d ≈ 0.15): they gain enforceable rights against specific abuses, hold arbitrage-grade exit (can wage war), and the constraint exists because they imposed it. Successor monarchs are dual-positioned (d ≈ 0.5): they gain legitimating coordination but lose the specific prerogatives John abused. Free men and villeins are excluded — the constraint does not reach them structurally. Royal justices are agenda_setters: they administer the new procedures but are constrained by them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (John's specific abuses) is dead. The constraint's original mandate — giving barons a legal trigger against John — expired with John's death (1216) and the charter's reissue without the security clause. Yet the clause persisted in modified form. This is not mandatrophy in the extractive sense: the clause was not repurposed to extract from new victims. Rather, its language was abstracted by later actors (the liberal due process reading) into a universal principle the original text did not contain. The originalist reading avoids mandatrophy by refusing the abstraction — it reads the clause as historically exhausted, its work done.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_foreclosure_scope,
    'Does the originalist reading''s core premise (clause 39 addresses only John''s documented 1215 abuses) logically foreclose the liberal due process reading within a single interpretive framework, or can a scholar hold both as valid at different levels of analysis?',
    'Examine whether any coherent interpretive framework can simultaneously maintain: (a) the clause''s referent is exclusively John''s 1215 abuses, and (b) the clause establishes a universal principle applicable to state power generally. If the framework must choose, the readings foreclose; if it can layer them (original meaning vs. evolved application), they coexist.',
    'If forecloses, the kernel contains a genuine logical contradiction between readings — the originalist and liberal readings cannot both be true in the same framework. If coexists_with, the kernel sustains multiple stable interpretive positions simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_foreclosure_scope, conceptual, 'Whether originalist and liberal readings of clause 39 are logically incompatible or can layer').

omega_variable(
    lex_terrae_reference_fixedness,
    'Does "lex terrae" (law of the land) in clause 39 refer to a fixed 1215 feudal customary corpus, or does its reference necessarily evolve with the legal system it inhabits?',
    'Comparative analysis of 1215-1225 usage in royal courts vs. later statutory and common law invocations. If 13th-century judges treat lex terrae as a fixed customary baseline, the originalist reading''s referent is stable. If they treat it as a living reference to current law, the liberal reading''s evolving referent has textual foothold.',
    'If lex terrae is fixed, the originalist reading''s bounded referent is textually grounded. If lex terrae is inherently evolving, the originalist reading imposes a static reading on a dynamic textual hook — the liberal reading''s expansion is structurally invited by the text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lex_terrae_reference_fixedness, empirical, 'Whether the clause''s key textual hook (lex terrae) is fixed or evolving in its original operation').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading (originalist_limitation_reading) of kernel magna_carta_clause_39. What would the sibling readings (liberal_due_process_reading, feudal_prerogative_reading) change structurally?',
    'Compare the three readings'' beneficiary/victim structures, ε values, and claimed types. The liberal reading expands victims to all subjects and beneficiaries to all citizens (ε higher, claimed type likely tangled_rope). The feudal reading narrows beneficiaries to barons only, victims to king only, claimed type rope with lower ε. The structural delta maps the kernel''s contestation space.',
    'Documents the kernel''s internal structural diversity — the same textual clause instantiates materially different constraints depending on which reading''s commitments are active.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Commiter frame: structural differences between sibling readings of the same kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_clause_39_originalist_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.4).
narrative_ontology:measurement(magna_carta_clause_39_originalist_tr_t1216, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1216, 0.35).
narrative_ontology:measurement(magna_carta_clause_39_originalist_tr_t1217, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1217, 0.25).
narrative_ontology:measurement(magna_carta_clause_39_originalist_tr_t1225, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1225, 0.15).

% Extraction over time
narrative_ontology:measurement(magna_carta_clause_39_originalist_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.35).
narrative_ontology:measurement(magna_carta_clause_39_originalist_be_t1216, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1216, 0.3).
narrative_ontology:measurement(magna_carta_clause_39_originalist_be_t1217, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1217, 0.25).
narrative_ontology:measurement(magna_carta_clause_39_originalist_be_t1225, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1225, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_clause_39_originalist_su_t1215, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1215, 0.25).
narrative_ontology:measurement(magna_carta_clause_39_originalist_su_t1216, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1216, 0.2).
narrative_ontology:measurement(magna_carta_clause_39_originalist_su_t1217, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1217, 0.15).
narrative_ontology:measurement(magna_carta_clause_39_originalist_su_t1225, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1225, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single textual clause 39 into three structurally distinct constraints. The originalist reading (this story) has ε=0.28, victims={king_john_person}, beneficiaries={english_barons_1215, king_john_successors_constrained}, claimed_type=rope. The liberal_due_process_reading has ε≈0.65, victims={all_subjects_under_state_power}, beneficiaries={all_citizens}, claimed_type=tangled_rope (universal coordination + asymmetric extraction from state). The feudal_prerogative_reading has ε≈0.15, victims={king_john_person}, beneficiaries={english_barons_1215_only}, claimed_type=rope (narrower coordination). The ε values differ because the referent of extraction changes: originalist targets John's specific abuses; liberal targets arbitrary state power generally; feudal targets royal prerogative infringing baronial custom.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__originalist_limitation_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
