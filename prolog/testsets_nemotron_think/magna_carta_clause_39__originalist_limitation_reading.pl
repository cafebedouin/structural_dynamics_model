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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 — Originalist Limitation Reading
 *   domain: constitutional_law/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the originalist_limitation_reading of
 *   the magna_carta_clause_39 kernel. The reading holds that Clause 39 ('No
 *   free man shall be seized... except by the lawful judgment of his peers or
 *   by the law of the land') is a specific historical settlement addressing
 *   King John's documented abuses against the barons who forced the charter.
 *   It is not a universal rights guarantee. The constraint's beneficiaries
 *   are the 1215 negotiating parties (barons and free men); its victim is the
 *   Crown's prerogative to exercise the specific arbitrary powers John used.
 *   The constraint requires active enforcement (Clause 61's security
 *   mechanism) and was a genuine coordination solution to a civil war — a
 *   rope. But it extracts from royal prerogative, making it tangibly
 *   extractive at origin. Over eight centuries, the specific extraction
 *   atrophies (the founding problem dies) while the text persists and is
 *   reinterpreted — theater_ratio rises as the originalist frame becomes
 *   performative relative to expansive readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.35).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.25).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 — Originalist Limitation Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional_law/legal_history").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '74187d7f-175f-4bd4-ae28-4e09597f1611').
narrative_ontology:cs_kernel_codification('74187d7f-175f-4bd4-ae28-4e09597f1611', fixed_text).
narrative_ontology:cs_authority_grounding('74187d7f-175f-4bd4-ae28-4e09597f1611', lineage).
narrative_ontology:cs_interpretation_layer_present('74187d7f-175f-4bd4-ae28-4e09597f1611').
narrative_ontology:cs_reading_relation('74187d7f-175f-4bd4-ae28-4e09597f1611', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('74187d7f-175f-4bd4-ae28-4e09597f1611', magna_carta_clause_39__liberal_due_process_reading, influences).
narrative_ontology:cs_axiom('74187d7f-175f-4bd4-ae28-4e09597f1611', foundational, clause_39_bounded_by_1215_grievances).
narrative_ontology:cs_axiom_status(clause_39_bounded_by_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('74187d7f-175f-4bd4-ae28-4e09597f1611', clause_39_bounded_by_1215_grievances, conventional).
narrative_ontology:cs_axiom('74187d7f-175f-4bd4-ae28-4e09597f1611', secondary, free_man_means_1215_legal_category).
narrative_ontology:cs_axiom_status(free_man_means_1215_legal_category, holdable).
narrative_ontology:cs_axiom_grounding('74187d7f-175f-4bd4-ae28-4e09597f1611', free_man_means_1215_legal_category, conventional).
narrative_ontology:cs_reference_frame('74187d7f-175f-4bd4-ae28-4e09597f1611', charter_peace_1215).
narrative_ontology:cs_drift_state('74187d7f-175f-4bd4-ae28-4e09597f1611', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('74187d7f-175f-4bd4-ae28-4e09597f1611', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, barons_1215).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, free_men_1215).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, crown_prerogative).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, church_1215).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, charter_peace_settlement).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, customary_law_limitation_on_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated Clause 39 as protection against King John's documented abuses: arbitrary disseisin, imprisonment, exile, and denial of justice. Their exit from the constraint is constrained — they are bound by feudal oath and the charter's own enforcement mechanism (Clause 61's 25 barons). They gain specific procedural protections against the specific abuses they suffered.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, barons_1215, beneficiary,
    powerful, biographical, constrained, national).

% The royal executive capacity to act without baronial consent — specifically the powers King John exercised: dispossession without judgment, imprisonment without trial, exile without cause, and sale/denial of justice. The Crown pays by surrendering these specific prerogatives. As agenda_setter, the Crown (through reissues and confirmations) administers the charter's continuation but is structurally the target of its extraction.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, crown_prerogative, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, crown_prerogative, agenda_setter).

% The clause text extends protections to 'no free man' — a category broader than barons but excluding serfs, villeins, and unfree persons. They gain the same procedural protections against the documented abuses. Their exit is constrained by feudal status and the charter's enforcement structure. They did not negotiate directly but benefit from the barons' settlement.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, free_men_1215, beneficiary,
    moderate, biographical, constrained, national).

% Protected by Clause 1 (church liberties) and indirectly by Clause 39's limitation on royal interference. The Church as an institution has mobile exit — it operates across Christendom and possesses independent canonical jurisdiction. It benefits from the charter's general limitation on arbitrary royal power but is not the primary target of the documented abuses.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, church_1215, beneficiary,
    institutional, generational, mobile, national).

% Serfs, villeins, and unfree persons — the majority of the 1215 population — are not 'free men' under the charter's terms. They have no procedural protections under Clause 39, remain subject to manorial justice, and have no exit from their condition. They would object to the constraint's limited scope if they could speak in the negotiation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, excluded_unfree, excluded,
    powerless, immediate, trapped, local).

% The interpretive tradition that reads Clause 39 across centuries. It sees the originalist reading as one historical frame among others — noting how the clause's language ('law of the land', 'judgment of peers') was expandable beyond 1215 grievances. It neither collects nor pays; it tracks the constraint's interpretive trajectory.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, common_law_tradition, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ended the 1215 baronial rebellion by converting the King's arbitrary exercise of specific powers (disseisin, imprisonment, exile, denial of justice) into procedurally bounded actions requiring lawful judgment — a peace settlement between Crown and baronage.
% TRANSFER_FUNCTION: Transfers the capacity for arbitrary executive action against the negotiating parties (barons and free men) from the Crown to a procedural requirement: no proceeding against person or property except by lawful judgment of peers or the law of the land.
% ABSENT_VOICES: The unfree population (serfs, villeins) — approximately 70-80% of 1215 England — are structurally excluded from 'free man' status. They would object to a settlement that locks in their exclusion. Jewish communities, foreign merchants, and women (whose legal personhood was mediated through male relatives) are likewise absent from the charter's protections.
% DISAPPEARANCE_RATIONALE: If the specific limitations on documented 1215 abuses vanished overnight, the Crown would regain the prerogative to disseise, imprison, exile, and deny justice to the baronage without procedural constraint — the 1215 peace settlement would collapse, likely triggering renewed baronial resistance or civil war.
% FOUNDING_PROBLEM: King John's pattern of arbitrary dispossession (disseisin), imprisonment without trial, exile without cause, and selling/denying justice — exercised against the barons who forced the charter — created a crisis of feudal reciprocity that threatened the Crown's legitimacy and provoked armed rebellion.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the charter's own preamble (naming the specific grievances), contemporary chronicles (Roger of Wendover, Matthew Paris), and the 1216/1217/1225 reissues which drop or modify enforcement clauses once the immediate crisis passes. No party outside the original beneficiary set (the Crown, the Church, later common lawyers) treats King John's specific abuses as a live problem — the status is corroborated by the historical record, not by the constraint's beneficiaries.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness starts moderate (0.40) because the constraint removes specific, valuable prerogatives from the Crown (arbitrary disseisin, imprisonment, exile, justice denial). It declines to near-zero as those specific abuses become historically obsolete and the Crown reasserts power through other channels. Suppression starts high (0.60) because the 1215 enforcement mechanism (25 barons with distraint power) is coercive; it collapses as the charter becomes statute (1297) and then symbolic. Theater_ratio rises from 0.10 to 0.45 because the originalist reading's claim — 'this limits only 1215 abuses' — becomes increasingly performative as the legal world adopts the liberal_due_process_reading; the constraint's operational function shifts from the original coordination to a symbolic anchor for later expansions.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's seat (agenda_setter/payer), the constraint is a coerced concession extracted by rebellion — extraction is experienced as loss of sovereign prerogative. From the barons' seat (beneficiary), it is a hard-won coordination solution — extraction is experienced as security against arbitrary power. From the common_law_tradition seat (observer), the constraint is a textual artifact whose meaning migrates — the originalist reading is one stable frame among others. The engine computes these divergences from the structural data; the claimed_type 'rope' reflects the barons' experience (genuine coordination), while the metrics capture the Crown's experience (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown (crown_prerogative) is the structural target (d ~0.75) — it loses specific executive capacities it previously exercised. The barons_1215 are structural beneficiaries (d ~0.15) — they gain procedural protection against the specific abuses they suffered. Free_men_1215 are moderate beneficiaries (d ~0.30) — they gain protections but did not negotiate directly. The Church is a mobile beneficiary (d ~0.20) with independent institutional exit. The excluded_unfree are trapped non-participants — the constraint does not govern them at all; their exclusion is a structural fact of the 1215 settlement. The analytical observer sits at d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (King John's specific abuses) is dead — the king is dead, the abuses are historical, the rebellion ended. The constraint persists because its text became a foundation for later rights claims (liberal_due_process_reading), not because the original coordination function survives. The originalist reading itself is a mandatrophic artifact: it maintains the original frame as a bulwark against expansion, but the frame's operational work is done. The theater_ratio rise documents this: the originalist limitation performs the 1215 settlement while the constraint's actual operation has migrated to the liberal reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'This constraint is one reading of the magna_carta_clause_39 kernel. What does the originalist_limitation_reading foreclose or influence in the sibling readings?',
    'Structural comparison of the three readings'' beneficiary/victim sets, extraction profiles, and authority_grounding. The originalist reading''s core premise — that Clause 39''s protections are exhausted by the 1215 grievance list — directly contradicts the liberal reading''s universalist premise. The feudal_prerogative reading shares the hierarchical frame but differs on the scope of procedural rights.',
    'If the originalist reading forecloses the liberal reading within a single legal framework, then adopting originalism structurally blocks due-process expansion. If they coexist, the kernel sustains a persistent interpretive contest. The engine computes foreclosure from cs_structure.axioms + drift_state; this omega documents the committer-frame ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Committee-frame structural relationship between this reading and its siblings in the magna_carta_clause_39 kernel').

omega_variable(
    free_man_scope_1215,
    'Does ''free man'' in 1215 Clause 39 refer only to the baronage and their immediate tenants, or does it extend to all legally free persons (including merchants, clerks, villeins who bought freedom)?',
    'Paleographic and legal-historical analysis of 1215 usage: Bracton, Glanvill, and contemporary court rolls. Compare with Clause 20 (amerciaments of free men) and Clause 39''s parallel structure.',
    'If ''free man'' = baronage only, the beneficiary set narrows to ~200 barons; extraction from Crown is concentrated. If ''free man'' = all legally free (~30-40% of male population), the coordination function broadens and extraction diffuses. This changes the constraint''s type at origin: narrow = rope for elite pact; broad = tangled_rope with wider coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(free_man_scope_1215, empirical, 'Historical scope of ''free man'' in 1215 Clause 39 — determines beneficiary set breadth').

omega_variable(
    clause_61_enforcement_atrophy,
    'How quickly did Clause 61''s enforcement mechanism (25 barons'' distraint power) become inoperative, and does its atrophy date the constraint''s transition from active coordination to symbolic artifact?',
    'Track the 1216/1217/1225 reissues: Clause 61 is omitted in all. The 1225 charter (enrolled as statute 1297) has no enforcement clause. The constraint''s active suppression requirement drops from 0.60 to 0.20 by 1297 (time_point 82).',
    'If enforcement atrophies within 10 years, the constraint''s active coordination phase is brief; the subsequent 800 years are theater/piton dynamics. The originalist reading''s claim that the constraint ''limits only 1215 abuses'' becomes a retrospective framing of a dead letter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clause_61_enforcement_atrophy, empirical, 'Timeline of Clause 61 enforcement atrophy and its effect on the constraint''s active life').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 810).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(magn_tr_t10, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(magn_tr_t82, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 82, 0.1).
narrative_ontology:measurement(magn_tr_t413, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 413, 0.25).
narrative_ontology:measurement(magn_tr_t576, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 576, 0.35).
narrative_ontology:measurement(magn_tr_t733, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 733, 0.4).
narrative_ontology:measurement(magn_tr_t810, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 810, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(magn_be_t10, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(magn_be_t82, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 82, 0.25).
narrative_ontology:measurement(magn_be_t413, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 413, 0.15).
narrative_ontology:measurement(magn_be_t576, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 576, 0.1).
narrative_ontology:measurement(magn_be_t733, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 733, 0.08).
narrative_ontology:measurement(magn_be_t810, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 810, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(magn_su_t10, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(magn_su_t82, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 82, 0.2).
narrative_ontology:measurement(magn_su_t413, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 413, 0.1).
narrative_ontology:measurement(magn_su_t576, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 576, 0.05).
narrative_ontology:measurement(magn_su_t733, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 733, 0.03).
narrative_ontology:measurement(magn_su_t810, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 810, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__originalist_limitation_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the Magna Carta Clause 39 kernel into three structurally distinct readings. The originalist_limitation_reading (this story) has ε=0.35 at origin, decaying to ~0.05, with beneficiaries limited to 1215 negotiating parties. The liberal_due_process_reading has higher sustained ε (extracting from state power broadly) and universal beneficiaries. The feudal_prerogative_reading has lower ε and narrower coordination within hierarchy. They are linked by affects_constraints because the originalist reading's authority_grounding (lineage) is cited by both siblings — the liberal reading claims lineage through textual expansion; the feudal reading claims lineage through hierarchical continuity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__originalist_limitation_reading, institutional, 0.75).
constraint_indexing:directionality_override(magna_carta_clause_39__originalist_limitation_reading, powerful, 0.15).
constraint_indexing:directionality_override(magna_carta_clause_39__originalist_limitation_reading, moderate, 0.3).
constraint_indexing:directionality_override(magna_carta_clause_39__originalist_limitation_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
