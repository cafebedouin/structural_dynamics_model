% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215 as Barons' Feudal Contract
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta 1215 read as a feudal contract: King John and the barons are
 *   the sole contracting parties. 'Free men' (liber homo) means landholding
 *   barons — the charter's protections (Clauses 39-40) apply only to them.
 *   Commoners, women, serfs, Jews, and Scots/Welsh are excluded from the
 *   protection set. The church is a beneficiary via Clause 1 but not a
 *   contracting party in the same sense. The constraint is the barons'
 *   collective extraction of procedural limits on royal power, enforced by
 *   Clause 61's security clause. This reading treats the 1215 text as a
 *   historically bounded peace treaty, annulled within months, whose later
 *   constitutional status is a retrospective projection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.45).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.3).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 as Barons' Feudal Contract").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '0a4de7e8-0e87-4ebd-828c-a3afe8323667').
narrative_ontology:cs_kernel_codification('0a4de7e8-0e87-4ebd-828c-a3afe8323667', fixed_text).
narrative_ontology:cs_authority_grounding('0a4de7e8-0e87-4ebd-828c-a3afe8323667', lineage).
narrative_ontology:cs_interpretation_layer_present('0a4de7e8-0e87-4ebd-828c-a3afe8323667').
narrative_ontology:cs_reading_relation('0a4de7e8-0e87-4ebd-828c-a3afe8323667', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a4de7e8-0e87-4ebd-828c-a3afe8323667', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('0a4de7e8-0e87-4ebd-828c-a3afe8323667', foundational, free_men_equals_landowning_barons).
narrative_ontology:cs_axiom_status(free_men_equals_landowning_barons, holdable).
narrative_ontology:cs_axiom_grounding('0a4de7e8-0e87-4ebd-828c-a3afe8323667', free_men_equals_landowning_barons, conventional).
narrative_ontology:cs_axiom('0a4de7e8-0e87-4ebd-828c-a3afe8323667', foundational, protection_limited_to_contracting_parties).
narrative_ontology:cs_axiom_status(protection_limited_to_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('0a4de7e8-0e87-4ebd-828c-a3afe8323667', protection_limited_to_contracting_parties, conventional).
narrative_ontology:cs_reference_frame('0a4de7e8-0e87-4ebd-828c-a3afe8323667', feudal_charter_1215).
narrative_ontology:cs_drift_state('0a4de7e8-0e87-4ebd-828c-a3afe8323667', contemporary_constitutional_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a4de7e8-0e87-4ebd-828c-a3afe8323667', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, church).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, crown).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, feudal_contractualism).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, baronial_peer_judgment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% King John as feudal overlord who issued the charter under duress. The constraint binds his arbitrary power over the barons — he can no longer seize their persons or property without lawful judgment by their peers. He bears the cost of lost prerogative and revenue. His exit is constrained: he cannot simply ignore the charter without risking baronial rebellion and loss of legitimacy, but he appeals to the Pope for annulment.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, crown, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, crown, payer).

% Landowning nobility who forced the charter at Runnymede. They gain legal protection against arbitrary royal seizure (Clause 39/40) and confirmation of feudal rights. They also become the enforcers — the charter creates a committee of 25 barons (Clause 61) authorized to seize royal castles and lands if the king violates it. Their exit is mobile: they can withdraw allegiance or rebel, which they did.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, barons, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, barons, agenda_setter).

% The English church gains explicit freedom of elections and liberties (Clause 1). Archbishop Langton was a key broker. The church benefits from royal non-interference but also holds spiritual authority over the king. Its exit is arbitrage-grade: it can appeal to Rome, excommunicate, or interdict — operating above the feudal contract.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, church, beneficiary,
    institutional, generational, arbitrage, national).

% The vast majority of the population — villeins, serfs, laborers, urban poor. In this reading, 'free men' does not include them. They gain no protection from Clause 39. They remain subject to manorial courts and arbitrary seigneurial power. Their exit is trapped: bound to the land, no legal personality in the charter, no voice in its making.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners, excluded,
    powerless, immediate, trapped, local).

% Women of all classes — noblewomen included — are not 'free men' in the 1215 feudal sense. Noblewomen have property rights through dower and inheritance but act through male guardians. The charter's protections (e.g., Clause 7-8 on dower) are procedural protections for male heirs, not substantive rights for women. Exit is trapped within patriarchal feudal structure.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women, excluded,
    powerless, immediate, trapped, local).

% Urban merchants and traders. Clause 41 grants them safe entry and exit, but this is a concession to secure London's support, not a recognition of rights. In the baronial reading, they are not contracting parties. Their exit is constrained: they depend on royal charters for market privileges but have no standing in the baronial enforcement mechanism.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, merchants, excluded,
    moderate, biographical, constrained, national).

% Scholars who read the 1215 text in its feudal context — examining the charter as a peace treaty between king and baronage, not as a constitutional document. They analyze the specific clauses, the historical circumstances of Runnymede, and the immediate annulment by Innocent III. Their seat is analytical: they neither collect nor pay, but their reading shapes how the constraint is understood.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the barons' collective action against royal arbitrariness: creates a shared legal framework (lawful judgment by peers) that replaces private force with a collective enforcement mechanism (the 25 barons of Clause 61). Solves the barons' collective action problem — individually vulnerable, jointly they can constrain the king.
% TRANSFER_FUNCTION: Transfers the power of arbitrary seizure and disposition from the crown to a baronial peer-judgment process. The crown loses revenue from feudal incidents (wardships, marriages, reliefs) when exercised without baronial consent. The barons gain procedural protection and a share of governance via the enforcement clause.
% ABSENT_VOICES: Commoners, villeins, serfs, women, Jews (targeted by Clauses 10-11 on debt), and the Welsh/Scots (Clause 56-58) — all are acted upon by the charter but not parties to it. They would object to their exclusion and to clauses that reinforce their subjection, but they were structurally absent from Runnymede.
% DISAPPEARANCE_RATIONALE: If the 1215 charter vanished overnight, the barons lose their legal shield against the king; the king regains unchecked feudal prerogative; the church loses its written guarantee of electoral freedom; the enforcement mechanism (Clause 61) dissolves. The feudal power balance reverts to naked force — the First Barons' War was the world rearranging around this constraint.
% FOUNDING_PROBLEM: King John's arbitrary exploitation of feudal incidents — excessive reliefs, wardship abuse, forced marriages, seizure of baronial lands without judgment — created a crisis of baronial tenure. The barons needed a binding commitment that the king would follow feudal custom and submit disputes to their peers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — John's specific abuses of feudal custom — died with John (1216). The reissues (1216, 1217, 1225) dropped Clause 61 (enforcement) and adapted the charter to new politics. Chroniclers (Wendover, Paris) and the charter's own reissue history corroborate: the 1215 text was a failed peace treaty, not a permanent settlement. No contemporary source treats it as a constitution for all Englishmen.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).
:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.45: the crown loses significant but not total feudal prerogative — arbitrary seizure is constrained, but the feudal relationship remains. Suppression 0.30: the charter was negotiated, not imposed by suppressing alternatives; the king agreed under threat of force. Theater 0.20 at inception: genuine coordination among barons with real enforcement teeth (Clause 61). Accessibility_collapse 0.60: for barons, the alternative of royal arbitrariness collapses — they now have a legal process. Resistance 0.70: high — the king immediately sought papal annulment, leading to civil war; the Pope declared it 'shameful, demeaning, illegal, and unjust.'
 *
 * PERSPECTIVAL GAP:
 *   From the crown's seat, this is a snare — coerced extraction of royal power under threat of deposition. From the barons' seat, it's a rope — genuine coordination solving their collective vulnerability. From the commoners' seat (excluded), it's irrelevant — their world doesn't rearrange either way. The engine computes this divergence from the structural data: same constraint, different types per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown: agenda_setter who becomes payer — the constraint extracts from the crown's prerogative (d near 1.0 for this constraint). Barons: beneficiaries who also set the enforcement agenda — they gain protection and enforcement power (d near 0.0). Church: beneficiary with independent spiritual authority — gains Clause 1 protection but operates outside the feudal contract (d near 0.1). Commoners/women: excluded — not in the protection set, not constrained by it, simply invisible to it (d undefined, but structurally trapped). Merchants: excluded — Clause 41 is a concession, not a right; they have no standing in the baronial committee.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1215 charter's founding problem (John's abuses) died in 1216. The constraint persisted only because it was reissued by Henry III's regents as a political tool — stripped of its enforcement clause (Clause 61) and adapted. The original mandate atrophied; what survived was a textual artifact that later readings (universal_rights, living_document) would repurpose. This reading declares mandatrophy resolved: the feudal contract served its purpose and expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_baronial_reading,
    'This constraint is one reading (baronial_privilege_reading) of the contested kernel magna_carta_1215. What structural elements distinguish it from the universal_rights_reading and living_document_reading?',
    'Compare the three readings'' beneficiary/victim sets, claimed types, and cs_structure axioms. The kernel admits multiple coherent readings; this reading''s ε is indexed to its own structural frame.',
    'Confirms this is a single ε-invariant constraint (per DP-001) rather than a hedged compromise. The sibling readings are separate constraint stories with their own ε, beneficiaries, and classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_baronial_reading, conceptual, 'Commitment kernel structure: this reading instantiates one specific constraint from the magna_carta_1215 kernel.').

omega_variable(
    free_men_semantic_scope_1215,
    'Did ''liber homo'' in 1215 feudal usage denote only landholding barons, or did it extend to free tenants and urban burgesses?',
    'Comparative analysis of contemporary charters, legal treatises (Glanvill, Bracton), and the charter''s own internal usage (Clauses 20, 39, 54 distinguish ''liber homo'' from ''villanus'' and ''mercator'').',
    'If ''free men'' included free tenants/burgesses, the beneficiary set widens and the constraint shifts toward rope; if strictly baronial, tangled_rope holds with narrow coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_semantic_scope_1215, empirical, 'Historical-linguistic ambiguity in the constraint''s core referent.').

omega_variable(
    clause_61_enforcement_reality,
    'Was Clause 61 (the 25 barons'' enforcement mechanism) a genuine coordination device or a baronial power grab that made the charter unacceptable to any king?',
    'Assess whether the clause was meant to operate or to fail — compare with contemporary surety clauses in feudal treaties; examine baronial conduct after Runnymede.',
    'If genuine coordination, the constraint is a working rope/tangled_rope at inception. If designed to fail, the charter is performative theater from day one — higher theater_ratio, lower coordination_function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clause_61_enforcement_reality, conceptual, 'Whether the enforcement mechanism was functional or theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magna_carta_1215_baronial_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.2).
narrative_ontology:measurement(magna_carta_1215_baronial_tr_t1216, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1216, 0.4).
narrative_ontology:measurement(magna_carta_1215_baronial_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.55).
narrative_ontology:measurement(magna_carta_1215_baronial_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.65).

% Extraction over time
narrative_ontology:measurement(magna_carta_1215_baronial_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.45).
narrative_ontology:measurement(magna_carta_1215_baronial_be_t1216, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1216, 0.35).
narrative_ontology:measurement(magna_carta_1215_baronial_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.3).
narrative_ontology:measurement(magna_carta_1215_baronial_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(magna_carta_1215_baronial_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.3).
narrative_ontology:measurement(magna_carta_1215_baronial_su_t1216, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1216, 0.45).
narrative_ontology:measurement(magna_carta_1215_baronial_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.55).
narrative_ontology:measurement(magna_carta_1215_baronial_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.1).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% Part of the Magna Carta 1215 constraint family. This reading (baronial_privilege) treats the charter as a feudal contract with narrow protection set. universal_rights_reading treats Clause 39 as emitting universal due process. living_document_reading treats the text as a substrate for interpretive accumulation. The ε values diverge: this reading has moderate extractiveness (crown constrained, barons benefit); universal_rights_reading has low extractiveness (universal protection, no concentrated extraction); living_document_reading has variable extractiveness depending on era. The baronial reading is the historical anchor; the siblings are later interpretive projections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__baronial_privilege_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
