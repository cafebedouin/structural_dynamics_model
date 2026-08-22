% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Clause 39 Originalist Limitation Reading: Procedural Constraints on Documented 1215 Royal Abuses
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   Magna Carta Clause 39 (the originalist limitation reading) is read as a
 *   constraint on specific, documented royal abuses that King John practiced
 *   against the baronial class in feudal contexts: wardship seizure, relief
 *   extortion, arbitrary exile and disinheritance. Under this reading, the
 *   clause does not establish a universal principle of due process or
 *   individual liberty; it codifies feudal procedure against known abuses
 *   affecting the negotiating parties. The constraint is CLAIMED as rope
 *   (genuine coordination of the feudal relationship, protecting the bargain)
 *   and the metrics reflect moderate extraction only against the enumerated
 *   grievances, not across all royal prerogative. The reading is bounded by
 *   the 1215 documentary context.
 *
 * KEY AGENTS:
 *   - Baronial class (powerful, negotiating at Runnymede; beneficiaries of procedure against documented abuses)
 *   - King John institution (Crown; subject to procedural constraint on enumerated incidents, otherwise untouched)
 *   - Ecclesiastical magnates (powerful, co-beneficiaries; Church liberty and property protection)
 *   - Unfree peasantry (powerless, excluded from the scope by role and wealth class)
 *   - Royal administration (justiciar system; implements procedure on enumerated cases)
 *   - Legal tradition keepers (judges, historians; interpret what the clause says and bounds)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.38).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.25).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Clause 39 Originalist Limitation Reading: Procedural Constraints on Documented 1215 Royal Abuses").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '052d91cb-11b2-4283-bf3e-a5bb450bbc83').
narrative_ontology:cs_kernel_codification('052d91cb-11b2-4283-bf3e-a5bb450bbc83', fixed_text).
narrative_ontology:cs_authority_grounding('052d91cb-11b2-4283-bf3e-a5bb450bbc83', lineage).
narrative_ontology:cs_interpretation_layer_present('052d91cb-11b2-4283-bf3e-a5bb450bbc83').
narrative_ontology:cs_reading_relation('052d91cb-11b2-4283-bf3e-a5bb450bbc83', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_reading_relation('052d91cb-11b2-4283-bf3e-a5bb450bbc83', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_axiom('052d91cb-11b2-4283-bf3e-a5bb450bbc83', foundational, clause_39_context_bounded_1215_grievances).
narrative_ontology:cs_axiom_status(clause_39_context_bounded_1215_grievances, holdable).
narrative_ontology:cs_axiom_grounding('052d91cb-11b2-4283-bf3e-a5bb450bbc83', clause_39_context_bounded_1215_grievances, empirically_contingent).
narrative_ontology:cs_axiom('052d91cb-11b2-4283-bf3e-a5bb450bbc83', foundational, feudal_incidents_enumerable_and_procedurable).
narrative_ontology:cs_axiom_status(feudal_incidents_enumerable_and_procedurable, holdable).
narrative_ontology:cs_axiom_grounding('052d91cb-11b2-4283-bf3e-a5bb450bbc83', feudal_incidents_enumerable_and_procedurable, conventional).
narrative_ontology:cs_axiom('052d91cb-11b2-4283-bf3e-a5bb450bbc83', secondary, baronial_class_primary_beneficiary).
narrative_ontology:cs_axiom_status(baronial_class_primary_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('052d91cb-11b2-4283-bf3e-a5bb450bbc83', baronial_class_primary_beneficiary, empirically_contingent).
narrative_ontology:cs_reference_frame('052d91cb-11b2-4283-bf3e-a5bb450bbc83', feudal_incident_procedure).
narrative_ontology:cs_drift_state('052d91cb-11b2-4283-bf3e-a5bb450bbc83', early_thirteenth_century_reinterpretation, gap(stable, minor, false)).
narrative_ontology:cs_created_at('052d91cb-11b2-4283-bf3e-a5bb450bbc83', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, ecclesiastical_magnates).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, king_john_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The negotiating parties to Magna Carta: landholding nobility and ecclesiastical magnates whose feudal holdings and jurisdictional privileges were subject to arbitrary royal action (wardship seizure, relief demands, escheat abuse). Clause 39 protects their specific documented grievances—wardship procedure, inheritance taxation, arbitrary exile—against the documented practices of King John. Their exit was rebellion or flight; the charter locked in procedural constraint against those specific abuses.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_class, beneficiary,
    powerful, generational, constrained, national).

% The Crown—or rather, the Crown as exercising the specific powers enumerated in 1215 complaint: arbitrary wardship seizure, relief extortion, arbitrary exile and disinheritance of baronial tenants. The constraint binds the monarchy to procedural formality in these documented grievances. The King cannot unilaterally exit without repudiating the charter itself; the baronial class can only force compliance through renewal oaths and renewal-time renegotiation.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, king_john_institution, payer,
    institutional, generational, trapped, national).

% The villeins and unfree agricultural population are explicitly outside the charter's scope in the originalist reading. Clause 39 speaks to 'free men' and the documented baronial grievances, not to the subjection of the unfree majority. They would have no claim to its protections and no standing to invoke it; their exclusion is structural to the reading.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, unfree_peasantry, excluded,
    powerless, biographical, trapped, national).

% The Church hierarchy—archbishops and great abbots—negotiated Clause 39 partly for themselves as landholders (wardship and relief abuses struck ecclesiastical properties equally) and partly for the Church's liberty. They had parallel exit via excommunication and interdict; the constraint crystallizes feudal procedure to protect their holdings and their electoral independence.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, ecclesiastical_magnates, beneficiary,
    powerful, generational, constrained, national).

% The Crown's administrative apparatus—the Curia Regis and its subordinate justiciar—implements the procedural constraint. In the originalist reading, they are bound to follow procedure in the enumerated cases (wardship, relief, exile) but retain full discretion in all other matters. The constraint shapes their operation only on the specific documented grievances, not across the entire royal prerogative.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, royal_administration, agenda_setter,
    institutional, generational, trapped, national).

% Judges, jurists, and historians who interpret Clause 39 within the originalist frame: what did the 1215 text say about the specific abuses it documented? Their role is analytical; they hold no stake in the constraint's operation but their interpretive authority shapes how it is understood and whether it migrates beyond its 1215 referent.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, legal_tradition_keepers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__originalist_limitation_reading, baronial_class).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__originalist_limitation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes written procedural formality for the feudal sovereign's exercise of documented arbitrary powers (wardship seizure, relief extortion, disinheritance): transforms unwritten custom into enumerated procedure for those specific grievances, reducing uncertainty for the baronial class.
% TRANSFER_FUNCTION: Moves the administrative burden and legitimacy cost of procedure in wardship, relief, and exile cases from the Crown (who could act arbitrarily) to a formalized adjudicatory process that requires justicials to follow written rule. No direct wealth transfer; the constraint redistributes discretion from the executive to the procedural form.
% ABSENT_VOICES: The unfree peasantry—the majority of the English population—have no seat at the charter table and no standing to invoke Clause 39 in the originalist reading. Merchant guilds and urban populations are outside the feudal relationships the charter addresses. Any later actor claiming Clause 39 as a universal right against arbitrary power is making a reading the originalist frame excludes by construction.
% DISAPPEARANCE_RATIONALE: If Clause 39 as originally framed disappeared, the Crown would revert to the 1215 status quo: unmediated wardship seizure, arbitrary relief demands, exile without procedure. The baronial class would need to re-negotiate or re-rebel; the feudal order's internal stability mechanism would vanish. Subsequent juridical interpretation might persist, but the original constraint's force—the procedural lock on documented abuses—would be gone.
% FOUNDING_PROBLEM: King John's documented abuses of feudal incidents: using wardship to seize baronial properties and revenues beyond the wardship period, extracting excessive relief payments on inheritance, arbitrary disinheritance and exile of tenants in capite. These abuses destabilized the feudal bargain (the Crown provides justice and protection; vassals provide service and counsel) by making the Crown's arbitrary exercise of feudal prerogative unpredictable.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary 1215 documents (the charter itself, the baronial grievance list, royal writ confirmations) attest the specific abuses. Later medieval crown registers and plea rolls document repeated Crown violations of the charter—evidence that the founding problem (arbitrary incident abuse) persisted and required constant re-enforcement at each reign change. Originalist historical scholarship (Holt, Turner, Carpenter) outside the benefiting parties attests the documentary basis for the 1215 grievances.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.38) because the Crown retains full prerogative outside the enumerated grievances; the constraint bites only on documented abuses (wardship, relief, exile). Suppression is low (0.25) because compliance is reinforced by baronial power (they can refuse to attend council, withdraw counsel, or rebel) rather than coercive force; the feudal hierarchy makes enforcement mutual. Theater is very low (0.12) because the constraint performs no function beyond what it states—no cover story, no hidden extraction masquerading as coordination. The measurement series shows slight compression from 1215 (when the Crown's violation risk was highest) toward stable operation by 1250–1265 (when procedure was internalized into royal administration). The originalist frame anchors the constraint's meaning in 1215 documentary context; interpretive drift toward universal due process is a sibling reading, not this one.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's seat and the baronial seat compute differently: from the Crown's position, the constraint is a check on specific documented abuses that already destabilized the realm—genuine coordination of the feudal bargain. From the baronial seat, it is enforceable procedure that eliminates arbitrary extraction of incidents. Both framings are coherent in the originalist reading because the constraint is genuinely bilateral: the King gets a stable feudal hierarchy with predictable incident yields; the barons get procedure instead of arbitrary seizure. The engine computes this from the structural data (both beneficiary and payer present; moderate extraction, low suppression) without needing to adjudicate which seat's narrative is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The baronial class benefits from the constraint (d near beneficiary end): they negotiate it, extract procedure from the Crown, and reduce their incident vulnerability. The Crown pays for it in predictability and lost discretionary revenue (d near payer end), but gains stable feudal order and legitimate incident yields. The unfree peasantry sits far from the constraint (not named in the charter, no feudal standing to claim it) so their d is near 0 in the originalist reading—the constraint does not extract from them or coordinate for them. The ecclesiastical magnates sit with the barons (co-negotiators, co-beneficiaries). Directionality overrides are not needed; the beneficiary/victim derivation from documented 1215 context is structurally sound.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (King John's incident abuses destabilizing feudal order) remains LIVE through 1265: crown violation of the charter is documented at each regnal transition, baronial dissatisfaction drives the Second Barons' War (1264–1267), and the constraint requires constant re-negotiation. This is not mandatrophy; the founding problem persists and the constraint's enforcement machinery (baronial power, council participation, renewal oaths) is continuously invoked. The theater ratio stays low because the constraint performs the function it announces—procedure on enumerated incidents—without performative cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    documentary_boundary_scope,
    'Is Clause 39 properly read as bounded by the documented 1215 grievances (wardship, relief, exile), or does its language establish a general procedural principle that transcends its founding context?',
    'Comparative textual analysis: examine whether the charter''s other clauses show similarly context-specific language (they do — Clause 8 on relief amounts, Clause 5 on wardship valuation) or whether Clause 39''s phrasing suggests intent for broader application. Medieval and early modern juridical interpretation: how did contemporary judges and legal minds apply the clause—only to feudal incidents, or to novel grievances?',
    'If the clause is context-specific, the originalist limitation holds and extractiveness remains moderate (only against documented incidents). If the language establishes a general principle, the constraint migrates toward liberal due process readings and extractiveness increases as the victim set expands beyond barons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentary_boundary_scope, conceptual, 'Whether Clause 39 is temporally bounded or generationally portable.').

omega_variable(
    free_man_exclusion,
    'Does ''free man'' in Clause 39 refer to the feudal elite (those with independent landed status), or does it extend to all non-servile persons in the kingdom?',
    'Etymology and charter usage: examine parallel clauses and royal writs to establish what ''free man'' meant in 1215 administrative language. Social history: determine the proportion of the population who held free (non-villein) status in 1215 England. Variant charter texts and interpretations: how did different regions and later copyists gloss the term?',
    'A narrow reading (free = feudal elite) keeps the constraint''s victim set small and extractiveness moderate. A broad reading (free = all non-serf) would expand the victim set and increase extractiveness, supporting liberal due process migration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_man_exclusion, empirical, 'The demographic scope of ''free man'' in 1215 context.').

omega_variable(
    charter_renewal_extractiveness,
    'Does the Crown''s practice of issuing reissues of the charter (1216, 1217, 1225, 1265) in exchange for new tax grants constitute a new extraction mechanism layered onto the constraint, or does it represent the constraint''s normal enforcement and renewal?',
    'Fiscal history: compare royal revenue in charter reissue years to non-reissue years; examine the Crown''s explicit rationale for demanding reissue in exchange for tax; analyze charter variants across reissues to determine what was retained, modified, or dropped.',
    'If reissue-for-tax is routine procedural renewal, the constraint stays as authored (moderate extractiveness, bilateral). If it represents the Crown using the charter as leverage to extract new taxes, extractiveness rises and the constraint becomes tangled rope (coordination covering extractive leverage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_renewal_extractiveness, empirical, 'Whether charter renewals are enforcement mechanism or extraction mechanism.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the originalist temporal boundary logically foreclose the liberal due process reading, or can both coexist as interpretive traditions within the same textual kernel?',
    'Jurisprudential analysis: examine whether a legal system can simultaneously hold that Clause 39 means what originalists say it means (context-bounded procedure) AND what liberals say it means (universal principle against arbitrary power). History of interpretation: show whether medieval and early modern courts treated the clause as context-specific or generalizable.',
    'If foreclosed, the originalist and liberal readings cannot both be law in the same jurisdiction at the same time; one reading''s triumph eliminates the other. If coexistent, both readings remain live even though they disagree on scope, and the constraint''s classification may differ per seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between originalist and liberal readings of Clause 39.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 1265).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.08).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1225, 0.1).
narrative_ontology:measurement_basis(magn_tr_t1225, observed).
narrative_ontology:measurement(magn_tr_t1235, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1235, 0.11).
narrative_ontology:measurement_basis(magn_tr_t1235, observed).
narrative_ontology:measurement(magn_tr_t1250, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1250, 0.12).
narrative_ontology:measurement_basis(magn_tr_t1250, observed).
narrative_ontology:measurement(magn_tr_t1265, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1265, 0.12).
narrative_ontology:measurement_basis(magn_tr_t1265, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.45).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1225, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1225, 0.4).
narrative_ontology:measurement_basis(magn_be_t1225, observed).
narrative_ontology:measurement(magn_be_t1235, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1235, 0.38).
narrative_ontology:measurement_basis(magn_be_t1235, observed).
narrative_ontology:measurement(magn_be_t1250, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1250, 0.37).
narrative_ontology:measurement_basis(magn_be_t1250, observed).
narrative_ontology:measurement(magn_be_t1265, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1265, 0.38).
narrative_ontology:measurement_basis(magn_be_t1265, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1215, 0.3).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1225, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1225, 0.27).
narrative_ontology:measurement_basis(magn_su_t1225, observed).
narrative_ontology:measurement(magn_su_t1235, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1235, 0.25).
narrative_ontology:measurement_basis(magn_su_t1235, observed).
narrative_ontology:measurement(magn_su_t1250, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1250, 0.25).
narrative_ontology:measurement_basis(magn_su_t1250, observed).
narrative_ontology:measurement(magn_su_t1265, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1265, 0.25).
narrative_ontology:measurement_basis(magn_su_t1265, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__originalist_limitation_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 instantiates three structurally distinct constraints depending on which reading is adopted. The ORIGINALIST LIMITATION READING (this file) bounds the constraint to documented 1215 grievances and the baronial class; the LIBERAL DUE PROCESS READING extends it to universal individual liberty against arbitrary state power; the FEUDAL PREROGATIVE READING preserves it as hierarchical procedure within established feudal order. All three are readings of the same kernel text, but each instantiates a different constraint (different ε, different victim set, different extracted function). The three are linked via network.affects_constraints to enable cross-reading comparative analysis. Each reading's omegas document the interpretive choices and counterfactuals that distinguish it from siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
