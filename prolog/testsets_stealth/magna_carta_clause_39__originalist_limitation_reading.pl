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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: magna_carta_clause_39__originalist_limitation_reading
 *   human_readable: Magna Carta Clause 39 — Originalist Limitation Reading (Bounded to the 1215 Grievance Catalog)
 *   domain: constitutional law/legal history/political theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   magna_carta_clause_39: the originalist limitation reading, under which
 *   clause 39 binds the English crown only against the specific abuses
 *   documented in the 1215 negotiations — imprisonment without judgment,
 *   disseisin of lands and castles, outlawry and exile used as instruments of
 *   exaction — and only for the class that negotiated the charter. The
 *   standing arrangement under contest is the sealed clause as a limitation
 *   on the crown; epsilon is authored for that arrangement as this reading
 *   holds it, assessed by the reading's own lights: because the reading
 *   freezes the clause's content at the 1215 grievance catalog, its
 *   extractive reach is moderate (0.44 at interval end) — it takes only what
 *   the record shows John did, not prerogative at large. The arrangement is
 *   structurally hybrid: a genuine coordination function (converting the
 *   king's forbearance from promise to enforceable term, ending the 1214-15
 *   armed crisis and enabling the bargained reissues) runs through the same
 *   structure that takes enumerated prerogative from the crown and leaves the
 *   unenfranchised majority outside the protection while they remain the
 *   crown's revenue base. The sibling readings — liberal_due_process_reading
 *   (universal individual right; larger victim set, higher epsilon) and
 *   feudal_prerogative_reading (procedural incident of hierarchy; extraction
 *   near the coordination floor) — are separate constraints in separate
 *   files, linked through network.affects_constraints; the contest among them
 *   is routed to omega variables, not averaged into this file. The interval
 *   1215-1297 spans the clause's original enforcement life: imposition by
 *   arms, civil war, the reissue lineage, the Provisions crisis, and the 1297
 *   Confirmation of Charters. KEY AGENTS (by structural relationship): -
 *   rebel_barons_of_1215: primary beneficiary (organized/constrained) —
 *   negotiated and enforced the charter; collect procedural security -
 *   the_crown: primary payer (institutional/constrained) — surrenders the
 *   enumerated prerogatives; receives settlement stability as partial offset
 *   - free_tenantry_of_the_shires: secondary beneficiary
 *   (moderate/constrained) — nominally covered by 'free man'; funds the
 *   reissue bargains - unfree_peasantry: payer (powerless/trapped) — outside
 *   the protected class; bears the residual royal extraction - the_papacy:
 *   external institutional actor (arbitrage) — annulled the 1215 charter;
 *   lever against baronial enforcement - later_reformers_and_grievants:
 *   excluded voice (organized/constrained) — grievances outside the 1215
 *   catalog - constitutional_historians: analytical observer — sees all three
 *   readings and the full structure
 *
 * KEY AGENTS:
 *   - rebel_barons_of_1215: primary beneficiary (organized power, constrained exit) — negotiated the charter, staffed the twenty-five-executor enforcement committee, and collect procedural security for persons, lands, and liberties
 *   - the_crown: primary payer (institutional power, constrained exit) — surrenders the enumerated prerogatives; receives the settlement itself (stability, bargained revenue) as a partial offset
 *   - free_tenantry_of_the_shires: secondary beneficiary (moderate power, constrained exit) — nominally inside 'any free man'; their protection rides on baronial enforcement and they fund the reissue bargains through taxation
 *   - unfree_peasantry: payer (powerless, trapped) — outside the protected class; continue to bear the royal extraction the settlement leaves untouched
 *   - the_papacy: external institutional actor (arbitrage exit) — annulled the 1215 charter; its judgment is a lever the crown invokes against baronial enforcement
 *   - later_reformers_and_grievants: excluded voice (organized, constrained) — subjects of the 1230s-1297 crises whose injuries fall outside the 1215 catalog; they invoke the clause beyond this reading's scope
 *   - constitutional_historians: analytical observer — reads the drafting record, reissue texts, and enforcement episodes; sees all three readings at once
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.44).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.5).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 — Originalist Limitation Reading (Bounded to the 1215 Grievance Catalog)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional law/legal history/political theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, '6986d2c1-2e57-43ca-9a1a-4da2ba924316').
narrative_ontology:cs_kernel_codification('6986d2c1-2e57-43ca-9a1a-4da2ba924316', fixed_text).
narrative_ontology:cs_authority_grounding('6986d2c1-2e57-43ca-9a1a-4da2ba924316', lineage).
narrative_ontology:cs_interpretation_layer_present('6986d2c1-2e57-43ca-9a1a-4da2ba924316').
narrative_ontology:cs_reading_relation('6986d2c1-2e57-43ca-9a1a-4da2ba924316', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('6986d2c1-2e57-43ca-9a1a-4da2ba924316', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('6986d2c1-2e57-43ca-9a1a-4da2ba924316', foundational, clause_scope_fixed_by_1215_grievance_catalog).
narrative_ontology:cs_axiom_status(clause_scope_fixed_by_1215_grievance_catalog, holdable).
narrative_ontology:cs_axiom_grounding('6986d2c1-2e57-43ca-9a1a-4da2ba924316', clause_scope_fixed_by_1215_grievance_catalog, empirically_contingent).
narrative_ontology:cs_axiom('6986d2c1-2e57-43ca-9a1a-4da2ba924316', secondary, liber_homo_read_as_negotiating_class).
narrative_ontology:cs_axiom_status(liber_homo_read_as_negotiating_class, holdable).
narrative_ontology:cs_axiom_grounding('6986d2c1-2e57-43ca-9a1a-4da2ba924316', liber_homo_read_as_negotiating_class, empirically_contingent).
narrative_ontology:cs_reference_frame('6986d2c1-2e57-43ca-9a1a-4da2ba924316', runnymede_negotiated_limitation).
narrative_ontology:cs_drift_state('6986d2c1-2e57-43ca-9a1a-4da2ba924316', contemporary_constitutional_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6986d2c1-2e57-43ca-9a1a-4da2ba924316', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, rebel_barons_of_1215).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, free_tenantry_of_the_shires).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, unfree_peasantry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, free_tenantry_of_the_shires).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, enumerated_royal_limitations_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Armed magnates who presented the 1214-15 grievance list, negotiated at Runnymede, and staffed the twenty-five-executor committee empowered to distrain the king on default. They receive procedural security for their persons, lands, castles, and liberties, and their heirs inherit that security. Exit from the arrangement means resuming civil war or submitting to the king's discretion; they chose the charter and enforced it with arms when John repudiated it.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, rebel_barons_of_1215, beneficiary,
    organized, biographical, constrained, national).

% The sovereign authority bound by the clause. It surrenders the power to imprison, dispossess, outlaw, or exile the protected class except by lawful judgment of peers or the law of the land. In exchange it receives the settlement itself: an end to the armed crisis, continued baronial service, and a revenue relationship conducted through bargained reissues in 1216, 1217, 1225, and 1297. It cannot leave the arrangement's jurisdiction; its options are compliance, renegotiation, or — as John attempted — repudiation backed by papal annulment, which cost a two-year civil war and a French invasion.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, the_crown, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, the_crown, beneficiary).

% The villein majority of the realm, outside the words 'any free man.' They were not parties at Runnymede and receive nothing from the settlement's protections. They continue to pay tallage, scutage, wardship, and merchet to the crown and its tenants; the settlement's stabilization of crown-baron relations leaves them as the residual base from which royal revenue is drawn. Exit is unavailable: villeinage binds them to the land and their obligations are not negotiable.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, unfree_peasantry, payer,
    powerless, biographical, trapped, national).

% Knights and freeholders below baronial rank, nominally covered by 'any free man.' Their protection in practice rides on baronial enforcement capacity rather than their own leverage. They pay the taxation and scutage bargains that accompany each reissue — the 1225 charter was confirmed in exchange for a fifteenth — so they fund the settlement whose protections they hold only derivatively.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, free_tenantry_of_the_shires, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, free_tenantry_of_the_shires, payer).

% The papal court, to which John had surrendered the realm as a fief in 1213. It judged the charter a forced and unjust diminution of a vassal king, annulled it in August 1215, and suspended its operation — an external seat whose judgment the crown could invoke against baronial enforcement. It operates above the realm's jurisdiction and bears none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, the_papacy, observer,
    institutional, generational, arbitrage, continental).

% Subjects of the later reigns covered by this interval — the reform movements of the 1230s-1250s, the Provisions coalition of 1258, the clerical and baronial opponents of Edward I in 1297 — whose grievances (misgovernment, foreign favorites, taxation without consent) do not match the 1215 catalog. They invoke the charter's language for their own crises; under this reading's scope their injuries fall outside the clause's content. They were never parties to the Runnymede settlement and cannot become ones.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, later_reformers_and_grievants, excluded,
    organized, biographical, constrained, national).

% The analytical seat: legal historians and constitutional scholars who read the drafting record, the reissue texts, and the enforcement episodes, and can see all three readings of the clause simultaneously. They collect nothing and pay nothing under the arrangement.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__originalist_limitation_reading, rebel_barons_of_1215).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__originalist_limitation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Made the king's forbearance credible: after a decade in which John had imprisoned magnates without judgment, seized their lands and castles, and used outlawry and exile as instruments of exaction, the clause converts baronial security from dependence on the king's word into a sealed, enforceable term — enabling the bargained taxation and service relationship the reissue record shows.
% TRANSFER_FUNCTION: Moves enumerated prerogative power from the crown to the protected class: the king's discretion over the persons, lands, and liberties of free men is transferred to a procedure (lawful judgment of peers or the law of the land) that the baronial class staffs and enforces. The attached taxation and scutage bargains move money from the shires and the realm to the crown in exchange for each confirmation.
% ABSENT_VOICES: The unfree majority — roughly nine-tenths of the population — were not at Runnymede and fall outside 'free man'; they would object that the settlement prices their protection at zero while leaving royal extraction from them intact. Later grievance-holders of the 1230s-1297 crises would object that the clause's scope freezes their remedies at Runnymede's moment. Both objections are structurally excluded: the originalist scope admits no parties who were not parties to the 1215 settlement.
% DISAPPEARANCE_RATIONALE: The charter was the settlement document of an active armed conflict. Overnight disappearance returns the parties to war: John repudiated the 1215 text within ten weeks and the realm went to war; the 1216-17 campaign, the French invasion, and every subsequent reissue show that baronial security, the taxation bargains, and the succession's stability depended on the charter's continued operation.
% FOUNDING_PROBLEM: King John's documented abuses of the preceding decade: imprisonment of magnates without judgment (including hostages held under threat of starvation), disseisin of baronial lands and castles, outlawry and exile used as instruments of exaction, and exploitation of feudal incidents — reliefs, wardships, widows' remarriage — the specific grievance list the rebel barons compiled in 1214-15.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the papal annulment of August 1215 attests the charter was a forced settlement for specific documented grievances rather than a general rights declaration; the chronicle record (Roger of Wendover, the Barnwell chronicler) independently catalogs John's imprisonments and disseisins; and the crown's own reissue texts (1216, 1217, 1225) re-enumerate the abuses being remedied, conceding their reality. No source attests the founding problem live after John's death in 1216 — the reading itself denies it, and the later crises were new grievances outside the catalog.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.44 (end of interval): the clause takes only enumerated prerogatives — under this reading its content is exhausted by the 1215 catalog — but what it takes is real, enforceable, and was extracted under armed duress; the settlement simultaneously leaves the crown's revenue powers over the unenfranchised majority intact, which is part of why the crown could accept it. Suppression 0.50: the arrangement's maintenance was genuinely coercive at the start (clause 61's distraint committee, then civil war and a French invasion when John repudiated) and thinned to political leverage after 1225, when the security clause was dropped from the definitive text and enforcement became a matter of bargained reconfirmation. Theater_ratio 0.42 at interval end, rising from 0.15: the clause's founding problem died with John in 1216, so an increasing share of the charter's operation is invocation rather than application — the 1297 Confirmation functioned chiefly as a taxation bargaining chip. Accessibility_collapse 0.25: the reading preserves the vast alternative space of non-enumerated prerogative; understanding the clause collapses almost no alternatives. Resistance 0.65: the bound party repudiated the charter within ten weeks, obtained papal annulment, and fought a two-year war before settling into bargained compliance. The claimed type (tangled_rope) is authored from structure — genuine coordination function, asymmetric incidence, active enforcement — independently of these metric values; the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the baronial seat the arrangement is a hard-won protection device whose enforcement they personally staffed — its costs are invisible and its benefits are their inheritances. From the crown's seat the same structure is a diminution of sovereignty imposed at sword-point — though the crown's dynastic stake in settlement stability and its bargained revenue recoveries damp the pure-target reading. From the unfree majority's position the celebrated clause is an arrangement that priced their protection at zero while stabilizing the regime that extracts from them; villeinage's atomization also forecloses the coalition route a powerless class might otherwise take. The papacy evaluates the whole structure from outside the realm's jurisdiction as an unjust forced concession. The analytical seat sees all of these simultaneously and can hold the originalist scope question — what the clause's content IS — apart from any seat's valuation of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (rebel_barons_of_1215, free_tenantry_of_the_shires) drive those seats toward the beneficiary end: the arrangement subsidizes their security and costs them little beyond the ordinary taxation bargains. Victim declarations place the_crown and unfree_peasantry toward the target end: the crown pays enumerated prerogative (its constrained exit and the war it fought to escape confirm the cost is real), and the peasantry bear the residual royal extraction the arrangement leaves running, with no exit at all. The crown's secondary beneficiary position (settlement stability, reissue revenue) damps its derived directionality below a pure target — authored as a secondary_role, not a directionality override, because the derivation chain reads the dual position directly. Suppression is authored as a raw structural property of the enforcement machinery and is NOT scaled; only extractiveness is scaled by the engine (by directionality and national spatial scope). The papacy sits near-symmetric as an external actor; excluded and analytical seats carry no extraction either way.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the charter as a pure coordination device (the civic-religion story of universal liberation) misses the asymmetric incidence this reading makes central: protection for the negotiating class, payment by the crown, the majority left outside — hence tangled_rope, not rope. Reading it as pure extraction (a baronial rent-seeking instrument) misses the documented coordination function: the clause solved a real commitment problem, and each reissue was purchased with real concessions. The R5 genealogy sharpens the drift: the founding problem (John's documented abuses) is dead — attested dead by the papal annulment record, the chronicles, and the crown's own reissue enumerations, and by no one attested live after 1216 — while the arrangement persists and the world rearranges if it is removed. That mismatch (dead founding problem, world-rearranging persistence) is the capture/zombie signature the R5 consumer cross-checks, and the rising theater_ratio is its temporal trace: what persists past 1216 is partly a bargaining standard and partly ceremony. Mandatrophy is not declared resolved in this file because within the interval the clause still performed real operative work at the 1258 and 1297 crises; the atrophy trend is carried by the measurement series instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clause_39_kernel_reading_underdetermination,
    'This story is one reading (originalist_limitation_reading) of the kernel magna_carta_clause_39; which reading governs the clause''s structural classification, and at which structural elements do the readings part ways?',
    'Comparative adjudication against the drafting record, the reissue texts (1216/1217/1225/1297), and enforcement episodes: the liberal_due_process_reading would expand the protected class to all subjects and all later grievances (raising epsilon and enlarging the victim set); the feudal_prerogative_reading would reduce the clause to a procedural incident of feudal hierarchy (shrinking extraction toward the coordination floor). The disagreement is located in the extension of ''free man'' and the content of ''the law of the land.''',
    'Adopting the liberal reading converts this moderate-epsilon tangled rope into a substantially extractive contested constraint with a mass victim set; adopting the feudal reading collapses extraction toward a rope''s floor. This file''s epsilon is valid only under the originalist scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clause_39_kernel_reading_underdetermination, conceptual, 'Which of three rival readings of clause 39 fixes the constraint''s scope, victim set, and epsilon.').

omega_variable(
    prerogative_limitation_extraction_status,
    'Is the crown''s surrendered prerogative extraction (value taken by the arrangement) or remedy (return of what the 1215 abuses had wrongly seized)?',
    'Adjudicate the wrongful character of John''s disseisins and imprisonments from the record: if they were wrongful seizures, the clause restores rather than takes, and the crown seat moves beneficiary-ward; if they were lawful prerogative curtailed, the clause takes, and the crown seat is a payer.',
    'If remedy, epsilon against the crown drops toward the coordination floor and the type trends toward rope; if extraction, the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prerogative_limitation_extraction_status, conceptual, 'Whether the crown''s lost prerogative counts as extraction or as restitution.').

omega_variable(
    displaced_extraction_magnitude,
    'How much royal extraction was structurally displaced onto (or simply left running against) the unprotected majority by a settlement that stabilized crown-baron relations while leaving the crown''s revenue powers over the majority intact?',
    'Economic-historical reconstruction of royal revenue composition (tallage, scutage, wardship, reliefs) before and after 1215 across the interval.',
    'Substantial displacement enlarges the victim set and firms the tangled_rope classification; negligible displacement reduces victims to the crown alone and trends toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_extraction_magnitude, empirical, 'Size of the extraction the settlement''s narrow scope left running against the unenfranchised.').

omega_variable(
    enforcement_capacity_attribution,
    'What fraction of the arrangement''s persistence across the interval rested on baronial coercive capacity versus crown self-interest in bargained legitimacy?',
    'Episode analysis of each enforcement event (1215 distraint, 1216-17 war, 1225 scutage bargain, 1244 joint demands, 1258 Provisions, 1265 Montfort parliament, 1297 confirmation): who supplied the coercive force, and what did each party concede?',
    'If crown self-interest dominates, suppression_requirement is over-measured and the arrangement is more self-sustaining (rope-ward); if baronial coercion dominates, the enforcement dependence is structural (tangled_rope confirmed, snare-ward at the coercive peaks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_attribution, empirical, 'Attribution of the arrangement''s persistence to coercion versus bargained interest.').

omega_variable(
    reissue_function_or_theater,
    'Were the successive reissues (1217, 1225, 1297) functional renewals of the clause''s original protective operation, or increasingly ceremonial consolidations whose operative content was the taxation bargain attached to them?',
    'Compare, per reissue, the ratio of enforcement episodes invoking the clause to ceremonial or confirmatory invocations; track the shrinking text (clause 61 dropped in 1225) against unchanged ceremonial language.',
    'Rising ceremonial share supports the theater_ratio trajectory and a post-1297 drift toward inertial maintenance; a stable functional share supports continued operative enforcement within the reading''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reissue_function_or_theater, empirical, 'Whether reissue activity renewed function or performed continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 1215, 1297).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_orig_tr_t1215, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement_basis(mc39_orig_tr_t1215, observed).
narrative_ontology:measurement(mc39_orig_tr_t1217, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1217, 0.2).
narrative_ontology:measurement_basis(mc39_orig_tr_t1217, observed).
narrative_ontology:measurement(mc39_orig_tr_t1225, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1225, 0.32).
narrative_ontology:measurement_basis(mc39_orig_tr_t1225, observed).
narrative_ontology:measurement(mc39_orig_tr_t1244, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1244, 0.3).
narrative_ontology:measurement_basis(mc39_orig_tr_t1244, observed).
narrative_ontology:measurement(mc39_orig_tr_t1258, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1258, 0.24).
narrative_ontology:measurement_basis(mc39_orig_tr_t1258, observed).
narrative_ontology:measurement(mc39_orig_tr_t1265, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1265, 0.28).
narrative_ontology:measurement_basis(mc39_orig_tr_t1265, observed).
narrative_ontology:measurement(mc39_orig_tr_t1280, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1280, 0.36).
narrative_ontology:measurement_basis(mc39_orig_tr_t1280, observed).
narrative_ontology:measurement(mc39_orig_tr_t1297, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 1297, 0.42).
narrative_ontology:measurement_basis(mc39_orig_tr_t1297, observed).

% Extraction over time
narrative_ontology:measurement(mc39_orig_be_t1215, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1215, 0.46).
narrative_ontology:measurement_basis(mc39_orig_be_t1215, observed).
narrative_ontology:measurement(mc39_orig_be_t1217, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1217, 0.41).
narrative_ontology:measurement_basis(mc39_orig_be_t1217, observed).
narrative_ontology:measurement(mc39_orig_be_t1225, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1225, 0.37).
narrative_ontology:measurement_basis(mc39_orig_be_t1225, observed).
narrative_ontology:measurement(mc39_orig_be_t1244, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1244, 0.42).
narrative_ontology:measurement_basis(mc39_orig_be_t1244, observed).
narrative_ontology:measurement(mc39_orig_be_t1258, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1258, 0.51).
narrative_ontology:measurement_basis(mc39_orig_be_t1258, observed).
narrative_ontology:measurement(mc39_orig_be_t1265, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1265, 0.56).
narrative_ontology:measurement_basis(mc39_orig_be_t1265, observed).
narrative_ontology:measurement(mc39_orig_be_t1280, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1280, 0.4).
narrative_ontology:measurement_basis(mc39_orig_be_t1280, observed).
narrative_ontology:measurement(mc39_orig_be_t1297, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 1297, 0.44).
narrative_ontology:measurement_basis(mc39_orig_be_t1297, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc39_orig_su_t1215, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1215, 0.85).
narrative_ontology:measurement_basis(mc39_orig_su_t1215, observed).
narrative_ontology:measurement(mc39_orig_su_t1217, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1217, 0.78).
narrative_ontology:measurement_basis(mc39_orig_su_t1217, observed).
narrative_ontology:measurement(mc39_orig_su_t1225, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1225, 0.55).
narrative_ontology:measurement_basis(mc39_orig_su_t1225, observed).
narrative_ontology:measurement(mc39_orig_su_t1244, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1244, 0.6).
narrative_ontology:measurement_basis(mc39_orig_su_t1244, observed).
narrative_ontology:measurement(mc39_orig_su_t1258, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1258, 0.72).
narrative_ontology:measurement_basis(mc39_orig_su_t1258, observed).
narrative_ontology:measurement(mc39_orig_su_t1265, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1265, 0.8).
narrative_ontology:measurement_basis(mc39_orig_su_t1265, observed).
narrative_ontology:measurement(mc39_orig_su_t1280, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1280, 0.42).
narrative_ontology:measurement_basis(mc39_orig_su_t1280, observed).
narrative_ontology:measurement(mc39_orig_su_t1297, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 1297, 0.5).
narrative_ontology:measurement_basis(mc39_orig_su_t1297, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39__feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Magna Carta clause 39' covers three structurally distinct constraints (epsilon-invariance decomposition of one kernel). This file instantiates the originalist limitation reading: scope frozen at the 1215 grievance catalog, baronial-class protection, moderate epsilon (0.44), tangled_rope structure. The liberal_due_process_reading instantiates a universal-rights constraint with an enlarged victim set (all later subjects facing arbitrary power) and substantially higher epsilon; the feudal_prerogative_reading instantiates a hierarchical-incident constraint whose extraction sits near the coordination floor. Each is a separate story with its own beneficiaries, victims, and epsilon; the family is linked through affects_constraints. The siblings are downstream of the same sealed text but diverge on the extension of 'free man' and the content of 'the law of the land.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
