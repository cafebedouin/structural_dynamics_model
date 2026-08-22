% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215 as Sealed Baronial Feudal Contract
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the baronial_privilege_reading of kernel
 *   magna_carta_1215: the charter as a sealed feudal contract between King
 *   John and the rebel baronage, in which 'free man' denotes the landowning
 *   baronial class and the protection set is exhausted by the contracting
 *   parties. On this reading the arrangement solves a real post-civil-war
 *   coordination problem — defining crown-baron fiscal and jurisdictional
 *   terms — while concentrating its gains on the drafting class, extracting
 *   concessions from the crown, and leaving the unfree majority,
 *   non-landholding freemen, and unpropertied women exactly where they were,
 *   with their exposure codified in places (differential amercements) and
 *   legitimated by the settlement's success. Constraint-family note: this is
 *   one of three linked readings of the same kernel; the epsilon referent
 *   here is the standing 1215 arrangement itself assessed by this reading's
 *   own lights (a feudal contract), NOT the universal-rights arrangement the
 *   universal_rights_reading would endorse and NOT the accumulated reissue
 *   chain the living_document_reading treats as the real constitution. The
 *   siblings are separate files linked via network.affects_constraints; their
 *   epsilon values differ because they instantiate different constraints, not
 *   because this constraint is measured differently from different angles.
 *
 * KEY AGENTS:
 *   - landowning_barons: primary beneficiary and drafter (organized/trapped) — collects fixed reliefs, consent rights, property protections; staffs the enforcement organ; no exit from the feudal relation itself
 *   - the_crown: primary payer (institutional/trapped) — surrenders discretionary revenue instruments under armed compulsion; recourse is escalation (papal appeal, war), not exit
 *   - english_church_hierarchy: secondary beneficiary (institutional/constrained) — written liberties and brokerage role, double-edged loyalty to Rome
 *   - security_committee_of_twenty_five: enforcement agenda-setter (organized/constrained) — clause 61 self-help organ, never successfully invoked, lapsed by 1216
 *   - baronial_widows_and_heirs: conditional beneficiaries (moderate/constrained) — protected as members of the baronial property complex, not as persons as such
 *   - unfree_villein_tenantry: excluded and cost-bearing (powerless/trapped) — no seats, no justiciable protections, differential amercement exposure, seigneurial exaction consolidated
 *   - non_landholding_free_tenants: excluded middle (moderate/constrained) — nominal textual coverage without leverage or enforcement access
 *   - papacy: analytical observer with coercive reach (institutional/analytical) — annulled the charter, excommunicated rebels, reshaped the arrangement's environment from outside the contract
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.58).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.35).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 as Sealed Baronial Feudal Contract").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '142dfb3e-57cf-4eb6-bea9-81488a4a2656').
narrative_ontology:cs_kernel_codification('142dfb3e-57cf-4eb6-bea9-81488a4a2656', fixed_text).
narrative_ontology:cs_authority_grounding('142dfb3e-57cf-4eb6-bea9-81488a4a2656', lineage).
narrative_ontology:cs_interpretation_layer_present('142dfb3e-57cf-4eb6-bea9-81488a4a2656').
narrative_ontology:cs_reading_relation('142dfb3e-57cf-4eb6-bea9-81488a4a2656', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('142dfb3e-57cf-4eb6-bea9-81488a4a2656', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('142dfb3e-57cf-4eb6-bea9-81488a4a2656', foundational, protection_limited_to_contracting_parties).
narrative_ontology:cs_axiom_status(protection_limited_to_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('142dfb3e-57cf-4eb6-bea9-81488a4a2656', protection_limited_to_contracting_parties, conventional).
narrative_ontology:cs_axiom('142dfb3e-57cf-4eb6-bea9-81488a4a2656', foundational, liber_homo_denotes_landowning_barons).
narrative_ontology:cs_axiom_status(liber_homo_denotes_landowning_barons, holdable).
narrative_ontology:cs_axiom_grounding('142dfb3e-57cf-4eb6-bea9-81488a4a2656', liber_homo_denotes_landowning_barons, empirically_contingent).
narrative_ontology:cs_reference_frame('142dfb3e-57cf-4eb6-bea9-81488a4a2656', sealed_feudal_contract_1215).
narrative_ontology:cs_drift_state('142dfb3e-57cf-4eb6-bea9-81488a4a2656', post_papal_annulment_reissue_chain, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('142dfb3e-57cf-4eb6-bea9-81488a4a2656', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, english_church_hierarchy).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, baronial_widows_and_heirs).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, unfree_villein_tenantry).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, consent_to_taxation_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, lawful_limit_on_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tenants-in-chief who drafted the terms presented at Runnymede, obtained fixed reliefs, consent rights over scutage and aids, protections for land, wardship, and marriage interests, and a standing committee empowered to distrain on royal castles upon breach. Their wealth and status were constituted by landholding and lordship within the realm; there was no life for them outside the feudal relation, so their lever against the king was collective armed refusal, not departure. They drafted, signed, and staffed the enforcement organ.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter).

% King John granted the charter with rebel armies in the field and London in hostile hands. The grant surrendered discretionary revenue instruments (negotiated reliefs, unconsented scutage, arbitrary amercements, exploitative wardship and marriage sales) that had financed royal policy. A monarch cannot exit his own kingdom's law; his recourse was escalation rather than exit — appealing over the barons' heads to the pope, which produced annulment, excommunication of the rebels, and eighteen months of civil war and French invasion before a modified restoration under his infant son.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, the_crown, payer,
    institutional, generational, trapped, national).

% The opening clause guarantees the church's liberties and free elections; Archbishop Stephen Langton brokered between the parties and supplied the ideological framing from coronation-oath tradition. The church gained written protection for its estates and elections while lending the settlement moral legitimacy. Its position was double-edged: its head sat in Rome, and Rome annulled the charter the church's English branch had helped midwife.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, english_church_hierarchy, beneficiary,
    institutional, generational, constrained, national).

% The clause 61 organ: twenty-five barons empowered to judge breaches and, with the majority of their fellows, to distrain on royal castles, lands, and possessions until redress. It was the arrangement's self-help enforcement arm, staffed by the beneficiary class itself. No successful distraint is recorded; the organ lapsed when the 1216 reissue dropped the clause, and its members folded back into the general baronial estate.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, security_committee_of_twenty_five, agenda_setter,
    organized, biographical, constrained, national).

% Widows and heirs of tenants-in-chief received written protections: a widow may remain in her husband's house for forty days, may not be compelled to remarry, and minors' lands are held without waste or exorbitant escheats. Their inclusion tracks membership in the contracting parties' property complex, not personhood as such — a woman connected to baronial land is inside the protection set; a woman without such connection is outside it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, baronial_widows_and_heirs, beneficiary,
    moderate, biographical, constrained, regional).

% The majority of the rural population had no seats at Runnymede and no standing under the settlement. The amercement clauses protect the chattels of free men while expressly permitting a villein's whole chattels to be taken; the general clause requiring magnates to observe the same customs toward their men carried no enforcement machinery a villein could invoke. Seigneurial dues, labor services, and jurisdictional profits over them continued untouched, and the settlement's success consolidated the lordly class that collected them. They could not leave the manor, the status, or the economy built on both.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, unfree_villein_tenantry, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, unfree_villein_tenantry, payer).

% Sokemen, burgesses, and minor freeholders occupy the ambiguous middle: some clauses reach them by the letter of 'free man,' but they held no leverage in the negotiation, no seat on the enforcement committee, and no independent means of compelling observance. Their practical access to the charter's protections ran through the goodwill of lords and sheriffs — the very officials the charter partially restrained.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landholding_free_tenants, excluded,
    moderate, biographical, constrained, regional).

% Pope Innocent III adjudicated the settlement's validity from outside the contracting parties: within ten weeks of Runnymede he quashed the charter, suspended Langton, and later excommunicated the rebel barons, treating a coerced diminution of a crusading king's realm as usurpation of both royal and apostolic authority. His intervention reshaped the arrangement's operating environment — driving it underground, then into the reissue sequence — without himself being bound or protected by it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, papacy, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:fixing_cost_class(magna_carta_1215__baronial_privilege_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settled the terms of fiscal and jurisdictional relations between the crown and its tenants-in-chief after two years of civil war: scutage and aids require common counsel except in specified cases; reliefs are fixed sums rather than negotiated exactions; wardship, marriage, and escheat are rule-bound; amercements are proportionate and assessed by peers; breach triggers a defined self-help procedure through a standing committee.
% TRANSFER_FUNCTION: Moves discretionary revenue and jurisdictional freedom from the crown to the baronial class — fixed reliefs instead of negotiable ones, consent gates before extraordinary taxation, protected succession and marriage interests — while confirming, and thereby legitimating, the barons' own jurisdictional and economic dominion over their tenants.
% ABSENT_VOICES: Unfree peasants, non-landholding freemen, the town commons, and women outside tenant-in-chief widowhood had no representation at Runnymede and no seat in the enforcement machinery; the settlement's unanimity arose because every exposed seat outside the landholding elite was never in the room. The crown's own assent was given under armed compulsion, so even the seated payer's consent was extracted rather than given.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight, the civil war resumes from the battlefield position of June 1215: the baronial coalition either forces harsher terms or, as nearly happened historically, delivers the realm to Capetian France through Prince Louis. The specific crown-baron fiscal bargain would have been rewritten under foreign arbitration or conquest rather than domestic contract, and the enforcement coalition's cohesion — the thing the charter froze into text — would have had to be maintained by continual warfare instead.
% FOUNDING_PROBLEM: Royal fiscal arbitrariness: King John's exploitation of feudal incidents — punitive reliefs, exploitative wardships and marriage sales, repeated unconsented scutage at elevated rates, disproportionate amercements — had made tenancy-in-chief economically unpredictable and politically intolerable to the baronial class, culminating in open revolt.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the crown's own minority government reissued the charter three times in a decade (1216, 1217, 1225), each reissue conceding the grievance was unresolved; the 1225 confirmation was explicitly purchased by a taxation grant, corroborating that the fiscal contest persisted; papal registers and the treaty ending the French intervention independently document that the underlying conflict outlived the 1215 text. No party claims the problem was solved in 1215 — the beneficiaries' own successors kept renegotiating it.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the arrangement transfers substantial prerogative value from crown to baronial class (fixed reliefs below market, consent gates on taxation, protected succession), and its exclusivity imposes entrenchment costs on non-parties; it falls short of pure rent collection because the obligations are mutual, specific, and grievance-mapped. Suppression 0.35 at interval end: the standing arrangement as of 1225 operates as ordinary law under a minority government that has consented to it, with coercive overhead routinized into courts and chancery — the scalar reports the end-state, while the measurement series records the militarized origin (0.78 at sealing, peaking 0.88 during the French invasion) decaying as the settlement normalizes. Theater 0.22: the 1215 text mapped clause-by-clause onto documented grievances, but the clause 61 machinery was never successfully invoked and was quietly dropped at reissue — a bounded share of performative text within a mostly functional instrument. Accessibility_collapse 0.35: alternatives visibly persisted — renewed war, Capetian arbitration, three renegotiated reissues in a decade; the arrangement did not foreclose its alternatives, it survived them. Resistance 0.80: immediate royal repudiation, papal annulment, eighteen months of civil war and foreign invasion — among the highest-resistance settlements on record, which is itself evidence that the arrangement bound real interests. All three tracked series run on one shared time grid (1215, 1216, 1217, 1219, 1222, 1225) with every metric authored at every point; the compiler's alignment rule is satisfied by construction. The trajectories tell one story: extraction dips while the text is void and war rages, then consolidates as the reissued settlement is purchased with taxation grants; suppression spikes with open military enforcement and decays as enforcement migrates into ordinary administration; theater rises while the text and its operation diverge (annulled but reissued) and falls when the 1225 text restores fit.
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute radically different arrangements from the same parchment. From the baronial seat this is hard-won lawful liberty: written limits on a predatory king, bought with rebellion and defended by an enforcement committee. From the crown's seat it is coerced confiscation of royal revenue sealed at swordpoint and void ab initio — the pope agreed. From the villein's seat it is a settlement among predators: the king's hand lightened on his enemies, his allies' hands on their tenants untouched and newly legitimated. From the papal seat it is usurpation of both regal and apostolic jurisdiction. The engine computes these divergences from the structural data (opposed roles at comparable power, differentiated exits); nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for landowning_barons, english_church_hierarchy, and baronial_widows_and_heirs; victim declarations drive high directionality for the_crown and unfree_villein_tenantry. Exit modulation matters: the crown and the villeins are both trapped, but the crown's institutional power dampens its effective extraction somewhat while the villeins' powerlessness leaves theirs undamped — the same victim declaration yields different effective extraction by seat. The barons' dual position (beneficiaries who also drafted and staffed enforcement) is captured by secondary_role rather than a directionality override, because the structural derivation already reads them as beneficiaries through the Phase B arrays and no override is needed to produce the correct relationship. No directionality_overrides are authored: every seat's derived directionality matches its structural position, and the guidance reserves overrides for cases the derivation gets wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents mislabeling in both directions. Reading the arrangement as pure coordination (rope) erases the asymmetric extraction: concentrated gains for the drafting class, compulsory concessions from the crown, and codified differential exposure for the excluded — the same structure that coordinates the elite entrenches everyone else's position. Reading it as pure extraction (snare) erases the genuine coordination function: the settlement really did solve a collective-action problem that two years of war had failed to solve, really did map onto documented grievances, and really did stabilize crown-baron relations once restored. The R5 interview confirms the arrangement is not a zombie: founding_problem_status is live (the fiscal contest recurred through every reissue and beyond) and disappearance_verdict is world_rearranges — the healthy cell, not the dead-problem-plus-dependence mismatch that flags capture. The clause 61 lapse is a partial atrophy signal (one enforcement organ died), but the arrangement escapes piton classification on the cost-asymmetry test: a concentrated beneficiary exists (gain_flow names the baronial class), the administrator could and did change the text, and the changes were fought over rather than neglected. Fixing_cost is prohibitive on direct evidence: the one historical attempt to remove the arrangement (papal annulment plus royal repudiation) produced civil war, foreign invasion, and a modified restoration within eighteen months.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_kernel_magna_carta_1215,
    'This story instantiates the baronial_privilege_reading of kernel magna_carta_1215; how would the constraint''s structure differ if instantiated under the universal_rights_reading or living_document_reading siblings?',
    'Comparative analysis of the compiled sibling stories: victim/beneficiary sets, epsilon values, and per-seat classifications across readings of the same sealed text.',
    'Under universal_rights_reading the protection set widens to all persons, adding large beneficiary and victim populations and redistributing directionality across seats; under living_document_reading the referent becomes the accumulated reissue-and-interpretation chain rather than the sealed 1215 contract, changing the temporal interval and the enforcement picture entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_kernel_magna_carta_1215, conceptual, 'Committer-frame routing: one kernel, three readings, three distinct constraints.').

omega_variable(
    liber_homo_semantic_scope,
    'Does ''liber homo'' in clauses 39-40 of the 1215 text denote landowning barons and tenants-in-chief only, or the broader class of free tenants including sokemen and burgesses?',
    'Philological analysis of the 1215 text''s internal usage, witness-list composition, contemporary glosses, and the narrowing or widening edits visible in the 1217 and 1225 reissues.',
    'If the term is broader than the baronage, the protection set exceeds the contracting parties and this reading''s exclusivity axiom weakens toward the universal sibling; if narrowly baronial, the reading is confirmed and the excluded-class victim structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_semantic_scope, empirical, 'The semantic scope of ''free man'' is the precise locus of the kernel contest.').

omega_variable(
    duress_validity_of_consent,
    'Can a contract sealed under armed compulsion at Runnymede ground a legitimate standing arrangement, or does the duress condition dissolve the contractual frame this reading depends on?',
    'Doctrinal analysis of medieval canon-law and Roman-law duress doctrine applied to the sealing circumstances, cross-checked against the parties'' own subsequent conduct (both sides treated the grant as binding enough to fight over).',
    'If duress voids consent, the arrangement is better modeled as imposed terms than as contract, shifting the coordination-function assessment and strengthening the extraction reading; if ratified by conduct and reissue, the contractual frame holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_validity_of_consent, conceptual, 'Whether the contract frame survives the compulsion under which it was sealed.').

omega_variable(
    clause_61_deterrence_vs_dead_letter,
    'Did the clause 61 enforcement machinery (the twenty-five barons'' power of distraint) operate as a deterrent that made invocation unnecessary, or was it a dead letter from birth that the crown simply outlasted?',
    'Chronicle and chancery-record analysis of any convening attempts, correspondence among the twenty-five, and the speed with which the machinery lapsed after the 1216 reissue dropped it.',
    'If deterrence was real, the enforcement requirement was cheaply satisfied and the coordination assessment improves; if dead letter, the 1215 settlement''s stability rested wholly on military stalemate and the enforcement declaration in this story describes capacity rather than operation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clause_61_deterrence_vs_dead_letter, empirical, 'Whether the charter''s distinctive self-help enforcement organ ever functioned.').

omega_variable(
    exclusion_or_entrenchment,
    'Did the charter merely leave non-parties (villeins, non-landholding freemen, women outside tenant-in-chief widowhood) unprotected, or did it actively entrench their exposure by codifying differential treatment and legitimating the settlement that preserved seigneurial jurisdiction?',
    'Clause-level analysis (differential amercement scales, the unenforceable reciprocity of the general observance clause) combined with manorial records tracking seigneurial exaction before and after 1215.',
    'If mere exclusion, the non-party populations sit outside the constraint''s operation and belong mainly in the absent-voices record; if entrenchment, they are genuine victims whose costs the baronial coalition''s enforcement implicitly purchased, raising effective extraction on the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_or_entrenchment, empirical, 'Whether the narrow protection set imposed costs on those it omitted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1216, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1216, 0.3).
narrative_ontology:measurement_basis(magn_tr_t1216, observed).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.27).
narrative_ontology:measurement_basis(magn_tr_t1217, observed).
narrative_ontology:measurement(magn_tr_t1219, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1219, 0.33).
narrative_ontology:measurement_basis(magn_tr_t1219, observed).
narrative_ontology:measurement(magn_tr_t1222, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1222, 0.29).
narrative_ontology:measurement_basis(magn_tr_t1222, observed).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.22).
narrative_ontology:measurement_basis(magn_tr_t1225, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.52).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1216, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1216, 0.46).
narrative_ontology:measurement_basis(magn_be_t1216, observed).
narrative_ontology:measurement(magn_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.44).
narrative_ontology:measurement_basis(magn_be_t1217, observed).
narrative_ontology:measurement(magn_be_t1219, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1219, 0.47).
narrative_ontology:measurement_basis(magn_be_t1219, observed).
narrative_ontology:measurement(magn_be_t1222, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1222, 0.52).
narrative_ontology:measurement_basis(magn_be_t1222, observed).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.58).
narrative_ontology:measurement_basis(magn_be_t1225, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.78).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1216, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1216, 0.88).
narrative_ontology:measurement_basis(magn_su_t1216, observed).
narrative_ontology:measurement(magn_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.72).
narrative_ontology:measurement_basis(magn_su_t1217, observed).
narrative_ontology:measurement(magn_su_t1219, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1219, 0.5).
narrative_ontology:measurement_basis(magn_su_t1219, observed).
narrative_ontology:measurement(magn_su_t1222, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1222, 0.42).
narrative_ontology:measurement_basis(magn_su_t1222, observed).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.35).
narrative_ontology:measurement_basis(magn_su_t1225, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, resource_allocation).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel magna_carta_1215 per the epsilon-invariance principle. The colloquial label 'Magna Carta' covers three structurally distinct claims: (1) this story — the sealed 1215 instrument as a feudal contract whose protection set is exhausted by the contracting parties; (2) universal_rights_reading — the same instrument as transhistorical rights precedent emitting universal due-process constraint; (3) living_document_reading — the accumulated reissue-and-interpretation chain as adaptive constitutional substrate. Each carries its own epsilon, beneficiary/victim structure, and interval: this reading authors epsilon for the narrow king-baron settlement (moderate-high, concentrated gains, excluded classes bearing entrenchment costs); the universal reading authors epsilon for an arrangement governing all persons; the living reading authors epsilon for a moving referent of precedential accumulation. This upstream reading supplies the fixed-text baseline that the living_document_reading's development claim is measured against, hence the influence edge; it coexists with the universal reading as rival answers to 'what is this document?' held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
