% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__originalist_limitation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Magna Carta Clause 39 - Originalist Limitation Reading (Bounded 1215 Grievance Corrective)
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   Clause 39 of Magna Carta (1215) forbids the Crown to seize, imprison,
 *   disinherit, exile, or outlaw any free man except by lawful judgment of
 *   his equals or the law of the land. This story instantiates the
 *   originalist limitation reading of that text: the clause is a bounded
 *   corrective aimed at the specific royal abuses documented in the 1215
 *   grievance record - John's disseisins, imprisonments without judgment,
 *   punitive amercements, and hostage-taking - protecting the chartered free
 *   class of that world and no one else, with no authority beyond its
 *   originating settlement. On this reading the constraint is a coerced
 *   bilateral settlement inside the feudal order: a genuine commitment device
 *   that made royal restraint credible and inspectable, purchased by the
 *   baronial coalition at Runnymede under arms, enforced first by the
 *   Twenty-Five's distraint powers and after 1217 by ordinary royal justice,
 *   and reissued in 1216, 1217, and 1225 with its enforcement machinery
 *   deleted and its exchange terms made explicit. CONSTRAINT FAMILY: this is
 *   one of three stories decomposed from the magna_carta_clause_39 kernel per
 *   the epsilon-invariance principle - the liberal_due_process_reading
 *   authors the same words as a universal rights guarantee (different
 *   protected class, different epsilon), and the feudal_prerogative_reading
 *   authors them as narrow vassal-lord procedure; each is a separate file
 *   linked through network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - the_crown: Primary target (institutional/constrained) -
 *   bears the constraint's restriction of coercive prerogative while
 *   administering it through its own courts - baronial_coalition_of_1215:
 *   Primary beneficiary (organized/constrained) - collects the settlement's
 *   security and staffed its original enforcement - chartered_free_tenantry:
 *   Secondary beneficiary (moderate/constrained) - formal protection as free
 *   men without magnate leverage - papal_see: Inter-institutional
 *   agenda-setter (institutional/arbitrage) - annulled the charter and
 *   supervised the reissues - unfree_villein_majority: Excluded party
 *   (powerless/trapped) - outside the clause's words entirely -
 *   constitutional_historians: Analytical observer - sees the full structure
 *   across the reissue chain
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, 0.47).
domain_priors:suppression_score(magna_carta_clause_39__originalist_limitation_reading, 0.58).
domain_priors:theater_ratio(magna_carta_clause_39__originalist_limitation_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(magna_carta_clause_39__originalist_limitation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__originalist_limitation_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__originalist_limitation_reading, "Magna Carta Clause 39 - Originalist Limitation Reading (Bounded 1215 Grievance Corrective)").
narrative_ontology:topic_domain(magna_carta_clause_39__originalist_limitation_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__originalist_limitation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__originalist_limitation_reading, 'f5cb6fbd-85ba-4902-a75f-e98a47b18ec3').
narrative_ontology:cs_kernel_codification('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', fixed_text).
narrative_ontology:cs_authority_grounding('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', lineage).
narrative_ontology:cs_interpretation_layer_present('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3').
narrative_ontology:cs_reading_relation('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', magna_carta_clause_39__feudal_prerogative_reading, coexists_with).
narrative_ontology:cs_axiom('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', foundational, grievance_record_exhausts_content).
narrative_ontology:cs_axiom_status(grievance_record_exhausts_content, holdable).
narrative_ontology:cs_axiom_grounding('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', grievance_record_exhausts_content, empirically_contingent).
narrative_ontology:cs_axiom('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', secondary, chartered_free_class_only).
narrative_ontology:cs_axiom_status(chartered_free_class_only, holdable).
narrative_ontology:cs_axiom_grounding('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', chartered_free_class_only, empirically_contingent).
narrative_ontology:cs_reference_frame('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', runnymede_grievance_settlement).
narrative_ontology:cs_drift_state('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', post_1225_confirmation_chain, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f5cb6fbd-85ba-4902-a75f-e98a47b18ec3', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__originalist_limitation_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, baronial_coalition_of_1215).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__originalist_limitation_reading, chartered_free_tenantry).
narrative_ontology:constraint_victim(magna_carta_clause_39__originalist_limitation_reading, the_crown).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, lawful_judgment_of_peers).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__originalist_limitation_reading, law_of_the_land_procedure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the coercive apparatus the clause binds. At Runnymede it conceded, under armed pressure, that seizure, imprisonment, disinheritance, exile, and outlawry require lawful judgment of peers or the law of the land, for the specific abuses the barons documented. It attempted to void the concession through papal annulment, which triggered renewed civil war and the king's own death; the reissues that followed made the restraint a royal grant administered through the Crown's own courts. Exit would mean repudiating the settlement a third time and re-fighting a war it already lost twice.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, the_crown, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__originalist_limitation_reading, the_crown, agenda_setter).

% The northern barons and associated earls who brought the kingdom to the brink at Runnymede. They drafted the grievance lists from their own documented injuries - disseisin of lands, imprisonment without judgment, punitive amercements, hostage-taking - and received in clause 39 a procedural lock against repetition. Their protection is bounded to the free status they themselves hold; they collected the settlement's security and staffed its original enforcement committee. Their lands are held of the king and their hostages were exposed, so resuming arms was always available but ruinous.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, baronial_coalition_of_1215, beneficiary,
    organized, biographical, constrained, national).

% Knights and substantial townsmen below the magnate rank who fall within the words 'free man'. They receive the clause's protection formally but lacked the enforcement leverage the magnates held; their access to remedy runs through royal courts the Crown controls, and their interests at Runnymede were voiced indirectly through their lords.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, chartered_free_tenantry, beneficiary,
    moderate, biographical, constrained, national).

% The spiritual superior of a crusader king. It declared the charter null within weeks of Runnymede, absolved the king from his oath, and later supervised the reissues through its legate. It collected nothing material from the settlement; its stake was jurisdictional - whether a vassal king's coerced contract could bind without papal license. It could engage or withdraw from the English field at will.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, papal_see, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% The overwhelming majority of the population - villeins, cottars, and slaves - whose persons and holdings lie entirely outside the clause's words. Nothing in the settlement changes their exposure to lordly or royal power. They had no seat at Runnymede, no voice in the grievance lists, and no path to the protection the charter grants.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, unfree_villein_majority, excluded,
    powerless, generational, trapped, local).

% Later scholarship reconstructing what the clause meant in its own world: the charter-era records, the reissue chain, and the plea rolls. They see the full structure - the documented grievances, the deleted enforcement machinery, the tax-for-liberty exchanges - across the whole interval, and take no side in the settlement.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__originalist_limitation_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__originalist_limitation_reading, baronial_coalition_of_1215).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__originalist_limitation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces case-by-case bargaining and armed reprisal between the Crown and its military tenants with a fixed procedural rule: seizure, imprisonment, disinheritance, exile, and outlawry only by lawful judgment of peers or the law of the land. It solves the commitment problem of personal monarchy - making the king's restraint credible and inspectable - for the specific abuses the 1215 grievance lists documented.
% TRANSFER_FUNCTION: Moves security of person and heritable property out of royal discretion and into legal entitlement for the chartered free class; correspondingly removes from the Crown the standing capacity to treat a subject's life and lands as revocable at will. The 1225 reissue makes the exchange explicit: procedural protection granted in return for a fifteenth in taxation.
% ABSENT_VOICES: The unfree majority - villeins and cottars, perhaps four-fifths of the population - are named nowhere in the charter and had no seat at Runnymede; the clause's words protect free men only. Town communities and the knightly class below the magnates had indirect voice through their lords. Had the unfree been present they would object that the settlement secures the propertied against the king while leaving lordly power over the unfree wholly intact.
% DISAPPEARANCE_RATIONALE: The 1215 settlement was the load-bearing term of the crown-baron truce: strike clause 39 and the documented-abuse protections vanish, the enforcement committee loses its warrant, and the tax-for-liberty exchanges of 1217 and 1225 have nothing to deliver. Royal administration reverts to discretionary seizure, and the armed coalition that extracted the concession resumes the war it suspended.
% FOUNDING_PROBLEM: King John's documented practices of the preceding decade: disseisin of baronial lands without judgment, imprisonment of opponents without process, punitive amercements, hostage-taking, and the use of prerogative to treat tenants-in-chief's holdings as revocable. The clause was drafted directly against that documented record.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the pipe and patent rolls recording John's disseisins and amercements - hostile-witness evidence compiled by the Crown's own clerks - and by the contemporary chronicles (the Barnwell annalist, Wendover, Matthew Paris). The 1258 Provisions of Oxford, drafted by a later generation of reformers against new grievances, attest the problem's recurrence in successor reigns.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__originalist_limitation_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__originalist_limitation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__originalist_limitation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__originalist_limitation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__originalist_limitation_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__originalist_limitation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__originalist_limitation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The interval runs 1215-1265 (grid points at years elapsed; one shared grid across all tracked metrics). Extractiveness sits moderate (0.47 at the late-interval steady state) because the clause's own operation extracts asymmetrically but narrowly: the settlement was imposed on the Crown under arms, its first enforcement machinery gave a baronial committee distraint powers over the king, and the 1225 reissue made protection an explicit purchase (a fifteenth in tax) - yet the clause takes nothing from the unfree majority, who lie outside its words entirely, and its protection tracks a service (procedure) rather than open rent. Suppression (0.58) is structural throughout - external barriers of tenure, hostages, armies, and papal politics, not internalized belief - so no internalization omega is required. Theater ratio (0.33) reflects the annulment-reissue cycle: each reissue ceremonially 'restored' a charter whose substance had shifted (enforcement machinery deleted, forest law separated, tax exchange made explicit), and by the 1250s confirmation had become a ritual both factions performed while fighting over its meaning. Accessibility collapse (0.55): once the settlement was understood, pure prerogative rule was politically foreclosed - every attempt to revert triggered war or forced confirmation - but the foreclosure was political rather than physical, and the Crown retained working alternatives in judicial delay, selective enforcement, and statutory evasion. Resistance (0.75) is the highest-authored metric because the constraint met sustained armed and juridical resistance across the whole interval: papal annulment, two wars, and serial repudiation attempts. The measurement series shows a crisis-settlement-relaxation-recurrence cycle (Runnymede, annulment and war, the Marshal reissues, the 1225 definitive text, quiet judicial operation, the Provisions crisis, Evesham); base_properties are authored at the post-1225 steady-state phase, not at the 1215 enforcement maximum. The oscillation is not itself the extraction mechanism - it is the visible form of the Crown's repeated attempts to shed a constraint it could not cheaply discard.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute differently from identical text. From the Crown's seat the clause is a coerced surrender of prerogative it never ceased trying to recover - annulment, reissue negotiation, statutory evasion - and the same institution that bears the constraint also administers it through its own courts, so its experience mixes target-loss with operator-control. From the baronial coalition's seat the clause is hard-won security with their own injuries written into it. From the papal seat it is a jurisdictional object - a vassal king's contract subject to spiritual review. From the unfree majority's position - a seat with no voice in 1215 - nothing at all changed. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations drive the derivation. the_crown is the sole declared victim and bears the constraint's entire restriction (d near the full-target end), moderated by its dual position as administrator - the constraint runs on royal justice, so the Crown is also the machine's operator, which pulls its effective directionality short of pure target. baronial_coalition_of_1215 and chartered_free_tenantry are declared beneficiaries (d toward the subsidized end); the coalition's constrained exit - lands held of the king, hostages exposed - keeps it off the arbitrage pole despite its military capacity. papal_see holds no beneficiary or victim position, so its directionality falls to the canonical fallback, which suits an actor whose stake was jurisdictional rather than material. unfree_villein_majority is authored as excluded, not victim: the constraint takes nothing from them; their absence is recorded, not scored (authored absence never drives classification). No directionality overrides are used - the beneficiary/victim declarations plus exit options produce the correct structure without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The genealogy interview guards the trap this constraint most invites: treating a thirteenth-century corrective as either eternal law (the liberal sibling's move, on this reading) or dead letter. The founding problem - John's documented practices - was concretely remedied, but successor reigns regenerated analogous grievances that reformers answered by re-invoking the same text, so founding_problem_status is authored contested rather than dead; combined with disappearance_verdict world_rearranges, no dead-mandate zombie flag fires. The tangled_rope classification is what prevents mislabeling in both directions: it registers the genuine coordination function (a credible, inspectable commitment device replacing case-by-case coercion) without letting that function launder the coercive extraction (armed imposition, class-bound benefit, purchase-by-taxation). Mandatrophy_resolved is deliberately not declared: within the interval the mandate has not plainly outlived its function - it mutated, and the parties dispute whether the mutation is revival or replacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'This constraint is one reading of the magna_carta_clause_39 kernel (originalist_limitation_reading); what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Compare the three family files'' protected-class extensions, lawful-judgment referents, and authority-survival premises. The disagreement is located in the extension of ''free man'', the referent of ''lawful judgment of peers or the law of the land'', and whether the clause''s authority survives its originating grievances.',
    'Adopting the liberal_due_process_reading would universalize the protected class and detach epsilon from the 1215 record, enlarging the victim set enormously; adopting the feudal_prerogative_reading would narrow the constraint to vassal-lord procedure and lower the enforcement stakes. This file''s epsilon is valid only for the bounded reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Committer structure: one kernel, three readings; this file instantiates the bounded originalist reading and forecloses the liberal reading within a single framework.').

omega_variable(
    free_man_extension_ambiguity,
    'Does ''free man'' in the drafting extend to all free tenants (knights, townsmen) or effectively to the baronial negotiating class and their peers?',
    'Charter-era plea roll usage, the 1217 reissue''s added protections, and early thirteenth-century citation patterns in the royal courts.',
    'Determines the beneficiary set''s width and whether protection was class-bound or status-bound; a narrow reading concentrates the gains in the coalition and raises effective extraction against the Crown, a wide one diffuses them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_man_extension_ambiguity, empirical, 'Extension of the protected class in the charter''s own world.').

omega_variable(
    enforcement_machinery_dispensability,
    'Was the Twenty-Five''s enforcement machinery load-bearing, or did the clause survive its deletion in the 1216 reissue on independent judicial footing?',
    'Compare the clause''s operation before and after the 1216-1217 reissues: did royal courts sustain it without the distraint committee?',
    'If the machinery was dispensable, the coercive-extraction phase was brief and the coordination component dominates; if indispensable, the post-1216 constraint was a declaration the Crown honored at pleasure, shifting the picture toward weaker enforcement and higher theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_dispensability, empirical, 'Whether deletion of the enforcement committee changed the constraint''s enforcement basis.').

omega_variable(
    warrant_recurrence_vs_expiry,
    'Is the clause''s warrant the specific 1215 grievance record (now remedied) or the recurring structure of royal discretion that successor reigns regenerated?',
    'Trace whether each re-invocation (1217, 1225, the 1258 Provisions) answers newly documented abuses or re-performs the old settlement; the originalist reading treats each as a new bounded settlement rather than growth of the original.',
    'If the warrant is the specific record, the mandate expired with the grievances and later operation is inertial maintenance; if the warrant recurs, the constraint remains live coordination and the contested founding-problem status is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrant_recurrence_vs_expiry, conceptual, 'Whether the constraint''s mandate expired with its founding grievances or recurs with each reign''s abuses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__originalist_limitation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_orig_tr_t0, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(mc39_orig_tr_t0, observed).
narrative_ontology:measurement(mc39_orig_tr_t8, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(mc39_orig_tr_t8, observed).
narrative_ontology:measurement(mc39_orig_tr_t16, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement_basis(mc39_orig_tr_t16, observed).
narrative_ontology:measurement(mc39_orig_tr_t25, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(mc39_orig_tr_t25, observed).
narrative_ontology:measurement(mc39_orig_tr_t33, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 33, 0.28).
narrative_ontology:measurement_basis(mc39_orig_tr_t33, observed).
narrative_ontology:measurement(mc39_orig_tr_t42, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 42, 0.33).
narrative_ontology:measurement_basis(mc39_orig_tr_t42, observed).
narrative_ontology:measurement(mc39_orig_tr_t50, magna_carta_clause_39__originalist_limitation_reading, theater_ratio, 50, 0.37).
narrative_ontology:measurement_basis(mc39_orig_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(mc39_orig_be_t0, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(mc39_orig_be_t0, observed).
narrative_ontology:measurement(mc39_orig_be_t8, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(mc39_orig_be_t8, observed).
narrative_ontology:measurement(mc39_orig_be_t16, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement_basis(mc39_orig_be_t16, observed).
narrative_ontology:measurement(mc39_orig_be_t25, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 25, 0.43).
narrative_ontology:measurement_basis(mc39_orig_be_t25, observed).
narrative_ontology:measurement(mc39_orig_be_t33, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 33, 0.45).
narrative_ontology:measurement_basis(mc39_orig_be_t33, observed).
narrative_ontology:measurement(mc39_orig_be_t42, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 42, 0.47).
narrative_ontology:measurement_basis(mc39_orig_be_t42, observed).
narrative_ontology:measurement(mc39_orig_be_t50, magna_carta_clause_39__originalist_limitation_reading, base_extractiveness, 50, 0.49).
narrative_ontology:measurement_basis(mc39_orig_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc39_orig_su_t0, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(mc39_orig_su_t0, observed).
narrative_ontology:measurement(mc39_orig_su_t8, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(mc39_orig_su_t8, observed).
narrative_ontology:measurement(mc39_orig_su_t16, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement_basis(mc39_orig_su_t16, observed).
narrative_ontology:measurement(mc39_orig_su_t25, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement_basis(mc39_orig_su_t25, observed).
narrative_ontology:measurement(mc39_orig_su_t33, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 33, 0.53).
narrative_ontology:measurement_basis(mc39_orig_su_t33, observed).
narrative_ontology:measurement(mc39_orig_su_t42, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 42, 0.62).
narrative_ontology:measurement_basis(mc39_orig_su_t42, observed).
narrative_ontology:measurement(mc39_orig_su_t50, magna_carta_clause_39__originalist_limitation_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(mc39_orig_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__originalist_limitation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__originalist_limitation_reading, feudal_prerogative_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the magna_carta_clause_39 kernel per the epsilon-invariance principle: the colloquial label 'clause 39' conflates three structurally distinct constraints. This file (originalist_limitation_reading) authors the clause as bounded to the documented 1215 grievances and the chartered free class - moderate epsilon, victim set centered on the Crown. liberal_due_process_reading authors the same words as a universal individual-rights guarantee (universal protected class, epsilon indexed to arbitrary state power generally). feudal_prerogative_reading authors them as narrow vassal-lord procedure within the established hierarchy. The upstream story (this file, highest documentary grounding in the 1215 record) influences the downstream liberal reading, which cites the clause's endurance as evidence for its universalizing claim. Each member links the others through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
