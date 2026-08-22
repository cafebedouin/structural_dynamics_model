% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Clause 39 as Feudal Privilege: Procedural Security for Free Men within the Hierarchical Order
 *   domain: constitutional/legal-historical/political
 *
 * SUMMARY:
 *   In 1215, after an armed baronial revolt, King John granted under seal
 *   that no free man would be seized, imprisoned, dispossessed, outlawed, or
 *   exiled except by the lawful judgment of his equals or the law of the
 *   land. On the feudal reading, this is a narrow procedural privilege inside
 *   the established order: the protected class is the free tenantry — barons,
 *   knights, free sokemen — a minority of a realm whose majority are unfree;
 *   'equals' means social peers; 'the law of the land' means settled feudal
 *   custom administered through the king's own courts. The crown keeps its
 *   courts, its feudal revenues, and its sovereignty, and surrenders
 *   discretionary seizure of the protected class; the barons who imposed the
 *   settlement are also its prime beneficiaries; the villeinage that makes up
 *   most of the population is untouched and unmentioned. The charter was
 *   annulled within ten weeks, refought in civil war, reissued in 1216, 1217,
 *   and definitively in 1225, and confirmed repeatedly through the century's
 *   crises down to Edward I's Confirmatio Cartarum in 1297 — an arc this
 *   story tracks from imposed settlement to entrenched, partly ceremonial
 *   privilege. KEY AGENTS (by structural relationship): - crown_of_england:
 *   primary target (institutional/constrained) — bears the settlement's
 *   costs; its prerogative of discretionary seizure over free men is what the
 *   clause takes - baronial_coalition: agenda-setter and primary beneficiary
 *   (organized/constrained) — imposed the terms at Runnymede, enforced them
 *   through the clause 61 committee, and is the prime protected class -
 *   free_tenantry: secondary beneficiary (moderate/constrained) — knights and
 *   free sokemen whose persons and holdings the guarantee secures -
 *   unfree_villeinage: excluded class (powerless/trapped) — the majority of
 *   the realm, outside the clause's words, with no seat in the arrangement
 *   that governs them - royal_justices: administrative seat
 *   (institutional/constrained) — give 'lawful judgment' and 'law of the
 *   land' their operative content case by case - papacy: external adjudicator
 *   (institutional/arbitrage) — annulled the 1215 grant, later protected the
 *   reissues; bound by none of the terms
 *
 * KEY AGENTS:
 *   - crown_of_england: primary target (institutional power, constrained exit) — bears the settlement's costs
 *   - baronial_coalition: agenda-setter and primary beneficiary (organized power, constrained exit) — imposed and enforced the settlement, and is its prime protected class
 *   - free_tenantry: secondary beneficiary (moderate power, constrained exit) — the wider protected class below the baronage
 *   - unfree_villeinage: excluded class (powerless, trapped) — the majority, outside the guarantee, with no voice in the arrangement
 *   - royal_justices: administrative seat (institutional power, constrained exit) — operationalize the law of the land
 *   - papacy: external adjudicator (institutional power, arbitrage position) — reprices its endorsement across reigns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.35).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.4).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Clause 39 as Feudal Privilege: Procedural Security for Free Men within the Hierarchical Order").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/legal-historical/political").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '3605a585-1a63-4395-884a-8c8e00485516').
narrative_ontology:cs_kernel_codification('3605a585-1a63-4395-884a-8c8e00485516', fixed_text).
narrative_ontology:cs_authority_grounding('3605a585-1a63-4395-884a-8c8e00485516', lineage).
narrative_ontology:cs_interpretation_layer_present('3605a585-1a63-4395-884a-8c8e00485516').
narrative_ontology:cs_reading_relation('3605a585-1a63-4395-884a-8c8e00485516', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('3605a585-1a63-4395-884a-8c8e00485516', magna_carta_clause_39__originalist_limitation_reading, influences).
narrative_ontology:cs_axiom('3605a585-1a63-4395-884a-8c8e00485516', foundational, protection_extends_to_free_men_only).
narrative_ontology:cs_axiom_status(protection_extends_to_free_men_only, holdable).
narrative_ontology:cs_axiom_grounding('3605a585-1a63-4395-884a-8c8e00485516', protection_extends_to_free_men_only, conventional).
narrative_ontology:cs_axiom('3605a585-1a63-4395-884a-8c8e00485516', foundational, peer_judgment_suffices_as_lawful_process).
narrative_ontology:cs_axiom_status(peer_judgment_suffices_as_lawful_process, holdable).
narrative_ontology:cs_axiom_grounding('3605a585-1a63-4395-884a-8c8e00485516', peer_judgment_suffices_as_lawful_process, conventional).
narrative_ontology:cs_reference_frame('3605a585-1a63-4395-884a-8c8e00485516', feudal_hierarchy_settlement).
narrative_ontology:cs_drift_state('3605a585-1a63-4395-884a-8c8e00485516', early_modern_common_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3605a585-1a63-4395-884a-8c8e00485516', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, baronial_coalition).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, free_tenantry).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, crown_of_england).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the charter's terms at Runnymede in 1215 after armed revolt, and enforced them through the clause 61 committee of twenty-five barons holding power of distraint. They are also the settlement's prime protected class: their persons, lands, and heirs are what the procedural guarantee secures. Their exit from the arrangement is rebellion — the costly path already traveled once — so they remain inside the order they bounded, using the charter across the century as leverage in taxation disputes and reform crises.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, baronial_coalition, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, baronial_coalition, beneficiary).

% Knights and free sokemen holding land freely of a lord, below the baronage. The guarantee of judgment by equals or the law of the land covers them, giving their holdings a security that royal and seigneurial discretion cannot touch. They serve the crown as knights and jurors and staff the local courts that give the law of the land its daily operation. Leaving the realm or their status is not a realistic option; the protection is worth more than any exit.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, free_tenantry, beneficiary,
    moderate, biographical, constrained, national).

% Grants the guarantee under seal at Runnymede and thereafter must route any action against a free man's person, lands, or liberties through lawful judgment of his equals or the law of the land. Attempts to leave the arrangement — John's repudiation and the papal annulment of 1215, renewed evasion under Henry III — met war, reissue, and political crisis, and each failure re-entrenched the settlement. The crown keeps its courts, its feudal revenues, and its sovereignty; what it surrenders is discretionary seizure of the protected class.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown_of_england, payer,
    institutional, generational, constrained, national).

% Villeins and serfs — the majority of the realm — bound to their manors and lords by unfree tenure. They had no seat at Runnymede and fall outside the clause's words: 'free man' does not name them. Their persons and holdings remain governed by manorial custom and their lord's court, with no recourse to peer judgment against seigneurial power. The settlement's operation leaves their condition exactly as it finds it; nothing in it speaks for them.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_villeinage, excluded,
    powerless, biographical, trapped, local).

% The bench and itinerant justices administering the law of the land in the king's name. They give 'lawful judgment' and 'law of the land' their operative meaning case by case, deciding in practice what process a free man is owed. They serve at the crown's pleasure and cannot rule against royal interest indefinitely, but their daily administration is what makes the guarantee real rather than declaratory.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, royal_justices, agenda_setter,
    institutional, biographical, constrained, national).

% External adjudicator of the charter's validity. Innocent III annulled the 1215 grant as extorted and derogatory to the royal dignity; later popes protected the reissued charters with sentences of excommunication against violators, repricing their support as crown-papacy relations shifted. The papacy is bound by none of the clause's terms and can move its endorsement between king and barons at will.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, papacy, observer,
    institutional, civilizational, arbitrage, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, baronial_coalition).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels royal power over free men's persons, lands, and liberties through established process — judgment by the man's equals or the law of the land — replacing ad hoc seizure with a procedure the whole free community can rely on, and giving the crown a predictable legal order in place of episodic baronial revolt.
% TRANSFER_FUNCTION: Moves security of person and property from a revocable royal indulgence into an entrenched entitlement of the free tenantry; correspondingly moves discretionary power out of the crown's hands and the compliance costs of lawful process onto royal administration.
% ABSENT_VOICES: The unfree majority — villeins and serfs, most of the realm's population — had no seat at Runnymede and are named only by the clause's silence; they would object that procedural security is rationed by status while their obligations run unqualified. The crown's prerogative view had advocates (John's appeal to Rome); the unfree had none at all.
% DISAPPEARANCE_RATIONALE: If the clause and its settlement vanished overnight in its era, seizure without judgment returns as royal practice, baronial lands and persons revert to the king's discretion, the 1215 war settlement unravels, and the realm re-enters the revolt cycle the charter was cut to end — the free community's legal order is built on it.
% FOUNDING_PROBLEM: King John's arbitrary seizure of free men — discretionary arrest, dispossession, exile, and taking of lands and castles without judgment, used for revenue and against political enemies — practiced openly in the years before 1215.
% FOUNDING_PROBLEM_CORROBORATION: The crown itself attests the practice from the paying seat: John granted the clause under seal at Runnymede, and no party to the settlement denied the seizures occurred. Monastic chroniclers outside the baronial coalition (the St Albans tradition — Roger of Wendover, Matthew Paris) recorded the seizure practice independently. The papacy's annulment of the 1215 grant attacked the manner of its imposition, not the existence of the abuses. No corroborating source claims the founding problem was already dead within the interval.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.35 is authored from this reading's own lights: the transfer is modest and class-rationed — security of person and property for the free tenantry, paid by curtailed prerogative and the compliance costs of lawful process — a fair bargain within its terms, with the asymmetry carried by the restricted beneficiary class rather than by a heavy rate. Suppression 0.40 reflects an arrangement imposed by armed coalition and enforced first through the clause 61 distraint committee, then through ordinary judicial process layered with confirmations and papal censure: coercion was decisive at imposition and at each crisis, but between crises the settlement ran as ordinary law. Theater 0.35: the clause did continuous real work through the interval — writs, pleadings, baronial leverage at 1237, 1258, and 1297 — while the confirmation ceremony layer (the charter read at coronations and parliaments) grew into partly ritual maintenance. Accessibility collapse 0.35: alternatives persist — royal evasion through other prerogative channels, appeal to Rome, the 1217 abolition of the enforcement committee — the clause bounds one channel of royal power rather than closing the field. Resistance 0.60: John repudiated the grant and obtained its annulment; civil war and French intervention followed; Henry III's administration tested the settlement repeatedly; the century's three crises each re-contested it. Claim and metrics are independent: tangled_rope is claimed from structure — a genuine coordination function (procedural security stabilizing the realm's legal order) with asymmetric extraction (benefits concentrate on the free tenantry, costs fall on the crown) held by active enforcement — while the metric values are authored as descriptive of the arrangement's actual operation, not tuned to any computed verdict. The measurement series run on one shared time grid (1215, 1225, 1237, 1253, 1258, 1265, 1297) with every tracked metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the crown's seat the arrangement is a forced surrender of traditional authority — extraction from the throne by an armed coalition, softened only by the crown's retention of everything outside the clause. From the baronial and free-tenant seats the same structure is hard-won procedural security, the minimum price of peace. From the excluded villein seat the arrangement is invisible: its protection does not reach them, its operation leaves their condition untouched, and its existence ratifies the order that binds them. The papacy's seat sees an instrument repriced across reigns rather than a settlement it belongs to. The engine computes these per-seat classifications from the power and exit data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (baronial_coalition, free_tenantry) drive low directionality for those seats — the settlement subsidizes them. The victim declaration (crown_of_england) drives high directionality: the crown is the full target, and its exit is constrained — repudiation, papal annulment, and evasion were each tried and each failed at escalating cost, so the crown sits near the full-target end rather than the mobile end. The excluded villeinage seat has no declared structural relationship to the clause's operation — it is outside the constraint's scope — so its classification rides no beneficiary or victim data; its exclusion is documented as an authored absence, commentary-grade, never a correction-grade input (R3). The papacy's arbitrage exit places it near the symmetric middle: it can reprice its endorsement either way.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — arbitrary royal seizure of free men — stayed live across the whole interval: each reign tested the settlement, and the 1258 crisis shows the problem's return when enforcement slackened, so no mandatrophy resolution is declared. The tangled_rope classification is what prevents the two adjacent mislabels: reading the clause as pure coordination would erase the class rationing and the crown-side extraction; reading it as pure extraction (a baronial device against the throne and nothing more) would erase the real, continuously used procedural function that made the settlement worth defending for a century. The theater growth in the measurement series is a symptom to watch — the confirmation ritual layer thickens after 1237 — but the function never atrophied within the interval, so the constraint is a tangled rope thickening toward ceremony, not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates one reading of the kernel magna_carta_clause_39 — the feudal_prerogative_reading. Would instantiating a sibling reading change the constraint''s victim set and epsilon structurally, or are all three readings the same constraint described at different resolutions?',
    'Comparative seat analysis across the sibling stories: if the protected class generalizes beyond free tenants (liberal_due_process_reading) or the prohibited conduct narrows to the documented 1215 abuses (originalist_limitation_reading), the victim set, directionality structure, and epsilon differ wholesale — the texts classify as different constraints, not one constraint at different precisions.',
    'If the readings are structurally distinct constraints (expected), this story''s tangled_rope classification with an elite-restricted victim set applies only to the feudal reading; the liberal sibling should compute with a far larger beneficiary class and a different extraction asymmetry, and the originalist sibling with a conduct-scoped rather than class-scoped structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of clause 39 the structural data describes — one kernel, three candidate constraints.').

omega_variable(
    free_man_boundary_attribution,
    'Is the exclusion of the unfree majority part of this constraint''s own structure (the clause''s class boundary is load-bearing for its operation), or a feature of the surrounding manorial order that this constraint merely sits inside without maintaining?',
    'Counterfactual scope test: ask whether the clause could operate identically if villeinage were abolished around it. If yes, the boundary belongs to the surrounding order and the victim set is correctly restricted to the crown; if the enforcement coalition depends on the status boundary, the exclusion is internal and belongs in the extraction accounting.',
    'If the boundary is internal, extractiveness rises and the victim set widens beyond the crown toward the excluded classes the arrangement entrenches; if external, the story stands as authored — a class-rationed privilege whose costs fall on royal prerogative alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_man_boundary_attribution, conceptual, 'Whether the free-man class boundary is the constraint''s structure or its environment''s.').

omega_variable(
    enforcement_machinery_identity,
    'Is the constraint''s enforcement the radical 1215 machinery (the clause 61 committee of twenty-five barons with power of distraint) or the normalized common-law operation that replaced it after 1217?',
    'Track which machinery actually secured compliance in each sub-period: 1215-1217 distraint and war; 1217-1258 ordinary judicial process punctuated by confirmations; 1258-1265 revived baronial enforcement; post-1297 parliamentary leverage. The suppression_requirement series encodes this arc.',
    'If the 1215 machinery is the identity, suppression is high and the imposed character dominates; if the normalized machinery is the identity, suppression is moderate and the constraint operates as ordinary law — the classification leans from imposed settlement toward entrenched legal order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_identity, empirical, 'Which enforcement generation defines the constraint''s operative suppression.').

omega_variable(
    extraction_accumulation_interpretation,
    'Does the rising extractiveness series measure growing rent-seeking layered onto the settlement, or a constant class privilege whose absolute value grew as the protected property base expanded and the crown''s fiscal needs intensified?',
    'Decompose the 1215-1297 rise into property-base growth, crown fiscal pressure, and baronial leverage events (1237, 1258, 1297); if the rise tracks leverage events rather than base growth, it is accumulation; if it tracks base growth, per-unit extraction is stable.',
    'If accumulation, the extraction-accumulation hypothesis is confirmed and the settlement is drifting toward harder extraction from the crown; if stable per-unit, the series is a valuation artifact and the constraint''s extractive character is static.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_accumulation_interpretation, empirical, 'Whether rising measured extraction is real accumulation or valuation drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1297).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc39_feudal_reading_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(mc39_feudal_reading_tr_t1225, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1225, 0.14).
narrative_ontology:measurement(mc39_feudal_reading_tr_t1237, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1237, 0.2).
narrative_ontology:measurement(mc39_feudal_reading_tr_t1253, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1253, 0.26).
narrative_ontology:measurement(mc39_feudal_reading_tr_t1258, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1258, 0.23).
narrative_ontology:measurement(mc39_feudal_reading_tr_t1265, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1265, 0.29).
narrative_ontology:measurement(mc39_feudal_reading_tr_t1297, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1297, 0.35).

% Extraction over time
narrative_ontology:measurement(mc39_feudal_reading_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.22).
narrative_ontology:measurement(mc39_feudal_reading_be_t1225, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1225, 0.24).
narrative_ontology:measurement(mc39_feudal_reading_be_t1237, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1237, 0.27).
narrative_ontology:measurement(mc39_feudal_reading_be_t1253, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1253, 0.29).
narrative_ontology:measurement(mc39_feudal_reading_be_t1258, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1258, 0.33).
narrative_ontology:measurement(mc39_feudal_reading_be_t1265, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1265, 0.35).
narrative_ontology:measurement(mc39_feudal_reading_be_t1297, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1297, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(mc39_feudal_reading_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.55).
narrative_ontology:measurement(mc39_feudal_reading_su_t1225, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1225, 0.4).
narrative_ontology:measurement(mc39_feudal_reading_su_t1237, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1237, 0.33).
narrative_ontology:measurement(mc39_feudal_reading_su_t1253, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1253, 0.36).
narrative_ontology:measurement(mc39_feudal_reading_su_t1258, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1258, 0.5).
narrative_ontology:measurement(mc39_feudal_reading_su_t1265, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1265, 0.45).
narrative_ontology:measurement(mc39_feudal_reading_su_t1297, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1297, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'clause 39' covers three structurally distinct claims and is decomposed into a three-story family: this feudal_prerogative_reading (class-rationed privilege; victim set restricted to the crown; low extractiveness against traditional authority), the liberal_due_process_reading (universal individual rights; its epsilon is assessed against universal-coverage failure), and the originalist_limitation_reading (conduct-scoped limit confined to documented 1215 abuses). The readings share a fixed text but differ in protected class, prohibited-conduct scope, and victim structure; each story carries its own epsilon and stakeholders and links its siblings here. The upstream feudal historiography underwrites the originalist sibling's narrow-scope claim (declared as an influences edge in cs_structure.reading_relations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
