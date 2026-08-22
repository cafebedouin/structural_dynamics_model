% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification of the Blood-Feud Obligation (Divine-Law Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   The standing arrangement under contest is the blood-feud obligation
 *   system of early medieval Latin Christendom: kin corporations bound by
 *   inherited duties to pursue or commute the slaughter of their members,
 *   governed by honor economy and customary composition rather than public
 *   courts. This file instantiates ONE reading of that kernel — the
 *   christianized_pacification_reading — under which the feud's defining
 *   obligation violates divine law (vengeance reserved to God, delegated to
 *   ecclesiastical and royal institutions), and the corrective arrangement
 *   concentrates in Church hands the authority to define licit violence,
 *   tariff its penance, and commute its settlements. Per the expected
 *   structural delta, every feud participant enters the victim set (the
 *   pincer: pursue vengeance and be a sinner, forbear and be dishonored), the
 *   Church enters the beneficiary set (interpretive monopoly over legitimate
 *   violence, expanded jurisdictional reach, composition flows), and
 *   suppression is pursued comprehensively through penitential discipline,
 *   anathema, and royal ban. The ε referent is fixed by the kernel-reading
 *   rule: the feud arrangement as it actually operated under this overlay,
 *   assessed by THIS reading's own lights — reading-indexed values over a
 *   stable referent (OQ-26), never the Church-ruled alternative this reading
 *   endorses. Sibling stories instantiate the other readings with different
 *   ε, beneficiaries, and victims; the family decomposition follows the
 *   ε-invariance principle, since the colloquial label 'the feud' covers
 *   structurally distinct claims. The claim/metrics independence rule is
 *   respected: tangled_rope is authored as the structural truth of the
 *   overlay arrangement, while the metrics independently report heavy, rising
 *   extraction — the engine computes per-seat types from the structural data,
 *   and any divergence from the claim is the measurement the corpus exists to
 *   take.
 *
 * KEY AGENTS:
 *   - - church_hierarchy: Primary beneficiary and agenda-setter (institutional/arbitrage) — defines vengeance as sin, administers penitential remedies, convenes peace councils, collects composition shares and expiatory endowments
 *   - - royal_adjudicative_officials: Secondary beneficiary and co-enforcer (powerful/mobile) — converts feud liabilities into fines and court jurisdiction, building the fiscal-judicial machine
 *   - - feud_bound_kin_groups: Primary target (organized/identity_locked) — bound to avenge or accept composition, condemned either way, with honor identity making refusal unthinkable
 *   - - missionized_frontier_kin_groups: Coerced target (organized/trapped) — frontier lineages whose customary feud law was condemned as paganism under conquest
 *   - - monastic_estates: Beneficiary (institutional/constrained) — immune estates receiving composition shares and expiatory gifts; principal doctrinal advocates
 *   - - protected_peasant_communities: Incidental beneficiary without agenda seat (powerless/trapped) — shielded by peace legislation they did not write
 *   - - historical_anthropology_observers: Analytical observer (analytical/analytical) — cross-cultural comparison attesting the family decomposition from outside the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.78).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.75).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification of the Blood-Feud Obligation (Divine-Law Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, 'f8fe31d7-183e-434c-af2b-64c5cb30a1ca').
narrative_ontology:cs_kernel_codification('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', fixed_text).
narrative_ontology:cs_authority_grounding('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', lineage).
narrative_ontology:cs_interpretation_layer_present('f8fe31d7-183e-434c-af2b-64c5cb30a1ca').
narrative_ontology:cs_reading_relation('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', foundational, vengeance_reserved_to_god_alone).
narrative_ontology:cs_axiom_status(vengeance_reserved_to_god_alone, holdable).
narrative_ontology:cs_axiom_grounding('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', vengeance_reserved_to_god_alone, theological).
narrative_ontology:cs_axiom('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', foundational, legitimate_violence_via_sacred_delegation).
narrative_ontology:cs_axiom_status(legitimate_violence_via_sacred_delegation, holdable).
narrative_ontology:cs_axiom_grounding('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', legitimate_violence_via_sacred_delegation, conventional).
narrative_ontology:cs_reference_frame('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', divine_violence_monopoly_order).
narrative_ontology:cs_drift_state('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', twelfth_century_canonist_survey, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f8fe31d7-183e-434c-af2b-64c5cb30a1ca', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_adjudicative_officials).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, monastic_estates).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, protected_peasant_communities).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_bound_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, missionized_frontier_kin_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines which killings are sins and which are licit, tariffs the penance owed for each, and holds the keys to absolution. Convenes peace councils that anathematize feuders, charters protected persons and places, and registers the compositions by which blood-debt is commuted into payment — a large share of which flows to episcopal courts, churches, and monasteries. Its jurisdiction over marriage, burial, and oath-taking sits inside every feud's lifecycle, so nearly every settlement passes through its offices. Its personnel and claims move across kingdoms, playing rival rulers against one another.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Kings, dukes, and their officers legislate the ban on private war inside their territories, summon disputants to their courts, and take fines, reliefs, and court dues wherever feud settlements are registered. Each feud brought under the ban is a fine collected and a jurisdiction captured; the peace edicts build a fiscal-judicial machine that outlives any single dynasty. Their enforcement capacity depends on ecclesiastical legitimation, and their practice alternates between suppressing the feud and licensing aristocratic violence as sanctioned military service.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_adjudicative_officials, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_adjudicative_officials, agenda_setter).

% Hold charters of immunity placing their lands, tenants, and treasures outside legitimate raiding, receive shares of compositions and expiatory gifts endowed for the souls of killers and slain alike, and staff the scriptoria in which the anti-vengeance doctrine is copied and taught. Their advocacy for the peace legislation is constant; their direct exposure is limited to the occasional robber-knight, and their estates are anchored to particular regions they cannot move.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, monastic_estates, beneficiary,
    institutional, generational, constrained, regional).

% Farmers, craftsmen, and merchants named under the peace legislation as persons who may neither be attacked nor made to answer for a kinsman's blood. They gain shelter from raids that once consumed their harvests as feud collateral, swear the collective peace oaths when summoned, pay tithes and dues like everyone else, and hold no seat where the terms of their protection are written. Leaving the land is rarely survivable.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, protected_peasant_communities, beneficiary,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, protected_peasant_communities, excluded).

% Lineage corporations whose adult men inherit standing duties: pursue the slayer of a kinsman or negotiate his composition, host and safeguard the truce meetings, and answer corporately for any member's breach. Refusal to pursue brands a man cowardly and unmarriageable within his own law; pursuit now brands him a sinner before the Church and an outlaw before the growing royal courts, whichever way he turns. Settlement payments route through ecclesiastical hands and royal fines take their share of the remainder. The duty is constitutive of who they are — a man who abandons it abandons his name — and it passes from father to son irrespective of anyone's preference.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_bound_kin_groups, payer,
    organized, generational, identity_locked, regional).

% Frontier peoples incorporated by conquest and baptism within the same generation — Saxon clans living under Carolingian capitulary law, Scandinavian lineages under missionary kings — whose customary feud procedure is condemned as heathen remnant and whose adherence to it is punished as rebellion. Their choice set is conversion together with renunciation of the blood-duty, or death; their assemblies are dissolved and their customary law is replaced by written capitularies they did not draft.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, missionized_frontier_kin_groups, payer,
    organized, generational, trapped, regional).

% Comparative historians and anthropologists reading charter evidence, conciliar canons, saga literature, and legal compilations across regions and centuries. They reconstruct what the feud system did before, alongside, and despite the pacification overlay, and they hold no stake in any party's salvation or revenue. Their seat sits entirely outside the interval.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, historical_anthropology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a supralocal framework for containing lethal private violence where no state enforcement exists: liturgically fixed truce windows during which fighting is suspended, sanctuary and protected-person statuses that remove whole classes from legitimate targeting, standardized composition tariffs that give disputes a foreseeable endpoint, and church-mediated settlement backed by sanctions (exclusion from sacraments, anathema) that bite where sheriffs do not.
% TRANSFER_FUNCTION: Moves material wealth — composition shares, court fines, penitential tariffs, expiatory endowments — from feud-bearing lineages to ecclesiastical and royal coffers; moves the authority to define and license violence from kin corporations to altar and throne; and moves spiritual risk onto the participants themselves (damnation and penance liability), with absolution sold back through the same offices that impose it.
% ABSENT_VOICES: The feud-bearing lineages had no seat at the councils that defined them as sinners: peace legislation spoke in their name without them. Widows administering compositions, the keepers of oral customary law, and the conquered frontier peoples entered the record only as objects of canons and capitularies. They are located inside the very obligations being legislated away, or outside the literate record altogether.
% DISAPPEARANCE_RATIONALE: If the pacification overlay vanished overnight, kin corporations would resume self-governed vengeance and composition under their own law — exactly what recurred wherever enforcement lapsed, as after the Carolingian fragmentation — while church and royal composition revenues, court jurisdictions, and the sacral monopoly on licensing violence would collapse together. Centuries of European institutional development routed through this arrangement would reroute.
% FOUNDING_PROBLEM: Unchecked cyclical killing in societies without centralized enforcement: retaliatory chains that consumed generations, sanctuaries and noncombatants destroyed as feud collateral, and — for the Church specifically — the theological emergency of a violently honor-bound ruling class converting en masse under a scripture that reserves vengeance to God alone.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set, modern historiography — charter studies of royal and episcopal court activity, regional feud-incidence reconstructions — attests that public adjudication was functioning in the core regions by the interval's end, so the founding problem of justice-without-public-authority had receded precisely where the pacification apparatus was strongest. No contemporary voice outside the Church-royal axis attests the problem's persistence in those cores; the continuing chaos-rhetoric survives chiefly in conciliar canons authored by the beneficiaries. At the periphery the problem demonstrably remained live longer, which is why the status is authored as dead-on-the-core rather than uniformly.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 at interval end) because this reading counts the whole pincer as levied cost: spiritual peril attaches to every permissible move, compositions route through beneficiary hands, and the indigenous justice system is delegitimized rather than merely regulated. Suppression (0.75) is authored as the raw structural force of the enforcement machinery — anathema, interdict, royal ban, penitential surveillance — and is deliberately NOT scaled here; only extractiveness is scaled by directionality and scope downstream. Theater (0.31) is moderate: the dispute-processing and truce functions were real, but a growing share of activity became ritualized performance (public penances, staged submissions, renewed anathemas against practices that persisted unchanged). Accessibility_collapse is LOW (0.35): the alternative — unmediated feud under customary law — never collapsed; it persisted defiantly for centuries in core and periphery alike, which is itself diagnostic that this is a defended construct rather than a natural limit. Resistance is HIGH (0.70): peace councils required serial reissuance because compliance repeatedly collapsed, and royal bans succeeded only where royal power did. The measurement series run on ONE shared seven-point grid so every tracked metric is authored at every examined time point. suppression_requirement is tracked (rather than left static) because the story's enforcement history is one of visible capacity-building: advisory penitential tariffs gave way to anathematizing Peace councils, then to papal-legatine and royal-canon fused enforcement — a rising trajectory modeling enforcement maturation, not extraction drift. Rising base_extractiveness over the interval additionally supplies T17-compatible accumulation evidence for investigation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural asymmetry explains why. From the church_hierarchy and royal seats, the arrangement is a peace-order they administer and staff: truce windows, settled compositions, protected classes — a functioning legal revolution they built. From the feud_bound_kin_group seats, the same structure operates as a double bind administered by outsiders: their own law criminalized, their settlements taxed twice (ecclesiastical composition, royal fine), their souls charged for conduct their honor makes unavoidable. Protected_peasant_communities experience a third surface again: genuine physical shelter purchased with other people's condemnation and paid for in tithes they owed regardless. The engine computes these divergent per-seat types from power, exit, and role data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly onto structural relationships. church_hierarchy collects the interpretive and material rents and controls the rules — nearest the beneficiary end. royal_adjudicative_officials collect fines and capture jurisdiction while sharing enforcement labor — low d, slightly above the Church's. monastic_estates collect endowments under immunity — low d. protected_peasant_communities receive subsidized protection — nearest the subsidy end. feud_bound_kin_groups bear the entire transfer stack with identity-locked exit (honor constitutes the person; the obligation is inherited) — nearest the full-target end. missionized_frontier_kin_groups bear the same stack under conquest with no exit at all — likewise near full-target. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations, power atoms, and exit options produces the correct relationships for every seat, so overriding would only obscure the derivation the engine owns.
 *
 * MANDATROPHY ANALYSIS:
 *   Typing this arrangement tangled_rope prevents mislabeling in both directions. A pure-snare collapse would erase the demonstrable coordination content — truce windows that measurably suspended fighting, composition tariffs that ended escalations, protected-class statuses that emptied feud's collateral damage — while a rope-wash would erase the fact that the peace-talk funded itself on the condemned, that enforcement tracked wealth and jurisdictional opportunity, and that the victim set included everyone the arrangement governed. The R5 interview sharpens the picture: the founding problem (justice where no public authority exists) receded in the cores by interval end while the apparatus persisted and its extraction kept rising — the founding_problem_status=dead x disappearance_verdict=world_rearranges mismatch should fire the capture/zombie flag, cross-checked against the rising theater and extraction series. mandatrophy_resolved is declared true accordingly: the mandate outlived its necessity and the arrangement persisted on beneficiary maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one of three readings of the feud_obligation_kernel. The siblings — stateless_coordination_reading (self-enforcing justice and deterrence absent centralized enforcement) and extraction_cycle_reading (a destructive loop depleting productive capacity and blocking territorial consolidation) — instantiate different constraints with different epsilon, beneficiary, and victim structures. Where in the kernel''s structure does the disagreement sit, and which structural facts would move classification between readings?',
    'Compile the three sibling stories and compare per-seat classifications on shared time grids. The disputed element is the legitimacy status of kin-reciprocal violence: coordination service (stateless reading), compulsive liability (cycle reading), or divine-law transgression requiring sacral management (this reading). Whichever legitimacy premise a corpus adopts determines which file''s structural data governs.',
    'Under the stateless_coordination_reading the structure inverts: lineage disputants become net beneficiaries, no Church rent appears, and epsilon falls sharply toward coordination-cost levels. Under the extraction_cycle_reading the Church drops out of the beneficiary set entirely and victims narrow to productive households. The high epsilon and Church-centered beneficiary set authored here hold only on this reading''s premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Kernel-contest routing: one of three readings; sibling adoption would restructure beneficiaries, victims, and epsilon.').

omega_variable(
    spiritual_cost_indexability,
    'The reading counts spiritual peril — damnation risk, penance liability, sacramental exclusion — as extraction. Does that counting travel to seats that do not share the reading''s theology, or does epsilon depend on accepting the theological ledger?',
    'Recompute the arrangement''s effective extraction with spiritual-cost terms removed and retain only material flows (compositions, fines, tariffs, endowments); compare the resulting classification across the corpus.',
    'If spiritual costs are discounted, epsilon falls materially — the pincer''s largest single contribution disappears — and the arrangement drifts toward a lower-extraction coordination profile closer to the sibling readings'' territory. The authored 0.78 is indexed to the theological ledger being accepted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_cost_indexability, conceptual, 'Whether the reading-indexed epsilon survives translation to non-theological seats.').

omega_variable(
    enforcement_selectivity,
    'Was enforcement of the vengeance-prohibition allocated by salvific concern or by yield — did anathema, penitential pressure, and judicial attention track offender wealth and disputable assets rather than offense severity?',
    'Compare conciliar anathema frequency, royal fine schedules, and surviving composition records across strata and regions: intensity proportional to offender wealth indicates rent-tracking; uniform intensity across poor and rich kin groups indicates a sincere moral campaign.',
    'Strong rent-tracking evidence shrinks the credible coordination component and pushes computed seats toward snare-side classification; uniform enforcement supports the tangled_rope claim with genuine coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_selectivity, empirical, 'Whether suppression effort tracked souls or revenue.').

omega_variable(
    peripheral_necessity,
    'In regions lacking functioning royal or canonical courts throughout the interval, did feud obligation remain the only working justice mechanism — making suppression there a removal of necessary coordination rather than rent-taking?',
    'Regional panel comparing court availability (itinerant justices, episcopal tribunals, chartered town jurisdictions) against feud incidence and composition uptake; regions where feud persisted despite available courts isolate identity- and preference-driven persistence from necessity.',
    'Where courts were genuinely unavailable, part of the measured extraction is transitional coordination cost with scaffold character; where courts existed and feud persisted anyway, persistence reflects identity lock rather than necessity, and the full extraction accounting stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peripheral_necessity, empirical, 'Whether suppressed feud was chosen or was the only justice available.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 550, 1150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_pacification_tr_t550, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 550, 0.15).
narrative_ontology:measurement(feud_pacification_tr_t650, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 650, 0.18).
narrative_ontology:measurement(feud_pacification_tr_t750, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 750, 0.2).
narrative_ontology:measurement(feud_pacification_tr_t850, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 850, 0.23).
narrative_ontology:measurement(feud_pacification_tr_t950, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 950, 0.27).
narrative_ontology:measurement(feud_pacification_tr_t1050, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1050, 0.3).
narrative_ontology:measurement(feud_pacification_tr_t1150, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1150, 0.31).

% Extraction over time
narrative_ontology:measurement(feud_pacification_be_t550, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 550, 0.55).
narrative_ontology:measurement(feud_pacification_be_t650, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 650, 0.58).
narrative_ontology:measurement(feud_pacification_be_t750, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 750, 0.62).
narrative_ontology:measurement(feud_pacification_be_t850, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 850, 0.66).
narrative_ontology:measurement(feud_pacification_be_t950, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 950, 0.72).
narrative_ontology:measurement(feud_pacification_be_t1050, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1050, 0.76).
narrative_ontology:measurement(feud_pacification_be_t1150, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1150, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(feud_pacification_su_t550, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 550, 0.45).
narrative_ontology:measurement(feud_pacification_su_t650, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 650, 0.5).
narrative_ontology:measurement(feud_pacification_su_t750, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 750, 0.57).
narrative_ontology:measurement(feud_pacification_su_t850, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 850, 0.61).
narrative_ontology:measurement(feud_pacification_su_t950, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 950, 0.67).
narrative_ontology:measurement(feud_pacification_su_t1050, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1050, 0.73).
narrative_ontology:measurement(feud_pacification_su_t1150, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1150, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, papal_crusade_violence_authorization).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the blood feud' covers at least three structurally distinct claims, authored as three files sharing the kernel feud_obligation_kernel. feud_obligation_kernel__stateless_coordination_reading is the upstream baseline (describes the system's function in stateless environments, low epsilon, rope-shaped); this reading and feud_obligation_kernel__extraction_cycle_reading are downstream reactions to it. This reading creates structural downstream pressure on both siblings without resolving their dispute: its suppression campaign raised the feud's operating costs (feeding the cycle reading's depletion dynamics) and redirected surplus aristocratic violence into externally licensed warfare (see papal_crusade_violence_authorization), while leaving the coordination reading empirically intact wherever state capacity failed to arrive. Each file carries its own epsilon, beneficiaries, and victims; linking edges propagate contamination analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
