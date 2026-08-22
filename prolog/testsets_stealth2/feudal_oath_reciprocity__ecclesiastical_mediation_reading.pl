% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Charity-Bound Sacramental Feudal Oath (Ecclesiastical Mediation Reading)
 *   domain: medieval political economy / legal history / institutional analysis
 *
 * SUMMARY:
 *   Within Latin Christendom c. 950-1250, the feudal oath was sworn on relics
 *   with God as witness, and the church held that an oath sworn against
 *   charity was no oath but perjury: bishops heard oath disputes, penitential
 *   machinery imposed satisfaction, and excommunication and interdict gave
 *   the charity limits coercive teeth against armed elites. This story
 *   instantiates the ecclesiastical_mediation_reading of the
 *   feudal_oath_reciprocity kernel: an arrangement that genuinely coordinates
 *   elite reciprocity — fidelity, protection-for-service, and dispute
 *   arbitration no state then provided — while extracting asymmetrically
 *   through the same structure: unfree rural households bear manorial burdens
 *   the charity limits barely reach, and the church converts its interpretive
 *   office into tithe income, court fees, and jurisdiction over the very
 *   elites it binds. Under this reading the arrangement is a hybrid of
 *   coordination and extraction held in place by active ecclesiastical
 *   enforcement, which is why the claimed type is tangled_rope and why the
 *   sibling readings (which see the charity layer as cover, or see charter
 *   text as the real enforcement) would compute different constraints
 *   entirely.
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: agenda-setting interpreter (institutional/arbitrage) — administers the charity limits, collects tithes and jurisdiction fees; the seat the arrangement's gains accrue to
 *   - secular_lords: coordinated-and-constrained party (powerful/identity_locked) — gains enforceable vassal fidelity and legitimated authority, pays foregone extractive discretion and jurisdictional subordination
 *   - lesser_vassals: coordinated party with costs (moderate/constrained) — holds enforceable protection claims, owes service and counsel
 *   - peasant_communities: protected-but-seatless party (organized/constrained) — gains a moral vocabulary and occasional episcopal forum, holds no seat where oaths are sworn
 *   - serf_households: primary burden-bearers (powerless/trapped) — bound to the manor by custom, covered least by the charity limits
 *   - cottar_laborers: residual burden-bearers (powerless/trapped) — heaviest burdens relative to substance, thinnest protection
 *   - jewish_lending_communities: excluded outside the sacramental frame (moderate/constrained) — protected by separate charters, not by charity-mediated reciprocity
 *   - canon_law_doctors: analytical observer (institutional/analytical) — theorize when oaths bind and when charity releases; see the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.5).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Charity-Bound Sacramental Feudal Oath (Ecclesiastical Mediation Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval political economy / legal history / institutional analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, '2983884f-7ab3-4f83-ac80-a1e961de7ebc').
narrative_ontology:cs_kernel_codification('2983884f-7ab3-4f83-ac80-a1e961de7ebc', formalized).
narrative_ontology:cs_authority_grounding('2983884f-7ab3-4f83-ac80-a1e961de7ebc', lineage).
narrative_ontology:cs_interpretation_layer_present('2983884f-7ab3-4f83-ac80-a1e961de7ebc').
narrative_ontology:cs_reading_relation('2983884f-7ab3-4f83-ac80-a1e961de7ebc', feudal_oath_reciprocity__lord_extraction_reading, forecloses).
narrative_ontology:cs_reading_relation('2983884f-7ab3-4f83-ac80-a1e961de7ebc', feudal_oath_reciprocity__vassal_coordination_reading, influences).
narrative_ontology:cs_axiom('2983884f-7ab3-4f83-ac80-a1e961de7ebc', foundational, oath_binding_measure_is_charity).
narrative_ontology:cs_axiom_status(oath_binding_measure_is_charity, holdable).
narrative_ontology:cs_axiom_grounding('2983884f-7ab3-4f83-ac80-a1e961de7ebc', oath_binding_measure_is_charity, theological).
narrative_ontology:cs_axiom('2983884f-7ab3-4f83-ac80-a1e961de7ebc', secondary, perjury_draws_divine_sanction).
narrative_ontology:cs_axiom_status(perjury_draws_divine_sanction, holdable).
narrative_ontology:cs_axiom_grounding('2983884f-7ab3-4f83-ac80-a1e961de7ebc', perjury_draws_divine_sanction, theological).
narrative_ontology:cs_reference_frame('2983884f-7ab3-4f83-ac80-a1e961de7ebc', caritas_bounded_sacramental_reciprocity).
narrative_ontology:cs_drift_state('2983884f-7ab3-4f83-ac80-a1e961de7ebc', high_medieval_royal_justice_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2983884f-7ab3-4f83-ac80-a1e961de7ebc', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasant_communities).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, serf_households).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, cottar_laborers).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_efficacy).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_binding_and_losing_authority).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, charity_as_measure_of_lordly_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, abbots, and papal judges-delegate adjudicate when an oath binds and when charity releases it, operate penitential and excommunication machinery, and hear oath disputes in ecclesiastical courts. Tithes, court fees, and the interpretive office itself flow to this seat, and its enforcement reach spans Latin Christendom; when a local ruler turns hostile it can shift courts, play princes against one another, and appeal over their heads to Rome.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy, beneficiary).

% Hold land and command vassal service under oaths sworn on relics with God as witness. The charity binding secures their vassals' fidelity, brands their lordship legitimate, and gives them a forum for disputes; the same binding caps their takings from tenants, exposes them to excommunication and episcopal courts, and obliges restraint and protection they would not otherwise practice. Their authority is constituted inside the sacramental order, so renouncing the frame would dissolve their own legitimacy; defiance of particular church demands is common, exit from the frame is not.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, beneficiary,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer).

% Owe mounted service and counsel for their fiefs and hold, through the same sworn bond, enforceable claims to their lord's protection and fair dealing. The charity limits give them grounds to resist confiscatory takings in ecclesiastical forums, but they remain bound to serve, often hold of several lords with divided loyalties, and cannot relinquish a fief without ruin. Staying means paying service and accepting their lord's conduct within the limits; leaving means surrendering land and status.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, lesser_vassals, payer).

% Village communities of free and semi-free tenantry hold their tenures under the protection the oath-chain is meant to guarantee. Charity limits give them a moral vocabulary and an occasional episcopal forum against private war, castle-building, and confiscatory tallage, and some communities bargain charters and commuted labor services. They also pay tithes and residual dues. Their protection depends on lords honoring the frame, and they hold no seat where oaths are sworn.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peasant_communities, beneficiary,
    organized, generational, constrained, local).

% Unfree households tied to a lord's manor owe week-work, merchet, tallage, and court amercements by custom rather than by any oath they swore as parties. The charity limits reach them mostly as pulpit instruction, and episcopal relief is sporadic. Flight to a town or another lord's land is the only exit and carries forfeiture and recapture under pursuit warrants.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, serf_households, payer,
    powerless, generational, trapped, local).

% Landless or smallholding laborers at the bottom of the manor hold a cottage and a few acres for seasonal work, bear the heaviest burdens relative to their substance, and are covered by the protection framework least of all. They have no tenure security, no forum, and no exit that does not begin with destitution.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, cottar_laborers, payer,
    powerless, immediate, trapped, local).

% Jewish communities living under royal or seigneurial protection stand outside the sacramental frame: they cannot swear Christian oaths, are not covered by charity-mediated reciprocity, and hold whatever security separate charters and cash tallage arrangements give them. They would object that the binding protects only those inside the baptismal order while lordship finances itself partly through their exclusion.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, jewish_lending_communities, excluded,
    moderate, biographical, constrained, regional).

% Decretists and decretalists at Bologna, Paris, and the episcopal schools theorize when oaths bind, when equity or charity releases them, and where ecclesiastical jurisdiction over feudal disputes ends. They see the whole structure — sacramental sanction, charity limits, seigneurial interest — and write the distinctions the courts later apply. Their seat is analytical: they collect no dues and bear no service.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canon_law_doctors, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__ecclesiastical_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the enforcement problem of feudal reciprocity in a world without state courts: mutual fidelity between lords and vassals, protection in exchange for service, and a cap on exaction, made credible by sacramental sanction — perjury, excommunication, denial of burial — rather than by police.
% TRANSFER_FUNCTION: Moves labor services, renders, military service, tithes, and court fees upward from peasant and vassal households to lords and the church; moves land tenure, protection, and dispute arbitration downward from lords; and moves interpretive authority over when oaths bind and when they release to ecclesiastical courts.
% ABSENT_VOICES: Unfree peasants bound by the arrangement never swore as parties — their obligations were customary, and their objections reached ecclesiastical forums only sporadically. Women holding or owing feudal tenures were largely absent from oath-making. Jewish communities under seigneurial protection stood wholly outside the sacramental frame. All three would object that the charity binding protects those inside the baptismal oath-chain and leaves everyone else to separate, weaker instruments.
% DISAPPEARANCE_RATIONALE: If the charity binding and its sacramental enforcement vanished overnight, oath enforcement would fall back to raw power ratios: lords would take to the limit of vassal and peasant resistance, private war and adulterine castle-building would expand, vassal service would become unreliable, and the church would lose its principal jurisdictional lever over secular elites. The enforcement architecture of the period's entire order depends on it.
% FOUNDING_PROBLEM: After Carolingian public authority fragmented, fidelity between armed elites — and between elites and the ruled — had no enforcer: no state courts, no police, only personal bonds. The arrangement was built to make those bonds enforceable by attaching divine sanction and ecclesiastical adjudication to the oath.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: royal chancery records show crowns building parallel oath-enforcement machinery precisely because ecclesiastical enforcement alone did not suffice, attesting both the problem's liveness and the framework's limits; and the seigneurial resistance record — lords contesting the limits where they had teeth — attests the binding was a real constraint rather than pure rhetoric. Monastic chronicles corroborate the problem's reality but sit inside the church's interest and are discounted accordingly. No attesting seat exists for the serfs and excluded communities the framework covers least.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55 at interval end) because the charity binding caps elite predation and channels dispute resolution, yet leaves the manorial burden structure intact and routes tithes, court fees, and interpretive rents upward; taking is real but bounded, not the open-ended extraction of a pure snare. Suppression (0.50) reflects real coercive machinery — excommunication, interdict, compulsory penance, denial of burial — aimed at elite conduct and dependent on sacramental belief; it is targeted rather than pervasive. Theater_ratio (0.30): charity rhetoric was invoked selectively, absolutions were negotiated, and ceremonial excommunications were sometimes lifted for payment, but the machinery had teeth (it deposed emperors and voided oaths), so only a minority of activity is performance. Accessibility_collapse (0.42) is moderate: for insiders the sacramental frame forecloses exit — an oath sworn on relics cannot be renounced without perjury — but alternative institution-space persisted in secular customary law, charter text, royal courts, and plain defiance, so alternatives are partly, not completely, collapsed. Resistance (0.55) is sustained elite resistance: the investiture contest, jurisdictional conflicts over oath disputes, and lords shopping between ecclesiastical and royal forums. The claim and the metrics are authored independently: tangled_rope is claimed from the structure (genuine coordination function plus asymmetric extraction plus active enforcement), and each metric is authored from the arrangement's observed operation without tuning toward a predicted engine verdict. The suppression_requirement series is authored because enforcement capacity is the tracked dynamic of this interval: it builds through the Gregorian reform and the classical canon-law period, peaks around T=200, then partially migrates to royal justice. The extractiveness series rises through the seigneurial expansion of the eleventh and twelfth centuries — the charity limits bound elite-vs-elite and elite-vs-church conduct more tightly than lord-vs-peasant conduct — then eases as commutation, municipal charters, and royal law spread.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the ecclesiastical seat the arrangement is coordination it administers at real cost: enforcement expenditure, martyrs, and jurisdictional war with emperors and kings. From the secular lord's seat the same structure is dual — the binding secures vassal fidelity and legitimates lordship, while the charity limits confiscate extractive discretion; lords contest particular church demands from inside the Christian identity rather than exiting the frame, which is the structural signature of identity lock: a lord's authority is constituted by the sacramental order, so apostasy is not a live exit even for the defiant, and the classification would change only if the identity frame broke (a lord who renounced the sacramental basis of his own legitimacy would exit into mere warlordship, and the binding would lose its grip on him entirely). From the vassal seat the binding approaches pure coordination: enforceable protection is what their service purchases. From the serf and cottar seats the charity binding is mostly pulpit rhetoric while manorial burdens are immediate and inescapable — from those seats the arrangement computes nearer a snare with occasional ecclesiastical relief. The engine derives this divergence from role, power, and exit data; this story authors the structure and does not adjudicate the per-seat verdicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ecclesiastical_hierarchy, lesser_vassals, peasant_communities) derive directionality toward the beneficiary end: the arrangement subsidizes them with interpretive rents, enforceable protection claims, and a moral vocabulary for resistance. Victims (serf_households, cottar_laborers, secular_lords) derive toward the target end: unfree households bear the residual burdens with trapped exit; lords bear constrained discretion. Continental spatial scope amplifies effective extraction modestly by making verification of lordly conduct harder. One directionality override is declared: secular_lords appear in the victims array, which would derive near-full-target directionality, but their position is genuinely dual — the same binding that caps their takings also secures their vassals' fidelity and brands their authority legitimate — so d is overridden to 0.5, near symmetric. The ecclesiastical seat needs no override: agenda-setter plus beneficiary derives low d, which matches its position as the seat the gains demonstrably accrue to. Suppression is authored as a raw structural property of the enforcement machinery and is left unscaled; only extractiveness is scaled by directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two misreadings. Reading the arrangement as pure coordination would erase the asymmetric extraction: serf and cottar households bear manorial burdens without proportionate protection, and the church converts its interpretive office into concentrated gains. Reading it as pure extraction would erase the genuine coordination: elite fidelity, protection-for-service, and oath-dispute arbitration that no other institution in the interval supplied and that lords themselves relied upon. On mandatrophy: the founding problem — enforcing reciprocity between armed elites, and between elites and the ruled, once public authority had fragmented — was live across the whole interval, so the mandate has not outlived its function here and mandatrophy is not resolved. The drift series records the seed of a future mandatrophy finding: enforcement migrates toward royal justice at the interval's end, which is where the arrangement's function would eventually be superseded — a development outside this story's interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_omega,
    'This story instantiates the ecclesiastical_mediation_reading of the feudal_oath_reciprocity kernel; would instantiating the lord_extraction_reading (the oath authorizes taking to the limit of vassal service capacity, charity being ritual cover) or the vassal_coordination_reading (the oath fixes bounded reciprocal obligations enforced by charter text) instead yield a different epsilon and type?',
    'Author the sibling readings as separate constraint stories and compare engine-computed classifications; divergence in epsilon across readings localizes the disagreement in what measures the oath''s binding force — charity, service capacity, or charter text.',
    'Under the lord_extraction_reading epsilon would be high and the type would drift snare-ward; under the vassal_coordination_reading epsilon would be low-moderate and the type rope-ward. This story''s moderate tangled_rope verdict is valid only within the ecclesiastical mediation frame and must not be read as a verdict on the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_omega, conceptual, 'Kernel-reading indexicality: epsilon and type are relative to the ecclesiastical_mediation_reading of the feudal_oath_reciprocity kernel.').

omega_variable(
    charity_binding_effectiveness,
    'Were the charity and sacramental limits functionally binding on lordly conduct, or primarily rhetorical cover that lords honored when convenient and ignored when costly?',
    'Compare excommunication and interdict records against actual restitution and subsequent conduct; measure whether oath-dispute outcomes in ecclesiastical courts changed lordly takings from tenants, using manorial account rolls as the conduct series.',
    'If the limits were mostly rhetorical, this reading''s epsilon is understated and the arrangement''s structure converges toward the lord_extraction_reading; if binding, the tangled_rope classification holds with the church as a genuine limiting power on secular elites.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charity_binding_effectiveness, empirical, 'Functional versus rhetorical force of the charity binding on elite conduct.').

omega_variable(
    interpretive_authority_rent,
    'Is the church''s interpretive authority over oaths a coordination service priced near its cost, or a rent stream that concentrates the arrangement''s gains in the ecclesiastical seat?',
    'Compare the operating cost of penitential and judicial machinery attributable to oath jurisdiction against tithe income, court fees, and composition payments flowing to ecclesiastical coffers from the same jurisdiction.',
    'If rents materially exceed coordination costs, the ecclesiastical seat''s directionality rises toward the target end, the arrangement''s gains concentrate in one capturer, and the receipt-surface reading of this story must be revised; if near cost, the mediation reads as priced coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_rent, empirical, 'Whether ecclesiastical interpretive authority is priced coordination or concentrated rent.').

omega_variable(
    serf_coverage_gap,
    'Did the charity limits'' protection extend down to unfree rural households, or only along the elite oath-chain of lords, vassals, and church?',
    'Compare manorial court records and penitential literature: did ecclesiastical forums hear unfree tenants'' complaints against lords, with what frequency and what outcomes, relative to disputes between sworn elites?',
    'If protection stopped at the elite chain, the arrangement is a bounded hybrid for elites and a near-pure extraction structure for serfs — the story would need decomposition into an elite-level and a village-level constraint with materially different epsilon values, linked by network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(serf_coverage_gap, empirical, 'Vertical reach of the charity limits: elite oath-chain versus unfree tenantry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_eccles_med_tr_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_tr_t0, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_tr_t50, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_tr_t50, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_tr_t100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 100, 0.23).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_tr_t100, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_tr_t150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 150, 0.26).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_tr_t150, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_tr_t200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 200, 0.28).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_tr_t200, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_tr_t250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 250, 0.29).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_tr_t250, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_tr_t300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 300, 0.3).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_tr_t300, observed).

% Extraction over time
narrative_ontology:measurement(feudal_oath_eccles_med_be_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_be_t0, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_be_t50, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_be_t50, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_be_t100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 100, 0.57).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_be_t100, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_be_t150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 150, 0.58).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_be_t150, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_be_t200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 200, 0.57).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_be_t200, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_be_t250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 250, 0.56).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_be_t250, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_be_t300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 300, 0.55).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_be_t300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_eccles_med_su_t0, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0, 0.26).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_su_t0, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_su_t50, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_su_t50, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_su_t100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 100, 0.46).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_su_t100, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_su_t150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 150, 0.53).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_su_t150, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_su_t200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 200, 0.54).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_su_t200, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_su_t250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 250, 0.52).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_su_t250, observed).
narrative_ontology:measurement(feudal_oath_eccles_med_su_t300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 300, 0.5).
narrative_ontology:measurement_basis(feudal_oath_eccles_med_su_t300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, peace_of_god_legislation).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'feudal oath reciprocity' decomposes into three readings with different epsilon over the same ritual kernel. This story (ecclesiastical_mediation_reading, moderate epsilon, tangled_rope) links to lord_extraction_reading (high epsilon — the charity layer read as ritual cover over capacity-bounded taking) and vassal_coordination_reading (low-moderate epsilon — charter-text enforcement of fixed reciprocal obligations). The ecclesiastical reading is upstream of the charter reading: canon law's power to void oaths sworn against charity pushed charter practice toward precise written terms and toward royal enforcement as sacramental enforcement became contestable. The peace_of_god_legislation edge records the church's legislative instrument for the same charity-binding project.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
