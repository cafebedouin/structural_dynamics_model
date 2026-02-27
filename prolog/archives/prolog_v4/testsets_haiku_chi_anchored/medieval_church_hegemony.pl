% ============================================================================
% CONSTRAINT STORY: medieval_church_hegemony
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medieval_church_hegemony, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: medieval_church_hegemony
 *   human_readable: The Medieval Ecclesiastical Hegemony
 *   domain: religious/economic/political
 *
 * SUMMARY:
 *   The Medieval Ecclesiastical Hegemony represents one of history's most
 *   comprehensive constraint systems, controlling access to salvation through
 *   three primary mechanisms: (1) mandatory tithing (10% wealth transfer),
 *   (2) Latin-only liturgy (preventing direct scriptural access), and (3)
 *   threat of excommunication (social and spiritual exclusion). Over the
 *   medieval period (approximately 500–1500 CE), this constraint evolved from
 *   early coordination function (providing institutional stability and moral
 *   order when secular authority was fragmented) into an increasingly
 *   extractive apparatus that concentrated wealth and spiritual authority in
 *   the Church. The constraint exhibits all six classification types from
 *   different structural positions, demonstrating how a single institutional
 *   mechanism can appear as natural law to theological defenders, as
 *   coordination to institutional beneficiaries, as extraction to powerless
 *   populations, and as a degraded ritual (piton) to late-medieval observers
 *   watching its authority decline. The theater ratio rose from 0.38 (early
 *   medieval) to 0.65 (late medieval), indicating that Church ritual
 *   increasingly became performative rather than functionally justified—a
 *   Goodhart drift visible in the rising disjunction between claimed
 *   spiritual authority and actual enforcement capacity. By the Reformation,
 *   the constraint had accumulated so much rent-seeking behavior
 *   (indulgences, pluralism, clerical corruption) that organized reform
 *   movements saw it as a temporary institutional arrangement with a sunset
 *   clause. The mandatrophy is resolved by recognizing that the constraint
 *   was genuinely coordination-enabling early (stabilizing a chaotic
 *   post-Roman political landscape) but evolved through institutional
 *   accumulation into a hybrid extraction-coordination system that became
 *   unsustainable once secular nation-states and literacy technologies
 *   provided alternative institutional bases.
 *
 * KEY AGENTS:
 *   - Ecclesiastical Hierarchy (Pope, Cardinals, Bishops): Institutional beneficiaries (institutional/arbitrage) — captures tithing revenue, land accumulation, political authority, and spiritual legitimation monopoly
 *   - Peasant Classes: Primary victims (powerless/trapped) — bears tithing burden, lacks scriptural understanding, faces damnation threat, cannot exit
 *   - Merchant/Craftsperson Classes: Secondary victims with coordination benefits (moderate/constrained) — gains institutional authority and contract enforcement; constrained by market restrictions and wealth extraction
 *   - Secular Rulers (Kings, Princes): Organized institutional actors (organized/constrained) — benefits from spiritual legitimation; constrained by ecclesiastical authority to excommunicate and interdict
 *   - Heretical and Unauthorized Movements: Suppressed groups (powerless/trapped) — faces inquisition, violent enforcement, maximal extraction through coercion
 *   - Reform Coalitions (Wycliffe, Hus, Conciliarists): Organized challengers (organized/mobile) — see constraint as temporary with sunset; building alternative institutional pathways (printing, councils, nationalism)
 *   - Papacy (Late Medieval): Institutional degradation agent (institutional/arbitrage) — maintains apparatus through inertia despite declining actual authority; experiences own mechanism as increasingly theatrical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medieval_church_hegemony, 0.58).
domain_priors:suppression_score(medieval_church_hegemony, 0.78).
domain_priors:theater_ratio(medieval_church_hegemony, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medieval_church_hegemony, extractiveness, 0.58).
narrative_ontology:constraint_metric(medieval_church_hegemony, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(medieval_church_hegemony, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medieval_church_hegemony, tangled_rope).
narrative_ontology:human_readable(medieval_church_hegemony, "The Medieval Ecclesiastical Hegemony").
narrative_ontology:topic_domain(medieval_church_hegemony, "religious/economic/political").

domain_priors:requires_active_enforcement(medieval_church_hegemony).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medieval_church_hegemony, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(medieval_church_hegemony, monastic_institutions).
narrative_ontology:constraint_beneficiary(medieval_church_hegemony, episcopal_landholders).
narrative_ontology:constraint_victim(medieval_church_hegemony, peasant_classes).
narrative_ontology:constraint_victim(medieval_church_hegemony, lay_commons).
narrative_ontology:constraint_victim(medieval_church_hegemony, unauthorized_teachers).
narrative_ontology:constraint_victim(medieval_church_hegemony, heretical_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANT CLASS (SNARE) — Trapped within the constraint by birth, geography, and spiritual threat. Cannot exit tithing, cannot access unmediated scripture, faces damnation if non-compliant. Theater ratio is high (0.65) because much of the peasant's religious experience is performative — ritual observance rather than understanding. d≈0.92, f(d)≈1.39, σ=0.9 → χ≈0.73. Pure extraction with maximal coercion.
constraint_indexing:constraint_classification(medieval_church_hegemony, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MERCHANT/CRAFTSPERSON CLASS (TANGLED ROPE) — Benefits from the Church's provision of scribal services, law enforcement (ecclesiastical courts), and moral authority that enforces trade contracts and oaths. Also constrained by tithing, confession requirements, and market restrictions (e.g., sabbath closures, usury prohibitions). d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.57. Hybrid: coordination (institutional authority) plus asymmetric extraction (mandatory wealth transfer).
constraint_indexing:constraint_classification(medieval_church_hegemony, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EPISCOPAL HIERARCHY (ROPE) — Benefits from tithing collection, land accumulation, and authority delegation. Experiences the constraint primarily as a coordination mechanism: the bishop is solving the collective action problem of maintaining moral order, canon law, and sacramental service provision. d≈0.15, f(d)≈0.10, σ=0.9 → χ≈0.05. Net beneficiary; constraint appears as coordination infrastructure.
constraint_indexing:constraint_classification(medieval_church_hegemony, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SECULAR AUTHORITY (TANGLED ROPE) — Benefits from the Church's moral legitimation of their rule and enforcement of oath-taking. Constrained by the Church's capacity to excommunicate, interdict entire territories, and mobilize religious populations. Ecclesiastical land holdings also reduce secular taxing base. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.41. Balanced extraction and coordination: the constraint simultaneously legitimates secular power and limits its reach.
constraint_indexing:constraint_classification(medieval_church_hegemony, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: PAPACY / HIGH ECCLESIASTICAL AUTHORITY (PITON) — By the late medieval period, the institutional basis of the hegemony is degrading while performative maintenance persists. Conciliar movements, secular nation-states, and theological challenges (Wycliffe, Hus) undermine papal authority, yet the Church maintains the apparatus of control through institutional inertia. theater_ratio=0.65 captures the gap between performed infallibility and actual declining enforcement capacity. d≈0.08, f(d)≈-0.08, σ=1.1 → χ≈-0.06. Negative effective extraction because the institutional beneficiary experiences its own mechanism as increasingly theatrical.
constraint_indexing:constraint_classification(medieval_church_hegemony, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: HERETICAL/UNAUTHORIZED COMMUNITIES (SNARE) — Systematically suppressed by inquisition, excommunication, and violent enforcement. Cannot exit without renouncing belief; cannot debate openly without penalty of death. The constraint's suppression (0.78) is enforced maximally against this group. d≈0.98, f(d)≈1.48, σ=0.9 → χ≈0.82. Extreme extraction through coercive institutional violence.
constraint_indexing:constraint_classification(medieval_church_hegemony, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 7: REFORM COALITION (SCAFFOLD) — By the 15th-16th centuries, organized movements (conciliarists, reformers, humanist scholars) see the ecclesiastical hegemony as a temporary institutional arrangement with a sunset clause. Printing technology, literacy expansion, and nationalist ideology create exit pathways. The constraint appears as a scaffold: real coordination benefits (institutional stability, moral authority) but increasingly temporary. d≈0.42, f(d)≈0.44, σ=1.0 → χ≈0.26. Suppression remains high (0.78) but organizers see it as defensible only temporarily.
constraint_indexing:constraint_classification(medieval_church_hegemony, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW THEODICY (FALSE SUMMIT) — Medieval theological framings present the ecclesiastical hegemony as divinely ordained, immutable, and natural to human salvation. From a 'civilizational/universal' analytical position (if accepting theological premises), the constraint appears as a Mountain: unchangeable spiritual law. However, the base properties (ε=0.58, suppression=0.78, requires_active_enforcement=true) contradict mountain classification. The engine detects this as a false natural law: what appears divinely immutable from inside theological frames is revealed as a contingent institutional extraction that required constant enforced maintenance.
constraint_indexing:constraint_classification(medieval_church_hegemony, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medieval_church_hegemony_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medieval_church_hegemony, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medieval_church_hegemony, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medieval_church_hegemony, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(medieval_church_hegemony, TR),
    TR >= 0.70.

:- end_tests(medieval_church_hegemony_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Church extracts approximately 10% of peasant wealth (tithing) plus additional revenue streams (church fees for sacraments, indulgences, endowments). This is substantial but not maximum because (1) the constraint provides genuine coordination benefits (institutional stability, dispute resolution, moral authority), (2) enforcement is imperfect (non-compliance exists), and (3) the Church reinvests some extraction into public goods (hospitals, schools, food relief during famines). The rising extractiveness over the interval (0.42→0.58) reflects institutional rent-seeking: indulgences, proliferation of church fees, and clerical corruption increased the pure extraction component relative to coordination services. Suppression (0.78): Very high. The Church employs excommunication (social death), interdict (community-level spiritual punishment), inquisition (torture and execution), and control of literacy to enforce compliance. Exit options are severely constrained for peasant populations and heretical movements. However, suppression is not complete (0.90+) because: (1) enforcement capacity was limited by logistics (traveling inquisitors could not reach all communities), (2) some peasants did manage heterodox belief, (3) secular rulers sometimes protected communities from ecclesiastical authority, and (4) excommunication itself paradoxically offered an exit mechanism (if one could bear social cost). Theater ratio (0.65): Moderate-high. The medieval Church maintained elaborate ritual (mass, confession, pilgrimage, relic veneration) that was largely performative—the peasant experienced these rituals as spiritually necessary but without understanding the Latin texts or theological justification. By late medieval period, the rise in theater reflects that Church authority was increasingly performed (ornate vestments, elaborate ceremonies, claims to infallibility) rather than genuinely believed. The theater rise also captures Goodhart drift: as enforcement became harder (due to literacy expansion and secular state competition), the Church doubled down on spectacle. Claimed type (Tangled Rope): The constraint has both genuine coordination function (providing moral framework, institutional authority, dispute resolution) and asymmetric extraction (wealth and authority concentration). No single classification fully captures it from all perspectives; the tangled_rope mediates between coordination (rope) and extraction (snare) components.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a complete perspectival divergence. (1) The peasant sees pure extraction (Snare: trapped, no exit, extreme coercion). (2) The merchant sees hybrid (Tangled Rope: benefits from institutional services but constrained by rules and extraction). (3) The bishop sees coordination (Rope: solving moral order, administrative problems). (4) The secular ruler sees complex hybrid (Tangled Rope: spiritual legitimation vs. ecclesiastical constraint on power). (5) The heretical movement sees maximal extraction (Snare: violently suppressed, no exit). (6) Late-medieval reformers see temporary constraint (Scaffold: with sunset clause as alternatives emerge). (7) High ecclesiastical authority experiences own apparatus as degraded (Piton: maintained through inertia, increasingly theatrical). (8) The theological defender sees natural law (Mountain: divinely ordained—FALSE SUMMIT). This perspectival range demonstrates that the constraint is fundamentally composed of extraction masquerading as coordination. From beneficiary perspectives (institutional hierarchy), it appears as legitimate coordination. From victim perspectives (peasants, heretics), it is pure extraction. From observer positions that see both, it is tangled rope with degrading legitimacy. The absence of consensus reveals the constraint's primary function: maintaining asymmetric power through institutional apparatus that claims universal benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Peasants: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction directionality; no exit options. Merchants: Both beneficiary (institutional services) + victim (extraction) + constrained → d≈0.68, f(d)≈1.05. Mixed directionality; perceive both coordination and extraction. Bishops: Beneficiary + arbitrage → d≈0.15, f(d)≈0.10. Low extraction directionality; net beneficiary. Secular rulers: Both beneficiary (legitimation) + victim (constraint on authority) + constrained → d≈0.50, f(d)≈0.65. Balanced; perceive mutual extraction and benefit. Heretical groups: Victim + trapped → d≈0.98, f(d)≈1.48. Extreme extraction directionality; violent suppression, no exit. Reformers: Victims (constrained by current rules) + mobile (alternative pathways visible) → d≈0.42, f(d)≈0.44. Low effective extraction because reformers can see and build exits. High ecclesiastical authority: Beneficiary + arbitrage but institutionally constrained (losing control) → d≈0.08, f(d)≈-0.08. Nominally beneficiary but negative effective extraction because the apparatus is degrading and high theater maintenance cost. The directionality chain reveals the core mandatrophy: institutional beneficiaries experience the constraint as natural coordination, but all non-institutional positions experience extraction. The constraint's survival depends on maintaining the beneficiary's perception and suppressing awareness among victims of the extraction's contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The classification is Tangled Rope (not pure Snare or pure Rope) because the constraint has both genuine coordination components AND asymmetric extraction, requires active enforcement, and beneficiaries + victims are both clearly identifiable. The mandatrophy is resolved by recognizing that the constraint is not a question of 'is this coordination or extraction?' but 'at what ratio has it evolved?' Early medieval (0–500): Genuine coordination (ε=0.42, lower theater). The Church provided institutional stability when no other organizational form could. The constraint was mixed but benefited both population (moral framework, institutional order) and hierarchy (authority). High medieval (500–1000): Increasingly asymmetric. Wealth accumulation by Church accelerates. Tithing becomes more coercive. Theater rises as extraction components increase relative to coordination services (ε=0.52). Late medieval (1000+): Degraded tangled rope transitioning toward snare from victim perspectives. Coordination services (dispute resolution, moral authority) are increasingly replaced by rent-seeking (indulgences, excessive church fees, clerical corruption). Theater reaches 0.65 as performative maintenance replaces functional authority. Reformers correctly identify the constraint as unsustainable hybrid. The 'mandatrophy' — the false claim that this is either pure coordination or pure nature-like constraint — is defeated by showing: (1) ε increases over time (extraction component accumulates), (2) theater ratio increases (functional justification declines), (3) different agents classify the constraint differently (it's not an objective property, but a relational one), (4) alternatives emerge that enable escape without spiritual cost (reform, printing, nation-states), revealing that the constraint was institutional, not inevitable. By 1500, the constraint had become unsustainable because the beneficiary's ability to suppress awareness of alternatives had failed — and once victims understand they can exit, the extraction mechanism loses effectiveness. The Church's response (Counter-Reformation, institutional reform) was an attempt to restore the tangled rope by reducing theater, improving service delivery, and reducing egregious extraction — essentially resetting the constraint's legitimacy before it tipped entirely into snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    spiritual_authenticity_threshold,
    'At what point does the Church''s institutional apparatus for spiritual delivery cross from ''necessary mediation'' into ''extraction mechanism''?',
    'Historical-comparative analysis: communities with and without institutional ecclesiastical control; rates of spiritual anxiety, moral behavior, and social cohesion; literacy and doctrinal understanding across stratified populations.',
    'If institutional mediation genuinely reduces spiritual disorder: constraint appears more as coordination (Rope) from all perspectives. If mediation is substantially theatrical and extraction: constraint appears as pure Snare for common people, masquerading as Mountain for authorities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spiritual_authenticity_threshold, conceptual, 'Whether ecclesiastical mediation provides genuine spiritual benefit or primarily enables extraction').

omega_variable(
    tithing_equivalence_class,
    'Is the 10% tithe functionally equivalent to a tax (in which case it is extractive per economic definition) or is it a legitimate devotional transfer (in which case it is voluntary coordination)?',
    'Enforcement data: what percentage of peasants pay willingly vs. under threat? What happens to non-payers? Comparative analysis with voluntary alms vs. mandatory tithing regions. Economic modeling of consumption impact.',
    'If tithing is genuinely voluntary: constraint is more Rope-like (coordination mechanism). If enforced through spiritual coercion: constraint is pure extraction (Snare), and the distinction from taxation is rhetorical only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tithing_equivalence_class, empirical, 'Whether tithing is voluntary devotion or coercive extraction').

omega_variable(
    latin_literacy_as_power_or_accident,
    'Did the Church deliberately restrict Latin literacy to maintain control, or did literacy restriction reflect unavoidable technological constraints that the Church did not invent?',
    'Historical evidence: Did Church authorities actively suppress vernacular literacy initiatives? Did they resist or support mass education? What literacy rates were technically achievable (parchment costs, scriptoria capacity) vs. actually achieved?',
    'If deliberate suppression: Latin restriction is an active extraction mechanism (Snare). If technological artifact: it is a coordination constraint with side effects (Rope or Tangled Rope) that became extractive post-hoc.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latin_literacy_as_power_or_accident, empirical, 'Whether Latin literacy restriction was deliberate institutional policy or technological necessity').

omega_variable(
    excommunication_exit_mechanism,
    'Did excommunication actually prevent exit or did it facilitate exit by releasing people from the constraint?',
    'Historical data on excommunicated populations: Did they form alternative communities? Did excommunication reduce Church revenue from that population (revealing it as costlier than retention)? Did secular rulers pardon excommunicated subjects?',
    'If excommunication enabled exit: constraint had an actual freedom mechanism available to those willing to bear social cost; d for ''trapped'' populations is overstated, and constraint appears less as Snare. If excommunication was purely punitive: it confirms trap (high suppression, low exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excommunication_exit_mechanism, empirical, 'Whether excommunication functioned as a trap mechanism or paradoxically as an exit mechanism').

omega_variable(
    mandate_and_actual_authority,
    'What percentage of Church doctrine and ethical enforcement was genuinely mandated by spiritual authority vs. opportunistically claimed to legitimize extraction?',
    'Textual analysis: compare biblical/patristic sources with actual Church practice. Compare official doctrine with actual enforcement patterns. Analyze sermons and theological justifications for tithing and confession requirements vs. scriptural sources.',
    'High mandate: constraint is justified coordination (Rope). Low mandate: constraint is fraudulent extraction (Snare), and the theological apparatus is theater (explaining high theater_ratio).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_and_actual_authority, empirical, 'Whether Church practices align with religious mandate or exceed it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medieval_church_hegemony, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mch_theater_early_medieval, medieval_church_hegemony, theater_ratio, 0, 0.38).
narrative_ontology:measurement(mch_theater_high_medieval, medieval_church_hegemony, theater_ratio, 500, 0.52).
narrative_ontology:measurement(mch_theater_late_medieval, medieval_church_hegemony, theater_ratio, 1000, 0.65).

% Extraction over time
narrative_ontology:measurement(mch_extract_early_medieval, medieval_church_hegemony, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mch_extract_high_medieval, medieval_church_hegemony, base_extractiveness, 500, 0.52).
narrative_ontology:measurement(mch_extract_late_medieval, medieval_church_hegemony, base_extractiveness, 1000, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medieval_church_hegemony, enforcement_mechanism).
narrative_ontology:affects_constraint(medieval_church_hegemony, religious_literacy_monopoly).
narrative_ontology:affects_constraint(medieval_church_hegemony, feudal_oath_binding).
narrative_ontology:affects_constraint(medieval_church_hegemony, indulgence_market_extraction).
narrative_ontology:affects_constraint(medieval_church_hegemony, heresy_suppression_apparatus).

% DUAL FORMULATION NOTE:
% The ecclesiastical hegemony is a constraint family. The primary constraint (medieval_church_hegemony, ε=0.58) represents the integrated system. Decomposed siblings: (1) religious_literacy_monopoly (ε=0.42) — Latin-only liturgy as distinct extraction mechanism; (2) feudal_oath_binding (ε=0.35) — Church's role in oath enforcement, coordination function; (3) indulgence_market_extraction (ε=0.71) — late-medieval rent-seeking, pure snare; (4) heresy_suppression_apparatus (ε=0.82) — violent extraction arm. These share the ecclesiastical hierarchy as common beneficiary but have distinct ε values reflecting their structural specificity. The primary story links to all four.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(medieval_church_hegemony, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
