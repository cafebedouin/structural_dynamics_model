% ============================================================================
% CONSTRAINT STORY: israel_gaza_ceasefire_violation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_gaza_ceasefire_violation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: israel_gaza_ceasefire_violation
 *   human_readable: Israel-Gaza Ceasefire Violation and Retaliatory Cycle
 *   domain: geopolitical/conflict
 *
 * SUMMARY:
 *   The Israel-Gaza ceasefire violation constraint describes a structural
 *   entrapment where agreements to reduce violence are repeatedly broken,
 *   triggering reciprocal escalation that harms primarily non-combatants. The
 *   initial ceasefire represents a coordination mechanism (reducing immediate
 *   casualties, allowing humanitarian access, providing diplomatic breathing
 *   room). Hamas violation of the ceasefire through rocket launches breaks
 *   this coordination function and initiates a retaliatory extraction cycle
 *   where Israeli military response imposes disproportionate costs on trapped
 *   Palestinian civilians. The cycle repeats because neither side can
 *   credibly commit to compliance without geopolitical cost: Hamas faces
 *   domestic legitimacy pressure to demonstrate resistance; Israel faces
 *   security pressure to deter future attacks. International mediation
 *   institutions exist (UN, regional mediators) but operate with limited
 *   enforcement capacity—they provide performative coordination theater
 *   rather than binding constraint on escalation. The constraint exhibits all
 *   six DR types depending on the observer's structural position: snare for
 *   trapped civilians on both sides, tangled rope for organized military
 *   actors who both coordinate and extract, scaffold for international
 *   mediators with sunset logic, piton for legal institutions with high
 *   theater, and a false mountain for analysts who naturalize the cycle as
 *   inevitable. The extractiveness has risen over the interval (0.42→0.68)
 *   reflecting deepening asymmetry: initial ceasefire period shows lower
 *   extractiveness because humanitarian gains are real and violation hasn't
 *   yet occurred; mid-cycle shows extraction rising as retaliatory strikes
 *   accumulate; final state shows high extractiveness as civilian costs
 *   accumulate faster than coordination benefits accrue.
 *
 * KEY AGENTS:
 *   - Palestinian Civilian Population: Primary victim (powerless/trapped) — bears asymmetric cost of retaliatory strikes regardless of participation in violations; cannot exit Gaza; cannot influence Hamas military decisions; experience maximum extraction
 *   - Israeli Civilian Population: Primary victim (powerless/trapped) — bears asymmetric cost of rocket attacks and disruption to normal life; cannot exit strike zones during hostilities; cannot influence Hamas compliance; experience extraction through forced vulnerability
 *   - Hamas Military Command: Organized actor (organized/constrained) — initiates violation through rocket launch; benefits from retaliatory response (demonstrates continued resistance, triggers domestic mobilization) but also suffers tactical costs; constrained exit (cannot fully disarm); engages in both extraction and coordination
 *   - Israeli Military Command: Organized actor (organized/constrained) — responds to violation through retaliatory strikes; constrained exit (legal/political obligation to respond); maintains deterrence but also escalates extraction cycle; operates in tangled rope coordination-extraction hybrid
 *   - International Mediators: Institutional actors (organized/mobile) — UN, regional powers, humanitarian organizations attempt to restore ceasefire; see structure as temporary scaffold with sunset; limited enforcement capacity; high theater ratio between protocols and actual influence
 *   - International Legal System: Institutional actor (institutional/constrained) — laws of war, humanitarian law, ceasefire agreements provide formal framework; enforcement is substantially performative (theater); differential application based on geopolitical interests; maintains piton-like inertia despite reduced functional capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks framing violation-retaliation as natural law of conflict; engine false summit detector will reveal this as naturalization of contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_gaza_ceasefire_violation, 0.68).
domain_priors:suppression_score(israel_gaza_ceasefire_violation, 0.75).
domain_priors:theater_ratio(israel_gaza_ceasefire_violation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, extractiveness, 0.68).
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(israel_gaza_ceasefire_violation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_gaza_ceasefire_violation, snare).
narrative_ontology:human_readable(israel_gaza_ceasefire_violation, "Israel-Gaza Ceasefire Violation and Retaliatory Cycle").
narrative_ontology:topic_domain(israel_gaza_ceasefire_violation, "geopolitical/conflict").

domain_priors:requires_active_enforcement(israel_gaza_ceasefire_violation).

% --- Structural relationships ---
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, palestinian_civilian_population).
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, israeli_civilian_population).
narrative_ontology:constraint_victim(israel_gaza_ceasefire_violation, humanitarian_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN CIVILIANS (SNARE) — Trapped within Gaza; cannot exit during hostilities. Bear asymmetric cost of retaliatory strikes regardless of whether they participated in rocket launches. No capacity to enforce ceasefire on militant groups or negotiate exit. Experience maximum extraction as non-combatants.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ISRAELI CIVILIANS (SNARE) — Trapped within strike range; cannot exit during rocket attacks. Bear costs of both initial Hamas violation and ongoing threat of retaliation. No capacity to enforce compliance on Hamas or negotiate ceasefire terms unilaterally. Experience extraction through forced exposure to violence and disruption.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: HAMAS MILITARY COMMAND (TANGLED ROPE) — Organized actor; constrained exit (cannot fully disarm without political cost). Ceasefire agreement represents coordination mechanism (reducing civilian casualties, allowing humanitarian access) but also extracts from Hamas' operational capacity. Violation re-initiates extraction cycle against both Hamas targets and civilian cover. Mixed extraction and coordination coercion.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ISRAELI MILITARY COMMAND (TANGLED ROPE) — Organized actor; constrained exit (legal/political obligation to respond to attacks). Ceasefire coordination reduces operational costs and international pressure but constrains strategic autonomy. Violation extraction on Hamas enables retaliatory extraction on civilian infrastructure. Hybrid coordination-extraction mechanism.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL MEDIATORS (SCAFFOLD) — UN, regional powers, humanitarian organizations see ceasefire as temporary coordination structure with sunset logic. Violation triggers enforcement rhetoric but enforcement capacity is limited. Framework reduces extraction per capita but only if compliance holds. Theater ratio reflects gap between mediation protocols and actual compliance mechanisms.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL SYSTEM (PITON) — Ceasefire agreements, laws of war, humanitarian law exist as institutional framework but enforcement is substantially performative. Violation response is conditioned on politics (great power vetoes at UNSC, differential application of 'violation' labels). Theater ratio high because legal mechanisms operate with limited actual enforcement capacity. Theater increases when same violations classified differently based on perpetrator identity.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the violation-retaliation cycle appears as inherent to asymmetric conflict: one side cannot credibly commit to restraint without sacrificing deterrence; the other cannot fully disarm without losing political legitimacy. The cycle appears immutable — a natural law of conflict dynamics. Engine's false summit detector will flag this as naturalization: the 'inevitable' cycle is contingent on institutional arrangements (lack of joint enforcement mechanism, distributed verification, mutual credibility restoration), not physical law.
constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_gaza_ceasefire_violation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_gaza_ceasefire_violation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_gaza_ceasefire_violation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_gaza_ceasefire_violation, TR),
    TR >= 0.70.

:- end_tests(israel_gaza_ceasefire_violation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68): High. The constraint operates as a pure extraction mechanism from the perspective of trapped civilians who bear disproportionate costs. Civilian casualties in retaliatory strikes are 3-5x higher than direct combatant losses; humanitarian infrastructure (hospitals, schools) becomes targeted; displacement creates secondary casualties. The extraction is not temporary or recovery-enabled—each cycle degrades baseline civilian capacity further. Suppression (0.75): Very high. Trapped populations cannot exit Gaza (physical barriers, border control); cannot influence military decisions (organized actors operate independently of civilian preference); cannot organize collective defense (military asymmetry is extreme); cannot refuse participation in shelter (civilians cannot opt out of being targeted). Suppression mechanisms are structural, not just coercive. Theater ratio (0.58): Moderate-high. International ceasefire agreements follow formal protocols with extensive negotiation theater, but actual enforcement capacity is limited. Attribution of violations is contested (both sides claim the other violated first); proportionality claims are asymmetric; humanitarian access windows are narrow and conditional. Theater increases over the interval as settlement-building continues despite ceasefire rhetoric, revealing gap between agreement claims and structural compliance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as fundamentally different types from different structural positions. Trapped civilians (powerless/trapped) classify it as pure snare—they experience only costs, no coordination benefits, no exit options. Military command actors (organized/constrained) experience tangled rope—the ceasefire coordinates with enforcement coercion; they derive both constraints (operational limits) and benefits (reduced international pressure, operational windows). International mediators (organized/mobile) see scaffold—the ceasefire is explicitly temporary with sunset logic; mediation institutions have exit (they can shift focus to other conflicts, declare mediation exhausted). The international legal system (institutional/constrained) sees piton—laws of war and humanitarian conventions exist as institutional theater but enforcement is selective and weak, maintained through procedural ritual rather than functional deterrence. The analytical observer risks seeing mountain—the cycle appears immutable, a natural consequence of asymmetric power and distributed rationality. But the structural data contradicts mountain: the mechanism relies entirely on contingent institutional arrangements (no joint enforcement, asymmetric geopolitical interests, distributed decision-making). This is not a natural law but a solvable institutional design problem—if mutual enforcement credibility could be established, if humanitarian corridors could be made automatic, if verification could be third-party, the cycle could break. The false summit reveals that 'inevitable conflict' is often 'institutional failure to design binding constraints.'
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is derived from their structural relationship to the extraction flow. Trapped civilians have zero exit options and zero benefits from the ceasefire mechanism—they are pure victims. Their d ≈ 0.95 (full target), producing maximum f(d) ≈ 1.42, giving high experienced extraction χ. Military command actors have organized power and constrained exit—they are neither pure beneficiaries nor pure victims. Hamas benefits from demonstrating continued resistance (low d if measured from internal legitimacy perspective) but suffers tactical costs (high d if measured from force preservation perspective). The engine derives d from beneficiary/victim declarations and exit options: since no agent is declared as beneficiary (ceasefire benefits no group directly), all agents start with victim-like directionality. Military actors move upward from baseline victim classification because organized power and constrained exit (not trapped exit) reduce f(d). International mediators have mobile exit (they can declare mediation exhausted) so they experience lower f(d) than trapped agents. The piton classification derives from theater ratio (0.58 > 0.50), not from high χ—the legal system experiences itself as degraded, performing compliance rituals without functional enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing legitimate enforcement (response proportional to violation, targeting combatants, minimizing civilian impact) from pure extraction (response disproportionate, targeting civilians, accumulating civilian cost faster than deterrence effect). The mandatrophy question: Is the ceasefire a coordination mechanism that breaks down under violation stress, or is it pure extraction machinery that uses violation as justification? The structural test: If retaliatory strikes target military infrastructure and combatants, and if strike escalation follows rational deterrence logic (smaller strikes to deter future violations), the classification trends toward tangled rope—mixed coordination and enforcement. If retaliatory strikes target dual-use or civilian infrastructure, and if strikes follow pattern of maximizing pressure regardless of deterrence effect, the classification trends toward snare—pure extraction that uses violation as justification. The data from the Israel-Gaza context over 2020-2024 supports the snare classification for trapped civilians: civilian death-to-combatant ratios of 2:1 to 5:1; targeting of hospitals and schools; displacement of 80%+ of civilian population; humanitarian access windows that don't meet basic civilian needs. This suggests that the retaliatory extraction is not proportional enforcement but opportunistic maximization of civilian cost, justified ex post by the violation. The mandatrophy is resolved by showing that 'response to violation' and 'extraction mechanism disguised as response' are empirically distinguishable: compare this conflict to others where violations trigger proportional, combatant-focused responses with humanitarian access provisions. The Israel-Gaza case shows high theater (formal ceasefire protocols with low compliance) and high extractiveness (civilian casualty asymmetry), confirming snare classification from victim perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    attribution_certainty,
    'How certain must attribution of rocket fire to Hamas be before retaliation is justified? What probability threshold converts ''violation response'' into ''pretext for extraction''?',
    'Forensic analysis of projectile origins; third-party ballistic verification; comparison of attribution certainty to international enforcement standards in other domains',
    'If threshold < 60%: high false-positive retaliation; extraction mechanisms appear justified. If threshold > 95%: true violations may be underresponded; coordination mechanism appears toothless. At 80%: boundary between legitimate enforcement and extraction becomes observable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_certainty, empirical, 'Attribution threshold for distinguishing legitimate enforcement from pretext retaliation').

omega_variable(
    proportionality_metric,
    'What constitutes proportional response to ceasefire violation? Is proportionality measured by tactical symmetry (rockets for air strikes), civilian impact parity, or political deterrence effect?',
    'Analysis of response scale relative to initiating violation; comparison of casualty ratios; assessment of whether response deters future violations or provokes escalation',
    'If proportionality undefined: retaliation becomes pure extraction (snare from victim view). If proportionality enforced: retaliation becomes legitimate enforcement (tangled_rope equilibrium). Standard used determines classification across all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_metric, conceptual, 'Definition and measurement of proportional response to violations').

omega_variable(
    civilian_cover_vs_legitimate_target,
    'When Hamas launches from civilian areas or stores munitions in civilian infrastructure, does this convert civilian areas into legitimate military targets? Does civilian presence transform defense extraction into combat necessity?',
    'Comparative analysis across multiple conflicts (civilian-proximity targeting rules); assessment of whether civilian impact is minimized or deliberately amplified; correlation between target choice and military necessity',
    'If civilian proximity allows targeting: retaliation classification softens to tangled_rope (mixed). If civilians are protected: civilian casualty patterns reveal pure extraction (snare). Classification shifts across agents based on how this omega resolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_cover_vs_legitimate_target, preference, 'Civilian presence conversion rules for military target legitimacy').

omega_variable(
    enforcement_credibility_gap,
    'Can international mediators credibly commit to enforcing ceasefire terms on both sides equally, or do geopolitical interests make selective enforcement inevitable?',
    'Historical analysis of enforcement consistency across similar ceasefire agreements; assessment of mediator incentives relative to enforcement costs; third-party verification of compliance vs response asymmetries',
    'If credible equal enforcement possible: scaffold perspective (sunset mechanism) is real. If geopolitical interests dominate: mediation is theater (piton perspective is correct). Determines whether international framework reduces or merely masks extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_credibility_gap, empirical, 'Whether mediators can enforce agreements credibly and equally').

omega_variable(
    mutual_deterrence_trap,
    'Is the violation-retaliation cycle a Prisoner''s Dilemma where both sides prefer peaceful coexistence but each fears first-move vulnerability? Or is it a pure extraction dynamic where one side benefits from continued conflict?',
    'Comparison of stated peace preferences to revealed incentive structures; analysis of whether escalation increases or decreases the violating side''s strategic position; assessment of whether violations follow predictable triggers or exploit windows of opportunity',
    'If Prisoner''s Dilemma: institutional design can solve (create joint enforcement, reduce first-move costs). If pure extraction: institutional design is insufficient (fundamental interests misaligned). Determines whether snare is fixable or immutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_deterrence_trap, empirical, 'Whether cycle reflects mutual deterrence trap or pure extraction asymmetry').

omega_variable(
    humanitarian_corridor_credibility,
    'Do ceasefires enable sufficient humanitarian access to reduce civilian suffering, or is the access window too brief and constrained to create net relief?',
    'Longitudinal measurement of humanitarian goods delivered during ceasefire windows; comparison to baseline civilian needs; assessment of whether ceasefire window enables medical treatment and reconstruction or merely resets for next cycle',
    'If net humanitarian benefit: ceasefire becomes rope-like (coordination function real). If access is performative theater: ceasefire remains snare (coordination claim is false). Determines whether international scaffolding has functional value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_corridor_credibility, empirical, 'Whether ceasefire windows enable meaningful humanitarian access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_gaza_ceasefire_violation, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igcv_theater_t0, israel_gaza_ceasefire_violation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(igcv_theater_t1, israel_gaza_ceasefire_violation, theater_ratio, 1, 0.53).
narrative_ontology:measurement(igcv_theater_t2, israel_gaza_ceasefire_violation, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(igcv_extract_t0, israel_gaza_ceasefire_violation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(igcv_extract_t1, israel_gaza_ceasefire_violation, base_extractiveness, 1, 0.55).
narrative_ontology:measurement(igcv_extract_t2, israel_gaza_ceasefire_violation, base_extractiveness, 2, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_gaza_ceasefire_violation, enforcement_mechanism).
narrative_ontology:affects_constraint(israel_gaza_ceasefire_violation, humanitarian_access_blockade).
narrative_ontology:affects_constraint(israel_gaza_ceasefire_violation, settlement_expansion_encroachment).
narrative_ontology:affects_constraint(israel_gaza_ceasefire_violation, asymmetric_deterrence_credibility).

% DUAL FORMULATION NOTE:
% The ceasefire violation constraint is downstream of deeper structural asymmetries: unequal military capacity (affects retaliation scale), asymmetric geopolitical support (affects enforcement credibility), and distributed decision-making on both sides (affects coordination capacity). Each upstream constraint has its own extractiveness; the ceasefire violation represents the interaction point where these asymmetries converge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
