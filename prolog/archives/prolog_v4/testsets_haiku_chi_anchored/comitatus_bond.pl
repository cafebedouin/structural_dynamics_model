% ============================================================================
% CONSTRAINT STORY: comitatus_bond
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_comitatus_bond, []).

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
 *   constraint_id: comitatus_bond
 *   human_readable: The Germanic Comitatus Code
 *   domain: social/political
 *
 * SUMMARY:
 *   The Germanic comitatus code represents the foundational socio-political
 *   constraint governing the relationship between a lord (the 'ring-giver')
 *   and his thanes in early Germanic and proto-feudal society (approximately
 *   1st-10th centuries CE). The constraint exhibits a structural hybrid: it
 *   provides genuine coordination benefits (mutual defense, resource pooling,
 *   territorial stability) while simultaneously enabling extraction of
 *   surplus labor, military obligation, and tribute from subordinate
 *   populations. The comitatus is neither pure coordination nor pure
 *   extraction — it is a tangled rope that solves a coordination problem for
 *   the privileged (lords and thanes) by shifting suppression and extraction
 *   costs to the powerless (commoners and subordinate tribes). The
 *   constraint's theater_ratio (0.55) reflects the balance between functional
 *   loyalty-binding and performative celebration of the bond in warrior
 *   culture. Over the interval from early Germanic settlement to the
 *   formalization of feudalism, extractiveness increased (0.22 → 0.38) as the
 *   ad-hoc gift-reciprocity system was progressively codified into mandatory
 *   tribute and service obligations. Theater ratio also increased (0.35 →
 *   0.55) as historical mythologizing obscured the coercive mechanisms
 *   beneath the honorific language of loyalty.
 *
 * KEY AGENTS:
 *   - Ring-Giver Lord: Primary beneficiary (institutional/arbitrage) — consolidates power through gift distribution and loyalty structuring; experiences comitatus as coordination mechanism
 *   - Thane Warrior: Mixed beneficiary-victim (moderate/constrained) — receives gifts and status but faces death obligation; experiences tangled coordination-extraction
 *   - Tributary Commoner: Primary victim (powerless/trapped) — produces surplus with no exit option; bears full cost of extraction and conscription
 *   - Subordinate Tribe: Secondary victim (powerful/mobile on paper, constrained in practice) — integrated through conquest; retains no meaningful independence
 *   - Feudal Successor Institutions: Organized agents (organized/constrained) — formalize comitatus into hereditary feudalism with written codification; create sunset mechanisms
 *   - Historical Mythmaking System: Institutional memory (institutional/arbitrage) — celebrates bond as purely honorific; obscures extraction mechanisms through performative elevation of loyalty
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable structural necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(comitatus_bond, 0.38).
domain_priors:suppression_score(comitatus_bond, 0.65).
domain_priors:theater_ratio(comitatus_bond, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(comitatus_bond, extractiveness, 0.38).
narrative_ontology:constraint_metric(comitatus_bond, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(comitatus_bond, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(comitatus_bond, tangled_rope).
narrative_ontology:human_readable(comitatus_bond, "The Germanic Comitatus Code").
narrative_ontology:topic_domain(comitatus_bond, "social/political").

domain_priors:requires_active_enforcement(comitatus_bond).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(comitatus_bond, ring_giver_lord).
narrative_ontology:constraint_beneficiary(comitatus_bond, thane_martial_elite).
narrative_ontology:constraint_victim(comitatus_bond, commoner_producers).
narrative_ontology:constraint_victim(comitatus_bond, subordinate_tribes).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRIBUTARY COMMONER (SNARE) — Bound to produce surplus for the comitatus while unable to exit. Faces military conscription obligations and extraction of goods with no reciprocal protection beyond minimal subsistence. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.61.
constraint_indexing:constraint_classification(comitatus_bond, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THANE WARRIOR (TANGLED ROPE) — Receives gifts, land, and status from the lord; coordinates loyalty, raiding, and defense within the comitatus structure. But also bears obligation to die in the lord's service and cannot leave without disgrace. d≈0.58, f(d)≈0.72, σ=0.9 → χ≈0.30.
constraint_indexing:constraint_classification(comitatus_bond, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RING-GIVER LORD (ROPE) — Benefits from the coordinated loyalty structure. Distributes gifts (rings, gold, weapons) to secure thane allegiance. Experiences the comitatus as a coordination mechanism solving the collective-action problem of defense and territorial control. d≈0.10, f(d)≈0.05, σ=0.9 → χ≈0.002. Minimal effective extraction; net beneficiary through coordination surplus.
constraint_indexing:constraint_classification(comitatus_bond, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SUBORDINATE TRIBE (SNARE) — Integrated into the comitatus hierarchy through conquest or vassalage. Extracts military value for the overlord but retains no independent exit option; absorption into the comitatus structure is mandatory. d≈0.85, f(d)≈1.18, σ=0.9 → χ≈0.53.
constraint_indexing:constraint_classification(comitatus_bond, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: FEUDAL SUCCESSOR INSTITUTIONS (SCAFFOLD) — Early medieval successor states (Frankish, Anglo-Saxon kingdoms) gradually formalize and institutionalize the comitatus into hereditary feudalism with written charters, land tenure codes, and manorial obligations. The raw comitatus extraction mechanism is being replaced by contractual reciprocity and institutional delegation. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.18. Lower effective extraction because institutional successors have agency in codifying the relationship.
constraint_indexing:constraint_classification(comitatus_bond, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: HISTORICAL MYTHMAKING (PITON) — By the High Middle Ages, the comitatus is remembered as a purely voluntary, honorific relationship of mutual loyalty rather than as an extractive institution. Medieval epics (Beowulf, Nibelungenlied) mythologize the bond as noble and reciprocal. The historical reality of coercion and extraction is obscured by performative celebration of loyalty. theater_ratio=0.55, indicating significant but not dominant performative content. The institutional memory serves to legitimize successor feudal structures.
constraint_indexing:constraint_classification(comitatus_bond, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN) — From a civilizational perspective, the comitatus appears as an immutable solution to the coordination problem of pre-state defense and resource sharing in acephalous societies. The constraint emerges naturally from the absence of centralized authority and the necessity of violent protection. However, the structural data (ε=0.38, suppression=0.65, theater=0.55) contradicts the mountain classification — the engine will identify this as a false summit, revealing that what appears structurally necessary is actually contingent on the absence of alternative coordination mechanisms.
constraint_indexing:constraint_classification(comitatus_bond, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(comitatus_bond_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(comitatus_bond, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(comitatus_bond, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(comitatus_bond, TR),
    TR >= 0.70.

:- end_tests(comitatus_bond_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The comitatus extracts significant surplus (labor, military service, goods) from commoners and subordinate tribes, but also provides coordination value (mutual defense, resource redistribution) that benefits all levels. The early Germanic version (ε≈0.22) was weighted more heavily toward coordination and gift-reciprocity; later feudal codification pushed it toward extraction (ε≈0.38) as obligations became mandatory rather than voluntary. Suppression (0.65): High. Commoners face military conscription, tribute extraction, and inability to exit without abandonment. Exit costs are severe — leaving the comitatus means loss of protection, land, and social identity. Thanes also face high suppression (death obligation) but with mitigating factors (status, property, agency in warfare). Theater ratio (0.55): Moderate. The comitatus includes genuine functional elements (coordination of defense, resource allocation) alongside performative elements (gift-giving ritual, loyalty celebration in epics). The theater increases over time as later medieval societies mythologize the bond while the actual extraction mechanisms become more legalized and routinized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement generates fundamentally different classifications across observer positions. The ring-giver lord sees coordination (Rope) — the solution to collective-action defense problems. The thane sees mixed coordination-extraction (Tangled Rope) — benefits from status and gifts but faces death obligation. The tributary commoner sees pure extraction (Snare) — produces surplus with no exit and minimal reciprocal protection. The subordinate tribe sees conquest-based extraction (Snare) — absorbed into hierarchy with no exit. Feudal successors see a temporary problem (Scaffold) — formalizing and moderating the bond through written obligation. Historical mythmaking sees a degraded, performatively sustained institution (Piton) — the actual extraction is obscured by celebratory narrative. The civilizational analytical observer risks seeing natural law (Mountain) — that comitatus emerges necessarily from pre-state conditions — but the structural data reveals this as a false summit: alternative coordination mechanisms existed (kinship reciprocity, tribal councils, non-hierarchical raiding bands), so the comitatus was not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Ring-giver lord: Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiary. Thane warrior: Mixed (benefits from gift/status, victim of death obligation) + constrained → d≈0.58, f(d)≈0.72. Moderate extraction. Tributary commoner: Victim + trapped → d≈0.92, f(d)≈1.38. High extraction — abstract but concrete collective good (peace, stability) is captured by elite while costs (labor, conscription) fall on powerless. Subordinate tribe: Victim + mobile (theoretically; actually constrained by military strength differential) → d≈0.85, f(d)≈1.18. High extraction — conquest and absorption eliminate independent exit. Feudal successors: Organized + constrained → d≈0.45, f(d)≈0.48. Low effective extraction; these agents have agency in codifying and moderating the relationship. Historical mythmaking: Institutional + arbitrage → d≈0.10, f(d)≈0.05. Piton classification derives from theater gate (0.55 ≥ 0.70 threshold not met, but still elevated), not from directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The comitatus resolves the mandatrophy by revealing how indexical classification captures the transition from pure coordination (early Germanic gift-reciprocity) to tangled coordination-extraction (classical comitatus) to formal feudal extraction (later codified). The constraint is NOT a mountain (immutable law of pre-state organization) — alternative coordination mechanisms existed in contemporary societies (tribal councils, egalitarian raiding bands, kinship-based reciprocity). The constraint IS a tangled rope because it genuinely solves coordination problems for the elite while simultaneously enabling extraction from the powerless. The perspectival gap between the lord's 'coordination problem solved' (Rope) and the commoner's 'extraction without exit' (Snare) is the diagnostic signature: when one group's coordination benefit is another group's suppression and extraction, the system is tangled rope, not pure coordination. The theater ratio (0.55) confirms this: if the system were pure coordination, theater would be low (≤0.30); if pure extraction, theater might be high but with no genuine function. At 0.55, the constraint has both functional and performative elements, characteristic of tangled rope. The historical mythmaking (Piton perspective) shows how the constraint's legitimacy was maintained through narrative elevation of honor and loyalty — once the explicit coercion could be formalized (feudalism), the need for performative elevation actually declined, suggesting a sunset mechanism where the comitatus is being institutionalized into something more contractual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntarism_vs_coercion_threshold,
    'At what point does the comitatus transition from voluntary gift-reciprocity to mandatory extraction of labor and military service?',
    'Historical analysis of thane defection rates, lord replacement cycles, and evidence of forced assimilation of conquered tribes. Comparison of early comitatus (Tacitus'' Germania) with later feudal codification.',
    'If primarily voluntary: classification shifts toward Rope from more perspectives. If primarily coercive: classification shifts toward Snare from thane perspective, confirming Tangled Rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntarism_vs_coercion_threshold, empirical, 'Threshold between voluntary reciprocal gift-giving and mandatory extraction').

omega_variable(
    subordinate_tribe_agency,
    'Did subordinate tribes retain capacity to exit the comitatus through migration, rebellion, or diplomatic realignment, or was absorption irreversible?',
    'Archaeological and historical evidence of tribal migration patterns, rebellion frequencies, and successful diplomatic realignment. Study of boundary zones and buffer populations.',
    'If exit available: subordinate tribe perspective shifts toward Rope or Scaffold. If exit blocked: confirms Snare perspective for tributary populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordinate_tribe_agency, empirical, 'Whether subordinate tribes retained meaningful exit options').

omega_variable(
    threshold_extraction_point,
    'Does the comitatus primarily function as coordination (mutual defense + resource distribution) or extraction (tribute capture with minimal reciprocal benefit)?',
    'Comparative analysis of thane/lord wealth ratios, land distribution, actual combat roles in warfare, and resource allocation during peace vs. raid cycles. Cross-cultural comparison with other gift-economy systems.',
    'If primarily coordination: ε should be ≤0.25, pushing classification toward Rope. If primarily extraction: ε≈0.38 confirms current Tangled Rope reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_extraction_point, empirical, 'Whether comitatus is fundamentally coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(comitatus_bond, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comitatus_tr_t0, comitatus_bond, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comitatus_tr_t3, comitatus_bond, theater_ratio, 3, 0.45).
narrative_ontology:measurement(comitatus_tr_t6, comitatus_bond, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(comitatus_be_t0, comitatus_bond, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(comitatus_be_t3, comitatus_bond, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(comitatus_be_t6, comitatus_bond, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(comitatus_bond, enforcement_mechanism).
narrative_ontology:affects_constraint(comitatus_bond, feudal_obligation_hierarchy).
narrative_ontology:affects_constraint(comitatus_bond, tribal_subordination_dynamic).

% DUAL FORMULATION NOTE:
% The comitatus is upstream of feudal obligation systems — feudalism is the institutional formalization and moderation of the raw comitatus extraction. The comitatus is also affected by tribal subordination dynamics: as tribes are conquered and absorbed into the comitatus hierarchy, their internal coordination systems (tribal councils, kinship reciprocity) are displaced by external extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(comitatus_bond, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
