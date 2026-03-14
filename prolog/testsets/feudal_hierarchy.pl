% ============================================================================
% CONSTRAINT STORY: feudal_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_hierarchy, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feudal_hierarchy
 *   human_readable: Feudal Hierarchy and Serfdom
 *   domain: political/economic/social
 *
 * SUMMARY:
 *   Feudal hierarchy represents a structural constraint on mobility, labor
 *   allocation, and resource extraction that governed European societies from
 *   approximately the 9th to the 15th centuries. The system organized society
 *   into a pyramid: serfs (bound to land), peasants (tenants with some
 *   mobility), craftspeople and merchants (guild-controlled), lesser nobility
 *   and vassals (bound by oaths and fiefs), and crown/high nobility (de jure
 *   at the apex, de facto competing for power). This constraint exhibits
 *   characteristics across all six DR types, revealing how a single
 *   institutional structure appears radically differently depending on the
 *   observer's structural position. The serf sees pure extraction (Snare);
 *   the merchant sees an alternative pathway with a sunset (Scaffold); the
 *   lesser vassal sees mixed coordination and extraction (Tangled Rope); the
 *   institutional legacy sees its own degraded ritual (Piton); the noble
 *   beneficiary sees coordination (Rope); and the civilizational observer
 *   risks naturalizing contingent arrangements as immutable law (Mountain).
 *   The trajectory of extractiveness and theater ratio over the interval
 *   (0-400 years) reflects institutional intensification: as population
 *   growth reduced frontier escape options and market competition increased,
 *   feudal lords enforced hierarchy more strictly and relied increasingly on
 *   performative ritual (oaths, chivalry, divine right) to maintain
 *   legitimacy.
 *
 * KEY AGENTS:
 *   - Peasant Serfs: Primary victims (powerless/trapped) — comprise 80-90% of population; bound to land with legal prohibition on exit; face severe punishment (execution, mutilation, enslavement) for flight. Provide labor, produce tribute, military conscription with minimal reciprocal protection. Extracted maximum.
 *   - Urban Laborers and Craftspeople: Secondary victims (powerless/trapped/constrained) — organized into guilds that control access to tools, materials, and markets; face legal penalties for guild violation; exit to cities partially available but restricted by guild monopolies and urban privileges.
 *   - Nobility and Landowners: Primary beneficiaries (institutional/arbitrage) — extract labor, produce, and military service from serfs; capture surplus; control dispute resolution and property law in their interest. Experience hierarchy as coordination mechanism with high benefit.
 *   - Crown and Central Authority: Secondary beneficiary (institutional/arbitrage) — claims ultimate property right to all land; extracts taxes, military service, and loyalty oaths from nobility; maintains legal system that enforces hierarchy. Benefits from consolidation of power at apex.
 *   - Lesser Vassal Class: Mixed position (institutional/constrained) — bound by oath and feudal obligation to provide military service and tribute; receive fiefs and protection in return. Occupy hybrid role: simultaneously extracted from and beneficiary. More exit options than serfs but costly.
 *   - Merchant and Charter Towns: Nascent alternative (organized/mobile) — represent emerging pathway outside feudal extraction; chartered towns provide legal protection independent of feudal hierarchy; guilds monopolize but also enable economic mobility; visible exit route as market mechanisms expand.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_hierarchy, 0.68).
domain_priors:suppression_score(feudal_hierarchy, 0.75).
domain_priors:theater_ratio(feudal_hierarchy, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_hierarchy, extractiveness, 0.68).
narrative_ontology:constraint_metric(feudal_hierarchy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feudal_hierarchy, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_hierarchy, snare).
narrative_ontology:human_readable(feudal_hierarchy, "Feudal Hierarchy and Serfdom").
narrative_ontology:topic_domain(feudal_hierarchy, "political/economic/social").

domain_priors:requires_active_enforcement(feudal_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_hierarchy, nobility).
narrative_ontology:constraint_beneficiary(feudal_hierarchy, crown).
narrative_ontology:constraint_victim(feudal_hierarchy, peasant_serfs).
narrative_ontology:constraint_victim(feudal_hierarchy, urban_laborers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE BOUND SERF (SNARE) — A peasant bound to land they do not own, with no legal right to exit, facing severe punishment (death, mutilation, enslavement) for attempting flight. Extraction is maximized: the serf provides labor, tribute, and military service with no reciprocal protection beyond bare subsistence. Suppression is total — legal prohibition, military enforcement, geographic isolation, and identity loss make exit structurally impossible. This is the primary target perspective.
constraint_indexing:constraint_classification(feudal_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE URBAN LABORER (SNARE) — Guild systems and urban craft hierarchies create similar structures: journeymen cannot advance without master approval; masters control access to tools, materials, and markets; fleeing a guild incurs severe penalties. While nominally more mobile than serfs, urban laborers face comparable extraction through legal barriers (guild monopolies), market control, and organized suppression of wage competition.
constraint_indexing:constraint_classification(feudal_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE NOBLE LANDOWNER (ROPE) — The noble experiences the hierarchy as a coordination mechanism: serf labor creates wealth, military hierarchy secures defense, ritual vassalage creates alliance networks. The noble can exit (sell lands, seek higher patronage) or arbitrage (claim service from multiple serf cohorts, shift allegiance between competing lords). For the beneficiary, the constraint appears as functional coordination with benefits; extraction is directed toward them, not away. Net experience is pure coordination.
constraint_indexing:constraint_classification(feudal_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE LESSER VASSAL (TANGLED ROPE) — Vassals occupy a hybrid position: they provide military service, fealty oaths, and tribute to overlords, but also receive land grants (fiefs), military protection, and dispute arbitration. Exit is costly but possible (seek new patronage, flee to frontier, join the Church). Suppression is significant but not total. The vassal experiences genuine coordination (mutual defense, dispute settlement) alongside asymmetric extraction (labor and military service flow upward, land and protection flow downward, but asymmetrically). Active enforcement maintains the oath system; both parties accept the arrangement as partially legitimate.
constraint_indexing:constraint_classification(feudal_hierarchy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE CROWN/CHURCH INSTITUTIONAL VIEW (PITON) — The feudal hierarchy's original function was coordination: dispersed political authority (no central state capacity) required delegation through vassal networks; warfare and banditry required militarized villages (serfs could not flee to safer lands). By the late medieval period, this coordination function has atrophied. The hierarchy persists through institutional inertia: succession law, property claims, church authority over marriage and inheritance. The theater ratio is high (ritual oaths, heraldry, chivalric codes) but the functional coordination is largely replaced by emerging national states and market mechanisms. The institution sees itself as degraded.
constraint_indexing:constraint_classification(feudal_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: THE MERCHANT CLASS (SCAFFOLD) — Emerging merchant networks, chartered towns, and guilds represent alternative institutional pathways. These create temporary extraction (tolls, market monopolies, apprenticeship restrictions) but with visible sunset: as markets expand and national states consolidate taxation, the feudal extraction mechanism loses force. Exit pathways multiply (urban migration, colonial expansion, legal incorporation). High suppression wanes as alternatives proliferate. This perspective sees the feudal hierarchy as a transitional constraint that is being replaced by market mechanisms and nation-states — a sunset is visible.
constraint_indexing:constraint_classification(feudal_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER (MOUNTAIN CLAIM) — A civilizational view might frame feudalism as an immutable response to pre-industrial scarcity: without central state capacity, without markets, without transportation, hierarchical delegation and inherited status are the only feasible governance structures. From this perspective, the hierarchy is a natural law of pre-industrial societies. However, the base properties contradict this: extractiveness (0.68), suppression (0.75), and a theater ratio (0.65) indicate that significant enforcement is required — a natural law would not need enforcement. This is a false summit, revealing that feudalism naturalized contingent institutions.
constraint_indexing:constraint_classification(feudal_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feudal_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feudal_hierarchy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(feudal_hierarchy, TR),
    TR >= 0.70.

:- end_tests(feudal_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting the core mechanism of feudal extraction — serfs produce surplus that is captured through legal prohibition, military enforcement, and ideological naturalization (divine right, natural hierarchy). The value is not maximal (0.85+) because: (1) serfs receive subsistence and protection (however inadequate), creating nominal reciprocal obligation; (2) lesser vassals genuinely participate in the coordination function (defense, dispute settlement); (3) merchant alternatives were increasingly available from 1200 onward, reducing effective extraction for agents with mobility. Suppression (0.75): High, reflecting severe barriers to exit — legal prohibition on serf movement, punishment infrastructure (gallows, mutilation), ideological capture (religious sanction of hierarchy), geographic isolation, military enforcement, and identity loss (serfs often lack patronymic surnames, treated as property extensions). Suppression is not total (0.95+) because: (1) some serf mobility occurred through urban flight, colonization, or gradual manumission; (2) lesser vassals could negotiate fiefs or seek alternative patronage; (3) Church offered partial escape (monasticism, priesthood). Theater ratio (0.65): Moderate-high, reflecting the increasing performative content of feudal hierarchy by the high medieval period. Rituals (knighthood, homage ceremonies, heraldic codes, chivalric romances) proliferated as the original coordination functions (defense, dispute settlement, population stabilization) were increasingly performed by alternatives (national armies, royal courts, market mechanisms). Theater increased over the interval as the system aged — institutional inertia required more ritual to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between powerless/trapped and institutional/arbitrage perspectives. The serf perceives Snare with chi ≈ 0.95 (high extraction, total suppression, zero exit). The noble perceives Rope with chi ≈ -0.12 (coordination mechanism, extraction flows toward them). This gap is not measurement error — it reflects real structural difference. The serf's experience of maximized extraction is not contradicted by the noble's experience of coordination; both are true from their respective positions. The gap reveals the constraint's core function: transfer of resources from one structural position to another.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position relative to extraction. Serfs occupy d ≈ 0.98 (trapped victims, maximum d → maximum f(d) ≈ 1.42 → high experienced extraction chi). Nobility occupy d ≈ 0.05 (beneficiaries with arbitrage options, minimum d → negative f(d) ≈ -0.12 → extraction flows toward them, negative chi). Lesser vassals occupy d ≈ 0.50 (hybrid position: constrained but with some agency and genuine benefit; symmetric costs/benefits → f(d) ≈ 0.65 → moderate chi). Urban merchants occupy d ≈ 0.40 (victims of guild restrictions but with exit options, moderate d → moderate f(d) ≈ 0.40). The analytical observer at civilizational scope might attempt d ≈ 0.72 (attempting universal position, but this risks false naturalization). The engine's derivation chain prioritizes: (1) explicit beneficiary/victim declarations (serfs are victims, nobility are beneficiaries); (2) exit options (trapped → high d, arbitrage → low d, constrained → medium d); (3) power level (powerless × trapped → highest d, institutional × arbitrage → lowest d). The perspectival gap emerges because different observers have radically different exit options and beneficiary status, producing divergent d values and thus divergent chi values despite identical base_properties metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   The feudal hierarchy exemplifies how mandatrophy resolution requires multiple perspectives. The system is not 'really' a Snare, or 'really' a Rope, or 'really' a Scaffold — it is genuinely all of them from different structural positions. The mandatrophy is resolved by recognizing that feudalism is a **presheaf structure**: the classification depends on the observer's position in the hierarchy. From the serf position, it is extractive Snare. From the merchant position, it is a degrading Scaffold. From the noble position, it is functional Rope. From the civilizational position, it appears (falsely) as Mountain. The analytical resolution: there is no single 'true' type; the indexed family of classifications across all perspectives is the complete description. The false summit (Mountain) is particularly instructive: the attempt to view feudalism as a natural law of pre-industrial governance is a naturalization of elite choice. The high suppression (0.75) is the key diagnostic: natural laws do not require suppression. Mountain classification is eliminated by the existence of significant enforcement overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    serf_exit_vs_slave,
    'Is the serf economically and structurally distinguishable from a chattel slave, or does the distinction collapse under analysis?',
    'Comparative institutional analysis: property rights in serfs'' labor vs. property rights in slave bodies; inheritance of status; freedom to marry, own property, or gain manumission; legal recourse against lord violence.',
    'If distinction holds: snare classification is correct — mixed extraction and coordination maintain some nominal reciprocal obligation. If distinction collapses: serf is chattel slave under different terminology — extractiveness should be 0.85+, suppression 0.95+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(serf_exit_vs_slave, empirical, 'Whether serfdom is structurally distinct from slavery').

omega_variable(
    vassal_protection_credibility,
    'How much of the lord''s protection obligation is genuinely provided vs. held as contingent on serf compliance?',
    'Historical analysis of lord response to external threat (raids, invasion) vs. internal threat (serf rebellion); documentation of protection withdrawal as punishment; comparison with mercenary or professional military alternatives.',
    'If protection is genuine and unconditional: tangled rope classification is accurate — real coordination exists. If protection is largely contingent or illusory: the lesser vassal perspective should reclassify as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vassal_protection_credibility, empirical, 'Credibility of reciprocal lord protection in vassal relationships').

omega_variable(
    necessity_vs_choice_in_delegation,
    'Did pre-industrial societies require feudal hierarchy out of structural necessity (population density, transportation limits, no alternative governance), or was feudalism a contingent choice by elites to concentrate power?',
    'Comparative analysis of pre-industrial societies that adopted vs. rejected feudal hierarchy; examination of alternative governance structures in similar material conditions; analysis of elite deliberation and choice points.',
    'If necessity: mountain perspective gains credibility — feudalism is partially a natural law response. If contingent choice: mountain is a false summit — elite choice naturalized as structural inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_vs_choice_in_delegation, conceptual, 'Whether feudal hierarchy was structurally necessary or contingent elite choice').

omega_variable(
    merchant_exit_timing,
    'At what point did merchant/market alternatives become genuinely available as exits from feudal extraction, and did this availability reduce feudal suppression?',
    'Timeline mapping: charter grants to towns, development of trade routes, expansion of urban opportunity; correlation with serf flight rates, reduction in punishment severity, or lord concessions.',
    'If exits became available early and suppression reduced: scaffold classification confirmed and sunset timing can be estimated. If exits were restricted or suppression increased despite alternatives: feudal elites actively maintained monopoly — snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merchant_exit_timing, empirical, 'Timeline of merchant/market alternatives and their impact on feudal suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_hierarchy, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_tr_t0, feudal_hierarchy, theater_ratio, 0, 0.45).
narrative_ontology:measurement(feudal_tr_t200, feudal_hierarchy, theater_ratio, 200, 0.55).
narrative_ontology:measurement(feudal_tr_t400, feudal_hierarchy, theater_ratio, 400, 0.65).

% Extraction over time
narrative_ontology:measurement(feudal_be_t0, feudal_hierarchy, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feudal_be_t200, feudal_hierarchy, base_extractiveness, 200, 0.62).
narrative_ontology:measurement(feudal_be_t400, feudal_hierarchy, base_extractiveness, 400, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_hierarchy, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_hierarchy, 0.18).
narrative_ontology:affects_constraint(feudal_hierarchy, divine_right_monarchy).
narrative_ontology:affects_constraint(feudal_hierarchy, serfdom_escape_pipeline).
narrative_ontology:affects_constraint(feudal_hierarchy, guild_monopoly_control).

% DUAL FORMULATION NOTE:
% Feudal hierarchy is a macro-level constraint decomposable into structural components: serfdom (extraction mechanism, ε≈0.82), vassalage (coordination mechanism, ε≈0.45), guild control (urban extraction, ε≈0.58), and ideological naturalization via divine right (theater mechanism, ε≈0.72). The present story models the integrated system; decomposed stories would reveal that different social strata operate under distinct constraints with distinct ε values. The macro ε=0.68 is a weighted average over these strata.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_hierarchy, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
