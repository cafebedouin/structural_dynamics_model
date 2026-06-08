% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_stone_directive_flat_control, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_directive_flat_control
 *   human_readable: Aneyoshi Stone Directive: Tsunami Hazard Memory Inscription
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone inscription 'Do not build your homes below this point'
 *   (Aneyoshi-seki) represents a material encoding of intergenerational
 *   hazard memory following the 1933 Sanriku tsunami. The stone was placed by
 *   survivors at the maximum extent of the tsunami's reach, creating a
 *   physical boundary that coordinates settlement patterns across
 *   generations. This constraint demonstrates how communities embed survival
 *   knowledge in landscape and material culture, creating a coordination
 *   mechanism that persists without formal enforcement. The constraint's
 *   classification varies dramatically across perspectives: from pure
 *   coordination (rope) for households and analytical observers, to mixed
 *   coordination-extraction (tangled_rope) for municipal governments that
 *   lose development revenue, to degraded ritual (piton) for contemporary
 *   zoning authorities whose functional role has been displaced by modern
 *   warning systems. The stone directive is a canonical example of how
 *   disaster anthropology reveals the institutional structures that preserve
 *   collective memory and coordinate hazard avoidance.
 *
 * KEY AGENTS:
 *   - Coastal Settlement Populations: Primary beneficiary (powerless/constrained) — benefit from intergenerational hazard knowledge preservation; experience constraint as coordination, not extraction
 *   - Individual Landowners: Secondary beneficiary (moderate/mobile) — benefit from hazard avoidance; can relocate or develop elsewhere; experience low extraction
 *   - Municipal Government: Mixed actor (organized/constrained) — coordinates hazard mitigation but loses development revenue from restricted lower-elevation land; experiences tangled_rope dynamics
 *   - Disaster Anthropology Community: Analytical observer (institutional/arbitrage) — sees pure coordination mechanism; no extraction visible from this perspective
 *   - Contemporary Zoning Authority: Institutional actor (institutional/constrained) — maintains stone directive through cultural reverence and institutional inertia; functional role displaced by modern systems
 *   - Intergenerational Hazard Knowledge: Non-agent beneficiary (analytical/analytical) — abstract collective good that benefits from the stone's preservation function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive_flat_control, 0.15).
domain_priors:suppression_score(aneyoshi_stone_directive_flat_control, 0.35).
domain_priors:theater_ratio(aneyoshi_stone_directive_flat_control, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, extractiveness, 0.15).
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive_flat_control, rope).
narrative_ontology:human_readable(aneyoshi_stone_directive_flat_control, "Aneyoshi Stone Directive: Tsunami Hazard Memory Inscription").
narrative_ontology:topic_domain(aneyoshi_stone_directive_flat_control, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(aneyoshi_stone_directive_flat_control, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, coastal_settlement_populations).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, intergenerational_hazard_knowledge).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, coastal_households).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, individual_landowners).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, municipal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households living in the Sanriku coastal region benefit from the stone directive's preservation of intergenerational hazard knowledge. The constraint coordinates settlement patterns away from tsunami-vulnerable zones, reducing their risk of catastrophic loss. Relocation is costly and difficult, but the constraint itself is not extractive — it benefits them by encoding survival knowledge in landscape and material culture. They experience the stone as a coordination mechanism that solves the collective-action problem of hazard memory across generations.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, coastal_households, beneficiary,
    powerless, generational, constrained, local).

% Individual landowners in the Sanriku region can choose to develop above or below the stone line. Those who develop above the line benefit from hazard avoidance without constraint. Those who own land below the line face development restrictions but can relocate or develop elsewhere. The constraint benefits all landowners by reducing tsunami risk and coordinating settlement patterns. Exit is available through relocation or alternative development sites.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, individual_landowners, beneficiary,
    moderate, biographical, mobile, local).

% The municipal government coordinates land-use planning and hazard mitigation through zoning codes that embed the stone directive. It benefits from hazard mitigation and reduced disaster costs, but loses development revenue from restricted lower-elevation land. The constraint requires active enforcement through zoning compliance and cultural transmission of the directive's meaning. The municipality is both the agenda-setter (it enforces zoning) and a beneficiary (it reduces disaster costs), but also bears costs (lost development revenue).
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, municipal_government, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_directive_flat_control, municipal_government, beneficiary).

% The disaster anthropology community studies the Aneyoshi stone directive as a canonical example of how communities encode hazard memory in landscape and material culture. They see the constraint as pure coordination — it solves the intergenerational hazard-memory problem without extraction. They can study other constraints or other aspects of disaster anthropology; their exit options are high. They benefit from the constraint's existence as a research exemplar.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, disaster_anthropology_community, observer,
    institutional, civilizational, arbitrage, global).

% The contemporary municipal zoning authority maintains the stone directive in zoning codes and planning documents. The constraint persists through cultural reverence and institutional inertia rather than active functional necessity. Modern tsunami-warning systems and building codes now perform the hazard-coordination function that the stone originally provided. The zoning authority is constrained by cultural expectations and institutional precedent; it cannot easily remove the directive even if its functional role has been displaced.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, contemporary_zoning_authority, agenda_setter,
    institutional, immediate, constrained, local).

% Intergenerational hazard knowledge is an abstract collective good that benefits from the stone directive's preservation function. The constraint encodes survival knowledge in material form, creating a mechanism for transmitting hazard awareness across generations. This is not an agent that collects rents or bears costs, but a non-agent entity that benefits from the constraint's coordination function.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, intergenerational_hazard_knowledge, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(aneyoshi_stone_directive_flat_control, intergenerational_hazard_knowledge).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone directive solves the intergenerational hazard-memory problem: how to preserve knowledge of tsunami risk across generations when individual memory fades and oral tradition becomes unreliable. The constraint coordinates settlement patterns away from tsunami-vulnerable zones by encoding hazard knowledge in landscape and material culture.
% TRANSFER_FUNCTION: The constraint transfers hazard knowledge from the 1933 survivors to contemporary and future generations. It moves attention and awareness from the abstract (tsunami risk) to the concrete (the stone marker and its location). It restricts development rights on lower-elevation land, transferring those rights to higher-elevation sites.
% ABSENT_VOICES: Voices absent from the constraint's original formulation include: (1) lower-elevation landowners who might have preferred to develop their property, (2) future generations who might face different hazard conditions due to climate change or coastal erosion, (3) non-human entities (ecosystems, marine life) affected by settlement patterns. The constraint was formulated by 1933 survivors and their immediate descendants; it does not include input from those who would be restricted by it or those who might benefit from alternative land-use patterns.
% DISAPPEARANCE_RATIONALE: If the stone directive disappeared overnight, the world would partially rearrange itself. Lower-elevation land would become available for development, potentially increasing settlement in tsunami-vulnerable zones. However, modern tsunami-warning systems and building codes would still constrain development in hazardous areas, so the rearrangement would be partial rather than complete. The constraint's functional role has been displaced by modern systems, so its disappearance would have less impact than it would have had in 1933. Some parties (lower-elevation landowners) would benefit from its disappearance; others (coastal households, disaster anthropologists) would lose the cultural artifact and intergenerational knowledge mechanism.
% FOUNDING_PROBLEM: The 1933 Sanriku tsunami killed approximately 3,000 people and destroyed thousands of homes. Survivors recognized that future generations would lack knowledge of the tsunami's extent and danger. They placed the stone inscription at the maximum extent of the tsunami's reach to create a permanent marker that would preserve hazard knowledge across generations and coordinate settlement patterns away from vulnerable zones.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (intergenerational hazard-memory loss) is corroborated by disaster anthropologists and historians who document how communities lose hazard awareness over time. However, the problem's contemporary status is contested: modern tsunami-warning systems and building codes now provide hazard information that the stone originally supplied. Some argue the founding problem is dead (modern systems have solved it); others argue it remains live (the stone provides cultural continuity and backup knowledge that modern systems cannot replace). The corroboration comes from both the disaster anthropology community (who study hazard memory) and from municipal planners (who maintain the stone in zoning codes despite modern systems).
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive_flat_control, contested).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL HOUSEHOLD (ROPE) — Powerless agents at generational time horizon experience the stone directive as genuine coordination: it solves the collective-action problem of hazard memory across generations. The constraint coordinates intergenerational knowledge transfer without coercive overhead. Exit is constrained (relocation is costly) but the constraint itself is not extractive — it benefits the household by preserving survival knowledge. Low experienced extraction because the coordination function is real and the household is a net beneficiary of hazard avoidance.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: INDIVIDUAL LANDOWNER (ROPE) — Moderate power agent at biographical time horizon with mobile exit options. The stone directive constrains development on lower-elevation land but does not extract — it coordinates hazard avoidance with minimal coercive overhead. The landowner can relocate or develop elsewhere; the constraint is a coordination signal, not a trap. The directive benefits the landowner by reducing tsunami risk. Experienced extraction is low because the constraint solves a genuine coordination problem (where to safely build) without asymmetric cost allocation.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: MUNICIPAL GOVERNMENT (TANGLED_ROPE) — Organized institutional actor at biographical time horizon. The stone directive coordinates land-use planning (genuine coordination function: preventing settlement in tsunami-vulnerable zones) but also constrains municipal revenue from lower-elevation development. The municipality benefits from hazard mitigation but bears costs from restricted buildable land. Requires active enforcement through zoning compliance and cultural transmission of the directive's meaning. Experienced extraction is moderate because the constraint has both coordination and asymmetric cost components — the municipality coordinates hazard avoidance but also loses development revenue.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DISASTER ANTHROPOLOGY COMMUNITY (ROPE) — Institutional analytical observer at civilizational time horizon. The stone directive is a pure coordination mechanism: it solves the intergenerational hazard-memory problem through material inscription and cultural transmission. No extraction is visible from this perspective — the constraint benefits all parties by preserving survival knowledge across generations. The directive is a canonical example of how communities encode hazard memory in landscape and material culture. Experienced extraction is negligible because the coordination function is complete and no party collects asymmetric benefit.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONTEMPORARY MUNICIPAL ZONING AUTHORITY (PITON) — Institutional actor at immediate time horizon. The stone directive persists in contemporary zoning codes and cultural memory, but its functional verification has atrophied. Modern hazard assessment relies on scientific modeling, not stone inscriptions. The directive is maintained through institutional inertia and cultural reverence rather than active enforcement of its original function. Theater ratio is moderate (0.28) because the directive is still cited in planning documents but its actual role in preventing settlement is now performed by building codes and tsunami-warning systems. The piton classification reflects that the constraint's primary function (hazard memory) is now redundant with modern systems, yet the directive persists as a cultural artifact.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the stone directive appears to encode an immutable natural fact: the tsunami hazard boundary is a physical limit that persists regardless of human preference or enforcement. The constraint emerges naturally from the geography and tsunami dynamics of the Sanriku coast. However, this perspective risks naturalizing what is actually a contingent institutional arrangement — the stone's location was chosen by the 1933 survivors based on their experience, not by geological law. The engine will likely compute this as a false summit, revealing that the 'natural hazard boundary' framing naturalizes a historically-specific institutional choice.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(aneyoshi_stone_directive_flat_control, TR),
    TR >= 0.70.

:- end_tests(aneyoshi_stone_directive_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The stone directive is primarily a coordination mechanism with minimal extraction. The constraint solves the genuine collective-action problem of hazard memory across generations. Extractiveness rises slightly over time (0.08 → 0.18 → 0.15) as development pressure increases and the restriction on lower-elevation land becomes more costly, but remains low because the coordination benefit (hazard avoidance) outweighs the development restriction cost. Suppression (0.35): Moderate. The constraint operates through cultural transmission and institutional embedding rather than coercive enforcement. Suppression is highest immediately after the 1933 tsunami (0.60) when survivor testimony and collective trauma enforce compliance, and declines over time (0.35) as the hazard becomes temporally distant and modern warning systems provide alternative hazard information. Theater ratio (0.28): Low-moderate. The stone directive is primarily functional (hazard memory) with modest performative content. Theater increases over time (0.10 → 0.28) as the constraint's functional role is displaced by modern systems and it becomes maintained through cultural reverence and institutional inertia rather than active hazard coordination. The low theater ratio reflects that the constraint remains substantially functional even in contemporary contexts — it is not yet a pure piton, though the trajectory suggests increasing performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence across time horizons and power levels. Households at generational time horizon see pure coordination (rope) — the stone solves the intergenerational hazard-memory problem. Individual landowners at biographical time horizon also see coordination (rope) — the constraint benefits them by reducing tsunami risk. The municipal government at biographical time horizon sees mixed coordination-extraction (tangled_rope) — it coordinates hazard mitigation but loses development revenue. The analytical observer at civilizational time horizon sees pure coordination (rope) — the constraint is a canonical example of how communities encode hazard memory. The contemporary zoning authority at immediate time horizon sees degraded ritual (piton) — the constraint persists through cultural reverence but its functional role has been displaced by modern systems. The natural law perspective risks seeing an immutable hazard boundary (mountain), but this is likely a false summit — the stone's location was chosen by survivors based on their experience, not by geological law. The perspectival gap reveals that the constraint's classification depends critically on time horizon: at generational and civilizational scales, it is pure coordination; at immediate and biographical scales, it shows extraction and performative maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is derived from beneficiary declarations and exit options. Coastal households are beneficiaries with constrained exit (relocation is costly) — they experience low d because the constraint benefits them. Individual landowners are beneficiaries with mobile exit (they can develop elsewhere) — they experience very low d because they have exit options and the constraint benefits them. The municipal government is a mixed actor: it benefits from hazard mitigation but loses development revenue — it experiences moderate d (0.5-0.6) because the constraint has both coordination and asymmetric cost components. The analytical observer is a beneficiary with arbitrage exit (they can study other constraints) — they experience near-zero d because the constraint is pure coordination. The contemporary zoning authority experiences moderate d because it maintains the constraint through institutional inertia while its functional role has been displaced. The directionality derivation reflects that the constraint's primary function (hazard coordination) benefits all parties, but secondary effects (development restriction, institutional maintenance) create modest asymmetries that increase over time as the hazard becomes temporally distant.
 *
 * MANDATROPHY ANALYSIS:
 *   The stone directive does not exhibit mandatrophy in the classical sense — its mandate (preserve intergenerational hazard memory) remains live and functional. However, the constraint shows signs of functional displacement: modern tsunami-warning systems and building codes now perform the hazard-coordination function that the stone originally provided. The constraint persists through cultural reverence and institutional embedding in zoning codes rather than through active functional necessity. This is the piton trajectory: a constraint whose primary function has atrophied but which persists through institutional inertia and cultural maintenance. The constraint's mandate has not become obsolete (hazard memory remains necessary), but the mechanism for fulfilling that mandate has shifted from the stone inscription to modern systems. The theater ratio trajectory (0.10 → 0.28) reflects this functional displacement — the constraint is increasingly maintained as a cultural artifact rather than as an active hazard-coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stone_location_empirical_basis,
    'Was the stone''s location chosen based on systematic observation of the 1933 tsunami''s maximum extent, or based on survivor testimony and collective memory?',
    'Historical analysis of 1933 Sanriku tsunami records; comparison of stone location with documented tsunami run-up measurements; interviews with descendants of survivors about location-selection process',
    'If systematic observation: the stone encodes empirical hazard data and the mountain perspective gains credibility. If testimony-based: the stone encodes social memory, not natural law, and the rope/tangled_rope perspectives are more accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stone_location_empirical_basis, empirical, 'Empirical basis for stone location selection').

omega_variable(
    intergenerational_transmission_mechanism,
    'How does the stone directive persist across generations? Through active cultural transmission, institutional embedding in zoning codes, or passive landscape presence?',
    'Ethnographic study of how contemporary residents learn about the stone; analysis of zoning code language and its relationship to the stone inscription; measurement of awareness rates across age cohorts',
    'If active transmission: the rope classification is robust — genuine coordination mechanism. If passive/institutional: the piton classification is more accurate — the constraint persists through inertia rather than functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Mechanism of intergenerational transmission of the stone directive').

omega_variable(
    modern_hazard_assessment_displacement,
    'Has the stone directive been functionally displaced by modern tsunami-warning systems and building codes, or does it remain the primary hazard-memory mechanism?',
    'Analysis of municipal planning documents; comparison of settlement patterns before and after modern warning systems; interviews with planners about decision-making process for development approvals',
    'If displaced: piton classification is correct — the constraint is maintained through cultural reverence but no longer performs its original function. If still primary: rope classification is correct — the constraint remains functionally central to hazard coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_hazard_assessment_displacement, empirical, 'Functional displacement of stone directive by modern hazard systems').

omega_variable(
    false_summit_naturalization_risk,
    'Does the ''natural hazard boundary'' framing naturalize what is actually a historically-contingent institutional choice?',
    'Comparison of stone location with geological/geomorphological hazard boundaries; analysis of how the stone is described in planning documents (natural law vs. institutional memory); examination of whether the stone''s authority derives from its physical location or from its cultural-historical status',
    'If naturalization occurs: the mountain perspective is a false summit, and the constraint should be reclassified as rope or tangled_rope. If the stone genuinely encodes natural law: the mountain perspective is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization_risk, conceptual, 'Risk of naturalizing contingent institutional choice as immutable hazard boundary').

omega_variable(
    extraction_through_development_restriction,
    'Does the stone directive extract value from lower-elevation landowners by restricting their development options, or is the restriction a genuine coordination benefit?',
    'Economic analysis of land values above and below the stone line; comparison of development patterns in restricted vs. unrestricted zones; assessment of whether restricted landowners receive compensation or alternative development opportunities',
    'If extraction occurs: the tangled_rope classification is more accurate than rope — the constraint has both coordination and asymmetric cost components. If no extraction: the rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_through_development_restriction, empirical, 'Whether development restriction constitutes extraction from lower-elevation landowners').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive_flat_control, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_tr_t0, aneyoshi_stone_directive_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aneyoshi_tr_t20, aneyoshi_stone_directive_flat_control, theater_ratio, 20, 0.15).
narrative_ontology:measurement(aneyoshi_tr_t40, aneyoshi_stone_directive_flat_control, theater_ratio, 40, 0.22).
narrative_ontology:measurement(aneyoshi_tr_t60, aneyoshi_stone_directive_flat_control, theater_ratio, 60, 0.28).
narrative_ontology:measurement(aneyoshi_tr_t80, aneyoshi_stone_directive_flat_control, theater_ratio, 80, 0.28).

% Extraction over time
narrative_ontology:measurement(aneyoshi_be_t0, aneyoshi_stone_directive_flat_control, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(aneyoshi_be_t20, aneyoshi_stone_directive_flat_control, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(aneyoshi_be_t40, aneyoshi_stone_directive_flat_control, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(aneyoshi_be_t60, aneyoshi_stone_directive_flat_control, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(aneyoshi_be_t80, aneyoshi_stone_directive_flat_control, base_extractiveness, 80, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_su_t0, aneyoshi_stone_directive_flat_control, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(aneyoshi_su_t20, aneyoshi_stone_directive_flat_control, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(aneyoshi_su_t40, aneyoshi_stone_directive_flat_control, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(aneyoshi_su_t60, aneyoshi_stone_directive_flat_control, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(aneyoshi_su_t80, aneyoshi_stone_directive_flat_control, suppression_requirement, 80, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive_flat_control, information_standard).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone directive is a single constraint story representing the flat construction of the substrate. It is not decomposed into readings or alternative framings. The constraint's classification varies across perspectives due to differences in time horizon, power level, and exit options, not due to contested readings of a kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
