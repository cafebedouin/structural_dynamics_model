% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_directive_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Directive
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi tsunami stone is a physical inscription placed by survivors
 *   of the 1933 Sanriku tsunami, reading 'High dwellings are the peace and
 *   harmony of our descendants. Remember the calamity of the great tsunamis.
 *   Do not build any homes below this point.' The stone marks an elevation
 *   threshold above which the village rebuilt. This constraint is a
 *   diagnostic case for distinguishing coordination from naturalization: the
 *   stone's directive solves a genuine collective action problem
 *   (coordinating settlement patterns to avoid tsunami risk) with minimal
 *   extractive overhead, yet the 1933 survivors who placed it experienced the
 *   threshold as a discovered natural law rather than a constructed social
 *   norm. The constraint exhibits stable low extraction and low theater
 *   across 90 years, with a modest uptick in theater as generational distance
 *   increases and the stone becomes partly commemorative. The 2011 Tohoku
 *   tsunami did not reach Aneyoshi, vindicating the stone's threshold and
 *   resetting extractiveness slightly downward (residents experienced renewed
 *   confidence in the coordination rather than increased cost). The stone is
 *   a rare example of a constraint that all perspectives classify as rope
 *   except the immediate survivors, who saw it as mountain — a perspectival
 *   gap driven entirely by time horizon and exit options, not by power
 *   asymmetry.
 *
 * KEY AGENTS:
 *   - Aneyoshi Residents: Primary beneficiaries (powerless to moderate / constrained to mobile depending on generation) — benefit from coordinated settlement above tsunami risk threshold; bear modest cost of building on higher, less convenient land
 *   - Future Generations: Secondary beneficiaries (powerless / trapped at time of potential disaster, but protected by ancestors' coordination) — the stone's explicit purpose is intergenerational risk transfer
 *   - 1933 Survivors: Original authors (powerless / trapped / immediate) — experienced the stone's threshold as natural law in the immediate aftermath; their perspective is the mountain classification
 *   - Regional and National Agencies: Institutional beneficiaries (institutional / arbitrage) — benefit from the stone as a model of low-cost disaster risk reduction without bearing enforcement costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_directive_flat_control, 0.12).
domain_priors:suppression_score(aneyoshi_stone_directive_flat_control, 0.25).
domain_priors:theater_ratio(aneyoshi_stone_directive_flat_control, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, extractiveness, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(aneyoshi_stone_directive_flat_control, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_directive_flat_control, rope).
narrative_ontology:human_readable(aneyoshi_stone_directive_flat_control, "Aneyoshi Tsunami Stone Land-Use Directive").
narrative_ontology:topic_domain(aneyoshi_stone_directive_flat_control, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(aneyoshi_stone_directive_flat_control, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, aneyoshi_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, aneyoshi_residents_contemporary).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_directive_flat_control, regional_development_planners).
narrative_ontology:constraint_victim(aneyoshi_stone_directive_flat_control, aneyoshi_residents_1933).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive_flat_control, embodied_memory_persistence).
narrative_ontology:constraint_vindicates(aneyoshi_stone_directive_flat_control, local_knowledge_superiority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Survivors of the 1933 Sanriku tsunami who placed the stone. They bear the immediate cost of rebuilding on higher, less convenient ground. Trapped by trauma, geography, and lack of resources to relocate. They experience the stone's threshold as a natural law — the discovered boundary of the ocean's reach — rather than a human choice.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, aneyoshi_residents_1933, payer,
    powerless, immediate, trapped, local).

% Contemporary residents (2020s) who inherit the stone's coordination function. They benefit from living above the tsunami risk threshold without needing to independently assess the risk. Constrained by community ties and property investment, but not trapped — they could leave if they chose. They experience the stone as coordination, not compulsion.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, aneyoshi_residents_contemporary, beneficiary,
    moderate, biographical, constrained, local).

% Descendants not yet born at the time of the stone's placement. They benefit from the stone's protection without bearing the cost of the 1933 decision. Trapped in the sense that they inherit the constraint without choosing it, but the constraint protects rather than extracts from them. The stone's explicit purpose is to benefit this group.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Government planners who study and promote the stone as a model of low-cost disaster risk reduction. They benefit from the stone's success (it reduces disaster exposure without requiring enforcement infrastructure) and can point to Aneyoshi as a best practice. Full exit options — they can work elsewhere and are not bound by the stone's directive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, regional_development_planners, beneficiary,
    institutional, generational, arbitrage, regional).

% Researchers who study the stone as a case of embodied institutional memory and intergenerational risk communication. They neither benefit from nor pay for the stone's function. They analyze the constraint's stability, transmission mechanisms, and effectiveness across time.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_directive_flat_control, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone solves a collective action problem in settlement patterns. Without coordination, individual residents face pressure to build in low-lying areas (cheaper land, closer to water, social proximity to existing structures). Each individual's choice to build low increases risk for everyone (clustered settlement in the inundation zone). The stone provides a Schelling point that coordinates everyone to safer ground, making the collectively rational choice (build high) individually rational as well.
% TRANSFER_FUNCTION: The stone transfers risk knowledge and settlement coordination across generations. The 1933 survivors transfer their trauma-derived knowledge (the ocean can reach this high) to descendants who did not experience the tsunami. The constraint moves the cost of risk avoidance (building on higher ground) from a decision each generation must make independently to a stable inherited norm. No monetary or status transfer — the stone extracts from no one.
% ABSENT_VOICES: Future coastal residents who might prefer to build below the stone line for economic or convenience reasons are not in the 1933 conversation. The stone's placement assumes their preferences should be overridden by safety considerations. However, this is not extractive absence — the excluded voices are protected by the constraint rather than exploited by it. The stone's directive is paternalistic (ancestors deciding for descendants) but not extractive (no one collects rents from the constraint).
% DISAPPEARANCE_RATIONALE: If the stone disappeared overnight, settlement patterns would gradually drift downward over generations as memory of the 1933 tsunami faded. Individual residents would face pressure to build closer to the water (economic, social, convenience factors), and without the stone's Schelling point, the collectively dangerous pattern would re-emerge. The 2011 Tohoku tsunami did not reach Aneyoshi, which could accelerate this drift if the stone were absent — residents might interpret the 2011 event as evidence that the risk is overstated. The world rearranges because the stone is doing real coordination work, not because it extracts from anyone.
% FOUNDING_PROBLEM: The founding problem is the 1933 Sanriku tsunami, which killed thousands along the Tohoku coast and destroyed Aneyoshi village. Survivors faced the immediate problem of where to rebuild. The stone was placed to solve two problems: (1) coordinate the current generation's rebuilding above the inundation line, and (2) transmit this knowledge to future generations who would not experience the tsunami directly. The founding problem is the combination of immediate disaster response and long-term intergenerational risk communication.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (tsunami risk on the Sanriku coast) remains live. The 2011 Tohoku tsunami killed over 18,000 people in the region, demonstrating that the risk persists. Aneyoshi village was not inundated in 2011, vindicating the stone's threshold. Corroboration comes from: (1) seismological and geological evidence that the subduction zone remains active, (2) historical records of recurring tsunamis (1896, 1933, 2011), and (3) government disaster agencies that continue to designate the coast as high-risk. The stone's founding problem is corroborated by sources outside the beneficiary set (scientific consensus, government risk assessment, historical record).
narrative_ontology:disappearance_verdict(aneyoshi_stone_directive_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_directive_flat_control, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTEMPORARY RESIDENT (ROPE) — Experiences the stone directive as coordination. The constraint solves a genuine collective action problem: without the stone, individual residents face pressure to build in dangerous low-lying areas (cheaper land, closer to water, social proximity). The stone provides a Schelling point that coordinates everyone to safer ground. Extraction is minimal — residents benefit from the coordination and bear only the modest cost of building higher. Exit is constrained by geography and community ties, but the constraint itself is not the binding force.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL PLANNER (ROPE) — Sees the stone directive as an effective low-cost coordination mechanism. No active enforcement infrastructure needed — the stone's physical presence and cultural transmission do the work. Compared to formal zoning with legal penalties, this is pure coordination with negligible overhead. The planner has exit options (can work elsewhere) and sees the constraint as a model of efficient risk communication.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: 1933 SURVIVOR IMMEDIATE (MOUNTAIN) — In the immediate aftermath of the 1933 Sanriku tsunami, the stone directive appears as natural law. The survivors who placed the stone experienced the tsunami as an immutable force of nature, and the elevation threshold as a discovered physical fact. From this perspective, the constraint is not a human construction but a recognition of where the ocean's reach ends. This is the perspective that authored the stone, and from their immediate/trapped context, it reads as mountain.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: NATIONAL AGENCY (ROPE) — Institutional actors with arbitrage exit see the stone as a successful coordination technology. The agency benefits from the stone's function (reduced disaster exposure without enforcement costs) and can point to Aneyoshi as a model. The constraint coordinates local behavior without requiring national resources. Net beneficiary with full exit options.
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical perspective, the stone directive is a coordination mechanism that solves a genuine collective action problem with minimal extractive overhead. The constraint persists because it works, not because anyone enforces it. The stone's physical durability and cultural transmission create a stable Schelling point. Extraction is low (modest cost of building higher), suppression is low (no penalties for non-compliance, only social pressure and self-interest), and the coordination function is genuine (prevents individually rational but collectively dangerous settlement patterns).
constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_stone_directive_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aneyoshi_stone_directive_flat_control, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(aneyoshi_stone_directive_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.12): Low. The stone imposes a modest cost on residents — building on higher ground is less convenient, land may be more expensive or scarce, and proximity to the water (for fishing communities) is reduced. But this cost is far outweighed by the benefit of tsunami protection, and the cost is borne roughly equally by all residents (no asymmetric extraction). The slight increase over time (0.08 to 0.12) reflects increasing land scarcity and development pressure as the village grows, making the constraint's opportunity cost more visible. The 2011 dip reflects renewed confidence after the stone's threshold was vindicated. Suppression (0.25): Low-moderate. The stone has no formal enforcement mechanism — no legal penalties for building below the line, no active monitoring. Suppression comes entirely from social pressure and self-interest. A resident could theoretically build below the stone, but would face community disapproval and personal risk. The suppression is real but not coercive. Theater ratio (0.15): Very low. The stone's function is almost entirely real — it coordinates settlement patterns and transmits risk knowledge. The modest theater component (rising from 0.05 to 0.15 over 90 years) reflects the stone's gradual acquisition of commemorative and touristic functions as generational distance increases. By 2023, the stone is partly a historical monument, but its primary function (land-use coordination) remains intact.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is temporal, not structural. The 1933 survivors in the immediate aftermath saw the stone as mountain — a recognition of natural law, the discovered boundary of the ocean's reach. All other perspectives, from biographical time onward, see rope — a coordination mechanism that solves a collective action problem. This gap is driven by time_horizon and exit_options: immediate/trapped agents experience constraints as immutable even when they are constructed, while biographical/constrained agents see the same constraint as mutable coordination. The stone is the same physical object with the same function, but the survivors' trauma and lack of exit options made the threshold feel like a law of nature. As generational distance increases and exit options improve (younger generations are more mobile), the mountain perspective disappears entirely and rope becomes universal. There is no power-based perspectival gap — powerless and institutional agents agree on the classification. The stone is a pure coordination success story, with the mountain classification appearing only in the founding moment.
 *
 * DIRECTIONALITY LOGIC:
 *   All agent groups are net beneficiaries of the stone's coordination function. Aneyoshi residents benefit from coordinated tsunami avoidance; future generations benefit from inherited safety; institutional actors benefit from a model of low-cost risk reduction. No agent group is a victim — the stone extracts from no one. The modest extractiveness (0.12) represents the opportunity cost of the constraint (less convenient land, reduced water access) rather than asymmetric extraction. Directionality values are uniformly low across all agents: residents with constrained exit options experience slightly higher effective extraction than institutional actors with arbitrage options, but all values are near the beneficiary end of the scale. The 1933 survivors are a special case: their immediate/trapped context produces a mountain classification not because of high extraction but because of perceived immutability — they experienced the threshold as a natural boundary, not a human choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The stone directive resolves mandatrophy by demonstrating that low extraction and low suppression can persist across civilizational time when the coordination function is genuine and the constraint is self-enforcing. The stone does not require active enforcement because compliance is individually rational (residents benefit from avoiding tsunami risk) and socially reinforced (community memory and peer pressure). The constraint's mandate (protect descendants from tsunami) has not outlived its function — the risk persists, and the stone continues to coordinate settlement patterns effectively. The modest increase in theater over time reflects the stone's acquisition of secondary commemorative functions, but the primary coordination function remains strong. This is rope, not piton: the function has not atrophied, and the performance is minimal. The stone is a model of how coordination constraints can remain stable and low-extraction when they solve real problems without creating enforcement rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalization_ambiguity,
    'Is the stone directive a discovered natural law (the ocean''s reach is a physical fact) or a constructed social norm (the elevation threshold is a human interpretation of risk tolerance)?',
    'Historical analysis of tsunami inundation patterns vs stone placement; comparison with other coastal communities'' risk thresholds; examination of whether the stone''s elevation represents maximum observed inundation or a safety margin chosen by survivors.',
    'If natural law: the 1933 survivor perspective (mountain) is correct, and the stone merely records a physical boundary. If constructed norm: all perspectives converge on rope, and the stone is a coordination technology that could have been placed higher or lower based on different risk preferences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_ambiguity, conceptual, 'Whether the stone directive is discovered natural law or constructed social norm').

omega_variable(
    compliance_mechanism_ambiguity,
    'Does the stone''s effectiveness depend on cultural transmission and social pressure (coordination) or on internalized fear and trauma memory (psychological compulsion)?',
    'Ethnographic study of compliance motivations across generations; comparison of compliance rates between families with direct tsunami experience vs those without; analysis of what happens when economic pressure (land scarcity, development incentives) conflicts with the stone''s directive.',
    'If coordination: rope classification is correct across all non-immediate perspectives. If psychological compulsion: suppression is higher than measured, and some perspectives should classify as constrained rather than pure coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_mechanism_ambiguity, empirical, 'Whether compliance is coordinated or psychologically compelled').

omega_variable(
    generational_decay_trajectory,
    'Will the stone''s directive maintain its force as generational distance from 1933 increases, or will it decay into a historical curiosity as living memory fades?',
    'Longitudinal tracking of compliance rates and cultural salience across generations; comparison with other disaster memorial sites; analysis of whether the 2011 Tohoku tsunami (which did not reach Aneyoshi) reinforced or undermined the stone''s authority.',
    'If decay: the constraint is scaffold-like (temporary coordination during the memory window) rather than rope (stable coordination). If persistent: the stone has achieved stable transmission and the rope classification holds at civilizational timescales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_decay_trajectory, empirical, 'Whether the stone''s authority persists or decays across generations').

omega_variable(
    counterfactual_enforcement,
    'If a resident chose to build below the stone line, would the community intervene through social pressure, legal mechanisms, or not at all?',
    'Ethnographic interviews about hypothetical violations; examination of any historical instances of non-compliance; analysis of whether the stone''s directive has been codified into formal zoning law or remains purely customary.',
    'If no intervention: suppression is even lower than measured, pure coordination. If social pressure: measured suppression (0.25) is accurate. If legal mechanisms exist: the constraint has been formalized and suppression is higher, potentially shifting some perspectives toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counterfactual_enforcement, empirical, 'What enforcement mechanisms exist for the stone''s directive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_directive_flat_control, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_theater_1933, aneyoshi_stone_directive_flat_control, theater_ratio, 0, 0.05).
narrative_ontology:measurement(aneyoshi_theater_1953, aneyoshi_stone_directive_flat_control, theater_ratio, 20, 0.1).
narrative_ontology:measurement(aneyoshi_theater_1973, aneyoshi_stone_directive_flat_control, theater_ratio, 40, 0.12).
narrative_ontology:measurement(aneyoshi_theater_1993, aneyoshi_stone_directive_flat_control, theater_ratio, 60, 0.15).
narrative_ontology:measurement(aneyoshi_theater_2011, aneyoshi_stone_directive_flat_control, theater_ratio, 78, 0.15).
narrative_ontology:measurement(aneyoshi_theater_2023, aneyoshi_stone_directive_flat_control, theater_ratio, 90, 0.15).

% Extraction over time
narrative_ontology:measurement(aneyoshi_extract_1933, aneyoshi_stone_directive_flat_control, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(aneyoshi_extract_1953, aneyoshi_stone_directive_flat_control, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(aneyoshi_extract_1973, aneyoshi_stone_directive_flat_control, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(aneyoshi_extract_1993, aneyoshi_stone_directive_flat_control, base_extractiveness, 60, 0.12).
narrative_ontology:measurement(aneyoshi_extract_2011, aneyoshi_stone_directive_flat_control, base_extractiveness, 78, 0.1).
narrative_ontology:measurement(aneyoshi_extract_2023, aneyoshi_stone_directive_flat_control, base_extractiveness, 90, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_directive_flat_control, information_standard).

% DUAL FORMULATION NOTE:
% The stone directive is a single constraint with no decomposition. The naturalization ambiguity (omega: is the threshold a discovered natural law or a constructed norm?) is handled as a perspectival gap (1933 survivors see mountain, all others see rope) rather than as separate constraint stories. The stone's physical and social functions are inseparable — the inscription is both the coordination technology and the cultural transmission mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
