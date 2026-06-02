% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__imperial_uniformity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__imperial_uniformity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nicene_creed_authority__imperial_uniformity_reading
 *   human_readable: Nicene Creed Authority: Imperial Uniformity Reading
 *   domain: religious_history/political_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The Council of Nicaea (325 CE) established a creedal standard ('same
 *   substance' — homoousios — of Father and Son) enforced through imperial
 *   and ecclesiastical authority to eliminate theological diversity and
 *   consolidate unified Christian identity under imperial governance. This
 *   constraint instantiates the imperial uniformity reading: the creed
 *   functions as a state-backed mechanism to suppress doctrinal dissent
 *   (Arianism, Nestorianism, Monophysitism, etc.) through excommunication,
 *   property seizure, exile, and execution. The empirical observable is the
 *   apparatus of enforcement — imperial edicts, episcopal councils,
 *   confiscation of non-Nicene church properties, and violent suppression of
 *   dissenting communities. The reading's core claim: the creed's primary
 *   function is consolidating imperial/ecclesiastical power through enforced
 *   doctrinal uniformity, not preserving theological truth. Victims include
 *   theological dissenters (Arians, Copts, Nestorians) who faced existential
 *   pressure to conform or be eliminated. Beneficiaries include the imperial
 *   hierarchy (state gains ecclesiastical control and eliminates sectarian
 *   fragmentation) and the Orthodox ecclesiastical establishment (bishops
 *   aligned with Nicene standard gain property, authority, and imperial
 *   backing). This reading is one of three interpretations of the contested
 *   Nicene kernel: the confessional reading emphasizes legitimate
 *   preservation of theological truth; the boundary-maintenance reading
 *   emphasizes necessary group identity formation; this imperial uniformity
 *   reading emphasizes extraction through enforced doctrinal uniformity
 *   grounded in power consolidation. The three readings coexist as live
 *   historical positions across different parties' interpretive frameworks.
 *
 * KEY AGENTS:
 *   - Theological Dissenters (Arians, Nestorians, Monophysites, Monotheletes): Primary victims (powerless/trapped) — face existential pressure to recant, accept exile, or risk execution; no exit that preserves theological integrity
 *   - Non-Aligned Regional Bishops: Secondary victims (moderate/constrained) — ecclesiastical authority in peripheral regions holding alternative theologies; face pressure through imperial authority and ecclesiastical isolation
 *   - Orthodox Ecclesiastical Hierarchy (Nicene bishops, church councils): Primary beneficiaries (institutional/arbitrage) — consolidated authority, property control, imperial backing, unified governance structure
 *   - Imperial Authority (Constantine I and successors): Secondary beneficiary (institutional/arbitrage) — achieves unified ecclesiastical hierarchy for governance purposes; eliminates sectarian fragmentation that destabilizes empire
 *   - Dissenting Theological Networks (Coptic, Armenian, Nestorian communities): Organized resistance (organized/constrained) — develop institutional structures and theological traditions that persist through suppression; constitute genuine coordination function despite persecution
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the Nicene apparatus as initially functional enforcement mechanism (genuine snare) that degrades over time into performative ritual (piton) as theological settlement stabilizes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__imperial_uniformity_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__imperial_uniformity_reading, 0.78).
domain_priors:theater_ratio(nicene_creed_authority__imperial_uniformity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__imperial_uniformity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__imperial_uniformity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nicene_creed_authority__imperial_uniformity_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__imperial_uniformity_reading, snare).
narrative_ontology:human_readable(nicene_creed_authority__imperial_uniformity_reading, "Nicene Creed Authority: Imperial Uniformity Reading").
narrative_ontology:topic_domain(nicene_creed_authority__imperial_uniformity_reading, "religious_history/political_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_creed_authority__imperial_uniformity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__imperial_uniformity_reading, '172a577a-06d5-42e3-82e3-69cf291f7898').
narrative_ontology:cs_kernel_codification('172a577a-06d5-42e3-82e3-69cf291f7898', formalized).
narrative_ontology:cs_authority_grounding('172a577a-06d5-42e3-82e3-69cf291f7898', extraction).
narrative_ontology:cs_interpretation_layer_present('172a577a-06d5-42e3-82e3-69cf291f7898').
narrative_ontology:cs_reading_relation('172a577a-06d5-42e3-82e3-69cf291f7898', nicene_creed_authority__confessional_reading, coexists_with).
narrative_ontology:cs_reading_relation('172a577a-06d5-42e3-82e3-69cf291f7898', nicene_creed_authority__boundary_maintenance_reading, influences).
narrative_ontology:cs_axiom('172a577a-06d5-42e3-82e3-69cf291f7898', foundational, uniformity_as_power_consolidation).
narrative_ontology:cs_axiom_status(uniformity_as_power_consolidation, holdable).
narrative_ontology:cs_axiom_grounding('172a577a-06d5-42e3-82e3-69cf291f7898', uniformity_as_power_consolidation, empirically_contingent).
narrative_ontology:cs_axiom('172a577a-06d5-42e3-82e3-69cf291f7898', foundational, doctrinal_enforcement_extraction_mechanism).
narrative_ontology:cs_axiom_status(doctrinal_enforcement_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('172a577a-06d5-42e3-82e3-69cf291f7898', doctrinal_enforcement_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('172a577a-06d5-42e3-82e3-69cf291f7898', imperial_ecclesiastical_uniformity_framework).
narrative_ontology:cs_drift_state('172a577a-06d5-42e3-82e3-69cf291f7898', post_chalcedon_settlement_451_ce, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('172a577a-06d5-42e3-82e3-69cf291f7898', '2026-02-27T14:32:00Z').
narrative_ontology:cs_kernel_id(nicene_creed_authority__imperial_uniformity_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__imperial_uniformity_reading, imperial_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__imperial_uniformity_reading, orthodox_ecclesiastical_establishment).
narrative_ontology:constraint_victim(nicene_creed_authority__imperial_uniformity_reading, theological_dissenters).
narrative_ontology:constraint_victim(nicene_creed_authority__imperial_uniformity_reading, non_chalcedonian_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__imperial_uniformity_reading, arian_confessors).
narrative_ontology:constraint_victim(nicene_creed_authority__imperial_uniformity_reading, monothelete_theologians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THEOLOGICAL DISSENTER (SNARE) — Faces structural elimination: recant doctrine, accept exile, or risk execution. No exit path that preserves both life and theological integrity. The constraint's suppression machinery (anathema, confiscation, exile, execution) operates without meaningful negotiation or alternative status. Maximum experienced extraction through forced conformity or physical elimination.
constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: REGIONAL BISHOP NON-ALIGNED (SNARE) — Ecclesiastical authority in peripheral regions may hold alternative theological positions (Arian, Nestorian, Monophysite). Faces pressure to conform: loss of appointment, ecclesiastical isolation, or pressure from imperial agents. Exit is theoretically available (embrace the Nicene standard) but at the cost of abandoning theological conviction and local ecclesiastical autonomy. High suppression, severe career risk.
constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: ORTHODOX ECCLESIASTICAL HIERARCHY (ROPE) — Primary beneficiary. Experiences the Nicene standard as a coordination mechanism: unified doctrine enables unified ecclesiastical governance, resource allocation, and political leverage with the imperial power. The constraint solves a coordination problem (how to adjudicate theological disputes and maintain institutional coherence) while simultaneously benefiting this actor through consolidated authority and property control. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: IMPERIAL AUTHORITY (ROPE) — Secondary beneficiary. Experiences uniformity mandate as coordination mechanism: standardized doctrine reduces sectarian conflict that fragments the empire, enables a unified ecclesiastical hierarchy the state can leverage for legitimacy and governance, and constrains alternative power centers (regional bishops, heterodox communities) that might challenge imperial authority. The constraint is a state-church coordination tool, not primarily extractive from the imperial perspective.
constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: HERETICAL THEOLOGICAL TRADITION (TANGLED ROPE) — Over generations, non-Nicene communities (Arian, Coptic, Nestorian) develop institutional structures, literary traditions, and theological learning that persist through suppression. The constraint extracts persecution costs but also generates coordination benefits: theological debate drives deeper doctrinal articulation, resistance networks develop sophisticated epistemic practices, and dissenting traditions become more intellectually rigorous. High suppression; genuine coordination function among dissenters; mixed extraction.
constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From contemporary distance, the Nicene uniformity apparatus appears largely theatrical after the empire's religious settlement stabilizes (4th-5th centuries). The mechanism persists through institutional inertia: diocesan structures, creedal recitation, and excommunication ritual maintain the apparatus long after the acute theological disputes have been displaced by new controversies (Christological disputes, Filioque). The primary function (preventing schism through uniformity) has degraded into performance of orthodoxy. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: NATURALIZED AUTHORITY (MOUNTAIN CANDIDATE) — This reading treats the Nicene standard as an immutable natural law of theological truth: the creed articulates eternal metaphysical truths about the nature of the Trinity, and enforcing adherence to truth is not extraction but preservation of divine order. Exit options are identity_locked for agents within this frame: to question the creed is to question the foundation of Christian identity itself, making dissent literally unthinkable from within the orthodox epistemic frame. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of what is actually a historical institutional arrangement grounded in power consolidation.
constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__imperial_uniformity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nicene_creed_authority__imperial_uniformity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__imperial_uniformity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nicene_creed_authority__imperial_uniformity_reading, TR),
    TR >= 0.70.

:- end_tests(nicene_creed_authority__imperial_uniformity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts conformity through existential threat (recant doctrine or face elimination). The imperial uniformity reading frames this not as legitimate truth-preservation but as state-backed consolidation of power. Career advancement, property ownership, and physical safety for dissenters depend on conformity to the standard. The extraction is not universal — some regions and periods show enforcement gaps — but systematic enough to constitute high base extractiveness. Suppression (0.78): High. The apparatus includes anathema (ecclesiastical death), confiscation of church properties (economic elimination), exile (geographic removal), and execution (physical elimination). These are severe structural barriers to dissent, and they operate without negotiation or alternative status. Theological conviction offers no protection. Theater ratio (0.55): Moderate. In the acute dispute phase (t=0), the constraint is relatively functional — councils are genuinely adjudicating theological positions, not merely performing orthodoxy. As the constraint matures and the dissent is suppressed, the theater increases: later councils increasingly focus on ritual affirmation of the standard rather than substantive theological engagement. By t=40, the constraint has partly degraded into performative uniformity (theater rises to 0.55) while simultaneously tightening enforcement (suppression rises to 0.82). This pattern suggests the constraint is a snare that accumulates enforcement machinery even as the intellectual foundation becomes increasingly theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. Theological dissenters experience pure extraction (snare) — conformity or elimination with no alternative. Regional bishops experience mixed extraction and constrained choice (snare with some negotiation space). The Orthodox hierarchy experiences coordination (rope) — unified doctrine enables governance. The imperial authority experiences coordination and state control (rope). Dissenting theological networks experience suppression balanced by genuine intellectual coordination (tangled rope) — persecution drives deeper doctrinal work. The analytical observer risks naturalizing the Nicene standard as metaphysical truth (mountain/false summit) or seeing it clearly as institutional power consolidation (snare). This reading specifically instantiates the snare classification: extractive enforcement grounded in power consolidation, not legitimate truth-preservation. The false summit candidate (perspective 7) represents the naturalized authority reading that the engine will detect as constructed rather than natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the constraint. Dissenters face maximum extraction — they are victims with no arbitrage options, trapped by the enforcement apparatus. Their d ≈ 0.95, producing high f(d) ≈ 1.42, yielding maximum experienced extractiveness chi. The Orthodox hierarchy are beneficiaries with arbitrage options (they can shift doctrine and maintain status) — their d ≈ 0.05, producing low f(d) ≈ -0.12, yielding negative chi (the constraint subsidizes them). The imperial authority derives benefit from unified ecclesiastical governance — d ≈ 0.10, producing f(d) ≈ -0.05, yielding negative chi. Regional bishops are intermediate — victims of the conformity pressure but with some negotiation capacity through ecclesiastical status — d ≈ 0.65, producing f(d) ≈ 1.00, yielding moderate-to-high chi. This directionality structure explains why the same constraint classifies as snare from the dissenter perspective (maximum d, maximum chi), rope from the beneficiary perspectives (minimum d, negative chi), and tangled rope from the organized dissenter network perspective (moderate d, mixed benefits and costs).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that it is genuinely extractive (snare from multiple perspectives: powerless dissenters, moderate regional bishops, organized dissenting networks) while also serving some coordination functions from the beneficiary side (rope from imperial and Orthodox perspectives). The imperial uniformity reading explicitly rejects the confessional reading's claim that enforcement is legitimate truth-preservation. Instead, the reading treats the Nicene standard as a contingent institutional arrangement grounded in power consolidation that deployed theological legitimacy as justification. The snare classification is held not because the constraint lacks coordination benefits for some agents (it does), but because from the perspective of dissenters, it is pure extraction without negotiable exit. The constraint fails the tangled rope requirements (mixed extraction and genuine coordination from both perspectives) because the asymmetry is too extreme: dissenters experience no coordination benefit, only suppression. The false summit perspective (perspective 7) represents the risk that treating the creed as immutable metaphysical truth naturalizes what is actually a contingent, extractive institutional arrangement. The engine's false summit detector will identify this: beneficiaries are declared, the mountain gates fail, and the constraint reclassifies as snare/tangled rope depending on suppression intensity. This reveals the ideological function of naturalizing power arrangements as eternal truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_truth_vs_institutional_power,
    'Does the Nicene standard represent authentic theological truth whose enforcement is justified preservation of faith, or is it a contingent institutional arrangement grounded in imperial power consolidation that used theological legitimacy as cover?',
    'Historical analysis of the Council''s composition (imperial control, geographic bias toward Eastern episcopal influence), the Edict of Milan/Constantinople (state machinery of enforcement), and pre-Nicene theological diversity (evidence that non-Nicene positions were intellectually coherent, not obviously false). Comparison with later Christological disputes (Chalcedon, etc.) showing that ''truth'' was itself unstable and politically contested.',
    'If truth-based: the constraint is legitimate theological enforcement (mountain or rope from authority perspectives). If power-based: the constraint is state-backed extraction weaponizing theological language (snare from dissenter perspectives, false summit from authority perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_truth_vs_institutional_power, conceptual, 'Whether Nicene standard represents theological truth or power consolidation').

omega_variable(
    enforcement_mechanism_extent,
    'How systematically did the imperial apparatus actually enforce Nicene uniformity? Were suppression costs (exile, execution, property seizure) applied uniformly across the empire, or were enforcement gaps significant enough to sustain non-Nicene communities?',
    'Historical documentation of enforcement actions: imperial edicts, confiscation records, martyrologies, and ecclesiastical correspondence. Geographic analysis of where enforcement was systematic vs. where non-Nicene communities persisted (Egypt, Syria, Persia). Timeline of enforcement intensity across reigns.',
    'If systematic: suppression ≥ 0.75, victims experience maximal constraint. If sporadic: suppression 0.50–0.65, victims have some exit capacity despite formal constraint. Affects whether classification is snare (systematic) or tangled rope (sporadic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_extent, empirical, 'Actual extent of Nicene enforcement apparatus').

omega_variable(
    dissenter_coordination_capacity,
    'Did non-Nicene communities develop genuine institutional structures and theological learning networks that constituted a coordination function despite persecution, or were they primarily reactive to oppression?',
    'Analysis of surviving theological texts, monastic traditions, episcopal succession records, and liturgical practices from non-Nicene communities (Coptic, Armenian, Nestorian). Evidence of sustained intellectual engagement with doctrinal questions, not merely defensive recitation.',
    'If genuine coordination: constraint is tangled rope (extraction + coordination) from dissenter perspective. If purely defensive: constraint is snare with no coordination benefit (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_coordination_capacity, empirical, 'Capacity for institutional coordination among persecuted theological dissenters').

omega_variable(
    kernel_reading_contest,
    'Which reading of the Nicene kernel is this constraint instantiating, and what do the sibling readings foreclose or coexist with?',
    'Structural analysis of this reading''s axioms (uniformity as extraction mechanism) vs. confessional reading''s axioms (uniformity as legitimate truth-preservation) vs. boundary-maintenance reading''s axioms (uniformity as necessary group identity marker). Determine whether readings logically foreclose each other or coexist as live historical positions.',
    'If readings foreclose: only one can be authoritatively held within a single theological framework (doctrinal settlement forces choice). If coexist: all three readings remain live as different parties'' frameworks through history (modern scholarship recognizes multiple legitimate interpretations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading structure and relationship between sibling readings').

omega_variable(
    extraction_accumulation_over_intervals,
    'Does the constraint''s extraction (base_extractiveness and suppression_requirement) increase or stabilize over time? Does enforcement machinery intensify, plateau, or decay?',
    'Temporal analysis of imperial edicts (compare Edict of Milan 313 to Edict of Theodosius I 380 to post-Chalcedon enforcement). Measurement of suppression costs over time (early councils vs. later inquisitorial machinery). Detection of ratcheting vs. stabilization patterns.',
    'If increasing: constraint is entropic snare (extraction grows as machinery matures). If stable: constraint is structural snare (extraction level reflects power imbalance). If decreasing: constraint is degrading toward piton (enforcement capacity atrophies post-settlement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_accumulation_over_intervals, empirical, 'Temporal trajectory of extraction and suppression intensity').

omega_variable(
    identity_lock_mechanism_in_dissent,
    'For agents classified with identity_locked exit options, what specific identity-fusion mechanism prevents dissent? Is it professional identity (episcopal ordination), relational identity (community belonging), ideological identity (theological worldview), or institutional identity (role within church hierarchy)?',
    'Analysis of historical texts (confessions, exiles, martyrologies) showing how dissenters describe their internal conflict. Evidence of whether exit barriers are external (imprisonment, property loss) or internal (inability to imagine oneself outside the faith tradition).',
    'If professional/institutional: exit barriers are primarily external (upgrade from identity_locked to constrained). If relational/ideological: exit is psychologically impossible even if physical escape is possible (identity_locked is accurate). Affects which exit options are assigned to different dissenter classes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_dissent, empirical, 'Nature of identity-fusion preventing dissenter exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__imperial_uniformity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_theater_t0_acute_dispute, nicene_creed_authority__imperial_uniformity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nicene_theater_t20_institutional_settlement, nicene_creed_authority__imperial_uniformity_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(nicene_theater_t40_degraded_ritual, nicene_creed_authority__imperial_uniformity_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(nicene_extractiveness_t0_council_325, nicene_creed_authority__imperial_uniformity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(nicene_extractiveness_t20_theodosius_380, nicene_creed_authority__imperial_uniformity_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(nicene_extractiveness_t40_post_chalcedon_451, nicene_creed_authority__imperial_uniformity_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(nicene_suppression_t0_early_enforcement, nicene_creed_authority__imperial_uniformity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(nicene_suppression_t20_imperial_coercion, nicene_creed_authority__imperial_uniformity_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(nicene_suppression_t40_enforcement_maturity, nicene_creed_authority__imperial_uniformity_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__imperial_uniformity_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__imperial_uniformity_reading, nicene_creed_authority__confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__imperial_uniformity_reading, nicene_creed_authority__boundary_maintenance_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__imperial_uniformity_reading, council_of_chalcedon_christological_uniformity).
narrative_ontology:affects_constraint(nicene_creed_authority__imperial_uniformity_reading, orthodox_doctrinal_enforcement_apparatus).

% DUAL FORMULATION NOTE:
% The Nicene Creed Authority kernel admits three structurally distinct readings with different epsilon values and different beneficiary/victim distributions. The imperial uniformity reading (this constraint, ε=0.68) treats the creed as extractive power consolidation. The confessional reading (sibling, distinct story) treats the creed as legitimate truth-preservation with lower epsilon. The boundary-maintenance reading (sibling, distinct story) treats the creed as identity-constitutive coordination. All three readings instantiate the same historical fact (the Council of Nicaea and its enforcement) but locate the constraint's primary function differently. The readings are linked through network.affects_constraints to enable contrastive analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__imperial_uniformity_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
