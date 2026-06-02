% ============================================================================
% CONSTRAINT STORY: theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_theological_climb_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: theological_climb_reading
 *   human_readable: Reformation as Theological Innovation: Justification by Faith Rediscovery
 *   domain: religious_history/historical_epistemology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the theological-climb reading of the
 *   Reformation event boundary. The core claim: Luther's recovery of
 *   justification by faith sola fide constituted a genuine doctrinal
 *   breakthrough in Christian theology, grounded in textual recovery (pauline
 *   epistles reread through grammatical-historical exegesis rather than
 *   scholastic-theological interpolation), which necessitated institutional
 *   separation from the Roman Catholic Church to preserve doctrinal
 *   coherence. From this reading, the Reformation enters history as a
 *   theological innovation event (1517 onwards) that freed believers from
 *   false doctrinal constructs (works-righteousness, merit-based grace,
 *   sacramental efficacy divorced from faith) and restored apostolic clarity.
 *   The constraint functions as pure coordination: reformed communities
 *   coordinate around a shared scriptural interpretation framework, with
 *   minimal theater and low extractiveness. Theater ratio declines from 0.42
 *   (pre-Reformation scholastic theology's performative disputation rituals)
 *   to 0.28 (reformed theology's direct scriptural clarity and catechetical
 *   alignment) because reformed doctrine reduces the gap between proclaimed
 *   principle and institutional practice. Extractiveness remains low
 *   (0.08–0.19) because the constraint's primary function is coordination
 *   (shared doctrine enabling unified community practice), not extraction.
 *   The constraint is contestable: rival readings (political-swap,
 *   composite-overdetermination) see institutional rupture as driven by
 *   political/economic factors rather than theology, or as jointly determined
 *   by theology and politics. This story generates only the theological-climb
 *   reading; the rivals are separate constraints with different ε values,
 *   different classifications, and different beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Theology reformers (Luther, Calvin, Zwingli): Institutional/analytical power — recover and systematize pauline exegesis; become authoritative interpreters of reformed scripture; beneficiaries of intellectual authority and disciplinary coherence
 *   - Reformed believers (northern European laity): Powerless/constrained exit — participate in theological coordination; gain doctrinal clarity and reduced theater in religious practice; constrained by geographic/linguistic access to reformed theology and social rupture from Catholic networks
 *   - Territorial princes and magistrates: Powerful/mobile exit — coordinate religious community governance under reformed framework; extract church lands and ecclesiastical authority; benefit from doctrinal legitimacy for territorial independence; could exit to Catholic reform but choose reformed theology for structural advantages
 *   - Roman Catholic institutional authority: Institutional/arbitrage exit — adversary in this reading; from the theological-climb perspective, they are the victim of doctrinal correction; they maintain apostolic transmission claim but it is here reframed as doctrinal drift rather than development
 *   - Medieval scholastic establishment: Institutional/arbitrage exit — degraded institution (piton perspective); scholastic theology persists through institutional inertia but coordination function has atrophied; no longer capable of unified doctrinal adjudication
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(theological_climb_reading, 0.18).
domain_priors:suppression_score(theological_climb_reading, 0.32).
domain_priors:theater_ratio(theological_climb_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(theological_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(theological_climb_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(theological_climb_reading, theater_ratio, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(theological_climb_reading, rope).
narrative_ontology:human_readable(theological_climb_reading, "Reformation as Theological Innovation: Justification by Faith Rediscovery").
narrative_ontology:topic_domain(theological_climb_reading, "religious_history/historical_epistemology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(theological_climb_reading, '811af59e-1e2a-48be-afc2-6e04b12a125e').
narrative_ontology:cs_created_at('811af59e-1e2a-48be-afc2-6e04b12a125e', '').
narrative_ontology:cs_kernel_codification('811af59e-1e2a-48be-afc2-6e04b12a125e', fixed_text).
narrative_ontology:cs_authority_grounding('811af59e-1e2a-48be-afc2-6e04b12a125e', lineage).
narrative_ontology:cs_interpretation_layer_present('811af59e-1e2a-48be-afc2-6e04b12a125e').
narrative_ontology:cs_kernel_id(theological_climb_reading, reformation_event_boundary).
narrative_ontology:cs_reading_relation('811af59e-1e2a-48be-afc2-6e04b12a125e', political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('811af59e-1e2a-48be-afc2-6e04b12a125e', composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('811af59e-1e2a-48be-afc2-6e04b12a125e', foundational, pauline_sola_fide_textually_defensible).
narrative_ontology:cs_axiom_status(pauline_sola_fide_textually_defensible, holdable).
narrative_ontology:cs_axiom_grounding('811af59e-1e2a-48be-afc2-6e04b12a125e', pauline_sola_fide_textually_defensible, empirically_contingent).
narrative_ontology:cs_axiom('811af59e-1e2a-48be-afc2-6e04b12a125e', foundational, doctrinal_innovation_justifies_institutional_separation).
narrative_ontology:cs_axiom_status(doctrinal_innovation_justifies_institutional_separation, holdable).
narrative_ontology:cs_axiom_grounding('811af59e-1e2a-48be-afc2-6e04b12a125e', doctrinal_innovation_justifies_institutional_separation, deontological).
narrative_ontology:cs_reference_frame('811af59e-1e2a-48be-afc2-6e04b12a125e', apostolic_scriptural_clarity).
narrative_ontology:cs_drift_state('811af59e-1e2a-48be-afc2-6e04b12a125e', medieval_scholastic_synthesis, gap(codification_collapse, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(theological_climb_reading, reformed_believers).
narrative_ontology:constraint_beneficiary(theological_climb_reading, theology_scholarship).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMON BELIEVER IN REFORMED TERRITORIES — Constrained by geography and language access to reformed theology, but not trapped. Participates in the coordination function (shared doctrine, liturgical clarity, direct scriptural access) with minimal coercion. Experiences reduced theater — sermon content aligns with written doctrine rather than performative ritual. Benefits from theological clarity on salvation; constraining cost is social rupture from Catholic networks.
constraint_indexing:constraint_classification(theological_climb_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER — Examines the constraint as a genuine doctrinal innovation grounded in textual recovery (pauline epistles reread through grammar, not scholastic interpolation). The constraint functions as pure coordination: reformed communities need shared scriptural interpretation to maintain doctrinal coherence; the 'constraint' is the coordination framework itself. No extraction mechanism visible from this perspective — the beneficiary and the coordinate are identical. Low extractiveness reflects coordination without asymmetry.
constraint_indexing:constraint_classification(theological_climb_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: CATHOLIC INSTITUTIONAL AUTHORITY — From the Catholic perspective grounded in apostolic transmission and councils, the Reformation's claim of 'rediscovery' is a false-summit candidate. The reading treats justification doctrine as settled (Council of Trent codification) and reinterpretation as innovation, not recovery. This perspective sees Luther's move as logical error (misreading of Augustine, Paul, and patristic sources through a novel lens) rather than doctrinal breakthrough. From this position, the Reformation appears as an institutional disruption (snare) rather than a theological climb (rope). However, this story instantiates only the theological-climb reading, not the Catholic counter-reading — the Catholic perspective is declared here to establish the perspectival gap.
constraint_indexing:constraint_classification(theological_climb_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: TERRITORIAL PRINCE (HOLY ROMAN EMPIRE) — Experiences the constraint as tangled coordination and extraction. The prince benefits from reformed theology: it legitimates territorial autonomy against papal authority, provides doctrinal justification for confiscating church lands, and consolidates princely power over ecclesiastical appointment. Simultaneously, the prince coordinates genuine religious community governance — reformed theology requires institutional framework (territorial church structure, consistory courts, catechism implementation) that the prince provides. Mixed benefits and costs: extraction from church (lands, authority), coordination gains (unified religious community, doctrinal authority legitimacy). Mobile exit: the prince could adopt Catholic reform (Council of Trent) but chooses reformed theology for structural advantages. The constraint is both genuine theological gain and political opportunity simultaneously.
constraint_indexing:constraint_classification(theological_climb_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDIEVAL SCHOLASTIC THEOLOGY ESTABLISHMENT — The constraint appears as a degraded institution from within scholasticism. The scholastic synthesis (Thomas Aquinas integrating Aristotle with Augustine) continues to function textually, but reformed polemics have attacked the textual warrant, fragmenting the shared hermeneutical frame. Scholasticism persists through institutional inertia (university chains, monastic preservation) but its coordination function has atrophied — no shared hermeneutical agreement remains. Theater ratio high: scholastic disputations continue performatively (theological commentary, university sermons) but no longer coordinate action coherently. Extractiveness low: the scholastic establishment extracts primarily through institutional position (tenure, endowment), not through the constraint itself.
constraint_indexing:constraint_classification(theological_climb_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(theological_climb_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(theological_climb_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(theological_climb_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(theological_climb_reading, TR),
    TR >= 0.70.

:- end_tests(theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The theological-climb reading sees the Reformation as primarily coordinative — reformers and believers share a doctrine-discovery coordination function. Extractiveness value reflects that reformed theology genuinely reduces theater (doctrinal clarity, liturgical transparency, catechetical alignment with scripture) and provides substantive benefit to believers (soteriological assurance, reduced sacramental performance burden). The small increase from pre-Reformation baseline (0.08 to 0.18) reflects that reformed institutions (consistory courts, pastoral authority, catechetical enforcement) create new extraction points — but these are secondary to the primary coordinative function. A rival reading (political-swap) would assess the same facts and assign extractiveness 0.55–0.70 by treating theological language as legitimation for political extraction. This story does not adopt that reading. Suppression (0.32): Moderate-low. Reformed theology encounters resistance from Catholic authorities, but suppression is neither severe nor total. Reformed believers face constraints (social rupture, family division, heresy prosecution in Catholic territories) but not universal barriers — geography creates pockets of reformed autonomy (Protestant territories). The suppression value reflects real costs without total foreclosure of exit. Theater ratio (0.28): Low. Reformed theology reduces theater by design — sola scriptura reduces priestly mediation, justification by faith eliminates sacramental performance requirements, vernacular worship eliminates Latin liturgical obscurity. The measured theater reflects that reformed institutions develop new rituals and authority structures (pastoral authority, catechism enforcement) that reintroduce some performative content, but net theater is substantially below medieval scholastic ritual. The trajectory (0.42 → 0.28 → 0.31) shows theater declining sharply in reformation establishment, rising slightly in orthodoxy phase as reformed theology itself becomes institutionalized and performative.
 *
 * PERSPECTIVAL GAP:
 *   This reading exhibits a subtle perspectival gap, not the stark divergence of other constraints. The gap is not between type classifications (most perspectives yield rope or mountain) but between experienced extractiveness: the beneficiary (reformed theologian) sees χ ≈ -0.05 (gain), the common believer sees χ ≈ 0.12 (modest cost), the territorial prince sees χ ≈ 0.08 (small cost for large political gain), and the Catholic institutional view (perspective 3, not the primary reading) would see χ ≈ 0.48 (snare/tangled_rope from their viewpoint). Within the theological-climb reading alone, perspectives agreement is high — all see rope or mountain, all assign low-to-moderate extractiveness. The perspectival gap is between this reading and its siblings: the political-swap reading would derive different d values for the same agents (treating princes as primary extractors, believers as secondary victims) and would assign ε ≈ 0.55–0.70 for the same constraint. This omega-level gap (not a within-reading gap) reveals that the theological-climb reading and political-swap reading are not empirically distinguishable by metrics alone — they are interpretively incommensurable readings of the same event.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: reformed believers (powerless/constrained) experience low-to-moderate d (around 0.55–0.65) because they bear some constraint costs (social rupture, geographic barriers) but also benefit from theological clarity and institutional inclusion. The sigmoid f(d) applied to their position yields moderate f(d) ≈ 0.75–0.85, which reduces chi slightly (low base ε × moderate f(d) × regional scope σ ≈ 0.90 yields chi ≈ 0.12–0.14). Reformed theologians and territorial princes (institutional/powerful with arbitrage or mobile exit) experience low d (0.15–0.25 as beneficiaries) and negative f(d) ≈ -0.01 to 0.02, driving chi toward negative or zero — they experience the constraint as enabling opportunity, not extraction. The analytical observer (analytical/analytical) has d ≈ 0.73 derived from canonical fallback, yielding f(d) ≈ 1.15, which would magnify chi if base ε were higher; at ε = 0.18, this produces moderate chi ≈ 0.19, reflecting that the observer perceives real structural coordination but at civilizational scope where theological coordination mechanisms become visible as genuine constraint rather than incidental institutional fact. No directionality overrides applied — the derivation chain produces accurate representation of each agent's structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING: This constraint does not exhibit mandatrophy within its own reading. Low extractiveness (0.18), genuine coordination function (beneficiary = reformed believers share theology; victim = medieval theology's false doctrines), and low suppression (0.32) all align coherently with the rope classification. The false summit risk is explicitly addressed: perspective 3 (Catholic institutional view) declares how the mountain classification would appear from the opposite reading. This story instantiates only the climb reading, which coherently resolves as rope. MANDATROPHY BETWEEN READINGS: The true mandatrophy emerges between the theological-climb reading and the political-swap reading. The same constraint (1517–1555 institutional rupture labeled 'Reformation') produces entirely different ε values, beneficiary/victim structures, and classifications depending on whether the reading privileges theological or political sources. This is not a measurement-basis ambiguity (DP-001 would forbid that); it is a kernel reading ambiguity (Rules 1–4 address this). The engine instantiates both readings as separate constraints with network links. The mandatrophy is structural, not epistemically resolvable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_rediscovery_vs_innovation,
    'Is justification by faith alone a genuine doctrinal rediscovery from apostolic sources, or a theological innovation with selective textual reading?',
    'Detailed philological analysis of Paul''s epistles in original Greek; comparison of reformation exegesis against patristic and scholastic interpretations; identification of hermeneutical principles driving each reading; assessment of whether alternative readings of key passages (Romans 3:28, Ephesians 2:8-9) are defensible within grammatical-historical constraints.',
    'If rediscovery: Reformation is theological climb (rope/mountain classification confirmed); Catholic doctrinal development is drift. If innovation: Reformation is theological break (tangled_rope or snare); Catholic continuity is legitimate. Classification outcome depends entirely on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_rediscovery_vs_innovation, empirical, 'Doctrinal status of justification by faith: recovery or innovation?').

omega_variable(
    institutional_separation_necessity,
    'Did the theological innovation of sola fide necessarily require institutional separation from the Catholic Church, or was institutional rupture contingent on political and social factors?',
    'Counterfactual historical analysis: examination of reform within other Christian traditions (Orthodox, Oriental churches) that adopted modified justification doctrines without institutional rupture; analysis of failed internal Catholic reform movements (conciliarists, mystics) to identify structural barriers to internal doctrinal revision; assessment of whether theological coherence required new institutions or whether institutional separation was politically driven.',
    'If institutionally necessary: the constraint is pure theological (rope/mountain). If contingent: the theological claim is separable from the institutional break, and politics/power become primary (tangled_rope/snare). The kernel reading depends on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_separation_necessity, conceptual, 'Whether institutional separation was necessitated by theology or contingent on politics').

omega_variable(
    sibling_reading_interpretive_difference,
    'What epistemic principles distinguish the theological-climb reading from the political-swap reading at the level of source selection and historical weight-assignment?',
    'Meta-analysis of source interpretation: the theological-climb reading prioritizes theological writings (Luther''s exegetical lectures, catechisms, confessional documents) as primary evidence; the political-swap reading prioritizes political documentation (diet records, territorial decrees, correspondence with princes) as the causal driver. Identify which source corpus is more probative for the specific claim: Did justification doctrine drive institutional separation, or did institutional interests (land, authority, family power) drive doctrinal legitimation?',
    'Different source hierarchies yield different classifications. If theological sources are probative: rope classification holds. If political sources are probative: the theological claim is secondary frame, and tangled_rope or snare classification emerges. The omega itself is not resolvable by external evidence — it reveals that the two readings operate from incommensurable epistemic priorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_interpretive_difference, conceptual, 'Epistemic incommensurability between theological-climb and political-swap readings').

omega_variable(
    doctrine_beneficiary_identity,
    'Who are the actual beneficiaries of justification by faith doctrine as an institutional practice, versus who are the proclaimed beneficiaries?',
    'Structural analysis of reformed theology implementation: identify which social groups gained institutional authority and resource control under reformed doctrine (theologians, territorial rulers, urban magistrates, literate laity, merchants); compare against medieval ecclesiastical hierarchies. Assess whether theological clarity (proclaimed benefit to believers) correlates with resource redistribution or primarily reorganizes extraction channels.',
    'If beneficiaries align (theological clarity benefits believers who also gain institutional power): rope/mountain classification holds. If beneficiaries diverge (doctrine benefits theological elite and rulers; benefits to common believers are secondary or offset by new constraints): tangled_rope or snare becomes more appropriate. This omega addresses the false-summit risk of the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_beneficiary_identity, empirical, 'Identity and actual benefits of theological doctrine''s beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(theological_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_pre_reformation, theological_climb_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(theater_established_reformation, theological_climb_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(theater_orthodoxy_phase, theological_climb_reading, theater_ratio, 40, 0.31).

% Extraction over time
narrative_ontology:measurement(extraction_pre_reformation, theological_climb_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(extraction_established_reformation, theological_climb_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(extraction_orthodoxy_phase, theological_climb_reading, base_extractiveness, 40, 0.19).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(theological_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(theological_climb_reading, 0.12).
narrative_ontology:affects_constraint(theological_climb_reading, political_swap_reading).
narrative_ontology:affects_constraint(theological_climb_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The Reformation event boundary is a contested kernel with three structurally distinct readings: theological_climb_reading (this file), political_swap_reading, and composite_overdetermination_reading. Each reading yields a different constraint with distinct ε values (theological climb: ε=0.18 rope; political swap: ε=0.58 tangled_rope; composite: ε=0.35 rope or tangled_rope depending on regional perspective). The readings are not observational variants of one constraint; they are incommensurable interpretations of a kernel that grounds legitimacy claims differently. All three affect one another: the theological reading influences political reading (theodicy for extraction), political reading influences theological reading (utility of theological legitimation), both influence composite reading (evidence for multi-causal determination). Siblings are linked with network.affects_constraints in all three files to model the kernel structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
