% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Realignment (Theology as Scaffold)
 *   domain: historical_epistemology/religious_history/commitment_systems
 *
 * SUMMARY:
 *   The Reformation, under the political_swap_reading, was primarily a
 *   realignment of authority structures and asset flows from Rome to secular
 *   princes. Luther's theological breakthrough (justification by faith alone)
 *   provided the intellectual and spiritual legitimacy required to mobilize
 *   populations and justify the institutional break, but the driving force
 *   was the princes' strategic interest in consolidating territorial power,
 *   extracting wealth from the Church, and breaking papal-imperial authority
 *   that constrained their autonomy. The constraint exhibits tangled_rope
 *   structure: secular rulers (beneficiaries) gain from centralized control
 *   over ecclesiastical matters and asset streams; the Catholic Church
 *   (victim) loses territorial states, revenue, and authority; urban
 *   merchants and peasants experience mixed coordination (reduced papal rent
 *   extraction) and re-extraction (princes consolidate control). Theater
 *   rises over the interval as theological disputes become increasingly
 *   formalized and performative — elaborate doctrinal systems justify
 *   decisions already made on political grounds. Suppression intensifies
 *   during the Wars of Religion (1524–1648) as princes enforce religious
 *   conformity through military force. The political settlement at Westphalia
 *   (1648) stabilizes the new authority architecture: princes control
 *   ecclesiastical matters in their domains (cuius regio eius religio), and
 *   theology becomes a degraded system (piton) — functioning through
 *   institutional inertia but subordinate to state power.
 *
 * KEY AGENTS:
 *   - German Princes (primarily Saxony, Brandenburg, Hesse, Palatinate): Primary beneficiaries (institutional/arbitrage) — gain asset streams from seized church lands, control ecclesiastical appointments, break papal authority; exit via territorial arbitrage (play Catholic/Protestant sides for advantage)
 *   - Catholic Church (Rome, Papal States, bishops): Primary victim (institutional/trapped) — loses territorial control, revenue streams, doctrine-setting authority; cannot exit without institutional self-dissolution
 *   - Peasant Populations: Secondary victims (powerless/trapped) — mobilized by religious language (Christian freedom) but crushed by princes in Peasant Wars; trapped under new Protestant authority after theological promises fail
 *   - Urban Merchant Guilds: Secondary beneficiaries with constraints (moderate/constrained) — gain from reduced papal taxation and local commercial autonomy but constrained by prince monopoly-setting and military burdens
 *   - Protestant Theologians (Luther, Calvin, Zwingli): Structural architects (organized/constrained) — generate doctrinal legitimacy for political realignment; constrained by dependence on prince protection and patronage
 *   - Holy Roman Emperor and Catholic Powers (Spain, Bavaria): Contending institutional power (powerful/mobile) — experience political realignment as threat to imperial authority; constrained by military/religious capabilities; gain some consolidation through Counter-Reformation
 *   - Analytical Historical Observer: Civilizational view (analytical/analytical) — risks naturalizing political power consolidation as theological inevitability; must see the staged roles of theology in the political narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.58).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.62).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Realignment (Theology as Scaffold)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_systems").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'e2bc4507-c44e-48ed-b993-4286963bd4c8').
narrative_ontology:cs_kernel_codification('e2bc4507-c44e-48ed-b993-4286963bd4c8', distributed).
narrative_ontology:cs_authority_grounding('e2bc4507-c44e-48ed-b993-4286963bd4c8', extraction).
narrative_ontology:cs_interpretation_layer_present('e2bc4507-c44e-48ed-b993-4286963bd4c8').
narrative_ontology:cs_reading_relation('e2bc4507-c44e-48ed-b993-4286963bd4c8', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2bc4507-c44e-48ed-b993-4286963bd4c8', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('e2bc4507-c44e-48ed-b993-4286963bd4c8', foundational, theology_post_hoc_rationalization).
narrative_ontology:cs_axiom_status(theology_post_hoc_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('e2bc4507-c44e-48ed-b993-4286963bd4c8', theology_post_hoc_rationalization, empirically_contingent).
narrative_ontology:cs_axiom('e2bc4507-c44e-48ed-b993-4286963bd4c8', foundational, princes_primary_causal_agent).
narrative_ontology:cs_axiom_status(princes_primary_causal_agent, holdable).
narrative_ontology:cs_axiom_grounding('e2bc4507-c44e-48ed-b993-4286963bd4c8', princes_primary_causal_agent, empirically_contingent).
narrative_ontology:cs_reference_frame('e2bc4507-c44e-48ed-b993-4286963bd4c8', papal_authority_model).
narrative_ontology:cs_drift_state('e2bc4507-c44e-48ed-b993-4286963bd4c8', post_westphalia_sovereignty_framework, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e2bc4507-c44e-48ed-b993-4286963bd4c8', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, protestant_princes).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, theological_integrity).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, peasant_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANT POPULATIONS (SNARE) — The Peasant Wars (1524–1525) reveal the snare: religious language mobilized peasants to challenge feudal order, but when Luther sided with princes, peasants were crushed without structural reform. Exit is impossible; born into serfdom under old Catholic lords, trapped under new Protestant princes. Theater of liberation (theology of Christian freedom) collapses into structural extraction (tithes, rents, obligations reassign to new extractors). Maximum suppression — no agency to organize against either Catholic or Protestant authority.
constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: URBAN MERCHANT CLASS (TANGLED ROPE) — Cities experience genuine coordination benefit (reduced papal taxation, local control over commerce) alongside asymmetric extraction (princes consolidate merchant guild contributions and impose new trade monopolies). Resources are tied to regional prince; exit costs are high (relocation, loss of established networks). The constraint is hybrid: coordinating local commerce while extracting wealth concentration to the prince. Suppression is moderate — merchants have some agency through guild organization but constrained by prince authority.
constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GERMAN PRINCES (ROPE) — Pure coordination from the beneficiary's structural position. Luther's theology is the intellectual cover for asset seizure (church lands, tithe streams, Rome-derived rents). The prince experiences the constraint as solving a coordination problem: how to break papal control without delegitimizing authority itself? Theology provides the legitimacy narrative. Exit options are arbitrage — the prince can play Catholic Habsburgs against Protestant Reformation to maximize territorial gain. Effective extraction runs toward this agent; experienced as efficient coordination.
constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CATHOLIC CHURCH INSTITUTIONAL STRUCTURE (SNARE) — Rome experiences maximum extraction: loss of territorial states (Papal States), loss of revenue streams (tithes diverted to princes), loss of authority to adjudicate doctrine (Protestant princes now control ecclesiastical matters in their domains). Exit is structurally impossible — the Church is embedded in the old authority architecture; cannot withdraw without self-dissolution. Suppression is total at the institutional level — the Church cannot appeal to secular authority (princes control it) or to the faithful (theology has legitimized their defection). The asset seizure is systematic and enforced by military power (Wars of Religion). Theater is high — theological disputes serve as the framing, but the underlying mechanism is wealth transfer.
constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 5: EUROPEAN NOBILITY / HAPSBURG CROWN (TANGLED ROPE) — Powerful actors experience mixed coordination and extraction. The Reformation creates coordination benefit (centralized secular authority replacing fragmented papal-feudal overlap) but also distributes extraction asymmetrically — Habsburg Spain loses Flanders and territorial advantage to Protestant princes; some Catholic nobility gain relative power through religious consolidation. Exit options are mobile but constrained by continental power balance. Suppression is moderate — powerful actors can negotiate, form alliances, switch sides (Peace of Augsburg, 1555: cuius regio eius religio = each prince chooses religion). Effective extraction is moderate because these actors have agency.
constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: THEOLOGICAL DISCOURSE SYSTEM (PITON) — Theology as a functional system for arbitrating truth claims becomes largely performative after c.1530. Both Catholic and Protestant theologians produce voluminous justifications for their positions, but the real decisions are made by princes (Peace of Augsburg, 1555; Peace of Westphalia, 1648). Theology persists through institutional inertia — universities, seminaries, and church councils continue producing doctrinal pronouncements — but the binding decisions are political, not theological. Theater ratio is high (0.68): elaborate theological dispute frameworks persisting long after the political settlement is made. The system is degraded because its primary function (adjudicating truth about salvation) has been subordinated to the princes' function (consolidating territorial power).
constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a civilizational/universal analytical position, one might classify the Reformation as an immutable structural feature of European history: the inevitable outcome of religious pluralism meeting territorial fragmentation, a natural consequence of printing technology and literacy. 'Religion and politics will always entangle when institutional structures fragment.' However, this naturalization conceals the beneficiary structure: secular rulers exploit theological disputes to their advantage, and theology provides cover for asset seizure. The mountain classification is a false summit. The engine's FSM detection will identify beneficiaries (secular rulers) and reclassify to tangled_rope or snare. The 'natural law' framing is itself a reading that benefits those who profit from the power transfer.
constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformation_event_boundary__political_swap_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, TR),
    TR >= 0.70.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The political realignment systematically transfers wealth and authority from Rome to princes. Asset seizure is substantial (church lands, tithe streams, rents). However, 0.58 (not 0.72+) reflects that significant coordination benefit coexists with extraction — merchants gain from reduced papal rent, some populations gain from theological liberation (before repression), and the new authority architecture (cuius regio eius religio) creates more stable local governance than the overlapping papal-feudal system. The constraint is genuinely hybrid, not pure extraction. Suppression (0.62): Moderate-high. Military force (Wars of Religion, 1524–1648) enforces the new religious order; theological conformity becomes mandatory in many territories. However, suppression declines after Westphalia (1648) when political settlement stabilizes — the state no longer needs continuous coercive enforcement of religious boundaries (shift from 0.72 to 0.58). Theology (0.68): High. Theological discourse reaches peak elaboration long after political decisions are made. The Peace of Augsburg (1555) is a political settlement; both Catholic and Protestant theology afterwards becomes increasingly formalized justification for decisions already determined by state interest. Theater rises as theological necessity declines.
 *
 * PERSPECTIVAL GAP:
 *   The peasant and trapped Catholic Church perspectives see snare (pure extraction, no agency, no exit). The merchant class sees tangled rope (mixed coordination and extraction). The German princes see rope (solving coordination problems through theological legitimacy). The piton perspective reveals theology as degraded — performative rather than functional after the political settlement. The analytical mountain perspective risks naturalizing the realignment as an inevitable consequence of history rather than a structured extraction event. This perspectival spread across 4 distinct types (snare, tangled_rope, rope, piton) reflects the constraint's hybrid nature and the fundamental disagreement about whether theology was the cause or the justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to extraction and coordination flows. German princes as beneficiaries with arbitrage exit options derive low d (d ≈ 0.10–0.20) — they control the outcome and exit via political realignment itself. The Catholic Church as institutional victim with trapped exit derives high d (d ≈ 0.95) — maximum extraction, no escape from the authority transfer. Peasants as powerless victims with trapped exit derive maximum d (d ≈ 1.0) — born into serfdom, mobilized by theology, crushed by princes, trapped again under new masters. Urban merchants as moderate agents with constrained exit derive moderate d (d ≈ 0.65) — benefit from reduced papal extraction but constrained by prince consolidation. The power atom (powerless vs institutional vs moderate) weights the impact of these d values on experienced chi. Trapped powerless agents experience high chi; constrained institutional agents experience lower chi despite similar structural extraction because their power and organizational capacity partially buffer the impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theology_determinism_vs_exploitative_rationalization,
    'Is the theology of the Reformation a genuine doctrinal breakthrough that drove institutional separation (theological_climb_reading), or post-hoc rationalization deployed to legitimize asset seizure motivated by secular princes'' interest in power consolidation (political_swap_reading)?',
    'Chronological analysis of theological development (did justification by faith alone emerge before or after princes began asset seizure?); comparative institutional dynamics (did theology emerge as a driving force in regions where princes lacked motive, or only where political advantage accrued?); textual analysis of motivational language in princes'' correspondence vs theological treatises.',
    'If breakthrough: classification shifts toward mountain (irreducible theological necessity) and theological_climb becomes primary reading. If rationalization: political_swap_reading and tangled_rope/snare classifications are confirmed. If both: composite_overdetermination_reading becomes primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theology_determinism_vs_exploitative_rationalization, empirical, 'Whether theology was genuine doctrinal innovation or post-hoc rationalization for political consolidation').

omega_variable(
    asset_seizure_mechanism_systematicity,
    'Was asset seizure a systematic planned policy by secular rulers to consolidate power, or an opportunistic side effect of theological dispute?',
    'Analysis of timing and pattern of asset transfers: did they cluster around prince-initiated political moves (territorial consolidation, military campaigns, dynastic disputes) or around spontaneous theological-popular movements? Examination of revenue allocation: were seized assets immediately directed to state/military apparatus or distributed inconsistently?',
    'If systematic: political_swap_reading is confirmed; beneficiary structure is intentional and coordinated. If opportunistic: extraction is less foreseeable; theological_climb reading gains credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asset_seizure_mechanism_systematicity, empirical, 'Systematicity of asset seizure as planned policy vs opportunistic exploitation').

omega_variable(
    theological_necessity_for_legitimacy,
    'Was theological innovation necessary to legitimize the political realignment (political_swap reading''s premise: theology is scaffold), or could secular rulers have seized power and assets through purely political means?',
    'Comparative institutional analysis: cases where princes seized church assets without theological justification (Ottoman sultanates, Chinese dynasties, Russian princes) vs European cases with theological cover. Analysis of legitimacy durability: did constraints justified by theology persist longer than those justified by force alone?',
    'If necessary: theology is a functional scaffold for enforcement (tangled_rope sustained by theological suppression layer). If incidental: theology is theater (piton classification); power consolidation occurs regardless. Affects whether suppression value should include theological internalization or is purely coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_necessity_for_legitimacy, conceptual, 'Whether theology was structurally necessary for political legitimization or incidental').

omega_variable(
    kernel_periodization_boundary,
    'Does the Reformation as political realignment end at Luther''s break (1517), the Peace of Augsburg (1555), or the Peace of Westphalia (1648)? Where is the stabilization point?',
    'Analysis of asset seizure completion, institutional consolidation, and revenue stabilization timelines. At what point did princes'' political authority over ecclesiastical matters become stable and accepted by the relevant institutional actors?',
    'If 1517: political_swap is rapid and discrete; constraint classification reflects immediate snare/tangled_rope dynamics. If 1555 or 1648: political_swap is a generational process; constraint measurements must track progression over decades; extractiveness may rise then stabilize as princes consolidate. Affects interval and measurement strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_periodization_boundary, empirical, 'Temporal boundary of political realignment stabilization').

omega_variable(
    sibling_reading_committer_ambiguity,
    'This constraint instantiates the political_swap_reading of reformation_event_boundary. The sibling theological_climb_reading and composite_overdetermination_reading embody different causal framings. Can a single historical event support all three readings, or does adopting one reading''s causal story logically foreclose the others?',
    'Logical analysis of the three readings'' core premises: Does political_swap (theology as post-hoc) directly contradict theological_climb (theology as driving force)? Can composite_overdetermination accommodate both without collapse? Or do they occupy incompatible epistemic positions?',
    'If forecloses: political_swap_reading is in genuine logical opposition to theological_climb; reading_relations should be ''forecloses'', and only one can be true. If coexists_with: both readings remain live; they describe the phenomenon at different levels of causation. Determines the structure of reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_committer_ambiguity, conceptual, 'Logical relationship between readings of reformation event boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1510, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1510_early_theological_dispute, reformation_event_boundary__political_swap_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_1527_peak_theological_justification, reformation_event_boundary__political_swap_reading, theater_ratio, 17, 0.52).
narrative_ontology:measurement(theater_1548_theological_formalization, reformation_event_boundary__political_swap_reading, theater_ratio, 38, 0.68).
narrative_ontology:measurement(theater_1641_post_settlement_performance, reformation_event_boundary__political_swap_reading, theater_ratio, 131, 0.71).

% Extraction over time
narrative_ontology:measurement(extraction_1510_baseline, reformation_event_boundary__political_swap_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(extraction_1527_peak_asset_seizure, reformation_event_boundary__political_swap_reading, base_extractiveness, 17, 0.45).
narrative_ontology:measurement(extraction_1548_consolidation, reformation_event_boundary__political_swap_reading, base_extractiveness, 38, 0.58).
narrative_ontology:measurement(extraction_1641_stabilization, reformation_event_boundary__political_swap_reading, base_extractiveness, 131, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1510_papal_authority_intact, reformation_event_boundary__political_swap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(suppression_1527_wars_of_religion_begin, reformation_event_boundary__political_swap_reading, suppression_requirement, 17, 0.65).
narrative_ontology:measurement(suppression_1548_forced_religious_conformity, reformation_event_boundary__political_swap_reading, suppression_requirement, 38, 0.72).
narrative_ontology:measurement(suppression_1641_post_westphalia_decline, reformation_event_boundary__political_swap_reading, suppression_requirement, 131, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, westphalian_peace__state_authority_codification).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, counter_reformation__institutional_repair).

% DUAL FORMULATION NOTE:
% The Reformation as a single event decomposes into three structurally distinct constraint stories with different causal narratives: (1) political_swap_reading (this constraint) models the Reformation as asset seizure and authority transfer legitimized by theology; (2) theological_climb_reading models the Reformation as genuine doctrinal innovation; (3) composite_overdetermination_reading models the Reformation as an irreducibly composite phenomenon. Each reading instantiates different constraint types (snare/tangled_rope vs mountain vs presheaf over composite space) and different ε values. The three stories are linked via network.affects_constraints as members of the reformation_event_boundary constraint family. See kernel_context in commentary for reading relations and axiom declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
