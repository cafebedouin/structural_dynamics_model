% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: The Reformation as Composite Overdetermined Event
 *   domain: historical_epistemology/religious_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the composite_overdetermination_reading of
 *   the contested Reformation kernel: the Reformation was not primarily a
 *   theological innovation (theological_climb_reading), not primarily a
 *   political realignment (political_swap_reading), but an irreducibly
 *   composite event in which theological innovation (climb), institutional
 *   collapse (drop), political realignment (swap), and denominational
 *   proliferation (emergence) occurred simultaneously and structurally
 *   interdependent. The reading rejects singular-causal explanation — no
 *   single driver (whether Luther's theology or northern princes' greed)
 *   captures the phenomenon. Instead, the constraint models the Reformation
 *   as overdetermined: multiple causal pathways converged such that the
 *   outcome was over-constrained by the causal structure itself. This
 *   overdetermination is not a failure of historical explanation but a
 *   structural feature of how large institutional systems transform. The
 *   constraint exhibits tangled-rope classification because the
 *   transformation involved genuine coordination (religious communities
 *   organizing around shared theological claims, secular rulers coordinating
 *   break with Rome) alongside asymmetric extraction (displacement of
 *   Catholic clergy, suppression of heterodox reformed movements, fiscal
 *   redistribution toward northern rulers). The theater_ratio rises over the
 *   interval as institutional performance increases: initial theological
 *   contestation (low theater) evolves into Counter-Reformation performance
 *   (high theater) and confessional hardening rituals. Suppression
 *   requirement rises as the constraint matures: initial theological debate
 *   (low suppression) becomes enforced denominational conformity (high
 *   suppression) through both reformed magistrates and Catholic bishops. The
 *   constraint's extractiveness increases from 0.32 (early theological phase)
 *   to 0.55 (confessional consolidation phase), reflecting the transition
 *   from theological contestation to institutional extraction via
 *   confessional enforcement.
 *
 * KEY AGENTS:
 *   - Northern Secular Rulers (Princes, Magistrates, City Councils): Institutional beneficiaries (institutional/arbitrage) — seized church properties, gained clergy appointment authority, consolidated state power relative to papal authority
 *   - Reformed Theology Communities (Lutheran, Reformed, Evangelical Networks): Organized beneficiaries (organized/constrained) — transmuted theological claims into institutional separation; required political patronage and suppression of alternative reformed movements to survive
 *   - Papal Institutional Authority: Institutional victim (institutional/arbitrage, but experiencing constraint as erosion of power monopoly) — lost territorial control, fiscal authority, clergy appointment power; survived through Counter-Reformation performance (piton degradation)
 *   - Displaced Catholic Clergy: Primary powerless victim (powerless/trapped) — lost institutional positions, faced suppression of Catholic practice in reformed regions, constrained to either conformity or exile
 *   - Lay Catholic Populations: Secondary victim (moderate/constrained) — fragmented by denominational enforcement, constrained by confessional conformity requirements, suppressed in reformed regions
 *   - Radical Reformation Movements: Organized victims (organized/constrained) — suppressed by both reformed magistrates and Catholic bishops; faced double-bind of persecution despite theological alignment with Protestant theology
 *   - Analytical Historians: Observational actors (analytical/analytical) — risk naturalizing contingent outcomes (northern power consolidation) as inevitable overdetermination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.48).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.65).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "The Reformation as Composite Overdetermined Event").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_systems").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '7727ddfa-6031-4cf5-9b74-da3cb0b5ea79').
narrative_ontology:cs_kernel_codification('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', distributed).
narrative_ontology:cs_authority_grounding('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', extraction).
narrative_ontology:cs_reading_relation('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', foundational, reformation_causal_overdetermination_irreducible).
narrative_ontology:cs_axiom_status(reformation_causal_overdetermination_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', reformation_causal_overdetermination_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', foundational, periodization_contested_by_completion_point_choice).
narrative_ontology:cs_axiom_status(periodization_contested_by_completion_point_choice, holdable).
narrative_ontology:cs_axiom_grounding('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', periodization_contested_by_completion_point_choice, conventional).
narrative_ontology:cs_reference_frame('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', unified_catholic_institutional_authority_framework).
narrative_ontology:cs_drift_state('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', confessional_consolidation_era_post_trent, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('7727ddfa-6031-4cf5-9b74-da3cb0b5ea79', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, northern_secular_rulers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, reformed_theology_communities).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, papal_institutional_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, catholic_clergy_displaced).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, lay_catholic_populations_fragmented).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED CATHOLIC CLERGY & LAY POPULATIONS (SNARE) — Trapped by jurisdictional collapse and theological coercion. Exit from Catholic institutional identity was legally and socially prohibited in many regions; conformity to reformed theology was enforced through suppression of Catholic practice. No alternative for those bound to displaced clergy institutions. Maximum experienced extraction without organizational capacity to resist.
constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: URBAN MERCHANT & ARTISAN CONSTITUENCIES (TANGLED ROPE) — Constrained by political and ecclesiastical authority but also benefited from fiscal autonomy and reduced papal taxation. Reform movements coordinated economic grievances with theological claims. Mixed extraction and coordination: benefited from redistribution of church assets and local merchant influence over theology, but constrained by religious conformity requirements and suppression of alternative theological expression.
constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NORTHERN SECULAR RULERS (ROPE) — Institutional beneficiaries experiencing the constraint as coordination mechanism. Seized papal properties, gained control of clergy appointments, consolidated state authority. Reformed theology provided legitimacy for political realignment. Experienced the constraint as solving a collective action problem: breaking papal monopoly on legitimate authority. Pure beneficiary with arbitrage options — could adopt or reject reformation claim-sets based on fiscal/political utility.
constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REFORMED THEOLOGY NETWORKS & EVANGELICAL COMMUNITIES (TANGLED ROPE) — Organized around theological claims (justification by faith alone, vernacular scripture) but constrained by suppression of heterodox theology and requirement for political patronage to survive. Benefited from institutional break with Rome but faced counter-suppression from Catholic authorities. Coordination function: transmitted theological innovation across print networks and oral communities. Extraction component: required conformity to reformed doctrine and suppression of alternative theological framings within communities (e.g., radical reformation movements suppressed by both Protestant magistrates and Catholic bishops).
constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: THE PAPAL INSTITUTIONAL SYSTEM (PITON) — The medieval Catholic institutional structure persisted through inertia despite theological contestation and political defection. Counter-Reformation (Council of Trent onward) was largely performative recovery ritual: reasserted doctrinal authority, reformed clergy discipline, but could not recover lost political territory or institutional monopoly. Theater_ratio high because much of the Counter-Reformation's activity was theatrical restoration of authority rather than restoration of actual institutional function. The papal system degraded from regulatory monopoly to one among competing institutional frameworks.
constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the Reformation's simultaneous theological, institutional, political, and denominational transformations are inevitable correlates: large-scale institutional fracture necessarily involves theological contestation, political realignment, and organizational emergence. No single causal driver could have produced only theological change or only political change — the four dimensions are structurally coupled. This perspective naturalizes the overdetermination as an immutable feature of how large institutional systems transform. However, the declared beneficiaries (northern rulers, reformed communities) suggest this is a false summit: the 'necessity' of the composite framing may be rationalizing the contingent institutional outcomes that benefited specific agents.
constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformation_event_boundary__composite_overdetermination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, TR),
    TR >= 0.70.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts significant asymmetric benefit toward northern rulers and reformed institutional networks. Church property seizure is direct fiscal extraction; clergy displacement is institutional extraction; denominational enforcement suppresses both Catholic practice and heterodox reformed movements. However, extractiveness is not maximal (0.70+) because genuine coordination functions exist: reformed theology networks coordinate around shared doctrinal claims (not pure coercion); merchant constituencies genuinely benefited from reduced papal taxation and increased local authority; theological innovation was not merely post-hoc rationalization but drove institutional reorganization. The composite reading declines extractiveness from the political_swap reading (which sees theology as pure pretext, extractiveness ~0.65) because it acknowledges theological innovation as causally efficacious. Suppression (0.65): Moderate-high. The constraint requires active suppression of Catholic institutional authority, Catholic liturgical practice in reformed regions, heterodox reformed theology (radical reformers), and alternative institutional arrangements. Suppression intensity rose across the interval (0.45 → 0.78) as confessional consolidation proceeded: initial theological contestation operated within partial pluralism; by mid-16th century, territorial confessional uniformity was enforced via suppression. Theater ratio (0.58): Moderate. Initial theological contestation (1517-1530s) had low theater — genuine disputational content, textual argument, theological innovation. As the constraint matured, theater increased: Counter-Reformation ritual reasserted papal authority theatrically despite power loss; confessional enforcement became performative religious conformity; institutional structures persisted through inertia (piton degradation). The rise in theater (0.35 → 0.72) reflects that the constraint shifted from contested theological claim-making to institutionalized performance of denominational identity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates wide perspectival divergence. Displaced Catholic clergy experience pure snare — trapped by jurisdictional collapse and theological/political suppression, no alternative authority to turn to. Lay merchant constituencies experience tangled rope — benefited from fiscal autonomy and merchant influence on theology, constrained by enforced religious conformity. Northern rulers experience rope — solving a coordination problem (breaking papal monopoly) that benefits them through property seizure and authority consolidation. Reformed theology communities experience tangled rope at the generational level — coordinated theological innovation but required political patronage and suppressed heterodox reform. The papal system experiences piton degradation — institutional structure persisting through performance rather than function. The analytical observer risks a false-summit mountain classification — naturalizing the overdetermination as inevitable feature of institutional transformation. The perspectival gaps reveal that which dimension (theological, institutional, political, organizational) one foregrounds is not observationally neutral but determines victim/beneficiary identification and causal narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options per agent. Northern rulers: beneficiaries + arbitrage exit → d ≈ 0.10, f(d) ≈ -0.05 (institutional negative chi). Reformed communities: organized beneficiaries + constrained exit → d ≈ 0.35, f(d) ≈ 0.25 (organized moderate chi). Displaced Catholic clergy: victims + trapped exit → d ≈ 0.95, f(d) ≈ 1.42 (powerless maximum chi). Lay Catholic populations: mixed (both victim and some beneficiary depending on region) + constrained → d ≈ 0.60, f(d) ≈ 0.85 (moderate moderate chi). The composite reading produces moderate directional values (0.35-0.60) rather than extreme (0.10 or 0.95) because the constraint is genuinely mixed: coordination and extraction are simultaneous, not sequential. A pure political_swap reading would produce extreme directionalities (0.05 for rulers, 0.95 for displaced clergy); a pure theological_climb reading would see reformed communities as pure victims of doctrinal oppression (high d). The composite reading's moderate d values reflect the over-determination: multiple agents are simultaneously beneficiaries and victims depending on which sub-event one traces.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    periodization_completion_threshold,
    'What temporal/structural completion point defines ''the Reformation'' as finished? Theological doctrinal separation (1520s-1530s), institutional consolidation (1555 Peace of Augsburg), confessional hardening (post-Trent, 1560s-1580s), or demographic stabilization (1650 post-Westphalia)?',
    'Historical corpus analysis: track when each reading''s historians declare the event ''complete'' and what structural indicators they use (textual authority stabilization, institutional power consolidation, population-level denominational distribution, end of armed confessional conflict). Each completion point favors a different reading.',
    'If theological separation (1520s) is endpoint: theological_climb_reading is validated. If political consolidation (1555) is endpoint: political_swap_reading is validated. If confessional hardening (1580s) is endpoint: composite_overdetermination_reading is validated. Periodization choice is not innocent — it redefines what counts as ''the Reformation.'' Different completion thresholds produce different causal narratives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(periodization_completion_threshold, conceptual, 'Periodization completion point and its historiographical implications').

omega_variable(
    theological_novelty_vs_institutional_pretext,
    'Was Luther''s theological innovation (justification by faith, sola scriptura) genuinely novel and causally primary, or post-hoc rationalization for institutional conflict driven by political and fiscal grievances?',
    'Textual exegesis of Luther''s writings in chronological sequence; correlation analysis of theological emphasis with political/fiscal pressure points; counterfactual: would theological claims alone (absent political patronage) have produced institutional separation? Could identical political realignment have occurred under different theological rubric?',
    'If theological novelty is causal: theological_climb_reading is validated — institutional separation was required consequence of doctrinal breakthrough. If post-hoc: political_swap_reading is validated — theology was instrumentalization. If both causally efficacious: composite_overdetermination_reading is validated — causal direction is genuinely irreducible (causal over-determination rather than singular determination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_novelty_vs_institutional_pretext, empirical, 'Causal status of theological innovation relative to institutional politics').

omega_variable(
    victim_beneficiary_asymmetry_by_framings,
    'Do different readings of ''the Reformation'' (theological vs. political vs. composite) identify different victim/beneficiary sets as primary?',
    'Historiographical analysis: track which social groups each reading identifies as ''winners'' and ''losers.'' Theological-climb reading emphasizes reformed believers as beneficiaries, Catholic institutional authority as victim. Political-swap reading emphasizes northern rulers as beneficiaries, papal institutional power and displaced clergy as victims. Composite reading emphasizes geographic/confessional fragmentation itself as the primary transformation, creating region-specific beneficiaries and victims. Mapping shows structural under-determination in victim/beneficiary identification.',
    'If beneficiary sets diverge by reading: historiographical framing is not observationally neutral — different framings reconstruct social causation from different baseline assumptions. This is the structural signature of overdetermination: the event contains enough causal force to support multiple victim/beneficiary reconstructions simultaneously.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_beneficiary_asymmetry_by_framings, conceptual, 'Victim/beneficiary set asymmetry across competing historiographical readings').

omega_variable(
    composite_versus_sequential_necessity,
    'Could the four dimensions (theological innovation, institutional collapse, political realignment, denominational proliferation) have occurred sequentially rather than simultaneously, or was their simultaneity structurally necessary?',
    'Counterfactual analysis: did institutional collapse require theological contestation, or could a purely fiscal/political conflict have produced the same result? Could denominational proliferation have occurred without theological innovation? Track temporal sequencing in different regions (some areas saw theology first, others political authority first) to identify whether local sequencing produced different outcomes.',
    'If sequential possibility: composite framing is historiographical choice, not structural necessity — one of the sibling readings captures the ''true'' causal sequence. If simultaneity is necessary: composite_overdetermination_reading is validated — the event''s irreducibility is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_versus_sequential_necessity, conceptual, 'Whether composite simultaneity was structurally necessary or historiographically imposed').

omega_variable(
    false_summit_natural_law_status,
    'Is the analytical mountain classification (overdetermination as natural law of institutional transformation) a genuine structural insight or a naturalizing rationalization of contingent outcomes that benefited northern rulers and reformed communities?',
    'Cross-case analysis: do other large institutional fractures (Eastern Orthodox schism, Islamic sectarian fragmentations, modern decolonization movements, technological platform disruptions) show the same four-dimensional pattern of simultaneous theological/institutional/political/organizational transformation? If pattern recurs: genuine natural law. If Reformation-specific: false summit.',
    'If natural law confirmed: mountain classification stands, and the beneficiary declarations trigger false-summit engine signature. If Reformation-specific: the mountain is false — the ''necessity'' is rationalization, and the constraint reclassifies as snare or tangled_rope from analytical perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, empirical, 'Whether four-dimensional composite is natural law or false summit rationalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ref_comp_tr_t0, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ref_comp_tr_t20, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(ref_comp_tr_t40, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 40, 0.72).

% Extraction over time
narrative_ontology:measurement(ref_comp_be_t0, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ref_comp_be_t20, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(ref_comp_be_t40, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ref_comp_su_t0, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ref_comp_su_t20, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(ref_comp_su_t40, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel decomposes into three constraint stories representing competing historiographical readings. Each story has its own extractiveness value reflecting what causal mechanism is foregrounded. composite_overdetermination_reading (ε=0.48) weights theological innovation + institutional collapse + political realignment equally; theological_climb_reading (ε≈0.25) foregrounds theology and de-emphasizes extraction; political_swap_reading (ε≈0.65) foregrounds extraction via property seizure and de-emphasizes theological novelty. The three readings are not observationally equivalent — they identify different beneficiary/victim sets and different completion points. Historians adopt different readings based on which causal mechanism they are tracing. This is not measurement ambiguity but genuine structural under-determination: the Reformation contains enough causal force to support all three readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__composite_overdetermination_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
