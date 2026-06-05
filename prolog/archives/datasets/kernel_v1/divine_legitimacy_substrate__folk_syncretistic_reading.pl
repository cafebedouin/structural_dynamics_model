% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Divine Legitimacy via Folk Syncretistic Household Ritual (One Reading of the Divine Legitimacy Substrate Kernel)
 *   domain: ancient_history/religious_studies/political_economy_of_belief
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel 'divine legitimacy
 *   substrate' in ancient Egypt. This reading instantiates the folk
 *   syncretistic position: divine legitimacy flows through household and
 *   village ritual practice that incorporates multiple deities pragmatically,
 *   independent of formal priesthood or pharaonic authority. The household
 *   head or village elder invokes whatever deities are locally relevant to
 *   the immediate problem (Osiris for agricultural fertility, Taweret for
 *   childbirth protection, Bes for household security, or less formally-known
 *   local deities), coordinating community action around planting, harvest,
 *   mutual aid, and crisis response. The sibling readings
 *   (amun_polytheistic_reading: legitimacy through formal priestly
 *   interpretation of Amun-Ra cosmology; atenist_monotheistic_reading:
 *   legitimacy through pharaonic revelation of Aten as sole true deity)
 *   represent competing institutional claims about where divine authority
 *   resides. The folk syncretistic reading claims that legitimate divine
 *   action manifests at the local, household level through pragmatic ritual
 *   coordination — it does not require theological unity, priestly
 *   interpretation, or pharaonic mandate. This reading is distinguished by
 *   its diffuse authority structure (no centralized arbiter), its resistance
 *   to top-down revision (folk practices persist regardless of official
 *   theology), and its ambiguous beneficiary structure (coordination benefits
 *   are distributed, extraction benefits are modest and local). The
 *   constraint's ε=0.35 reflects that folk syncretism functions primarily as
 *   coordination with modest asymmetric extraction at the elder/specialist
 *   level. Suppression rises over the interval (0.15 → 0.32) as institutional
 *   pressures (Ptolemaic administrative consolidation, later Roman and
 *   Christian state policies) increasingly target folk practice as heretical
 *   or backward, requiring enforcement rather than voluntary participation.
 *
 * KEY AGENTS:
 *   - Household Heads & Mothers: Primary practitioners (powerless/constrained) — invoke deities to coordinate household work, manage crisis, ensure fertility and protection. Benefit from predictable reciprocal aid norms; constrained by kinship obligations.
 *   - Village Elders & Ritual Specialists: Secondary practitioners (moderate/constrained) — maintain ritual calendar, interpret omens, resolve disputes through ritual adjudication. Extract status and resource advantage from specialist role; provide genuine coordination service.
 *   - Craft Guilds & Organized Collectives: Organized practitioners (organized/constrained) — depend on standardized ritual calendar for scheduling labor, enforce craft-worker compensation norms, coordinate inter-village exchange. Benefit from transparent reciprocal scheduling.
 *   - Formal Priesthood (Temple System): Institutional authority (institutional/arbitrage) — maintain elaborate Amun-Ra cosmology and initiation rites; see folk practice as degraded performance; maintain institutional control through interpretive monopoly. View folk practice as theater.
 *   - Agricultural Laborers & Slave Population: Powerless non-practitioners (powerless/trapped) — forced to participate in household rituals controlled by masters; no autonomy over spiritual practice; extracted labor during ritual events without reciprocal benefit.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees folk syncretism as autonomous solution to coordination problem independent of institutional authority; perceives constraint as genuine Rope with self-sustaining legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.35).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.32).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Divine Legitimacy via Folk Syncretistic Household Ritual (One Reading of the Divine Legitimacy Substrate Kernel)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "ancient_history/religious_studies/political_economy_of_belief").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, 'f9e28434-2fdf-4e2f-b671-5917a1c7bd72').
narrative_ontology:cs_kernel_codification('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', distributed).
narrative_ontology:cs_authority_grounding('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', practice).
narrative_ontology:cs_interpretation_layer_present('f9e28434-2fdf-4e2f-b671-5917a1c7bd72').
narrative_ontology:cs_reading_relation('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', divine_legitimacy_substrate__atenist_monotheistic_reading, influences).
narrative_ontology:cs_axiom('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', foundational, household_autonomy_in_divine_practice).
narrative_ontology:cs_axiom_status(household_autonomy_in_divine_practice, holdable).
narrative_ontology:cs_axiom_grounding('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', household_autonomy_in_divine_practice, conventional).
narrative_ontology:cs_axiom('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', foundational, pragmatic_pluralism_over_theological_unity).
narrative_ontology:cs_axiom_status(pragmatic_pluralism_over_theological_unity, holdable).
narrative_ontology:cs_axiom_grounding('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', pragmatic_pluralism_over_theological_unity, instrumental).
narrative_ontology:cs_reference_frame('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', household_ritual_autonomy).
narrative_ontology:cs_drift_state('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', late_ptolemaic_roman_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f9e28434-2fdf-4e2f-b671-5917a1c7bd72', '2026-02-26T14:33:22Z').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_heads).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, local_craft_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLD PRACTITIONER (ROPE) — Constrained by kinship obligations and local resource dependencies, but benefits from the coordination function of syncretistic ritual: predictable seasonal patterns, shared harvest protocols, collective defense against crop failure and disease. The constraint is experienced as solving a real coordination problem (when to plant, whom to call on in crisis, how to ensure community reciprocity). Extraction is minimal because the household gets genuine reciprocal value. The deities are pragmatically invoked as tools, not as sources of domination.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE ELDER / RITUAL SPECIALIST (TANGLED ROPE) — Genuinely coordinates village ritual life (genuine coordination function) but also extracts status, favorable grain allocation, and first access to craft goods from their position as keeper of ritual knowledge. The elder experiences both the coordination benefit (the system works for collective defense and agricultural coordination) and the asymmetric extraction (status and resource advantage from the role). Neither pure coordination nor pure extraction — the system serves the village and serves the elder's interests simultaneously.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FORMAL PRIESTHOOD / TEMPLE SYSTEM (PITON) — From the formal priesthood's institutional view, folk syncretism is a degraded, folk performance of proper theology. The priesthood maintains elaborate interpretive frameworks (Amun-Ra cosmology, complex initiation rites) that the folk practices ignore or reduce to performative gesture. The priesthood sees itself as authoritative but finds that folk religion persists despite being theater from the perspective of learned theology. The priesthood's elaborate cosmology functions to maintain institutional control, but folk practice bypasses this entirely — making the formal priesthood's authority partially vestigial. High theater ratio from the priesthood's view: the folk are performing religion without understanding it; but from the folk's view, the priesthood's elaborate cosmology is expensive overhead without local function.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CRAFTS GUILD / ORGANIZED COLLECTIVE (ROPE) — Organized actors (stonemasons, leatherworkers, grain merchants) benefit from the standardized ritual calendar that folk syncretism provides: predictable scheduling for festivals, consistent protocols for craft-worker compensation at ritual events, mutual aid norms encoded in deity attribution. Syncretism solves a genuine multi-party coordination problem (how to reliably schedule craft labor, how to enforce fair-value exchange across villages). The guild experiences low extraction because the coordination mechanism is transparent and reciprocal — if the ritual calendar failed to deliver value, the guild would abandon it.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: AGRICULTURAL LABORER / SLAVE POPULATION (SNARE) — Those bound to land or owner by coercive relationship experience syncretistic ritual as theater without coordination benefit for themselves. Their labor is extracted during ritual events; their own spiritual autonomy is denied (they are expected to participate in household rituals controlled by their masters, not their own choices). The constraint creates no exit for them — they cannot refuse participation in the household's syncretistic practice. Maximum extraction with minimal coordination benefit for this population.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FOLK PRAGMATISM VIEW (ROPE) — From a civilizational perspective, syncretistic folk religion represents the genuine solution to the constraint 'how do households and villages coordinate survival and reciprocity without a unified state authority?' The deities function as abstract coordination tokens (nodes for synchronizing harvest, healing, mutual defense) rather than as literal divine beings. The folk reading is analytically coherent: it solves a real problem and sustains itself through genuine reciprocal value. The constraint appears as pure coordination from this view — no hidden extraction, because the coordination function is transparent and self-sustaining.
constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(divine_legitimacy_substrate__folk_syncretistic_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, TR),
    TR >= 0.70.

:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Modest. Folk syncretism functions primarily as a coordination mechanism solving real collective action problems (when to plant, whom to call on in crisis, how to schedule craft labor). Asymmetric extraction occurs at the elder/specialist level (status advantage, preferred grain allocation, first access to craft goods) and at the master/slave level (forced ritual labor). But the overall mechanism is reciprocal: households that participate in the ritual calendar receive protection, favorable weather attribution, healing intervention, and mutual aid. The extractiveness is not severe because the coordination benefit is transparent and distributed. If the ritual system failed to deliver reciprocal value, participants would abandon it (low lock-in). Suppression (0.32): Moderate at the interval endpoint. Initial suppression (0.15) reflects minimal enforcement required — folk practice is voluntary and self-enforcing through reciprocal value. Suppression rises over the interval as institutional pressures mount (state consolidation, priestly campaigns against folk religion, later Christian/Islamic suppression) that require active enforcement to suppress competing authority claims. The rising suppression traces institutional attempts to impose theological unity or eliminate folk autonomy, not the mechanism of the constraint itself. Theater ratio (0.55): Moderate. Folk ritual has both functional and performative elements. Genuinely functional: planting ceremonies coordinate labor, healing rituals organize mutual aid, protective rituals encode safety protocols (e.g., Taweret amulets for childbirth are talisman of practical wisdom about high mortality risk). Performative: the specific deities invoked have no empirical verification, the causality is symbolic rather than mechanical, and the liturgy carries theatrical elements. The theater ratio reflects mixed genuine function and symbolic performance — lower than priestly theater (which is theatrical from folk view, higher than pure pragmatic coordination (which folk practice includes but is not exhausted by).
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the household/village practitioners (who experience genuine coordination value and modest extraction) and the agricultural laborers/slaves (who experience pure extraction without coordination benefit). The gap reveals that the constraint is Rope at the autonomous household level and Snare at the coerced labor level — they are not separate phenomena but the same institutional structure with different membership boundaries. For those within the reciprocal network (free households, elders, craft guilds), syncretism is Rope. For those outside it (bound laborers, slaves), it is Snare. The priesthood's Piton classification reveals that formal theology treats folk practice as degraded theater — but the folk would say the priesthood's elaborate cosmology is expensive overhead without local function. Each party experiences the other's practice as theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to extraction flow. Household practitioners are symmetric (costs ≈ benefits from coordination) → moderate d (~0.50) → moderate chi. Village elders are beneficiaries with constrained exit (extract status, provide coordination) → lower d (~0.35) → lower chi. Priesthood are institutional beneficiaries with arbitrage exit (maintain authority, could exit to other domains) → very low d (~0.12) → negative chi (they see the system as coordinating their authority). Agricultural laborers are victims with trapped exit (forced participation, no choice) → high d (~0.90) → high chi (maximum experienced extraction). The analytical observer treats the mechanism as symmetric coordination → moderate d (~0.50) → moderate chi. Directionality reveals that the same extractiveness value (0.35) produces different experienced chi depending on structural position: for beneficiaries and symmetric parties, chi is low (Rope experience); for trapped victims, chi is high (Snare experience).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    folk_belief_sincerity_vs_pragmatism,
    'Did folk practitioners genuinely believe in the deities they invoked, or did they treat them as pragmatic coordination tokens?',
    'Ethnographic analysis of household ritual descriptions; comparison with documented folk practices in early modern Europe and modern polytheistic societies; textual evidence of prayer content and stated intent',
    'If sincere belief: the constraint is genuine coordination of shared cosmology (Rope classification). If pragmatic: the constraint is coordination of action-patterns using deities as abstract symbols (still Rope, but with different legitimacy grounding). If mixed (belief + pragmatism coexisting in same household): classification stable as Rope but with higher theater component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_belief_sincerity_vs_pragmatism, conceptual, 'Whether folk practitioners held sincere belief or pragmatic instrumentalism toward deities').

omega_variable(
    folk_versus_priestly_authority_competition,
    'To what degree did folk syncretistic practice actively resist or subvert formal priestly authority versus passively coexisting with it?',
    'Historical records of priestly condemnation of folk practices; evidence of enforcement attempts and folk compliance or evasion; textual genres that preserve folk voice (magical papyri, ostraca, household shrines)',
    'If active resistance: folk reading creates structural pressure on priestly reading (influences relation). If passive coexistence: the readings coexist without direct pressure (coexists_with relation). If folk practices were explicitly forbidden: potential foreclosure dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_versus_priestly_authority_competition, empirical, 'Degree of competitive pressure between folk and priestly authority structures').

omega_variable(
    extractiveness_from_laboring_classes,
    'How much labor extraction occurred through syncretistic ritual obligations placed on enslaved and dependent populations?',
    'Archaeological evidence of labor-intensive ritual sites; textual records of work schedules tied to festivals; comparison with non-ritual labor demands',
    'If extraction is substantial: base_extractiveness should increase to 0.45–0.55, classification shifts toward Tangled Rope. If minimal: current 0.35 reflects genuine coordination with asymmetry only at elite level. If slavery-dependent ritual labor is systematized: argument for Snare classification at the societal level despite Rope at household level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_from_laboring_classes, empirical, 'Magnitude of labor extraction through syncretistic ritual obligations on dependent populations').

omega_variable(
    kernel_reading_contest_epistemology,
    'Which reading of the divine legitimacy substrate kernel is the one scholars should center in historical accounts — the folk reading, the priestly reading, or the pharaonic reading?',
    'This is a preference omega. No empirical resolution; depends on historiographical choice about whose perspective constitutes ''legitimacy in ancient Egypt.'' Does legitimacy flow where power was exercised (pharaonic/priestly institutional view), where meaning was created (folk syncretic view), or where authority was formally claimed (priestly theological view)?',
    'If folk reading is centered: emphasizes resilience, autonomy, and coordination capacity of non-elite populations; destabilizes narratives of top-down religious control. If priestly reading is centered: emphasizes theological sophistication and institutional continuity; marginalizes folk agency. If pharaonic reading is centered: treats folk practice as backdrop to state-level dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_epistemology, preference, 'Historiographical choice about which reading of divine legitimacy should be primary in ancient Egyptian history').

omega_variable(
    syncretism_as_resistance_versus_adaptation,
    'Does folk syncretism represent active cultural resistance to state-imposed religion, adaptive accommodation to external pressures, or autonomous local practice independent of state dynamics?',
    'Comparative analysis with other syncretistic religious contexts (colonial settings, diaspora communities, imperial rule). Examination of temporal patterns: did syncretism increase or decrease with state centralization? Did folk practices target elites directly or operate orthogonally to state authority?',
    'If resistance-driven: folk reading creates ideological pressure on pharaonic/priestly readings (influences relation toward foreclosure). If adaptive: readings coexist as responses to different constraints (coexists_with). If autonomous: folk reading is orthogonal to institutional readings (coexists_with, no mutual pressure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretism_as_resistance_versus_adaptation, conceptual, 'Whether folk syncretism operated as resistance, adaptation, or autonomous practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(folk_sync_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(folk_sync_tr_t500, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 500, 0.52).
narrative_ontology:measurement(folk_sync_tr_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 1000, 0.55).

% Extraction over time
narrative_ontology:measurement(folk_sync_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(folk_sync_be_t500, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 500, 0.28).
narrative_ontology:measurement(folk_sync_be_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 1000, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(folk_sync_su_t0, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(folk_sync_su_t500, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 500, 0.24).
narrative_ontology:measurement(folk_sync_su_t1000, divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 1000, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, attachment_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel is instantiated by three separate constraint stories, one per reading. Each story has its own epsilon value reflecting the extractiveness of that specific institutional/ideological configuration. The folk_syncretistic_reading (eps=0.35, Rope) is the least extractive because it distributes coordination benefits widely and requires minimal enforcement. The amun_polytheistic_reading (eps=0.48, Tangled Rope) extracts more because it maintains priestly interpretive monopoly. The atenist_monotheistic_reading (eps=0.68, Snare) is most extractive because it requires enforcement against all competing practices. The three stories are linked via network.affects_constraints to show their structural interdependence: when one reading is adopted as state policy, it creates pressure on the others (e.g., Atenist monotheism targets both folk and Amun priesthoods as heretical). The ε-invariance principle ensures that each reading is a distinct constraint with stable extractiveness — changing measurement perspective does not change epsilon within a reading, because each reading has a different legitimacy claim and extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__folk_syncretistic_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
