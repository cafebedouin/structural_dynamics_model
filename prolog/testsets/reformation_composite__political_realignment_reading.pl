% ============================================================================
% CONSTRAINT STORY: reformation_composite__political_realignment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__political_realignment_reading, []).

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
 *   constraint_id: reformation_composite__political_realignment_reading
 *   human_readable: Reformation as Political Realignment: Nation-States Assert Sovereignty via Religious Differentiation
 *   domain: political_economy/religious_history/historical_epistemology
 *
 * SUMMARY:
 *   The Reformation is generated in this reading as a fundamentally political
 *   event: emerging territorial nation-states weaponize religious
 *   differentiation to assert sovereignty against the inherited universal
 *   authority claims of the papal church and the Holy Roman Empire. The core
 *   observable is Cuius regio eius religio (the ruler determines the
 *   religion) — a principle that subordinates religious identity to political
 *   territorial jurisdiction. Primary beneficiaries are territorial monarchs
 *   (France, England, Scandinavia, the Palatinate, Saxony, Brandenburg) and
 *   rising merchant republics (Venice, Geneva, the Dutch) who consolidate
 *   administrative power, capture ecclesiastical revenues, and break
 *   dependence on Rome. Primary victims are the papacy and the empire itself,
 *   whose authority derives from claims to universal Christian jurisdiction —
 *   claims that political fragmentation makes untenable. This is one reading
 *   of a contested kernel: the Reformation composite. The
 *   theological_fragmentation_reading understands the Reformation as
 *   fundamentally about competing soteriological and ecclesiological
 *   commitments (justification by faith alone, sola scriptura,
 *   predestination, the nature of the church) that generate structurally
 *   incompatible denominations. The technological_mediation_reading
 *   understands it as fundamentally about the printing press transforming
 *   local theological dissent (which existed throughout the medieval period)
 *   into a continent-wide movement by enabling rapid distribution and
 *   standardization of texts. This political_realignment_reading claims that
 *   while both theological innovation and technological mediation are real
 *   and necessary conditions, they are not sufficient to explain the
 *   Reformation as a historical event — the critical driver is the
 *   political-economic need of emerging nation-states to assert sovereignty
 *   against universal authority structures, and religious differentiation is
 *   the mechanism through which this assertion is legitimated and
 *   institutionalized. The constraint exhibits genuine coordination function
 *   (establishing a new territorial church settlement) alongside asymmetric
 *   extraction (papacy loses authority, empire loses centrality, peasants and
 *   those without exit options bear the costs of religious warfare and forced
 *   confessionalism). Extractiveness rises across the interval (0.32 → 0.58 →
 *   0.52) as the initial political consolidation gives way to religious
 *   enforcement machinery, then moderates as settlements (Augsburg Peace,
 *   Westphalia Treaty) establish stable territorial arrangements. Suppression
 *   peaks during the enforcement phase (0.45 → 0.68) then declines as
 *   territorial confessionalism becomes normalized. Theater increases early
 *   (0.28 → 0.44) as theological justification becomes necessary legitimation
 *   for political moves, then slightly declines (0.44 → 0.38) as political
 *   power consolidates and theological debate becomes increasingly
 *   performative window-dressing rather than actual mechanism.
 *
 * KEY AGENTS:
 *   - Territorial Monarchs (France, England, Scandinavia, German principalities): Primary beneficiaries (institutional/arbitrage) — consolidate power by establishing territorial churches, capture ecclesiastical wealth, break imperial/papal dependence
 *   - Papacy: Primary victim (institutional/trapped) — loses jurisdictional authority, revenue, epistemic monopoly on Christian truth, forced into defensive Counter-Reformation
 *   - Holy Roman Empire: Secondary victim (institutional/trapped) — loses capacity to enforce religious uniformity, weakened by religious fragmentation, authority becomes increasingly formal/theatrical
 *   - Peasantry and Urban Working Poor: Tertiary victims (powerless/trapped) — bear costs of religious warfare, forced conversion, shifting loyalty oaths without agency in sovereign-versus-pope contest
 *   - Provincial Clergy and Monastic Communities: Mixed position (moderate/constrained) — forced to choose between territorial ruler and Rome; some gain power through reformed churches, some lose revenues and autonomy
 *   - Merchant City-States (Venice, Geneva, Dutch cities): Secondary beneficiaries (institutional/arbitrage) — benefit from break-up of universal authority, gain relative power through commercial sovereignty
 *   - Analytical Observer: Perspectival position (analytical/analytical) — sees the constraint as a political-economic realignment that uses religious differentiation as legitimation mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, 0.58).
domain_priors:suppression_score(reformation_composite__political_realignment_reading, 0.68).
domain_priors:theater_ratio(reformation_composite__political_realignment_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reformation_composite__political_realignment_reading, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__political_realignment_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__political_realignment_reading, "Reformation as Political Realignment: Nation-States Assert Sovereignty via Religious Differentiation").
narrative_ontology:topic_domain(reformation_composite__political_realignment_reading, "political_economy/religious_history/historical_epistemology").

domain_priors:requires_active_enforcement(reformation_composite__political_realignment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__political_realignment_reading, 'ae4b7650-1d6f-4eac-a863-84b1bcc30703').
narrative_ontology:cs_kernel_codification('ae4b7650-1d6f-4eac-a863-84b1bcc30703', formalized).
narrative_ontology:cs_authority_grounding('ae4b7650-1d6f-4eac-a863-84b1bcc30703', extraction).
narrative_ontology:cs_reading_relation('ae4b7650-1d6f-4eac-a863-84b1bcc30703', reformation_composite__theological_fragmentation_reading, influences).
narrative_ontology:cs_reading_relation('ae4b7650-1d6f-4eac-a863-84b1bcc30703', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('ae4b7650-1d6f-4eac-a863-84b1bcc30703', foundational, sovereignty_assertion_primary_driver).
narrative_ontology:cs_axiom_status(sovereignty_assertion_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('ae4b7650-1d6f-4eac-a863-84b1bcc30703', sovereignty_assertion_primary_driver, empirically_contingent).
narrative_ontology:cs_axiom('ae4b7650-1d6f-4eac-a863-84b1bcc30703', foundational, religion_instrumental_to_politics).
narrative_ontology:cs_axiom_status(religion_instrumental_to_politics, holdable).
narrative_ontology:cs_axiom_grounding('ae4b7650-1d6f-4eac-a863-84b1bcc30703', religion_instrumental_to_politics, instrumental).
narrative_ontology:cs_reference_frame('ae4b7650-1d6f-4eac-a863-84b1bcc30703', universal_christian_authority_structure).
narrative_ontology:cs_drift_state('ae4b7650-1d6f-4eac-a863-84b1bcc30703', reformation_consolidation_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('ae4b7650-1d6f-4eac-a863-84b1bcc30703', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(reformation_composite__political_realignment_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, emerging_territorial_monarchs).
narrative_ontology:constraint_beneficiary(reformation_composite__political_realignment_reading, rising_merchant_city_states).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, holy_roman_empire).
narrative_ontology:constraint_victim(reformation_composite__political_realignment_reading, universal_christendom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANTRY / URBAN WORKING POOR (SNARE) — Bear the costs of religious warfare, forced conversion, and shifting loyalty oaths without agency in the sovereign-versus-pope contest. Trapped between territorial rulers enforcing new confessions and suppressed alternatives. No exit option — caught in the extraction machinery as collateral damage. Maximum experienced extraction.
constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROVINCIAL CLERGY / RELIGIOUS COMMUNITIES (TANGLED ROPE) — Constrained by the realignment — must choose allegiance to territorial ruler or maintain loyalty to Rome, with severe career consequences for the wrong choice. Also benefit from the constraint: some clergy gain power through new established churches; monastic lands become available; career paths open in reformed administration. Mixed extraction and benefit — coordinating the new religious settlement while bearing suppression of alternatives.
constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TERRITORIAL MONARCHS (ROPE) — Direct beneficiaries. Religious differentiation enables sovereignty assertion — by establishing a territorial church (Lutheran, Reformed, Calvinist), a ruler consolidates administrative power, captures ecclesiastical wealth, and breaks imperial/papal authority. Experiences the constraint as pure coordination: establishing a coherent religious-political settlement solves the coordination problem of asserting territorial independence. Arbitrage options are maximal — can switch allegiances, negotiate with both pope and emperor, extract maximum advantage.
constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PAPACY / UNIVERSAL CHRISTENDOM (SNARE) — Trapped by loss of territorial authority and religious monopoly. Cannot exit — the very framework the papacy depends on (universal Christian authority) is being systematically dismantled through political fragmentation. Forced to defend via military, theological, and administrative means (Counter-Reformation) but lacks the structural capacity to restore the medieval synthesis. Bears maximum extraction — loses revenues, territory, jurisdictional authority, and the epistemic monopoly on Christian truth.
constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 5: HOLY ROMAN EMPIRE / IMPERIAL DIETS (SCAFFOLD) — Organized imperial structure experiences religious differentiation as a temporary coordination problem managed via negotiated settlements (Peace of Augsburg, Treaty of Westphalia). The empire continues to exist but in substantially weakened form — the religious settlement is a scaffold in the sense that it provides temporary territorial peace without resolving the underlying sovereignty question. The sunset is formal: Westphalia (1648) codifies the end of religious uniformity as an imperial requirement, replacing it with territorial-jurisdiction rules. Theater ratio reflects performative imperial authority — the diets maintain ritual unity while actual power devolves to territorial rulers.
constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: MEDIEVAL THEOLOGICAL APPARATUS (PITON) — The sophisticated theological frameworks of the High Middle Ages (Aquinas, Bonaventure, Duns Scotus) persist in form but lose functional authority. Scholastic disputation continues but is increasingly performative — the real political action has shifted to sovereign assertion and territorial consolidation. The theatrical maintenance of theological justification for political moves continues well after theology has ceased to be the actual mechanism driving political change. Theater ratio is high (0.70+) because theological debate continues as cover for what is fundamentally a political restructuring.
constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / POLITICAL REALIGNMENT READING (TANGLED ROPE) — From the standpoint of historical political economy, the Reformation is genuinely a dual process: religious differentiation BOTH coordinates the new territorial settlement (coordination function: legitimates territorial rulers, establishes new administrative structures, captures church wealth) AND extracts from those who lose authority and autonomy (extraction function: papacy, empire, those without exit options). The coordination is real and necessary — the religious settlement genuinely solves the problem of how to organize post-universal Christendom. The extraction is also real — systematic asymmetry in who gains and who loses power. This reading carries no natural law presumption; it is contingent on political economy of state formation.
constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__political_realignment_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reformation_composite__political_realignment_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__political_realignment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reformation_composite__political_realignment_reading, TR),
    TR >= 0.70.

:- end_tests(reformation_composite__political_realignment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine coordination function (establishing territorial settlements, ending religious chaos of the early Reformation) alongside significant asymmetric extraction (papacy loses authority and revenue; empire weakens; those without exit options bear costs). The extractiveness is not maximal (would be 0.70+) because the coordination function is genuine — the territorial settlement does solve a real coordination problem that universal Christendom could no longer manage. The temporal progression (0.32 → 0.58 → 0.52) reflects initial political consolidation, peak extraction during enforcement of religious settlement, then moderation as arrangements stabilize into accepted territorial confessionalism. Suppression (0.68): High. Enforcement of territorial confessionalism requires substantial coercion: forced conversion, religious warfare, suppression of alternative theologies, administrative enforcement of adherence. Suppression is not maximal (would be 0.95+) because enforcement succeeds — once confessionalism becomes normalized in a territory, suppression requirements decline as internalization increases. Theater ratio (0.44): Moderate. Early in the process (t=0), religious differentiation is presented as genuine theological innovation and recovery of primitive Christianity — relatively low theater because the theological claims are perceived as substantive by believers. By the midpoint (t=50), theological justification becomes increasingly necessary to legitimize political moves — theater rises as the political mechanism becomes more visible and requires more cover. By the endpoint (t=100), the theological apparatus (continued scholastic disputation, Counter-Reformation councils) becomes increasingly performative — the political settlement is established and theology serves as ceremonial legitimation. The moderate theater ratio reflects that theological substance is always present (unlike a pure piton where substance has disappeared entirely) but increasingly serves a legitimating rather than generative function. Tangled_rope type: Satisfies all gates — genuine coordination function (establishing territorial settlements), asymmetric extraction (papacy and empire lose authority), active enforcement (religious warfare, conversion policies, suppression of alternatives). This is the most parsimonious classification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival fragmentation. The territorial monarchs (Rope perspective) experience the constraint as pure coordination — solving the problem of how to establish a coherent religiously-integrated state. The papacy (Snare perspective) experiences it as total loss of authority with no exit option. The empire (Scaffold perspective) experiences it as a temporary settlement that weakens imperial structure over time. The powerless (Snare perspective) experience it as coercion and warfare they cannot escape. The analytical observer sees Tangled Rope — genuine coordination alongside asymmetric extraction. The theological apparatus sees itself as Piton — performing legitimating function as the political machinery operates underneath. The perspectival gaps are not reconciled by claiming one perspective is correct — they reflect genuine structural differences in how different agents experience the constraint based on their power, exit options, and structural relationship to the sovereignty assertion mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural relationship to the extraction flow. Territorial monarchs are direct beneficiaries with maximum arbitrage exit options (they can switch religious allegiances to optimize sovereignty) — derived d ≈ 0.05, producing negative effective extraction chi (they gain relative to the constraint). The papacy is a direct victim with no exit option (cannot escape the loss of universal authority without ceasing to be the papacy) — derived d ≈ 0.95, producing maximum effective extraction. The powerless are victims with trapped exit options — derived d ≈ 0.95, maximum extraction. The clergy are mixed (some benefit from new careers, some lose autonomy) with constrained exit — derived d ≈ 0.55. The empire is a victim with constrained exit (can negotiate settlements but cannot escape the underlying fragmentation) — derived d ≈ 0.75. These d values feed directly into chi = ε × f(d) × σ(S), producing the empirically observed perspectival gaps: low chi for beneficiaries (Rope experience), high chi for victims (Snare experience). No directionality overrides are necessary — the structural derivation from beneficiary/victim status plus exit options captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is genuinely Tangled Rope from the analytical perspective — it possesses BOTH a coordination function (establishing territorial religious settlements) AND asymmetric extraction (from papacy, empire, powerless). The resolution is NOT to collapse it into Snare or Rope by ignoring one component. The tension between coordination and extraction is constitutive of the constraint. The peasantry experience maximum extraction (Snare) not because the constraint is 'purely' extraction, but because their structural position (powerless, trapped) makes them experience the coordination benefit as invisible and the suppression as overwhelming. The monarchs experience pure coordination (Rope) not because extraction is absent, but because their structural position (powerful, arbitrage) makes them experience the asymmetry as favorable. The analytical observer sees Tangled Rope by holding both experiences simultaneously and recognizing that both are structurally real. The mandatrophy persists at the perspectival level but is resolved at the structural level: this is a Tangled Rope, and the perspectival fragmentation is exactly what Tangled Rope predicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_religion_vs_politics,
    'Does religious differentiation CAUSE the political realignment, or does the need for political sovereignty CREATE THE DEMAND for religious differentiation as a legitimation tool?',
    'Chronological-causal analysis: Did rulers seek theology to justify pre-existing sovereignty impulses, or did theological innovation drive rulers to seek independence? Textual analysis of justifications provided by rulers for religious breaks vs. theological innovations. Comparative analysis with cases where religious innovation did NOT produce political break (e.g., Hussites, early Waldensians).',
    'If causality runs theology→politics: the theological_fragmentation_reading becomes primary, and the political reading is downstream rationalization. If causality runs politics→theology: the political reading is primary, and theology is instrumental. If bidirectional: all three readings coexist as mutually reinforcing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_direction_religion_vs_politics, empirical, 'Causal direction between religious differentiation and political realignment').

omega_variable(
    beneficiary_scope_ambiguity,
    'Who exactly benefits from the constraint? All territorial rulers, or only a subset? Do merchants and rising urban classes benefit distinctly from monarchs?',
    'Wealth and power distribution analysis: Compare beneficiary trajectories (monarchs, merchants, clergy, city-states) across cases where Reformation was adopted vs. not adopted. Track revenue flows, territorial consolidation, and institutional power before/after religious break. Distinguish between direct beneficiaries (rulers, reformed clergy) and secondary beneficiaries (merchants, urban centers).',
    'If benefits are concentrated among monarchs: constraint is extraction-heavy (Snare features prominent). If benefits diffuse across merchants and urban centers: more Rope-like coordination. If benefits stratify by region/timing: different ε values for different reading contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_scope_ambiguity, empirical, 'Scope of beneficiary groups in political realignment').

omega_variable(
    reading_contest_structure,
    'What is the actual logical relationship between the political_realignment_reading, technological_mediation_reading, and theological_fragmentation_reading?',
    'Structural analysis of each reading''s core claim and what it would require to be false. Does the technological reading (printing press as driver) REQUIRE the theological reading (genuine theological innovation) to be true? Does the political reading FORECLOSE either of the others by asserting that religion is instrumental? Or are all three compatible as descriptions of different causal mechanisms operating simultaneously?',
    'If readings foreclose each other: only one can be true; the kernel contest is a zero-sum competition. If they coexist: the kernel is genuinely contested but not foreclosing; different parties can hold all three simultaneously. If they are stratified (technology enables theology enables politics): they form a causal chain rather than competitors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_structure, conceptual, 'Structural relationships among competing Reformation readings').

omega_variable(
    extraction_vs_legitimate_transfer,
    'Where is the boundary between legitimate wealth/authority transfer (territorial consolidation as rational political economy) and extraction (unfair asymmetry imposed on losers)?',
    'Comparative institutional analysis: What would a voluntary religious settlement look like vs. the coercive settlements that actually occurred? How much of the transfer was negotiated vs. imposed? What alternatives were available to losers (papacy, empire) and why couldn''t they be taken?',
    'If most transfer was coercive and alternatives were genuinely unavailable: higher suppression, higher extraction scores, more Snare-like. If much was negotiated and losers had some exit options: lower extraction, more Rope-like. This determines ε and the overall type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_legitimate_transfer, preference, 'Legitimacy boundary between transfer and extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__political_realignment_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ref_pol_theater_t0, reformation_composite__political_realignment_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ref_pol_theater_t50, reformation_composite__political_realignment_reading, theater_ratio, 50, 0.44).
narrative_ontology:measurement(ref_pol_theater_t100, reformation_composite__political_realignment_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(ref_pol_extractiveness_t0, reformation_composite__political_realignment_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ref_pol_extractiveness_t50, reformation_composite__political_realignment_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(ref_pol_extractiveness_t100, reformation_composite__political_realignment_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ref_pol_suppression_t0, reformation_composite__political_realignment_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(ref_pol_suppression_t50, reformation_composite__political_realignment_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(ref_pol_suppression_t100, reformation_composite__political_realignment_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__political_realignment_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, papal_authority_collapse).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, holy_roman_empire_fragmentation).
narrative_ontology:affects_constraint(reformation_composite__political_realignment_reading, european_state_system_emergence).

% DUAL FORMULATION NOTE:
% The Reformation composite decomposes into three constraint families, each with distinct ε values and causal mechanisms: (1) political_realignment_reading (ε=0.58, Tangled Rope) — state formation using religious differentiation; (2) theological_fragmentation_reading (ε≈0.45, Tangled Rope) — genuine theological innovation generating denominations; (3) technological_mediation_reading (ε≈0.35, Rope) — printing press enabling mass distribution. These are not three measurements of the same constraint — they are three structurally distinct constraints with different beneficiary/victim structures and different extractiveness values. The political reading dominates institutional/analytical historiography; the theological reading dominates religious studies; the technological reading dominates media history. All three are true; none forecloses the others. The ε-invariance principle requires separate stories because the observables differ: measuring Reformation via political consolidation yields ε=0.58; measuring it via theological change yields ε=0.45; measuring it via printing distribution yields ε=0.35.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
