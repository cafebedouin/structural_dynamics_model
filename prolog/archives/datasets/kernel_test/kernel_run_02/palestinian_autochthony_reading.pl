% ============================================================================
% CONSTRAINT STORY: palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_palestinian_autochthony_reading, []).

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
 *   constraint_id: palestinian_autochthony_reading
 *   human_readable: Palestinian Legitimacy via Autochthony, Displacement Trauma, and Right of Return
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The Palestinian autochthony reading grounds legitimacy claims in three
 *   structural arguments: (1) continuous habitation of Palestinian
 *   territories across centuries, with documented presence in Ottoman,
 *   British, and post-Mandate periods; (2) displacement trauma from 1948 and
 *   subsequent wars as ongoing injustice requiring remedy; (3) right of
 *   return as a non-negotiable principle grounded in both UN resolution 194
 *   and natural justice. This reading declares the Israeli state's
 *   territorial legitimacy as contested and its continued settlement and
 *   administrative control as extractive occupation. The constraint manifests
 *   as suppression of Palestinian voice in discourse, denial of return and
 *   repatriation, confinement to fragmented territories, and institutional
 *   perpetuation of refugee status across generations. The reading is ONE
 *   READING of the contested kernel 'territorial legitimacy dual,' which also
 *   encompasses zionist_refuge_reading (Jewish historical connection and
 *   diaspora return aspiration) and two_state_coexistence_reading (bounded
 *   sovereignty division). The autochthony reading and zionist reading employ
 *   different historical timescales: the autochthony reading privileges
 *   continuous habitation across the last millennium and emphasizes recent
 *   displacement as injustice; the zionist reading privileges ancient Jewish
 *   presence and modern return after diaspora as remedy. The coexistence
 *   reading brackets the competing legitimacy arguments and frames the
 *   problem as a coordination challenge to be managed through institutional
 *   division.
 *
 * KEY AGENTS:
 *   - Palestinian Population: Primary victim (powerless/trapped) — subject to displacement regime, confinement, denial of return; bears full extraction
 *   - Palestinian Authority & Civil Society: Secondary institutional actor (moderate/constrained) — manages coordination functions while experiencing extraction of sovereignty; experiences both organizational benefits and institutional subordination
 *   - International Community (UN, NGOs, States): Institutional mediator (institutional/arbitrage) — derives benefit from managing status quo; maintains diplomatic role while selective enforcement of remedies
 *   - Palestinian Resistance & Rights Movements: Organized victim (organized/constrained) — asserts legitimacy claim while suppressed; experiences extraction via discourse suppression and organizational constraint
 *   - Palestinian Diaspora & Solidarity Movements: External powerful constituency (powerful/mobile) — retains mobility and global platform but experiences institutional suppression of legitimacy assertion
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing displacement as inevitable historical process; false summit framing presents contingent regime as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(palestinian_autochthony_reading, 0.68).
domain_priors:suppression_score(palestinian_autochthony_reading, 0.78).
domain_priors:theater_ratio(palestinian_autochthony_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(palestinian_autochthony_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(palestinian_autochthony_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(palestinian_autochthony_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(palestinian_autochthony_reading, snare).
narrative_ontology:human_readable(palestinian_autochthony_reading, "Palestinian Legitimacy via Autochthony, Displacement Trauma, and Right of Return").
narrative_ontology:topic_domain(palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(palestinian_autochthony_reading, 'e916c69e-8f72-461d-a514-d1c19fc59df4').
narrative_ontology:cs_created_at('e916c69e-8f72-461d-a514-d1c19fc59df4', '').
narrative_ontology:cs_kernel_codification('e916c69e-8f72-461d-a514-d1c19fc59df4', fixed_text).
narrative_ontology:cs_authority_grounding('e916c69e-8f72-461d-a514-d1c19fc59df4', lineage).
narrative_ontology:cs_interpretation_layer_present('e916c69e-8f72-461d-a514-d1c19fc59df4').
narrative_ontology:cs_kernel_id(palestinian_autochthony_reading, territorial_legitimacy_dual).
narrative_ontology:cs_reading_relation('e916c69e-8f72-461d-a514-d1c19fc59df4', zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('e916c69e-8f72-461d-a514-d1c19fc59df4', two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('e916c69e-8f72-461d-a514-d1c19fc59df4', foundational, continuous_habitation_legitimates_territorial_claim).
narrative_ontology:cs_axiom_status(continuous_habitation_legitimates_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('e916c69e-8f72-461d-a514-d1c19fc59df4', continuous_habitation_legitimates_territorial_claim, empirically_contingent).
narrative_ontology:cs_axiom('e916c69e-8f72-461d-a514-d1c19fc59df4', foundational, displacement_injustice_requires_remedy_including_return).
narrative_ontology:cs_axiom_status(displacement_injustice_requires_remedy_including_return, holdable).
narrative_ontology:cs_axiom_grounding('e916c69e-8f72-461d-a514-d1c19fc59df4', displacement_injustice_requires_remedy_including_return, deontological).
narrative_ontology:cs_reference_frame('e916c69e-8f72-461d-a514-d1c19fc59df4', ottoman_period_palestinian_presence).
narrative_ontology:cs_drift_state('e916c69e-8f72-461d-a514-d1c19fc59df4', contemporary_post_1948, gap(practice_drift, severe, false)).

% --- Structural relationships ---
narrative_ontology:constraint_victim(palestinian_autochthony_reading, palestinian_population).
narrative_ontology:constraint_victim(palestinian_autochthony_reading, territorial_sovereignty_claim).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED PALESTINIAN POPULATION (SNARE) — Powerless agents with no exit from displacement regime. Structural bars to return, repatriation, or land restitution are maintained through military force, legal prohibition, and settlement expansion. The constraint extracts territorial rights, property, and freedom of movement. Suppression is maximal — trapped by denial of return, denial of citizenship in origin lands, confinement to fragmented territories. Theater ratio is moderate because the dispossession is structurally transparent — the extraction is visible and acknowledged by all parties, not obscured by performative ritual.
constraint_indexing:constraint_classification(palestinian_autochthony_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PALESTINIAN AUTHORITY & CIVIL SOCIETY (TANGLED ROPE) — Moderate institutional power with constrained exit. Experience coordination function: managing water resources, coordinating humanitarian response, maintaining administrative infrastructure in fragmented territories. Simultaneous extraction: limited territorial control, revenue dependency on external sources, security sector subordination. Active enforcement required to maintain both coordination and extraction — Israeli military administration and administrative structures overlap with PA governance. Mixed experience: genuine coordination problems solved, genuine extraction of sovereignty.
constraint_indexing:constraint_classification(palestinian_autochthony_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL COMMUNITY (ROPE) — Institutional actors with arbitrage options. Experience the constraint as a coordination problem to be managed: humanitarian access, refugee status, international law compliance, two-state solution frameworks. Derive benefit from maintaining status quo (stable international order), from being positioned as mediator/arbiter (diplomatic leverage), and from selective enforcement (some violations flagged, others normalized). No direct extraction from Palestinian population — benefits are indirect (institutional role, diplomatic capital). Low suppression relative to other perspectives — international actors retain mobility and can exit (diplomatic recognition, aid withdrawal, etc.).
constraint_indexing:constraint_classification(palestinian_autochthony_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PALESTINIAN RESISTANCE & RIGHTS MOVEMENTS (SNARE) — Organized agents (PLO, BDS, civil society) face severe suppression (designation as terror, arrest, movement restriction, media blackout) for asserting the legitimacy claim itself. The constraint extracts not just territory but also voice — suppression of discourse is part of the extraction mechanism. Constrained exit (can organize but face severe retaliation) rather than trapped (some mobility retained through diaspora networks, international support). Classification remains snare because organized agents, despite higher power than powerless perspective, still experience net extraction without corresponding coordination benefit.
constraint_indexing:constraint_classification(palestinian_autochthony_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: PALESTINIAN DIASPORA & EXTERNAL CONSTITUENCIES (SNARE) — Powerful institutional and civil society actors (diaspora networks, BDS movement, international Palestinian solidarity) retain mobility and can organize globally. However, they experience the constraint as extractive in a different register: their voice is marginalized in international forums, their legitimacy claims are suppressed by counternarrative, their rights assertions face institutional and social friction. Mobile and powerful relative to trapped Palestinians, yet still experiencing extraction through discursive and institutional mechanisms. Snare classification reflects that even powerful external agents cannot override the core constraint structure — they experience it as immutable suppression rather than a coordination problem they can solve.
constraint_indexing:constraint_classification(palestinian_autochthony_reading, snare,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HISTORICAL NECESSITY VIEW (MOUNTAIN) — From a universal/civilizational framing, territorial displacement and demographic change are presented as inevitable historical processes ('demographic realities,' 'security necessities,' 'irreversible settlements'). This perspective sees the constraint as a natural feature of modern state formation — all nation-states involved displacements; territorial claims clash irreducibly; some injustices cannot be remedied at scale. The mountain framing naturalizes what this reading declares as constructed injustice. Engine's false summit detection will flag: beneficiaries exist (Israeli state, settler constituencies), the extraction is structurally transparent, and the 'inevitability' framing serves to suppress remedy discourse. The mountain is a false summit.
constraint_indexing:constraint_classification(palestinian_autochthony_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(palestinian_autochthony_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(palestinian_autochthony_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(palestinian_autochthony_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts territorial rights, property, freedom of movement, and dignity across generational timescale. Unlike transactional extraction (explicit payment for service), this reading identifies extraction through structural denial: denial of return, denial of repatriation, denial of property restitution, denial of full citizenship. The measurement trajectory shows rising extractiveness over the interval (0.48 → 0.68) as settlement expansion continues, territorial fragmentation deepens, and integration into Israeli state governance increases without corresponding Palestinian sovereignty or return rights. Suppression (0.78): Very high. Structural suppression includes military occupation, legal prohibition of return, settlement expansion preventing territorial contiguity, administrative control of water and resources, movement restrictions, and institutional suppression of return discourse. Psychological suppression includes displacement trauma perpetuation, normalization prevention, and institutional refugee status maintenance. Theater ratio (0.55): Moderate. The extraction is structurally transparent — dispossession is visible and acknowledged by all parties, not obscured by performative ritual. However, theater emerges in international diplomatic discourse: 'peace process' framing performs solution-seeking while structural barriers to implementation remain; 'two-state solution' language performs path-to-remedy while settlements expand; humanitarian discourse performs concern while return rights are denied. Rising theater ratio reflects increasing gap between diplomatic performance and structural stasis.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival variance. Trapped Palestinians see pure extraction without coordination benefit (Snare). Palestinian authorities experience mixed coordination and extraction — they solve real administrative problems while experiencing sovereignty subordination (Tangled Rope). International mediators see a coordination challenge to be managed — they occupy position of neutral arbiter and derive diplomatic benefit from doing so (Rope). Palestinian resistance sees extraction through discourse suppression — organization and assertion of rights are themselves suppressed (Snare). Diaspora and solidarity movements, despite mobility and power, experience institutional suppression of their legitimacy claims (Snare). The civilizational analytical observer risks seeing displacement as a natural historical feature of state formation — this is a false summit, naturalizing what the reading declares as constructed injustice. The perspectival gap reveals that the same structural phenomenon is experienced as immutable law (mountain), as manageable coordination problem (rope), and as extractive oppression (snare) depending on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) reflects their structural position relative to the constraint. Trapped Palestinians with no exit (d ≈ 0.95) experience maximum f(d) ≈ 1.42, amplifying experienced extraction to near-maximum. Palestinian authorities with constrained exit (d ≈ 0.65) experience f(d) ≈ 1.00, moderate amplification, and perceive mixed coordination-extraction (tangled rope). International institutional mediators with arbitrage options derive benefit (d ≈ 0.10) and experience negative or minimal f(d), perceiving coordination function (rope). Organized resistance with constrained exit (d ≈ 0.70) experience high f(d) ≈ 1.15, and perceive extraction despite organizational power (snare). Diaspora with mobile exit but institutional suppression of discourse (d ≈ 0.75) experience f(d) ≈ 1.18, maintaining snare perception despite mobility. The engine computes d from victim/beneficiary declarations and exit options; this reading declares Palestinian population and territorial sovereignty claim as victims, with no structural beneficiaries (unlike false-summit mountains, beneficiaries from the regime are institutional actors external to the territorial claim itself — see omega variable on competing autochthony claims and one-state vs two-state structural delta).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING INSTANTIATION: This reading resolves potential mandatrophy by declaring the constraint as snare from the primary victim perspective and by routing the competing autochthony and remedial grounding questions to omega variables. The reading does not claim that snare is the 'true' classification for all contexts — it claims this is what the structure produces from the Palestinian autochthony framing. The sibling readings (zionist_refuge_reading, two_state_coexistence_reading) will produce different classifications from their different axioms and reference frames. The mandatrophy is resolved by the committer frame: each reading is one reading of a contested kernel, not a claim to universal classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    return_feasibility_threshold,
    'Is the right of return a structurally realizable remedy or is it invoked as a non-negotiable principle precisely because the structural barriers make it unrealizable?',
    'Empirical analysis: survey of return rates in historical displacement cases (post-WWII Europe, partition India-Pakistan, Cyprus); modeling of integration timelines and resource requirements; comparison with actual Palestinian return willingness vs negotiated return quotas in past agreements',
    'If realizable: constraint classification shifts toward tangled_rope (coordination problem with solution path). If unrealizable but asserted: constraint remains snare (impossibility-principle used to suppress remedy discourse, making the claim itself the extraction mechanism). If asymmetrically realizable (subset returns negotiable, large-scale return blocked): snare confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_feasibility_threshold, empirical, 'Structural feasibility of right of return as remedy').

omega_variable(
    autochthony_claim_grounding,
    'Does Palestinian autochthony claim ground legitimacy in continuous habitation (evidentiary base), in indigenous rights frameworks (normative base), in UN resolutions (legal base), or in displacement injustice requiring remedy (remedial base)? Are these grounding types compatible or in tension?',
    'Historiographical analysis: archaeological evidence, Ottoman and British administrative records, census data, oral histories. Normative analysis: consistency of indigenous rights frameworks with Palestinian legal position. Legal analysis: UN resolution 242, 194, current international law status. Remedial analysis: whether injustice argument depends on or is independent of autochthony evidence.',
    'If grounded primarily in continuous habitation: vulnerability to counterclaim via demographic change evidence. If grounded in indigenous rights: exposure to critique of indigenous framework applicability to settled agricultural societies. If grounded in injustice/remedy: strongest against empirical counterargument but faces question of whether remedy is symmetrical or asymmetrical. Mixing grounds creates ambiguity in what would falsify or validate the claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autochthony_claim_grounding, conceptual, 'Grounding type and compatibility of Palestinian autochthony claim').

omega_variable(
    displacement_trauma_perpetuation_mechanism,
    'Is trauma perpetuation (intergenerational transmission of displacement memory, institutional reinforcement of refugee status, prevention of normalization) a structural feature of the displacement regime itself, or is it a byproduct of failed integration policies?',
    'Comparative trauma studies: displacement trauma outcomes in cases where integration was offered vs cases where integration was structurally blocked; measurement of institutional vs psychological perpetuation mechanisms; analysis of refugee camp governance structures and their role in sustaining displacement identity',
    'If structural feature: suppression is intentional and the constraint is purely extractive (snare). If byproduct: constraint contains mixed coordination-extraction (tangled rope toward integration failure). Changes whether trauma is diagnostic of oppression or of incomplete transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_trauma_perpetuation_mechanism, empirical, 'Whether displacement trauma perpetuation is structural or contingent').

omega_variable(
    competing_autochthony_claim,
    'Does the zionist_refuge_reading''s claim to Jewish historical connection to the same territory as an autochthony claim (ancient Jewish kingdoms, rabbinic tradition, diaspora return aspiration) logically foreclose or coexist with Palestinian autochthony claim?',
    'Historiographical analysis: comparative strength of evidence bases for Jewish and Palestinian continuous habitation. Normative analysis: whether autochthony is a status that can be held simultaneously by two groups or is zero-sum. Legal analysis: how international law handles competing autochthony claims. Philosophical analysis: whether ''return'' (diaspora repatriation) and ''autochthony'' (never-left presence) can both ground territorial legitimacy.',
    'If forecloses: only one autochthony claim can be valid in any single framework, and choosing Palestinian reading means rejecting Jewish historical claim (high conflict potential, foreclosure gate applies). If coexists: both claims remain live but create asymmetric legitimacy structure (zionist reading emphasizes return, Palestinian emphasizes presence — different grounds, both claimed). Determines whether reading relations should include forecloses (rare) or coexists_with (more common in territorial disputes).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_autochthony_claim, conceptual, 'Logical relationship between Palestinian and Jewish autochthony claims').

omega_variable(
    one_state_vs_two_state_structural_delta,
    'Does the Palestinian autochthony reading entail one-state solution (Palestinians + Israelis in shared polity) or is it compatible with bounded two-state coexistence where autochthony is exercised within Palestinian state territory?',
    'Textual analysis: statements from Palestinian authorities, civil society, and rights movements on territorial scope of autochthony claim. Comparative analysis: how autochthony claims function in multicultural states vs ethnonational states. Scenario modeling: integration outcomes under one-state vs two-state configurations.',
    'If one-state entailed: constraint structure shifts from territorial sovereignty dispute to legitimacy dispute within shared polity (different classification topology). If two-state compatible: autochthony claim is about exercise of self-determination within defined territory, and coexistence reading becomes structurally possible. Changes whether coexistence_reading truly coexists or is foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(one_state_vs_two_state_structural_delta, conceptual, 'Scope of Palestinian autochthony claim: one-state vs two-state compatibility').

omega_variable(
    remedy_symmetry_asymmetry,
    'Is the remedial argument symmetric (both sides entitled to equivalent restitution and return) or asymmetric (Palestinian displacement is unique injustice requiring singular remedy)?',
    'Historiographical analysis: comparison with other mid-20th-century mass displacements (India-Pakistan partition, Greek-Turkish population exchange, Nazi expulsion of ethnic Germans). Normative analysis: whether singular injustice requires singular remedy vs whether all displacement claims are equivalent. Legal analysis: international law treatment of return rights across different displacement contexts.',
    'If symmetric: constraint grounds remedy in universal principles of repatriation, and zionist_refuge_reading''s displacement narrative (Jewish diaspora centuries earlier) becomes relevant comparative claim. If asymmetric: constraint grounds remedy in specific historical injustice timeline and scope, and zionist reading is disanalogy. Determines whether compensation should include Israeli return rights for post-1948 displacement of Jewish communities, or whether Palestinian return is singular entitlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_symmetry_asymmetry, preference, 'Symmetry of remedial claims in displacement justice framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(palestinian_autochthony_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pale_tr_t0, palestinian_autochthony_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pale_tr_t25, palestinian_autochthony_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(pale_tr_t50, palestinian_autochthony_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(pale_be_t0, palestinian_autochthony_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(pale_be_t25, palestinian_autochthony_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(pale_be_t50, palestinian_autochthony_reading, base_extractiveness, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(palestinian_autochthony_reading, identity_coordination).
narrative_ontology:affects_constraint(palestinian_autochthony_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(palestinian_autochthony_reading, two_state_coexistence_reading).
narrative_ontology:affects_constraint(palestinian_autochthony_reading, settler_colonial_extraction_logic).
narrative_ontology:affects_constraint(palestinian_autochthony_reading, right_of_return_enforcement).

% DUAL FORMULATION NOTE:
% The Palestinian autochthony reading is part of a constraint family on territorial legitimacy. The zionist_refuge_reading has its own ε, perspectives, and axioms instantiating the competing legitimacy claim. The two_state_coexistence_reading attempts to bracket competing claims and treat the problem as coordination rather than legitimacy dispute. Each story is independently valid as a reading of the kernel; they are linked via network.affects_constraints to show family structure and mutual influence. The measurement trajectories differ: autochthony reading shows rising extractiveness as settlements expand; coexistence reading shows therapy-like attempted stabilization that fails (repeated 'peace process' cycles); zionist reading shows affirming trajectory of demographic consolidation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
