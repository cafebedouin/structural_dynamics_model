% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__labor_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__labor_zionism_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_territorial_claim__labor_zionism_reading
 *   human_readable: Jewish Territorial Claim: Labor Zionism Reading
 *   domain: political_history/settler_colonialism/nationalism
 *
 * SUMMARY:
 *   Labor Zionism is a nationalist ideology that frames Jewish territorial
 *   settlement in Palestine as national regeneration through socialist
 *   transformation. The 'conquest of labor' (kibbush ha-avoda) is both
 *   coordination mechanism (collective settlement, pooled resources,
 *   cooperative economy) and extraction apparatus (systematic exclusion of
 *   Arab labor, land appropriation, economic enclosure). The reading claims
 *   the constraint is fundamentally about coordinating Jewish autonomy and
 *   productive redemption; the authorized metrics describe substantially
 *   extractive operation that requires active enforcement of labor
 *   exclusivity and territorial appropriation. This divergence is structural
 *   and intentional: the engine measures how the two seats (beneficiary and
 *   victim) compute different types from the same organizational structure.
 *
 * KEY AGENTS:
 *   - jewish_labor_movement: Coordinates and enforces the conquest of labor policy; sets settlement strategy; excludes Arab workers as mechanism of Hebrew economy construction
 *   - jewish_settler_community: Implements settlement through kibbutzim and moshavim; benefits from land access and ideological coherence; identity fused with settlement project
 *   - arab_laborers: Systematically excluded from Jewish employment; suffer downward wage pressure and labor displacement; powerless to resist
 *   - palestinian_peasants: Lose land through purchase and administrative appropriation; marginalized from the new Hebrew economy; constrained exit
 *   - yishuv_political_leadership: Coordinates labor policy with state-building strategy; manages mandate authority relations
 *   - british_mandate_authority: Enables settlement through administrative passivity; creates structural ambiguity that permits the constraint to operate
 *   - excluded_arab_political_leadership: Objects to the program but lacks enforcement capacity equivalent to the organized Hebrew economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, 0.68).
domain_priors:suppression_score(jewish_territorial_claim__labor_zionism_reading, 0.71).
domain_priors:theater_ratio(jewish_territorial_claim__labor_zionism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(jewish_territorial_claim__labor_zionism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__labor_zionism_reading, tangled_rope).
narrative_ontology:human_readable(jewish_territorial_claim__labor_zionism_reading, "Jewish Territorial Claim: Labor Zionism Reading").
narrative_ontology:topic_domain(jewish_territorial_claim__labor_zionism_reading, "political_history/settler_colonialism/nationalism").

domain_priors:requires_active_enforcement(jewish_territorial_claim__labor_zionism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__labor_zionism_reading, '89d0c04d-6c81-4ad7-83c9-1e0aafee5d89').
narrative_ontology:cs_kernel_codification('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', distributed).
narrative_ontology:cs_authority_grounding('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', lineage).
narrative_ontology:cs_interpretation_layer_present('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89').
narrative_ontology:cs_reading_relation('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', jewish_territorial_claim__cultural_zionism_reading, forecloses).
narrative_ontology:cs_reading_relation('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', jewish_territorial_claim__political_zionism_reading, influences).
narrative_ontology:cs_reading_relation('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', foundational, jewish_national_regeneration_requires_productive_labor).
narrative_ontology:cs_axiom_status(jewish_national_regeneration_requires_productive_labor, holdable).
narrative_ontology:cs_axiom_grounding('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', jewish_national_regeneration_requires_productive_labor, instrumental).
narrative_ontology:cs_axiom('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', foundational, hebrew_labor_exclusion_enables_jewish_autonomy).
narrative_ontology:cs_axiom_status(hebrew_labor_exclusion_enables_jewish_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', hebrew_labor_exclusion_enables_jewish_autonomy, empirically_contingent).
narrative_ontology:cs_axiom('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', secondary, incremental_settlement_builds_state_without_military_maximalism).
narrative_ontology:cs_axiom_status(incremental_settlement_builds_state_without_military_maximalism, holdable).
narrative_ontology:cs_axiom_grounding('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', incremental_settlement_builds_state_without_military_maximalism, instrumental).
narrative_ontology:cs_reference_frame('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', jewish_national_regeneration_via_labor).
narrative_ontology:cs_drift_state('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', post_1948_state_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89d0c04d-6c81-4ad7-83c9-1e0aafee5d89', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_labor_movement).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__labor_zionism_reading, jewish_settler_community).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, arab_laborers).
narrative_ontology:constraint_victim(jewish_territorial_claim__labor_zionism_reading, palestinian_peasants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes the ideological and practical program of 'conquest of labor' (kibbush ha-avoda): Jewish workers colonize the Palestinian territory through collective agricultural settlement, cooperative economy, and exclusion of Arab labor from Jewish-controlled enterprises. They set labor policy, coordinate settlement placement, and manage the Hebrew economy's boundaries. They benefit from territorial acquisition, collective ownership, and the vision of Jewish regeneration through productive labor.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_labor_movement, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__labor_zionism_reading, jewish_labor_movement, beneficiary).

% Settlers acquire land, establish kibbutzim and moshavim, build productive capacity, and construct the material and social infrastructure of Jewish Palestine. They benefit from land access, ideological coherence (redemption through labor), and incremental sovereign capacity. Their identity is constituted through the act of settlement itself and the transformation narrative.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, jewish_settler_community, beneficiary,
    moderate, generational, identity_locked, regional).

% Are systematically excluded from employment in Jewish-controlled agricultural and industrial enterprises through a coordinated economic policy. They lose access to labor opportunities and face downward wage pressure where employment persists. They cannot organize effectively against the policy; geographic and economic dependency trap them. Their labor is displaced by Jewish settlement rather than incorporated into it.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, arab_laborers, payer,
    powerless, immediate, trapped, local).

% Face land acquisition by settlers (through legal purchase, settlement placement, and later state appropriation), subsistence disruption as the Hebrew economy builds around them but excludes them, and the material transformation of the territory into a separate economic sphere. They retain some formal property and political voice but are structurally marginalized from the primary economic project.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, palestinian_peasants, payer,
    moderate, biographical, constrained, regional).

% Coordinates the conquest of labor program with territorial settlement strategy, manages relations with British mandate authority, and adjudicates claims between labor ideology and political sovereignty goals. They treat labor exclusivity as the mechanism for incremental state-building and demographic consolidation.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, yishuv_political_leadership, agenda_setter,
    organized, generational, analytical, regional).

% Administers the territory under League of Nations mandate following World War I. They permit Jewish settlement and labor organization within legal frameworks; they also permit Arab protest and land retention. Their passivity and structural ambiguity (dual obligation to Jewish immigration and Arab rights) enables the constraint to operate with moderate enforcement visibility.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, british_mandate_authority, observer,
    institutional, biographical, constrained, regional).

% Would contest the labor exclusion policy and the land acquisition pattern as structural dispossession, but their voice is mediated through the same mandate authority that enabled Jewish settlement. They articulate resistance through petitions, economic counter-organization, and political representation, but lack enforcement capacity equivalent to the organized Hebrew economy.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, excluded_arab_political_leadership, excluded,
    moderate, generational, constrained, regional).

% Observes the labor Zionist program as a potential model for socialist reconstruction and worker redemption. Some factions provide ideological legitimacy and international coordination; others are ambivalent about the territorial and exclusionary mechanics. Their observation lends authority to the labor socialism framing.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__labor_zionism_reading, international_socialist_movement, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__labor_zionism_reading, jewish_labor_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__labor_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national regeneration through collective labor and productive settlement: kibbutzim and labor collectives pool resources, share risk, and construct the material infrastructure of Jewish self-governance. The Hebrew labor economy solves coordination problems of capital scarcity, land fragmentation, and the need for collective security and social cohesion that individual settlement could not achieve.
% TRANSFER_FUNCTION: Transfers control of land and labor opportunity from Arab peasants and landowners to Jewish settler collectives. Arab workers are excluded from employment in Jewish enterprises; their labor is replaced by Jewish labor at higher cost but with ideological meaning. Palestinian peasants lose access to land through purchase and later administrative appropriation; peasant labor is extracted from the land by the new owners but peasants themselves are excluded from the new economic order.
% ABSENT_VOICES: Arab laborers have no seat at the table of labor policy; they cannot negotiate the terms of their exclusion or propose alternative economic arrangements. Arab landowners and Palestinian political leadership voice objection through the mandate framework but lack enforcement capacity. International socialist movements sympathetic to Jewish labor have largely not engaged the territorial and exclusionary premises of the program.
% DISAPPEARANCE_RATIONALE: If the labor Zionist constraint—the coordinated exclusion of Arab labor, the settlement program, and the incremental sovereign construction—vanished, the territorial reorganization of Palestine would unwind. Land would revert or be restituted; the separate Hebrew economy would dissolve into regional labor markets; Jewish political capacity would be severely diminished without the territorial and institutional facts on the ground. The settlement project is the material and ideological substrate of Jewish Palestine.
% FOUNDING_PROBLEM: Jewish national survival and regeneration in the face of European antisemitism and diaspora marginalization: how can Jews rebuild collective identity, autonomous economic capacity, and territorial self-determination? The labor Zionist reading frames this as achievable through productive labor, collective settlement, and the transformation of Jews from diaspora intellectuals and merchants into farmers and workers rooted in the land.
% FOUNDING_PROBLEM_CORROBORATION: Labor Zionist ideologists and early settlers attest the founding problem is live and the program is its solution. Palestinian leadership and Arab political observers contest both the framing (they deny Jews face existential threat requiring territorial displacement) and the solution (they argue labor Zionism appropriates Palestinian territory and labor). British mandate officials and international observers acknowledge the antisemitism context but dispute whether territorial appropriation follows logically. Post-1948 scholarship, including critical Israeli historians and Palestinian scholarship, contests whether the problem could not have been addressed through non-territorial means or co-territorial accommodation.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__labor_zionism_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_territorial_claim__labor_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__labor_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__labor_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__labor_zionism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__labor_zionism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_territorial_claim__labor_zionism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_territorial_claim__labor_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness increases monotonically from 0.35 (early settlement, limited scope) to 0.68 (1948, near-total institutional consolidation) as the Hebrew economy encircles the territory and Arab exclusion becomes systemic rather than incidental. Suppression rises faster (0.38 to 0.71) because active enforcement of labor exclusivity required—without coordinated policy, Arab workers would enter Jewish enterprises; without land controls, settlement would not consolidate; without political coordination, the yishuv's institutions would fracture. Theater ratio grows from 0.12 to 0.42 because the ideological framing (socialist redemption, moral labor) increasingly operates as cover for territorial appropriation and demographic displacement. By 1948, a substantial share of enforcement activity is devoted to maintaining the appearance of socialist coordination rather than defending the labor program itself—what began as a solution to capital scarcity has become a mechanism for managing Arab economic competition. The measurements are authored on a single shared time grid (1882, 1900, 1920, 1930, 1940, 1948) so every metric is available at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Jewish labor movement), the constraint is genuine coordination solving problems of capital scarcity, security, and collective regeneration. From the payer seat (Arab laborers), it is enforced economic exclusion. From the Palestinian peasant seat, it is land appropriation dressed in labor ideology. The organizational structure is identical—kibbutzim, labor collectives, land purchase—but the seated perceiver of that structure computes radically different types. The engine derives this divergence directly from the structural data (power atoms, exit options, beneficiary/victim declarations) without requiring the authoring seat to pre-adjudicate which reading is 'correct.' The labor Zionist reading supplies the coordination narrative; the engine measures what that narrative computes as from each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish labor movement: powerful+organized, generational horizon, constrained exit (ideologically committed to the project), beneficiary role = d near 0.1 (full beneficiary subsidy). Jewish settlers: moderate power, generational, identity_locked exit (their identity is constituted through settlement), beneficiary role = d near 0.15 (beneficiary with identity fusion). Arab laborers: powerless, immediate horizon, trapped exit (economic dependency, geographic isolation, no alternative employment), payer role = d near 0.95 (full target). Palestinian peasants: moderate power, biographical horizon, constrained exit (can organize politically but lack economic alternatives), payer role = d near 0.78 (substantial target, less than laborers because they retain some formal rights and political voice). The derivation chain runs beneficiary/victim + exit_options → d; where structural data is clean (trapped powerless laborer) the derivation is unambiguous. Where it is ambiguous (identity-locked settler whose benefit is ideological), the override mechanism exists but is not invoked here—the beneficiary role + identity_locked exit already produces the right d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope on the authored claim: there is genuine coordination (Hebrew labor movement solves collective action problems of capital scarcity, security, and regeneration) AND asymmetric extraction (Arab labor is excluded, peasants are displaced). The engine will compute per-seat types—the beneficiary seat (Jewish labor) will likely compute rope (coordination dominant, extraction minimal from their perspective); the payer seat (Arab laborers) will compute snare (the coordination narrative is cover for exclusion); the analytical seat (yishuv political leadership) will compute tangled_rope or snare depending on whether they weight coordination or extraction in their steering logic. The authored claim (tangled_rope) reflects the constraint's structural truth: both coordination and extraction are present, neither is dispensable, and persistence requires active enforcement of both. The metrics support this: extractiveness at 0.68 is substantial (not coordination-only); suppression at 0.71 is higher still (the extraction must be actively defended); theater at 0.42 indicates that a growing share of the apparatus is performative. A purely extractive constraint (snare) would show theater near 0.1-0.2; a purely coordination constraint (rope) would show theater near 0.05-0.15. A tangled rope at 0.42 theater suggests the coordination function is gradually being replaced by the ideological apparatus that defends it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_socialism_vs_territorial_exclusion,
    'Is the ''conquest of labor'' fundamentally a socialist coordination mechanism, or is it structurally dependent on territorial appropriation and Arab exclusion?',
    'Counterfactual analysis: could the labor movement achieve its socialist goals (collective ownership, worker autonomy, redemption through productive labor) without excluding Arab workers and appropriating Arab land? Examine attempted alternatives (binational labor movements, co-operative Arab-Jewish enterprises) and their outcomes.',
    'If separable: the constraint should be decomposed into two stories—one about socialist labor coordination (rope, low extraction), one about territorial appropriation (snare, high extraction). If inseparable: the constraint is genuinely tangled because the socialism is the ideological machinery that makes the territorial exclusion politically sustainable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_socialism_vs_territorial_exclusion, conceptual, 'Whether labor socialism and territorial exclusion are structurally coupled or incidental combinations.').

omega_variable(
    founding_problem_contest,
    'Is the founding problem (Jewish national survival and regeneration in the face of antisemitism) authentically live in the Palestinian context, or is it a borrowed justification for territorial settlement?',
    'Examine non-territorial alternatives proposed by contemporary actors (cultural Zionism, socialist internationalism, diaspora autonomy movements) and their feasibility. Compare the historical antisemitism context in Europe with the actual security environment in Ottoman/Mandate Palestine at the time of settlement.',
    'If the problem is authentically live in Palestine, the constraint''s founding justification holds and the labor reading''s framing is internally coherent. If the problem is primarily European and not territorially specific, the constraint becomes a cover story and should be reclassified as snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_contest, conceptual, 'Whether the founding problem justifies the territorial method chosen.').

omega_variable(
    suppression_internalization,
    'Among Arab workers and Palestinian peasants, is the suppression primarily structural (external barriers, economic coercion, legal prohibition) or internalized (belief that Hebrew labor is superior, acceptance of their own exclusion)?',
    'Post-exit suppression trajectory: if Arab workers who exit the labor market (through migration, employment outside the Hebrew economy, or political organization) retain the belief in Hebrew labor superiority, suppression is partially internalized. If they construct alternative economic systems and psychological frames once barriers are removed, suppression is primarily structural.',
    'If internalized: the effective suppression is higher than the structural measure suggests—the targets carry the suppression with them after exit, making the constraint more resilient. If structural: opening alternatives would rapidly erode the constraint. This feeds the omega about whether suppression can be asymptotically reduced or is tied to identity fusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of Arab labor is structural or internalized.').

omega_variable(
    kernel_reading_coherence,
    'Does the labor Zionist reading coherently instantiate a single constraint, or does it elide two structurally distinct claims: (1) Jewish national regeneration is achievable via collective labor, and (2) Jewish national regeneration requires Palestinian territorial displacement?',
    'Examine the ideological texts and programmatic writings of labor Zionist leaders: do they argue that territorial appropriation is necessary for socialism, or that socialism merely happens to require it? Would labor socialists accept a non-territorial framework if offered one?',
    'If the claims are coupled: the labor reading is coherent and the constraint is genuinely tangled. If decoupled: the reading elides them and should be split into two constraints with different ε values and claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coherence, conceptual, 'Whether the labor reading coherently couples labor socialism with territorial appropriation.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the labor Zionist reading''s economic-separation strategy logically foreclose the cultural Zionism reading (Jewish cultural center without territorial majority or political exclusivity), or can both coexist?',
    'Examine whether cultural Zionists could have accepted an integrated economy (Arab-Jewish joint enterprises) and a cultural institution (University, theaters, publishing) operating at parity with Arab cultural institutions. If yes, the readings coexist; if no, labor Zionism forecloses cultural Zionism.',
    'If foreclosed: labor Zionism and cultural Zionism are logically incompatible within a single framework, and only one can be true. If coexist: they are different value orderings held by different factions, neither logically eliminating the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether labor Zionism logically forecloses cultural Zionism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__labor_zionism_reading, 1882, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1882, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1882, 0.12).
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(jewi_tr_t1930, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1930, 0.36).
narrative_ontology:measurement(jewi_tr_t1940, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1940, 0.4).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__labor_zionism_reading, theater_ratio, 1948, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1882, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1882, 0.35).
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1900, 0.41).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(jewi_be_t1930, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1930, 0.61).
narrative_ontology:measurement(jewi_be_t1940, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1940, 0.65).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__labor_zionism_reading, base_extractiveness, 1948, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1882, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1882, 0.38).
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1900, 0.45).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(jewi_su_t1930, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1930, 0.67).
narrative_ontology:measurement(jewi_su_t1940, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1940, 0.7).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__labor_zionism_reading, suppression_requirement, 1948, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__labor_zionism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__labor_zionism_reading, 0.18).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__cultural_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, hebrew_labor_exclusion_policy).
narrative_ontology:affects_constraint(jewish_territorial_claim__labor_zionism_reading, palestinian_land_appropriation_pattern).

% DUAL FORMULATION NOTE:
% The jewish_territorial_claim kernel has four distinct readings, each with different ε values, beneficiary/victim structures, and claims. Labor Zionism (this story) emphasizes economic separation and incremental state-building; Political Zionism emphasizes territorial sovereignty as instrumental to Jewish security; Cultural Zionism emphasizes cultural center without necessarily territorial majority; Revisionist Zionism emphasizes maximalist territorial claim with military enforcement. Each reading is authored as a separate constraint story, linked via network.affects_constraints to enable cross-reading comparison. The labor reading's ε is substantially higher (0.68) than cultural Zionism's (estimated 0.35-0.45) because labor Zionism couples ideological redemption with actual territorial appropriation, while cultural Zionism could theoretically coexist with Arab sovereignty. This difference in ε is structural, not observational—it reflects different referents (what the reading is actually about), not different measurements of the same thing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
