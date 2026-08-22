% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Settler-Colonial Project: Displacement as State-Building (Settler-Colonial Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint represents one reading of the contested kernel 'Zionist
 *   legitimacy basis.' The settler-colonial reading interprets Zionism as a
 *   European settler-colonial project that establishes an ethno-state through
 *   the systematic displacement of an indigenous Palestinian population. It
 *   identifies the constraint as the apparatus through which European Jewish
 *   settlement, land appropriation, and political sovereignty are established
 *   on territory inhabited and governed by Palestinians for centuries. The
 *   settler-colonial reading recognizes displacement as constitutive and
 *   structural to the project, not incidental or temporary. This reading
 *   coexists with two sibling readings: the national-liberation reading
 *   (Zionism as Jewish people's return and self-determination against
 *   historical dispersion and persecution) and the religious-restoration
 *   reading (Zionism as fulfillment of religious covenant and messianic
 *   process). These readings share the same historical events but frame their
 *   meaning and legitimacy differently. The settler-colonial reading
 *   specifically identifies structural parallels to other settler-colonial
 *   projects (North America, Australia, South Africa, Algeria) and applies
 *   settler-colonialism theory to the instantiation.
 *
 * KEY AGENTS:
 *   - European Jewish settlers: organized migration and settlement, forming political and military institutions, benefiting from displacement and state establishment
 *   - Zionist movement leadership: agenda-setting organization, negotiating with colonial authorities, strategically directing settlement and state-building
 *   - Palestinian indigenous population: powerlessly targeted, displaced from land and villages, excluded from political voice, bearing costs directly and across generations
 *   - British mandate authority: provides legal and military framework enabling displacement; benefits geopolitically from state creation
 *   - Western international powers: benefit from Western-aligned state in strategic region; provide diplomatic recognition and institutional backing
 *   - Anti-colonial analysts: document displacement structure and settler-colonial parallels; excluded from mainstream Western institutional recognition during the interval
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.87).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.81).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Settler-Colonial Project: Displacement as State-Building (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, 'd63134fc-252a-417d-8ef1-4a195f2b777a').
narrative_ontology:cs_kernel_codification('d63134fc-252a-417d-8ef1-4a195f2b777a', formalized).
narrative_ontology:cs_authority_grounding('d63134fc-252a-417d-8ef1-4a195f2b777a', extraction).
narrative_ontology:cs_interpretation_layer_present('d63134fc-252a-417d-8ef1-4a195f2b777a').
narrative_ontology:cs_reading_relation('d63134fc-252a-417d-8ef1-4a195f2b777a', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d63134fc-252a-417d-8ef1-4a195f2b777a', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('d63134fc-252a-417d-8ef1-4a195f2b777a', foundational, displacement_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(displacement_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('d63134fc-252a-417d-8ef1-4a195f2b777a', displacement_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('d63134fc-252a-417d-8ef1-4a195f2b777a', foundational, indigenous_presence_grounds_territorial_claim).
narrative_ontology:cs_axiom_status(indigenous_presence_grounds_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('d63134fc-252a-417d-8ef1-4a195f2b777a', indigenous_presence_grounds_territorial_claim, deontological).
narrative_ontology:cs_reference_frame('d63134fc-252a-417d-8ef1-4a195f2b777a', indigenous_palestinian_territorial_sovereignty_pre_colonization).
narrative_ontology:cs_drift_state('d63134fc-252a-417d-8ef1-4a195f2b777a', state_establishment_1948, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('d63134fc-252a-417d-8ef1-4a195f2b777a', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_movement_leadership).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, arab_communities_in_mandate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, british_mandate_authority).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, international_western_powers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_diaspora).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, settler_colonialism_as_structural_model).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, displacement_as_state_foundation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Jewish immigrants establish settlements on Palestinian-inhabited land, acquire property through purchase and seizure, build political institutions and military organizations, and eventually obtain citizenship and political voice in a new state. They leverage European capital, industrial knowledge, and institutional experience to build state infrastructure. Their exit option was to remain in Europe or migrate to other countries; instead, they chose Palestine because it offered the possibility of establishing a Jewish-majority polity. They directly benefit from the land appropriated and the state apparatus created.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, european_jewish_settlers, beneficiary,
    organized, generational, arbitrage, continental).

% Zionist political organizations (Zionist Congress, Jewish Agency, labor movements, defense forces) coordinate settlement strategy, acquire land, negotiate with colonial authorities, build military and administrative infrastructure, and establish the legitimating narratives for displacement. They manage the apparatus's expansion and enforce the constraint's maintenance through law, military force, and institutional administration. They set the strategic direction and maintain the project's momentum across the 66-year interval.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_movement_leadership, agenda_setter,
    institutional, generational, mobile, global).

% Palestinians inhabit the land for centuries, govern villages and regions, own property, and maintain a connected community. As European Jewish settlement accelerates, they experience land loss (through purchase at coercive low prices, through military conquest, through legal confiscation), displacement from villages, loss of property, political subordination, and exclusion from the new state's citizenship and governance. Their resistance is met with military suppression (Arab Revolt, war). By 1948, most Palestinians are displaced and refugee, stateless, permanently excluded from return by Law of Return and occupation law. They are trapped by geography, lack of external patron with effective military force, and the apparatus's prevention of return.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, regional).

% Broader Arab communities in Palestine and surrounding Levantine regions are excluded from the settlement apparatus's decision-making. They experience displacement flows, refugee populations within their territories, political destabilization, and loss of a potential claim to the territory. Neighboring Arab states attempt military intervention but lack the capacity to prevent displacement. They bear costs through refugee absorption and regional instability but have constrained capacity to reverse the displacement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, arab_communities_in_mandate, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, arab_communities_in_mandate, excluded).

% Britain holds League of Nations mandate authority over Palestine following the Ottoman collapse. The mandate structure provides the legal framework under which settlement accelerates: Britain grants concessions to Zionist organizations, allows land purchase and settlement, provides military protection for settlements, and facilitates institutional development (Jewish Agency, Haganah). Britain benefits geopolitically from a state aligned with Western interests and later from military alliances. The mandate authority administers the apparatus through which displacement occurs and provides the institutional legitimacy for land confiscation and settlement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, british_mandate_authority, agenda_setter,
    institutional, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, british_mandate_authority, beneficiary).

% Western powers (Britain, US, France) benefit from the settlement politically and strategically. The Balfour Declaration (Britain), mandate grant (League of Nations), and diplomatic recognition (US, others) constitute institutional support. They view a Western-aligned Jewish state in the Middle East as advancing Western geopolitical interests (containing Arab nationalism, securing oil access, establishing anti-Soviet alliance). Western powers maintain the settlement through diplomatic recognition, military and economic aid, and institutional backing in international organizations.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_western_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Anti-colonial scholars, historians, and political analysts (some Palestinian, some international) document the displacement, identify settler-colonial structural parallels to North America, Australia, Algeria, and South Africa. They produce accounts naming displacement as constitutive to the project's structure. They are analytical observers outside the beneficiary coalition, positioned to see the constraint's operation from outside the constraint's legitimating narratives.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, anti_colonial_observers, observer,
    analytical, generational, analytical, global).

% Palestinian refugees and their descendants, dispersed across multiple countries (Syria, Lebanon, Jordan, Egypt, and diaspora globally) and stateless, bear the constraint's extraction across generations. They are excluded from return by Law of Return, which admits Jews globally but Palestinian refugees nowhere. They depend on UNRWA aid, live in camps or as stateless residents, and pass down the experience of displacement to their children. Their trapped status is reinforced by international law and the apparatus's legal prevention of return. They carry the dispossession across generations and borders.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_diaspora, payer,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, zionist_movement_leadership).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settler-colonial reading identifies no coordination function in the traditional sense: instead, the structure solves a problem exclusively for the settler population (European Jewish migration and political refuge) while imposing its solution onto an indigenous population that did not consent. The only 'coordination' is internal to the settler apparatus: coordinating land acquisition, settlement, and state-building among European Jewish organizations and their Western patrons.
% TRANSFER_FUNCTION: Moves ownership and political authority from Palestinian indigenous holders to European Jewish settlers, transferred through land purchase (often at coercive low prices), military conquest, and administrative law. Transfers displacement and statelessness from Europeans to Palestinians. The transfer is asymmetric: Palestinians lose land, property, political voice, and security; settlers gain land, property, political voice, and the apparatus of state power.
% ABSENT_VOICES: Palestinian voice is structurally excluded from the Zionist apparatus's decision-making. Palestinians had no vote in the settlement decisions, no seat in Zionist organizations, and no formal representation in the mandate authority where the key institutional choices were made. Arab state voices were present but powerless to stop the displacement. Anti-colonial observers documenting the structure as settler-colonial were excluded from mainstream Western institutional discourse until decades later.
% DISAPPEARANCE_RATIONALE: If the settler-colonial apparatus and its displacing force vanished overnight, the Palestinian population would reclaim land and rebuild political institutions; Israeli state institutions would collapse or radically reconfigure without the settler-legitimacy narrative and the appropriated land base. The entire political geography of the region would reorganize around indigenous Palestinian claims and Arab state sovereignty rather than Jewish settler sovereignty.
% FOUNDING_PROBLEM: From the European perspective: Jewish persecution in Europe and the need for a refuge territory where European Jews could establish political sovereignty and escape antisemitism. From the settler-colonial analysis: this founding problem is real but solvable without displacement—through immigration to existing states, through integrating into European societies, or through establishing a refuge state on European territory or in an uninhabited region. Instead, the chosen solution was displacement of an inhabited indigenous population, which suggests the founding problem does not explain the specific form of the solution.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historians and Jewish refugee advocates attest that European Jewish persecution was real and urgent (Holocaust evidence, pogroms, discriminatory law are well-documented). Anti-colonial historians and Palestinian scholars attest that the founding problem was solvable through non-displacing means, and that the choice to displace Palestinians rather than negotiate a shared governance structure or respect indigenous land claims reveals that the stated founding problem was not the sole driver of the project. The settler-colonial reading asserts: if the problem was purely refuge, the solution would not have required displacement as its constitutive core.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 0.15 (early individual land purchases with minimal organized enforcement) to 0.87 (state apparatus at full capacity in 1948, displacement complete, law and military fully mobilized). Suppression requirement follows the same trajectory: early-phase land acquisition faced Palestinian resistance but limited organized military response; by 1936 (Arab Revolt) and 1948 (War of Independence), suppression becomes the dominant enforcement mechanism—military force is necessary to maintain displacement and prevent Palestinian return. Theater ratio rises from 0.15 to 0.62 because early narratives of 'making the desert bloom' and 'land without a people' carry more performative weight relative to the actual displacement activity; by 1948, the theater is significant (portraying forced displacement as legitimate state-building, framing resistance as terrorism, narrating ethno-state creation as universal liberation) but the underlying extraction mechanism is visible to observant parties. The measurement series is aligned on one shared grid: every metric is authored at the same five time points (1882, 1900, 1920, 1936, 1948) so temporal analysis reads a coherent picture of rising extraction, suppression, and theatrical maintenance across the 66-year interval.
 *
 * PERSPECTIVAL GAP:
 *   From the Zionist agenda-setter's seat, the arrangement appears as national liberation and refuge-building (a rope or tangled-rope coordination problem solved), justified by the founding problem of persecution and the religious/historical claim to the land. From the Palestinian target seat, the same arrangement operates as settler colonialism and displacement. The engine computes this per-seat divergence from the authored structural data: the beneficiary position (settlers) and the victim position (Palestinians) derive different effective extraction values because one collects from the constraint and one is dispossessed by it. The authored claim (snare) reflects the settler-colonial reading's judgment that the structure is primarily extraction, not coordination—a claim the sibling readings reject. The engine's per-seat computation will confirm that beneficiary and target seats see incompatible types, which is exactly what the settler-colonial reading predicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the beneficiary/victim declarations and exit analysis. European Jewish settlers and Zionist leadership benefit directly (low d), so the engine derives d near 0.0-0.15 from the beneficiary role + organizational power + arbitrage/mobile exit (they can leverage European capital and knowledge; they chose Palestine, not because they had no alternatives, but because it served their strategic interests). Palestinian population is declared as victim with powerless power level, trapped exit (no alternative territory, no external patron with effective force to restore them), and regional scope—the engine derives d near 1.0, full target. This asymmetry is the directionality structure that stabilizes the snare classification. No overrides are needed because the derivation chain from the structural data captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The settler-colonial reading identifies a clear mandatrophy: the founding mandate is 'establish a refuge for persecuted European Jews,' but the evolving function becomes 'maintain a Jewish ethno-state through permanent exclusion of indigenous Palestinians.' The original problem (European Jewish persecution) is solvable without displacement and becomes obsolete once the state is established; yet the displacement apparatus persists and requires increasing suppression to maintain (Arab Revolts, wars, occupation law). The founding problem does not justify the specific form of the solution (displacement rather than shared governance, negotiated integration, or refuge in uninhabited land). The settler-colonial reading interprets this drift as evidence that the displacement was not incidental to solving the founding problem but constitutive of the agenda from the outset—the founding problem provided moral cover, not the actual driver of the project structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_vs_solution_mismatch,
    'Does the founding problem (European Jewish persecution) logically necessitate the specific solution (displacement of Palestinians), or would non-displacing solutions (immigration to existing states, establishing refuge state in uninhabited territory, negotiated shared governance) adequately address the founding problem?',
    'Comparative historical analysis of alternative solutions proposed and rejected; examination of Zionist leadership documents revealing whether displacement was viewed as necessary or chosen instrumentally.',
    'If non-displacing solutions were sufficient, the choice of displacement suggests the settler-colonial reading is correct—displacement was not mandated by the founding problem but was the preferred agenda. If displacement was truly the only viable solution, the constraint''s classification would shift toward coordinate-extraction hybrid (tangled_rope) rather than pure extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_vs_solution_mismatch, empirical, 'Whether displacement is necessitated by the founding problem or instrumentally chosen.').

omega_variable(
    indigenous_claim_validity_across_readings,
    'What epistemic weight should Palestinian claims to land and governance carry relative to Jewish historical/religious claims? Is this a factual question resolvable by evidence, or a normative question depending on which reading''s axioms one adopts?',
    'This is a conceptual/preference question: a settler-colonial reading prioritizes indigenous residence and governance as the grounding for territorial claim; a national-liberation reading prioritizes historical/religious connection; a religious-restoration reading prioritizes divine covenant. No empirical test resolves the weighting across axioms.',
    'Resolution determines which reading''s legitimacy claim is accepted. If indigenous residence is the canonical grounding (settler-colonial reading), then displacement is illegitimate. If historical/religious claim overrides indigenous residence (national-liberation or religious-restoration readings), then displacement is justified. If the weightings are themselves contested, then the kernel remains genuinely under-determined—no single reading can claim universal authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_claim_validity_across_readings, conceptual, 'The epistemically irreducible disagreement about which claims ground territorial legitimacy.').

omega_variable(
    settler_vs_refugee_classification,
    'Are European Jewish migrants to Palestine ''settlers'' (arriving to establish a new polity on inhabited land) or ''refugees'' (fleeing persecution and arriving to join or establish a refuge)? The classification determination shifts the reading''s framing.',
    'Examine Zionist organizational intent: if the primary intent was refuge-seeking (individual or community safety), the refugee framing gains credence; if the primary intent was state-building and political sovereignty (organizational entity reproduction), the settler framing is supported. The evidence is mixed: some migrants fled persecution (refugee motive), but Zionist organizations strategically directed migration and land acquisition toward state-building (settler motive). Both may be true for different cohorts and times.',
    'Pure refugee status would support the national-liberation reading (persecuted people finding safety). Settler status supports the settler-colonial reading (organized project of political sovereignty on inhabited land). Mixed status means both readings partially capture the phenomenon; the settler-colonial reading argues that settler intent dominated and instrumentalized refugee narrative for legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_vs_refugee_classification, empirical, 'Whether the primary motivation was refuge-seeking or state-building, and how this determination affects reading classification.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can all three readings (settler-colonial, national-liberation, religious-restoration) coexist as live positions held by different parties, or does one reading''s core premise logically foreclose the others?',
    'Logical analysis of the axiom sets: do the foundational claims of each reading contradict the others in a way that no single framework could hold all three, or can they be understood as different weightings of overlapping concerns (persecution, indigenous displacement, religious restoration) that different parties emphasize differently?',
    'If readings coexist (coexists_with relation), the kernel is genuinely contested and no reading achieves universal authority. If one reading forecloses the others (forecloses relation), that reading''s core premise is logically incompatible with the others, and the kernel involves a fundamental contradiction that must be resolved. The settler-colonial reading and religious-restoration reading seem to foreclose each other on the indigenous-claim axis; national-liberation might coexist with either if it emphasizes self-determination without commitment to a specific territorial form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the three readings can logically coexist or whether some readings foreclose others.').

omega_variable(
    suppression_internalization,
    'To what extent is Palestinian resistance to displacement suppressed by structural barriers (military force, law, economic dependency) versus internalized beliefs (acceptance of Israeli legitimacy, assimilation into occupation narratives)? The suppression metric (0.81) masks this distinction.',
    'Post-displacement trajectories: if Palestinian resistance persists across generations despite force and law, suppression is primarily structural; if resistance dissipates as new generations grow up under occupation, suppression has partially internalized. Multi-generational studies of Palestinian political consciousness and resistance patterns provide evidence.',
    'If suppression is structural, the constraint''s persistence depends on maintaining military and legal force; if internalized, the constraint has deeper institutional roots and is harder to reverse. The classification (snare) holds either way, but the stability and reversibility implications differ significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'The mechanism of suppression—structural or internalized—and its implications for constraint stability.').

omega_variable(
    alternative_readings_epistemic_status,
    'Are the sibling readings (national-liberation, religious-restoration) empirically false accounts, or are they reading-indexed knowledge claims that capture real features of the phenomenon the settler-colonial reading emphasizes differently?',
    'This is a meta-epistemic question about how alternative readings relate to truth. Are they false claims that should be corrected, or alternative valid framings that emphasize different features? The settler-colonial reading asserts that displacement is constitutive; the national-liberation reading asserts that Jewish self-determination is constitutive. Both can be factually true—displacement did occur, Jewish self-determination did occur—yet the readings diverge on which feature determines legitimacy. This is not a falsifiability question but a framing question.',
    'If readings are equally valid framings of the same events (OQ-26 reading-indexing model), then the kernel is irreducibly contested and no single reading achieves universal authority. If one reading is objectively true and others false, then the kernel is resolvable through better evidence or argument. The settler-colonial reading assumes the first (readings are indexed); the national-liberation and religious-restoration readings often assert the second (their reading is objectively true).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_readings_epistemic_status, preference, 'Whether alternative readings are false claims or alternative valid framings of the same events.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1882, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t1882, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1882, 0.15).
narrative_ontology:measurement_basis(zion_tr_t1882, observed).
narrative_ontology:measurement(zion_tr_t1900, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement_basis(zion_tr_t1900, observed).
narrative_ontology:measurement(zion_tr_t1920, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1920, 0.38).
narrative_ontology:measurement_basis(zion_tr_t1920, observed).
narrative_ontology:measurement(zion_tr_t1936, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1936, 0.52).
narrative_ontology:measurement_basis(zion_tr_t1936, observed).
narrative_ontology:measurement(zion_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.62).
narrative_ontology:measurement_basis(zion_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(zion_be_t1882, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1882, 0.15).
narrative_ontology:measurement_basis(zion_be_t1882, observed).
narrative_ontology:measurement(zion_be_t1900, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement_basis(zion_be_t1900, observed).
narrative_ontology:measurement(zion_be_t1920, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement_basis(zion_be_t1920, observed).
narrative_ontology:measurement(zion_be_t1936, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1936, 0.71).
narrative_ontology:measurement_basis(zion_be_t1936, observed).
narrative_ontology:measurement(zion_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.87).
narrative_ontology:measurement_basis(zion_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t1882, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1882, 0.2).
narrative_ontology:measurement_basis(zion_su_t1882, observed).
narrative_ontology:measurement(zion_su_t1900, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement_basis(zion_su_t1900, observed).
narrative_ontology:measurement(zion_su_t1920, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(zion_su_t1920, observed).
narrative_ontology:measurement(zion_su_t1936, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1936, 0.72).
narrative_ontology:measurement_basis(zion_su_t1936, observed).
narrative_ontology:measurement(zion_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.81).
narrative_ontology:measurement_basis(zion_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__settler_colonial_reading, 0.18).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugee_status__law_of_return).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, occupation_law__west_bank_administration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'Zionist legitimacy basis' and is structurally inseparable from its sibling readings (national_liberation_reading, religious_restoration_reading). The three readings share the same historical events and institutions but frame their meaning and legitimacy differently. The settler-colonial reading emphasizes the constitutive role of displacement and applies settler-colonialism theory; the national-liberation reading emphasizes Jewish self-determination and persecution-response; the religious-restoration reading emphasizes covenant fulfillment and messianic process. These readings coexist across different parties' commitments (Israeli institutions and supporters adopt national-liberation and religious-restoration framings; Palestinian scholars and anti-colonial analysts adopt settler-colonial framing). The network edge indicates that classification of this constraint directly affects the classification of its siblings: treating Zionism as settler-colonial implies the national-liberation reading misconstrues the phenomenon; treating it as national liberation implies the settler-colonial reading over-emphasizes displacement relative to other constitutive features.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
