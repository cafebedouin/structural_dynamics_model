% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Zionist State Framework and Ethnic-National Privilege Structure
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The post-Zionist reading interprets the Israeli state as having achieved
 *   its founding coordination function (Jewish statehood, security refuge,
 *   diaspora gathering) but now operating primarily as an institutional
 *   mechanism for ethnic-national privilege that obstructs civic equality and
 *   regional integration. This reading examines the constraint through the
 *   lens of those who experience the ethnic-national framework as extractive
 *   rather than protective: Palestinian citizens lacking equal legal status,
 *   occupied Palestinians denied self-determination, and displaced
 *   Palestinian diaspora denied return. The reading does not deny that Jewish
 *   historical persecution was real or that statehood was achieved; it argues
 *   that the continuation of ethnic-national institutionalization beyond the
 *   security crisis that justified it now constitutes structural extraction
 *   from non-Jewish populations. This is ONE reading of the contested kernel;
 *   sibling readings (liberal nationalist, religious Zionist, settler
 *   colonial, cultural Zionist) offer different interpretations of the same
 *   foundational state project.
 *
 * KEY AGENTS:
 *   - jewish_israeli_citizens: institutional agenda-setter, beneficiary via Law of Return and state resource preference
 *   - palestinian_israeli_citizens: powerless payer, subject to legal and administrative discrimination
 *   - west_bank_palestinian_population: powerless payer under military occupation, denied return and self-determination
 *   - gaza_palestinian_population: powerless payer under blockade and periodic military enforcement
 *   - israeli_state_security_apparatus: institutional agenda-setter, administers enforcement of ethnic-privileging framework
 *   - zionist_settler_movement: organized beneficiary and agenda-setter, identity-locked to territorial expansion
 *   - palestinian_diaspora_communities: excluded from state institutions despite genealogical claims and lived displacement
 *   - international_community: observer, inconsistently positioned on constraint legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.68).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.72).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Zionist State Framework and Ethnic-National Privilege Structure").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '2dd49888-c5d3-47c4-b739-9dbc649fa1cf').
narrative_ontology:cs_kernel_codification('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', fixed_text).
narrative_ontology:cs_authority_grounding('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', lineage).
narrative_ontology:cs_interpretation_layer_present('2dd49888-c5d3-47c4-b739-9dbc649fa1cf').
narrative_ontology:cs_reading_relation('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_axiom('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', foundational, founding_problem_resolution_through_statehood).
narrative_ontology:cs_axiom_status(founding_problem_resolution_through_statehood, holdable).
narrative_ontology:cs_axiom_grounding('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', founding_problem_resolution_through_statehood, empirically_contingent).
narrative_ontology:cs_axiom('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', foundational, civic_equality_compatibility_with_refuge).
narrative_ontology:cs_axiom_status(civic_equality_compatibility_with_refuge, holdable).
narrative_ontology:cs_axiom_grounding('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', civic_equality_compatibility_with_refuge, instrumental).
narrative_ontology:cs_reference_frame('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', ethnic_national_jewish_statehood).
narrative_ontology:cs_drift_state('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', contemporary_post_occupation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2dd49888-c5d3-47c4-b739-9dbc649fa1cf', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_israeli_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinian_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinian_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, zionist_settler_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitute the demographic and political majority of the Israeli state apparatus. Hold institutional control via elected government, security services, and judiciary. Benefit from Law of Return (automatic citizenship privilege), preferential land access through state-controlled land authority, and settlement expansion policy. The founding narrative centers Jewish statehood as the solution to historical persecution, which frames the state's ethnic-national character as justified and necessary for survival. Can exit through emigration but retain citizenship rights and property claims; most remain invested in the state's ethnic-national continuity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens, agenda_setter,
    institutional, generational, mobile, national).

% Palestinian citizens holding Israeli nationality (approximately 20% of population). Experience structural legal and administrative discrimination: land access restricted, military law enforcement asymmetries, underrepresentation in state employment and resource allocation. The Law of Return privileges Jewish immigration while denying Palestinian refugees right of return. State education system centers Jewish historical narrative; Palestinian history is marginalized. Exit via emigration is effectively permanent—returning is difficult. Cannot meaningfully alter state institutions from within as a minority without coalition with Jewish citizens.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_israeli_citizens, payer,
    powerless, generational, trapped, national).

% Under military administration via Israeli occupation. Subject to military law, settlement expansion, land confiscation through state and settler actor networks, water access restrictions, and movement control via checkpoints. No voting rights in Israeli state despite territorial governance. Resistance is met with military response. Exit options are severely constrained—displacement and refusal-of-return policy (right of return denied) mean departure is permanent diaspora status. Subjected to both state military enforcement and institutional settler organization.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinian_population, payer,
    powerless, immediate, trapped, regional).

% Under blockade enforced by Israeli military (and Egyptian border control). Subject to periodic military campaigns, civilian casualty accumulation, infrastructure destruction, severe resource restrictions. No political participation in Israeli state decision-making despite being subject to Israeli military authority. Exit is structurally prevented—sea/land/air borders controlled. Palestinian governance through Hamas exists but under resource scarcity and military pressure. Subjected to ongoing enforcement of the blockade mechanism itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinian_population, payer,
    powerless, immediate, trapped, regional).

% Benefit from the existence of a Jewish state as refuge, sanctuary claim, and collective national expression. Provide political, diplomatic, and financial support to Israeli state. Can mobilize through international networks and advocacy. Maintain citizenship or migration options in home countries; relationship to Israel is volitional rather than trapped. Some diaspora communities critique the ethnic-national framework; others defend it as necessary insurance against persecution.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Military and intelligence services administer the enforcement of territorial control, settlement expansion, occupation administration, and domestic security. Operate under the mandate that Jewish state existence requires demographic and territorial security. Frame security imperatives as justifying ethnic-national privilege structure and occupation enforcement. Can reform institutional practice but would require political decision to deprioritize ethnic-national security framing.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_state_security_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% UN bodies, governments, and international law frameworks observe and occasionally intervene. Issue condemnations, provide humanitarian aid, engage in peace negotiations. Possess enforcement capacity (sanctions, isolation, aid restriction) but lack unified political will to deploy it. Readings of the constraint differ: some frame it as legitimate national self-determination, others as settler colonialism or apartheid. Power asymmetries mean intervention capacity is constrained and inconsistent.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_community, observer,
    institutional, generational, analytical, global).

% Palestinian refugees and their descendants globally denied right of return to historic territories or to Israeli state. Excluded from any voice in Israeli state institutions or occupation governance. Constitute millions of people with genealogical claims and lived displacement from territories now under Israeli control. Would powerfully contest the constraint's framing and persist if granted participation; their exclusion is structural to the arrangement's maintenance.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_diaspora_communities, excluded,
    organized, generational, trapped, global).

% Organized political and social movement advocating territorial expansion, settlement deepening, and ethnic-national privileging. Institutionally embedded in state policy via political parties in coalition government. Frames settlement expansion as fulfilling historical claim and ensuring Jewish security. Benefits directly from land allocation, state subsidy, and military protection. Exit would require ideological abandonment of core identity; most are identity-locked to the project.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, zionist_settler_movement, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, zionist_settler_movement, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Zionist state apparatus solves the historical coordination problem of Jewish dispersion and persecution vulnerability by creating a centralized territorial state with Jewish demographic majority, unified governance, and collective security framework. This coordination addresses diaspora fragmentation and historical statelessness.
% TRANSFER_FUNCTION: Transfers preferential political voice, legal status, land access, and state resource allocation to Jewish citizens; extracts those same resources and political voice from Palestinian populations (both citizen and non-citizen). The transfer operates via Law of Return (automatic citizenship for Jews, denied to Palestinian refugees), state land authority control (Jewish preference in allocation), military law enforcement asymmetries, and occupation administration that concentrates decision-making power in Jewish-majority institutions.
% ABSENT_VOICES: Palestinian refugee diaspora (approximately 5-6 million people globally) are structurally excluded from any participation in the Israeli state institutions that govern territories affecting them or in which they possess genealogical claims. They would contest both the framing of the founding problem (Palestinian displacement as incidental rather than structural outcome) and the legitimacy of the current arrangement. Zionist critics within Jewish communities who advocate de-Zionization or binational framings are marginalized in state discourse, though present in civil society.
% DISAPPEARANCE_RATIONALE: If the ethnic-national Zionist framework and its institutional instantiation disappeared overnight, the entire legal, land-access, and demographic governance structure would require reorganization. Israeli state apparatus would shift from ethnic-privileging to civic-equality basis; Law of Return would convert to standard immigration law; military occupation would lose its ethnic-legitimation framework; Palestinian refugees would press return claims; regional integration possibilities would emerge. The arrangement persists because it is institutionally defended, not because it is inevitable or preferred by all parties.
% FOUNDING_PROBLEM: Historical Jewish persecution, diaspora fragmentation, and vulnerability as stateless minority created a collective security crisis for Jewish communities. The founding problem was: how to end Jewish vulnerability to persecution and displacement by establishing a sovereign Jewish state with Jewish demographic majority in a historically Jewish-identified territory.
% FOUNDING_PROBLEM_CORROBORATION: Jewish Zionist institutional actors and many diaspora communities attest the founding problem remains live, citing continuing antisemitism and need for refuge. Post-Zionist scholars, Palestinian populations, and international human-rights bodies attest the founding problem was substantially solved (Jewish state exists, regional integration is now the real problem), and the continuation of ethnic-national privilege is now the persistence of a solved-problem infrastructure doing new extraction work. Israeli historians and sociologists (e.g., Illan Pappé, Avi Shlaim) and international law scholars argue the founding problem resolution was achieved through a mechanism (ethnic displacement) that created new victims whose problem-status was never addressed—making the founding problem partially masked rather than fully resolved.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint concentrates political voice, legal status, land access, and security resources in Jewish-majority institutions while systematically denying those same resources to Palestinian populations. Suppression is high (0.72) because the arrangement's persistence requires active military occupation, legal discrimination, settlement enforcement, and historical narrative dominance—Palestinian resistance to these mechanisms is met with enforcement escalation. Theater is moderate-high (0.41) because the state increasingly justifies the ethnic-national framework through appeals to security necessity and historical justice, while the actual function has shifted from addressing Jewish vulnerability (largely solved by statehood itself) to maintaining Jewish demographic and territorial advantage. Accessibility collapse is moderate (0.62) because Palestinians face severe barriers to exit (trapped in territories, denied return, facing legal discrimination), while Jewish Israeli citizens retain higher mobility and option-value. Resistance is moderate (0.58) because Palestinian populations mount continuous resistance (BDS movements, civil rights litigation, political organizing, armed resistance in occupied territories), but face institutional and military asymmetries that limit effectiveness. The measurement trajectory shows extractiveness stabilizing (1948-2015 rise, then plateau), theater increasing sharply post-1967 (occupation requires growing narrative work), and suppression requirement increasing alongside territorial expansion. These metrics reflect this reading's assessment that the constraint transitioned from coordination (1948-1967, addressing genuine Jewish security crisis) to tangled-rope (1967-2000, occupation becomes extractive alongside remaining security function) to increasingly theater-dominant extraction (2000-present, security necessity becomes narrative cover while ethnic-national privilege institutionalizes).
 *
 * PERSPECTIVAL GAP:
 *   The Jewish Israeli and settler-movement seats compute the constraint as rope or even mountain (necessary security arrangement, natural outcome of historical necessity). The Palestinian and diaspora-Palestinian seats compute it as snare (coordinated-on-paper security becomes cover for ethnic expropriation). The international observer seat perceives the cognitive divergence itself—some see legitimate nation-state, others see settler colonialism or apartheid. The engine's per-seat computation should produce this divergence automatically from the structural data: beneficiary seats receive subsidy-level d values (low extractiveness per seat), target seats receive high-extraction values. The perspectival gap is NOT a measurement error—it is the core structural fact this constraint exhibits: two parties experiencing the same institutional arrangement as having opposite functions.
 *
 * DIRECTIONALITY LOGIC:
 *   From the Jewish Israeli citizen seat: d~0.15 (beneficiary). They perceive the constraint as security guarantee and collective self-determination achievement; it subsidizes them through preferential legal status and state resource allocation. From the Palestinian Israeli citizen seat: d~0.88 (target). They bear legal discrimination and resource exclusion within a state they cannot meaningfully alter; exit is effectively impossible and identity-bound to the territory. From the West Bank Palestinian seat: d~0.92 (full target). Military occupation, land confiscation, and return-denial create maximal extractiveness; no mobility, no political voice, no exit option that retains claim. From the Gaza Palestinian seat: d~0.95 (full target). Blockade plus military authority plus return-denial represent near-maximal structural targeting. From the Israeli security apparatus seat: d~0.20 (beneficiary). They collect institutional power, budget, and mandate from the ethnic-national security framing; threat-inflation incentivizes continuation. From the settler movement seat: d~0.05 (major beneficiary). Direct land acquisition, state subsidy, and ideological fulfillment; identity-locked investment in expansion. From the diaspora Jewish seat: d~0.25 (moderate beneficiary). Emotional/political investment in Jewish state without bearing direct extraction costs or institutional responsibility. From the Palestinian diaspora seat: d~0.98 (full target, excluded). Permanently denied return and political voice; bearing identity-based exclusion. The directional spread is large because the constraint has fundamentally asymmetric costs and benefits across ethnic-national lines.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-Zionist reading argues for mandatrophy resolution: the founding problem (Jewish vulnerability to stateless persecution) was substantially solved by statehood achievement itself (1948). The continuation of ethnic-national privileging past the security crisis that justified it now constitutes mandatrophy—a solved problem whose solution infrastructure persists and does new extraction work. The mandate was 'create a Jewish state as refuge and security for Jewish people'; the state was created and achieved that function. The current mandate—maintaining ethnic-national majority and Jewish-privileged institutions—is a new mandate that lacks the same foundational urgency and instead functions as institutional extraction. The constraint should be reclassified as tangled_rope (coordination function largely archived; extraction function now dominant) or snare (if pure ethnic privilege is the actual current function). The theater ratio rise (0.12 → 0.41) over the interval tracks this mandatrophy: security narratives increasingly perform a justification function rather than describe current operational necessity, as occupation deepens and settlement expansion becomes the actual enforced policy. Decoupling the ethnic-national institutional structure from state security would require reframing the state from ethnic-privilege protection to civic-equality governance—a transformation that the current agenda-setter seats (Jewish citizens, security apparatus, settler movement) perceive as threatening rather than necessary. This is why mandatrophy persists: the beneficiary seats have no incentive to acknowledge the mandate's obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_resolution_status,
    'Has the founding problem (Jewish vulnerability to persecution and statelessness) been substantially resolved by the achievement of Jewish statehood, or does it remain live enough to justify continued ethnic-national privileging?',
    'Empirical assessment: is Jewish safety/security/refugee-access materially better with ethnic-national state institutions than it would be with civic-equality state institutions offering the same territorial refuge? Comparative historical analysis of refugee acceptance, persecution rates, and security outcomes under different institutional models.',
    'If the founding problem is resolved, the constraint reclassifies from security-justified coordination to pure institutional extraction (snare or piton), and the mandatrophy_resolved flag activates. If the founding problem remains live, the constraint retains legitimacy as tangled_rope (real coordination function + real extraction). This omega resolves whether post-Zionist reading''s mandatrophy diagnosis is accurate or whether the reading misunderstands continuing necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_resolution_status, empirical, 'Whether the founding security problem persists or has been solved by statehood itself.').

omega_variable(
    ethnic_privilege_necessity_decoupling,
    'Could the same Jewish statehood security function be achieved through civic-equality institutions (constitutional protection of rights regardless of ethnicity, immigration law equally applied) rather than through ethnic-national privilege structures (Law of Return, state-privileged land allocation)?',
    'Normative political theory analysis + institutional design thought experiments: what would a non-ethnic-national Jewish-majority state look like, and would it lose security/refuge functions? Comparative examination of civic-nationalist models (e.g., Rwanda post-reconciliation, South Africa post-apartheid transition attempts) to assess whether refuge and security can be institutionalized without ethnic privilege.',
    'If the functions are decoupled, ethnic-national privilege is revealed as institutional choice rather than necessity, increasing extractiveness classification and supporting post-Zionist critique. If decoupling fails (security collapses without ethnic institutions), the constraint retains its tangled-rope classification and the privilege structures gain legitimacy as coordination costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ethnic_privilege_necessity_decoupling, conceptual, 'Whether ethnic-national institutionalization is functionally necessary or institutionally contingent.').

omega_variable(
    suppression_mechanism_structural_vs_ideological,
    'Is the measured suppression (0.72) primarily structural (military occupation, legal barriers, resource scarcity) or primarily ideological (internalized historical narratives, identity-fusion of Jewish security with ethnic-national state)?',
    'Post-exit trajectory analysis: if suppression of Palestinian political voice declined substantially after hypothetical removal of military occupation / legal discrimination (natural experiment via jurisdiction-level reforms or counterfactual modeling), the suppression is primarily structural. If Palestinian internalized acceptance of subordination persists even after structural barriers are removed (as in post-apartheid South Africa), suppression is partially internalized.',
    'If primarily structural, the constraint''s effective suppression could be significantly reduced through institutional reform without addressing ideology. If primarily ideological, removal requires cultural/educational transformation and longer timeline. This affects both classification (more internalized = more persistent snare characteristics) and remediation strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_ideological, empirical, 'Whether suppression operates through external barriers or internalized beliefs, and what its post-removal trajectory would be.').

omega_variable(
    reading_contest_naturalization,
    'Which of the five sibling readings (liberal_nationalist, settler_colonial, religious_zionist, cultural_zionist, post_zionist) is most likely to be naturalized—treated as inevitable, obvious, or non-contested—by international legal and political institutions?',
    'Institutional discourse analysis: survey UN bodies, ICJ rulings, academic/policy consensus formation over the interval 2000-2026. Track which reading enters institutional baseline assumptions vs. which remains contested.',
    'The naturalized reading sets the constraint''s classification baseline for external actors (international_community seat). If settler_colonial reading naturalizes, the constraint classifies as snare across all seats. If liberal_nationalist naturalizes, it classifies as rope for beneficiary seats. Naturalization of any single reading obscures the kernel contest and risks false-summit detection (treating contested political claim as natural law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_naturalization, empirical, 'Which reading becomes institutional default assumption, potentially masking the contest itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(jewi_tr_t1982, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1982, 0.28).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(jewi_tr_t2015, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(jewi_tr_t2026, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.58).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(jewi_be_t1982, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1982, 0.65).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(jewi_be_t2015, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(jewi_be_t2026, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.52).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.61).
narrative_ontology:measurement(jewi_su_t1982, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1982, 0.66).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(jewi_su_t2015, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(jewi_su_t2026, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__post_zionist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, israeli_military_occupation_west_bank).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, gaza_blockade_regime).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_citizenship_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_sovereignty_palestine kernel. The post_zionist_reading interprets the state project as having achieved its founding coordination function but now operating as institutional extraction from non-Jewish populations. Sibling readings (liberal_nationalist, settler_colonial, religious_zionist, cultural_zionist) offer competing interpretations of the same state apparatus and foundational claim. These are NOT alternative measurements of one constraint but rather distinct constraints generated from different readings of the kernel. The shared constraint_id prefix (jewish_sovereignty_palestine) marks them as a kernel family; each reading generates its own extracted epsilon, its own beneficiary/victim structure, and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__post_zionist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
