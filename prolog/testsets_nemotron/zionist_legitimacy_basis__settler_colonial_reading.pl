% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Legitimacy Basis — Settler-Colonial Reading
 *   domain: political/historical/nationalism
 *
 * SUMMARY:
 *   This constraint story models the settler_colonial_reading of the
 *   zionist_legitimacy_basis kernel: the claim that Zionism is structurally a
 *   European settler-colonial movement that established an ethno-state
 *   through the constitutive displacement of the indigenous Palestinian
 *   population. The reading asserts that displacement is not incidental but
 *   foundational — the coordination of Jewish immigration and state-building
 *   (the 'rope' function) is inextricably bound to the extraction of
 *   Palestinian land, sovereignty, and demographic presence (the 'snare'
 *   function). The constraint has intensified over time: early Zionist land
 *   purchases (pre-1917) show low extractiveness; the Balfour Declaration and
 *   Mandate period (1917-1948) escalate coordination and suppression; 1948
 *   establishes the state through mass displacement (Nakba); 1967 extends
 *   control over remaining Palestinian territory; Oslo (1993) creates a
 *   theater of 'peace process' while settlement expansion continues; the
 *   current period shows peak extractiveness with formal annexation moves and
 *   apartheid-designations by human rights organizations. The claimed_type is
 *   tangled_rope because the constraint genuinely coordinates Jewish
 *   collective self-determination (beneficiaries) while asymmetrically
 *   extracting from Palestinians (victims) through active enforcement
 *   (military occupation, legal architecture, demographic engineering).
 *
 * KEY AGENTS:
 *   - zionist_movement_leadership: Primary agenda_setter (pre-1948) / institutional beneficiary (post-1948) — designs and executes the colonial project
 *   - israeli_state_institutions: Primary agenda_setter (post-1948) — administers and enforces the constraint through law, military, planning
 *   - jewish_settler_populations: Beneficiary (collective) — receives land, housing, state resources, citizenship privileges
 *   - western_geopolitical_allies: Beneficiary/enabler — UK (1917-48), US (1967-present) provide diplomatic cover, arms, UN vetoes
 *   - palestinian_arab_population: Primary victim — subject to displacement, dispossession, military rule, fragmented autonomy
 *   - palestinian_refugees_and_descendants: Victim — denied return, stateless, dependent on UNRWA, 5.9M registered
 *   - palestinian_citizens_of_israel: Victim — second-class citizenship, land confiscation, discriminatory laws (Nation-State Law)
 *   - displaced_palestinian_communities: Victim — ongoing displacement in West Bank, East Jerusalem, Negev
 *   - palestinian_resistance_movements: Excluded/counter-constraint — armed, diplomatic, civil resistance excluded from legitimacy framework
 *   - international_legal_institutions: Observer — ICJ, ICC, UN bodies document violations but lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.87).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.82).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis — Settler-Colonial Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political/historical/nationalism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '52f0388f-719d-4945-92f4-797bfb0a26d8').
narrative_ontology:cs_kernel_codification('52f0388f-719d-4945-92f4-797bfb0a26d8', distributed).
narrative_ontology:cs_authority_grounding('52f0388f-719d-4945-92f4-797bfb0a26d8', extraction).
narrative_ontology:cs_interpretation_layer_present('52f0388f-719d-4945-92f4-797bfb0a26d8').
narrative_ontology:cs_reading_relation('52f0388f-719d-4945-92f4-797bfb0a26d8', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('52f0388f-719d-4945-92f4-797bfb0a26d8', zionist_legitimacy_basis__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('52f0388f-719d-4945-92f4-797bfb0a26d8', foundational, zionism_is_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('52f0388f-719d-4945-92f4-797bfb0a26d8', zionism_is_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('52f0388f-719d-4945-92f4-797bfb0a26d8', foundational, displacement_is_constitutive_not_incidental).
narrative_ontology:cs_axiom_status(displacement_is_constitutive_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('52f0388f-719d-4945-92f4-797bfb0a26d8', displacement_is_constitutive_not_incidental, empirically_contingent).
narrative_ontology:cs_axiom('52f0388f-719d-4945-92f4-797bfb0a26d8', secondary, jewish_state_requires_palestinian_unfreedom).
narrative_ontology:cs_axiom_status(jewish_state_requires_palestinian_unfreedom, holdable).
narrative_ontology:cs_axiom_grounding('52f0388f-719d-4945-92f4-797bfb0a26d8', jewish_state_requires_palestinian_unfreedom, deontological).
narrative_ontology:cs_reference_frame('52f0388f-719d-4945-92f4-797bfb0a26d8', pre_1882_palestine_ottoman_district).
narrative_ontology:cs_drift_state('52f0388f-719d-4945-92f4-797bfb0a26d8', contemporary_apartheid_recognition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52f0388f-719d-4945-92f4-797bfb0a26d8', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, zionist_movement_leadership).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_populations).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, western_geopolitical_allies).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_arab_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees_and_descendants).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, displaced_palestinian_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pre-1948: designed the colonial project (Herzl, Weizmann, Ben-Gurion), secured Great Power patronage, built proto-state institutions (JNF, Histadrut, Haganah). Post-1948: transitioned into Israeli state leadership. They authored the constraint's logic: Jewish demographic majority requires Palestinian demographic minority. Their exit is arbitrage — they could have pursued other national projects (Uganda, Birobidzhan) but chose Palestine; once chosen, the constraint became their creation and legacy.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, zionist_movement_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, zionist_movement_leadership, beneficiary).

% The government, military (IDF), courts, planning authorities, land administration (ILA/JNF) that administer and enforce the constraint daily. They maintain the legal architecture: Absentee Property Law, Law of Return, Nation-State Law, military orders in West Bank. They coordinate Jewish settlement and suppress Palestinian development. Exit is constrained — the state *is* the constraint; dismantling it would end the institutional arrangement they embody.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Jewish Israeli citizens (inside Green Line) and settlers (beyond Green Line) who receive the constraint's benefits: citizenship rights, land access, housing subsidies, water allocation, military protection, national belonging. They also pay costs: military service, taxation, social tension, moral injury from occupation. Exit is constrained — emigration is possible but means leaving the collective project, family, homeland; the constraint structures their identity and life options.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_populations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_populations, payer).

% UK (1917-1948): Balfour Declaration, Mandate administration — gained regional foothold, Suez access, imperial management. US (1967-present): strategic asset, intelligence sharing, arms market, domestic political coalition (evangelical, pro-Israel lobby). They provide diplomatic cover (UN vetoes), military aid ($3.8B/year), technology transfer. Exit is arbitrage — they could condition aid, support ICC/ICJ, recognize Palestine; the cost is domestic political backlash and strategic recalculation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, western_geopolitical_allies, beneficiary,
    powerful, biographical, arbitrage, global).

% The indigenous population of historic Palestine (pre-1948: ~1.3M; post-1948: fragmented into citizens of Israel, West Bank/Gaza residents, refugees). They bear the constraint's extraction: 78% lost land in 1948; remaining 22% occupied 1967; ongoing settlement, land confiscation, home demolitions, movement restrictions, water apartheid. Exit is trapped — no right of return, no citizenship in host states (Lebanon, Syria), no sovereignty. Identity is fused to the land; leaving is existential loss.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_arab_population, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, palestinian_arab_population, payer).

% 5.9 million UNRWA-registered refugees (2023) and descendants, scattered across Gaza, West Bank, Jordan, Lebanon, Syria, diaspora. Denied return by Israeli Law of Return (for Jews only) and Absentee Property Law. Stateless or second-class in host countries (Lebanon: no work rights, no property; Syria: pre-2011 limited rights; Jordan: citizenship for some). Exit is trapped — return is the only justice but is structurally banned; integration is blocked; resistance is criminalized.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugees_and_descendants, payer,
    powerless, generational, trapped, global).

% 1.6M Palestinian citizens of Israel (21% of population). Formal citizenship but structural subordination: Nation-State Law (2018) defines Israel as nation-state of Jewish people only; 65+ discriminatory laws; land confiscation continues (Negev Bedouin); planning restrictions prevent community expansion; political participation delegitimized. Exit is identity_locked — they are Palestinian *and* Israeli; leaving means abandoning their community, land, identity; staying means accepting subordination. The constraint makes their identity the site of extraction.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel, payer).

% Communities facing active displacement: Sheikh Jarrah, Silwan, Masafer Yatta, Khan al-Ahmar, Naqab/Negev Bedouin. Subject to home demolitions, settler takeovers, military firing zones, planning denial. No legal recourse in Israeli courts (which uphold displacement). Exit is trapped — resistance leads to arrest/injury/death; compliance leads to expulsion. Their situation is the constraint's sharp edge: daily, visible, irreversible extraction.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, displaced_palestinian_communities, payer,
    powerless, immediate, trapped, local).

% PLO (diplomatic), Hamas/Islamic Jihad (armed), BDS (civil), popular committees, prisoner movement. They contest the constraint's legitimacy and enforcement. Excluded from the 'peace process' framework (Oslo) which required recognizing Israel's 'right to exist' without reciprocal Palestinian rights. Their resistance is labeled 'terrorism' or 'incitement' — the constraint's suppression machinery targets them. Exit is constrained — they fight within a structure that denies their political agency; victory would mean constraint dissolution.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_resistance_movements, excluded,
    moderate, biographical, constrained, national).

% ICJ (2024 genocide case, 2004 Wall advisory opinion), ICC (2021 Palestine investigation), UNHRC, UNRWA, treaty bodies. They document violations, issue opinions, define legal frameworks (apartheid, occupation, self-determination). But they lack enforcement power — the constraint's beneficiaries (US, Israel) ignore or defy them. Their role is analytical: they name the constraint's crimes but cannot stop them. Exit is analytical — they observe from outside the constraint's coercive reach.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective self-determination: immigration (aliyah), Hebrew revival, state institutions, defense, economy, national culture — solving the problem of Jewish statelessness and vulnerability by creating a sovereign Jewish state.
% TRANSFER_FUNCTION: Transfers land (93% of historic Palestine), water resources (80% of West Bank aquifers to settlements), sovereignty (no Palestinian state), demographic space (Jewish majority maintained by Palestinian exclusion), and political rights from Palestinians to the Jewish state and its beneficiaries. The transfer is enforced by military law, planning law, citizenship law, and demographic engineering.
% ABSENT_VOICES: Palestinian refugees (5.9M) are structurally excluded from any negotiation — their right of return is a 'red line' Israel will not cross. The 1948 generation who experienced Nakba directly — their testimony was never heard in the forums that legitimated the state. Palestinian citizens of Israel are included as voters but excluded from the definition of the state (Nation-State Law). The global South, which recognizes the colonial structure, is excluded from the Western-dominated 'peace process' framework.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight: the legal architecture of Jewish supremacy (Law of Return, Nation-State Law, Absentee Property Law) would collapse; military occupation would lose its legitimacy; settlers would lose state protection; Palestinian return would become legally possible; the Jewish demographic majority would be challenged; the US would lose its primary regional anchor. The entire political geography of Israel/Palestine would reorganize — either toward a single democratic state, a binational confederation, or renewed conflict. The world *rearranges* because the constraint is the structural spine of the current order.
% FOUNDING_PROBLEM: The founding problem per *this reading* is not Jewish persecution (that is the national_liberation_reading's founding problem) but the European nationalist/colonial impulse to solve the 'Jewish Question' by transplanting a European population onto inhabited land — the problem of how to establish a European-style nation-state in a non-European territory already populated. The 'solution' was displacement.
% FOUNDING_PROBLEM_CORROBORATION: The colonial founding problem (how to establish a Jewish state in Palestine) was 'solved' by 1948 through displacement — but the *displacement itself* became the ongoing founding act. Israeli 'New Historians' (Morris, Pappé, Flapan, Shlaim) — Jewish Israeli scholars using Israeli archives — corroborate that displacement was intentional, systematic, and constitutive. Palestinian historians (Khalidi, Sayigh, Masalha) and international scholars (Said, Robinson, Piterberg) corroborate from outside the beneficiary set. The national_liberation_reading disputes this, claiming the founding problem (Jewish vulnerability) remains live — but that is the *sibling reading's* framing, not this one's.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.87, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.87) reflects the near-total transfer of land (93% of historic Palestine under Israeli control), sovereignty (no Palestinian state), and resources (water, aquifers, East Jerusalem) from Palestinians to the Jewish state. The 1880-2023 trajectory shows accumulation: early purchases were voluntary (low ε); 1948 war and Absentee Property Law effected mass transfer; 1967 occupation enabled settlement enterprise; Oslo institutionalized fragmentation while extraction continued. Suppression (0.82) is high because the constraint requires military occupation, permit regimes, movement restrictions, and legal barriers to Palestinian development — alternatives (one state, binational state, full return) are structurally suppressed. Theater ratio (0.41) captures the 'peace process' as performative: Oslo created Palestinian Authority as subcontractor of occupation while settlements tripled; the two-state solution discourse masks ongoing annexation. Accessibility collapse (0.78) is high: the constraint's logic (Jewish demographic majority + Jewish state = Palestinian displacement) makes alternatives cognitively and politically inaccessible within the framework. Resistance (0.68) is substantial but fragmented: armed resistance triggers disproportionate force; diplomatic resistance achieves recognition without liberation; BDS grows but faces criminalization in beneficiary states.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent seat types: from the zionist_movement_leadership/israeli_state_institutions seat (agenda_setter, institutional power, arbitrage exit), the constraint appears as coordination (rope-like) — they built a state, revived a language, created a refuge. From the palestinian_arab_population seat (victim, powerless/moderate, trapped/identity_locked exit), the same constraint appears as pure extraction (snare-like) — displacement is total, return is banned, resistance is crushed. Jewish_settler_populations (beneficiary, organized, constrained exit) experience genuine coordination benefits but are locked into the extraction structure — leaving means abandoning the collective project. Western_allies (beneficiary/enabler, powerful/institutional, arbitrage exit) extract geopolitical value but could disengage at high political cost. The structural asymmetry is the point: the same arrangement is rope for some, snare for others, tangled_rope as a system.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: zionist_movement_leadership (architects), israeli_state_institutions (administrators), jewish_settler_populations (collective recipients), western_geopolitical_allies (external patrons). All derive structural benefit: state power, land, resources, strategic position. Their exit options range from arbitrage (allies) to constrained (settlers) — directionality d near 0.0-0.3. Victims declared: palestinian_arab_population (primary target), palestinian_refugees_and_descendants (permanent exclusion), palestinian_citizens_of_israel (subordinate inclusion), displaced_palestinian_communities (ongoing targets). All bear asymmetric costs: land loss, statelessness, military rule, demographic threat. Exit options are trapped (refugees), identity_locked (citizens — cannot exit Palestinianness), constrained (West Bank/Gaza). Directionality d near 0.8-1.0. The engine derives d from these declarations + power + exit; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (per national_liberation_reading) was Jewish persecution and statelessness — a genuine problem. The settler_colonial_reading argues this problem was solved by 1948 (state established) or 1967 (territorial control), but the constraint persists and intensifies because its *actual* function is not solving the founding problem but maintaining the colonial structure of privilege. The mandate (Jewish safety) has atrophied into the extraction (Palestinian unfreedom). The arrangement no longer serves its declared coordination function for its declared beneficiaries — Jewish safety now depends on perpetual domination, creating insecurity. This is mandatrophy: the constraint persists because the beneficiaries cannot imagine security without domination, and the victims cannot dismantle the constraint without overwhelming force. The theater_ratio rise (0.10→0.41) tracks the displacement of the founding problem by the maintenance of privilege.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the zionist_legitimacy_basis kernel, specifically the settler_colonial_reading, and how does it structurally differ from the national_liberation_reading and religious_restoration_reading?',
    'Comparative structural analysis of the three readings'' beneficiary/victim structures, coordination/extraction claims, and founding problem framings. Each reading instantiates a distinct constraint with its own ε.',
    'If readings are not structurally distinct constraints, the ε-invariance principle is violated and the decomposition fails. Confirms this reading''s ε = 0.87 refers to the standing arrangement (settler-colonial ethno-state establishment) assessed by this reading''s lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to kernel/reading decomposition: this constraint is the settler_colonial_reading of zionist_legitimacy_basis; sibling readings are national_liberation_reading and religious_restoration_reading.').

omega_variable(
    colonial_structure_vs_liberation_framing,
    'Is the Zionist project''s legitimacy foundationally a settler-colonial displacement structure, or a national liberation return structure — and does the colonial character determine the constraint''s classification regardless of the liberation narrative?',
    'Historical-structural analysis of Zionist institutions'' land acquisition, demographic engineering, and legal frameworks from 1880s onward; comparison with other settler-colonial projects (Algeria, South Africa, Australia); assessment of whether ''return'' narrative operates as ideological cover for displacement mechanics.',
    'If colonial structure is constitutive, the constraint is tangled_rope (coordination of Jewish immigration/state-building + extraction via Palestinian displacement). If liberation structure is primary, classification shifts toward rope/scaffold. The reading''s axioms commit to colonial primacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonial_structure_vs_liberation_framing, conceptual, 'Core structural ambiguity: colonial displacement as constitutive vs. incidental to the legitimacy claim.').

omega_variable(
    displacement_mechanism_coordination_extraction,
    'Does the displacement of Palestinians function as a coordination mechanism (enabling Jewish state formation) AND an extraction mechanism (transferring land, resources, sovereignty), or is displacement an incidental byproduct?',
    'Institutional history of JNF, Haganah/IDF, Absentee Property Law, Planning and Building Law; demographic data on land ownership 1948 vs. present; analysis of whether displacement was planned/necessary for state viability.',
    'If dual function, tangled_rope is structurally correct. If displacement is incidental, the constraint may be rope with high externalities. The reading''s transfer_function asserts dual function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displacement_mechanism_coordination_extraction, empirical, 'Whether displacement is structurally constitutive (coordination + extraction) or contingent.').

omega_variable(
    western_allies_beneficiary_or_enabler,
    'Are Western geopolitical allies (UK 1917-1948, US post-1967) beneficiaries of the constraint (gaining regional foothold) or enablers (providing diplomatic/military cover without direct extraction), and does this distinction affect the beneficiary structure?',
    'Analysis of US-Israel strategic relationship, arms transfers, UN veto patterns, oil geopolitics; whether allies extract value or bear costs; whether the constraint would persist without external enforcement.',
    'If allies are beneficiaries, the beneficiary set extends beyond Zionist/Israeli actors. If enablers, the constraint''s enforcement depends on external power — affecting suppression scoring and network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(western_allies_beneficiary_or_enabler, empirical, 'Structural position of external patrons in the constraint''s beneficiary/victim architecture.').

omega_variable(
    palestinian_resistance_as_counter_constraint,
    'Does Palestinian resistance (armed, diplomatic, civil) constitute a counter-constraint that modifies the original constraint''s extraction/suppression dynamics, or is it external opposition?',
    'Historical analysis of resistance phases (1936-39, 1967-93, 1987-93, 2000-05, 2023-present); impact on Israeli policy, international law, demographic realities; whether resistance forced constraint adaptation or merely slowed it.',
    'If resistance is a counter-constraint, the system is a constraint family with network edges. If external, the constraint''s metrics reflect unilateral imposition. Affects resistance scoring and network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_resistance_as_counter_constraint, conceptual, 'Whether Palestinian agency structures the constraint''s evolution or merely reacts to it.').

omega_variable(
    religious_restoration_reading_influence,
    'How does the religious_restoration_reading (post-1967 settler movement, messianic Zionism) structurally influence this settler_colonial_reading — does it intensify extraction, shift the coordination function, or create a distinct constraint?',
    'Analysis of Gush Emunim, settlement enterprise post-1967, religious-nationalist coalition politics; whether religious restoration provides ideological cover for continued displacement or constitutes a separate legitimacy claim with its own ε.',
    'If influences, network.affects_constraints should link them. If distinct constraint, separate story. The reading_relations declare influences for religious_restoration_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_restoration_reading_influence, conceptual, 'Structural relationship between settler_colonial_reading and religious_restoration_reading within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1880, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_tr_t1880, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_tr_t1917, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.32).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_tr_t1993, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_tr_t2023, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2023, 0.41).

% Extraction over time
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_be_t1880, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_be_t1917, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_be_t1993, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1993, 0.81).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_be_t2023, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2023, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_su_t1880, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1880, 0.2).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_su_t1917, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1917, 0.45).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.78).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_su_t1993, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1993, 0.8).
narrative_ontology:measurement(zionist_legitimacy_basis__settler_colonial_reading_su_t2023, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2023, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__settler_colonial_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, palestinian_national_movement_constraint).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, israeli_occupation_architecture).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, us_israel_special_relationship).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, international_law_erosion_constraint).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% This story is the settler_colonial_reading of the zionist_legitimacy_basis kernel. It decomposes the kernel into three constraint stories: (1) this one — tangled_rope, ε=0.87, colonial displacement as constitutive; (2) national_liberation_reading — likely rope/scaffold, ε lower, Jewish persecution as founding problem solved by statehood; (3) religious_restoration_reading — likely tangled_rope/snare, ε high, divine promise as coordination cover for settlement expansion post-1967. The ε values differ because the referent arrangements differ: (1) the standing settler-colonial ethno-state, (2) the national liberation achievement, (3) the messianic territorial maximalism. They are linked via network.affects_constraints and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
