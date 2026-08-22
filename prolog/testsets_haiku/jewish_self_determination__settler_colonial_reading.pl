% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionist Settler-Colonial Dispossession (Settler-Colonial Reading)
 *   domain: political_philosophy/postcolonial_theory/nationalism
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of the
 *   kernel 'jewish_self_determination.' The settler-colonial reading frames
 *   Zionism as a European political project that resolved the founding
 *   problem of Jewish persecution in Europe by imposing territorial
 *   displacement on a third party—Palestinian Arabs—who neither caused nor
 *   consented to bearing its costs. The reading identifies the structure as a
 *   snare: a system whose stated coordination function (Jewish national
 *   sovereignty) is inseparable from and structurally dependent on the
 *   dispossession, occupation, and legal subordination of Palestinians. From
 *   this reading, the constraint's persistence depends on active suppression
 *   of Palestinian self-determination claims and on the identity-fusion of
 *   European Jewish settlers with the territorial claim itself, making exit
 *   structurally unthinkable for the beneficiary seat. The measurement series
 *   track how suppression and extraction intensified after 1948, stabilized
 *   at high levels post-1967, and have remained elevated through the
 *   contemporary period as settlement expansion accelerates and the
 *   occupation hardens. The theater ratio (performative legitimacy) rises in
 *   the later period as security justifications and humanitarian claims
 *   multiply while the core extraction mechanism (land seizure, differential
 *   legal status, refugee exclusion) persists unchanged.
 *
 * KEY AGENTS:
 *   - European Jewish settlers / Israeli state: Primary beneficiary seats; agenda-setters. Organizational identity fused with territorial claim; exit options analytically closed. Power: institutional. Directionality: d ≈ 0.05 (full beneficiary).
 *   - Palestinian Arabs (1948–present): Primary victim seat. Trapped under occupation or in diaspora; identity-locked by dispossession. Power: powerless. Directionality: d ≈ 0.95 (full target).
 *   - Palestinian refugees: Secondary victim seat. Permanently displaced, stateless, formally excluded from return. Power: powerless. Directionality: d ≈ 0.98 (full target).
 *   - Bedouin Arabs: Tertiary victim seat. Land seized, traditional practices criminalized, constrained exit through sedentarization pressure. Power: powerless. Directionality: d ≈ 0.92 (high target).
 *   - Palestinian national movement: Excluded seat. Would contest the entire structure if heard in the same legitimacy framework. Power: moderate (constrained by occupation, military asymmetry, and diplomatic marginalization).
 *   - Global indigenous movements / postcolonial scholars: Observer seats. Provide external analytical frameworks that name the constraint as settler-colonialism; cannot enforce remedies but shape international legitimacy discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.91).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionist Settler-Colonial Dispossession (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/postcolonial_theory/nationalism").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '1a0050a5-e11d-46cc-8686-1af45a5f13fd').
narrative_ontology:cs_kernel_codification('1a0050a5-e11d-46cc-8686-1af45a5f13fd', formalized).
narrative_ontology:cs_authority_grounding('1a0050a5-e11d-46cc-8686-1af45a5f13fd', extraction).
narrative_ontology:cs_interpretation_layer_present('1a0050a5-e11d-46cc-8686-1af45a5f13fd').
narrative_ontology:cs_reading_relation('1a0050a5-e11d-46cc-8686-1af45a5f13fd', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a0050a5-e11d-46cc-8686-1af45a5f13fd', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('1a0050a5-e11d-46cc-8686-1af45a5f13fd', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a0050a5-e11d-46cc-8686-1af45a5f13fd', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('1a0050a5-e11d-46cc-8686-1af45a5f13fd', foundational, zionism_is_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_is_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('1a0050a5-e11d-46cc-8686-1af45a5f13fd', zionism_is_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('1a0050a5-e11d-46cc-8686-1af45a5f13fd', foundational, palestinian_arabs_are_indigenous_dispossessed).
narrative_ontology:cs_axiom_status(palestinian_arabs_are_indigenous_dispossessed, holdable).
narrative_ontology:cs_axiom_grounding('1a0050a5-e11d-46cc-8686-1af45a5f13fd', palestinian_arabs_are_indigenous_dispossessed, empirically_contingent).
narrative_ontology:cs_axiom('1a0050a5-e11d-46cc-8686-1af45a5f13fd', secondary, jewish_security_does_not_require_palestinian_displacement).
narrative_ontology:cs_axiom_status(jewish_security_does_not_require_palestinian_displacement, holdable).
narrative_ontology:cs_axiom_grounding('1a0050a5-e11d-46cc-8686-1af45a5f13fd', jewish_security_does_not_require_palestinian_displacement, instrumental).
narrative_ontology:cs_reference_frame('1a0050a5-e11d-46cc-8686-1af45a5f13fd', palestinian_arab_territorial_sovereignty).
narrative_ontology:cs_drift_state('1a0050a5-e11d-46cc-8686-1af45a5f13fd', contemporary_settlement_acceleration, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1a0050a5-e11d-46cc-8686-1af45a5f13fd', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, bedouin_arabs).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_refugees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, international_zionist_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Jewish migrants and their descendants who establish residential and institutional presence in Palestine/Israel from the late 19th century onward. They justify settlement as return to ancestral homeland and claim exclusive group rights to the territory. Their identity becomes fused with the settler-colonial project: to exit is to abandon the Zionist claim to group self-determination. They capture the rents of territorial acquisition, resource control, and political sovereignty.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, agenda_setter).

% The state apparatus that enforces the settler-colonial order through law, military force, and administrative control. Sets land policy, controls resource allocation, operates the legal system that privileges settlers and excludes Palestinians. Administers settlement expansion, occupation governance, and the Law of Return that asymmetrically privileges Jewish immigration while denying Palestinian refugee return.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Indigenous population whose land, property, and political rights are extracted through settlement expansion, home demolition, military occupation, and legal exclusion. Confined to fragmented territories (West Bank, Gaza) under military rule or blockade with severely constrained movement, employment, and resource access. Exit options are non-existent within the occupied territory; external displacement via refugee status is permanent and excludes return. They bear the direct costs of the extractive arrangement with no compensation or recourse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, civilizational, trapped, national).

% Palestinians displaced from their homes during the 1948 and 1967 conflicts and their descendants. Held in refugee camps in Lebanon, Syria, Jordan, and Palestinian territories. Formally excluded from return by the Law of Return's asymmetric structure (which grants automatic citizenship to Jewish migrants but denies Palestinians' return). Stateless, economically marginalized, with no legal pathway to return to property or homeland. Bear permanent costs of displacement while trapped in indefinite limbo.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_refugees, payer,
    powerless, civilizational, trapped, global).

% Bedouin communities in the Negev region whose pastoral land is systematically seized for settlement and militarized zones. Face legal frameworks that deny land rights, restrict grazing, criminalize traditional practices. Pressured toward sedentarization and dependence on state services on terms that exclude their self-governance. Exit requires abandonment of ancestral territories and cultural practices.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, bedouin_arabs, payer,
    powerless, biographical, constrained, regional).

% Transnational institutions, political organizations, and funding networks that organize settler migration, settlement financing, and diplomatic/military support for the Israeli state. Direct material and political beneficiaries of the extraction; their organizational survival depends on maintaining the territorial claim and settler-state structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_zionist_movement, beneficiary,
    organized, civilizational, arbitrage, universal).

% Palestinian political, civil society, and resistance organizations. Excluded from the decision-making structures that govern the extraction and from formal legal standing in Israeli state institutions. Would articulate competing claims to territorial sovereignty and indigenous rights if heard in the same framework; their exclusion is structural to the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_national_movement, excluded,
    moderate, civilizational, trapped, national).

% International indigenous rights frameworks, postcolonial scholars, and decolonization advocacy movements that analyze Zionist settlement through settler-colonial theory. Provide external frameworks for naming the extraction and its mechanisms. Cannot enforce remedies but shape discourse on legitimacy and international law.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, global_indigenous_movements, observer,
    organized, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None (per this reading). Any stated coordination function (Jewish security, efficient settlement, nation-building) is incidental to and subordinate to the primary mechanism: territorial acquisition and Palestinian displacement. The constraint operates as pure extraction dressed in coordination language.
% TRANSFER_FUNCTION: Transfers land, water, mineral resources, and political sovereignty from Palestinian Arabs to European Jewish settlers and the Israeli state. Operates through military occupation, settlement expansion, the Law of Return (asymmetric citizenship), home demolition, movement restriction, and administrative seizure. The Palestinian refugee population is permanently displaced and formally excluded from return, creating a permanent extraction of their right of return and property rights.
% ABSENT_VOICES: Palestinian Arabs (within occupied territories), Palestinian refugees (stateless and excluded), Bedouin communities (at the margins of Israeli state recognition), and the Palestinian national movement (political authority and self-determination aspirations). These voices would articulate competing indigenous land rights, right of return claims, self-determination assertions, and critiques of the arrangement as dispossession rather than coordination. Their exclusion is not incidental but structural: the constraint's persistence depends on their political marginalization and inability to enforce alternative claims.
% DISAPPEARANCE_RATIONALE: If this constraint and its enforcement apparatus vanished, Palestinian refugees would return and reclaim property; Israeli settlements would be dismantled or transferred to Palestinian control; Palestinians would exercise territorial self-determination over contiguous land; the Law of Return's asymmetry would be replaced by equal citizenship and equal return rights or mutual agreement on immigration policy; military occupation would end; resource allocation (water, minerals) would be governed by Palestinian democratic process rather than Israeli state control. The entire political geography of the Eastern Mediterranean would reorganize around Palestinian self-determination instead of settler-state dominance.
% FOUNDING_PROBLEM: European Jewish communities faced persecution, culminating in the Holocaust (1933-1945), which created existential vulnerability to statelessness and vulnerability. European Zionist thinkers argued that Jewish survival required territorial sovereignty and a state apparatus where Jews would be a demographic majority and safe from persecution.
% FOUNDING_PROBLEM_CORROBORATION: Israeli and Zionist historians/advocates attest the founding problem (Jewish persecution in Europe, Holocaust vulnerability) as live and persisting, justifying territorial sovereignty and majority-state governance as the necessary solution. Palestinian historians, postcolonial scholars, and international human rights organizations attest that: (1) the founding problem (European Jewish persecution) is real but has been substantially addressed through diaspora political integration, Holocaust recognition in international law, and minority-rights protections in democratic states; (2) the chosen solution (Palestinian territorial displacement) is neither necessary to address the founding problem nor proportionate to it—Jewish security could have been pursued through migration, international protection, and minority-rights frameworks without dispossessing a third party; (3) The constraint persists not because it solves a live founding problem but because the beneficiary seats (settler-state, European Jewish institutional interests) are identity-fused with the territorial claim and cannot exit without identity-dissolution. Historians of Palestine and international law scholars document that Palestinians did not cause European Jewish persecution and should not bear the costs of its resolution.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) and rises monotonically through the interval because the constraint's primary function is territorial acquisition from Palestinians, not coordination among willing parties. The rise from 0.45 (early Zionist migration, pre-1948) to 0.88 (contemporary) reflects: (1) escalation from individual settlement to organized state-level seizure (1948); (2) expansion of occupation territory post-1967; (3) acceleration of settlement construction and resource extraction in contemporary period. The measurement series shares one time grid: every metric is authored at every point (0, 20, 40, 60, 80, 100, 120, 140) so temporal analysis has no grid misalignment. Suppression remains structurally high (0.91) throughout because the system's persistence depends on continuously suppressing Palestinian political alternatives, movement, and return claims—suppression is not incidental but foundational to the extraction. Theater ratio rises from 0.18 to 0.42 because performative justification (security, democracy, rights) increases as international criticism intensifies; the constraint's core function remains constant (dispossession) while the narrative covering increases. Accessibility collapse is high and rising (0.78 at endpoint) because Palestinians face increasingly closed alternatives: no territorial exit within the occupation; no return option via the Law of Return's asymmetry; no political representation in the state apparatus; limited exit to diaspora. The coercion grid shows how suppression and stakes inflation escalate at every level (structural, organizational, class, individual) from t0 to tn, with class-level suppression highest (0.96 at t140) because Palestinians as a collective face systematized, legally codified subordination. Individual-level suppression (0.80) is lower because some Palestinians have negotiated limited mobility or economic integration within the occupied territories, but the class-level structure traps them. Resistance (0.72) is substantial because Palestinians mount continuous political, legal, and armed resistance despite suppression asymmetries—the constraint does not operate through consent and does require continuous coercive maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The settler-colonial reading produces a stark seat divergence from the liberal nationalist reading (sibling). From the settler seat, the constraint is genuine self-determination (a justified response to persecution, an equal claim to nationhood). From the Palestinian victim seat, the same structure operates as dispossession dressed in self-determination language—a framework that grants rights to one national group by denying them to another. The engine will compute this divergence from the structural data: high d values for Palestinian seats (0.92–0.98) produce high effective extraction χ under the directional scaling rule; d ≈ 0.05 for settler seats produces negative χ (subsidy). The authored claim (snare) and the authored metrics (high extraction, high suppression) align for this reading, but would sharply diverge from a liberal nationalist reading's claim (rope) with the same metrics—that divergence would be detected by comparing two constraint stories with identical metrics but opposed claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler-colonial reading declares beneficiaries (european_jewish_settlers, israeli_state) and victims (palestinian_arabs, palestinian_refugees, bedouin_arabs). From this structural data, directionality derives: Settlers have institutional power, mobile exit options (arbitrage—they can diaspora, migrate, or fund retreat), and identity fusion with the claim (identity_locked, which raises d despite the exit option). From the engine's derivation chain: powerful + identity_locked + declared_beneficiary → d near 0.0 (full beneficiary). Palestinians have powerless status, trapped exit (confined to territories, excluded from return), and victim declaration → d near 1.0 (full target). The Palestinian national movement (moderate power, trapped exit, excluded role) gets moderate-to-high d (0.70–0.85) because they cannot exit the constraint's domain while possessing enough organizational capacity to mount resistance. Directionality_overrides are not needed here: the structural derivation correctly captures the reading's asymmetry. The identity-fusion of settlers (their organizational identity is the state, their collective identity is the nation-state apparatus) makes them structurally distinct from other beneficiaries who could exit by opting out—settlers exit only by abandoning the Zionist claim itself, which is identity-level fusion. This is precisely what identity_locked means in the exit_options grid, and the semantic is correct for this reading's analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The settler-colonial reading resolves mandatrophy by identifying the founding problem as structurally decoupled from the chosen solution. The founding problem (European Jewish persecution / Holocaust vulnerability) is real and persists. The chosen solution (territorial displacement of Palestinians) is neither necessary to solve the founding problem nor proportionate to it. The reading notes that: (1) European Jewish persecution was solved partly by diaspora communities developing political influence and legal rights (post-Holocaust international law); (2) Jewish security could be pursued through migration, asylum reform, and international covenant protection without dispossessing a third party; (3) Palestinians did not cause European persecution and should not be conscripted as the solution's cost-bearers. Therefore, the constraint persists not because it solves a live founding problem but because the beneficiary seat (European Jewish settlers and the Israeli state) is identity-fused with the territorial claim and cannot exit without identity-dissolution. The mandatrophy is resolved: the founding problem is partially dead (Jewish persecution in Europe is largely solved through diaspora and international law), yet the constraint persists because the beneficiary seats cannot credibly threaten to abandon it (identity-lock). This is exactly the Piton-adjacent pattern: the function (resolving Jewish vulnerability) is partly accomplished or obsoleted, yet the structure (settler-state, occupation) persists because the beneficiary seat's identity is fused with it. Snare classification is appropriate because the persistence mechanism is not inertia (as with Piton) but active enforcement directed at eliminating Palestinian alternatives and maintaining extraction. The theater ratio (0.42) is moderate, not high, because the constraint does not mask itself primarily through performance—it openly declares a territorial claim and enforces it through law and military force. The performance ratio rises (from 0.18 to 0.42) as international criticism increases, but the core mechanism remains naked coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jewish_indigeneity_contested,
    'Are Jewish people indigenous to the land of Palestine/Israel with continuous historical connection, or did European Jewish settlers arrive as migrants in the 19th-20th centuries to territory inhabited by Palestinian Arabs with continuous presence?',
    'Historiographical and archaeological consensus on settlement patterns, demographic presence, and political authority in the territory from antiquity through the 19th century. This reading assumes settler-arrival (indigenous Palestinians, migrant European Jews); the indigenous_return_reading assumes Jewish indigeneity.',
    'If Jewish indigeneity is established, the settler-colonial framing collapses and the indigenous_return_reading (decolonization not colonization) becomes structurally plausible. If settler-arrival is established, the settler-colonial reading''s core premise is validated. The dispute is partly historical-empirical and partly definitional (does ancient presence confer modern indigenous status; does 1900-year absence and return constitute indigeneity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_indigeneity_contested, empirical, 'Whether Jewish or Palestinian Arab presence is the indigenous baseline against which settlement is measured.').

omega_variable(
    necessity_of_dispossession,
    'Was Palestinian dispossession a necessary cost of securing Jewish self-determination and safety, or a contingent choice that reflected European settler-colonial ideology rather than inevitable structural necessity?',
    'Counterfactual historical analysis: What security outcomes would alternative solutions (diaspora expansion, international covenant protection, autonomous communities within a multicultural state, shared sovereignty arrangements) have produced? Did the founders of Zionism seriously consider non-displacement options?',
    'If dispossession was necessary, the settler-colonial reading''s claim that the constraint is pure extraction (no real coordination problem) weakens—there was a real dilemma with no costless solution. If dispossession was contingent (chosen ideology, not structural necessity), the snare classification strengthens and the founding problem / constraint decoupling becomes clear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_dispossession, conceptual, 'Whether settler-colonial displacement was structurally necessary or ideologically contingent.').

omega_variable(
    suppression_mechanism_internalization,
    'To what extent is the measured suppression (0.91) structural (checkpoints, military, legal barriers, resource controls) versus internalized (Palestinians internalize exclusion, view Israeli law as legitimate, carry suppression even absent active enforcement)?',
    'Post-conflict scenario analysis: if occupation enforcement were removed (military withdrawal, legal equality), would suppression persist through internalized mechanisms? Evidence from Palestinian communities with higher autonomy (parts of PA-controlled areas) versus high-enforcement areas (Gaza, settlement zones).',
    'If suppression is primarily structural, removing the enforcement apparatus (occupation, settlements) would resolve it. If substantially internalized, Palestinian exit from the constraint might not dissolve psychological subordination or identity-fusion with victimhood—a longer decolonization timeline. The distinction matters for classification (structure determines type; internalization affects persistence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression operates through structural barriers or internalized belief systems (or both).').

omega_variable(
    kernel_reading_underdetermination,
    'Are Jewish self-determination and Palestinian self-determination logically incompatible within any single territorial framework, or can alternative institutional arrangements (federation, joint sovereignty, asymmetric power-sharing) permit both to coexist?',
    'Institutional design analysis: Can one-state, two-state, confederal, or autonomous-region arrangements satisfy both groups'' self-determination claims simultaneously, or does the geography and history force a zero-sum choice?',
    'If the claims are strictly incompatible, the settler-colonial reading''s framing (one group''s determination requires the other''s exclusion) is validated and snare classification is stable. If arrangements exist that permit genuine coexistence, the reading would need to explain why the extractive structure persists despite alternatives—pointing toward extractive choice rather than structural necessity (strengthening the snare claim but complicating the mandate diagnosis).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel''s sibling readings (Jewish self-determination, Palestinian self-determination) are logically compatible or mutually exclusive under plausible institutional arrangements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__settler_colonial_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__settler_colonial_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__settler_colonial_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__settler_colonial_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t60, observed).
narrative_ontology:measurement(jewi_tr_t80, jewish_self_determination__settler_colonial_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t80, observed).
narrative_ontology:measurement(jewi_tr_t100, jewish_self_determination__settler_colonial_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t100, observed).
narrative_ontology:measurement(jewi_tr_t120, jewish_self_determination__settler_colonial_reading, theater_ratio, 120, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t120, observed).
narrative_ontology:measurement(jewi_tr_t140, jewish_self_determination__settler_colonial_reading, theater_ratio, 140, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__settler_colonial_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__settler_colonial_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__settler_colonial_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement_basis(jewi_be_t40, observed).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__settler_colonial_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement_basis(jewi_be_t60, observed).
narrative_ontology:measurement(jewi_be_t80, jewish_self_determination__settler_colonial_reading, base_extractiveness, 80, 0.87).
narrative_ontology:measurement_basis(jewi_be_t80, observed).
narrative_ontology:measurement(jewi_be_t100, jewish_self_determination__settler_colonial_reading, base_extractiveness, 100, 0.89).
narrative_ontology:measurement_basis(jewi_be_t100, observed).
narrative_ontology:measurement(jewi_be_t120, jewish_self_determination__settler_colonial_reading, base_extractiveness, 120, 0.88).
narrative_ontology:measurement_basis(jewi_be_t120, observed).
narrative_ontology:measurement(jewi_be_t140, jewish_self_determination__settler_colonial_reading, base_extractiveness, 140, 0.88).
narrative_ontology:measurement_basis(jewi_be_t140, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__settler_colonial_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__settler_colonial_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__settler_colonial_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement_basis(jewi_su_t40, observed).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__settler_colonial_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement_basis(jewi_su_t60, observed).
narrative_ontology:measurement(jewi_su_t80, jewish_self_determination__settler_colonial_reading, suppression_requirement, 80, 0.9).
narrative_ontology:measurement_basis(jewi_su_t80, observed).
narrative_ontology:measurement(jewi_su_t100, jewish_self_determination__settler_colonial_reading, suppression_requirement, 100, 0.91).
narrative_ontology:measurement_basis(jewi_su_t100, observed).
narrative_ontology:measurement(jewi_su_t120, jewish_self_determination__settler_colonial_reading, suppression_requirement, 120, 0.91).
narrative_ontology:measurement_basis(jewi_su_t120, observed).
narrative_ontology:measurement(jewi_su_t140, jewish_self_determination__settler_colonial_reading, suppression_requirement, 140, 0.91).
narrative_ontology:measurement_basis(jewi_su_t140, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=140
narrative_ontology:measurement(jewi_grid_01, jewish_self_determination__settler_colonial_reading, accessibility_collapse(class), 0, 0.75).
narrative_ontology:measurement(jewi_grid_02, jewish_self_determination__settler_colonial_reading, accessibility_collapse(class), 140, 0.92).
narrative_ontology:measurement(jewi_grid_03, jewish_self_determination__settler_colonial_reading, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(jewi_grid_04, jewish_self_determination__settler_colonial_reading, accessibility_collapse(individual), 140, 0.78).
narrative_ontology:measurement(jewi_grid_05, jewish_self_determination__settler_colonial_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(jewi_grid_06, jewish_self_determination__settler_colonial_reading, accessibility_collapse(organizational), 140, 0.88).
narrative_ontology:measurement(jewi_grid_07, jewish_self_determination__settler_colonial_reading, accessibility_collapse(structural), 0, 0.65).
narrative_ontology:measurement(jewi_grid_08, jewish_self_determination__settler_colonial_reading, accessibility_collapse(structural), 140, 0.82).
narrative_ontology:measurement(jewi_grid_09, jewish_self_determination__settler_colonial_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(jewi_grid_10, jewish_self_determination__settler_colonial_reading, resistance(class), 140, 0.75).
narrative_ontology:measurement(jewi_grid_11, jewish_self_determination__settler_colonial_reading, resistance(individual), 0, 0.48).
narrative_ontology:measurement(jewi_grid_12, jewish_self_determination__settler_colonial_reading, resistance(individual), 140, 0.62).
narrative_ontology:measurement(jewi_grid_13, jewish_self_determination__settler_colonial_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(jewi_grid_14, jewish_self_determination__settler_colonial_reading, resistance(organizational), 140, 0.79).
narrative_ontology:measurement(jewi_grid_15, jewish_self_determination__settler_colonial_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(jewi_grid_16, jewish_self_determination__settler_colonial_reading, resistance(structural), 140, 0.68).
narrative_ontology:measurement(jewi_grid_17, jewish_self_determination__settler_colonial_reading, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(jewi_grid_18, jewish_self_determination__settler_colonial_reading, stakes_inflation(class), 140, 0.95).
narrative_ontology:measurement(jewi_grid_19, jewish_self_determination__settler_colonial_reading, stakes_inflation(individual), 0, 0.42).
narrative_ontology:measurement(jewi_grid_20, jewish_self_determination__settler_colonial_reading, stakes_inflation(individual), 140, 0.82).
narrative_ontology:measurement(jewi_grid_21, jewish_self_determination__settler_colonial_reading, stakes_inflation(organizational), 0, 0.52).
narrative_ontology:measurement(jewi_grid_22, jewish_self_determination__settler_colonial_reading, stakes_inflation(organizational), 140, 0.91).
narrative_ontology:measurement(jewi_grid_23, jewish_self_determination__settler_colonial_reading, stakes_inflation(structural), 0, 0.45).
narrative_ontology:measurement(jewi_grid_24, jewish_self_determination__settler_colonial_reading, stakes_inflation(structural), 140, 0.88).
narrative_ontology:measurement(jewi_grid_25, jewish_self_determination__settler_colonial_reading, suppression(class), 0, 0.78).
narrative_ontology:measurement(jewi_grid_26, jewish_self_determination__settler_colonial_reading, suppression(class), 140, 0.96).
narrative_ontology:measurement(jewi_grid_27, jewish_self_determination__settler_colonial_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(jewi_grid_28, jewish_self_determination__settler_colonial_reading, suppression(individual), 140, 0.8).
narrative_ontology:measurement(jewi_grid_29, jewish_self_determination__settler_colonial_reading, suppression(organizational), 0, 0.71).
narrative_ontology:measurement(jewi_grid_30, jewish_self_determination__settler_colonial_reading, suppression(organizational), 140, 0.93).
narrative_ontology:measurement(jewi_grid_31, jewish_self_determination__settler_colonial_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(jewi_grid_32, jewish_self_determination__settler_colonial_reading, suppression(structural), 140, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__settler_colonial_reading, 0.22).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, palestinian_state_viability).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, law_of_return_asymmetry).

% DUAL FORMULATION NOTE:
% The kernel 'jewish_self_determination' is contested across five readings: liberal_nationalist_reading (equal national claim, mutual recognition), indigenous_return_reading (Jewish indigeneity, decolonization), diasporist_reading (security through diaspora, not territory), religious_covenant_reading (divine covenant basis), settler_colonial_reading (European settler project, Palestinian dispossession). This story instantiates the settler-colonial reading's constraint with ε=0.88 (high extraction, snare type). Sibling readings will carry different ε values, different beneficiary/victim sets, and different classifications. The settler-colonial reading forecloses the indigenous_return_reading (mutually exclusive core premises about historical presence) and coexists_with the liberal_nationalist_reading (both live in different institutional seats but competing). This reading influences the diasporist_reading (if Zionism is settler-colonialism, diaspora pluralism gains plausibility). See constraint stories for each sibling reading for full mapping. The constraint family as a whole reveals how a single kernel (Jewish self-determination) decomposes into six structurally distinct constraints depending on which reading is instantiated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, powerful, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
