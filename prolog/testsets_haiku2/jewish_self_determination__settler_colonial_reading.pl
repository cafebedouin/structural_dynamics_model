% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   domain: political_philosophy/nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of Jewish
 *   self-determination — one reading of a contested kernel. The reading
 *   interprets Zionism as a European settler-colonial project that
 *   dispossessed indigenous Palestinians through systematic violence, legal
 *   exclusion, and land expropriation. Under this reading, the constraint is
 *   not voluntary coordination (rope) or even coercive hybrid coordination
 *   (tangled rope), but rather a pure extraction mechanism (snare) designed
 *   to eliminate indigenous political power and transfer their land and
 *   resources to settler institutions. The beneficiary structure is clear:
 *   European Jewish settlers and the Israeli state apparatus that administers
 *   the constraint. The victim structure is equally clear: Palestinian Arabs,
 *   who experience forced displacement, legal subordination, occupation, and
 *   denial of return rights. The extractiveness is high (0.87) and rising
 *   over the interval, reflecting continuous settlement expansion,
 *   institutional entrenchment, and deepening legal asymmetries. The theater
 *   ratio (0.41) reflects the constraint's dual operation: genuine security
 *   and institution-building functions coexist with performative legitimacy
 *   narratives (security framing for settlement expansion, nationalism
 *   framing for land claims) that mask dispossession.
 *
 * KEY AGENTS:
 *   - European Jewish settlers: primary beneficiaries, structural architects of land acquisition and legal exclusion mechanisms
 *   - Israeli state apparatus: agenda-setter, enforcer through military occupation, legal discrimination, and institutional consolidation
 *   - Palestinian Arabs: primary victims, structurally trapped within occupied territories and diaspora, denied equal legal status
 *   - Diaspora Jewish communities: observers providing legitimacy and political support, internally contested on settler-colonial frame
 *   - Western liberal democracies: observers providing military, economic, and diplomatic support, shielding the constraint from consequences
 *   - Palestinian resistance movements: excluded from meaningful power despite representing victim population, labeled 'terrorism' to delegitimize
 *   - International legal order: analytical observers documenting violations while lacking enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.87).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.82).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionist Settler-Colonial Dispossession (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '4b46503b-7ca9-4010-89a2-23e171f51201').
narrative_ontology:cs_kernel_codification('4b46503b-7ca9-4010-89a2-23e171f51201', fixed_text).
narrative_ontology:cs_authority_grounding('4b46503b-7ca9-4010-89a2-23e171f51201', extraction).
narrative_ontology:cs_interpretation_layer_present('4b46503b-7ca9-4010-89a2-23e171f51201').
narrative_ontology:cs_reading_relation('4b46503b-7ca9-4010-89a2-23e171f51201', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b46503b-7ca9-4010-89a2-23e171f51201', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('4b46503b-7ca9-4010-89a2-23e171f51201', jewish_self_determination__religious_covenant_reading, influences).
narrative_ontology:cs_reading_relation('4b46503b-7ca9-4010-89a2-23e171f51201', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('4b46503b-7ca9-4010-89a2-23e171f51201', foundational, palestinian_indigenous_status_primacy).
narrative_ontology:cs_axiom_status(palestinian_indigenous_status_primacy, holdable).
narrative_ontology:cs_axiom_grounding('4b46503b-7ca9-4010-89a2-23e171f51201', palestinian_indigenous_status_primacy, deontological).
narrative_ontology:cs_axiom('4b46503b-7ca9-4010-89a2-23e171f51201', foundational, zionism_as_european_settler_colonialism).
narrative_ontology:cs_axiom_status(zionism_as_european_settler_colonialism, holdable).
narrative_ontology:cs_axiom_grounding('4b46503b-7ca9-4010-89a2-23e171f51201', zionism_as_european_settler_colonialism, empirically_contingent).
narrative_ontology:cs_axiom('4b46503b-7ca9-4010-89a2-23e171f51201', secondary, land_dispossession_primary_mechanism).
narrative_ontology:cs_axiom_status(land_dispossession_primary_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('4b46503b-7ca9-4010-89a2-23e171f51201', land_dispossession_primary_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('4b46503b-7ca9-4010-89a2-23e171f51201', palestinian_indigenous_sovereignty_and_return).
narrative_ontology:cs_drift_state('4b46503b-7ca9-4010-89a2-23e171f51201', contemporary_post_oslo_accords_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4b46503b-7ca9-4010-89a2-23e171f51201', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, settler_colonialism_theory).
narrative_ontology:constraint_vindicates(jewish_self_determination__settler_colonial_reading, indigenous_dispossession_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European Jewish immigrants and their descendants who established Jewish-majority settlements and ultimately state institutions in Palestine/Israel. They collected land, resources, political authority, and security guarantees through migration, legal mechanisms favoring Jewish immigration (Law of Return), and military force. Their settlement proceeded through purchasing land from absentee owners, legal exclusion of Palestinian Arabs from ownership in Jewish areas, and displacement of Palestinian populations. They benefit from differential legal status, property rights, and security apparatus that protects Jewish settlement expansion while restricting Palestinian movement and building. Their exit option is to remain in Israel or migrate to third countries; returning disputed land or equalizing legal status would require abandoning the foundational advantage.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__settler_colonial_reading, european_jewish_settlers, agenda_setter).

% The institutional structure (military, courts, legislatures, bureaucracy) that administers the constraint through law and force. Enacts and enforces the Law of Return (granting automatic citizenship to Jewish immigrants while denying Palestinian refugees return rights), maintains military occupation, expands settlements, controls resource allocation (water, land, security permits), and uses legal mechanisms to legitimize dispossession. The state's legitimacy and institutional survival depend on maintaining Jewish demographic and political majority and settler territorial control. Its enforcement capacity (military, law enforcement, judiciary) is devoted to suppressing Palestinian resistance and normalizing settlement expansion. The apparatus is trapped: dismantling the constraint would dissolve its foundational power structure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Indigenous Palestinian population whose land, resources, and political authority were systematically expropriated through settler colonization. They experience the constraint as forced displacement (Nakba), ongoing military occupation, legal subordination (different rights in Jewish-only settlement areas, military law vs. civil law bifurcation), resource deprivation (water access, building permits), and denial of return rights for refugees (asymmetric to Law of Return). They cannot exit the constraint because leaving means permanent loss of land and property claims; staying means living under occupation or in confined enclaves with restricted movement. Resistance is met with state violence. Their children face a legal system that privileges settlers and denies their prior claims. The constraint's persistence depends on their inability to organize effective counter-power and the international community's failure to enforce equal rights.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_arabs, payer,
    powerless, generational, trapped, national).

% Jewish communities outside Israel who do not directly experience the constraint's extraction but whose support (financial, political, moral legitimacy) sustains it. They inhabit a contested seat: some see Israeli statehood as Jewish security after persecution; others see settler colonialism as incompatible with Jewish ethical traditions. Their exit option is to withdraw support, but doing so creates internal community conflict. They are not paid directly by the constraint but contribute to its operation through diaspora nationalism and political organizing that frames Israeli statehood as necessary Jewish self-determination.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, diaspora_jewish_communities, observer,
    powerful, generational, mobile, global).

% UN bodies, humanitarian institutions, and formal international law frameworks that ostensibly constrain settler colonialism and protect indigenous rights. Produce resolutions and legal interpretations naming Israeli settlements as violations of international law, yet lack enforcement mechanisms to compel compliance. Their role is contradictory: they witness and document the constraint while the most powerful state actors that could enforce remedies block action. They remain analytical observers unable to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, international_legal_order, observer,
    institutional, generational, analytical, global).

% Organized Palestinian political, military, and civil resistance seeking to end occupation, recover land, and establish Palestinian self-determination. They are structurally excluded from meaningful negotiating power despite their representation of the primary victim population. Their resistance is labeled 'terrorism' by the state apparatus, delegitimizing their voice and justifying suppression. If they were included in decisions over land allocation, resource distribution, and legal status, the constraint's structure would fundamentally shift.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, palestinian_resistance_movements, excluded,
    moderate, generational, trapped, national).

% States whose military, economic, and diplomatic support sustains Israeli state capacity and shields it from consequences of settlement expansion and occupation. They frame their stance through liberal-nationalist reading (nations have self-determination rights) while overlooking settler-colonial framework that rejects this reading's applicability. They could alter the constraint by conditioning aid on settlement freezes and equal rights, but choose not to, framing the constraint as a regional dispute rather than a structure of indigenous dispossession.
narrative_ontology:constraint_stakeholder(jewish_self_determination__settler_colonial_reading, western_liberal_democracies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(jewish_self_determination__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settler colonialism creates a coherent institutional order: it coordinates land acquisition, resource control, demographic advantage, and political authority into a unified structure favoring European Jewish settlers and their descendants. It solves the problem of establishing a Jewish-majority state in a land already inhabited by Palestinian Arabs by eliminating the indigenous population's legal standing and political power.
% TRANSFER_FUNCTION: Moves land, property, water rights, political authority, and security guarantees from Palestinian Arabs to European Jewish settlers and Israeli state institutions. The mechanism transfers via: (1) legal classification enabling Jewish land purchase while restricting Palestinian ownership (Ottoman and Mandate laws, later Israeli law); (2) military expropriation (conquest, settlement expansion); (3) discriminatory citizenship (Law of Return asymmetry); (4) differential legal status (separate court systems, permit regimes); (5) resource control (water allocation, building permits).
% ABSENT_VOICES: Palestinian resistance movements are excluded from meaningful power despite representing the primary victim population. Their interests would center on land return, equal legal status, and self-determination — claims structurally incompatible with the constraint's operation. Diaspora Jewish communities skeptical of settler colonialism are absent from institutional decision-making, where state apparatus consolidates the constraint.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight — if legal asymmetries were eliminated, Palestinian refugees granted return rights, settlements dismantled, and occupied territories placed under Palestinian sovereignty — the Israeli state as constituted would cease to exist (it depends on Jewish demographic majority, settler land ownership, and military control). Palestinian society would reorganize around return, property recovery, and self-governance. The regional power structure would shift fundamentally. The world-rearranges verdict holds because the constraint is not incidental to the political structure — it is constitutive of it.
% FOUNDING_PROBLEM: The founding problem (as framed by settler-colonial reading): European persecution of Jews created a diaspora minority vulnerable to violence, culminating in the Holocaust. Zionist ideology proposed territorial sovereignty as the solution, selecting Palestine as the site. But this framing omits the prior fact: Palestine was inhabited by Palestinian Arabs with their own claims. The founding problem, rightly stated, was: how to establish a Jewish state in a land already inhabited by another people? The settler-colonial answer was: dispossess the indigenous population through legal exclusion and force.
% FOUNDING_PROBLEM_CORROBORATION: From seats within the settler-colonial reading: Palestinian historians and scholars document the Nakba (catastrophe) as the foundational event of Israeli statehood; international human rights organizations cite settlement expansion and occupation as ongoing dispossession. From outside benefiting parties: postcolonial theorists (Edward Said, Frantz Fanon applied to Palestine) analyze Zionism through settler-colonial framework; UN fact-finding missions report systematic displacement and legal discrimination. From Israeli seats challenging the constraint: Israeli historians (Ilan Pappe, Avi Shlaim) document displacement of Palestinians and settlement planning as core to state-building. The corroboration exists outside the settler-beneficiary circle: it comes from victims, critical scholars, and international observers.
narrative_ontology:disappearance_verdict(jewish_self_determination__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__settler_colonial_reading, 0.87, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.87) reflects the constraint's design purpose: transfer of land, resources, and political authority from Palestinians to settlers. The measurement series show a steady rise from 0.65 at the founding (early settlement period) to 0.87 at the interval end, documenting increasing institutional entrenchment and settlement expansion. Suppression (0.82) is correspondingly high because the constraint's persistence depends on active state enforcement: military occupation suppresses Palestinian armed resistance, legal mechanisms deny Palestinian building permits while fast-tracking settler development, and international pressure is deflected by Western support. Theater ratio (0.41, rising to plateau) reflects the constraint's evolution: early periods emphasized security framing for settlement (higher functional legitimacy), but as expansion accelerates beyond security needs, the ratio stabilizes as rhetoric must increasingly perform legitimacy while material dispossession continues. Accessibility collapse (0.78) is high because Palestinian alternatives are systematically closed: land ownership is legally blocked, return rights are denied (asymmetric to Law of Return), and movement is restricted by occupation. Resistance (0.73) is substantial despite Palestinian structural disadvantage because dispossession generates continuous resistance (intifadas, popular movements, legal challenges) that the state must continuously suppress — resistance does not decline with enforcement, indicating the constraint is not legitimized by targets. All measurements share one time grid; every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The largest seat divergence is between the settler-beneficiary view and the victim view. From the settler institutional seat, the constraint appears as legitimate nation-building and security (Jewish self-determination post-Holocaust). From the Palestinian victim seat, the same structure appears as colonial dispossession and occupation. The liberal-nationalist reading (sibling constraint) would classify this as rope from the beneficiary seat and tangled rope from the victim seat, because it emphasizes mutual coordination problems and mutual nation-building. The settler-colonial reading rejects this: it asserts the constraint is snare from every seat except those benefiting from dispossession. The divergence is not epistemic uncertainty about the same fact, but fundamental disagreement about which facts are structurally relevant — whether land ownership prior to settlement counts as a legal right, whether the Holocaust justifies overriding indigenous claims, whether forced displacement is compatible with democratic nation-building.
 *
 * DIRECTIONALITY LOGIC:
 *   The settler-beneficiary seat (european_jewish_settlers, institutional power) has directionality near 0.0 (full beneficiary): they collect land, security guarantees, and political authority without running the occupation themselves (though they enable it through settlement). The state apparatus seat (israeli_state_apparatus, institutional power) has directionality near 0.5 (symmetric): it bears enforcement costs (military expenditure, international isolation) but collects monopoly control and legitimacy. The victim seat (palestinian_arabs, powerless) has directionality near 1.0 (full target): they pay through expropriation and oppression with no collected benefit. The diaspora observer seat (diaspora_jewish_communities, powerful) sits ambiguously at roughly 0.4 (moderate beneficiary): they contribute support but do not directly experience extraction, and internal dissent from the settler-colonial frame means some diaspora members reject the beneficiary position. The international observer seat (international_legal_order, institutional) remains analytical — it documents but cannot alter the constraint. The excluded Palestinian resistance seat (palestinian_resistance_movements, moderate power) would have directionality near 1.0 if included, which is precisely why they are excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish persecution and need for secure sovereignty) was historically live. But under the settler-colonial reading, it has become decoupled from the constraint's current operation. The founding problem status is DEAD: Jewish security is no longer served by continuous settlement expansion into Palestinian territory, occupation, and denial of Palestinian statehood. Yet the constraint persists and intensifies. This is a classic mandatrophy pattern: the constraint's original legitimating purpose (security against persecution) no longer explains its operation, but the institutional apparatus (military, settlement industry, legal system) that grew up around it continues expanding because it benefits entrenched interests. The theater ratio plateau (0.41, stabilizing rather than declining) supports this: the constraint has aged into a performative structure where security rhetoric maintains legitimacy while dispossession continues as the real function. The rising extractiveness despite stable rhetoric indicates function drift — extraction is no longer a byproduct of security-seeking but the primary operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_colonial_definition_boundary,
    'Does settler colonialism require continuity of indigenous population displacement, or does it apply once the structure is established even if displacement is historical? In other words, is the constraint still settler-colonial if settlement expansion stops and Palestinians are granted equal rights?',
    'Comparative postcolonial analysis: examine other cases where settler-colonial structures persisted after demographic displacement ceased (Australia, Canada, New Zealand) and whether theorists classify them as ongoing settler colonialism or as degraded/piton-like institutional remnants.',
    'If settler colonialism requires continuous displacement, a hypothetical future where settlements stabilize and rights equalize would shift the constraint''s type to piton or even rope. If it applies structurally (colonial property relations, legal asymmetries) regardless of contemporaneous displacement, the constraint remains snare indefinitely unless those structures are dismantled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_definition_boundary, conceptual, 'Whether settler colonialism is defined by continuous dispossession or by institutional structure persisting after initial displacement.').

omega_variable(
    beneficiary_identification_asymmetry,
    'Are diaspora Jewish communities genuine beneficiaries of the constraint, or are they observers whose support is mobilized under the liberal-nationalist frame despite their structural non-participation in extraction?',
    'Survey and interview diaspora communities: do they perceive themselves as benefiting from Israeli statehood, and if so, at what cost (identity tension, political isolation in their home countries, complicity in occupation)? Track whether diaspora support correlates with awareness of settler-colonial frame.',
    'If diaspora communities are genuine beneficiaries, the beneficiary group is broader (european_jewish_settlers + diaspora_jewish_communities) and the constraint''s legitimacy rests on global Jewish nationalism. If they are mobilized observers who do not benefit, the constraint''s real beneficiary base is narrower (settlers + state apparatus) and its global legitimacy rests more on Western state support than on Jewish collective interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_asymmetry, empirical, 'Whether diaspora Jewish communities are beneficiaries or non-benefiting supporters of the constraint.').

omega_variable(
    kernel_vs_reading_committer_frame,
    'Is the settler-colonial reading instantiating the true structural reality of Zionism, or is it one among equally valid readings whose truth-claim depends on normative commitments about indigenous rights and decolonization?',
    'Operator ruling: the framework treats all readings as ''true for their reading'' — what is true is that this reading DOES impose a settler-colonial structure on the kernel. But the deeper question (whether that structure was inevitable, justified, avoidable) remains contestable. This omega documents that the constraint''s classification as snare is internal to the settler-colonial reading''s commitments; a liberal-nationalist reading would compute a different type from the same structural facts.',
    'If the settler-colonial reading is the uniquely correct framing, then the other readings are false cover stories and the constraint should be classified snare from every analytical seat. If readings are equally valid but value-indexed, then the snare classification holds for this reading but the engine would also compute rope or tangled-rope from other readings'' reference frames. The difference affects how the corpus understands constraint families: are they alternative verdicts on one objective fact, or are they structural facts indexed to different normative frameworks?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_vs_reading_committer_frame, preference, 'Whether the settler-colonial reading is the uniquely correct interpretation or one among value-indexed alternative readings of the same kernel.').

omega_variable(
    suppression_internalization_palestinian_seat,
    'To what extent is Palestinian compliance with occupation structurally enforced (military checkpoints, legal barriers, resource deprivation) versus internalized (acceptance of subordination, legal identity as occupied rather than dispossessed)?',
    'Post-state scenarios: if occupation enforcement were suddenly withdrawn, would suppression collapse immediately or persist through internalized subordination norms? Study Palestinian resistance trajectories and identity formation under occupation.',
    'If suppression is primarily structural, removing enforcement would allow rapid Palestinian reorganization. If suppression is partially internalized, Palestinian society would carry the constraint''s effects even after occupation ends, with long-term generational costs to recovery. This affects assessment of whether the constraint is sustainable long-term or faces inherent pressure from resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_palestinian_seat, empirical, 'Structural versus internalized mechanisms of Palestinian suppression under occupation.').

omega_variable(
    competing_kernel_readings_logical_status,
    'Between the settler-colonial reading and the indigenous-return reading: are these logically incompatible (one must be false if the other is true), or do they describe different partial truths that could both be partially valid?',
    'Genealogical analysis: trace the historical claims — Jewish presence in antiquity, diaspora period, modern return, Palestinian continuous residence. Determine whether both peoples can claim continuous indigenous ties to the same land, or whether one claim necessarily forecloses the other.',
    'If indigenous status is mutually exclusive (first peoples alone), one reading forecloses the other and political resolution requires settling which people are indigenous by fact. If indigenous status can be shared or reconstructed, both readings could partially apply and the constraint is a conflict between partial claims rather than settler colonialism versus indigenous return. This affects whether the constraint''s resolution requires one reading to be accepted as true or whether political settlement can accommodate plural historical narratives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competing_kernel_readings_logical_status, conceptual, 'Whether settler-colonial and indigenous-return readings logically foreclose each other or both capture partial truths about land claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__settler_colonial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(jewi_tr_t0, projected).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__settler_colonial_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t10, observed).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__settler_colonial_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t20, observed).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__settler_colonial_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t30, observed).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__settler_colonial_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(jewi_tr_t40, observed).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__settler_colonial_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t50, observed).
narrative_ontology:measurement(jewi_tr_t60, jewish_self_determination__settler_colonial_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t60, observed).
narrative_ontology:measurement(jewi_tr_t70, jewish_self_determination__settler_colonial_reading, theater_ratio, 70, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t70, observed).
narrative_ontology:measurement(jewi_tr_t80, jewish_self_determination__settler_colonial_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__settler_colonial_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(jewi_be_t0, projected).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__settler_colonial_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement_basis(jewi_be_t10, observed).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__settler_colonial_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(jewi_be_t20, observed).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__settler_colonial_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(jewi_be_t30, observed).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__settler_colonial_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(jewi_be_t40, observed).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__settler_colonial_reading, base_extractiveness, 50, 0.83).
narrative_ontology:measurement_basis(jewi_be_t50, observed).
narrative_ontology:measurement(jewi_be_t60, jewish_self_determination__settler_colonial_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement_basis(jewi_be_t60, observed).
narrative_ontology:measurement(jewi_be_t70, jewish_self_determination__settler_colonial_reading, base_extractiveness, 70, 0.87).
narrative_ontology:measurement_basis(jewi_be_t70, observed).
narrative_ontology:measurement(jewi_be_t80, jewish_self_determination__settler_colonial_reading, base_extractiveness, 80, 0.87).
narrative_ontology:measurement_basis(jewi_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_self_determination__settler_colonial_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(jewi_su_t0, projected).
narrative_ontology:measurement(jewi_su_t10, jewish_self_determination__settler_colonial_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(jewi_su_t10, observed).
narrative_ontology:measurement(jewi_su_t20, jewish_self_determination__settler_colonial_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(jewi_su_t20, observed).
narrative_ontology:measurement(jewi_su_t30, jewish_self_determination__settler_colonial_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement_basis(jewi_su_t30, observed).
narrative_ontology:measurement(jewi_su_t40, jewish_self_determination__settler_colonial_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(jewi_su_t40, observed).
narrative_ontology:measurement(jewi_su_t50, jewish_self_determination__settler_colonial_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement_basis(jewi_su_t50, observed).
narrative_ontology:measurement(jewi_su_t60, jewish_self_determination__settler_colonial_reading, suppression_requirement, 60, 0.81).
narrative_ontology:measurement_basis(jewi_su_t60, observed).
narrative_ontology:measurement(jewi_su_t70, jewish_self_determination__settler_colonial_reading, suppression_requirement, 70, 0.82).
narrative_ontology:measurement_basis(jewi_su_t70, observed).
narrative_ontology:measurement(jewi_su_t80, jewish_self_determination__settler_colonial_reading, suppression_requirement, 80, 0.82).
narrative_ontology:measurement_basis(jewi_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__settler_colonial_reading, 0.18).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, palestinian_self_determination__dispossession_frame).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, occupation_governance_structure).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, law_of_return_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is part of a five-reading kernel family on Jewish self-determination. The settler-colonial reading models Zionism as snare (extraction through dispossession); the liberal-nationalist reading models it as tangled-rope (coordination + asymmetric extraction); the indigenous-return reading denies the settler frame and models it as rope (coordinate mutual defense); the religious-covenant reading shifts the justification from secular nationalism to divine claim; the diasporist reading rejects territorial sovereignty entirely. Each reading instantiates a different constraint with a different epsilon, different beneficiary/victim structure, and different type. They are linked structurally (all interpret the same kernel) and causally (the settler-colonial reading's empirical claims about dispossession support the diasporist reading's argument that territorial sovereignty is incompatible with Jewish ethics; the indigenous-return reading's empirical claims about prior Jewish presence support the liberal-nationalist reading's argument that the conflict is symmetrical). The settler-colonial reading influences all others by establishing dispossession as a fact that any reading must account for or deny.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
