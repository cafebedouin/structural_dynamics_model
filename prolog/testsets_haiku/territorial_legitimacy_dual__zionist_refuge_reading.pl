% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Israeli Territorial Legitimacy (Zionist Refuge Reading)
 *   domain: political_theory/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel of
 *   territorial legitimacy in the Levant. The zionist_refuge_reading grounds
 *   Israeli territorial sovereignty in three pillars: (1) historical
 *   persecution of Jews requiring a secure refuge, (2) divine promise
 *   (covenant narrative), and (3) UN Partition authorization (1947). The
 *   reading frames the 1948 establishment of the state as legitimate and
 *   largely uncontested; the 1967 occupation and territorial expansion as
 *   negotiable but justified by security imperatives and Palestinian
 *   rejection of partition; and Palestinian displacement as a consequence of
 *   Arab refusal to accept the partition, not as an inherent cost of the
 *   constraint itself. This reading coexists with the
 *   palestinian_autochthony_reading (which grounds Palestinian legitimacy in
 *   continuous habitation and displacement trauma) and the
 *   two_state_coexistence_reading (which seeks mutual recognition of both
 *   claims). The claim/metric gap is deliberate: the reading claims
 *   tangled_rope (genuine coordination function — refuge for a persecuted
 *   people — plus asymmetric extraction — dispossession and occupation of
 *   Palestinians); the authored metrics show the constraint operates with
 *   high extractiveness, substantial suppression, and rising theater
 *   (security framing increasingly performing rather than implementing
 *   coordination). The measurement series tracks the constraint from its
 *   ideological origins (1897 Herzl) through realization (1948) and expansion
 *   (1967) to present, showing extraction rising after partition and theater
 *   increasing after the Oslo Accords framework institutionalized the
 *   occupation.
 *
 * KEY AGENTS:
 *   - Israeli state apparatus: administers and enforces the constraint through military and legal institutions; claims legitimacy from the reading's three pillars
 *   - Jewish national movement and diaspora: articulates the reading's narrative; benefits from security umbrella and territorial refuge the reading provides
 *   - Palestinian Arabs (1948 displaced, 1967 occupied): bear the constraint's costs; experience dispossession and military control; excluded from adjudicating the legitimacy question
 *   - Western governments (US, EU): endorse the reading through diplomatic recognition and strategic support; benefit from aligned legitimacy framing
 *   - Arab states and international bodies: contest or remain excluded from the reading; would voice Palestinian autochthony and right-of-return claims if given institutional power
 *   - Conservative diaspora Jewish constituencies: benefit from refuge provision and security guarantees; maintain identity while outside the enforcement zone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.71).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Israeli Territorial Legitimacy (Zionist Refuge Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f').
narrative_ontology:cs_kernel_codification('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', fixed_text).
narrative_ontology:cs_authority_grounding('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', lineage).
narrative_ontology:cs_interpretation_layer_present('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f').
narrative_ontology:cs_reading_relation('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', foundational, jewish_historical_persecution_establishes_refuge_legitimacy).
narrative_ontology:cs_axiom_status(jewish_historical_persecution_establishes_refuge_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', jewish_historical_persecution_establishes_refuge_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', foundational, divine_covenant_claim_territorial_right).
narrative_ontology:cs_axiom_status(divine_covenant_claim_territorial_right, holdable).
narrative_ontology:cs_axiom_grounding('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', divine_covenant_claim_territorial_right, theological).
narrative_ontology:cs_axiom('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', secondary, un_partition_international_authorization).
narrative_ontology:cs_axiom_status(un_partition_international_authorization, holdable).
narrative_ontology:cs_axiom_grounding('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', un_partition_international_authorization, conventional).
narrative_ontology:cs_axiom('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', secondary, security_occupation_justified_by_arab_rejection).
narrative_ontology:cs_axiom_status(security_occupation_justified_by_arab_rejection, holdable).
narrative_ontology:cs_axiom_grounding('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', security_occupation_justified_by_arab_rejection, empirically_contingent).
narrative_ontology:cs_reference_frame('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', jewish_refuge_covenant).
narrative_ontology:cs_drift_state('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', post_oslo_accords_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e60cf61a-1bf2-4462-9bc2-1ec9c74bec5f', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_refugees).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_national_movement).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_arabs_displaced_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_arabs_occupied_1967).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, western_governments_us_eu).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, conservative_jewish_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_national_authority).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, jewish_historical_persecution_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, divine_promise_covenantal_claim).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The global Jewish diaspora gains from the reading's provision of territorial refuge and security guarantees. They benefit from the historical narrative that frames their vulnerability as legitimate and requiring sovereign protection. The Law of Return provides exit option (they can immigrate to Israel) but most remain in diaspora while enjoying the security umbrella the state provides.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_refugees, beneficiary,
    powerful, civilizational, arbitrage, global).

% Administers and enforces the constraint through military, legal, and institutional structures. The state's legitimacy claim depends on the reading: that Jewish historical vulnerability establishes the right to territorial sovereignty, that divine promise or UN partition authorizes the state, and that security imperatives justify territorial control. Without the reading, the state's sovereignty becomes negotiable with Palestinian claims.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_apparatus, agenda_setter,
    institutional, civilizational, trapped, national).

% The institutional apparatus (World Zionist Organization, institutional networks in Israel and diaspora) articulates and sustains the reading's narrative. The movement mobilizes constituencies around the historical persecution and refugee legitimacy framing and resists alternative readings as delegitimizing threats. Embedded in Israeli state institutions and diaspora Jewish civil society.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_national_movement, beneficiary,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, jewish_national_movement, agenda_setter).

% Experienced dispossession from homes, land, and property upon establishment of the Israeli state. The reading frames their displacement as a consequence of Arab rejection of partition, not as a direct cost of the constraint. Their Palestinian identity is inseparable from the lost homeland; they cannot exit the constraint. They experience the reading as enforced narrative that justifies their loss.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_arabs_displaced_1948, payer,
    powerless, generational, identity_locked, regional).

% Live under military occupation justified by the reading's security framing. Movement is restricted, resources are appropriated, and governance is subordinated to Israeli security administration. The reading frames occupation as temporary pending final status negotiation; Palestinians experience it as indefinite territorial control. Their exit options are blocked by military administration and legal restrictions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_arabs_occupied_1967, payer,
    powerless, generational, constrained, regional).

% UN institutions witness and adjudicate the constraint's operation. The reading appeals to UN Partition Plan 181 as authorizing legitimacy; UN bodies increasingly invoke Palestinian rights and occupation status as countervailing claims. The reading's legitimacy appeal remains contested within international institutions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_community_un_bodies, observer,
    institutional, generational, analytical, universal).

% Benefit from the reading's legitimacy framework by endorsing Israeli statehood as grounded in post-WWII international law and Holocaust remembrance. They gain a strategic ally in a contested region. They have options to shift support to alternative readings but maintain this framing as the primary working basis for Middle East diplomacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, western_governments_us_eu, beneficiary,
    institutional, generational, arbitrage, global).

% Rejected the reading at its founding (1947 Partition Plan rejection) and remain structurally excluded from adjudicating the constraint's legitimacy within the international consensus. Their power is substantial regionally but constrained in the global legitimacy apparatus the reading appeals to. They would articulate alternative readings (Palestinian autochthony) if given institutional voice.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_states_opposition, excluded,
    powerful, generational, constrained, regional).

% Administers limited autonomy under Israeli occupation (post-Oslo Accords framework) but is excluded from adjudicating the fundamental legitimacy question. Would articulate Palestinian autochthony reading if given full voice but lacks institutional power to contest the Zionist refuge reading at the global level. Bears the cost of limited sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_national_authority, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_national_authority, excluded).

% Maintains diaspora identity while benefiting from the constraint's provision of territorial refuge and national sovereignty. The reading vindicates their historical experience of vulnerability. They have the option to immigrate to Israel (Law of Return) or remain in diaspora, making their exit options more open than those trapped in the enforcement zone.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, conservative_jewish_diaspora, beneficiary,
    organized, civilizational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial refuge and sovereign state structure for a historically persecuted diaspora (Jewish peoples) who lacked legal protection or territorial anchor during centuries of displacement. Solves the coordination problem of how a stateless people with dispersed global presence can establish collective security, legal standing, and cultural continuity.
% TRANSFER_FUNCTION: Transfers territory, resources (land, water, property), and political authority from Palestinian Arabs to the Jewish state and its institutions. The reading frames this transfer as justified by Jewish historical claim, divine promise, and UN partition authorization. The gain accrues to the Israeli state apparatus, which administers the territory and collects rents from resource extraction and territorial control.
% ABSENT_VOICES: Palestinian Arabs (the dispossessed and occupied) are present as payers but excluded from adjudicating the constraint's legitimacy. Arab states are excluded from the international legal consensus. Diaspora Jews who dissent from the reading (anti-Zionists, those questioning occupation legitimacy) are marginalized within institutional structures. International bodies that challenge the reading (UN General Assembly resolutions, human rights organizations) are framed as delegitimizing rather than co-adjudicators.
% DISAPPEARANCE_RATIONALE: If the constraint (Israeli territorial legitimacy grounded in the zionist_refuge_reading) disappeared globally, the territorial order would reorganize: Palestinian governance would expand, refugee return and property restitution would become live questions, regional geopolitics would shift, and Western Middle East strategy would require fundamental reframing.
% FOUNDING_PROBLEM: Jewish historical vulnerability: centuries of diaspora statelessness, persecution, pogroms, culminating in the Holocaust. The founding problem was how to establish a territorial sanctuary where Jewish self-determination and security could be guaranteed without dependence on the mercy of others.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state apparatus, Jewish national movement, and Western governments attest the founding problem is foundational and remains live (antisemitism, security threats, need for refuge). Palestinian national authority and Arab states attest the founding problem, while serious, does not justify ongoing occupation. International human rights bodies affirm the founding problem but contest the territorial solution as disproportionate to the problem it solves. No corroboration exists from outside the benefiting parties (Israeli state, Western governments, Jewish institutional networks) that the specific territorial solution and ongoing occupation are justified by the founding problem.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from near-zero in 1897 (ideological stage, no enforcement) through 0.15 at 1947 (pre-state, framework-building) to 0.52 at 1948 (immediate displacement extraction upon state realization) and plateaus around 0.64-0.68 after 1967 (occupation extraction settles into a stable but asymmetric arrangement). The reading justifies the extraction by appeal to Jewish historical vulnerability and security necessity. Suppression is high and rising (0.58 at 1948, 0.71 by 2026) because the constraint's persistence depends on actively excluding alternative readings and suppressing Palestinian resistance to the territorial arrangement — alternatives (right of return, Palestinian sovereignty, equal rights) are framed as delegitimizing rather than as legitimate counterpositions. Theater rises from 0.15 at 1948 (when the security problem was more acute) to 0.42 by 2026, reflecting that security briefings increasingly serve to justify territorial control rather than to respond to changing threat assessments. The constraint exhibits tangled_rope structure: genuine coordination function (territorial refuge for a persecuted diaspora) paired with asymmetric extraction (Palestinian displacement and occupation). The measurement grid uses a shared time axis across all three metrics because the constraint's operation is a unitary process: extraction rises as the security enforcement apparatus becomes capable (1948-1967), theater rises as that apparatus becomes performative (1967-present), and suppression remains high and stable because alternative readings are always threatening to the legitimacy consensus.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state apparatus and diaspora Jewish constituencies compute this constraint as a genuine coordination necessity with justifiable security enforcement — they see refuge for the persecuted as overriding extraction concerns. Palestinian victims and excluded Arab states compute it as pure extraction dressed in historical narrative — they see dispossession and occupation, with the security framing as post-hoc justification. Western governments occupy a middle seat: they endorse the legitimacy reading (beneficiary side) while expressing concern about occupation conduct (partial acknowledgment of extraction). The engine computes these divergences per-seat from the structural data. The perspectival gap is the reading itself — alternative readings (Palestinian autochthony, two-state coexistence) would produce different per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state apparatus: d near 0.0 (full beneficiary) — sets the rules, enforces them, collects legitimacy and territorial control. Jewish diaspora: d near 0.15 (beneficiary) — gains refuge and security umbrella without being in the enforcement zone. Palestinian victims: d near 1.0 (full target) — territorial control is enforced upon them, exit is blocked by military administration and identity lock. Western governments: d near 0.2 (partial beneficiary) — gain legitimacy framing and strategic ally without bearing enforcement costs. The reading's narrative (persecution, divine promise, UN partition) is authored as legitimate coordinative justification; the extraction it produces (displacement, occupation) is authored as justified by security imperatives and Palestinian rejection of partition. This directionality structure is what makes the constraint tangled_rope: beneficiaries (diaspora, state, Western powers) coordinate around the reading; victims (Palestinian Arabs) are extracted from without meaningful voice in the legitimacy consensus.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish vulnerability requiring territorial refuge) is CONTESTED in status: Israeli institutions attest it is live; Palestinian and Arab authorities attest it is solved (a state exists) but does not justify ongoing occupation; international human rights bodies attest the security framing masks indefinite territorial control. The founding_problem_status is contested because the corroboration divides: benefiting parties (Israeli state, Western governments) attest the problem remains live; excluded parties (Palestinian authority, Arab states, international bodies) attest the problem is solved but does not license the current territorial solution. The reading avoids simple mandatrophy (where the founding problem is dead and the arrangement persists as pure theater) by maintaining that security threats remain live — the measurement series reflects this by keeping suppression_requirement stable and high even as theater rises, claiming that security imperatives remain binding even as the framing becomes increasingly theatrical. The omega variables document this contested status: is the founding problem actually live, or is the reading maintaining a threat narrative to justify permanent occupation?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_live_vs_resolved,
    'Is the Jewish historical vulnerability that the reading cites as founding problem still live, or has territorial statehood resolved it such that the ongoing occupation is no longer justified by security imperatives?',
    'Empirical assessment of security threats to Israel and Jewish diaspora after the establishment of the state. Compare actual threat trajectories (terrorist incidents, international sanctions, war frequency) to the reading''s ongoing security justifications. Examine whether threat levels justify the maintenance of occupation and suppression of alternatives.',
    'If the founding problem is dead (state is secure, threats are contained), the measurement of mandatrophy becomes critical: the constraint persists as an enforced extraction masked by security theater, shifting classification toward snare. If the founding problem is live, the tangled_rope classification holds — the extraction is justified by genuine security necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_vs_resolved, empirical, 'Whether Jewish historical vulnerability justifies ongoing territorial control.').

omega_variable(
    divine_promise_legitimacy_status,
    'Does appeal to divine promise (covenant narrative, religious claim to territory) constitute a legitimate basis for territorial claims in an international legal system grounded in secular sovereignty and self-determination?',
    'Jurisprudential analysis of how international law (UN Charter, human rights treaties) treats territorial claims grounded in religious or historical-mythological narratives versus territorial claims grounded in self-determination and continuous habitation. Examine whether the reading''s reliance on divine promise is accepted by international adjudicators (International Court of Justice) or treated as non-legal.',
    'If divine promise is accepted as legitimate legal ground, the reading''s three-pillar legitimacy holds. If it is treated as non-legal (as many international authorities treat it), the reading''s legitimacy rests on persecution history and UN partition alone — narrowing the ground. This affects whether the reading''s foundational axioms are holdable in contemporary international law or require reformulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_legitimacy_status, conceptual, 'Whether religious/historical claims constitute legitimate territorial authority in secular international law.').

omega_variable(
    partition_consent_and_displacement,
    'Does Arab rejection of the 1947 Partition Plan justify framing Palestinian displacement as a consequence of Arab rejection rather than as a direct cost of the constraint''s implementation?',
    'Historical and causal analysis: examine the sequence of events during 1947-1949 (partition vote, Arab rejection, state establishment, forced displacement). Assess whether Palestinian displacement was: (a) a direct and predictable consequence of establishing a Jewish state in territory inhabited by Palestinian Arabs, or (b) a contingent consequence of Arab military rejection of the partition. Examine whether Palestinians (as opposed to Arab states) consented to or rejected partition, and whether their individual rejection justifies their dispossession.',
    'If displacement is understood as a direct cost of the constraint (option a), the reading''s framing is a cover story — the extraction is inherent, not contingent. If displacement is contingent on Arab military rejection (option b), the reading''s causal framing holds — the constraint coordinates refuge without inherently requiring displacement. This affects the interpretation of 1948 as the founding moment: is it the moment extraction begins, or the moment coordination succeeds?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_consent_and_displacement, empirical, 'Whether Palestinian displacement is a direct cost of establishing the state or a contingent consequence of Arab rejection.').

omega_variable(
    reading_vs_sibling_logical_structure,
    'Are the zionist_refuge_reading, palestinian_autochthony_reading, and two_state_coexistence_reading logically incompatible in any single territorial framework, or do they represent genuinely coexisting positions held by different constituencies?',
    'Examine whether the three readings could be held simultaneously by a single institutional actor (Israeli state, Palestinian authority, international community) or whether adopting one entails rejecting the others. Test whether a framework accepting mutual recognition (two-state coexistence) necessarily forecloses Palestinian full autochthony or Israeli full refuge. Examine whether the readings represent genuinely alternative legitimacy paradigms or merely different emphasis within a shared framework.',
    'If the readings are logically incompatible, the constraint''s type diverges per-reading: one reading computes as legitimate coordination, another as pure extraction, depending on which legitimacy paradigm is adopted. If the readings coexist as alternative constituencies'' positions within a pluralistic frame, the constraint remains contested and tangled_rope across seats. This determines whether the engine should compute a single type or read per-reading divergence as the primary signal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_sibling_logical_structure, conceptual, 'Logical compatibility of sibling readings within a single territorial framework.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of Palestinian resistance and alternative legitimacy readings (measured at 0.71) primarily structural (military occupation, legal restrictions, institutional barriers) or internalized (Palestinians'' adoption of the reading''s framing, loss of hope in alternatives)?',
    'Post-occupation suppression trajectory: if occupation and suppression measures were removed, would Palestinian resistance and alternative readings immediately revive (structural suppression) or persist in attenuation (internalized suppression)? Examine historical cases where occupation ended (Golan Heights agreements, Camp David Accords) and track whether resistance and counter-readings revived or remained suppressed.',
    'If suppression is primarily structural, removing enforcement machinery would restore alternatives; the constraint''s persistence depends on active coercion. If suppression is partially internalized, the reading has become naturalized in Palestinian consciousness and self-conception; exit remains constrained even after enforcement is removed. This affects the measurement of effective suppression: the scalar 0.71 does not distinguish mechanisms, but an omega reveals the deeper structural question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether Palestinian suppression is maintained by external military/legal force or by internalized acceptance of the reading''s framing.').

omega_variable(
    kernel_reading_committer_frame,
    'Is this constraint best understood as a reading of a contested kernel (territorial legitimacy grounded in competing foundational narratives), or as an autonomous claim that Israeli statehood''s legitimacy is natural and uncontested?',
    'Examine the constraint''s generation context: if the analysis assumes the reading is one among multiple legitimate alternatives, it is a kernel reading; if it assumes the reading is the only legitimate legitimacy narrative, it is autonomous. Check whether the analysis includes the sibling readings as live alternatives or as illegitimate challenges. The kernel reading frame requires symmetric treatment of siblings; the autonomous frame treats alternatives as mistakes or propaganda.',
    'If kernel reading: the classification is reading-relative, and alternative readings generate different classifications; the engine computes per-reading divergence as the primary signal, and mandatrophy analysis focuses on the contested foundational premises. If autonomous: the classification is objective, alternatives are errors, and mandatrophy analysis focuses on institutional persistence. The difference affects how the constraint feeds into cross-reading coupling analysis and how the corpus interprets consent and legitimacy consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, preference, 'Whether this constraint is one reading of a contested kernel or an autonomous legitimacy claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1897, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1897, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1897, 0.0).
narrative_ontology:measurement_basis(terr_tr_t1897, projected).
narrative_ontology:measurement(terr_tr_t1947, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement_basis(terr_tr_t1947, observed).
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1993, 0.38).
narrative_ontology:measurement_basis(terr_tr_t1993, observed).
narrative_ontology:measurement(terr_tr_t2026, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(terr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1897, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1897, 0.0).
narrative_ontology:measurement_basis(terr_be_t1897, projected).
narrative_ontology:measurement(terr_be_t1947, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement_basis(terr_be_t1947, observed).
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.52).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.64).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1993, 0.66).
narrative_ontology:measurement_basis(terr_be_t1993, observed).
narrative_ontology:measurement(terr_be_t2026, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(terr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1897, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1897, 0.0).
narrative_ontology:measurement_basis(terr_su_t1897, projected).
narrative_ontology:measurement(terr_su_t1947, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1947, 0.22).
narrative_ontology:measurement_basis(terr_su_t1947, observed).
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.68).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1993, 0.7).
narrative_ontology:measurement_basis(terr_su_t1993, observed).
narrative_ontology:measurement(terr_su_t2026, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(terr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.18).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel territorial_legitimacy_dual. The zionist_refuge_reading grounds Israeli legitimacy in historical persecution, divine promise, and UN partition. Sibling readings (palestinian_autochthony_reading and two_state_coexistence_reading) ground legitimacy in Palestinian continuous habitation / displacement trauma and in negotiated mutual recognition respectively. The readings form a constraint family linked by network.affects_constraints: each reading influences the others' operational conditions and legitimacy consensus. The cluster demonstrates constraint decomposition per DP-001 (ε-invariance): a single natural-language concept (territorial legitimacy in the Levant) decomposes into three structurally distinct constraints with different ε values, different beneficiary/victim structures, and different measured types — the decomposition is not observable-dependent but reading-dependent (different normative framings produce different structural facts about what benefits whom and what is extracted).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
