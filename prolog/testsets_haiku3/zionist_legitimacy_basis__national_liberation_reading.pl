% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionist Legitimacy Basis — National Liberation Reading
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint models Zionism as a national-liberation reading of Jewish
 *   political claims to territorial establishment in the Levant. The reading
 *   frames Jewish return as vindicated by continuous historical-cultural
 *   presence, justified by persecution and diaspora vulnerability, and as a
 *   legitimate exercise of self-determination. Under this reading,
 *   Palestinian Arab presence is characterized as recent settlement or
 *   secondary to Jewish historical claims; Arab opposition is delegitimized
 *   as denial of Jewish rights rather than recognized as competing
 *   territorial claims. The constraint's structure is tangled_rope: it
 *   achieves genuine coordination of diaspora communities into political
 *   unity AND coordinates Hebrew cultural revival, but does so by extracting
 *   territorial control and political authority from Palestinian residents
 *   who did not consent and whose competing claims are suppressed by the
 *   reading's framing. Extractiveness rises over the 30-unit interval as
 *   settlement expands and institutional enforcement hardens; theater ratio
 *   rises as security/rights narratives bear increasing weight relative to
 *   functional coordination; suppression requirement rises as resistance to
 *   the reading's premises intensifies.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_communities — primary beneficiary of refuge/homeland establishment; provide political and material support; exit options include staying diaspora (arbitrage advantage)
 *   - hebrew_cultural_revivalists — secondary beneficiary of cultural-institutional space; constrained exit (cultural identity fusion); biologize the reading through language and norm-setting
 *   - palestinian_arab_residents — structural victims; bear dispossession and subordination; constrained exit (local entrapment); their continuous occupation and self-determination claims are reframed as secondary by the reading
 *   - international_jewish_organizations — agenda_setter; articulate and enforce the reading institutionally; mobilize diaspora resources and international diplomatic support; delegitimize competing readings
 *   - arab_neighboring_states — diffuse payers (refugee burden, regional instability); organizational power but constrained regional exit; structurally excluded from canonical authority over the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.68).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.72).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist Legitimacy Basis — National Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '402d752d-b90b-447e-a3d6-519bfbc7ad89').
narrative_ontology:cs_kernel_codification('402d752d-b90b-447e-a3d6-519bfbc7ad89', formalized).
narrative_ontology:cs_authority_grounding('402d752d-b90b-447e-a3d6-519bfbc7ad89', lineage).
narrative_ontology:cs_interpretation_layer_present('402d752d-b90b-447e-a3d6-519bfbc7ad89').
narrative_ontology:cs_reading_relation('402d752d-b90b-447e-a3d6-519bfbc7ad89', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('402d752d-b90b-447e-a3d6-519bfbc7ad89', zionist_legitimacy_basis__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('402d752d-b90b-447e-a3d6-519bfbc7ad89', foundational, persecution_justifies_territorial_return).
narrative_ontology:cs_axiom_status(persecution_justifies_territorial_return, holdable).
narrative_ontology:cs_axiom_grounding('402d752d-b90b-447e-a3d6-519bfbc7ad89', persecution_justifies_territorial_return, empirically_contingent).
narrative_ontology:cs_axiom('402d752d-b90b-447e-a3d6-519bfbc7ad89', foundational, historical_cultural_claim_supersedes_occupant_rights).
narrative_ontology:cs_axiom_status(historical_cultural_claim_supersedes_occupant_rights, holdable).
narrative_ontology:cs_axiom_grounding('402d752d-b90b-447e-a3d6-519bfbc7ad89', historical_cultural_claim_supersedes_occupant_rights, deontological).
narrative_ontology:cs_axiom('402d752d-b90b-447e-a3d6-519bfbc7ad89', secondary, jewish_peoplehood_continuous_across_diaspora).
narrative_ontology:cs_axiom_status(jewish_peoplehood_continuous_across_diaspora, holdable).
narrative_ontology:cs_axiom_grounding('402d752d-b90b-447e-a3d6-519bfbc7ad89', jewish_peoplehood_continuous_across_diaspora, empirically_contingent).
narrative_ontology:cs_reference_frame('402d752d-b90b-447e-a3d6-519bfbc7ad89', jewish_diaspora_vulnerability_and_rightful_return).
narrative_ontology:cs_drift_state('402d752d-b90b-447e-a3d6-519bfbc7ad89', contemporary_post_1967_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('402d752d-b90b-447e-a3d6-519bfbc7ad89', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, hebrew_cultural_revivalists).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_neighboring_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a homeland framed as rightful return, resolving statelessness and vulnerability to persecution. The reading vindicates their continuous peoplehood claim and transforms historical victimhood into a legitimating narrative for territorial establishment. They provide political, financial, and emigration support to the emerging state.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, arbitrage, global).

% Achieve institutional space to practice and revive Hebrew language, culture, and social institutions after centuries of diaspora suppression. The reading frames this revival as restoration rather than creation, naturalizing Hebrew as the language of the land and legitimizing cultural dominance.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, hebrew_cultural_revivalists, beneficiary,
    organized, biographical, constrained, regional).

% Bear the structural cost of displacement, dispossession, and subordination under the reading's logic. Their pre-existing presence and claims are reframed as recent settlement or secondary to Jewish historical claims. Their resistance is delegitimized as denial of Jewish rights rather than recognized as defense of territorial possession.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arab_residents, payer,
    moderate, biographical, constrained, local).

% Absorb refugee populations, confront state-level territorial reorganization, and navigate geopolitical pressure to recognize or contest the establishment. Their costs are diffuse (humanitarian burden, regional instability, sovereignty complications) and their voice in the reading's legitimacy claim is structurally excluded.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_neighboring_states, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, arab_neighboring_states, observer).

% Set and enforce the reading through institutional advocacy, narrative framing, and diplomatic pressure. They articulate the persecution-justifies-return thesis, delegitimize competing readings (settler-colonial, indigenous-rights), and mobilize resources to maintain the legitimacy frame.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, international_jewish_organizations, agenda_setter,
    institutional, generational, arbitrage, global).

% Provide evidentiary substrate for the reading: continuous Jewish presence in the region over 2,000 years, diaspora persecution narratives, and linguistic/cultural continuity claims. They do not set the reading but supply the factual warrant for the legitimacy frame.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, historians_of_jewish_dispersion, observer,
    analytical, biographical, analytical, global).

% Would contest the reading by asserting Palestinian continuous residence, self-determination rights, and indigenous status, but are structurally excluded from the reading's canonical authority — their objections are treated as denial of Jewish rights rather than as competing legitimacy claims.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_political_leadership, excluded,
    moderate, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, international_jewish_organizations).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diaspora Jewish communities dispersed across the globe into a unified political entity claiming territorial authority, transforms religious/cultural identity into national sovereignty, and creates institutional infrastructure for cultural-linguistic revival in a territorial anchor.
% TRANSFER_FUNCTION: Transfers territorial control and political authority from Palestinian Arab residents (and Ottoman/British administrative structures) to Jewish institutional leadership; transforms abstract historical claim into enforceable land ownership and governance structure; moves resources (capital, emigration, arms) from diaspora communities toward state establishment.
% ABSENT_VOICES: Palestinian Arab residents have no seat at the reading's canonical authority — their objections to displacement are not heard as legitimate territorial claims but reframed as denial of Jewish peoplehood. Arab neighboring states' geopolitical concerns about regional reorganization are subordinated to the Jewish security/rights frame. Indigenous-rights frameworks that would prioritize continuous occupation over historical connection are excluded by the reading's definitional boundaries.
% DISAPPEARANCE_RATIONALE: If the Zionist national-liberation reading and its institutional enforcement disappeared, the territorial reorganization would be immediately contested: Palestinian Arab claims to the same territory would become the default frame (they hold continuous occupation), regional geopolitics would realign without a Jewish-majority state, diaspora communities would lose a homeland anchor and revert to minority status in host nations, and Hebrew revival would persist but without territorial sovereignty backing it.
% FOUNDING_PROBLEM: Jewish communities across Europe and the Middle East faced systematic persecution, legal disabilities, pogroms, and existential threat; diaspora status created permanent vulnerability and lack of political self-determination; Jewish identity was treated as incompatible with citizenship in host nations (the 19th-century national-liberation moment).
% FOUNDING_PROBLEM_CORROBORATION: Jewish historical scholarship and advocacy institutions attest the persecution founding problem is live (contemporary antisemitism, past pogroms). Palestinian historical scholarship and international human-rights bodies attest that the founding problem (persecution) does NOT justify the enacted solution (displacement of another population) and that the reading converts a legitimate refuge problem into an illegitimate territorial claim. Academic scholarship outside both parties (comparative genocide, settler-colonial studies) disputes whether the founding problem description captures the full context or omits the pre-existing Arab presence.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the constraint's asymmetric transfer: Palestinian territorial control moves to Jewish institutional leadership without Palestinian consent; the beneficiary seats (diaspora, cultural revivalists) gain sovereignty and institutional dominance; the payer seats (Palestinian residents, neighboring states) bear permanent costs. Suppression (0.72) reflects active enforcement: the reading's core frame (Jewish historical claim supersedes Arab territorial occupation) requires continuous institutional work — delegitimizing competing historical narratives, reframing Palestinian resistance as denial rather than rights assertion, and maintaining geopolitical pressure to recognize Jewish state authority. Theater ratio (0.41) reflects moderate performative loading: security/rights justifications have become more prominent than coordination function over the interval; as settlement has hardened and resistance has intensified, the reading increasingly requires symbolic reaffirmation of the persecution-justifies-return thesis. Accessibility collapse (0.58) reflects partial-rather-than-complete foreclosure of alternatives: the reading is contestable (competing readings remain live among powerful academic and political communities); it does not collapse alternatives completely but makes them costly to hold in mainstream institutional contexts. Resistance (0.81) reflects substantial active opposition from Palestinian political leadership, Arab states, international human-rights frameworks, and academic settler-colonial studies; the reading is NOT sustained by participant preference but by active enforcement and institutional pressure. The measurement series track rising extraction (settlement expansion), rising theater (security narrative prominence), and rising suppression (enforcement intensity) across the interval — the constraint is not naturally self-sustaining but increasingly dependent on active coercive maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The national-liberation reading and the settler-colonial reading compute different types from the SAME structural data — that divergence is central to the constraint story. From the agenda_setter seat (international_jewish_organizations), the constraint appears as genuine coordination: Jewish diaspora communities are unified, cultural revival is enabled, persecution is remedied through self-determination. From the payer seats (Palestinian residents, neighboring states), the same structure appears as forced displacement and subordination: territorial claims justified by ancient history override current occupation; resistance to dispossession is delegitimized as denial of Jewish rights; enforcement is coercive, not consensual. The engine computes per-seat types from the structural data (beneficiary/victim, power atoms, exit options, enforcement mechanisms); the agenda_setter seat will compute closer to rope (coordination dominates their perception), while the payer seats compute closer to snare (extraction and suppression dominate). This divergence is not a defect — it is the core measurement the corpus exists to take: how does the same constraint look to different structural positions?
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish diaspora communities: full beneficiary direction (d near 0.0) — they gain homeland, refuge status, sovereignty claim, and cultural revival space. Their exit options (arbitrage) mean they retain optionality and can choose whether to emigrate to Israel or remain diaspora; the constraint does not trap them. Hebrew cultural revivalists: near-beneficiary (d ~0.15) — they gain institutional space but are identity-locked; their exit options are constrained by cultural-identity fusion; they benefit but cannot easily exit if they lose commitment. Palestinian Arab residents: full target direction (d near 1.0) — they lose territorial control, political authority, and are subordinated within the reading's frame. Their exit options are constrained (trapped or identity-locked by geographic and communal ties); they bear the structural cost and cannot arbitrage out. International Jewish organizations: near-beneficiary (d ~0.2) — they are the institutional beneficiaries/agenda_setters but bear reputational and political costs (international criticism, contestation); they have organized power that gives them some exit optionality (they could reframe or abandon the reading). Arab neighboring states: near-target (d ~0.75) — they absorb refugee populations and bear sovereignty complications; their exit options are constrained (trapped by regional geography); they are organized institutional actors but have limited ability to escape the constraint's regional effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish persecution, diaspora vulnerability) was historically LIVE and is CONTESTED now. The reading's founding-problem_status is 'contested' not because persecution no longer occurs (antisemitism persists) but because the relationship between the founding problem (persecution justifies refuge) and the enacted solution (territorial displacement of another population) has become incoherent. Persecution justifies REFUGE — right to a safe place. But refuge and territorial sovereignty are distinct; refuge does not automatically justify territorial establishment, especially not displacement of third parties. The reading conflates two separable problems: (1) How should persecuted diaspora communities gain security? (2) Where should they establish that security, and who bears the cost? The reading answers (1) correctly but answers (2) by declaring that historical-cultural claims to ancestral territory override current occupation and resident self-determination — a contested answer. Mandatrophy criterion: the founding problem (persecution/refuge) remains live, but the reading's articulated solution (territorial establishment via historical claim) no longer tracks the founding problem. The theater loading arises because the reading must continuously reaffirm why this particular solution (this territory, this displacement) is justified by the founding problem, when alternative solutions (diaspora integration, international protection, refuge in other territories) would also solve the founding problem without the territorial-displacement extraction. The increasing theater ratio over the interval (0.28 → 0.41) tracks the constraint's increasing departure from functional coordination toward institutional narrative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_claim_prioritization,
    'Between continuous historical-cultural claim (Jewish presence 2,000 years ago, diaspora restoration) and continuous territorial occupation (Palestinian Arab presence for centuries without significant break), which establishes indigenous status and primary territorial claim in international law?',
    'Comparative law analysis of indigenous rights frameworks (UN Declaration on Rights of Indigenous Peoples, tribunal precedents on territorial claims); determination of which temporal/presence criterion is canonical. Historical archaeology establishing continuous settlement patterns and population discontinuity or continuity.',
    'If continuous occupation is canonical, Palestinian Arabs are the indigenous people and the reading''s displacement logic is reversed. If historical-cultural claim is canonical, the reading stands. If both criteria are valid and contradictory, the constraint becomes fundamentally incoherent (both readings are partially true).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_claim_prioritization, conceptual, 'Whether historical connection or continuous occupation determines indigenous priority and legitimacy.').

omega_variable(
    persecution_justifies_displacement_logic,
    'Does victimization by persecution in one context (European antisemitism) justify displacing a third-party population (Palestinian Arabs) who did not commit the persecution? Is this a legitimate refuge right or does it require consensual territorial arrangement with the occupant population?',
    'Comparative analysis of national-liberation movements and their territorial claims; precedent review for whether persecution justifies unilateral territorial claim vs. requiring host-nation consent or negotiation. Philosophical/legal analysis of refugee rights vs. displacement rights.',
    'If persecution justifies unilateral displacement, the reading''s logic holds. If displacement requires consent/negotiation, the constraint''s enforcement becomes illegitimate coercion. If refuge rights are valid but territorial sovereignty is distinct, the reading conflates two separable problems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_justifies_displacement_logic, preference, 'Whether persecution of one group legitimizes territorial displacement of another group without consent.').

omega_variable(
    arab_opposition_delegitimization_mechanism,
    'Is Arab resistance reframed as denial of Jewish rights (delegitimizing it) a fair characterization, or is it a separate claim — Palestinian self-determination and territorial rights that coexist with Jewish persecution claims rather than contradict them?',
    'Structural analysis of the reading: does it hold that Arab objections must be false because Jewish rights are true (logical delegitimization), or does it hold that Jewish rights take precedence while Arab rights exist but are subordinated (hierarchical claim)? Examine whether the reading permits both claims to be true simultaneously.',
    'If Arab opposition is logically denied by Jewish rights (forecloses), the reading is either empirically falsifiable (both can be true) or conceptually incoherent (no one party can be fully right). If Arab rights are subordinated rather than denied, the constraint is tangled_rope with genuine conflict, not a rope with false opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_opposition_delegitimization_mechanism, conceptual, 'Whether the reading delegitimizes Arab claims or subordinates them.').

omega_variable(
    reading_vs_settler_colonial_overlap,
    'Does the national-liberation reading prevent the settler-colonial reading from being true, or are both empirically accurate descriptions of different aspects of the same constraint (persecution justifies establishment, AND establishment displaces indigenous population)?',
    'Structural analysis of axioms: if ''persecution justifies return'' and ''return involved displacing occupants'' are both true, the readings are not forecloses but coexists_with; if the readings make mutually exclusive claims about the nature of the establishment (liberation vs. colonization), determine if those claims can both be true or are truly contradictory.',
    'If coexists_with, both readings remain live and the engine reports contamination/non-purity. If forecloses, one reading logically eliminates the other (the engine''s default would be coexists_with unless the axioms are genuinely contradictory).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_settler_colonial_overlap, conceptual, 'Whether national-liberation and settler-colonial readings are logically foreclosed or can coexist as true descriptions of overlapping aspects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(zion_tr_t10, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(zion_tr_t20, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(zion_tr_t30, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(zion_be_t10, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(zion_be_t20, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(zion_be_t30, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(zion_su_t10, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(zion_su_t20, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(zion_su_t30, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_self_determination_right).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, international_refugee_protection_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'zionist_legitimacy_basis'. The settler_colonial_reading and religious_restoration_reading are sibling constraints under the same kernel; they share the same referential facts (territorial establishment, Palestinian displacement) but derive different ε values, different stakeholder positions, and different type computations because they ground their legitimacy in different foundational axioms. This reading argues legitimacy through persecution-justifies-return + historical continuity + national self-determination; the settler_colonial reading argues illegitimacy through displacement-of-occupants + power asymmetry + institutional extraction; the religious_restoration reading argues legitimacy through divine covenant + theological restoration. No single measurement reconciles them — they are genuinely different constraints. The network edges indicate structural coupling: the national_liberation_reading's institutional enforcement affects the settler_colonial_reading's observable conditions (settlement expansion validates settler-colonial thesis); the religious_restoration_reading's theoretical development influences the national_liberation_reading's legitimacy frame (post-1967 theological claims strengthen historical-restoration claims). The edges do not imply one reading is 'upstream' — all three are live and contemporaneous; they influence each other bidirectionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
