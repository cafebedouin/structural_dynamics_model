% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:coordination_type/2,
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
 *   domain: political_history/nationalism/settler_colonial_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the settler-colonial reading — of
 *   the contested kernel 'what legitimates Zionism.' Per the epsilon-referent
 *   rule, extractiveness is authored for the STANDING ARRANGEMENT (the
 *   Zionist settlement-and-sovereignty structure as it actually operates,
 *   1881 to present) as this reading assesses it: a European-origin settler
 *   movement that assembled a state through land acquisition and the
 *   displacement of Palestine's Arab population, and that maintains itself
 *   through legal and military closure of the return channel. The reading's
 *   distinctive move — displacement as constitutive rather than incidental —
 *   is carried in the metrics and in the axioms, not hedged across readings.
 *   Sibling readings (national-liberation, religious-restoration) are
 *   separate constraints with their own epsilon values; this file links them
 *   via network.affects_constraints and routes the contest into omega
 *   variables. Claim/metric independence: the claimed type (tangled_rope)
 *   asserts this reading's structural judgment that a real coordination
 *   function (refuge, state-building) and constitutive asymmetric extraction
 *   operate through the same structure; the metrics are authored
 *   descriptively, and the engine computes per-seat types independently of
 *   the claim.
 *
 * KEY AGENTS:
 *   - - jewish_settler_community: Primary beneficiary (organized/constrained) — receives land, subsidy, and security flows; bears conscription and war costs
 *   - - israeli_state_apparatus: Agenda setter (institutional/identity_locked) — administers the structure; dual-positioned as beneficiary
 *   - - palestinian_refugee_diaspora: Primary target (powerless/trapped) — bears the constitutive cost, denied return across generations
 *   - - occupied_territories_residents: Primary target (powerless/trapped) — lives under direct military administration
 *   - - palestinian_indigenous_communities: Secondary target (moderate/trapped) — split between citizenship and displacement
 *   - - diaspora_jewish_refugees: Distant beneficiary (moderate/arbitrage) — holds the refuge option without bearing daily costs
 *   - - religious_zionist_settler_movement: Beneficiary-driver (organized/identity_locked) — theological engine of expansion
 *   - - international_powers: Great-power patron (institutional/arbitrage) — supplies enforcement cover, collects alliance rents
 *   - - arab_host_states: Cost-bearing neighbor (institutional/mobile) — absorbed displacement costs, now normalizing
 *   - - binational_state_advocates: Excluded voice (moderate/constrained) — proposes the alternative structure, holds no seat
 *   - - international_legal_bodies: Analytical observer — certifies compliance gaps without enforcement power
 *   - - human_rights_organizations: Analytical observer — compiles the evidentiary record from outside the institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.84).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.85).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Legitimacy Basis — Settler-Colonial Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonial_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '30dcf602-b1e1-4ed8-841e-c0acea22ee06').
narrative_ontology:cs_kernel_codification('30dcf602-b1e1-4ed8-841e-c0acea22ee06', distributed).
narrative_ontology:cs_authority_grounding('30dcf602-b1e1-4ed8-841e-c0acea22ee06', extraction).
narrative_ontology:cs_interpretation_layer_present('30dcf602-b1e1-4ed8-841e-c0acea22ee06').
narrative_ontology:cs_reading_relation('30dcf602-b1e1-4ed8-841e-c0acea22ee06', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('30dcf602-b1e1-4ed8-841e-c0acea22ee06', zionist_legitimacy_basis__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('30dcf602-b1e1-4ed8-841e-c0acea22ee06', foundational, colonial_structure_voids_settlement_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_voids_settlement_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('30dcf602-b1e1-4ed8-841e-c0acea22ee06', colonial_structure_voids_settlement_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('30dcf602-b1e1-4ed8-841e-c0acea22ee06', secondary, self_determination_symmetry_required).
narrative_ontology:cs_axiom_status(self_determination_symmetry_required, holdable).
narrative_ontology:cs_axiom_grounding('30dcf602-b1e1-4ed8-841e-c0acea22ee06', self_determination_symmetry_required, deontological).
narrative_ontology:cs_reference_frame('30dcf602-b1e1-4ed8-841e-c0acea22ee06', indigenous_non_displacement_baseline).
narrative_ontology:cs_drift_state('30dcf602-b1e1-4ed8-841e-c0acea22ee06', contemporary_post_icj_advisory_opinion, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('30dcf602-b1e1-4ed8-841e-c0acea22ee06', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_community).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, diaspora_jewish_refugees).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_communities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, occupied_territories_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, religious_zionist_settler_movement).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, international_powers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_community).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, arab_host_states).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, sovereignty_over_return_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Jewish national community built in Palestine and its successor state. Receives state land allocation, housing subsidies, conscription-based defense, and preferential immigration under the Law of Return. Bears the arrangement's internal costs: taxation, conscription, wars, and civil obligations. Leaving is possible — tens of thousands emigrate yearly — but means abandoning homes, pensions, family networks, and a national project most members understand as their collective life.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_community, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_community, payer).

% Administers the whole structure: the land registry, the absentee-property custodian, the military government in the West Bank, the permit regime, the immigration system, and the diplomatic defense of the arrangement abroad. Its courts, schools, and information services continuously reconcile practice with the founding narrative. The state cannot step outside the arrangement without repudiating its own founding acts; its institutions have grown into the arrangement they administer. It also draws revenue, labor, and strategic depth from the territories it controls.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus, beneficiary).

% Jewish populations outside the region for whom the state functions as a guaranteed refuge — exercised massively during and after the Holocaust and the mid-century expulsions from Arab countries, and held as a standing option by communities worldwide. Those who immigrate join the settler community; those who remain hold the option at a distance, contributing donations and advocacy while bearing none of the daily costs of the arrangement.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, diaspora_jewish_refugees, beneficiary,
    moderate, generational, arbitrage, global).

% The Arab population of pre-1948 Palestine and its descendants. Roughly 150,000 remained inside the 1949 lines and became citizens — voting and taxed, but largely outside the land-allocation channels and governed for two decades by emergency regulations; their descendants are about a fifth of the citizenry. The majority were displaced: barred from returning by the absentee-property regime and successor legislation, their villages destroyed or repopulated. Exit is closed in both directions — the displaced cannot return, and citizens who leave forfeit residency rights.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_indigenous_communities, payer,
    moderate, generational, trapped, regional).

% Descendants of the displaced, concentrated in Jordan, Lebanon, Syria, and the occupied territories, with communities worldwide. Host states grant varying degrees of residence and work rights — Lebanon's camps exclude holders from many professions and from property ownership. The return channel is closed by the state that displaced them; relief registration transmits the claim across generations. No state represents them in negotiations on equal terms.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, continental).

% Palestinians of the West Bank and Gaza, governed since 1967 by military administration: separate court systems, permit requirements for work, movement, and water, fragmented jurisdictional areas, and since 2007 a blockade of Gaza. They hold no citizenship in the state that controls their borders, airspace, and tax clearance. Daily life routes through checkpoints and coordination mechanisms on which they hold no seat.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, occupied_territories_residents, payer,
    powerless, biographical, trapped, local).

% The national-religious current that after 1967 read the territorial outcome as providential and built the settlement enterprise: yeshivas, outposts, municipal blocs, and a youth pipeline into the officer corps and ministries. Members fuse personal destiny, theology, and land — abandoning the project would unravel identity, marriage networks, and vocation together. The movement supplies successive governments their most reliable coalition base and drives expansion policy from inside.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, religious_zionist_settler_movement, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, religious_zionist_settler_movement, agenda_setter).

% Great-power patrons — Britain at the Mandate stage, the United States since 1967 — that supply military aid, diplomatic vetoes, and financial backing, and receive in return strategic alignment, intelligence cooperation, and basing access. They shaped the arrangement's founding instruments (Balfour, Partition, Camp David) and police its boundaries when convenient. Support is revisable administration by administration, and each patron retains the option to reroute its investment.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, international_powers, agenda_setter).

% Neighboring states that absorbed the displaced: Jordan granted citizenship to most, Lebanon and Syria confined refugees to camps, Egypt governed Gaza from 1949 to 1967. They bore decades of camp costs, border wars, and internal instability traceable to the displacement. Several have since converted the relationship into transactional normalization agreements, trading recognition for economic and security returns.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, arab_host_states, payer,
    institutional, generational, mobile, regional).

% Advocates of single-state equality — Palestinian federalists, joint civic movements, diaspora intellectuals — who propose replacing communal sovereignty with equal citizenship. Excluded from the Oslo architecture, marginal in both polities' elections, and restricted at the margins by anti-normalization and boycott statutes. Their proposals circulate in journals and conferences without a negotiating seat.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, binational_state_advocates, excluded,
    moderate, generational, constrained, global).

% The ICJ, ICC, treaty bodies, and General Assembly machinery that issue advisory opinions, open investigations, and pass resolutions measuring the arrangement against the Geneva Conventions and related instruments. They compile the record and certify violations but hold no enforcement power; their findings feed domestic litigation and sanction debates elsewhere.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Israeli, Palestinian, and international monitoring organizations that document land takeover, demolition counts, permit denials, and casualty data. Several have been designated or banned domestically; they publish from outside the arrangement's institutions and supply the evidentiary base on which other seats act.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, human_rights_organizations, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, jewish_settler_community).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the collective-action problem of assembling a dispersed, persecuted population into a defensible sovereign community: coordinated immigration, land acquisition and allocation, Hebrew language revival, defense forces, water and energy infrastructure, and state institutions — problems no household or congregation could solve alone.
% TRANSFER_FUNCTION: Moves land, water, and sovereign authority from Palestinian inhabitants to the Jewish national collective; places the labor, customs revenue, and mobility of occupied residents under the controlling state's administration; moves diaspora donations and great-power military and diplomatic capital into the settler-state economy.
% ABSENT_VOICES: The displaced themselves: the villages emptied in 1948 held no seat at Lausanne, and the refugees were excluded from the Oslo architecture; binational-equality advocates are marginal in both polities; the surrounding Arab populations were present as states, never as the displaced. Dissent existed at every juncture — Brit Shalom's binational proposals, Ahad Ha'am's early warnings, Palestinian leadership's rejection memoranda — but none of it held decision power at a founding moment.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would dissolve a state of roughly ten million people, strand its institutions, reopen the refuge question for world Jewry, and place the displaced and their descendants in a radically altered legal landscape. The region's state system, the great-power alliance structure, and the domestic politics of every Western democracy would rearrange around the vacuum.
% FOUNDING_PROBLEM: European antisemitism and the demonstrated failure of emancipation to protect Jewish life — Herzl's diagnosis after the Dreyfus affair — later catastrophically confirmed by the Holocaust, and extended by the mid-century expulsion of Jewish communities from Arab and Muslim countries.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the documentary record of European antisemitism (pogrom archives, Vichy and Reich deportation records) and of the twentieth-century expulsions from Arab states is maintained by historians of every nationality, including Palestinian and Arab historiography, which attests the persecution while disputing the remedy chosen. Contemporary antisemitism monitoring by European Union agencies and civil-society bodies independent of Israeli institutions confirms the underlying problem persists.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored high (0.84 at interval end) because this reading treats displacement as constitutive: land transfer, absentee-property seizure, settlement expansion, and denial of return are not side effects of a coordination scheme but its operating method. Suppression (0.85) reflects the enforcement stack — military occupation, permit and checkpoint regimes, administrative detention, and the legislated closure of the return channel — that the structure requires to hold. Theater (0.44) tracks the growing share of activity devoted to narrating the arrangement (public-diplomacy apparatus, peace-process choreography, democratic self-presentation) relative to its substantive operation. The three series share one time grid (eleven points spanning 1881–2024). The trajectory is cyclical rather than monotonic: each crisis (1929, 1936–39, 1948, 1987, 2000–05, 2023–) ends with the structure enlarged — more land controlled, more restrictions institutionalized — so the Oslo dip (1993) marks partial relaxation followed by steeper accumulation. The oscillation functions as a ratchet: each uprising supplies the security justification for the next layer of control, making the cycle itself part of the accumulation mechanism rather than noise around it. Suppression_requirement is tracked because enforcement capacity visibly changed across the interval — from private guards under Ottoman rule, through Mandate-backed enforcement, to mass mobilization in 1948, to a permanent occupation administration — which is exactly the enforcement-infrastructure change the scalar base_properties.suppression cannot represent.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and target seats should compute opposite types from identical structural data. From the settler-community seat the arrangement is the coordination mechanism that made survival possible — refuge, defense, statehood — and its costs read as the price of that coordination. From the refugee-diaspora and occupied-resident seats the same institutions read as elimination: every structure that shelters one population operates on the other as dispossession. The diaspora seat experiences the arrangement as optionality — an insurance policy held at distance — while the great-power seat experiences it as a strategic asset yielding alliance rents. A coalition question sits underneath the seat structure: the three Palestinian seats are fragmented across different legal regimes (citizenship, camp registration, military occupation), and that fragmentation is itself maintained by the arrangement — the differing statuses prevent the coalition formation that pooled power would otherwise permit. The engine computes these per-seat classifications from power, exit, and directional position; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward zero for the settler community, the state apparatus, the diaspora, the religious-settler movement, and the great powers; victim declarations drive d toward one for the three Palestinian seats. Exit modulation does the discriminating work: the refugee diaspora and occupied residents are trapped (no return channel, no sovereign port, permit regimes), placing them near the full-target end; diaspora Jews hold arbitrage-grade exit — the refuge is an option they exercise or decline — pinning them near the beneficiary end despite shared ethnicity with the settler seat; the great powers hold arbitrage, as shifting administrations demonstrate. Identity-lock deepens two beneficiary-side positions rather than moderating them: the state apparatus has institutionally fused with the arrangement it administers (it cannot exit without repudiating its founding acts), and the religious-settler movement fuses theology, livelihood, and land. Spatial scope amplifies effective extraction for the trapped targets — the refugee diaspora's continental dispersion with closed return makes verification of its condition hardest precisely where extraction is deepest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so this is not a resolved-mandatrophy case in the classic sense: the arrangement still performs its founding function — sheltering persecuted Jews — which is exactly why the pure-extraction reading fails and why the tangled-rope claim carries the coordination gate. The mandatrophy risk runs in the opposite direction from the usual one: the live founding problem functions as a permanent shield against obsolescence questions, because a real refuge need exempts the specific structure (displacement-constituted sovereignty) from the sunset scrutiny a dead mandate would trigger. The classification keeps both truths load-bearing: the coordination function is real for the beneficiary seats, and the extraction is constitutive, not incidental, for the target seats. On the mismatch consumer: founding_problem_status=live combined with disappearance_verdict=world_rearranges raises no zombie flag — the arrangement does what it was built to do — but the refuge-substitutability omega keeps open the question that would convert this into a mandatrophy case: if the refuge function were shown to be achievable without the displacement-constituted form, the structure's persistence past its necessity would become the piton-and-sunset question the corpus should continue to ask.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel zionist_legitimacy_basis: does the colonial-structure determination correctly identify the arrangement''s operative structure, or do the national-liberation or religious-restoration readings?',
    'Comparative structural analysis across the three readings: metropole presence, continuity of the returning population with prior residence, the mechanism of land transfer, and which reading better predicts the arrangement''s actual enforcement behavior over the interval.',
    'If the national-liberation reading prevails, epsilon collapses toward coordination-cost levels and the victim/beneficiary asymmetry inverts; if the religious-restoration reading prevails, the referent shifts to a theological-obligation structure this file does not model.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the legitimacy kernel correctly fixes the constraint''s structure.').

omega_variable(
    constitutive_vs_incidental_displacement,
    'Is the displacement of 1948 constitutive of the Zionist project (transfer as planned method) or incidental (a war''s refugee flow that policy then declined to reverse)?',
    'Historiographic resolution: the Transfer Committee papers, the Plan Dalet drafting record, Ben-Gurion''s correspondence, and the sequencing of land acquisition relative to demonstrated military necessity.',
    'If incidental, the arrangement is a tangled rope with a remediable extraction component that return-plus-compensation could close; if constitutive, the structure trends snare and no compensation package touches the core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_incidental_displacement, empirical, 'Whether displacement was the project''s method or its byproduct.').

omega_variable(
    metropole_typology_fit,
    'Classical settler colonialism presupposes a metropole; Zionism had none before 1948, and Britain''s Mandate role is contested between sponsorship and constraint. Does the settler-colonial genus fit this case, or does the deviation require a distinct category?',
    'Comparative typology against metropole and non-metropole cases (French Algeria, Australia, Liberia) testing which structural features — not the label — carry the predictive weight.',
    'If the genus misfits, the foundational axiom loses its empirical anchor and this reading''s pressure on the national-liberation sibling weakens; if it fits, the colonial-structure determination stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_typology_fit, conceptual, 'Whether ''settler colonialism'' is the correct structural genus for this case.').

omega_variable(
    refuge_substitutability,
    'Does the live founding problem (Jewish persecution) require THIS structure specifically, or is the refuge function substitutable by other arrangements — pluralist democracies, binding international guarantees?',
    'Counterfactual policy analysis: comparative safety outcomes for diaspora populations with and without a sovereign refuge, and the historical track record of alternative protective regimes.',
    'If substitutable, the structure''s persistence past its necessity becomes the mandatrophy question and sunset scrutiny engages; if not, the coordination leg of the tangled rope stays load-bearing indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refuge_substitutability, empirical, 'Whether the refuge function uniquely requires the displacement-constituted structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 0, 143).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(zion_tr_t24, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(zion_tr_t41, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 41, 0.15).
narrative_ontology:measurement(zion_tr_t55, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 55, 0.18).
narrative_ontology:measurement(zion_tr_t67, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 67, 0.22).
narrative_ontology:measurement(zion_tr_t86, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 86, 0.25).
narrative_ontology:measurement(zion_tr_t106, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 106, 0.28).
narrative_ontology:measurement(zion_tr_t112, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 112, 0.35).
narrative_ontology:measurement(zion_tr_t122, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 122, 0.4).
narrative_ontology:measurement(zion_tr_t136, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 136, 0.42).
narrative_ontology:measurement(zion_tr_t143, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 143, 0.44).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(zion_be_t24, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement(zion_be_t41, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 41, 0.42).
narrative_ontology:measurement(zion_be_t55, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 55, 0.5).
narrative_ontology:measurement(zion_be_t67, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 67, 0.72).
narrative_ontology:measurement(zion_be_t86, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 86, 0.76).
narrative_ontology:measurement(zion_be_t106, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 106, 0.75).
narrative_ontology:measurement(zion_be_t112, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 112, 0.72).
narrative_ontology:measurement(zion_be_t122, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 122, 0.79).
narrative_ontology:measurement(zion_be_t136, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 136, 0.81).
narrative_ontology:measurement(zion_be_t143, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 143, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(zion_su_t24, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(zion_su_t41, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 41, 0.3).
narrative_ontology:measurement(zion_su_t55, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 55, 0.48).
narrative_ontology:measurement(zion_su_t67, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 67, 0.62).
narrative_ontology:measurement(zion_su_t86, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 86, 0.66).
narrative_ontology:measurement(zion_su_t106, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 106, 0.7).
narrative_ontology:measurement(zion_su_t112, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 112, 0.64).
narrative_ontology:measurement(zion_su_t122, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 122, 0.76).
narrative_ontology:measurement(zion_su_t136, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 136, 0.8).
narrative_ontology:measurement(zion_su_t143, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 143, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Zionism's legitimacy' decomposes into three structurally distinct claims with different epsilon values — this file (settler_colonial_reading, epsilon approximately 0.84: colonial structure determines legitimacy, displacement constitutive), the national-liberation reading (epsilon near the coordination floor: persecution response with wartime displacement treated as incidental), and the religious-restoration reading (referent shifts to theological obligation). Upstream/downstream: the national-liberation narrative was the historically dominant legitimation and is the primary object of this reading's critique; the religious reading gained institutional force after 1967 and is pressured, not foreclosed, by this reading's reframing of the settlement enterprise as colonial expansion. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
