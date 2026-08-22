% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionist Settlement Arrangement as Displacement Regime (Settler-Colonial Reading)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the settler_colonial_reading of the kernel
 *   jewish_sovereignty_palestine: on this reading, the Zionist settlement of
 *   Palestine reproduces the European settler-colonial pattern - immigration,
 *   land acquisition, and institution-building that jointly operated as a
 *   displacement regime for the existing Arab society, with the 'regardless
 *   of intent' clause holding that refugee-status motivation does not alter
 *   the structural characterization. The referent of epsilon is the STANDING
 *   ARRANGEMENT - the territorial-sovereignty structure built by the Yishuv
 *   and consolidated by the State of Israel, including its land-transfer,
 *   citizenship-allocation, and occupation machinery - assessed by this
 *   reading's own lights. Per the epsilon-invariance principle, the four
 *   sibling readings (liberal_nationalist, religious_zionist,
 *   cultural_zionist, post_zionist) are separate constraint files over the
 *   same referent with their own epsilon values, beneficiary/victim
 *   structures, and classifications; they are linked via
 *   network.affects_constraints and are NOT averaged into this file.
 *   CONSTRAINT FAMILY NOTE: this file's epsilon (0.86) is reading-indexed - a
 *   liberal_nationalist file over the identical referent will author
 *   materially lower epsilon because it does not count the sovereignty
 *   transfer as extraction; the divergence between those two numbers is
 *   precisely the measurement the kernel family exists to take. KEY AGENTS
 *   (by structural relationship): - zionist_institutional_leadership:
 *   agenda-setter (institutional/identity_locked) - plans, administers, and
 *   enforces the settlement and displacement machinery -
 *   israeli_jewish_citizenry: primary beneficiary (powerful/identity_locked)
 *   - receives land, housing, citizenship, sovereignty -
 *   world_jewish_diaspora_communities: secondary beneficiary
 *   (organized/identity_locked) - collects refuge-optionality and identity
 *   anchoring - british_imperial_establishment: Mandate-era beneficiary and
 *   co-agenda-setter (institutional/arbitrage) - collected strategic
 *   position, exited in 1948 - us_hegemonic_establishment: post-1967
 *   beneficiary (institutional/arbitrage) - collects alliance rents at
 *   client-subsidized cost - palestinian_pre_1948_landholders: primary victim
 *   (powerless/trapped) - bore the founding displacement -
 *   palestinian_refugee_diaspora: primary victim (powerless/trapped) - bears
 *   the intergenerational cost of denied return -
 *   palestinian_citizens_of_israel: victim (moderate/constrained) - formal
 *   inclusion, structural subordination - occupied_territories_residents:
 *   victim (powerless/trapped) - governed without citizenship by the state
 *   extracting from them - neighboring_arab_states: excluded voice
 *   (organized/constrained) - never consulted at any founding juncture -
 *   un_international_legal_bodies: analytical observer
 *   (institutional/analytical) - revisionist_historians: analytical observer
 *   (moderate/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.86).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.82).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionist Settlement Arrangement as Displacement Regime (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '65c6c67b-97e7-4105-b045-19d8c0737c66').
narrative_ontology:cs_kernel_codification('65c6c67b-97e7-4105-b045-19d8c0737c66', distributed).
narrative_ontology:cs_authority_grounding('65c6c67b-97e7-4105-b045-19d8c0737c66', distributed).
narrative_ontology:cs_reading_relation('65c6c67b-97e7-4105-b045-19d8c0737c66', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('65c6c67b-97e7-4105-b045-19d8c0737c66', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('65c6c67b-97e7-4105-b045-19d8c0737c66', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('65c6c67b-97e7-4105-b045-19d8c0737c66', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('65c6c67b-97e7-4105-b045-19d8c0737c66', foundational, settlement_constitutes_displacement_regardless_of_intent).
narrative_ontology:cs_axiom_status(settlement_constitutes_displacement_regardless_of_intent, holdable).
narrative_ontology:cs_axiom_grounding('65c6c67b-97e7-4105-b045-19d8c0737c66', settlement_constitutes_displacement_regardless_of_intent, empirically_contingent).
narrative_ontology:cs_axiom('65c6c67b-97e7-4105-b045-19d8c0737c66', foundational, displacement_voids_sovereign_title).
narrative_ontology:cs_axiom_status(displacement_voids_sovereign_title, holdable).
narrative_ontology:cs_axiom_grounding('65c6c67b-97e7-4105-b045-19d8c0737c66', displacement_voids_sovereign_title, deontological).
narrative_ontology:cs_reference_frame('65c6c67b-97e7-4105-b045-19d8c0737c66', indigenous_palestinian_tenure_baseline).
narrative_ontology:cs_drift_state('65c6c67b-97e7-4105-b045-19d8c0737c66', contemporary_post_nakba_consolidation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('65c6c67b-97e7-4105-b045-19d8c0737c66', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, israeli_jewish_citizenry).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, world_jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_establishment).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_hegemonic_establishment).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_pre_1948_landholders).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, occupied_territories_residents).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, settler_colonial_elimination_logic).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, structural_over_intent_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The World Zionist Organization, Jewish Agency, Jewish National Fund, and after 1948 the Government of Israel: plan and execute immigration waves, purchase and register land, build parallel state institutions (Histadrut, Haganah, Hebrew University, water and electricity networks), and after sovereignty wield state law (Absentees' Property Law, Land Acquisition Law, citizenship and admissions committees) and military force to consolidate the territorial outcome. Administer the diaspora fundraising apparatus that finances the project. Exit would mean dissolving the project they constitute - the leadership's professional and national identity is fused with the arrangement's continuation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, zionist_institutional_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive citizenship, land, housing, employment preference, and the protection of a sovereign state apparatus built on the transferred territorial base. Most participate in the national project as voters, soldiers, and settlers of the interior. Identity is fused with the state - exit means emigrating away from the collective self. They also bear real costs the arrangement generates: universal conscription, war exposure, and international censure, which they experience as the price of holding the gains rather than as extraction.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, israeli_jewish_citizenry, beneficiary,
    powerful, biographical, identity_locked, national).

% Fund the project through federations and philanthropy, supply immigrants, and hold the state as refuge-insurance and identity anchor. What flows to them is optionality (an exit from persecution elsewhere) and meaning (the center of collective identity), not land. Their identification makes criticism of the arrangement feel like self-attack, which locks their interpretive position.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, world_jewish_diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).

% Issued the Balfour Declaration in 1917 and drafted the Mandate's terms, administering Palestine 1920-1948: secured the eastern Mediterranean land bridge and Suez approaches at low cost by sponsoring a loyal immigrant population. Suppressed the Arab Revolt 1936-39 using the arrangement's enforcement needs. Exited entirely in 1948 when the maintenance cost exceeded the strategic return - the defining arbitrage exit, demonstrating that the metropole's position was always revocable.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_establishment, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_establishment, agenda_setter).

% Post-1967 patron: military aid, diplomatic shielding in the Security Council, intelligence cooperation, and joint doctrine development. The alliance projects American power in the region at client-subsidized cost and anchors a wider alignment system. Exit would require rewriting a core alliance commitment and conceding regional influence - structurally available but politically expensive, keeping the seat near the beneficiary end with retained arbitrage capacity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_hegemonic_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Tenant farmers, orchard owners, and townspeople who held the land before large-scale immigration: displaced through purchase-evictions, tenancy terminations, wartime flight and expulsion in 1947-49. Lost title, homes, orchards, and livelihoods; those who remained became a minoritized remnant, those who fled became refugees. Had no channel to contest the decisions that disposed of their position - the Balfour Declaration, the Mandate terms, and the partition plan were all taken without their consent.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_pre_1948_landholders, payer,
    powerless, biographical, trapped, regional).

% Approximately 700,000 displaced in 1947-49 and further hundreds of thousands in 1967, now dispersed across camps and cities in Lebanon, Syria, Jordan, the West Bank, Gaza, and further afield. Denied return by the state that succeeded to their property, which passed to the Custodian of Absentee Property and thence to Jewish national institutions. Refugee status transmits across generations through UNRWA registration - the trap is inherited, not chosen, and no host state offers integration equal to return.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% The roughly 150,000 who remained in 1949 and their descendants (~20% of the state's citizens): lived under military government until 1966, hold formal citizenship with the vote, but face structural disadvantage in land allocation (JNF charter restrictions, admissions committees), municipal budgets, planning permits, and the 2018 Nation-State Law's constitutional downgrading. Exit exists formally - emigration - but means abandoning homeland and community, so the constraint binds through belonging rather than barbed wire.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% West Bank, East Jerusalem, and Gaza populations governed since 1967 by military administration, settlement encirclement, permit regimes, separation barriers, house demolitions, and periodic large-scale military operations. East Jerusalem residents hold revoked-or-conditional residency rather than citizenship; West Bank residents are tried in military courts while adjacent settlers are tried in civilian courts. Governed in the territory where they live by a state in which they hold no citizenship.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, occupied_territories_residents, payer,
    powerless, biographical, trapped, regional).

% Were not consulted at any founding juncture - Balfour, the Mandate instrument, and the partition vote were decided by external powers over recorded Arab objection. Responded with war (1948, 1967, 1973), embargo, and boycott; absorbed refugee populations at lasting domestic and fiscal cost; and later normalized piecemeal (Egypt 1979, Jordan 1994, Abraham Accords 2020) as facts hardened beyond reversal. Their early absence from the conversation is the canonical case of the arrangement's unanimity being manufactured by exclusion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, neighboring_arab_states, excluded,
    organized, generational, constrained, continental).

% Record the arrangement against international-law benchmarks: partition resolution 181, resolution 194 on return, the Fourth Geneva Convention's applicability to the territories, Security Council resolutions 242 and 338, ICJ advisory opinions and wall ruling. Possess recording and legitimation power but no enforcement capacity against the arrangement's state holder.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, un_international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Archival researchers - the Israeli New Historians, Palestinian scholars, and international colleagues - who documented Plan Dalet, village-depopulation records, land-transfer files, and refugee-count controversies from declassified archives. Their findings supply this reading's empirical base and are contested by nationalist historiography on all sides; their seat is analytical, with career and reputational stakes in the contest they document.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, revisionist_historians, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__settler_colonial_reading, israeli_jewish_citizenry).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a real collective-action problem for a persecuted, stateless diaspora: coordinated immigration logistics, land acquisition and registration, institution-building, defense organization, and eventually refuge and statehood for a population facing recurring European violence culminating in the Holocaust. Stated without evaluation: the arrangement did coordinate a population's survival and nation-building.
% TRANSFER_FUNCTION: Moves land, housing, agricultural property, and ultimately sovereign political authority from Palestine's Arab inhabitants to the incoming and succeeding Jewish society - via purchase and tenancy termination (pre-1948), wartime displacement and absentee-property law (1948), and occupation-plus-settlement (post-1967) - with a secondary stream of strategic rents flowing to the sponsoring metropoles (British Mandate position, then the American alliance).
% ABSENT_VOICES: Palestinian Arabs were absent from every founding decision: the Balfour Declaration (1917) was issued without consulting the roughly 90% Arab majority; the Mandate was ratified over documented Arab objection; the partition vote was carried by external powers. The neighboring Arab states entered the conversation only after facts were created, and the refugee population - the party with the largest outstanding claim - has never held a seat in any final-status negotiation. Where they would be: outside the room, in camps and diaspora, represented intermittently by proxies who do not answer to them.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would trigger massive rearrangement: a sovereignty vacuum over the territory, attempted refugee return of millions, immediate regional war and great-power intervention, collapse of the American alliance system in the region, an identity crisis across world Jewish communities for whom the state is the collective anchor, and renegotiation of every water, energy, and security arrangement in the Levant. Nothing in the current regional architecture is independent of this arrangement.
% FOUNDING_PROBLEM: European Jewish statelessness and persecution: a people dispersed across polities that periodically turned murderous, with no sovereign capacity to admit, protect, or concentrate them - the arrangement was built to solve Jewish insecurity through territorial concentration and self-governance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's existence is corroborated far outside the beneficiary set: mainstream Holocaust historiography, European antisemitism-monitoring bodies (OSCE/ODIHR incident data), and the archival record of pre-war exclusionary regimes all attest the insecurity was real and lethal. But the claim that THIS arrangement remains the necessary or sufficient solution is attested almost exclusively by the beneficiary coalition (Israeli state institutions, major diaspora organizations); the four Palestinian victim seats and much of the postcolonial scholarship attest instead that the solution-mode generated an unresolvable counter-problem. The corroboration asymmetry - problem corroborated externally, solution attested only internally - is itself signal.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.86) because the arrangement's core operation is zero-sum territorial transfer: land, homes, property, and ultimately sovereign authority moved from one people to another, with the 1947-49 displacement as the founding transfer and post-1967 settlement as its continuation. Suppression is high (0.82) and is authored as a RAW STRUCTURAL PROPERTY - the engine scales only extractiveness by directionality and scope; the raw figure reflects denial of return, absentee-property and land-acquisition legislation, military government (1948-1966), permit regimes, house demolitions, and administrative detention. Theater ratio is moderate (0.42): the state's civic, economic, and refuge functions are real and load-bearing, but a large share of legitimation activity - democratic self-presentation over an ethnocratic allocation structure, 'making the desert bloom' erasure narratives, peace-process choreography that ran parallel to settlement expansion - is performative maintenance of the arrangement's acceptability. Accessibility collapse is moderate (0.55): binational, federal, and cultural-autonomy alternatives were proposed at multiple junctures and were foreclosed in practice once the demographic-sovereignty trajectory locked in, yet they persist as live discursive and diplomatic alternatives, so collapse is incomplete. Resistance is high (0.8): the arrangement has faced continuous organized resistance across the full interval - the 1936-39 revolt, 1948 and subsequent wars, two intifadas, BDS and UN diplomacy - plus internal documentary resistance from revisionist historians. MEASUREMENT GRID: all three tracked series run on ONE SHARED GRID of ten points (t=0 First Aliyah era 1881; t=16 Basel Congress 1897; t=36 Balfour Declaration 1917; t=55 Arab Revolt 1936; t=67 Nakba/war 1948; t=86 Six-Day War 1967; t=96 Likud settlement acceleration 1977; t=112 Oslo 1993; t=136 annexation-momentum era 2017; t=145 present 2026), so every metric is authored at every examined time point. CYCLICAL PATTERN: the series oscillate around a rising ratchet - revolt/crisis, suppression, relaxation, renewed accumulation (1936-39, 1948, 1987-93, 2000-05) - and the oscillation is partly the extraction mechanism itself: each security crisis justifies the next round of land consolidation ('facts on the ground'), so calm phases are accumulation phases, not recovery phases. The Oslo dip in extractiveness (t=112) and suppression is real but partial: autonomy was transferred over pockets while settlement construction accelerated, and the ratchet resumed. Base-properties scalars reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from identical structural data. From the israeli_jewish_citizenry seat, the arrangement presents as a rope that worked: a persecuted diaspora's collective-action problem (statelessness, extermination) was solved, refuge was delivered, institutions function - the extraction is invisible from inside because the gains are the background condition of ordinary life. From the four Palestinian payer seats, the same structure presents as a snare: every founding decision was taken without them, exit was closed by force and law, and the enforcement machinery exists to maintain their exclusion. From the zionist_institutional_leadership seat, the arrangement is coordination-plus-defense: administration of a national project under existential siege. From the metropole seats (Britain, then the United States), it was and is a cheap strategic asset - the arbitrage exit defines their relationship: they can and did walk away when the price rose. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it. SAME-LEVEL DYNAMICS: the four Palestinian seats hold similar nominal power yet occupy differentiated exit positions - trapped (refugees, occupied residents, pre-1948 holders) versus constrained (citizens of Israel, who hold formal mobility but exit means abandoning homeland entirely) - and this differentiation is itself a stability mechanism: segmenting the victim population into juridically distinct classes (refugee, citizen, resident) fragments coalition formation, which the COALITION CHECK flags as the principal reason a nominally powerless victim class has not converted demographic parity into bargaining power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The israeli_jewish_citizenry sits near the full-beneficiary end (d near 0.0): the constraint subsidizes them with land, housing, and sovereign protection, and their identity_locked exit amplifies the subsidy's grip - they cannot cash out. The diaspora communities collect optionality and meaning rather than land, so their d sits slightly above the citizenry's but still firmly on the beneficiary side. The metropole seats derive low d from their beneficiary declarations plus arbitrage exit - Britain demonstrated the exit in 1948, and the United States retains it structurally, which is why their effective extraction contribution is dampened despite their agenda-setting influence in the Mandate period. The four Palestinian seats derive high d: trapped exit places refugees, occupied residents, and pre-1948 holders near the full-target end (d approaching 1.0), with the refugee diaspora the extreme case - three generations of inherited non-return. Palestinian citizens of Israel derive slightly lower d than the trapped seats because constrained (not trapped) exit and formal civic membership modulate their target position. The agenda-setting leadership derives a mid-to-low d reflecting its dual position: it administers the machinery AND collects through it (land trusts, diaspora fundraising, state revenue), which is why no directionality override was needed - the structural data already captures the setter-as-collector fusion. Scope amplification applies: the arrangement's national-to-global scope makes verification of extraction conditions harder and scales effective extraction upward for the trapped target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters acutely here because both mislabeling directions are politically weaponized. Labeling the arrangement a pure snare erases the genuine coordination function the reading itself concedes - a persecuted, stateless diaspora DID solve a real collective-action problem, and pretending otherwise hands the beneficiary coalition a refutation. Labeling it a pure rope erases the structural displacement the same reading documents - pretending the refuge function launders the transfer mechanism. Tangled_rope holds both: coordination and extraction operate through the SAME structure (the immigration-and-land apparatus that rescued European Jews is the identical apparatus that dispossessed Palestinian Arabs), held together by active enforcement. On the R5 genealogy interview: the founding problem (European Jewish statelessness and persecution) was real, externally corroborated, and partially solved - but the solution's mode generated a counter-problem (Palestinian dispossession) that is now the arrangement's principal legitimacy liability, and the founding problem's status is contested rather than dead because the beneficiary coalition attests its persistence (antisemitism surveillance data corroborates the underlying insecurity even outside the beneficiary set). The mismatch consumer should note: founding_problem_status=contested with verdict=world_rearranges is the honest cell here - the world HAS rearranged around this arrangement, and the parties genuinely dispute whether the founding problem still lives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the settler_colonial_reading of the kernel jewish_sovereignty_palestine; four sibling readings (liberal_nationalist, religious_zionist, cultural_zionist, post_zionist) instantiate different constraints over the same referent with different victim sets, beneficiary sets, and epsilon values. Which reading''s structural description is adopted changes the classification wholesale - so what work is done by adopting THIS reading rather than a sibling?',
    'Cross-reading comparison within the kernel family: compile all five sibling stories and compare computed types, victim sets, and effective extraction over the identical referent. The disagreement is located specifically in the genesis question (national self-determination realized through displacement vs. displacement regime regardless of intent) and in who counts as victim.',
    'If the liberal_nationalist reading is adopted, the victim arrays empty out and the type migrates toward rope/scaffold; if this reading is adopted, the tangled_rope-vs-snare boundary is the live question. No single-file resolution exists - the indexical choice IS the classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame omega: this constraint is one reading of a five-reading kernel; sibling readings are separate constraint files, not hedges inside this one.').

omega_variable(
    intent_irrelevance_clause,
    'The reading''s ''regardless of intent'' clause treats refugee-status motivation (flight from European persecution) as structurally irrelevant to the displacement characterization. Is that clause sound, or does settler intent modulate the constraint''s structure?',
    'Comparative settler-colonial analysis: test whether arrangements with comparable displacement but persecuted-refugee settler origins (vs. profit-motivated settlers) exhibit different structural profiles - enforcement intensity, alternative suppression, extraction durability.',
    'If intent is structurally relevant, part of the measured extraction reclassifies as the price of refuge-coordination and the rope component strengthens; if irrelevant (Wolfe''s structure-over-intent thesis), the extraction stands unmuted and the snare boundary stays live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_irrelevance_clause, conceptual, 'Whether the ''regardless of intent'' clause is a structural finding or a normative stipulation.').

omega_variable(
    metropole_benefit_attribution,
    'How much of the arrangement''s persistence is attributable to imperial/hegemonic sponsorship (Britain 1917-1948, the United States thereafter) versus endogenous settler-society drive?',
    'Counterfactual sponsorship analysis: examine junctures where metropole pressure and settler initiative diverged (1939 White Paper, 1956 Suez, 1991 Madrid/Oslo sequencing) and measure whether the displacement regime advanced, paused, or reversed when metropolitan interests shifted.',
    'If metropole benefit is load-bearing, the colonial-metropole beneficiary declaration is central and the arrangement reads as externally sponsored extraction; if marginal, beneficiaries contract to the settler society and the frame shifts toward an endogenous ethno-national conflict with different network edges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_benefit_attribution, empirical, 'Weight of metropolitan sponsorship in sustaining the displacement regime.').

omega_variable(
    binational_alternative_foreclosure,
    'Were viable non-displacing alternatives available at the decisive junctures (Brit Shalom''s binational proposals of the 1920s-30s, the 1947 minority report, federal schemes), such that displacement was chosen rather than forced?',
    'Historical counterfactual reconstruction: assess the demographic, diplomatic, and military feasibility of each binational/federal proposal at its own juncture, using archival records of why each was rejected and by whom.',
    'If viable alternatives existed and were rejected, accessibility_collapse is a choice artifact and the extraction is fully attributable to the arrangement''s agents; if no alternative was feasible given regional rejectionism, part of the measured extraction is the tragic price of the constraint space itself, softening the snare reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(binational_alternative_foreclosure, conceptual, 'Counterfactual viability of non-displacing coordination alternatives.').

omega_variable(
    nakba_mechanism_composition,
    'What was the actual composition of the 1947-49 displacement - planned expulsion (Plan Dalet execution), wartime flight, panic contagion, and elite flight - and in what proportion?',
    'Village-by-village archival reconstruction (declassified IDF archives, Haganah files, British Mandate records, oral histories): classify each depopulated locality by dominant mechanism and build the aggregate distribution.',
    'A high planned-expulsion proportion confirms the displacement regime as designed rather than emergent, raising effective extraction and pushing toward snare; a high flight proportion supports an emergent-tragedy reading with lower attributional weight on the arrangement''s agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nakba_mechanism_composition, empirical, 'Empirical composition of the founding displacement event.').

omega_variable(
    annexation_drift_toward_snare,
    'Is the contemporary trajectory (settlement expansion, the 2018 Nation-State Law, annexation initiatives, de facto permanent occupation) moving the arrangement from tangled_rope across the snare boundary by extinguishing the residual coordination component?',
    'Longitudinal tracking of the coordination residue: measure whether citizenship-pathway, resource-sharing, or mutual-recognition functions still operate for any Palestinian seat, or whether all four victim seats now sit in pure extraction positions with no coordinated benefit stream.',
    'If the coordination residue reaches zero for all victim seats, the computed type flips to snare and the tangled_rope claim becomes a false-cover detection; if residue persists, the hybrid classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annexation_drift_toward_snare, empirical, 'Whether current trajectory extinguishes the residual coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 0, 145).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jewi_tr_t16, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(jewi_tr_t36, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 36, 0.18).
narrative_ontology:measurement(jewi_tr_t55, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 55, 0.24).
narrative_ontology:measurement(jewi_tr_t67, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 67, 0.27).
narrative_ontology:measurement(jewi_tr_t86, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 86, 0.31).
narrative_ontology:measurement(jewi_tr_t96, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 96, 0.35).
narrative_ontology:measurement(jewi_tr_t112, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 112, 0.42).
narrative_ontology:measurement(jewi_tr_t136, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 136, 0.4).
narrative_ontology:measurement(jewi_tr_t145, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 145, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(jewi_be_t16, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 16, 0.3).
narrative_ontology:measurement(jewi_be_t36, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 36, 0.44).
narrative_ontology:measurement(jewi_be_t55, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 55, 0.57).
narrative_ontology:measurement(jewi_be_t67, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 67, 0.81).
narrative_ontology:measurement(jewi_be_t86, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 86, 0.83).
narrative_ontology:measurement(jewi_be_t96, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 96, 0.85).
narrative_ontology:measurement(jewi_be_t112, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 112, 0.82).
narrative_ontology:measurement(jewi_be_t136, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 136, 0.85).
narrative_ontology:measurement(jewi_be_t145, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 145, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(jewi_su_t16, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(jewi_su_t36, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 36, 0.35).
narrative_ontology:measurement(jewi_su_t55, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 55, 0.55).
narrative_ontology:measurement(jewi_su_t67, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 67, 0.78).
narrative_ontology:measurement(jewi_su_t86, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 86, 0.8).
narrative_ontology:measurement(jewi_su_t96, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 96, 0.82).
narrative_ontology:measurement(jewi_su_t112, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 112, 0.76).
narrative_ontology:measurement(jewi_su_t136, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 136, 0.84).
narrative_ontology:measurement(jewi_su_t145, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 145, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, resource_allocation).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language label 'Zionism/the Jewish sovereignty question' covers five structurally distinct claims instantiated by five readings of one kernel. Each sibling file authors its own epsilon over the IDENTICAL referent (the standing territorial-sovereignty arrangement): this file (settler_colonial_reading) authors epsilon=0.86 counting the sovereignty/land transfer as extraction; the liberal_nationalist file will author materially lower epsilon treating the same transfer as legitimate self-determination; the religious_zionist file treats the transfer as theological fulfillment; the cultural_zionist file evaluates a narrower cultural-center referent; the post_zionist file accepts the founding fact and evaluates the present civic framework. Upstream/downstream: the liberal_nationalist and religious_zionist readings are upstream (they supplied the legitimating frame under which the arrangement was built); this reading and the post_zionist reading are downstream critiques whose empirical base (archival displacement documentation) was produced against the upstream frame. All five files carry mutual affects_constraints edges; orphaning any member breaks contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
