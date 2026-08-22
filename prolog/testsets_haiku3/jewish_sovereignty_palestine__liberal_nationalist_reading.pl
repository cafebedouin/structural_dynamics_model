% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination and Statehood in Palestine (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   Under the liberal nationalist reading, Jewish people constitute a nation
 *   with legitimate collective self-determination rights, grounded in
 *   transhistorical peoplehood and the universal liberal principle that all
 *   nations possess the right to self-governance and homeland restoration.
 *   The reading affirms statehood in Palestine / Eretz Yisrael as the
 *   legitimate exercise of this right. Critically, the liberal nationalist
 *   reading DIFFERS structurally from the religious Zionist reading (which
 *   grounds the claim in divine promise) and the settler-colonial reading
 *   (which interprets Jewish immigration as a displacement regime). The
 *   liberal nationalist reading positions Palestinians as CO-EQUAL
 *   self-determination claimants and frames legitimacy through PARTITION or
 *   BINATIONAL governance, not through Jewish demographic or religious
 *   dominance. This constraint is ONE reading of the contested kernel
 *   'jewish_sovereignty_palestine'; other readings will author different ε
 *   values and different victim/beneficiary structures. The claim/metric gap
 *   is intentional: extractiveness is authored at 0.62 (moderate, reflecting
 *   territorial compromise and Palestinian co-claims) and suppression at 0.58
 *   (reflecting active enforcement of Jewish state authority over Palestinian
 *   populations). The engine will compute per-seat classifications; the
 *   authored divergence between beneficiary and payer seats is the point of
 *   measurement.
 *
 * KEY AGENTS:
 *   - jewish_collective_as_nation: beneficiary and agenda-setter (institutional power, identity-locked); establishes and administers statehood
 *   - palestinian_collective_as_nation: payer and co-claimant (organized power, constrained exit); bears territorial loss and subordination under Jewish state framework
 *   - jewish_diaspora_communities: distributed beneficiaries (organized power, mobile exit); gain from sovereignty as refuge and cultural center
 *   - palestinian_diaspora_and_displaced: distributed payers (moderate to powerless, constrained/trapped exit); bear dispossession and exclusion
 *   - liberal_nationalist_tradition: observer (analytical, non-agent); interpretive authority grounding the legitimacy claim
 *   - colonial mandate authority: historical agenda-setter (institutional, now biographical time-horizon); enabled statehood through partition and sovereignty transfer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.62).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.58).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Self-Determination and Statehood in Palestine (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '1cb51201-d4e9-4294-896c-4908862c087c').
narrative_ontology:cs_kernel_codification('1cb51201-d4e9-4294-896c-4908862c087c', fixed_text).
narrative_ontology:cs_authority_grounding('1cb51201-d4e9-4294-896c-4908862c087c', lineage).
narrative_ontology:cs_interpretation_layer_present('1cb51201-d4e9-4294-896c-4908862c087c').
narrative_ontology:cs_reading_relation('1cb51201-d4e9-4294-896c-4908862c087c', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cb51201-d4e9-4294-896c-4908862c087c', jewish_sovereignty_palestine__religious_zionist_reading, influences).
narrative_ontology:cs_reading_relation('1cb51201-d4e9-4294-896c-4908862c087c', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('1cb51201-d4e9-4294-896c-4908862c087c', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('1cb51201-d4e9-4294-896c-4908862c087c', foundational, universal_self_determination_rights_apply_to_all_peoples).
narrative_ontology:cs_axiom_status(universal_self_determination_rights_apply_to_all_peoples, holdable).
narrative_ontology:cs_axiom_grounding('1cb51201-d4e9-4294-896c-4908862c087c', universal_self_determination_rights_apply_to_all_peoples, deontological).
narrative_ontology:cs_axiom('1cb51201-d4e9-4294-896c-4908862c087c', foundational, palestinian_self_determination_co_equal_claim).
narrative_ontology:cs_axiom_status(palestinian_self_determination_co_equal_claim, holdable).
narrative_ontology:cs_axiom_grounding('1cb51201-d4e9-4294-896c-4908862c087c', palestinian_self_determination_co_equal_claim, deontological).
narrative_ontology:cs_axiom('1cb51201-d4e9-4294-896c-4908862c087c', secondary, partition_or_binational_framework_legitimate_resolution).
narrative_ontology:cs_axiom_status(partition_or_binational_framework_legitimate_resolution, holdable).
narrative_ontology:cs_axiom_grounding('1cb51201-d4e9-4294-896c-4908862c087c', partition_or_binational_framework_legitimate_resolution, deontological).
narrative_ontology:cs_reference_frame('1cb51201-d4e9-4294-896c-4908862c087c', liberal_self_determination_universalism).
narrative_ontology:cs_drift_state('1cb51201-d4e9-4294-896c-4908862c087c', contemporary_post_oslo_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1cb51201-d4e9-4294-896c-4908862c087c', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, international_liberal_order).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_diaspora_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, indigenous_palestinian_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish people organized as a national collective claiming self-determination rights and territorial sovereignty in Palestine / Eretz Yisrael. Under this reading, the Jewish people possess a legitimate collective right to establish and maintain a nation-state in their claimed ancestral homeland, parallel to other nations' self-determination claims. This reading affirms Jewish peoplehood as a transhistorical category bearing rights to self-governance and homeland restoration.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation, agenda_setter).

% Palestinian people organized as a national collective with their own self-determination claim grounded in continuous presence and sovereignty in the same territory. Under the liberal nationalist reading, Palestinians are co-equal claimants to self-determination; the constraint's legitimacy (from this seat) depends on territorial partition or binational framework that honors BOTH claims. Palestinian exit from the constraint involves either territorial sovereignty or integration in a shared political structure that guarantees equal national rights.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation, payer,
    organized, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation, beneficiary).

% Jewish communities worldwide who do not reside in the Palestinian territory but identify with the Jewish collective's self-determination project and benefit from the existence of a Jewish nation-state as a refuge, cultural center, and expression of Jewish sovereignty. Their attachment is relational and diasporic, not territorial; they gain symbolically and practically from the constraint's establishment.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Palestinian communities displaced from or excluded from Palestinian territories by the establishment of Jewish statehood, or who remain within territories subordinated to the Jewish state's governance. They bear the cost of territorial loss, diasporization, and structural exclusion from sovereignty over their claimed homeland.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_diaspora_communities, payer,
    moderate, generational, constrained, global).

% The post-WWII liberal international framework that enshrines national self-determination as a human right. The liberal nationalist reading invokes and validates this framework by claiming Jewish self-determination is a legitimate application of universal self-determination rights. The international order benefits from the existence of a test case affirming that national self-determination applies across cultural-religious-historical groups.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_liberal_order, beneficiary,
    institutional, generational, analytical, global).

% British mandate authority and earlier Ottoman/imperial structures that facilitated Jewish immigration and community-building, then authorized partition and statehood. These actors set the procedural and legal conditions under which the constraint took institutional form; they administered the territorial arrangement and enforced its boundaries (though their enforcement capacity weakened over time).
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, colonial_powers_and_mandate_authority, agenda_setter,
    institutional, biographical, analytical, global).

% Palestinians resident in the territory at the time of Jewish state formation and afterward, who experienced displacement, subordination, or loss of majority governance. Their 'trapped' exit status reflects that remaining means living under a state structure they did not choose and that does not recognize them as constitutive to its founding legitimate purpose. Departure means leaving homeland; staying means accepting minority or non-citizen status in a state constituted around Jewish national identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, indigenous_palestinian_residents, payer,
    powerless, civilizational, trapped, regional).

% Intellectual tradition affirming national self-determination as a universal right and framework for legitimate statehood. This is not an agent but the interpretive lens through which this reading is grounded; it is included for completeness because the constraint's legitimacy hinges on invoking this tradition as its authority grounding.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_nationalist_tradition, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_nationalist_tradition).

% Arab states and Palestinian leadership espousing alternative nationalist frameworks (Arab nationalism encompassing Palestinians as part of larger Arab nation, Islamic governance frameworks, secular civic nationalism) that are structurally excluded from the constraint's formation logic. These readings would contest the Jewish collective's exclusive or primary self-determination claim and assert Palestinian national primacy or binationalism; they are kept from shaping the constraint's institutional form by the liberal nationalist framework's prioritization of the Jewish collective as the foundational self-determining unit.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, competing_nationalist_claimants, excluded,
    powerful, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes political sovereignty and self-governance for a historically dispersed national collective (Jewish people) that lacks territorial control, affirming the universal principle that all peoples possess the right to self-determination expressed through statehood. Solves the coordination problem of how a diaspora nation can exercise political rights: by concentrating sovereignty in a homeland and creating institutional structures (citizenship, law, governance) through which Jewish self-determination becomes operationalized.
% TRANSFER_FUNCTION: Moves territorial control, political authority, and sovereignty from Palestinian Arabs (and Ottoman/British mandate structures) to Jewish institutions and the Jewish collective. The constraint transfers primary decision-making power over the territory and its governance to Jewish national institutions, granting Jewish people a state where they constitute the founding political nation, while Palestinians are positioned as minorities, excluded from national constitution, or offered separate territorial arrangements (partition framework).
% ABSENT_VOICES: Palestinian national representatives rejecting the partition framework in favor of unitary democratic statehood or right of return; Arab nationalist movements asserting Palestinian Arabs as part of a larger Arab nation; indigenous Palestinian residents displaced by state formation; post-Zionist Jews questioning whether ethnic-national statehood aligns with liberal egalitarian principles; settler-colonial and postcolonial scholars who read the constraint as instantiating a displacement regime. These voices would reject the liberal nationalist framing itself and assert that self-determination for one group cannot legitimately override territorial presence and self-determination claims of another group in the same space.
% DISAPPEARANCE_RATIONALE: If Jewish statehood and the liberal nationalist self-determination claim disappeared overnight, the institutional form of governance over Palestinian territory would be reorganized — either toward Palestinian national statehood, toward a binational democratic state, or toward regional Arab integration. The removal of Jewish-centered sovereignty would displace institutional structures, citizenship arrangements, and political identity frameworks currently organized around Jewish self-determination. Diaspora Jewish communities would lose a focal point of national sovereignty and territorial refuge; the validation that liberal self-determination rights apply equally to Jewish nationhood would be contested or overridden by alternative frameworks.
% FOUNDING_PROBLEM: Jewish people, historically dispersed and stateless, lacked a political vehicle for collective self-determination and were vulnerable to persecution, exclusion, and dispossession across diaspora contexts. Zionist ideology framed statehood in a claimed ancestral homeland as the solution: a nation-state would allow Jews to exercise self-governance, secure a refuge from antisemitism, and restore Jewish peoplehood to political agency on the international stage.
% FOUNDING_PROBLEM_CORROBORATION: Jewish nationalist and liberal international theorists attest the founding problem as live: antisemitism and diaspora vulnerability persist, and statehood provides ongoing security and national dignity. Palestinian and postcolonial scholars attest the founding problem is inaccurately framed: the 'solution' (Jewish statehood via displacement) creates a NEW problem (Palestinian statelessness and dispossession) that overshadows the original problem, and the framing elides how the solution was pursued without Palestinian consent. International human rights bodies and liberal philosophers note that while Jewish self-determination is a legitimate right, it cannot be exercised in a way that nullifies Palestinian self-determination — the corroboration is split across reading communities.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at 0.62 to reflect moderate extraction tied to territorial displacement and Palestinian subordination, moderated by the liberal nationalist reading's commitment to Palestinian co-determination and partition as legitimate resolution. At 1948 (statehood establishment) extractiveness peaks at 0.71 because maximal territorial transfer occurs and Palestinian displacement is immediate and total at that moment. By 2026 it has declined to 0.62 as international pressure for Palestinian statehood and rights recognition has partially institutionalized co-equal claims (Palestinian Authority, Oslo Accords, international recognition), though execution remains contested. Suppression peaks at 0.74 in 1967 (post-territorial conquest and military occupation) when enforcement of Jewish state authority over Palestinian populations intensifies, then declines toward 0.58 as Palestinian institutional governance expands in limited areas. Theater ratio remains low (0.08 to 0.28) because the state's foundational function — Jewish national governance — is continuously performed rather than performative. The single time grid ensures every metric is authored at every examined point, avoiding false temporal attribution.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Jewish collective) and payer seat (Palestinian collective) compute fundamentally different type classifications from the same structural data. From the Jewish beneficiary seat: legitimate national self-determination exercised through statehood establishment, a genuine coordination function (bringing together a diaspora to form a nation) with moderate extraction costs borne by Palestinians as transition price toward co-equal partition. From the Palestinian payer seat: the same structure operates as enforced dispossession justified retroactively by reference to Jewish historical claims, with no genuine coordination of Palestinian self-determination — only subordination. The liberal nationalist reading explicitly acknowledges this perspectival gap and frames legitimacy through Palestinian CO-CLAIMS and PARTITION, not through dismissing Palestinian grievance. Directionality differs: Jewish collective sits near d=0.2–0.3 (beneficiary with identity-lock), Palestinians near d=0.7–0.8 (trapped payers). From the Palestinian seat, the constraint computes as snare or tangled-rope-extraction-asymmetry; from the Jewish seat, rope-with-coordination. The engine's per-seat computation captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective benefits from statehood establishment and territorial sovereignty (d toward beneficiary end, 0.2–0.3), with identity-locked exit making the claim non-negotiable at the collective level — exit for Jewish peoplehood means abandoning the self-determination claim itself, which is identity-constitutive. Palestinian collective bears territorial loss, subordination under Jewish state governance, and exclusion from majority self-determination (d toward target end, 0.7–0.8), with constrained exit (territorial partition offered but not autonomy in current territory) and trapped exit for residents unable to leave. Jewish diaspora has mobile exit (can choose to engage with or abandon the Jewish state project) and arbitrage options (support Jewish statehood while remaining in diaspora), d near 0.3. Palestinian diaspora and displaced persons have trapped or identity-locked exit (identity as Palestinians remains regardless of physical location, but agency is constrained). The liberal nationalist reading's commitment to Palestinian co-determination means the constraint's legitimacy depends on meaningful Palestinian exit options — partition with viable Palestinian statehood. Where partition is blocked, extracted value rises and suppression increases. Commentary notes: the author believes extractiveness reflects the territorial compromise framework inherent to liberal nationalism; a settler-colonial reading would author much higher extractiveness (approaching 0.85+) because it treats partition itself as inadequate remedy for displacement.
 *
 * MANDATROPHY ANALYSIS:
 *   The liberal nationalist reading avoids mislabeling by explicitly anchoring legitimacy to UNIVERSAL self-determination rights and Palestinian CO-CLAIMS. The constraint is not pure rope (no universal beneficiary; coordination asymmetrically benefits Jewish collective) and not pure snare (extraction is justified by reference to self-determination rights, not pure coercion). Tangled rope fits: genuine coordination function (Jewish national self-governance) paired with asymmetric extraction (Palestinian displacement and subordination) justified through reference to competing legitimate claims, requiring active enforcement of the state's authority over Palestinian populations. The mandatrophy question: does the founding problem (Jewish statelessness and vulnerability to persecution) remain LIVE enough to justify ongoing enforcement of extraction? The liberal reading answers: yes, but only if Palestinians' founding problem (territorial sovereignty and non-subordination) is ALSO honored through partition and co-equal statehood. Where partition is blocked, mandatrophy resolves against the constraint — Jewish security is achieved, but at the cost of permanently extracted Palestinian self-determination. The reading thus prevents the snare cover story ('we are coordinating Jewish security') by building Palestinian co-determination into the legitimacy condition itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    territorial_compromise_vs_inalienable_claim,
    'Is the liberal nationalist reading''s commitment to partition compatible with Jewish collective identity''s historical self-understanding of an inalienable right to the entire territory of Eretz Yisrael?',
    'Genealogical analysis of liberal Zionist thought (Ahad Ha''am, A.B. Yehoshua, David Grossman) vs. maximalist territorial readings; polling of Jewish collective self-understanding; examination of whether partition is framed as permanent solution or temporary compromise.',
    'If partition conflicts with core Jewish collective identity, the liberal reading''s legitimacy claim becomes internally contradictory — liberalism requires respecting Palestinian co-determination, but Jewish identity may demand undivided territory. The constraint''s type could shift toward snare (identity-lock preventing compromise) or toward perpetual tangled-rope (coordination paralyzed by incompatible claims). If partition is sustainable as legitimate compromise within Jewish identity, the reading remains coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_compromise_vs_inalienable_claim, conceptual, 'Compatibility of liberal nationalist framework with territorial maximalism in Jewish collective self-understanding').

omega_variable(
    beneficiary_extraction_asymmetry,
    'Does the liberal nationalist reading''s acknowledgment of Palestinian co-determination claims reduce extraction to genuinely negotiated compromise, or does institutional asymmetry (Jewish majority governance, security dominance, resource control) perpetuate extraction regardless of partition framework?',
    'Post-partition comparative analysis: in cases where Palestinian statehood was negotiated (Oslo Accords, potential two-state outcomes), do metrics of Palestinian institutional autonomy, resource sovereignty, and security control match Jewish state metrics? Or do structural asymmetries persist such that extraction continues despite formal partition?',
    'If structural asymmetries persist post-partition, the constraint remains tangled rope with high extraction and high suppression; legitimacy depends on asymmetry progressively eroding toward equality. If partition genuinely produces symmetric co-determination, extraction declines and suppression becomes mutual enforcement of shared rules rather than Jewish dominance. The reading''s coherence depends on partition producing real symmetry, not formal partition masking persistent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_asymmetry, empirical, 'Whether liberal nationalist partition framework produces genuine symmetry or perpetuates asymmetric extraction').

omega_variable(
    diaspora_identity_lock_asymmetry,
    'Jewish diaspora beneficiaries claim identity-locked attachment to the statehood project (exit is identity negation), while Palestinian diaspora payers also have identity-locked attachment (identity as Palestinians persists regardless of location). Is the liberal reading''s framework symmetrical regarding identity-lock, or does it privilege Jewish identity-lock over Palestinian identity-lock?',
    'Normative analysis: does the liberal reading grant equal weight to Palestinian diaspora right of return / participation in Palestinian self-determination as it grants to Jewish diaspora right to support Jewish statehood? Or does the reading structure the constraint such that Jewish diaspora identity-lock is treated as constitutive (legitimate claim) while Palestinian diaspora identity-lock is treated as residual (accommodated through compensation, not repatriation)?',
    'If Jewish identity-lock is privileged as constitutive and Palestinian identity-lock is marginalized, the constraint remains asymmetrically extractive and the liberal reading''s universalist claim (self-determination for all nations) is contradicted by its particularity (Jewish claims frame the constraint, Palestinian claims are fitted within it). This would suggest the reading''s actual structure is closer to settler colonialism or religious Zionism (Jewish supremacy) than to true liberal nationalism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_identity_lock_asymmetry, conceptual, 'Symmetry of identity-lock treatment across Jewish and Palestinian diasporas in liberal nationalist framework').

omega_variable(
    universal_self_determination_vs_particularist_application,
    'Does invoking universal self-determination rights (the liberal foundation) as grounds for Jewish statehood genuinely apply to all peoples equally, or does the reading''s emphasis on Jewish peoplehood''s historical continuity and victimhood carve out a particular exception?',
    'Comparative application: do the same self-determination principles the reading uses for Jewish national claims equally support Kurdish, Uyghur, Tibetan, or other national groups'' statehood claims? Are there principled reasons the reading would treat Jewish self-determination differently, or is the universalism applied selectively?',
    'If the universalism is genuinely universal, the reading''s liberal foundations are sound but the constraint''s extractiveness reflects territorial scarcity (multiple competing claims in bounded space), not Jewish particularism. If the universalism is applied selectively (Jewish claims privileged, others deferred or denied), the reading masks particularist extraction under universalist language and is closer to false summit or snare territory. The reading''s integrity depends on non-selective application.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_self_determination_vs_particularist_application, preference, 'Whether liberal nationalism''s universal self-determination principle is applied equally across all national groups or selectively privileges Jewish claims').

omega_variable(
    reading_vs_settler_colonial_structural_convergence,
    'Notwithstanding the liberal reading''s explicit rejection of settler colonialism, does the actual institutional structure — Jewish immigration, territorial displacement of Palestinians, majority-rule governance under Jewish constitution, security dominance — converge with settler-colonial patterns regardless of the reading''s normative framing?',
    'Structural-phenomenological analysis: comparing institutional form (Who immigrated? Who was displaced? Who governs? On what constitutional basis?) across the liberal Zionist case and established settler-colonial cases (North America, Australia, South Africa). If institutional forms converge despite normative disagreement, the reading may be describing the same pattern using different vocabulary.',
    'If structural convergence exists, the liberal reading does NOT escape the settler-colonial pattern — it legitimizes it through rights language. The constraint''s actual operation (displacement, subordination, majority governance) would match the settler_colonial_reading''s analysis, suggesting the liberal reading is a normative disagreement about the same facts, not a different structural arrangement. This would reposition the readings from logical coexistence to factual coexistence with different value judgments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_settler_colonial_structural_convergence, conceptual, 'Structural convergence between liberal nationalist reading''s institutional form and established settler-colonial patterns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1880, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(jewi_tr_t1920, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.24).
narrative_ontology:measurement(jewi_tr_t1990, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1990, 0.27).
narrative_ontology:measurement(jewi_tr_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement(jewi_be_t1920, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1920, 0.48).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.71).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.76).
narrative_ontology:measurement(jewi_be_t1990, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(jewi_be_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1880, 0.25).
narrative_ontology:measurement(jewi_su_t1920, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1920, 0.42).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.68).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.74).
narrative_ontology:measurement(jewi_su_t1990, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(jewi_su_t2026, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.18).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_self_determination_claim).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, right_of_return_palestinian_diaspora).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the kernel 'jewish_sovereignty_palestine'. The liberal_nationalist_reading grounds legitimacy in universal self-determination rights and commits structurally to Palestinian CO-DETERMINATION through partition or binational frameworks. This differs fundamentally from the religious_zionist_reading (divine promise, territorial maximalism), settler_colonial_reading (displacement regime regardless of intent), cultural_zionist_reading (cultural center, not political dominance), and post_zionist_reading (statehood achieved, now problematic). Each reading authors a different ε, different beneficiary/victim structure, and different type classification. The readings are linked through this constraint family to enable comparative analysis of how the same kernel is read differently across legitimacy frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
