% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diaspora Pluralism as Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The diasporist reading of Jewish self-determination posits that Jewish
 *   collective survival and flourishing are best secured through
 *   minority-rights frameworks and diaspora pluralism, not through
 *   territorial sovereignty or Zionism. This reading instantiates an
 *   atrophied alternative: the diaspora institutional framework that
 *   historically organized Jewish political life has been subordinated by
 *   Zionist institutional hegemony since 1948. The reading is ONE claim about
 *   ONE kernel—the kernel being the contested question of Jewish
 *   self-determination itself. The reading does NOT claim that diaspora
 *   survival is currently the dominant institutional reality; rather, it
 *   claims that diaspora pluralism remains THE LEGITIMATE framework for
 *   Jewish self-determination, and that Zionist frameworks that tie Jewish
 *   fate to a militarized state are a dangerous deviation that has weakened
 *   diaspora institutions and exposed Jews to risk. This reading coexists
 *   with liberal-nationalist, indigenous-return, religious-covenant, and
 *   settler-colonial readings of the same kernel—all instantiated as separate
 *   constraint stories linked by network relations.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Maintain distinct cultural and political identities within host societies; view diaspora institutions as the authentic locus of Jewish continuity.
 *   - jews_coerced_into_zionist_framework: Experience suppression of diasporist alternatives through institutional gatekeeping and identity-fusion pressure; labeled as inauthentic or self-hating when dissenting.
 *   - jews_endangered_by_israeli_state_association: Bear heightened antisemitic backlash and security risk when Israeli military actions generate international condemnation, yet are held collectively responsible.
 *   - zionist_institutional_hegemony: Controls major Jewish organizations, federations, and advocacy bodies; frames Zionism as mandatory Jewish identity and suppresses diasporist alternatives through epistemic gatekeeping.
 *   - host_state_tolerance_regime: Determines material viability of diaspora survival through minority-rights protections; the reading is structurally vulnerable to host-state policy shifts.
 *   - israeli_state_apparatus: Excluded from the reading's core logic—the reading's legitimacy rests on the claim that Jewish flourishing does NOT require territorial sovereignty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.62).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.71).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diaspora Pluralism as Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, 'b3479aba-c7db-4d4d-b609-d06b1efdc343').
narrative_ontology:cs_kernel_codification('b3479aba-c7db-4d4d-b609-d06b1efdc343', distributed).
narrative_ontology:cs_authority_grounding('b3479aba-c7db-4d4d-b609-d06b1efdc343', distributed).
narrative_ontology:cs_reading_relation('b3479aba-c7db-4d4d-b609-d06b1efdc343', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3479aba-c7db-4d4d-b609-d06b1efdc343', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3479aba-c7db-4d4d-b609-d06b1efdc343', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3479aba-c7db-4d4d-b609-d06b1efdc343', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('b3479aba-c7db-4d4d-b609-d06b1efdc343', foundational, diaspora_pluralism_sufficient_for_jewish_flourishing).
narrative_ontology:cs_axiom_status(diaspora_pluralism_sufficient_for_jewish_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('b3479aba-c7db-4d4d-b609-d06b1efdc343', diaspora_pluralism_sufficient_for_jewish_flourishing, empirically_contingent).
narrative_ontology:cs_axiom('b3479aba-c7db-4d4d-b609-d06b1efdc343', foundational, territorial_sovereignty_unnecessary_and_dangerous_for_jewish_self_determination).
narrative_ontology:cs_axiom_status(territorial_sovereignty_unnecessary_and_dangerous_for_jewish_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('b3479aba-c7db-4d4d-b609-d06b1efdc343', territorial_sovereignty_unnecessary_and_dangerous_for_jewish_self_determination, instrumental).
narrative_ontology:cs_reference_frame('b3479aba-c7db-4d4d-b609-d06b1efdc343', pre_zionist_diaspora_pluralism).
narrative_ontology:cs_drift_state('b3479aba-c7db-4d4d-b609-d06b1efdc343', contemporary_post_1948_hegemony, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b3479aba-c7db-4d4d-b609-d06b1efdc343', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_state_association).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain distinct cultural, religious, and linguistic identities within pluralist host societies. They have built parallel institutions—synagogues, schools, cultural organizations, political advocacy networks—over centuries. The diasporist reading affirms their legitimacy as the authentic expression of Jewish continuity. They benefit from minority-rights frameworks that protect group autonomy without requiring territorial sovereignty. Their exit is constrained by the accumulated weight of diaspora institutions and identities.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Experience pressure from Zionist institutional monopolies (major Jewish organizations, federations, advocacy bodies) that frame support for Israel as mandatory Jewish identity. Diasporists who dissent from Zionist consensus are excluded from decision-making bodies, defunded, and labeled as self-hating or inauthentic. Exit is structurally blocked by identity fusion: rejecting the Zionist framework feels like rejecting Jewishness itself, a boundary the constraint maintains theatrically. They pay through loss of voice, institutional belonging, and the cognitive burden of dissimulation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionist_framework, payer,
    moderate, biographical, identity_locked, global).

% Bear heightened security risk and antisemitic backlash in host countries when Israeli military operations generate international condemnation. They are held collectively responsible for Israeli state actions despite having no vote in Israeli governance. Diasporist reading posits that tying Jewish identity to territorial sovereignty in a militarized state transfers risk from the state apparatus to dispersed Jewish communities worldwide. Their exit is trapped: they cannot disassociate from the Zionist framing without institutional expulsion, yet association with the Israeli state's actions creates material danger.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_state_association, payer,
    powerless, immediate, trapped, global).

% Controls the major channels of institutional Jewish life—federations, major advocacy organizations, educational networks, philanthropic priorities—and frames Zionism as the default expression of Jewish identity. Historically, diaspora institutions competed with Zionist institutions; post-1948, Zionist framing absorbed and subordinated diaspora alternatives. The institutional agenda-setter administers the suppression not through overt coercion but through epistemic gatekeeping: what counts as legitimate Jewish discourse, who gets speaking platforms, which organizations receive funding. This agent is not a unified actor but a structural configuration of overlapping organizations with converged interests.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutional_hegemony, agenda_setter,
    institutional, generational, arbitrage, global).

% The diasporist reading depends structurally on host-state willingness to grant minority-rights protections and pluralist inclusion. Where host states withdrew those protections (Eastern Europe in the interwar period, North Africa post-1948), diaspora institutions collapsed and Zionist alternatives became hegemonic. The observer seat here is not passive: host-state policies determine whether diaspora survival is materially possible. The reading is vulnerable to erosion whenever host-state tolerance contracts.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_state_tolerance_regime, observer,
    institutional, generational, analytical, national).

% Is structurally excluded from the diasporist reading—the reading posits Jewish flourishing WITHOUT territorial sovereignty, making Israeli state interests orthogonal or contradictory to the diasporist framework. The state apparatus cannot advocate for diasporist alternatives without undermining its own legitimacy claim (that Jewish self-determination requires territorial sovereignty). This exclusion is structural, not procedural: the reading's core logic forecloses Israeli state advocacy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, israeli_state_apparatus, excluded,
    institutional, generational, trapped, national).

% Scholars and historians documenting diaspora Jewish thought, medieval Jewish pluralism, Enlightenment-era assimilationist and autonomist alternatives to Zionism, and postcolonial critiques of nationalism. They occupy the analytical seat: their work reconstructs the historical coherence and contemporary viability of the diasporist reading. Their exclusion from institutional Jewish decision-making bodies is nearly total, yet their intellectual work feeds diasporist legitimacy claims.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, intellectual_genealogy_custodians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, zionist_institutional_hegemony).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes collective Jewish identity and political advocacy around pluralist minority-rights frameworks rather than territorial sovereignty. Coordinates the maintenance of diaspora institutions (synagogues, schools, cultural organizations, political networks) as the legitimate locus of Jewish self-expression. Solves the problem of Jewish group continuity without requiring state power: through civil-society institutions, cultural transmission, and political participation as minorities in host-state democratic frameworks.
% TRANSFER_FUNCTION: Transfers political legitimacy from territorial-sovereignty-based claims (Zionism) to diaspora-institution-based claims (pluralism). Moves institutional resources and decision-making authority from Zionist hegemonic bodies to competing diaspora frameworks. Redirects the frame of 'Jewish interest' from Israeli state security to diaspora community autonomy and host-state minority protections.
% ABSENT_VOICES: Israeli state actors, right-wing and left-wing Zionist constituencies, and Jewish communities inside Israel whose primary institutional attachment is to Israeli state structures are structurally excluded from the diasporist framework—the reading posits that Israeli state interests are NOT the locus of Jewish self-determination. Also absent: Haredi (ultra-Orthodox) communities with their own anti-Zionist theology, whose voice would complicate the reading's universalism claim. Postcolonial Palestinian scholars who theorize diaspora as survival strategy are intellectually aligned but institutionally absent from Jewish community decision-making.
% DISAPPEARANCE_RATIONALE: If the diasporist reading disappeared (if Zionist hegemony became total monopoly over Jewish institutional life), diaspora Jewish institutions would not vanish but would operate under a different interpretive frame—they would be reframed as 'preparation for aliyah,' 'temporary accommodation,' or 'diaspora exile' rather than as legitimate expressions of Jewish flourishing. Conversely, if diasporist alternatives reasserted institutional power, Zionist organizations would continue to exist but would lose their claim to represent 'Jewish interest' as such. The disagreement is about which framework legitimately speaks for collective Jewish self-determination, not about whether Jewish communities exist in diaspora. The verdict is contested because the two readings compete for the same institutional and legitimacy space; neither can disappear without reshaping the other.
% FOUNDING_PROBLEM: Jewish survival across dispersal and statelessness: how to maintain collective identity, political autonomy, and cultural continuity without territorial sovereignty. The diasporist reading posits that this problem has been SOLVED through centuries of diaspora institution-building—community organizations, religious practice, intellectual traditions, and political advocacy networks all sustained Jewish peoplehood across the diaspora. Zionism reframed this as an 'unsolved problem' (statelessness as existential failure) and offered territorial sovereignty as the only solution.
% FOUNDING_PROBLEM_CORROBORATION: Diasporist intellectual tradition (Isaac Deutscher, Bund political theory, Yosef Hayim Yerushalmi's historical work on Jewish cultural memory, contemporary postcolonial scholars like Jacqueline Rose and Judith Butler) attests that diaspora institutions successfully maintained Jewish identity and community for millennia before Zionism. Host-state minority-rights frameworks (liberal democratic constitutions, multiculturalism policies) provide external corroboration that diaspora survival is structurally possible. However, Zionist institutional custodians and Israeli state intellectuals dispute whether diaspora survival is ADEQUATE—they argue that minoritized status leaves Jews perpetually vulnerable. Postcolonial scholars outside the Jewish community corroborate that diaspora alternatives to territorial nationalism are viable political strategies, but they do not claim authority over what constitutes 'Jewish interest.'
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The diasporist reading is classified as PITON—an atrophied alternative that persists through performative rather than functional force. Extractiveness is moderate (0.62): the suppression of diasporist alternatives is real, but diaspora institutions retain organizational capacity and continue to operate even under Zionist hegemony. Suppression is substantial (0.71): the constraint maintains itself through epistemic gatekeeping (controlling what counts as legitimate Jewish discourse), institutional gatekeeping (who gets speaking platforms, funding), and identity-fusion pressure (making dissent feel like identity betrayal). Theater_ratio is high (0.68): the largest share of institutional Jewish activity devoted to suppressing diasporist voice is performative—reaffirmations of Zionist identity, conditioning funding on loyalty oaths, public shaming of diasporist intellectuals. These performances maintain the constraint's internal coherence, not its external function. The measurement series track 80 years of accumulating Zionist institutional dominance (1945–2025): extractiveness rose from 0.38 to 0.62 as Zionist institutions absorbed and subordinated competing diaspora alternatives; theater_ratio rose from 0.25 to 0.68 as the performative maintenance of Zionist monopoly intensified relative to genuine coordination functions. Suppression_requirement tracks the erosion of alternatives: in 1945, competing frameworks (Bundist, autonomist, religious anti-Zionist) still had organizational capacity; by 2025, diasporist voice exists primarily in intellectual niches outside mainstream institutional Jewish life. The coercion grid shows LEVELED DYNAMICS: individual-level suppression is highest at organizational gatekeeping and lowest at structural (system-level alternatives still exist, just not in hegemonic positions). Class-level resistance has eroded (Jews as a global class have internalized Zionist framing as their 'interest'). Organizational resistance weakened most sharply (diaspora institutions were absorbed into Zionist governance structures). Structural alternatives remain contested (postcolonial and diaspora-theory communities outside Jewish institutional life maintain intellectual frameworks that validate diasporist claims). The measurement grid reflects the piton pattern: the constraint persists not because it solves a genuine coordination problem (that function transferred to Zionist institutions), but because dismantling it would require institutional restructuring that no concentrated group is hurt enough to demand and no concentrated group is helped enough by maintaining it to defend.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Zionist hegemony) and the target seats (coerced and endangered Jews) should compute different types. From the Zionist institutional seat, the constraint appears as ROPE: genuine coordination of Jewish collective security around shared statehood, freely chosen by participants, minimal coercive overhead (the reading would say). From the target seats, the same structure computes as SNARE: extraction maintained through suppression, coercive institutional gatekeeping, and identity-fusion that prevents exit. The engine computes this divergence per-seat from the structural data: the target seats' authored high suppression, identity_locked exit, and high extractiveness produce high χ (effective extraction) even though the nominal coordination function exists. The agenda-setter seat's low suppression, arbitrage exit, and beneficiary role produce low χ. This is the diagnostic divergence the constraint-story apparatus measures—not a flaw but the measurement's purpose.
 *
 * DIRECTIONALITY LOGIC:
 *   From the diaspora_jewish_communities seat (d near 0.3–0.4 / beneficiary-skewed): the diasporist reading AFFIRMS their institutional autonomy and cultural legitimacy, offering them a framework where their identity is not subordinated to state interests. They pay through loss of institutional voice and participation in major Jewish decision-making bodies; they benefit through cultural validation. From the jews_coerced_into_zionist_framework seat (d near 0.75 / target-skewed): the constraint extracts through suppression of their voice, identity-fusion pressure that makes exit feel like identity death, and epistemic gatekeeping that frames their positions as inauthentic. Exit is identity_locked: leaving the Zionist frame feels like ceasing to be Jewish in the eyes of institutional structures. From the jews_endangered_by_israeli_state_association seat (d near 0.95 / full target): the constraint extracts through transferred state risk—they bear heightened personal security risk and antisemitic backlash without governance input. Their exit is trapped: they cannot disassociate from the Israeli-state-Zionist-Jewish-identity linkage because that linkage is itself the constraint. From the zionist_institutional_hegemony seat (d near 0.1 / beneficiary-skewed): the constraint yields institutional power, resource control, and legitimacy to frame 'Jewish interest' as Zionist interest. They pay minimal costs because the hegemonic position is maintained by others' suppression. From the host_state_tolerance_regime seat (d near 0.5 / symmetric): the reading offers a framework for host-state minority-rights management (Jews are just another ethnic/religious minority), but also potentially threatens state-Jewish symbiosis models where the Jewish diaspora is instrumentalized for host-state strategic interests (e.g., U.S.–Israel alliance benefits). From the israeli_state_apparatus seat: excluded structurally—the reading's core logic forecloses Israeli state advocacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist reading exhibits classic PITON signals: (1) Founding problem is contested (Jewish survival—solved by diaspora for millennia, or unsolved without statehood?). (2) Founding problem status is dead for one seat (institutional Zionism would say diaspora survival is inherently unstable and Zionism solved it) and live for another (diasporist intellectuals say diaspora survived for 2,000 years and solves the problem adequately). (3) The constraint persists through theatrical maintenance: major institutional Jewish activity is devoted to affirming Zionist identity and suppressing diasporist alternatives, not to solving any live coordination problem. Diaspora institutions continue to deliver real community, cultural transmission, and social support—but these functions are now subordinated to Zionist frames rather than central to the arrangement. (4) No concentrated party is hurt enough to fix it: diaspora communities still benefit from community institutions even under Zionist framing; the endangered and coerced groups lack institutional power to restructure. No concentrated party profits enough from maintaining it: Zionist institutions capture benefits but also bear costs (defending the arrangement, managing internal dissent, managing external criticism). The arrangement persists because the cost to change it exceeds the immediate benefit for anyone with power to change it. This is classic piton inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diaspora_institutional_viability,
    'Can diaspora Jewish institutions maintain cultural and political autonomy indefinitely within pluralist host states, or do they require periodic ''refresh'' from religious/national sources to remain viable?',
    'Historical trajectories of Jewish diaspora communities across 2,000+ years: did diaspora institutions persist through internal innovation or through periodic territorial/national reference? Post-2025 trajectory of diaspora communities in Western democracies: do they strengthen, stabilize, or erode as Zionist institutional dominance persists?',
    'If diaspora institutions can sustain themselves internally, the diasporist reading''s viability claim is strengthened. If they structurally require territorial/national refreshment, the founding-problem claim (''diaspora solved the problem'') is undermined, and Zionism''s founding problem (''diaspora is unsustainable'') reasserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_institutional_viability, empirical, 'The long-term structural viability of diaspora Jewish institutions as autonomous entities.').

omega_variable(
    host_state_tolerance_as_structural_condition,
    'Is diaspora survival structurally dependent on host-state minority-rights frameworks that can be withdrawn, or can diaspora institutions persist through ''thickened culture'' even if host-state legal frameworks contract?',
    'Historical case studies of Jewish diaspora under non-tolerant regimes: did diaspora institutions persist when legal protections were withdrawn (e.g., Eastern Europe interwar, North Africa post-1948)? Did they persist or collapse? Counterfactual: if host-state tolerance frameworks contract globally, what happens to diasporist institutional claim?',
    'If host-state tolerance is structurally necessary, the diasporist reading is vulnerable to political shifts and the Zionist claim (territorial sovereignty as insurance against diaspora vulnerability) gains force. If diaspora can persist without formal tolerance, the reading''s resilience increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(host_state_tolerance_as_structural_condition, empirical, 'Whether diaspora institutional survival requires host-state legal tolerance or can persist through ''thickened culture'' alone.').

omega_variable(
    identity_lock_mechanism_internalized_vs_structural,
    'Is the suppression of diasporist alternatives maintained through structural coercion (institutional gatekeeping, resource control, organizational barriers) or through internalized identity fusion (Jews have genuinely adopted Zionist frames as part of their self-concept)?',
    'Measurement of post-suppression trajectories: if institutional suppression is removed (diasporist alternatives gain platform and resources), does diaspora-frame adoption persist or erode? Are there population segments where dissent emerges quickly versus slowly? Do second/third-generation diaspora activists show different attachment patterns than first-generation?',
    'If suppression is primarily structural, removing institutional barriers would rapidly restore diasporist alternatives. If internalized, even institutional opening would not restore viability—identity-lock would persist. This differentiates the constraint''s type: if internalization is deep, the constraint approaches SNARE; if structural, it remains PITON.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized_vs_structural, empirical, 'Whether the suppression of diasporist alternatives is maintained through external coercion or internalized identity fusion.').

omega_variable(
    zionist_hegemony_contingent_or_necessary,
    'Is Zionist institutional dominance contingent (result of historical choices and power asymmetries post-1948) or structurally necessary (required by the logic of modern nationalism and security)?',
    'Counterfactual histories: if the 1948 founding had produced different power distributions (diaspora institutions retained strength, Zionist institutions shared power), what would Jewish institutional life look like? Do other diaspora communities (Armenian, Greek, Irish, etc.) show patterns where diaspora and homeland institutions coexist as equals, or does homeland usually dominate?',
    'If contingent, the diasporist reading remains viable as a path-dependent alternative that could be restored. If necessary, the reading describes a world that could never have been and has no practical future—a purely retrospective lament.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zionist_hegemony_contingent_or_necessary, conceptual, 'Whether Zionist institutional hegemony is contingent or structurally necessary.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the diasporist and indigenous_return readings logically foreclose each other, or can both be held by different parties simultaneously?',
    'Logical analysis: the diasporist reading claims Jewish flourishing requires diaspora pluralism and rejection of territorial sovereignty. The indigenous_return reading claims Jews are indigenous with unbroken connection and therefore entitled to territorial return. Can a single framework hold both ''diaspora is good'' and ''indigenous return is obligatory''? Or must a chooser between them?',
    'If foreclosed, the readings compete as incompatible frameworks; movement between them means framework shift. If coexisting, Jews can simultaneously affirm diaspora autonomy AND indigenous connection (which reading group already tries to do). Resolution affects how the engine classifies sibling-reading dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the diasporist and indigenous_return readings logically foreclose each other or can coexist.').

omega_variable(
    mandatrophy_resolution_path,
    'Can the diasporist reading re-establish institutional viability, or has Zionist institutional absorption reached irreversibility?',
    'Post-2025 trajectories: do diasporist institutions rebuild organizational capacity, gain access to institutional decision-making, or restore voice in major Jewish organizations? Or do they remain marginalized intellectual positions without institutional base? Does a political-opening event (major Israeli policy shift, host-state policy change) trigger institutional reorganization?',
    'If viability can be restored, the constraint remains a contested piton with potential for re-emergence. If irreversible, the constraint approaches memorial status—a reading that once organized Jewish institutional life and now survives only as intellectual history.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_resolution_path, empirical, 'Whether diasporist institutional viability can be restored or has reached irreversible marginalization.').

omega_variable(
    reading_kerneling_alternative_framings,
    'Is the kernel I''ve identified (''Jewish self-determination'') the right boundary, or should the kernel be larger (''Jewish peoplehood and its basis'') or smaller (''Diaspora as organizing principle for Jewish life'')?',
    'Logical decomposition: if the kernel is larger, more readings coexist within a single frame. If smaller, some readings belong to different kernels. The choice determines which readings are siblings (coexist) versus which are unrelated.',
    'If the kernel boundaries are misdrawn, the network relations declared in cs_structure.reading_relations should be revised. The cs_structure fields assume these five readings are all interpretations of the SAME kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kerneling_alternative_framings, conceptual, 'Whether the kernel boundary is correctly identified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1945, jewish_self_determination__diasporist_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(jewi_tr_t1960, jewish_self_determination__diasporist_reading, theater_ratio, 1960, 0.4).
narrative_ontology:measurement(jewi_tr_t1975, jewish_self_determination__diasporist_reading, theater_ratio, 1975, 0.55).
narrative_ontology:measurement(jewi_tr_t1990, jewish_self_determination__diasporist_reading, theater_ratio, 1990, 0.63).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__diasporist_reading, theater_ratio, 2005, 0.67).
narrative_ontology:measurement(jewi_tr_t2025, jewish_self_determination__diasporist_reading, theater_ratio, 2025, 0.68).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1945, jewish_self_determination__diasporist_reading, base_extractiveness, 1945, 0.38).
narrative_ontology:measurement(jewi_be_t1960, jewish_self_determination__diasporist_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement(jewi_be_t1975, jewish_self_determination__diasporist_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(jewi_be_t1990, jewish_self_determination__diasporist_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__diasporist_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(jewi_be_t2025, jewish_self_determination__diasporist_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1945, jewish_self_determination__diasporist_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(jewi_su_t1960, jewish_self_determination__diasporist_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement(jewi_su_t1975, jewish_self_determination__diasporist_reading, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(jewi_su_t1990, jewish_self_determination__diasporist_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__diasporist_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(jewi_su_t2025, jewish_self_determination__diasporist_reading, suppression_requirement, 2025, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2025
narrative_ontology:measurement(jewi_grid_01, jewish_self_determination__diasporist_reading, accessibility_collapse(class), 1945, 0.25).
narrative_ontology:measurement(jewi_grid_02, jewish_self_determination__diasporist_reading, accessibility_collapse(class), 2025, 0.48).
narrative_ontology:measurement(jewi_grid_03, jewish_self_determination__diasporist_reading, accessibility_collapse(individual), 1945, 0.3).
narrative_ontology:measurement(jewi_grid_04, jewish_self_determination__diasporist_reading, accessibility_collapse(individual), 2025, 0.55).
narrative_ontology:measurement(jewi_grid_05, jewish_self_determination__diasporist_reading, accessibility_collapse(organizational), 1945, 0.35).
narrative_ontology:measurement(jewi_grid_06, jewish_self_determination__diasporist_reading, accessibility_collapse(organizational), 2025, 0.68).
narrative_ontology:measurement(jewi_grid_07, jewish_self_determination__diasporist_reading, accessibility_collapse(structural), 1945, 0.4).
narrative_ontology:measurement(jewi_grid_08, jewish_self_determination__diasporist_reading, accessibility_collapse(structural), 2025, 0.52).
narrative_ontology:measurement(jewi_grid_09, jewish_self_determination__diasporist_reading, resistance(class), 1945, 0.65).
narrative_ontology:measurement(jewi_grid_10, jewish_self_determination__diasporist_reading, resistance(class), 2025, 0.58).
narrative_ontology:measurement(jewi_grid_11, jewish_self_determination__diasporist_reading, resistance(individual), 1945, 0.45).
narrative_ontology:measurement(jewi_grid_12, jewish_self_determination__diasporist_reading, resistance(individual), 2025, 0.42).
narrative_ontology:measurement(jewi_grid_13, jewish_self_determination__diasporist_reading, resistance(organizational), 1945, 0.52).
narrative_ontology:measurement(jewi_grid_14, jewish_self_determination__diasporist_reading, resistance(organizational), 2025, 0.48).
narrative_ontology:measurement(jewi_grid_15, jewish_self_determination__diasporist_reading, resistance(structural), 1945, 0.4).
narrative_ontology:measurement(jewi_grid_16, jewish_self_determination__diasporist_reading, resistance(structural), 2025, 0.52).
narrative_ontology:measurement(jewi_grid_17, jewish_self_determination__diasporist_reading, stakes_inflation(class), 1945, 0.15).
narrative_ontology:measurement(jewi_grid_18, jewish_self_determination__diasporist_reading, stakes_inflation(class), 2025, 0.55).
narrative_ontology:measurement(jewi_grid_19, jewish_self_determination__diasporist_reading, stakes_inflation(individual), 1945, 0.2).
narrative_ontology:measurement(jewi_grid_20, jewish_self_determination__diasporist_reading, stakes_inflation(individual), 2025, 0.62).
narrative_ontology:measurement(jewi_grid_21, jewish_self_determination__diasporist_reading, stakes_inflation(organizational), 1945, 0.25).
narrative_ontology:measurement(jewi_grid_22, jewish_self_determination__diasporist_reading, stakes_inflation(organizational), 2025, 0.71).
narrative_ontology:measurement(jewi_grid_23, jewish_self_determination__diasporist_reading, stakes_inflation(structural), 1945, 0.3).
narrative_ontology:measurement(jewi_grid_24, jewish_self_determination__diasporist_reading, stakes_inflation(structural), 2025, 0.58).
narrative_ontology:measurement(jewi_grid_25, jewish_self_determination__diasporist_reading, suppression(class), 1945, 0.1).
narrative_ontology:measurement(jewi_grid_26, jewish_self_determination__diasporist_reading, suppression(class), 2025, 0.52).
narrative_ontology:measurement(jewi_grid_27, jewish_self_determination__diasporist_reading, suppression(individual), 1945, 0.18).
narrative_ontology:measurement(jewi_grid_28, jewish_self_determination__diasporist_reading, suppression(individual), 2025, 0.65).
narrative_ontology:measurement(jewi_grid_29, jewish_self_determination__diasporist_reading, suppression(organizational), 1945, 0.25).
narrative_ontology:measurement(jewi_grid_30, jewish_self_determination__diasporist_reading, suppression(organizational), 2025, 0.78).
narrative_ontology:measurement(jewi_grid_31, jewish_self_determination__diasporist_reading, suppression(structural), 1945, 0.4).
narrative_ontology:measurement(jewi_grid_32, jewish_self_determination__diasporist_reading, suppression(structural), 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__diasporist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the kernel 'jewish_self_determination'. All five readings share the kernel—a contested commitment about what constitutes legitimate Jewish self-determination—but instantiate different constraints with different epsilon values, different beneficiary/victim structures, and different classifications. The diasporist reading here (piton) emphasizes the atrophy of diaspora institutions under Zionist hegemony; the liberal_nationalist reading (rope/tangled_rope) emphasizes equal national self-determination; the indigenous_return reading (rope/tangled_rope) emphasizes territorial connection; the religious_covenant reading (rope/scaffold) emphasizes divine obligation; the settler_colonial reading (snare) emphasizes dispossession. Each is authored as a separate constraint story. They are linked by network.affects_constraints because each reading's institutional viability affects the viability of the others—if diasporist institutions strengthen, they create structural pressure on Zionist institutional monopoly; if indigenous-return reading gains legitimacy, it undermines diasporist claims, etc.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__diasporist_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
