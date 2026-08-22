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
 *   human_readable: Zionist National Liberation Legitimacy Doctrine
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint captures the legitimacy doctrine framing Zionism as a
 *   national liberation movement for a historically persecuted people
 *   returning to an ancestral homeland. This is ONE READING of a contested
 *   kernel: the same historical events and territorial claim are read
 *   alternatively as settler colonialism (displacing indigenous Arab
 *   inhabitants) or as religious restoration (fulfillment of divine promise).
 *   This story instantiates the NATIONAL LIBERATION READING specifically: it
 *   authorizes displacement on grounds of Jewish persecution history and
 *   claims to ancestral connection, delegitimizes Palestinian Arab opposition
 *   as denial of Jewish rights, and embeds asymmetric legal status (Law of
 *   Return for Jews, no Palestinian return right) into governance. The
 *   reading has been institutionalized globally through recognition of
 *   Israel, Western support, and educational frameworks that teach the return
 *   narrative as natural-law restoration rather than as a contestable
 *   political choice.
 *
 * KEY AGENTS:
 *   - Jewish population in Israel: primary beneficiaries; identity fused with the return narrative; exit would require abandoning state nationality and security framing
 *   - Zionist institutional leadership: agenda-setter; administers law, education, memory institutions that embed the doctrine
 *   - Palestinian Arabs displaced (1948 and after): primary victims; barred from return by law; no legal standing within the framework that justifies their dispossession
 *   - Palestinian citizens within Israel: secondary victims/excluded; live in a state whose legitimacy centers on Jewish ethno-national self-determination; secondary citizenship status embedded in institutions
 *   - Western liberal states: secondary beneficiaries; provide diplomatic/military/economic support partly on the basis of this reading; could shift to alternatives but would require renegotiating legitimacy
 *   - Arab states and Palestinian authorities: payers/observers; contest the reading but constrained by asymmetric power; experience doctrine as foreclosing their own territorial claims
 *   - Anti-Zionist Jewish voices: structurally excluded; reveal the reading is internally contested; advocate diaspora identity, shared citizenship, refugee return
 *   - Indigenous rights framework advocates: analytical observers; highlight the structural contradiction between 'indigenous rights' and 'settler colonialism' when applied to this case
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
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist National Liberation Legitimacy Doctrine").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, 'e8206c41-c4ee-409e-ba2c-0cd61c68a841').
narrative_ontology:cs_kernel_codification('e8206c41-c4ee-409e-ba2c-0cd61c68a841', formalized).
narrative_ontology:cs_authority_grounding('e8206c41-c4ee-409e-ba2c-0cd61c68a841', extraction).
narrative_ontology:cs_interpretation_layer_present('e8206c41-c4ee-409e-ba2c-0cd61c68a841').
narrative_ontology:cs_reading_relation('e8206c41-c4ee-409e-ba2c-0cd61c68a841', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('e8206c41-c4ee-409e-ba2c-0cd61c68a841', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('e8206c41-c4ee-409e-ba2c-0cd61c68a841', foundational, jewish_indigenous_restoration_justified).
narrative_ontology:cs_axiom_status(jewish_indigenous_restoration_justified, holdable).
narrative_ontology:cs_axiom_grounding('e8206c41-c4ee-409e-ba2c-0cd61c68a841', jewish_indigenous_restoration_justified, deontological).
narrative_ontology:cs_axiom('e8206c41-c4ee-409e-ba2c-0cd61c68a841', foundational, persecution_history_overrides_prior_inhabitance).
narrative_ontology:cs_axiom_status(persecution_history_overrides_prior_inhabitance, holdable).
narrative_ontology:cs_axiom_grounding('e8206c41-c4ee-409e-ba2c-0cd61c68a841', persecution_history_overrides_prior_inhabitance, empirically_contingent).
narrative_ontology:cs_reference_frame('e8206c41-c4ee-409e-ba2c-0cd61c68a841', jewish_diaspora_security_and_self_determination).
narrative_ontology:cs_drift_state('e8206c41-c4ee-409e-ba2c-0cd61c68a841', contemporary_post_1967_settlement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e8206c41-c4ee-409e-ba2c-0cd61c68a841', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, jewish_population_israel).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, zionist_institutional_leadership).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs_displaced).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_citizens_within_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, western_liberal_states).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_palestinian_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish population in Israel benefits from the legitimacy doctrine that justifies their presence, legal status, and state resources. Exit would require abandoning state nationality and security guarantees framed as essential to group survival after historical persecution. Identity is fused with the founding narrative: Israeli Jewish identity is institutionally constructed around the 'return' story and persecution recovery.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, jewish_population_israel, beneficiary,
    organized, generational, identity_locked, national).

% Sets and enforces the national liberation narrative as the binding legitimacy claim for the Israeli state. Administers institutions (law, education, public memory) that embed this reading into governance. Could in principle adopt alternative framings but would lose the foundational narrative that sustains institutional authority. Collects state authority and international recognition through this doctrine.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_institutional_leadership, agenda_setter,
    institutional, generational, mobile, global).

% Displaced from territories claimed as ancestral Jewish homeland under the national liberation doctrine. Barred from return by law (Law of Return applies to Jews only). Their displacement is framed within the legitimacy narrative as a tragic but necessary cost of Jewish national self-determination. No legal recourse within the framework that justifies their dispossession.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_arabs_displaced, payer,
    powerless, generational, trapped, regional).

% Arab citizens of Israel live within a state whose legitimacy doctrine centers on Jewish return and Jewish national self-determination. Citizenship is granted but secondary to the ethno-national narrative. Education and public institutions transmit the legitimacy doctrine that frames their own presence as an exception or demographic problem. Legal equality exists on paper; political legitimacy hierarchies embed the reading into daily governance.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_citizens_within_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, palestinian_citizens_within_israel, excluded).

% Recognize Israeli statehood and provide diplomatic, military, and economic support partly on the basis of the national liberation narrative (Jewish self-determination following genocide, return from exile). This reading aligns with liberal nationalism and minority rights frameworks; it simplifies their own moral accounting. Could shift to alternative readings but would require renegotiating their relationship to the historical legitimacy claim.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, western_liberal_states, beneficiary,
    institutional, generational, mobile, global).

% Experience the national liberation doctrine as the legitimacy claim that forecloses their own territorial claims and native-inhabitance arguments. They contest the reading (asserting Palestinian indigeneity and pre-1948 occupation rights) but cannot unilaterally revoke the institutional framework embedded in Israeli law and international recognition. Constrained by asymmetric power to change the binding narrative.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_palestinian_authorities, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, arab_states_and_palestinian_authorities, observer).

% Jewish communities and intellectuals who reject the national liberation reading or advocate alternative frameworks (diaspora rights, non-ethnic citizenship, shared sovereignty, return of refugees). Excluded from the institutional consensus and often framed as delegitimizing Jewish self-determination or denying historical persecution. Their presence reveals the reading is internally contested, not universal among the beneficiary group.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, anti_zionist_jewish_voices, excluded,
    moderate, biographical, mobile, global).

% Academic and advocacy networks engaged in comparative analysis of indigenous rights, settler colonialism, and return narratives. Observe the constraint structurally as a case where 'indigenous rights' and 'settler colonialism' framings yield contradictory classifications. No direct stake in the outcome but central to documenting the constraint's structural ambiguity.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, indigenous_rights_framework_advocates, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, zionist_institutional_leadership).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a collective identity and state framework for a diaspora Jewish population seeking security and self-determination after centuries of persecution and genocide. Solves the coordination problem of building a state from dispersed, politically fragmented communities by centering a shared narrative of return and restoration.
% TRANSFER_FUNCTION: Transfers land, legal status, and political legitimacy from Arab inhabitants (who occupied the territory for ~1300 years continuously before 1948) to the Jewish population establishing sovereignty. The doctrine frames this transfer as justified restoration of historical rights rather than as displacement.
% ABSENT_VOICES: Palestinian Arabs were structurally excluded from the founding deliberations that established the doctrine; their indigeneity claims and continuous habitation were not recognized as competing rights within the framework. Anti-Zionist Jewish voices and alternative Jewish frameworks (diaspora identity, non-ethnic citizenship models) are also excluded from the institutional consensus.
% DISAPPEARANCE_RATIONALE: If the national liberation legitimacy doctrine disappeared, Israeli state authority would require alternative justifications (purely civic, territorial conquest, strategic geography); Palestinian return and refugee claims would lose their primary counter-narrative ground; the legal asymmetries (Law of Return, settlement policy, dispossession law) would become harder to defend within liberal frameworks. The entire geopolitical settlement would reorganize around contested legitimacy.
% FOUNDING_PROBLEM: The persecution of European Jews in the 19th–20th centuries (culminating in the Holocaust) created a diaspora population seeking refuge and self-determination. The doctrine arose to answer: how can dispersed, historically stateless people establish a secure sovereign state with collective self-determination?
% FOUNDING_PROBLEM_CORROBORATION: The historical fact of persecution is attested by historians and survivor testimony outside the benefiting parties. The CLAIM that this justifies displacement is contested: Palestinian historians and scholars attest that displacement of indigenous inhabitants is not a legitimate response to third-party persecution; international humanitarian law scholars argue refugee return rights supersede settler security claims; even sympathetic scholars (e.g., Benny Morris, Ilan Pappe) acknowledge the founding-problem-to-doctrine chain required erasing Palestinian agency and prior rights.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at interval end) because the doctrine transfers land and political legitimacy from Arab inhabitants to the Jewish population, justified by historical persecution and claimed ancestral connection rather than by benefiting both groups or by compensating those displaced. The measurement series shows EXTRACTION ACCUMULATION over 75 years: from 0.45 (early, when displacement was still incomplete and return was nascent) to 0.68 (contemporary, with dispossession law, settlement expansion, and administrative discrimination institutionalized). Suppression is similarly high and rising: the doctrine requires active enforcement against competing claims (Palestinian return, anti-colonial readings, alternative Jewish frameworks). This rising trajectory reflects the constraint becoming more extractive as time passes—the founding-problem rationale (persecution recovery) becomes less temporally pressing (survivors age out, Israel becomes a stable state) while the extraction mechanisms (land law, settlement, demographic control) become more elaborate and harder to justify on coordination grounds alone. Theater rises moderately: increasing share of enforcement activity defends the doctrine symbolically (education, memorial, international diplomacy) rather than functionally (the coordination problem was solved decades ago—current theater maintains institutional consensus against rising contestation). The constraint claims to be TANGLED ROPE (genuine coordination + asymmetric extraction): it genuinely coordinated a diaspora population into state formation; simultaneously it extracted land and rights from Palestinians by framing their displacement as justified by the coordination's founding narrative. Both elements are structural. The rising metrics reflect the extraction component becoming more visible and the coordination component more historical.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary and agenda-setter seats compute this constraint differently from payer seats. From the Jewish Israeli seat: this is genuine national liberation (coordination function is real and substantial; founding problem of diaspora security was solved). From the Palestinian displaced seat: this is pure extraction masked as liberation rhetoric (the coordination solved a third-party problem at their expense; their dispossession was not part of any coordination they consented to). From the Western liberal seat: national self-determination for a persecuted minority (coordination frame dominates; Palestinian claims are secondary). From the anti-Zionist Jewish seat: enforced ethnic nationalism using Holocaust trauma as cover for territorial conquest (theater is high, extraction is transparent). The engine computes these divergences from the structural data—beneficiary/victim declarations, power atoms, exit options, scope. The authored claim (tangled_rope) reflects the structural fact that BOTH coordination and extraction are present; the metrics' rising trend reflects the extraction component becoming increasingly central to persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish Israelis: beneficiaries (institutional identity, state resources, legal primacy); powerful organized power atom, identity_locked exit (founding narrative is fused with state and personal identity; exit would mean abandoning citizenship, Hebrew language, community, security framing). Directionality near 0.2–0.3 (strong beneficiary side): the constraint subsidizes them. Zionist institutional leadership: agenda-setters (administers the doctrine); institutional power, mobile exit (could shift to alternative narratives but would lose foundational authority). Directionality ~0.4 (mild beneficiary, controls the frame). Palestinian displaced: victims (dispossessed); powerless, trapped exit (no legal recourse, no return right, no state of their own within the doctrine's framework). Directionality ~0.95 (full target): the constraint extracts from them with near-total suppression of alternatives. Palestinian citizens: payers (secondary status, daily discrimination); moderate power, constrained exit (can leave Israel but at high identity/community cost). Directionality ~0.75 (strong target). Western liberal states: secondary beneficiaries (simplified moral accounting, strategic ally in Middle East); institutional power, mobile exit (could recognize Palestinian state or shift to post-Zionist framing but geopolitical cost is high). Directionality ~0.35 (mild beneficiary). Arab states: payers/observers; organized power, constrained exit (recognize Israel under duress, cannot unilaterally change doctrine). Directionality ~0.65 (target but with retained power). Anti-Zionist Jews: excluded; moderate power, mobile exit (can migrate to diaspora or post-Zionist frameworks). Directionality ~0.5 (symmetric—neither full beneficiary nor full target, but forcibly outside the consensus). No directionality overrides needed; the structural data (declared beneficiaries/victims + exit options) generates accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy test CLEARLY: the founding problem (diaspora security after persecution) is DEAD as a live coordination problem for most of the interval—Israel is a stable state by the 1960s, with military, economy, and international recognition secured. Yet the constraint persists with rising extraction (dispossession law, settlement expansion, administrative discrimination). This is the classic mandatrophy signature: founding problem dead, extraction mechanism alive. The theater ratio rising (from 0.22 to 0.41) while functional coordination is complete supports this reading. The constraint should be reclassifiable from TANGLED ROPE toward PITON (atrophied function, theatrical persistence) or SNARE (the coordination cover story is no longer operative; what remains is pure extraction defended by historical memory ritual). However, this reading instantiates the national liberation frame, which still holds the founding problem as LIVE—the persistence of antisemitism, the threat of displacement, the need for Jewish self-determination. The mandatrophy divergence reflects the kernel contest itself: the reading that holds the problem as live also holds the extraction as necessary cost; the sibling readings (settler-colonial, religious restoration) read the same persistence as mandatrophy and decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_claim_ambiguity,
    'Do Jews constitute an indigenous people with historical claim to the territory, or are they a diaspora population seeking refuge in a territory already inhabited by another indigenous people?',
    'Archaeological and historical evidence of continuous Jewish habitation vs. continuous Palestinian habitation; comparative indigenous-rights framework analysis of what constitutes indigenous status (descent, continuous presence, cultural maintenance, pre-dispossession occupation).',
    'If Jews are indigenous, the national liberation reading is strengthened and displacement can be framed as restoration. If Palestinians are indigenous and Jews are diaspora, the settler-colonial reading is strengthened and displacement is illegitimate. The constraint''s core legitimacy hinges on this classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_claim_ambiguity, conceptual, 'The foundational ambiguity: which population is indigenous and which is settler/diaspora. Archaeological evidence exists for both Jewish presence in antiquity and continuous Arab presence in medieval-modern period.').

omega_variable(
    persecution_justification_scope,
    'Does historical persecution of Jews in Europe and the Middle East justify displacing a third population (Palestinian Arabs) who did not perpetrate the persecution?',
    'International humanitarian law doctrine on refugee rights, reparations ethics, and innocent-party protections; comparative case analysis of whether persecution of group A justifies harm to unrelated group B.',
    'If persecution justifies displacement of uninvolved third parties, the doctrine''s extraction is ethically legitimate within its own frame. If persecution creates refugee rights but not displacement rights, the doctrine''s extraction requires additional justification and the constraint becomes snare-class (cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_justification_scope, preference, 'Normative question: whether historical trauma to one group justifies contemporary displacement of another.').

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (diaspora security, persecution recovery, Jewish self-determination) still live, or has it been substantially solved by 1967–2000 and the constraint now persists through inertia and extraction?',
    'Time-series analysis of antisemitism rates, Israeli state stability, military capacity, international recognition; comparison of founding-problem conditions in 1948 vs. 1967 vs. 2000 vs. 2026.',
    'If founding problem is live, the constraint retains tangled-rope status (coordination + justified extraction cost). If founding problem is dead and extraction persists, the constraint reclassifies to piton or snare, and the mandatrophy flag fires.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem that justified the constraint still motivates its persistence.').

omega_variable(
    suppression_mechanism_identity_fused,
    'Is the measured suppression (0.72) structural (legal barriers, military force, institutional exclusion) or internalized (Israeli Jews and Palestinians both accept the narrative as natural or inevitable)?',
    'Survey evidence on acceptance of the doctrine among Israeli Jews, Palestinians, and diaspora communities; post-exit behavior of people who migrate away from the framework (do they maintain the reading or abandon it?); education content analysis tracking narrative transmission.',
    'If suppression is mostly structural, removing the doctrine would open alternatives quickly. If mostly internalized, the constraint carries its suppression with people across borders and generations, making it harder to dislodge. Affects the cost-of-fixing estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_fused, empirical, 'Structural vs. internalized suppression mechanism in the persistence of the legitimacy doctrine.').

omega_variable(
    alternative_jewish_frameworks_excluded,
    'Why do anti-Zionist, post-Zionist, and diaspora Jewish frameworks remain excluded from institutional recognition despite representing a substantial minority within Jewish communities globally?',
    'Institutional analysis of how Israeli law, education, and diaspora organization enforce the national liberation reading as canonical; documentation of cost imposed on alternative frameworks; interviews with excluded voices on why their readings are foreclosed.',
    'If alternatives are excluded by active enforcement, the constraint''s suppression is higher than measured and its theater is performing unanimity that does not exist. If alternatives are naturally marginalized by preference, the exclusion is less coercive. Affects classification of extracted value: if suppression is high, extraction may exceed stated metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_jewish_frameworks_excluded, empirical, 'Why competing Jewish frameworks are structurally excluded from legitimacy within Israeli institutions.').

omega_variable(
    kernel_reading_foreclosure_relation,
    'Do the national liberation and settler-colonial readings genuinely foreclose one another (logically incompatible within any single framework), or do they coexist as different readings of the same facts held by different parties?',
    'Logical analysis of the core premises: national liberation presumes indigenous return; settler-colonialism presumes indigenous displacement. Can a single framework hold both ''this is indigenous restoration'' and ''this is indigenous displacement''? Framework analysis: can liberalism, religious tradition, or secular nationalism accommodate both readings without contradiction?',
    'If they foreclose (truly incompatible), the readings are in genuine logical conflict and one must be true. If they coexist (held by different parties without internal contradiction), the conflict is political, not logical, and both readings remain live. Affects the engine''s reading-relation classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_relation, conceptual, 'Whether the national liberation and settler-colonial readings logically foreclose one another or coexist as live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(zion_tr_t12, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(zion_tr_t25, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 25, 0.34).
narrative_ontology:measurement(zion_tr_t38, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 38, 0.38).
narrative_ontology:measurement(zion_tr_t50, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(zion_tr_t62, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 62, 0.41).
narrative_ontology:measurement(zion_tr_t75, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 75, 0.41).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(zion_be_t12, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(zion_be_t25, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement(zion_be_t38, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 38, 0.65).
narrative_ontology:measurement(zion_be_t50, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement(zion_be_t62, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 62, 0.68).
narrative_ontology:measurement(zion_be_t75, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(zion_su_t12, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(zion_su_t25, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(zion_su_t38, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 38, 0.68).
narrative_ontology:measurement(zion_su_t50, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(zion_su_t62, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 62, 0.72).
narrative_ontology:measurement(zion_su_t75, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(zionist_legitimacy_basis__national_liberation_reading, 0.12).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, palestinian_return_right_constraint).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, israel_law_of_return_asymmetry).

% DUAL FORMULATION NOTE:
% The constraint 'zionist_legitimacy_basis' is a contested kernel with three structurally distinct readings, each instantiating a different constraint with different ε values, victim sets, and types. This story represents the NATIONAL LIBERATION READING. The settler-colonial reading and religious restoration reading are authored separately with their own base_properties, stakeholder perspectives, and axioms. The three stories are linked via network.affects_constraints: each reading's doctrine influences the legitimacy conditions of the others' institutional persistence. The national liberation reading frames itself as indigenous restoration; the settler-colonial reading reads the same actions as illegitimate displacement; the religious restoration reading reads them as messianic fulfillment. These are not alternative observables of one constraint—they are different constraints with incompatible foundational axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zionist_legitimacy_basis__national_liberation_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
