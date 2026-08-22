% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Covenant Title to Eretz Yisrael (Religious Zionist Reading)
 *   domain: political philosophy/nationalism/theology
 *
 * SUMMARY:
 *   The religious zionist reading grounds Jewish political claims to
 *   Palestine/Eretz Yisrael in divine covenant promise articulated in Hebrew
 *   scripture (Torah, Tanakh). Under this reading, the land is non-negotiable
 *   — it is divinely promised to the Jewish people as an eternal inheritance;
 *   statehood is the fulfillment of that promise, not merely a political
 *   achievement subject to negotiation or partition. Palestinian Arabs are
 *   treated as subordinate claimants whose presence and claims are secondary
 *   to the inalienable covenant title. This reading has shaped Israeli
 *   constitutional law (no written constitution, but the Law of Return, Basic
 *   Laws treating the state as inherently Jewish), settlement policy beyond
 *   1967 armistice lines, and the politics of territorial maximalism. The
 *   constraint is CLAIMED as tangled_rope (genuine coordination of diaspora
 *   Jewry around a shared goal, plus asymmetric extraction from Palestinian
 *   inhabitants) and the metrics author high extractiveness, substantial
 *   suppression, and mounting theater (the coordination function plateaus
 *   while the theater of 'security' and 'Jewish demographic necessity'
 *   grows).
 *
 * KEY AGENTS:
 *   - Jewish people as covenant community (identity-locked beneficiary; collective agent fused with territorial claim)
 *   - Palestinian Arabs and non-Jewish inhabitants (trapped payers; territorial claims subordinate by theological definition)
 *   - Religious zionist institutions and movements (agenda-setters; articulate and defend the reading, shape Israeli state law)
 *   - Liberal nationalist and post-zionist competitors (excluded from the reading's authority structure; would reframe the claim in secular terms)
 *   - International law regime (observer; contested by divine-title invocation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.89).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.76).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Covenant Title to Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political philosophy/nationalism/theology").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '4f4d8741-4eb1-448f-9940-fb7e8a03f017').
narrative_ontology:cs_kernel_codification('4f4d8741-4eb1-448f-9940-fb7e8a03f017', fixed_text).
narrative_ontology:cs_authority_grounding('4f4d8741-4eb1-448f-9940-fb7e8a03f017', lineage).
narrative_ontology:cs_interpretation_layer_present('4f4d8741-4eb1-448f-9940-fb7e8a03f017').
narrative_ontology:cs_reading_relation('4f4d8741-4eb1-448f-9940-fb7e8a03f017', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('4f4d8741-4eb1-448f-9940-fb7e8a03f017', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('4f4d8741-4eb1-448f-9940-fb7e8a03f017', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('4f4d8741-4eb1-448f-9940-fb7e8a03f017', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_axiom('4f4d8741-4eb1-448f-9940-fb7e8a03f017', foundational, covenant_inalienable_title_to_eretz_yisrael).
narrative_ontology:cs_axiom_status(covenant_inalienable_title_to_eretz_yisrael, holdable).
narrative_ontology:cs_axiom_grounding('4f4d8741-4eb1-448f-9940-fb7e8a03f017', covenant_inalienable_title_to_eretz_yisrael, theological).
narrative_ontology:cs_axiom('4f4d8741-4eb1-448f-9940-fb7e8a03f017', foundational, jewish_peoplehood_constituted_by_territorial_claim).
narrative_ontology:cs_axiom_status(jewish_peoplehood_constituted_by_territorial_claim, holdable).
narrative_ontology:cs_axiom_grounding('4f4d8741-4eb1-448f-9940-fb7e8a03f017', jewish_peoplehood_constituted_by_territorial_claim, deontological).
narrative_ontology:cs_reference_frame('4f4d8741-4eb1-448f-9940-fb7e8a03f017', covenant_torah_promise_eternal_title).
narrative_ontology:cs_drift_state('4f4d8741-4eb1-448f-9940-fb7e8a03f017', contemporary_post_1967_occupation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4f4d8741-4eb1-448f-9940-fb7e8a03f017', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs_in_mandate_territory).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, non_jewish_inhabitants_post_1948).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, covenant_theology_divine_sovereignty).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, jewish_historical_nexus_to_land).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collective bearer of the covenant promise articulated in Hebrew scripture; collective identity fused with territorial claim to Eretz Yisrael. The religious zionist reading constitutes Jewish peoplehood as inseparable from the claim — return and sovereignty are not optional political achievements but theological imperatives tied to Jewish identity itself. No exit from identity as covenant people without dissolving the claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community, agenda_setter).

% Inhabitants of Palestine under Ottoman and then British Mandate (1917–1948) whose presence, property claims, and political self-determination are subordinate to or incompatible with the covenant claim under this reading. The reading does not deny their existence but treats their territorial claims as secondary to the inalienable Jewish covenant title. They bear the cost of territorial displacement, dispossession, and political subordination as the covenant claim is asserted.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs_in_mandate_territory, payer,
    organized, generational, trapped, regional).

% Palestinians and other non-Jewish inhabitants remaining in 1948 Israel and territories claimed under the reading; also refugees and displaced persons from 1948 and 1967. Under the religious zionist reading, their presence is tolerated as a subordinate fact within a Jewish-majority covenant state, but their claims to return, property restitution, or equal political voice are subordinated to the inalienable Jewish claim. Exit options are limited: acceptance of subordinate status, departure, or resistance.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, non_jewish_inhabitants_post_1948, payer,
    powerless, biographical, constrained, regional).

% Religious zionist movements, parties, and institutions (Mizrachi, Bnei Akiva, National Religious Party, settler movements, yeshivot) that articulate, teach, and defend the covenant-claim reading. They set the theological and political agenda for territorial boundaries, settlement policy, and the constitutional framing of the state as Jewish-majority. They are invested in maintaining the reading's dominance in Israeli public law and discourse.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_institutions, agenda_setter,
    institutional, civilizational, identity_locked, national).

% The post-1945 international legal order based on self-determination, territorial sovereignty, and equal standing of states. The religious zionist reading invokes divine title over international law, claiming the covenant claim overrides territorial partition and minority-rights instruments. International law institutions serve as a seat of contention with this reading but are not parties collecting or paying.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_law_regime, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(jewish_sovereignty_palestine__religious_zionist_reading, international_law_regime).

% Jewish political movements and intellectuals (Labor Zionists, liberal nationalists, secular Israelis) who ground Jewish statehood in secular self-determination rights rather than divine covenant. They would argue for territorial partition, minority rights, and international law recognition rather than inalienable covenant claim. They are excluded from the religious zionist reading's fundamental premise.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, liberal_nationalist_competitors, excluded,
    organized, generational, constrained, national).

% Israeli and Palestinian scholars, activists, and movements who argue the zionist project, regardless of intent, has instantiated a settler-colonial displacement regime. They would demand de-linking of Jewish identity from state nationalism, right of return for Palestinians, and civic rather than ethnic-national statehood. They are excluded from participation in the religious zionist reading's authority structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, post_zionist_critics, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_as_covenant_community).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orients Jewish identity and collective political action toward territorial self-determination in the ancestral homeland, unifying diaspora communities and internally fractured Jewish movements around a shared theological and political goal: the establishment of Jewish sovereignty in Eretz Yisrael as the fulfillment of covenant promise.
% TRANSFER_FUNCTION: Moves Palestinian territorial claims, property rights, and political standing from primary to subordinate status — the land itself transfers in legal/theological title from the previous inhabitants to the Jewish covenant community, and political authority transfers to Jewish-majority institutions claiming theological legitimacy for that transfer.
% ABSENT_VOICES: Palestinians and other indigenous inhabitants who would testify to continuous presence and prior claim; liberal nationalist Zionists who would ground the claim in secular self-determination rather than divine covenant; post-zionist critics who would challenge the entire settler-colonial framework; international law regimes that would contest the invocation of divine title over the territorial sovereignty principle.
% DISAPPEARANCE_RATIONALE: If this reading — the claim that the covenant grounds inalienable Jewish title and statehood is theological fulfillment — were to disappear as a governing constitutional principle, Israeli law would reframe Jewish statehood as secular self-determination or civic nationality rather than covenant fulfillment; territorial boundaries would become negotiable under international law and self-determination principles; Palestinian claims to return and property restitution would move from illegitimate subordinate claims to equal bargaining positions in a post-ethnic-nationalist framework.
% FOUNDING_PROBLEM: Diaspora Jewish communities lack territorial security and self-determination; Jewish existence is precarious in gentile-majority states; Jewish identity threatens assimilation without a territorial center and collective sovereign authority to organize Jewish life according to Jewish law and culture.
% FOUNDING_PROBLEM_CORROBORATION: Religious zionist institutions and contemporary Israeli government statements attest the founding problem remains live: Diaspora Jewry faces persistent antisemitism and existential vulnerability; Israel is necessary as the refuge and sovereign expression of Jewish peoplehood. Secular historians and post-zionist scholars contest this framing: they argue the founding problem was addressed by 1950 (statehood achieved) and the theological reading now serves territorial maximalism and Palestinian subordination rather than Jewish security. Palestinian institutions attest they never agreed the founding problem justified their displacement. Liberal internationalist sources attest the problem was addressed through international law mechanisms (refugee protection, self-determination) without requiring a territorial claim over inhabited land.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.72 to 0.89 over the interval (t=0 to t=100, mapped to 1917–2024) because the reading's territorial claims expand — from the initial Balfour Declaration aspiration (coordination around diaspora return) to post-1967 settlement maximalism (pure extraction of Palestinian land and subordination). The suppression metric rises from 0.61 to 0.76 because the constraint's persistence requires increasingly active state enforcement: military occupation, settlement expansion, legal discrimination, and suppression of Palestinian political alternatives. The theater ratio plateaus around 0.42 because while early Zionism could point to genuine coordination achievements (building communities, creating institutions, establishing statehood by 1948), post-1967 Israeli state action increasingly defends territorial maximalism and Palestinian subordination rather than Jewish security — the rhetoric of 'security' and 'Jewish demographic necessity' performs the function of legitimation, but the underlying dynamics are extractive. The measurement series uses one shared time grid (every metric authored at every time point) so temporal analysis is aligned.
 *
 * PERSPECTIVAL GAP:
 *   The religious zionist institutional seat and the agenda-setter seat compute the constraint as coordination — organizing Jewish identity around a shared goal, fulfilling a covenant, establishing collective self-determination. The Palestinian payer seats compute it as pure extraction — territorial dispossession, political subordination, enforced displacement. The engine computes these divergences from the structural data: beneficiary with identity-locked exit and civilizational time horizon (Jewish people) versus powerless and organized payers with trapped/constrained exit (Palestinians). The seated divergence is structural, not opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish people as covenant community: identity-locked exit means no agent can exit the claim without dissolving Jewish identity (under this reading); organized power means capacity to mobilize diaspora resources; beneficiary role means the claim yields Jewish sovereignty and territorial control. Directionality near 0.0 (full beneficiary). Palestinian Arabs: trapped exit (cannot leave occupied territory without abandonment); organized-to-powerless power depending on time period; payer role means they bear territorial loss and political subordination. Directionality near 1.0 (full target). The reading's theological frame locks both sides into the identity category — Jewish identity IS the covenant claim, Palestinian identity IS the dispossessed territorial presence — making identity the binding mechanism of extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora Jewish vulnerability, lack of self-determination) is LIVE in the religious zionist reading — the reading treats Jewish precarity as permanent and constitutive, justifying perpetual territorial maximalism and Palestinian subordination as necessary consequences of Jewish survival. However, the disappearance verdict is world_rearranges: if the divine covenant reading were rejected, statehood would reframe as secular self-determination, territorial boundaries would become negotiable, and Palestinian claims would move to equal footing. The divergence signals mandatrophy: the reading justifies territorial maximalism as covenant fulfillment, but the founding problem (security) was largely addressed by 1950. Post-1967 expansion is not addressing the founding problem — it is extending the extraction and theater to defend territorial claims that no longer serve the original coordination goal. The constraint persists because it benefits the agenda-setter and because identity fusion locks the beneficiary into it, not because the founding problem requires it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_theology_interpretive_authority,
    'Who has the authority to interpret the divine covenant and what land area it encompasses? Is the interpretation binding on all Jews, or do alternative interpretive traditions (reform, conservative, secular, post-zionist Jewish) have standing to reject or reframe it?',
    'Survey Jewish communities and theologies to identify how many Jewish traditions recognize the religious zionist interpretation as normative versus how many contest or reframe it. Track changes in Israeli law''s treatment of alternative Jewish readings.',
    'If the religious zionist interpretation is the consensus reading of the covenant, the constraint''s theological grounding is stronger. If alternative Jewish readings are live and organized, the covenant itself becomes a contested kernel, not a settled ground for the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_theology_interpretive_authority, conceptual, 'Whether the religious zionist reading monopolizes covenant interpretation or shares authority with other Jewish traditions.').

omega_variable(
    palestinian_subordination_theological_necessity,
    'Is Palestinian political subordination and territorial displacement a necessary theological consequence of the covenant claim, or is it a contingent political outcome that a different implementation could avoid?',
    'Examine religious zionist theology to identify whether texts and authorities claim Palestinian subordination is mandatory or whether they permit egalitarian arrangements within the covenant framework. Test whether post-zionist or bi-national Jewish readings can access the same theological texts.',
    'If subordination is theologically mandated, the constraint is tangled_rope with permanent extraction (no partition legitimacy, no Palestinian equality). If subordination is contingent political choice, the constraint could be reframed as rope (coordination around covenant) with negotiable territorial/political implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(palestinian_subordination_theological_necessity, conceptual, 'Whether Palestinian subordination is theologically necessary or politically contingent.').

omega_variable(
    internalized_identity_lock_mechanism,
    'Is the measured suppression structural (military occupation, legal discrimination, barriers to Palestinian exit) or internalized (Palestinian acceptance of Jewish right to the land, loss of counter-narrative resources, identity fusion of ''being Palestinian'' with land loss)?',
    'Post-peace-agreement trajectory: if Palestinian political alternatives are opened and Palestinians maintain organized resistance, suppression was primarily structural. If resistance collapses under continued structural openness, suppression is partly internalized.',
    'If suppression is internalized, the constraint''s effective coercive force is higher than the structural measure suggests, and it persists as extraction even if structural barriers were removed. If structural, removing barriers would enable Palestinian alternatives and reduce effective suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_identity_lock_mechanism, empirical, 'Structural versus internalized suppression mechanism in the Palestinian payer position.').

omega_variable(
    sibling_reading_forecast,
    'Which sibling reading (liberal_nationalist, settler_colonial, post_zionist, cultural_zionist) is structurally most likely to become the dominant reading of the jewish_sovereignty_palestine kernel in the next 50 years?',
    'Monitor Israeli state policy, institutional decisions, and Jewish diaspora consensus. Track whether territorial expansion continues or reverses; whether Palestinian political claims gain standing in Israeli law; whether Jewish identity detaches from territorial maximalism.',
    'If liberal_nationalist reading gains dominance, extractiveness drops and partition becomes legitimate. If post_zionist reading gains dominance, statehood persists but frames as post-ethnic pluralism, making Palestinian equality a state-constitutive principle. If settler_colonial reading gains international consensus, the constraint''s legitimacy collapses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_forecast, preference, 'Long-term trajectory of the kernel''s dominant reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t16, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t16, observed).
narrative_ontology:measurement(jewi_tr_t32, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement_basis(jewi_tr_t32, observed).
narrative_ontology:measurement(jewi_tr_t48, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 48, 0.39).
narrative_ontology:measurement_basis(jewi_tr_t48, observed).
narrative_ontology:measurement(jewi_tr_t64, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 64, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t64, observed).
narrative_ontology:measurement(jewi_tr_t80, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t80, observed).
narrative_ontology:measurement(jewi_tr_t100, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t16, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement_basis(jewi_be_t16, observed).
narrative_ontology:measurement(jewi_be_t32, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 32, 0.83).
narrative_ontology:measurement_basis(jewi_be_t32, observed).
narrative_ontology:measurement(jewi_be_t48, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 48, 0.86).
narrative_ontology:measurement_basis(jewi_be_t48, observed).
narrative_ontology:measurement(jewi_be_t64, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 64, 0.88).
narrative_ontology:measurement_basis(jewi_be_t64, observed).
narrative_ontology:measurement(jewi_be_t80, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 80, 0.89).
narrative_ontology:measurement_basis(jewi_be_t80, observed).
narrative_ontology:measurement(jewi_be_t100, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 100, 0.89).
narrative_ontology:measurement_basis(jewi_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t16, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement_basis(jewi_su_t16, observed).
narrative_ontology:measurement(jewi_su_t32, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(jewi_su_t32, observed).
narrative_ontology:measurement(jewi_su_t48, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 48, 0.73).
narrative_ontology:measurement_basis(jewi_su_t48, observed).
narrative_ontology:measurement(jewi_su_t64, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 64, 0.75).
narrative_ontology:measurement_basis(jewi_su_t64, observed).
narrative_ontology:measurement(jewi_su_t80, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement_basis(jewi_su_t80, observed).
narrative_ontology:measurement(jewi_su_t100, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 100, 0.76).
narrative_ontology:measurement_basis(jewi_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__religious_zionist_reading, 0.22).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, palestine_self_determination_claim).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_right_of_return).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel jewish_sovereignty_palestine. The kernel is the divine promise in Torah/Tanakh and the historical presence of the Jewish people in the territory; different reading traditions (religious zionist, liberal nationalist, settler colonial, post-zionist, cultural zionist) interpret this kernel differently and produce different constraint stories with different beneficiary structures and different extractiveness profiles. The religious zionist reading treats the covenant as absolute and inalienable; the liberal nationalist reading treats the covenant as one among many self-determination claims; the post-zionist reading treats the covenant as a cover story for settler colonialism. Each reading is a separate constraint story linked by network.affects_constraints. The divergence is not an observable-selection problem — the readings produce genuinely different ε values because they define the beneficiary structure differently (only Jewish people vs. both Jewish and Palestinian; only covenant vs. self-determination; colonial logic vs. indigenous justice). See omegas for the interpretive authority and theological necessity questions that distinguish the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__religious_zionist_reading, organized, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
