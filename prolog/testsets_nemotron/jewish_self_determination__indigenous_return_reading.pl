% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigenous Return Reading of Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint story represents the indigenous_return_reading of the
 *   jewish_self_determination kernel: the claim that Jewish people are
 *   indigenous to the land (Eretz Yisrael) with an unbroken connection
 *   spanning millennia, making Zionism a decolonization movement — the return
 *   of an indigenous people to their ancestral homeland — rather than a
 *   colonial enterprise. The reading draws on archaeological continuity,
 *   genetic studies, liturgical and textual orientation toward the land, and
 *   the persistence of Jewish communities in the land through successive
 *   empires. It frames competing claims (particularly the
 *   settler_colonial_reading) as either denying Jewish history or
 *   subordinating it to a European colonial paradigm that misidentifies the
 *   actors. The constraint operates as a rope: it coordinates Jewish
 *   collective action (aliyah, state-building, defense) around a shared
 *   historical narrative that legitimizes sovereignty, while the coordination
 *   function is contested by rival readings that challenge its factual and
 *   moral premises. The extractiveness is low when the reading is internally
 *   accepted (indigenous status is binary — you are or you aren't) but rises
 *   in contexts where the claim must be actively defended against foreclosure
 *   by rival frameworks.
 *
 * KEY AGENTS:
 *   - jewish_people_as_indigenous_collective: Primary beneficiary (institutional/biographical/identity_locked) — receives legitimization of sovereignty claim, coordination for return and statehood
 *   - palestinian_arab_population: Excluded/co-indigenous with subordinate claim (organized/biographical/constrained) — presence acknowledged but framed as later arrival or co-indigenous with subordinate claim; not a victim in this reading's internal logic
 *   - zionist_movement_institutions: Agenda setter (institutional/generational/arbitrage) — administers the return, builds sovereignty structures, derives authority from the reading
 *   - international_legal_diplomatic_community: Observer (institutional/generational/analytical) — adjudicates competing indigeneity claims, recognizes or contests the reading
 *   - settler_colonial_reading_adherents: Excluded (organized/biographical/constrained) — would object to the reading's core premise but are not in the conversation from this reading's perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.18).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.35).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigenous Return Reading of Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, 'b67d67e4-1c36-4a28-a4c6-fa4355bd9a58').
narrative_ontology:cs_kernel_codification('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', distributed).
narrative_ontology:cs_authority_grounding('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', extraction).
narrative_ontology:cs_interpretation_layer_present('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58').
narrative_ontology:cs_reading_relation('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', foundational, jewish_people_are_indigenous_to_eretz_yisrael).
narrative_ontology:cs_axiom_status(jewish_people_are_indigenous_to_eretz_yisrael, holdable).
narrative_ontology:cs_axiom_grounding('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', jewish_people_are_indigenous_to_eretz_yisrael, empirically_contingent).
narrative_ontology:cs_axiom('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', foundational, zionism_is_decolonization_not_colonization).
narrative_ontology:cs_axiom_status(zionism_is_decolonization_not_colonization, holdable).
narrative_ontology:cs_axiom_grounding('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', zionism_is_decolonization_not_colonization, deontological).
narrative_ontology:cs_reference_frame('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', ancestral_indigenous_sovereignty).
narrative_ontology:cs_drift_state('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', post_1967_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b67d67e4-1c36-4a28-a4c6-fa4355bd9a58', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_people_as_indigenous_collective).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, jewish_indigeneity_to_eretz_yisrael).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, zionism_as_decolonization_not_colonization).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, unbroken_jewish_connection_to_land).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The collective subject of the reading: Jewish people worldwide who identify with the indigenous return narrative. They receive legitimization of their connection to the land, coordination for aliyah and state-building, and a moral framework that frames sovereignty as decolonization. Exit from this identity frame is identity_locked — the reading constitutes Jewish peoplehood itself; leaving it means leaving the collective self-understanding. The constraint provides the structural basis for Law of Return citizenship, settlement enterprise, and international advocacy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_people_as_indigenous_collective, beneficiary,
    institutional, generational, identity_locked, global).

% The Palestinian Arab population in the land (Israel, West Bank, Gaza) and in diaspora. In this reading's internal logic, they are not victims — their presence is framed as later arrival (post-7th century) or as co-indigenous with a subordinate claim (their connection is real but secondary to the prior Jewish claim). Structurally, they are excluded from the coordination function: the reading's legitimization of Jewish sovereignty does not coordinate their self-determination. Their exit options are constrained by Israeli military control, legal restrictions, and the reading's own denial of their equal indigeneity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_arab_population, excluded,
    organized, biographical, constrained, regional).

% The institutional apparatus (World Zionist Organization, Jewish Agency, Israeli state ministries, settlement enterprises) that administers the return, builds sovereignty structures, and derives authority from the indigenous return narrative. They set the agenda for how the reading is operationalized: where settlement occurs, who qualifies for return, how the narrative is deployed internationally. They have arbitrage-grade exit — they could pivot to liberal_nationalist_reading or religious_covenant_reading as legitimizing frameworks without losing their institutional position.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, zionist_movement_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% UN bodies, ICJ, ICC, EU, US State Department, and other international actors who adjudicate competing indigeneity claims. They do not collect from the constraint but their recognition or contestation shapes its structural effectuation. They observe the reading's deployment in Law of Return, settlement policy, Jerusalem recognition, and counter it with competing frameworks (settler_colonial_reading in UNHRC, liberal_nationalist_reading in Oslo framework). Their analytical seat has no exit — they must engage the contestation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_legal_diplomatic_community, observer,
    institutional, generational, analytical, global).

% Activists, scholars, international NGOs, and state actors who hold the settler_colonial_reading (Zionism as European settler-colonial project). They would object to this reading's core premise (Jewish indigeneity) but are excluded from this reading's conversation — the indigenous_return_reading does not engage their framework as legitimate, it forecloses it. Their exit options are constrained: they operate in international forums where this reading has limited traction, or in academic discourse where they contest it.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, settler_colonial_reading_adherents, excluded,
    organized, biographical, constrained, global).

% Jewish communities and organizations (e.g., Jewish Voice for Peace, Satmar Hasidim, Bundist descendants) who hold the diasporist_reading: Jewish survival via diaspora pluralism, not territorial sovereignty. They are excluded from this reading's coordination — the indigenous return reading treats diaspora as exile to be ended, not a valid form of Jewish life. They have mobile exit: they can maintain their framework independently, but they lose access to Israeli state resources and mainstream Jewish institutional recognition.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, diasporist_jewish_communities, excluded,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, jewish_people_as_indigenous_collective).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective action around return to ancestral land: aliyah, state-building, defense, Hebrew revival, and international advocacy are unified by the narrative that this is an indigenous people's decolonization, not a colonial project. The reading solves the coordination problem of 'why this land, why now, by what right' with a binary historical claim.
% TRANSFER_FUNCTION: Moves moral and legal authorization for sovereignty from the indigenous return narrative to Jewish collective institutions (Israeli state, Zionist organizations). The transfer is not material extraction from others in this reading's logic — it is the conversion of historical claim into political authority. The reading declares no transfer from Palestinians (their claim is subordinated, not extracted).
% ABSENT_VOICES: Palestinian voices asserting equal indigeneity and self-determination are structurally absent — the reading's premise (Jewish indigeneity with unbroken connection) does not accommodate equal Palestinian indigeneity. Diasporist Jewish voices rejecting sovereignty as the Jewish future are absent. Both would object if present; their absence is maintained by the reading's binary frame (indigenous return vs. exile/colonization).
% DISAPPEARANCE_RATIONALE: If the indigenous return reading vanished overnight, the primary legitimizing framework for Israeli sovereignty (Law of Return, settlement enterprise, Jerusalem claim) would lose its historical-moral foundation. The Israeli state would need to rely solely on liberal_nationalist_reading (self-determination as a nation among nations) or religious_covenant_reading (divine promise) — both of which carry different structural implications for borders, demographics, and international law. The Palestinian claim would lose its primary rival framework. The conflict's discursive structure would rearrange.
% FOUNDING_PROBLEM: Jewish statelessness, vulnerability to persecution in diaspora, and the inability to secure collective survival without territorial sovereignty — the 'Jewish Question' as posed in 19th-20th century Europe, compounded by the Holocaust and the expulsion of Jews from Arab lands.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by antisemites (who continue to target Jews), by diaspora Jewish insecurity (rising antisemitism in Europe and US post-2023), by the historical record of Jewish expulsion from Arab lands (1948-1970s), and by the continued existence of stateless populations globally. It is NOT corroborated solely by Zionist institutions — the problem's persistence is attested by adversaries of the reading as well. However, the reading's *expansion* beyond the founding problem (settlement beyond 1967, demographic engineering) is contested as mandatrophy by liberal_nationalist_reading and diasporist_reading adherents.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).
:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The reading claims mountain-like status (indigeneity as historical fact, binary and unchanging) but operates as a rope because its coordination function depends on active maintenance against rival readings that challenge its factual basis (genetic continuity, unbrokenness of connection) and moral conclusion (decolonization not colonization). Extractiveness is low (0.18) because the reading itself does not extract from others — it coordinates Jewish collective action around a return narrative. Suppression (0.35) reflects the discursive and institutional pressure to conform to the settler_colonial_reading in international forums, not structural barriers to the reading's own operation (Israel effectuates the reading via Law of Return, settlement, sovereignty). Theater ratio (0.22) captures the gap between the reading's binary historical claim and the complex demographic reality it must navigate — performative invocation of 'unbroken connection' against documented population shifts. Accessibility collapse (0.72) is high: once the indigenous return frame is accepted, alternative framings (liberal nationalist, diasporist) lose coherence as primary legitimizers. Resistance (0.58) is significant: the reading faces active contestation from settler_colonial_reading, diasporist_reading, and liberal_nationalist_reading adherents who reject its historical premises or moral conclusions.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish indigenous collective seat: the constraint is a mountain (historical fact) that enables rope-like coordination (return, sovereignty). From the Palestinian seat (excluded in this reading): the constraint operates as a snare — their indigeneity is denied or subordinated, their displacement justified. From the international observer seat: the constraint is a tangled rope — genuine coordination function for one people, asymmetric extraction from another, requiring active enforcement (Israeli state apparatus) to maintain. The engine computes per-seat classifications from the structural data; this reading's internal logic declares no victims, but the structural reality includes excluded parties who experience extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is the Jewish people as an indigenous collective — they receive the legitimization of their sovereignty claim and the coordination mechanism for return. The reading declares no victims (Palestinian presence reframed as later arrival or co-indigenous with subordinate claim). However, the structural reality includes the Palestinian population as an excluded party whose indigeneity claim is contested by this reading's premises. The directionality derivation would assign low d to Jewish collective (beneficiary), high d to Palestinian population (excluded/target), and moderate d to international observers. The reading's self-declaration of zero victims is itself a structural feature — the foreclosure of the rival indigeneity claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (Jewish statelessness and vulnerability in diaspora) remains live (antisemitism persists, diaspora insecurity recurs), so mandatrophy is not resolved. However, the reading's application has expanded beyond the founding problem: the coordination function now includes settlement beyond 1967 lines, demographic engineering, and legal regimes that exceed the 'return' justification. This expansion without sunset clause creates a mandatrophy risk — the constraint persists and grows beyond its original coordination mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''jewish_self_determination'', instantiating the ''indigenous_return_reading''?',
    'Committee frame confirmation: the kernel_id and reading_id are assigned by the UKE_SCOPE decomposition manifest; this omega records that this file is that reading and nothing else.',
    'If this is not a kernel reading, the cs_structure.reading_relations and cs_structure.axioms blocks are misplaced and must be removed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this story is the indigenous_return_reading of the jewish_self_determination kernel.').

omega_variable(
    indigeneity_contestation_as_extraction_driver,
    'Does the contested status of Jewish indigeneity (rather than the claim itself) drive the constraint''s effective extraction upward?',
    'Compare epsilon in contexts where the reading is accepted (e.g. Israeli constitutional discourse, certain international legal arguments) vs. contexts where it is actively contested (e.g. UNHRC resolutions, BDS discourse, settler-colonial-reading-dominated frameworks). If epsilon differs systematically by discursive context, the extraction is context-dependent, not reading-intrinsic.',
    'If extraction is driven by contestation, the rope classification reflects the *political cost of maintaining the reading against rivals* rather than the reading''s internal structure. This would make the constraint a ''rope under fire'' — coordination maintained by active defense against foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_contestation_as_extraction_driver, empirical, 'Whether the reading''s measured extractiveness stems from the claim itself or from the structural pressure of rival readings.').

omega_variable(
    palestinian_indigeneity_foreclosure_boundary,
    'Does this reading''s core premise (Jewish indigeneity with unbroken connection) logically foreclose the settler_colonial_reading''s core premise (Zionism as European settler-colonial project dispossessing indigenous Palestinians), or do they coexist as competing frameworks?',
    'Test whether a single legal/political framework could simultaneously recognize Jewish indigeneity *and* Palestinian indigeneity with full self-determination for both. If mutual recognition is logically possible (e.g. binational or confederal models), the relation is coexists_with. If the readings'' core premises are mutually exclusive in any single framework, the relation is forecloses.',
    'If forecloses: the indigenous_return_reading structurally eliminates the settler_colonial_reading as a live option within its framework — the kernel cannot hold both. If coexists_with: both remain live positions held by different parties, and the constraint family contains genuine structural contention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_indigeneity_foreclosure_boundary, conceptual, 'Whether Jewish indigeneity claim logically forecloses Palestinian indigeneity/settler-colonial framing or coexists with it as a rival framework.').

omega_variable(
    suppression_mechanism_discursive_vs_structural,
    'Is the measured suppression (0.35) primarily discursive (marginalization of the reading in international forums, academic discourse) or structural (legal/political barriers to acting on the reading)?',
    'Trace the reading''s operational capacity: does it translate into effective sovereignty claims (Law of Return, settlement policy, international recognition of Jerusalem) or does it remain a discourse-level claim with limited structural effectuation? High structural effectuation with discursive marginalization = low structural suppression. Low structural effectuation = higher structural suppression.',
    'If suppression is primarily discursive, the reading operates as a rope with high coordination function but contested legitimacy. If suppression is structural (the reading cannot be effectuated), the coordination function is degraded and the constraint may drift toward scaffold or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_discursive_vs_structural, empirical, 'Discursive vs. structural suppression of the indigenous return reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_ir_tr_t0, jewish_self_determination__indigenous_return_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jsd_ir_tr_t0, observed).
narrative_ontology:measurement(jsd_ir_tr_t25, jewish_self_determination__indigenous_return_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(jsd_ir_tr_t25, observed).
narrative_ontology:measurement(jsd_ir_tr_t50, jewish_self_determination__indigenous_return_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(jsd_ir_tr_t50, observed).
narrative_ontology:measurement(jsd_ir_tr_t75, jewish_self_determination__indigenous_return_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement_basis(jsd_ir_tr_t75, projected).
narrative_ontology:measurement(jsd_ir_tr_t100, jewish_self_determination__indigenous_return_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement_basis(jsd_ir_tr_t100, projected).

% Extraction over time
narrative_ontology:measurement(jsd_ir_be_t0, jewish_self_determination__indigenous_return_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(jsd_ir_be_t0, observed).
narrative_ontology:measurement(jsd_ir_be_t25, jewish_self_determination__indigenous_return_reading, base_extractiveness, 25, 0.15).
narrative_ontology:measurement_basis(jsd_ir_be_t25, observed).
narrative_ontology:measurement(jsd_ir_be_t50, jewish_self_determination__indigenous_return_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(jsd_ir_be_t50, observed).
narrative_ontology:measurement(jsd_ir_be_t75, jewish_self_determination__indigenous_return_reading, base_extractiveness, 75, 0.2).
narrative_ontology:measurement_basis(jsd_ir_be_t75, projected).
narrative_ontology:measurement(jsd_ir_be_t100, jewish_self_determination__indigenous_return_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(jsd_ir_be_t100, projected).

% Suppression requirement over time
narrative_ontology:measurement(jsd_ir_su_t0, jewish_self_determination__indigenous_return_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(jsd_ir_su_t0, observed).
narrative_ontology:measurement(jsd_ir_su_t25, jewish_self_determination__indigenous_return_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement_basis(jsd_ir_su_t25, observed).
narrative_ontology:measurement(jsd_ir_su_t50, jewish_self_determination__indigenous_return_reading, suppression_requirement, 50, 0.35).
narrative_ontology:measurement_basis(jsd_ir_su_t50, observed).
narrative_ontology:measurement(jsd_ir_su_t75, jewish_self_determination__indigenous_return_reading, suppression_requirement, 75, 0.38).
narrative_ontology:measurement_basis(jsd_ir_su_t75, projected).
narrative_ontology:measurement(jsd_ir_su_t100, jewish_self_determination__indigenous_return_reading, suppression_requirement, 100, 0.35).
narrative_ontology:measurement_basis(jsd_ir_su_t100, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.1).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, israeli_law_of_return).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jerusalem_status_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one member of the jewish_self_determination constraint family. The kernel 'jewish_self_determination' decomposes into five readings with distinct ε values, beneficiary/victim structures, and constraint types. This reading (indigenous_return) claims mountain-like historical fact but operates as rope due to contestation. The settler_colonial_reading claims snare/tangled_rope structure with Palestinian victims. The liberal_nationalist_reading claims rope with symmetric coordination. The religious_covenant_reading claims mountain (divine decree). The diasporist_reading claims snare (Zionism extracts from diaspora Jews). All five are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__indigenous_return_reading, institutional, 0.1).
constraint_indexing:directionality_override(jewish_self_determination__indigenous_return_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
