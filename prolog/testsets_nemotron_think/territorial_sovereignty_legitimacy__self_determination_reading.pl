% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Self-Determination Reading of Territorial Sovereignty Legitimacy
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story models the self-determination reading of
 *   territorial sovereignty legitimacy over Palestine/Israel. The reading
 *   asserts that sovereignty derives exclusively from the modern principle of
 *   self-determination applied to the Arab population that constituted the
 *   demographic majority and maintained continuous residence during the
 *   19th-20th centuries. It frames the 1947 partition and 1948 establishment
 *   of Israel as an unjust imposition by external powers (colonial
 *   imposition), the Israeli state as a colonial-settler project, and the
 *   right of return as restoration of the status quo ante. The constraint
 *   operates through international law (UN resolutions, ICJ advisory
 *   opinions), diplomatic consensus, and Palestinian national movement
 *   institutions. It structurally extracts from the Israeli/Jewish claim by
 *   delegitimizing its sovereignty basis while providing a coordination
 *   function (a legal-democratic standard for sovereignty allocation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.72).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.78).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Self-Determination Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '36291d20-09bf-4c93-b423-906d2e253738').
narrative_ontology:cs_kernel_codification('36291d20-09bf-4c93-b423-906d2e253738', distributed).
narrative_ontology:cs_authority_grounding('36291d20-09bf-4c93-b423-906d2e253738', distributed).
narrative_ontology:cs_reading_relation('36291d20-09bf-4c93-b423-906d2e253738', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('36291d20-09bf-4c93-b423-906d2e253738', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('36291d20-09bf-4c93-b423-906d2e253738', foundational, self_determination_as_sovereignty_basis).
narrative_ontology:cs_axiom_status(self_determination_as_sovereignty_basis, holdable).
narrative_ontology:cs_axiom_grounding('36291d20-09bf-4c93-b423-906d2e253738', self_determination_as_sovereignty_basis, conventional).
narrative_ontology:cs_axiom('36291d20-09bf-4c93-b423-906d2e253738', secondary, colonial_project_characterization).
narrative_ontology:cs_axiom_status(colonial_project_characterization, holdable).
narrative_ontology:cs_axiom_grounding('36291d20-09bf-4c93-b423-906d2e253738', colonial_project_characterization, empirically_contingent).
narrative_ontology:cs_axiom('36291d20-09bf-4c93-b423-906d2e253738', secondary, right_of_return_as_restoration).
narrative_ontology:cs_axiom_status(right_of_return_as_restoration, holdable).
narrative_ontology:cs_axiom_grounding('36291d20-09bf-4c93-b423-906d2e253738', right_of_return_as_restoration, conventional).
narrative_ontology:cs_reference_frame('36291d20-09bf-4c93-b423-906d2e253738', mandate_period_demographic_legitimacy).
narrative_ontology:cs_drift_state('36291d20-09bf-4c93-b423-906d2e253738', post_1967_occupation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36291d20-09bf-4c93-b423-906d2e253738', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, zionist_national_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, self_determination_as_sovereignty_basis).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, colonial_character_of_zionist_settlement).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, right_of_return_as_restoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Arab population that constituted the demographic majority in the territory during the 19th-20th centuries. This reading grants them sovereign legitimacy through the principle of self-determination. Their claim rests on continuous residence and demographic continuity. Exit from this identity frame is identity_locked — Palestinian national identity is constituted through the claim to the land and the right of return; abandoning the claim dissolves the collective self.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arab_population, beneficiary,
    organized, generational, identity_locked, national).

% Descendants of those displaced in 1948 and 1967. They benefit from the right-of-return claim this reading generates, but bear the material costs of statelessness, camp life, and denial of citizenship. Their exit options are trapped — no state recognizes their return, host states deny integration, and the identity frame makes return the only acceptable resolution.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants, beneficiary,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_and_descendants, payer).

% The Jewish population that established and maintains the Israeli state. This reading structurally extracts from them by framing their sovereignty as illegitimate colonial imposition rather than legitimate self-determination. Their exit is identity_locked — Israeli national identity fuses survival, sovereignty, and territorial control; conceding the self-determination reading's core premise would dissolve the state's legitimacy basis.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_jewish_population, payer,
    institutional, generational, identity_locked, national).

% The organized political movement that built and sustains Israeli sovereignty. This reading delegitimizes its foundational narrative (return to ancestral homeland, historical right). Constrained exit — the movement could theoretically accept a binational or two-state framework, but its core institutions and donor base are structured around exclusive sovereignty.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, zionist_national_movement, payer,
    organized, generational, constrained, global).

% UN system, ICJ, international human rights law. Administers the self-determination principle through resolutions (194, 242, 338), advisory opinions, and treaty bodies. Sets the agenda for legitimacy discourse but lacks enforcement power. Analytical exit — it observes and adjudicates but is not subject to the constraint's extraction.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_legal_order, agenda_setter,
    institutional, civilizational, analytical, universal).

% UK (mandate power), US (primary Israeli patron), USSR/Russia, EU. They imposed the partition (1947) and sustain the conflict structure through arms, diplomacy, and vetoes. This reading frames them as colonial imposers; they are excluded from the self-determination calculus except as obstructors. Arbitrage exit — they can shift alliances, reframe interests, or disengage without identity cost.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, external_great_powers, excluded,
    powerful, biographical, arbitrage, global).

% Jewish communities from Arab/Muslim lands who migrated to Israel. This reading's binary (Arab self-determination vs. colonial settlers) erases their indigeneity to the region and their forced displacement. Constrained exit — they participate in Israeli politics but their historical narrative is marginalized by both the self-determination reading and the dominant Zionist narrative.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, mizrahi_and_arab_jews, excluded,
    moderate, biographical, constrained, national).

% Scholars of international law, political theory, history. They map the structural claims, trace the kernel contest, and assess the constraint's operation without collecting or paying. Pure analytical exit.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a normative framework for determining territorial sovereignty based on the modern legal principle of self-determination applied to the population with continuous demographic presence during the formative modern period (19th-20th centuries), replacing historical/religious claims with a democratically legible standard.
% TRANSFER_FUNCTION: Transfers sovereignty legitimacy from historical-religious claims (covenant continuity) and existential-survival claims (existential matrix) to the demographic self-determination of the Arab majority population; transfers the political status of the territory from Israeli state sovereignty to Palestinian national sovereignty; transfers the right of return from a humanitarian claim to a restorative legal entitlement.
% ABSENT_VOICES: Mizrahi/Arab Jews whose indigeneity and displacement contradict the colonial-settler binary; Bedouin and other non-nationalist populations in the territory; the global Jewish diaspora for whom Israel represents existential insurance rather than colonial project; Palestinian citizens of Israel who hold Israeli citizenship while identifying with the Palestinian national claim — their dual position is structurally invisible in the binary.
% DISAPPEARANCE_RATIONALE: If the self-determination reading vanished overnight, the primary legal basis for Palestinian sovereignty claims in international forums would collapse. The UN resolution framework (194, 242, 338) would lose its interpretive anchor. The two-state solution consensus would lose its legal foundation. The right of return would revert to a humanitarian negotiable rather than a legal right. The entire diplomatic architecture since 1967 would reorganize around power facts rather than legal principles.
% FOUNDING_PROBLEM: The problem of legitimating territorial sovereignty in the post-Ottoman, post-colonial Middle East after the collapse of imperial authority — specifically, how to allocate sovereignty over Palestine between the Arab majority population and the immigrating Jewish population without reproducing colonial-imperial imposition.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the League of Nations mandate system's own terms (Article 22: 'well-being and development' of inhabitants), the King-Crane Commission (1919) which found overwhelming Arab opposition to Zionism, and UNSCOP minority report (1947) recommending a federal state. Israeli historians (Morris, Pappé, Shlaim) corroborate the demographic reality and displacement. The Zionist movement's own archives document the transfer concept. No corroboration exists from outside the benefiting parties for the claim that the problem is 'dead' — the dispute remains live in every diplomatic forum.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the reading structurally denies the legitimacy of the opposing claim entirely — it is not a sharing principle but a replacement principle. Suppression (0.78) is high because maintaining this reading as the exclusive legitimacy framework requires active suppression of the covenant-continuity and existential-matrix readings through diplomatic pressure, legal adjudication, and narrative enforcement. Theater ratio (0.38) reflects genuine legal-diplomatic machinery (UN, ICJ, treaties) performing real coordination work, but with growing performative invocation as the two-state solution becomes practically unimplementable. Accessibility collapse (0.73) is high because accepting self-determination as the sole legitimacy basis collapses the alternative frameworks (historical right, existential survival) into illegitimacy. Resistance (0.81) is very high because the targeted population (Israeli Jews) possesses state power, military capacity, and a competing identity-locked legitimacy narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian seat, the constraint is a rope — genuine coordination providing a legal path to sovereignty. From the Israeli seat, it is a snare — pure extraction denying their legitimacy. The international legal order experiences it as a scaffold — transitional coordination meant to produce a two-state settlement (sunset clause implicit in the two-state framework). The engine computes this divergence from the structural data; the claimed_type (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are structurally real and simultaneously operative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Palestinian Arab population and refugees are structural beneficiaries (d near 0.0) — the constraint subsidizes their claim with international legal recognition. The Israeli Jewish population and Zionist movement are structural targets (d near 1.0) — the constraint extracts their sovereignty legitimacy. The international legal order is the agenda-setter (d ~0.5) — it administers the principle but bears no extraction. External great powers are excluded (arbitrage exit) — they created the constraint structure but can reposition. Mizrahi/Arab Jews are excluded with constrained exit — the binary erases their position. The analytical observer sits at d=0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimating sovereignty post-empire) remains live and contested. The arrangement (self-determination as exclusive legitimacy basis) has not resolved the problem — it has become one pole of a frozen contest. The mandate to implement self-determination (via two-state solution) has atrophied into a ritual invocation while facts on the ground (settlements, demographic shifts) make implementation increasingly impossible. This is mandatrophy: the coordination function (two-state solution) is dead but the constraint (self-determination reading) persists as extraction against the other claim. The theater_ratio rise from 0.15 to 0.38 tracks this atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint a genuine reading of a shared kernel, or a distinct constraint masquerading as a reading?',
    'Test whether the three readings share a single stabilized commitment (the kernel) that they interpret differently, or whether they are three independent sovereignty claims with no common referent. If no shared kernel exists, decompose into three separate constraint stories without reading_relations.',
    'If no shared kernel, the reading_relations and drift_state in cs_structure are misauthored — the engine''s cross-reading contamination analysis would produce false signals. The constraint family structure would be invalid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the three declared readings genuinely share a kernel or are independent claims').

omega_variable(
    temporal_scope_legitimacy,
    'Does the modern period (19th-20th century) demographic majority create a perpetual sovereignty right, or is the temporal scope itself a contested choice that privileges this reading?',
    'Compare with other self-determination cases: does the principle apply to the population at the moment of decolonization only, or to any historical demographic snapshot? Examine ICJ advisory opinions and UN practice on temporal anchoring of self-determination.',
    'If the temporal scope is a reading-specific choice rather than a principle requirement, the constraint''s extractiveness is partly constructed by the reading''s framing choices — the ''modern period'' cutoff excludes Jewish demographic presence in earlier periods and post-1948 demographic changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_legitimacy, conceptual, 'Whether the modern-period temporal scope is intrinsic to self-determination or a reading-specific framing choice').

omega_variable(
    demographic_continuity_threshold,
    'What degree of demographic continuity is required for the self-determination claim, and how does the reading handle population movements on both sides?',
    'Analyze the reading''s treatment of: (a) Jewish population present in 19th century (Old Yishuv), (b) Arab population displaced in 1948/1967, (c) Jewish refugees from Arab lands, (d) post-1967 settlers. Does the reading apply a consistent demographic standard?',
    'If the reading applies asymmetric demographic standards (counting Arab continuity but discounting Jewish continuity, or vice versa), the constraint''s extractiveness includes a framing-bias component that the engine''s ε cannot distinguish from structural extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_continuity_threshold, empirical, 'Whether the demographic continuity criterion is applied symmetrically across populations').

omega_variable(
    colonial_frame_as_extraction_cover,
    'Does the ''colonial project'' characterization of Israel serve a genuine analytical function in this reading, or does it function as an extraction cover that forecloses negotiation by moralizing the opponent?',
    'Test whether the reading can generate a viable coexistence framework (binational, confederation, two-state) while maintaining the colonial characterization, or whether the characterization structurally requires the other''s elimination. Compare with other decolonization cases where colonial characterization coexisted with negotiated settlement.',
    'If the colonial frame forecloses any negotiated legitimacy for the other side, the constraint''s coordination function is illusory — it coordinates only around the other''s disappearance, making it functionally a snare wearing a rope''s clothing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_frame_as_extraction_cover, conceptual, 'Whether the colonial characterization is analytically necessary or extractively foreclosing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(terr_tr_t1947, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1947, 0.25).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(terr_be_t1947, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1947, 0.55).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.65).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(terr_su_t1947, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1947, 0.65).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__self_determination_reading, 0.08).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_right_of_return).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, un_resolution_242_implementation).

% DUAL FORMULATION NOTE:
% This reading and covenant_continuity_reading share the kernel 'territorial_sovereignty_legitimacy' but instantiate different constraints with different ε values. Self-determination reading ε=0.72 (substantial extraction from Israeli claim); covenant_continuity reading would have lower ε from Israeli seat but higher from Palestinian seat. The existential_matrix_reading operates at a different structural level (existential vs. juridical) and coexists as a meta-constraint on both juridical readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, institutional, 0.15).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, powerless, 0.95).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
