% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Reading of Dual Territorial Legitimacy (1967 Framework)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint models the two-state coexistence reading of the contested
 *   territorial legitimacy kernel: mutual recognition of both peoples'
 *   1948-rooted legitimacy claims, with the pre-1967 boundary line (plus
 *   negotiated swaps) serving as the compromise basis for partition, a
 *   Palestinian right of return limited to the future Palestinian state
 *   rather than to 1948 territory, and security cooperation displacing
 *   zero-sum military competition. This is the dominant
 *   diplomatic-establishment reading since Oslo (1993). It is one of three
 *   sibling readings of the same kernel: the zionist_refuge_reading grounds
 *   Israeli legitimacy in persecution/UN partition and does not concede a
 *   symmetric Palestinian founding claim; the palestinian_autochthony_reading
 *   grounds Palestinian legitimacy in continuous habitation and an
 *   unrestricted right of return, which the compromise reading explicitly
 *   narrows. This story evaluates only the coexistence reading's own
 *   structural properties — its own ε is not blended with either sibling's.
 *
 * KEY AGENTS:
 *   - international_diplomatic_establishment: agenda_setter (institutional/analytical) — sustains the framework as diplomatic vocabulary
 *   - israeli_security_moderates: beneficiary (powerful/mobile) — gains stable recognized borders and security cooperation
 *   - palestinian_authority_leadership: beneficiary/payer (organized/constrained) — gains institutional legitimacy, pays in narrowed return claim and domestic credibility
 *   - israeli_settlers_beyond_green_line: payer (organized/trapped) — territorially exposed by the 1967 baseline
 *   - palestinian_refugees_outside_mandate_territory: payer (powerless/trapped) — bears the narrowing of the 1948 return claim
 *   - gaza_residents_under_blockade: payer (powerless/trapped) — bears deferred implementation without material benefit
 *   - east_jerusalem_palestinians: payer (powerless/constrained) — bears the framework's unresolved 'final status' deferral
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.52).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.61).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Reading of Dual Territorial Legitimacy (1967 Framework)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '695bb816-4b85-4f6d-bb8f-69ab423e2792').
narrative_ontology:cs_kernel_codification('695bb816-4b85-4f6d-bb8f-69ab423e2792', distributed).
narrative_ontology:cs_authority_grounding('695bb816-4b85-4f6d-bb8f-69ab423e2792', distributed).
narrative_ontology:cs_reading_relation('695bb816-4b85-4f6d-bb8f-69ab423e2792', territorial_legitimacy_dual__zionist_refuge_reading, influences).
narrative_ontology:cs_reading_relation('695bb816-4b85-4f6d-bb8f-69ab423e2792', territorial_legitimacy_dual__palestinian_autochthony_reading, influences).
narrative_ontology:cs_axiom('695bb816-4b85-4f6d-bb8f-69ab423e2792', foundational, dual_1948_legitimacy_symmetric_recognition).
narrative_ontology:cs_axiom_status(dual_1948_legitimacy_symmetric_recognition, holdable).
narrative_ontology:cs_axiom_grounding('695bb816-4b85-4f6d-bb8f-69ab423e2792', dual_1948_legitimacy_symmetric_recognition, conventional).
narrative_ontology:cs_axiom('695bb816-4b85-4f6d-bb8f-69ab423e2792', foundational, right_of_return_bounded_by_partition_state).
narrative_ontology:cs_axiom_status(right_of_return_bounded_by_partition_state, holdable).
narrative_ontology:cs_axiom_grounding('695bb816-4b85-4f6d-bb8f-69ab423e2792', right_of_return_bounded_by_partition_state, instrumental).
narrative_ontology:cs_reference_frame('695bb816-4b85-4f6d-bb8f-69ab423e2792', oslo_interim_framework).
narrative_ontology:cs_drift_state('695bb816-4b85-4f6d-bb8f-69ab423e2792', post_2020_normalization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('695bb816-4b85-4f6d-bb8f-69ab423e2792', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_moderates).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, regional_normalization_states).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_beyond_green_line).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_outside_mandate_territory).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, gaza_residents_under_blockade).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_palestinians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% UN bodies, the Quartet, and successive mediating states have institutionalized the 1967-lines-plus-swaps formula as the default diplomatic vocabulary since Oslo. They convene negotiations, draft frameworks, and allocate legitimacy recognition around this compromise; their own institutional relevance and career investment ride on the framework remaining the reference point even as facts on the ground diverge from it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_establishment, agenda_setter,
    institutional, generational, analytical, global).

% Israeli political and security figures who favor separation gain a framework that stabilizes Israel's Jewish-majority character within recognized borders, offers security cooperation with a Palestinian state instead of permanent occupation costs, and provides international legitimacy for Israel's core pre-1967 territory. They can exit into harder unilateralist or annexationist politics if the framework stalls.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_security_moderates, beneficiary,
    powerful, generational, mobile, national).

% The PA's institutional existence and international standing derive from being the recognized partner for a future state on 1967 lines; it collects diplomatic legitimacy, aid, and limited self-governance from the framework. It also pays: it must renounce the broader right-of-return claim and administer security cooperation that its own population increasingly views as collaboration, eroding its domestic legitimacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_authority_leadership, payer).

% Gulf and other regional states use nominal endorsement of the two-state framework as cover for normalizing relations with Israel and pursuing their own economic and security interests, while bearing none of the framework's territorial or demographic costs.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_normalization_states, beneficiary,
    institutional, generational, arbitrage, regional).

% Communities established in the West Bank since 1967 would face relocation, annexation-line redrawing, or contested residency under any 1967-based partition. Government policy has encouraged their settlement for decades even as the diplomatic framework nominally treats their land as future Palestinian territory, leaving them structurally exposed to a compromise they did not choose and actively resist.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settlers_beyond_green_line, payer,
    organized, biographical, trapped, local).

% Refugees and descendants in Lebanon, Jordan, Syria, and the diaspora who trace displacement to 1948 (not just 1967) find their return claim foreclosed or radically narrowed by a framework that confines right of return to a Palestinian state on 1967 lines, not to homes and land inside Israel's 1948 boundaries. Their loss is structural: the framework's compromise is built substantially on the currency of their forfeited claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugees_outside_mandate_territory, payer,
    powerless, generational, trapped, regional).

% Gaza's population lives under blockade and periodic military escalation while the two-state framework's implementation stalls indefinitely; the framework promises eventual statehood but delivers none of its material benefits, while its diplomatic persistence is cited to defer more urgent humanitarian and political remedies.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, gaza_residents_under_blockade, payer,
    powerless, immediate, trapped, local).

% Residents of East Jerusalem live under a status the two-state framework treats as the hardest unresolved variable — nominally part of a future Palestinian capital, practically under continuous Israeli administrative and demographic pressure. The framework's persistence as an unresolved 'final status issue' has coincided with decades of settlement expansion and residency revocations they cannot appeal outside a system that does not recognize their preferred sovereignty.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, east_jerusalem_palestinians, payer,
    powerless, biographical, constrained, local).

% Assess the framework's legal coherence, its relationship to UN Resolution 242 and successor instruments, and its practical divergence from ground realities. They document the widening gap between the framework's diplomatic persistence and its non-implementation without holding power to enforce or revise it.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_and_international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__two_state_coexistence_reading, international_diplomatic_establishment).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mutually legible compromise vocabulary that lets two national movements each claim legitimacy for their 1948 founding narrative while establishing a territorial line (pre-1967 borders, with negotiated swaps) that both sides' moderate factions can point to as a floor for negotiation, avoiding the zero-sum framing where recognizing one people's legitimacy requires denying the other's.
% TRANSFER_FUNCTION: Moves diplomatic legitimacy and negotiating leverage toward the recognized institutional partners (PA leadership, Israeli government) and moves substantive resolution — particularly the broader Palestinian right-of-return claim rooted in 1948, and the status of settlers and Jerusalem residents — into indefinite deferral, at the cost of those whose claims fall outside the 1967 compromise line.
% ABSENT_VOICES: Palestinian refugees and their descendants outside the West Bank/Gaza (whose 1948-rooted claims are structurally narrowed by the framework), and Israeli settlers beyond the Green Line (whose presence the framework treats as illegitimate to the compromise), are both effectively unrepresented in the diplomatic process that produces and sustains the framework — they are discussed, not consulted.
% DISAPPEARANCE_RATIONALE: Diplomatic institutions, the PA's international standing, and normalization diplomacy all depend on the two-state framework's continued existence as reference point — its disappearance would force those institutions to either adopt one of the sibling readings outright or construct a new compromise. Some analysts argue the framework has already been substantively hollowed out by settlement expansion and non-implementation, such that its formal disappearance would change little on the ground; others argue it remains the only mutually legible off-ramp from permanent zero-sum conflict, so its removal would materially foreclose peaceful paths forward. Both positions are actively held by serious parties.
% FOUNDING_PROBLEM: Two national movements each grounded their legitimacy claims in the same territory through incompatible founding narratives (1948 statehood vindication vs. 1948 dispossession), and the 1967 war created a second contested territorial baseline; a framework was needed that could let negotiators discuss partition without either side being required to renounce its foundational legitimacy claim as a precondition for talking.
% FOUNDING_PROBLEM_CORROBORATION: International mediators and successive US, EU, and UN officials attest the framework remains the only workable basis for negotiation. Independent from the diplomatic establishment, Israeli human rights organizations (B'Tselem) and Palestinian civil society groups have separately attested that the framework has functioned for decades primarily as a process that manages rather than resolves the conflict, citing continuous settlement expansion during the framework's diplomatic dominance as evidence the founding problem persists unaddressed rather than being actively solved.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, contested).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate-rising 0.52: the framework performs a genuine coordination function (a shared vocabulary letting negotiation proceed without a legitimacy precondition-fight) but that coordination has, since Oslo, increasingly served as diplomatic cover for continued settlement expansion and non-implementation, transferring real costs onto groups whose claims fall outside the 1967 line. Suppression (0.61) reflects that maintaining the framework as THE reference point requires active diplomatic and institutional work to marginalize both harder Israeli annexationist positions and the broader Palestinian 1948 return claim — both must be actively kept off the table for the compromise to hold. Theater ratio rises from 0.15 to 0.44 over the measured interval, tracking the widening gap between the framework's continued diplomatic invocation and its actual non-implementation on the ground (settlement growth, stalled negotiations) — a Goodhart-style substitution of the framework's persistence-as-vocabulary for its persistence-as-solution.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (diplomatic establishment), the framework looks like patient, necessary coordination infrastructure. From the payer seats with no negotiating presence — 1948-rooted refugees, Gaza residents, East Jerusalem Palestinians, settlers — the same structure looks like a mechanism that converts their exclusion into the price of other parties' stability. The engine computes this divergence from the structural power/exit data; it is not resolved by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The international diplomatic establishment and regional normalization states sit near the beneficiary end: they extract legitimacy, relevance, and diplomatic capital from the framework's continued invocation with minimal exposure to its costs (analytical/arbitrage exit). Israeli security moderates and PA leadership are dual-positioned: real beneficiaries of the compromise's stabilizing function, but PA leadership specifically pays through eroded domestic legitimacy for administering security cooperation and forfeiting the 1948 return claim. The four payer groups (settlers, 1948-rooted refugees, Gaza residents, East Jerusalem Palestinians) are structurally outside the compromise's benefit distribution — their claims or presence fall on the wrong side of the 1967 line the framework treats as settled, and none has meaningful exit (trapped or constrained).
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's founding problem — how to negotiate partition without requiring either side to renounce its foundational legitimacy claim — remains partially live (negotiation genuinely requires such a vocabulary) but is contested as to whether the CURRENT instantiation still serves it or has become a mechanism for indefinitely deferring resolution while facts on the ground (settlement expansion, blockade, Jerusalem status) harden against the framework's own terms. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (it is not pure extraction — it did produce interim self-governance and did reduce some forms of violence) while the required victims and active enforcement fields register that the coordination's costs are asymmetrically borne by groups excluded from the negotiating table.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compromise_vs_deferral_mechanism,
    'Is the 1967-based two-state framework functioning as genuine incremental progress toward resolution, or has it become a structural mechanism for indefinitely deferring resolution while non-implementation (settlement expansion, blockade, Jerusalem status changes) forecloses the framework''s own terms?',
    'Longitudinal tracking of settlement population growth, negotiation frequency and substantive content, and implementation milestones against the framework''s own stated benchmarks (Oslo Accords, Roadmap, subsequent initiatives) — a rising theater_ratio alongside stalled implementation would support the deferral-mechanism reading.',
    'If genuinely incremental, extraction is properly bounded and the tangled_rope classification with declining victim exposure over time is appropriate. If deferral mechanism, the framework functions closer to a snare wearing coordination language, and the rising theater_ratio and extractiveness trend in the authored measurements would be read as confirming evidence rather than noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_vs_deferral_mechanism, empirical, 'Whether the two-state framework''s persistence reflects progress or structural deferral.').

omega_variable(
    kernel_reading_selection_bias,
    'Is the international diplomatic establishment''s adoption of THIS reading (rather than either sibling reading) itself a structural artifact of which parties have institutional access to shape the negotiating vocabulary, rather than a neutral synthesis of both peoples'' claims?',
    'Comparative analysis of which stakeholder groups (states, NGOs, diaspora communities) had negotiating access at Oslo, Camp David, and subsequent rounds, versus which groups'' claims were structurally external to those rooms (1948 refugees, settler movements outside government coalitions).',
    'If reading-selection tracked negotiating access rather than claim-merit, the coexistence reading''s status as ''the'' pragmatic center is itself an artifact of asymmetric access, which would lower confidence in treating this reading as neutrally intermediate between the two sibling readings rather than as its own institutionally-produced compromise with its own beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_bias, conceptual, 'Whether the framework''s centrist status reflects genuine synthesis or institutional access asymmetry.').

omega_variable(
    right_of_return_narrowing_legitimacy,
    'Does confining the Palestinian right of return to a future Palestinian state (rather than to 1948 homes and land) represent a legitimate negotiated compromise, or does it extinguish a distinct legal/moral claim (grounded in UN Resolution 194 and international refugee law) without the consent of the claim-holders, most of whom are not parties to the negotiations that narrow their claim?',
    'Legal analysis of whether UN Resolution 194''s return provisions can be modified through negotiations to which the affected refugee population was not a direct party, versus international law''s general requirements for extinguishing individual property and return rights.',
    'If the narrowing is treated as illegitimate absent refugee consent, the victim status of palestinian_refugees_outside_mandate_territory intensifies and the extraction attributable to this reading rises; if treated as a legitimate negotiated compromise given practical constraints, current victim/extraction levels are appropriately bounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_narrowing_legitimacy, preference, 'Whether narrowing the right of return without direct refugee consent is a legitimate compromise or an extinguishment of an unconsented claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 1993, 0.15).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(terr_tr_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(terr_tr_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 1993, 0.32).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(terr_be_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2008, 0.46).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2014, 0.49).
narrative_ontology:measurement(terr_be_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2020, 0.51).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 1993, 0.4).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2000, 0.47).
narrative_ontology:measurement(terr_su_t2008, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2008, 0.53).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2014, 0.57).
narrative_ontology:measurement(terr_su_t2020, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 2024, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__two_state_coexistence_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_legitimacy_dual kernel. zionist_refuge_reading and palestinian_autochthony_reading are separate constraint files with their own ε, beneficiaries, and victims. This reading occupies the structural middle by symmetrically conceding both 1948 founding legitimacy claims while narrowing (not abolishing) the Palestinian right of return and adopting the 1967 line as the partition baseline — it does not average the siblings' ε values, it is a structurally distinct claim with its own extraction and suppression profile, higher than the theoretical zero-extraction ideal of a genuinely voluntary bilateral compromise because its actual operation since 1993 has been accompanied by rising theater and non-implementation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__two_state_coexistence_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
