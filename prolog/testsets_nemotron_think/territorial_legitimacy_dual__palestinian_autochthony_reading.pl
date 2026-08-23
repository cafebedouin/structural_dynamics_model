% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Legitimacy Grounded in Continuous Habitation and Right of Return
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story models the Palestinian autochthony reading of the
 *   territorial_legitimacy_dual kernel. The reading asserts that Palestinian
 *   legitimacy derives from continuous habitation since antiquity, the trauma
 *   of the 1948 Nakba (displacement), and an inalienable right of return. It
 *   frames these as natural-law entitlements that admit no compromise. The
 *   constraint operates as a legitimacy claim that demands full territorial
 *   restoration and refugee return, contesting Israeli state legitimacy and
 *   rejecting the two-state compromise framework. The claimed_type is
 *   mountain (the reading's self-presentation), but the authored metrics
 *   reveal substantial extractiveness (0.78) and suppression (0.72) — the
 *   engine will measure this divergence. Beneficiaries are the Palestinian
 *   people and refugees; victims are the Israeli state and citizens who bear
 *   the territorial and demographic costs. The constraint family includes two
 *   sibling readings: zionist_refuge_reading and
 *   two_state_coexistence_reading.
 *
 * KEY AGENTS:
 *   - palestinian_people: Primary beneficiary (organized/identity_locked) — receives legitimacy claim and right of return
 *   - palestinian_refugees: Primary beneficiary (organized/identity_locked) — direct claimants of return
 *   - israeli_state: Primary payer (institutional/constrained) — bears territorial and sovereignty costs
 *   - israeli_citizens: Secondary payer (organized/constrained) — bear demographic and security costs
 *   - palestinian_leadership: Agenda setter (organized/constrained) — articulates and enforces the claim
 *   - international_community: Observer (institutional/analytical) — mediates and documents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.78).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, mountain).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Legitimacy Grounded in Continuous Habitation and Right of Return").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:emerges_naturally(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, 'a602cab8-dea1-4aae-a79c-fe6ef6376019').
narrative_ontology:cs_kernel_codification('a602cab8-dea1-4aae-a79c-fe6ef6376019', distributed).
narrative_ontology:cs_authority_grounding('a602cab8-dea1-4aae-a79c-fe6ef6376019', lineage).
narrative_ontology:cs_interpretation_layer_present('a602cab8-dea1-4aae-a79c-fe6ef6376019').
narrative_ontology:cs_reading_relation('a602cab8-dea1-4aae-a79c-fe6ef6376019', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('a602cab8-dea1-4aae-a79c-fe6ef6376019', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('a602cab8-dea1-4aae-a79c-fe6ef6376019', foundational, palestinian_autochthony_grounds_legitimacy).
narrative_ontology:cs_axiom_status(palestinian_autochthony_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a602cab8-dea1-4aae-a79c-fe6ef6376019', palestinian_autochthony_grounds_legitimacy, deontological).
narrative_ontology:cs_axiom('a602cab8-dea1-4aae-a79c-fe6ef6376019', foundational, right_of_return_non_negotiable).
narrative_ontology:cs_axiom_status(right_of_return_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('a602cab8-dea1-4aae-a79c-fe6ef6376019', right_of_return_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('a602cab8-dea1-4aae-a79c-fe6ef6376019', pre_1948_palestinian_majority_continuous_habitation).
narrative_ontology:cs_drift_state('a602cab8-dea1-4aae-a79c-fe6ef6376019', post_1948_nakba, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a602cab8-dea1-4aae-a79c-fe6ef6376019', '2026-08-15T12:00:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_people).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_leadership).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, right_of_return_inalienable).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__palestinian_autochthony_reading, autochthony_grounds_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Palestinian people collectively hold the autochthony claim as core to their national identity. They benefit from the legitimacy narrative that grounds their right to the land. Exit from this identity is structurally impossible — the claim constitutes their political self-conception. They are organized through the PLO/PA and civil society, but lack sovereign power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_people, beneficiary,
    organized, generational, identity_locked, regional).

% Refugees and their descendants (millions) are the direct claimants of the right of return. They bear the material deprivation of exile but hold the moral leverage of the return demand. Their identity is fused with the return claim; exit means abandoning their core grievance. They are organized in camps and diaspora networks but lack state power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_refugees, beneficiary,
    moderate, biographical, identity_locked, regional).

% The Israeli state bears the territorial, demographic, and sovereignty costs of the claim. It controls the territory and has military superiority, but the claim constrains its legitimacy internationally and fuels conflict. Exit from the conflict is constrained by security doctrine, demographic fears, and ideological commitment to the land. It administers the occupation that the claim contests.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_state, payer,
    institutional, generational, constrained, regional).

% Israeli citizens (Jewish and Arab) bear the security, economic, and moral costs of the unresolved conflict. The autochthony claim threatens the Jewish demographic majority and the state's Jewish character. Exit options vary: emigration is possible but costly; many are identity_locked to the Zionist project. They are organized through civil society and political parties.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, israeli_citizens, payer,
    organized, biographical, constrained, regional).

% The PLO/PA and Hamas articulate the autochthony claim, negotiate (or refuse) on its basis, and derive legitimacy from representing it. They benefit politically from the claim's mobilization power but are constrained by it — they cannot concede the right of return without losing their base. They exercise limited governance in fragmented territories.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_leadership, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_leadership, beneficiary).

% UN bodies, states, NGOs, and legal institutions observe, document, and occasionally adjudicate the claim. They provide the international legal framework (right of return resolutions, ICJ opinions) that the claim invokes. They are not direct parties but their recognition confers legitimacy. Exit is analytical — they can shift policy focus.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__palestinian_autochthony_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_leadership).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__palestinian_autochthony_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Palestinian national identity, diaspora solidarity, and international advocacy around a single non-negotiable demand: full return and sovereignty. Provides a unifying narrative that transcends factional divides (Fatah/Hamas) and anchors the struggle in a moral-legal framework.
% TRANSFER_FUNCTION: Transfers territorial sovereignty over historic Palestine and demographic rights (right of return) from the Israeli state to the Palestinian people. Moves the burden of proof: Israel must justify its existence against the autochthony claim; Palestinians need only assert continuous presence and displacement.
% ABSENT_VOICES: Palestinian citizens of Israel who might prefer equality within Israel over return; Israeli peace camp that accepts two-state but not right of return; Palestinian refugees who would accept compensation and resettlement; Mizrahi Jews displaced from Arab countries — all are excluded from the dominant autochthony narrative.
% DISAPPEARANCE_RATIONALE: If the autochthony claim vanished overnight, the Palestinian national movement would lose its foundational moral-legal anchor. The right of return would drop from negotiations, enabling a two-state settlement on 1967 lines. Israeli legitimacy would be less contested. The conflict would shift from existential/ideological to territorial/borders — a fundamentally different political landscape.
% FOUNDING_PROBLEM: The 1948 Nakba: the displacement of 700,000+ Palestinians, destruction of their society, and prevention of return. The founding problem is the ongoing injustice of that displacement and the denial of the right to return to their homes and lands.
% FOUNDING_PROBLEM_CORROBORATION: UNRWA registration of 5.9 million refugees; UNGA Resolution 194 (right of return); ICJ 2004 Wall Opinion; human rights organizations (Amnesty, HRW, B'Tselem) documenting ongoing displacement; Israeli historians (Benny Morris, Ilan Pappé) confirming expulsion; Palestinian civil society (BADIL, Al-Haq) maintaining the claim. No significant corroboration from Israeli state or Zionist institutions — they contest the narrative.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__palestinian_autochthony_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__palestinian_autochthony_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, ExtMetricName, E),
    domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(territorial_legitimacy_dual__palestinian_autochthony_reading),
    narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the claim demands full reversal of 1948 and 1967 territorial changes plus demographic return, imposing massive costs on the Israeli side. Suppression is high (0.72) because the reading treats the right of return as non-negotiable, actively suppressing compromise alternatives (e.g., two-state, refugee compensation). Theater ratio is low (0.25) — the claim's performative dimension (rhetoric of return) is subordinate to its substantive demand. Accessibility collapse is very high (0.88) — once the autochthony premise is accepted, alternative frameworks (partition, compromise) appear illegitimate. Resistance is very high (0.85) — the Israeli state and citizens vigorously contest the claim militarily, legally, and politically. The measurement series (1948–2024) shows rising extractiveness and suppression as the claim hardens and the power asymmetry grows.
 *
 * PERSPECTIVAL GAP:
 *   From the Palestinian seat (beneficiary, identity_locked), the constraint appears as a mountain — a natural right that cannot be bargained away. From the Israeli seat (payer, constrained), it appears as a snare — an extractive demand enforced by demographic leverage and international sympathy. The engine computes this seat divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian people and refugees are structural beneficiaries: the constraint subsidizes their claim to the entire territory (d near 0.0). Israeli state and citizens are structural targets: the constraint extracts territory, sovereignty, and demographic stability (d near 1.0). Palestinian leadership is agenda_setter but also identity_locked — it cannot abandon the claim without losing legitimacy. International community is analytical observer (d=0.5). Exit options: Palestinians are identity_locked (national identity fused with the claim); Israelis are constrained (some emigration possible but high cost). Spatial scope is regional for primary parties, global for observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1948 displacement) remains live — the refugee population has grown, and the territorial situation has worsened. The constraint has not atrophied; its extraction has increased over time. No mandatrophy resolution: the claim's function (mobilizing Palestinian national struggle) remains active, but its coordination function (unifying a political program) has narrowed as the two-state alternative faded. The constraint is not a piton — it is actively enforced by Palestinian leadership and international advocates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_claim,
    'Is the Palestinian autochthony claim a genuine natural law of territorial legitimacy, or a constructed political claim that benefits identifiable agents?',
    'Comparative analysis of other autochthony-based legitimacy claims (e.g., indigenous rights globally) and their classification outcomes; legal-philosophical scrutiny of whether continuous habitation generates inalienable sovereignty.',
    'If natural law, the constraint is a mountain with near-zero extraction; if constructed, the beneficiary/victim structure reveals extraction and the constraint reclassifies as tangled_rope or snare via false_summit_mountain signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_claim, conceptual, 'Natural-law vs. constructed status of autochthony-based legitimacy.').

omega_variable(
    right_of_return_feasibility,
    'Is the right of return practically implementable without catastrophic disruption to the existing Israeli population, or does its non-negotiability function as a structural extraction demand?',
    'Demographic modeling of return scenarios; historical precedent of mass return (e.g., post-WWII population transfers); negotiation theory on non-negotiable demands.',
    'If implementable with manageable disruption, the claim''s extraction is lower; if catastrophic, the non-negotiability constitutes a snare-like extraction from Israeli citizens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_feasibility, empirical, 'Feasibility and extraction intensity of the right of return demand.').

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate the palestinian_autochthony_reading of the territorial_legitimacy_dual kernel, and how does it structurally relate to the zionist_refuge_reading and two_state_coexistence_reading?',
    'Commitment-system analysis of the three readings'' axioms, reference frames, and drift states; mapping of logical foreclosure, coexistence, or influence relations.',
    'Clarifies whether the three readings are mutually exclusive (forecloses), rival but coexisting (coexists_with), or upstream/downstream (influences). Affects contamination propagation in the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system positioning of this reading within the territorial_legitimacy_dual kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tld_par_tr_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(tld_par_tr_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(tld_par_tr_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(tld_par_tr_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(tld_par_tr_t2020, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(tld_par_tr_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(tld_par_be_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(tld_par_be_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(tld_par_be_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement(tld_par_be_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(tld_par_be_t2020, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(tld_par_be_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tld_par_su_t1948, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(tld_par_su_t1967, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(tld_par_su_t1993, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement(tld_par_su_t2000, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(tld_par_su_t2020, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(tld_par_su_t2024, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.08).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'territorial legitimacy dual' kernel into three readings. The autochthony reading (this story) claims mountain but shows high extraction/suppression, triggering false_summit_mountain evaluation. The zionist reading likely claims mountain with its own beneficiary/victim structure. The two-state reading claims scaffold (transitional compromise) with sunset clause. All three are linked via affects_constraints. The ε-invariance principle applies: each reading has distinct ε, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__palestinian_autochthony_reading, organized, 0.15).
constraint_indexing:directionality_override(territorial_legitimacy_dual__palestinian_autochthony_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
