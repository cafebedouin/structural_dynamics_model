% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial legitimacy via UN partition and state recognition (partition reading)
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   The partition reading of territorial legitimacy treats state legitimacy
 *   as deriving from international legal recognition of defined borders, as
 *   instantiated in UN Resolution 181 (1948). Under this reading, both
 *   Israeli and Palestinian states are legitimate within their UN partition
 *   boundaries; settlements beyond the 1967 lines are illegitimate intrusions
 *   on Palestinian territory; and the two-state solution is structurally
 *   possible if both parties accept partition boundaries as the legitimacy
 *   criterion. This reading competes within a contested kernel: the
 *   indigenous_continuity_reading rejects partition itself as imposed
 *   colonialism, and the security_necessity_reading argues partition
 *   boundaries cannot provide sufficient security. This JSON instantiates
 *   ONLY the partition reading as a clean, ε-invariant constraint, with the
 *   kernel contest routed to omega variables and cs_structure fields per the
 *   committer frame rules.
 *
 * KEY AGENTS:
 *   - International legal order: benefits from rule-based border legitimacy, institutional power, analytical seat
 *   - Two-state political movement: organized, constrained exit, seeks recognition within partition boundaries
 *   - Border-administering states (Israel, PA): institutional agenda-setters, trapped exit, contest enforcement
 *   - Settlers beyond partition line: moderate power, identity-locked exit, rendered illegitimate by the reading
 *   - Displaced populations (1948 onwards): powerless, trapped exit, structurally excluded from return
 *   - Residents of disputed territories: powerless, trapped exit, political status suspended by partition ambiguity
 *   - Indigenous continuity advocates: organized, constrained exit, excluded because they contest the partition itself
 *   - Security necessity advocates: institutional, constrained exit, excluded because they argue partition is insufficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.71).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial legitimacy via UN partition and state recognition (partition reading)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '70fae8b2-9bb5-4f37-b19e-524d559d75c8').
narrative_ontology:cs_kernel_codification('70fae8b2-9bb5-4f37-b19e-524d559d75c8', fixed_text).
narrative_ontology:cs_authority_grounding('70fae8b2-9bb5-4f37-b19e-524d559d75c8', lineage).
narrative_ontology:cs_interpretation_layer_present('70fae8b2-9bb5-4f37-b19e-524d559d75c8').
narrative_ontology:cs_reading_relation('70fae8b2-9bb5-4f37-b19e-524d559d75c8', territorial_legitimacy__indigenous_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('70fae8b2-9bb5-4f37-b19e-524d559d75c8', territorial_legitimacy__security_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('70fae8b2-9bb5-4f37-b19e-524d559d75c8', foundational, international_partition_supremacy).
narrative_ontology:cs_axiom_status(international_partition_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('70fae8b2-9bb5-4f37-b19e-524d559d75c8', international_partition_supremacy, conventional).
narrative_ontology:cs_axiom('70fae8b2-9bb5-4f37-b19e-524d559d75c8', foundational, borders_define_legitimacy).
narrative_ontology:cs_axiom_status(borders_define_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('70fae8b2-9bb5-4f37-b19e-524d559d75c8', borders_define_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('70fae8b2-9bb5-4f37-b19e-524d559d75c8', un_resolution_181_partition).
narrative_ontology:cs_drift_state('70fae8b2-9bb5-4f37-b19e-524d559d75c8', contemporary_two_thousand_twenty_six, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70fae8b2-9bb5-4f37-b19e-524d559d75c8', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, two_state_political_movement).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, border_administering_states).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, settlers_beyond_partition_line).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, displaced_populations_from_1948).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, residents_of_disputed_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The UN partition framework (Resolution 181) stabilizes international law by establishing the principle that state legitimacy derives from international recognition of defined borders. The system benefits from having clear rules for contested territorial claims: partition-based legitimacy is rule-based and precedent-generating, which strengthens predictability across territorial disputes worldwide. The constraint codifies 'recognized borders' as the legitimacy criterion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, international_legal_order, beneficiary,
    institutional, civilizational, analytical, universal).

% International legal recognition of Palestinian statehood within UN Resolution 181 boundaries provides a structurally coherent claim to self-determination and sovereign territory. Advocates argue this reading licenses Palestinian political authority and Palestinian claims to resources, institutions, and security arrangements within defined borders. It also provides a framework for conflict resolution: if both parties are legitimate within partition boundaries, settlements beyond those boundaries are illegitimate by the same reading.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, two_state_political_movement, beneficiary,
    organized, generational, constrained, global).

% Israel and Palestinian Authority jointly administer (or contest) the enforcement of a border. Israel holds security and administrative control over occupied territories and claims border authority; the PA claims border authority within its delegated domains. Both states' legitimacy under this reading depends on their acceptance by the international legal system as administered through the UN. The constraint requires that both states organize their territorial claims within the partition framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, border_administering_states, agenda_setter,
    institutional, generational, trapped, regional).

% Individuals and communities who have established residency and infrastructure beyond the UN partition boundaries (the 1967 lines) are rendered illegitimate by this reading: their territorial claims, property rights, and self-determination aspirations are structurally foreclosed. Under the partition reading, their presence is defined as a violation of Palestinian border sovereignty. Exit options are constrained by identity fusion (religious/national identity tied to settlement location) and by the absence of equivalent alternative territorial bases within partition boundaries.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, settlers_beyond_partition_line, payer,
    moderate, biographical, identity_locked, regional).

% Palestinians displaced during 1948 and their descendants are structurally excluded from return under the partition reading, which allocates territory to the Israeli state and defines Palestinian territory separately. The reading treats the partition as legitimate, which means treating their displacement as a structural consequence of the partition itself rather than as a violation of prior rights. Trapped because return is ruled out by the reading's own legitimacy frame, not by external barriers.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, displaced_populations_from_1948, payer,
    powerless, generational, trapped, regional).

% Residents of territories whose allocation is contested by both readings (Jerusalem, border enclaves) bear the costs of indefinite partition-boundary ambiguity: their political status, property rights, security, and freedom of movement are suspended pending settlement of which partition reading applies. They are excluded from the conversation about partition legitimacy because their territorial assignment is exactly what the readings contest.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, residents_of_disputed_territories, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__partition_reading, residents_of_disputed_territories, excluded).

% Representatives of the indigenous_continuity_reading would argue that partition legitimacy is a European colonial legal framework imposed on a territory with continuous Palestinian inhabitation and claim. They argue the partition itself is the illegitimate act, not settlements beyond partition lines. This reading is structurally excluded from the partition-reading framework because it contests the partition itself as the source of legitimacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, indigenous_continuity_advocates, excluded,
    organized, civilizational, constrained, regional).

% Proponents of the security_necessity_reading argue that the 1967 borders plus strategic depth are the only territorially legitimate arrangement because partition boundaries alone cannot provide security. This reading would argue that settlements and territorial expansion beyond 1967 lines are justified by security necessity. They are excluded from the partition-reading frame because they reject 1948 partition boundaries as the legitimacy criterion.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__partition_reading, security_necessity_advocates, excluded,
    institutional, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:fixing_cost_class(territorial_legitimacy__partition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule-based mechanism for allocating contested territory: instead of resolving the dispute through force or indigenous claim, partition allocates territory by international recognition and agreed borders. This stabilizes the international legal order by making border legitimacy a function of international agreement rather than power or history.
% TRANSFER_FUNCTION: Transfers territorial legitimacy from indigenous-claim-based or security-based frameworks to international-legal-partition-based frameworks. Transfers authority over border definition from local/military actors to the international legal system. Transfers property and security rights away from settlers and toward the partition boundary.
% ABSENT_VOICES: Indigenous continuity advocates and security-necessity advocates are structurally excluded from the partition reading because their core premises contest the reading's foundational claim. Palestinians who hold the indigenous_continuity reading are not in the conversation; Israeli security-doctrine proponents are not in the conversation. The reading does not accommodate their objections within its own frame.
% DISAPPEARANCE_RATIONALE: If international legal partition legitimacy disappeared, territorial disputes would reorganize around indigenous claims, security necessity, or force — the three competing readings would no longer be subordinated to partition law, and the entire post-WWII international order based on recognized borders would lose its primary legitimacy mechanism. Territorial claims would revert to power-based and history-based arguments.
% FOUNDING_PROBLEM: Post-WWII international order required a rule-based mechanism for allocating contested territories so that great-power competition did not resolve territorial disputes through war. The partition reading provides that mechanism: agreed borders, international recognition, and law-based legitimacy in place of force-based legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The international legal system continues to invoke partition and border recognition as the legitimacy criterion for state formation (witnessed in every UN seat allocation, recognition protocol, and international dispute settlement). The UN General Assembly reaffirms this principle regularly. However, the security_necessity reading and indigenous_continuity reading both argue the founding problem is INCOMPLETELY solved: partition legitimacy cannot coexist with security necessity (Israeli position) or indigenous continuity (Palestinian position), so the rule has failed to generate consensus on this specific case. Independent international law scholars attest that partition legitimacy remains the formal standard but has proven insufficient to resolve the Israeli-Palestinian dispute.
narrative_ontology:disappearance_verdict(territorial_legitimacy__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__partition_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the partition reading generates clear beneficiaries (the international legal order gains rule-based predictability; two-state advocates gain recognized legitimacy) and clear victims (settlers beyond the line are delegitimized; displaced populations' return is foreclosed; residents of disputed territory are suspended in legal limbo). The extraction arises because the reading allocates legitimacy unequally: those within partition boundaries are legitimate, those outside are not, regardless of continuous presence or security claims. Suppression is substantially high (0.71) because maintaining the partition reading as the operative legitimacy criterion requires active enforcement against the security_necessity reading (which drives territorial expansion beyond 1967 lines) and against the indigenous_continuity reading (which contests partition itself as a legitimate framework). Theater ratio is moderate (0.48) because the partition reading is invoked both sincerely (by two-state advocates and international law institutions) and performatively (by parties that endorse it in UN forums but disregard it in territorial practice). The measurement series trace a century of operation: extractiveness rises from 1948 to 2016 as the reading's incompleteness becomes evident (security necessity and indigenous claims grow stronger), then projects downward to 2026 if the reading is superseded or integrated into a more comprehensive legitimacy framework.
 *
 * PERSPECTIVAL GAP:
 *   The partition reading looks like coordination (rule-based border allocation) from seats that benefit from predictable international order (the international legal system, potential two-state political leaders). It looks like extraction from seats that are delegitimized by it (settlers, indigenous continuity advocates) or trapped outside it (displaced populations, disputed-territory residents). The security_necessity reading views partition as incomplete coordination (it solves border allocation but not security provision). This perspectival divergence is structural: the same constraint produces different type classifications at different seats, which is exactly what the engine measures. The authored claim is tangled_rope from the committer's seat (both genuine coordination function AND asymmetric extraction present); the engine will show that some seats compute it as pure snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are assigned d values near the low end (near 0.0 = full subsidy): the international legal order benefits from partition as the legitimacy criterion (low d, ~0.1), and two-state advocates benefit from recognition within partition boundaries (moderate d, ~0.3). Victims are assigned d values near the high end (near 1.0 = full target): settlers beyond the line are structurally delegitimized (high d, ~0.85), displaced populations are trapped outside the framework entirely (maximum d, ~0.95), and residents of disputed territory bear the cost of indefinite ambiguity (high d, ~0.8). The boundary-administering states are dual-positioned: Israel benefits from having a recognized state legitimacy (beneficiary seat, low d) but also pays the cost of international pressure regarding settlements and occupation (payer seat, moderate-high d). The PA benefits from recognized statehood legitimacy (beneficiary, low-moderate d) but pays the cost of being materially weaker than Israel in enforcing its borders (payer, moderate d). These are captured in the stakeholder secondary_role declarations and in the directionality override logic if needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading's founding problem (post-WWII rule-based territorial allocation to prevent great-power war) remains live in formal international law but is contested in practice. Extractiveness has risen over the 78-year interval (1948–2026) because the reading's incompleteness has become evident: it cannot accommodate both indigenous claims and partition, cannot reconcile partition with security necessity, and has generated a large population of permanently displaced persons. The rising theater_ratio (0.25 in 1948 to 0.54 by 2016, projecting down to 0.48 by 2026) indicates that more of the reading's enforcement activity is performative (affirming partition in international forums while disregarding it in territorial practice) than functional. However, mandatrophy is not fully resolved: the reading has not yet collapsed into pure theater (theater_ratio is still <0.5), and attempts to implement the two-state solution suggest the reading retains some functional force. The projected decline to 0.48 by 2026 reflects an alternative: either the reading integrates with the other legitimacy criteria (security necessity, indigenous continuity) to form a more comprehensive framework, or it begins to fade as the two-state option becomes politically untenable. The measurement series does not determine which; that is a question the corpus is assembled to answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_legitimacy_vs_indigenous_continuity,
    'Can international partition legitimacy coexist with indigenous-continuity claims in a single normative framework, or does accepting partition require foreclosing indigenous claims?',
    'Jurisprudential analysis of how international law has treated partition in cases involving populations with continuous pre-partition presence. Test case: can Palestinian indigenous claims to pre-1948 territory be reconciled with Israeli legitimate statehood under partition?',
    'If the reading forecloses indigenous claims, the constraint is a pure imposition on populations with continuous inhabitation. If they coexist, the reading accommodates dual legitimacy (both partition and indigenous claim hold). The partition reading as stated here assumes foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partition_legitimacy_vs_indigenous_continuity, conceptual, 'Whether partition legitimacy structurally forecloses indigenous-continuity legitimacy or can coexist with it.').

omega_variable(
    security_necessity_override_to_partition,
    'Does security necessity override partition boundaries as a legitimacy criterion, or does partition remain supreme?',
    'Historical analysis of Israeli security doctrine and territorial expansion: if security necessity is invoked to justify crossing partition boundaries, does the international legal system accept this override, or does it maintain partition as the supreme criterion?',
    'If security necessity overrides partition, then territorial control beyond 1967 lines can be legitimate under the security_necessity_reading even though the partition_reading forecloses it. The relationship between these two readings determines whether the territorial dispute is resolvable within either single reading or requires choosing between them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_necessity_override_to_partition, empirical, 'Whether security necessity and partition boundaries are hierarchically ordered in practice or whether they compete as coordinate legitimacy criteria.').

omega_variable(
    displaced_population_return_rights,
    'Under the partition reading, do Palestinian refugees displaced during 1948 and their descendants have a right of return to former homes within Israeli territory, or does partition legitimacy foreclose that right?',
    'Legal analysis of UN Resolutions 194 and 181 and their interaction. If Resolution 194 grants return rights while Resolution 181 grants Israel legitimate statehood, can both be upheld simultaneously, or does one override the other?',
    'If return rights are upheld, displaced populations are not fully victimized by the partition reading; extraction is asymmetric (Israeli settlers targeted, Palestinians granted remedies). If return rights are foreclosed, displacement is a structural cost of partition legitimacy, making the reading more purely extractive for displaced populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_population_return_rights, conceptual, 'Whether partition legitimacy includes remedies for displacement or treats displacement as a closed structural consequence of the partition itself.').

omega_variable(
    contested_kernel_committer_reading,
    'This constraint instantiates the partition_reading of the territorial_legitimacy kernel. Do the sibling readings (security_necessity_reading, indigenous_continuity_reading) logically foreclose this reading, or do they coexist as live positions held by different parties?',
    'Structural analysis of whether the three readings'' core premises are logically contradictory (one forecloses the others) or whether they remain compatible claims that different actors hold simultaneously.',
    'If readings foreclose each other, the dispute is fundamentally about which reading is correct (winner-take-all). If they coexist, the dispute is about power and legitimacy distribution across competing readings (negotiable). This affects the classification of the entire kernel: are the three readings three different constraints, or are they three contested instantiations of the same constraint?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_committer_reading, conceptual, 'The structural relationship between the partition reading and its sibling readings in the territorial legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__partition_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__partition_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__partition_reading, theater_ratio, 1987, 0.44).
narrative_ontology:measurement_basis(terr_tr_t1987, observed).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__partition_reading, theater_ratio, 2005, 0.51).
narrative_ontology:measurement_basis(terr_tr_t2005, observed).
narrative_ontology:measurement(terr_tr_t2016, territorial_legitimacy__partition_reading, theater_ratio, 2016, 0.54).
narrative_ontology:measurement_basis(terr_tr_t2016, observed).
narrative_ontology:measurement(terr_tr_t2026, territorial_legitimacy__partition_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(terr_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__partition_reading, base_extractiveness, 1948, 0.42).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__partition_reading, base_extractiveness, 1967, 0.51).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__partition_reading, base_extractiveness, 1987, 0.62).
narrative_ontology:measurement_basis(terr_be_t1987, observed).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__partition_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement_basis(terr_be_t2005, observed).
narrative_ontology:measurement(terr_be_t2016, territorial_legitimacy__partition_reading, base_extractiveness, 2016, 0.68).
narrative_ontology:measurement_basis(terr_be_t2016, observed).
narrative_ontology:measurement(terr_be_t2026, territorial_legitimacy__partition_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(terr_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__partition_reading, suppression_requirement, 1948, 0.38).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__partition_reading, suppression_requirement, 1967, 0.62).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__partition_reading, suppression_requirement, 1987, 0.71).
narrative_ontology:measurement_basis(terr_su_t1987, observed).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__partition_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement_basis(terr_su_t2005, observed).
narrative_ontology:measurement(terr_su_t2016, territorial_legitimacy__partition_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement_basis(terr_su_t2016, observed).
narrative_ontology:measurement(terr_su_t2026, territorial_legitimacy__partition_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(terr_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% The territorial_legitimacy kernel has three readings: partition_reading (this file), indigenous_continuity_reading, and security_necessity_reading. All three construe the same territorial dispute but instantiate different legitimacy criteria and produce different beneficiary/victim structures. Each reading is a complete constraint story with its own ε, stakeholders, and type classification. The three stories are linked here and in the network.affects_constraints of the siblings. The family models the kernel contest: three live positions, no single winner, structural tension between the legitimacy criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__partition_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
