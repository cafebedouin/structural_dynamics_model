% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Territorial Legitimacy
 *   domain: political/international/territorial
 *
 * SUMMARY:
 *   This constraint story instantiates the zionist_refuge_reading of the
 *   territorial_legitimacy_dual kernel. It asserts Israel's legitimacy as
 *   grounded in three pillars: (1) historical persecution of Jews culminating
 *   in the Holocaust, establishing a moral claim to refuge; (2) divine
 *   promise / historical connection to the land, providing a
 *   civilizational-narrative anchor; (3) UN General Assembly Resolution 181
 *   (1947) partition acceptance, providing international legal personality.
 *   The reading holds 1948 boundaries as settled legitimacy, 1967 territories
 *   as negotiable security buffer, Palestinian displacement as consequence of
 *   Arab rejection of partition, and ongoing territorial control as justified
 *   by security imperatives. This is ONE reading of a contested kernel — the
 *   sibling readings (palestinian_autochthony_reading,
 *   two_state_coexistence_reading) instantiate different constraints with
 *   different beneficiary/victim structures and different ε values.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.72).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '03a22082-f7cf-41b6-b92c-dd72f3ff643e').
narrative_ontology:cs_kernel_codification('03a22082-f7cf-41b6-b92c-dd72f3ff643e', formalized).
narrative_ontology:cs_authority_grounding('03a22082-f7cf-41b6-b92c-dd72f3ff643e', lineage).
narrative_ontology:cs_interpretation_layer_present('03a22082-f7cf-41b6-b92c-dd72f3ff643e').
narrative_ontology:cs_reading_relation('03a22082-f7cf-41b6-b92c-dd72f3ff643e', territorial_legitimacy_dual__palestinian_autochthony_reading, forecloses).
narrative_ontology:cs_reading_relation('03a22082-f7cf-41b6-b92c-dd72f3ff643e', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('03a22082-f7cf-41b6-b92c-dd72f3ff643e', foundational, jewish_historical_right_to_sovereign_refuge).
narrative_ontology:cs_axiom_status(jewish_historical_right_to_sovereign_refuge, holdable).
narrative_ontology:cs_axiom_grounding('03a22082-f7cf-41b6-b92c-dd72f3ff643e', jewish_historical_right_to_sovereign_refuge, deontological).
narrative_ontology:cs_axiom('03a22082-f7cf-41b6-b92c-dd72f3ff643e', foundational, un_partition_resolution_181_as_legal_basis).
narrative_ontology:cs_axiom_status(un_partition_resolution_181_as_legal_basis, holdable).
narrative_ontology:cs_axiom_grounding('03a22082-f7cf-41b6-b92c-dd72f3ff643e', un_partition_resolution_181_as_legal_basis, conventional).
narrative_ontology:cs_axiom('03a22082-f7cf-41b6-b92c-dd72f3ff643e', secondary, arab_rejection_of_partition_absolves_displacement_responsibility).
narrative_ontology:cs_axiom_status(arab_rejection_of_partition_absolves_displacement_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('03a22082-f7cf-41b6-b92c-dd72f3ff643e', arab_rejection_of_partition_absolves_displacement_responsibility, instrumental).
narrative_ontology:cs_reference_frame('03a22082-f7cf-41b6-b92c-dd72f3ff643e', zionist_refuge_legitimacy_framework).
narrative_ontology:cs_drift_state('03a22082-f7cf-41b6-b92c-dd72f3ff643e', post_oslo_post_settlement_acceleration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03a22082-f7cf-41b6-b92c-dd72f3ff643e', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_israeli_population).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_west_bank_gaza).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_israel).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, jewish_israeli_population).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, regional_arab_states).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, jewish_self_determination_right).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, historical_persecution_grounds_statehood).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_resolution_181_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territorial regime, controls borders and residency, allocates land and resources, enforces security architecture. Derives legitimacy from UN recognition and historical narrative; controls the legal framework defining citizenship, property, and movement. Can project power regionally and influence international diplomacy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions, beneficiary).

% Collective beneficiary of statehood, security, and resource allocation. Bears costs through military service, taxation, and social cohesion demands. Exit is constrained by identity attachment, economic integration, and lack of comparable refuge alternatives. Experiences the constraint as both protective shelter and demanding obligation.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_israeli_population, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, jewish_israeli_population, payer).

% Derives existential insurance and identity anchor from Israel's existence. Provides political advocacy, financial resources, and demographic reservoir. Not subject to the constraint's direct enforcement but invested in its persistence. Exit is mobile — can disengage politically without personal cost — but identity linkage creates pull toward engagement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, mobile, global).

% Displaced in 1948 war, denied return, stateless or host-country dependent. Bear the territorial constraint's founding displacement cost without voice in its administration. No effective exit: return blocked, integration blocked, compensation unoffered. Their claim is structurally excluded from the reading's legitimacy framework.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948, excluded).

% Live under military occupation or blockade, subject to permit regimes, settlement expansion, and security enforcement. No citizenship, no vote, no access to the legal protections the constraint extends to beneficiaries. Exit is trapped: movement restricted, economy constrained, political agency denied. Experience the constraint as pure extraction enforced by arms.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_west_bank_gaza, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_west_bank_gaza, excluded).

% Hold Israeli citizenship but experience structural inequality: land allocation, planning restrictions, symbolic exclusion from national ethos. Benefit from civil rights, social services, and economic access denied to Palestinians in occupied territories. Exit constrained by identity, community, and lack of alternative polity. Dual position: included in the franchise, excluded from the founding myth.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_israel, beneficiary).

% Holds the UN Charter, Geneva Conventions, ICJ advisory opinions, and human rights treaty bodies as reference frames. Observes the constraint's operation against international law standards. Can issue rulings, authorize sanctions, shape diplomatic recognition — but enforcement depends on member state compliance. Analytical seat: sees the full structure without bearing its costs or collecting its benefits.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_legal_order, observer,
    institutional, generational, analytical, global).

% Bear security costs, refugee hosting burdens, and diplomatic constraints from the territorial dispute. Some have normalized relations (Egypt, Jordan, Abraham Accords states); others remain in formal hostility. Their position has shifted from collective rejection to fragmented engagement. Exit constrained by regional politics, domestic legitimacy, and strategic calculations.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, regional_arab_states, observer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, regional_arab_states, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a recognized sovereign refuge for a historically persecuted people, solving the coordination problem of collective security, cultural continuity, and international legal personality for the Jewish national project.
% TRANSFER_FUNCTION: Moves territorial control, resource allocation, demographic weight, and legal sovereignty from the pre-1948 Palestinian Arab population to the Jewish national movement and its state institutions — justified by historical persecution, divine promise, and UN partition acceptance.
% ABSENT_VOICES: The 1948 Palestinian refugee generation — those directly displaced — are structurally absent from the reading's legitimacy framework. Their descendants remain excluded. The reading frames their displacement as consequence of Arab rejection of partition, not as a founding injustice requiring redress. Their voice would contest the transfer_function's moral accounting.
% DISAPPEARANCE_RATIONALE: If the Zionist refuge reading's legitimacy claim vanished overnight, the legal basis for Israeli sovereignty over 1948 territories would collapse, the settlement enterprise in 1967 territories would lose its ideological engine, the Palestinian right of return would become the dominant legal claim, and the regional order would reorganize around a fundamentally different territorial-legitimacy framework.
% FOUNDING_PROBLEM: The Jewish people lacked a sovereign territorial refuge capable of protecting them from recurrent persecution, culminating in the Holocaust; the international community recognized this via UNGA Resolution 181 (1947), and the 1948 war established the state through defensive victory against Arab rejection of partition.
% FOUNDING_PROBLEM_CORROBORATION: Israeli historians (Benny Morris, Anita Shapira) and international scholars (Alan Dershowitz, Martin Gilbert) corroborate the persecution-refuge-partition sequence from within the reading's tradition. Palestinian historians (Walid Khalidi, Rashid Khalidi) and critical Israeli historians (Ilan Pappé, Avi Shlaim) contest the defensive-war framing and the moral weight assigned to partition acceptance versus Palestinian rejection. No external neutral arbiter resolves the dispute; the corroboration field is itself contested.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the constraint's operation: it transfers territory, resources, and sovereignty from one people to another, justified by a narrative that beneficiaries accept and victims reject. Suppression (0.72) is high because the constraint's persistence depends on military enforcement, legal exclusion, and narrative control — not voluntary coordination. Theater ratio (0.38) captures the genuine coordination function (refuge, self-determination, security) alongside the performative maintenance of democratic legitimacy while maintaining structural inequality. Accessibility collapse (0.45) is moderate: alternatives (binational state, full return, equal citizenship) are cognitively available but politically blocked. Resistance (0.62) is substantial: Palestinian national movement, international legal challenges, BDS, and internal dissent all contest the constraint. The claimed_type tangled_rope reflects the dual character: real coordination for beneficiaries, real extraction from victims, active enforcement required.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter/beneficiary seats (Israeli institutions, Jewish Israelis, diaspora) experience the constraint as protective coordination — a genuine refuge solving a real collective-action problem. The payer/excluded seats (Palestinian refugees, occupied residents) experience it as enforced extraction — a territorial claim sustained by arms and legal exclusion. Palestinian citizens of Israel occupy the tension: included enough to see the coordination, excluded enough to feel the extraction. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and Jewish Israeli population are structural beneficiaries (d near 0.0–0.2): they collect sovereignty, security, resources, and narrative validation. Diaspora Jewish communities are beneficiaries with mobile exit (d ~0.1). Palestinian refugees 1948 and West Bank/Gaza residents are full targets (d near 0.9–1.0): they bear displacement, statelessness, occupation, and exclusion with trapped exit. Palestinian citizens of Israel are partial targets with constrained exit (d ~0.6): included in franchise but excluded from founding myth and resource equality. International legal order and regional states are observers with analytical or constrained exit. The directionality derivation follows beneficiary/victim declarations + power + exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (persecution -> need for refuge) was historically real and the 1948 arrangement solved it for the beneficiaries. The question is whether the arrangement's extension to 1967 territories, settlement enterprise, and permanent occupation of another people constitutes mandatrophy — the original mandate (refuge) has been satisfied but the constraint expands beyond it. The reading claims the founding problem remains live (security threats, demographic vulnerability); critics say the problem is solved and the constraint now serves expansion. This contested status is exactly what the mandate_fulfillment check measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the territorial_legitimacy_dual kernel a single commitment with multiple readings, or are these three fundamentally different legitimacy claims that only appear related through the shared geography?',
    'Test whether the three readings'' axioms can be simultaneously held by a single authority structure without contradiction. If no single framework can accommodate all three, they are distinct kernels masquerading as one.',
    'If distinct kernels, each reading should be authored as a separate constraint family root rather than siblings of one kernel. This would change the cs_structure relationships and the network topology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel is a genuine single commitment or a conflation of distinct claims.').

omega_variable(
    divine_promise_epistemic_status,
    'What is the epistemic status of the ''divine promise'' pillar in this reading — is it a foundational axiom held by the authority structure, a rhetorical device for mobilization, or a sincerely held belief that functions as coordination infrastructure?',
    'Analyze the authority structure''s internal discourse: does it treat the divine promise as legally binding (halakhic), politically instrumental, or identity-constitutive? Track how the claim functions in legal arguments, diplomatic positions, and educational curricula.',
    'If divine promise is foundational and legally binding, the reading forecloses territorial compromise on core zones (Judea/Samaria) — affecting the ''1967 negotiable'' claim. If instrumental, the reading has more flexibility than its rhetoric suggests. If identity-constitutive, it creates identity_locked exit for beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_promise_epistemic_status, conceptual, 'The epistemic role of the divine promise claim in the reading''s authority structure.').

omega_variable(
    security_vs_expansion_boundary,
    'Where is the structural boundary between legitimate security requirements and territorial expansion in this reading''s operation — and who has the authority to define it?',
    'Track the correlation between stated security justifications and actual settlement patterns, military deployments, and resource allocations over time. Compare with independent military assessments of defensible borders.',
    'If the boundary is indistinct and controlled by the agenda_setter, the reading''s ''1967 negotiable'' claim is performative — the constraint operates as snare on 1967 territories. If a clear, externally verifiable boundary exists, the tangled_rope classification holds for 1967 territories as genuine security coordination with extraction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_expansion_boundary, empirical, 'Whether the security-coordination function has a verifiable boundary or bleeds into expansion.').

omega_variable(
    partition_acceptance_interpretation,
    'Does ''UN partition acceptance'' in this reading mean acceptance of the specific borders of Resolution 181, or acceptance of the principle of partition with borders to be determined by subsequent events?',
    'Examine the 1947-49 diplomatic record: what did the Jewish Agency accept, what did it reject, what did it conquer beyond the partition lines, and how was the gap justified at the time vs. retrospectively.',
    'If acceptance was of the specific 181 borders, then 1948 expansion beyond those lines is extraction not justified by the pillar. If acceptance was of the principle only, the reading has more internal coherence but the ''1948 uncontested'' claim narrows to the partition lines.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partition_acceptance_interpretation, empirical, 'The historical scope of the partition acceptance claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tld_zrr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(tld_zrr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.22).
narrative_ontology:measurement(tld_zrr_tr_t1987, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(tld_zrr_tr_t1993, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(tld_zrr_tr_t2000, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(tld_zrr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(tld_zrr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(tld_zrr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(tld_zrr_be_t1987, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1987, 0.65).
narrative_ontology:measurement(tld_zrr_be_t1993, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(tld_zrr_be_t2000, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(tld_zrr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tld_zrr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.58).
narrative_ontology:measurement(tld_zrr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(tld_zrr_su_t1987, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1987, 0.7).
narrative_ontology:measurement(tld_zrr_su_t1993, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement(tld_zrr_su_t2000, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(tld_zrr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.08).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, israeli_settlement_enterprise).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_right_of_return_claim).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, jerusalem_status_claim).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, gaza_blockade_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the territorial_legitimacy_dual kernel. The zionist_refuge_reading and palestinian_autochthony_reading are mutually foreclosing on core territorial claims (each asserts exclusive legitimacy over the same territory). The two_state_coexistence_reading coexists_with both as a compromise framework but is structurally influenced by both: its viability depends on the relative power of the two foreclosing readings. The ε values differ substantially: this reading ε=0.68 (substantial extraction from Palestinians); palestinian_autochthony_reading would author high ε for Israeli occupation; two_state_coexistence_reading would author moderate ε for both sides' compromises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, institutional, 0.15).
constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, organized, 0.25).
constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
