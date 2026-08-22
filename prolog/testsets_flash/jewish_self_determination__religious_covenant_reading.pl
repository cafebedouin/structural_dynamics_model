% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Self-Determination: Religious Covenant Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the Jewish claim to the land as derived from a
 *   divine covenant, framing territorial sovereignty as a religious
 *   obligation. This reading positions the claim as a 'mountain' (divine
 *   command, immutable) but its operationalization in secular political
 *   contexts, particularly through the settlement enterprise, functions as a
 *   'tangled rope' or 'snare' due to its high extractiveness and suppression
 *   of alternative frameworks and populations. The claimed type is 'mountain'
 *   reflecting the internal theological perspective, while the metrics
 *   reflect its external impact and enforcement.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: Primary agenda-setter (institutional/identity_locked) — interprets and enforces the divine mandate.
 *   - settlement_enterprise: Primary beneficiary (organized/identity_locked) — directly benefits from the theological justification for expansion.
 *   - secular_negotiation_frameworks: Primary victim (institutional/trapped) — undermined by the religious claim.
 *   - palestinian_population: Primary victim (powerless/trapped) — bears the direct costs of territorial expansion.
 *   - international_community: Observer (institutional/analytical) — attempts to mediate but is often dismissed by the religious framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.85).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.9).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Self-Determination: Religious Covenant Reading").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, 'e9d2abc2-e265-45ce-a1b0-92dd76badd70').
narrative_ontology:cs_kernel_codification('e9d2abc2-e265-45ce-a1b0-92dd76badd70', fixed_text).
narrative_ontology:cs_authority_grounding('e9d2abc2-e265-45ce-a1b0-92dd76badd70', lineage).
narrative_ontology:cs_interpretation_layer_present('e9d2abc2-e265-45ce-a1b0-92dd76badd70').
narrative_ontology:cs_reading_relation('e9d2abc2-e265-45ce-a1b0-92dd76badd70', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('e9d2abc2-e265-45ce-a1b0-92dd76badd70', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('e9d2abc2-e265-45ce-a1b0-92dd76badd70', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9d2abc2-e265-45ce-a1b0-92dd76badd70', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('e9d2abc2-e265-45ce-a1b0-92dd76badd70', foundational, divine_covenant_unconditional_land_grant).
narrative_ontology:cs_axiom_status(divine_covenant_unconditional_land_grant, holdable).
narrative_ontology:cs_axiom_grounding('e9d2abc2-e265-45ce-a1b0-92dd76badd70', divine_covenant_unconditional_land_grant, theological).
narrative_ontology:cs_axiom('e9d2abc2-e265-45ce-a1b0-92dd76badd70', foundational, territorial_sovereignty_religious_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_religious_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e9d2abc2-e265-45ce-a1b0-92dd76badd70', territorial_sovereignty_religious_obligation, deontological).
narrative_ontology:cs_reference_frame('e9d2abc2-e265-45ce-a1b0-92dd76badd70', biblical_covenantal_mandate).
narrative_ontology:cs_drift_state('e9d2abc2-e265-45ce-a1b0-92dd76badd70', contemporary_political_reality, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9d2abc2-e265-45ce-a1b0-92dd76badd70', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_negotiation_frameworks).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the divine covenant as a mandate for Jewish sovereignty over the entire land, viewing territorial compromise as a religious transgression. Actively promotes and supports settlement expansion, influencing state policy through political and religious institutions. Their identity is fused with the fulfillment of this covenant.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    institutional, generational, identity_locked, national).

% Directly benefits from the religious covenant reading, which provides a theological justification for their presence and expansion in disputed territories. They receive state support and protection, and their actions are framed as fulfilling a divine command, making exit unthinkable for many participants.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, identity_locked, local).

% Represents the diplomatic and legal structures for territorial compromise based on international law and political agreements. This framework is undermined and often foreclosed by the religious covenant reading, which asserts a non-negotiable divine right to the land, rendering secular solutions illegitimate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_negotiation_frameworks, payer,
    institutional, biographical, trapped, regional).
narrative_ontology:stakeholder_non_agent(jewish_self_determination__religious_covenant_reading, secular_negotiation_frameworks).

% Bears the direct costs of territorial expansion justified by the religious covenant reading, including displacement, loss of land, and denial of self-determination. Their claims to indigenous rights and self-determination are systematically suppressed by the theological framing of Jewish sovereignty.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Observes the conflict, often attempting to mediate through secular frameworks. Their efforts are frequently frustrated by the religious covenant reading, which dismisses international law as irrelevant to a divine mandate. They can impose diplomatic or economic pressure but cannot alter the theological premise.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions and identity of religious Zionist groups and settlers around a shared theological mandate for territorial control, providing a coherent framework for collective action and legitimizing expansion.
% TRANSFER_FUNCTION: Transfers land, resources, and political authority from the Palestinian population and secular negotiation frameworks to the religious Zionist movement and settlement enterprise, justified by divine right.
% ABSENT_VOICES: Palestinian voices advocating for self-determination and secular governance are systematically excluded from the discourse shaped by the religious covenant reading. Diasporist Jewish voices, who prioritize Jewish ethical universalism over territorial nationalism, are also marginalized.
% DISAPPEARANCE_RATIONALE: If the religious covenant reading vanished overnight, the theological justification for settlement expansion would collapse, significantly altering the political landscape. The religious Zionist movement would lose its core ideological coherence, and secular negotiation frameworks would gain legitimacy, leading to a fundamental reorganization of territorial claims and power dynamics.
% FOUNDING_PROBLEM: The perceived existential threat to Jewish identity and continuity after centuries of diaspora and persecution, leading to a desire for secure self-determination in the ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist leaders and their followers attest that the problem of Jewish insecurity and the divine mandate remain live. Critics, including some Jewish scholars and human rights organizations, acknowledge the historical context of insecurity but argue that the religious covenant reading has been instrumentalized to justify ongoing dispossession, shifting the nature of the 'problem' from existential threat to expansionist ideology.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) and suppression (0.9) reflect the real-world impact of this reading: it actively dispossesses one population while foreclosing secular avenues for resolution. The 'mountain' claim is based on the internal logic of divine command, which is absolute within its own framework, leading to a high accessibility collapse (0.95) for those who accept it. However, the active enforcement and resistance (0.7) indicate it is not a universally accepted natural law, but a contested claim requiring continuous defense. The low theater ratio (0.2) suggests that the religious justification is genuinely held and acted upon, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious Zionist movement, this is a divine mandate, an unchangeable 'mountain' that coordinates their actions. From the perspective of the Palestinian population and secular negotiation frameworks, it operates as a 'snare' or 'tangled rope,' extracting land and rights through a religiously justified, actively enforced system. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist movement and settlement enterprise are clear beneficiaries (d near 0.0) as they gain land and political influence, and their identity is locked into the constraint. Secular negotiation frameworks and the Palestinian population are clear targets (d near 1.0) as they lose land, rights, and legitimacy. The international community is an observer, attempting to influence but not directly subject to the constraint's internal logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (divine covenant) is considered 'live' by its proponents, preventing a 'piton' classification. However, the high extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest that while the mandate persists, its function has drifted significantly from addressing existential insecurity to justifying territorial expansion. The classification as a 'mountain' (claimed type) with high extractiveness (metrics) highlights this tension, preventing mislabeling it as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_political_instrumentalization,
    'Is the religious covenant reading a genuine, immutable divine mandate, or has it been instrumentalized to serve political and territorial expansionist goals?',
    'Theological and historical analysis of the evolution of interpretations, combined with empirical observation of the correlation between religious rhetoric and political outcomes (e.g., settlement growth vs. security needs).',
    'If instrumentalized, the ''mountain'' claim is a cover for a ''snare'' or ''tangled rope,'' increasing effective extractiveness and justifying reclassification. If a genuine mandate, the internal logic of the ''mountain'' holds, but its external impact remains extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_political_instrumentalization, conceptual, 'Ambiguity between genuine religious belief and political instrumentalization of a divine claim.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option for religious Zionists and settlers a genuine internal commitment, versus a product of structural coercion and social pressure within their communities?',
    'Sociological studies of individual motivations and exit narratives, analysis of community sanctions for dissent, and comparison with similar identity-based movements.',
    'If primarily coerced, the effective suppression is higher, and the ''identity_locked'' status is less about free commitment and more about structural entrapment, potentially reclassifying the constraint as more snare-like for these agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Distinguishing genuine identity fusion from community-level coercion in ''identity_locked'' agents.').

omega_variable(
    secular_framework_legitimacy,
    'Can secular negotiation frameworks ever achieve legitimacy or efficacy in resolving territorial disputes when confronted with a claim grounded in divine covenant?',
    'Empirical observation of successful or failed negotiations in similar contexts, and analysis of the conditions under which religious and secular claims can be reconciled or compartmentalized.',
    'If secular frameworks are inherently foreclosed, the constraint''s suppression of alternatives is absolute, reinforcing its ''mountain'' aspect for proponents and its ''snare'' aspect for targets. If reconciliation is possible, the suppression is contingent, opening pathways for resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_framework_legitimacy, empirical, 'The inherent compatibility or incompatibility of religious and secular claims in territorial disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(jewi_tr_t1980, jewish_self_determination__religious_covenant_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(jewi_tr_t1995, jewish_self_determination__religious_covenant_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__religious_covenant_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(jewi_be_t1980, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(jewi_be_t1995, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1995, 0.8).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(jewi_su_t1980, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(jewi_su_t1995, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1995, 0.85).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
