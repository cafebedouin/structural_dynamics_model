% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter: Guided Nationalism Reading (Religious Identity as Sovereign Legitimacy)
 *   domain: constitutional_law/political_transitions/state_building
 *
 * SUMMARY:
 *   This constraint story instantiates the 'guided_nationalism_reading' of
 *   the July Charter, which establishes an Islamic-nationalist framework with
 *   religious identity as the sovereign legitimacy ground. This reading
 *   posits that the charter's primary function is to unify the nation under a
 *   shared religious and national identity, providing a stable basis for
 *   governance. However, this comes at the cost of constraining secular
 *   institutions, elevating religious law, and creating identifiable victims
 *   among secular civil society and religious minorities. The claimed type is
 *   'tangled_rope' because it purports a coordination function (national
 *   unity) but operates with significant extraction and suppression.
 *
 * KEY AGENTS:
 *   - islamic_nationalist_elites: Primary agenda_setter (institutional/arbitrage) — defines and enforces the framework, benefits from its legitimacy.
 *   - religious_establishment: Primary beneficiary (institutional/constrained) — gains constitutional status and influence.
 *   - secular_civil_society: Primary payer (powerless/identity_locked) — constrained, marginalized, rights curtailed.
 *   - religious_minorities: Primary payer (powerless/trapped) — face discrimination and legal disadvantages.
 *   - military_leadership: Powerful observer (institutional/constrained) — monitors stability, potential arbiter of order.
 *   - international_human_rights_bodies: Analytical observer (analytical/analytical) — critiques human rights impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.85).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter: Guided Nationalism Reading (Religious Identity as Sovereign Legitimacy)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '9eae0ef5-d457-4b9c-8476-48f7b20d9d83').
narrative_ontology:cs_kernel_codification('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', fixed_text).
narrative_ontology:cs_authority_grounding('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', lineage).
narrative_ontology:cs_interpretation_layer_present('9eae0ef5-d457-4b9c-8476-48f7b20d9d83').
narrative_ontology:cs_reading_relation('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', foundational, islamic_law_is_supreme).
narrative_ontology:cs_axiom_status(islamic_law_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', islamic_law_is_supreme, deontological).
narrative_ontology:cs_axiom('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', foundational, national_identity_is_islamic).
narrative_ontology:cs_axiom_status(national_identity_is_islamic, holdable).
narrative_ontology:cs_axiom_grounding('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', national_identity_is_islamic, conventional).
narrative_ontology:cs_reference_frame('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', islamic_state_founding_principles).
narrative_ontology:cs_drift_state('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', contemporary_globalized_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9eae0ef5-d457-4b9c-8476-48f7b20d9d83', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, national_unity_through_faith).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These elites define, interpret, and enforce the charter's Islamic-nationalist framework. They derive their legitimacy and power from this framework, benefiting from the exclusion of secular and minority voices. Their position is secured by the charter's constitutional status.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% The religious establishment gains constitutional status, significant influence over legal and social norms, and state funding. While benefiting, their autonomy is constrained by their integration into the state apparatus, making exit from the state-sanctioned role difficult.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_establishment, beneficiary,
    institutional, generational, constrained, national).

% Secular institutions and individuals find their rights curtailed, their worldview marginalized, and their political participation severely constrained. Many are 'identity_locked' as their professional and social lives are tied to the nation, making physical or ideological exit highly costly.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    powerless, biographical, identity_locked, national).

% Religious minorities face systemic discrimination, legal disadvantages, and social pressure due to the charter's emphasis on a dominant religious identity. Their options are limited to assimilation, emigration, or enduring marginalization, making them effectively 'trapped'.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, generational, trapped, national).

% The military leadership observes the implementation of the charter, primarily concerned with national stability. While not the primary ideological driver of this reading, they are a powerful actor whose actions can ratify or challenge the framework's enforcement, often acting as a final arbiter of order.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, military_leadership, observer,
    institutional, generational, constrained, national).

% These bodies monitor and critique the charter's impact on universal human rights, freedom of religion, and minority protections. Their influence is primarily through reporting and advocacy, rather than direct enforcement within the nation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_bodies, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To unify the nation post-revolution by establishing a shared religious and national identity as the basis for state legitimacy, thereby providing a stable framework for governance and social cohesion.
% TRANSFER_FUNCTION: Transfers political power, legal authority, and social legitimacy from diverse, secular, or minority groups to those aligned with the dominant Islamic-nationalist identity and its associated institutions.
% ABSENT_VOICES: Exiled secular intellectuals, leaders of suppressed minority religious and ethnic groups, and international advocates for universal human rights are structurally excluded from the national discourse. They would argue for a pluralistic, secular state and universal rights.
% DISAPPEARANCE_RATIONALE: If the charter and its legitimacy ground vanished overnight, the entire state's constitutional order, legal system, and social contract would collapse. This would lead to a profound crisis of governance, potential civil unrest, and a fundamental re-evaluation of national identity and power structures.
% FOUNDING_PROBLEM: The charter was established to address post-revolutionary instability, a perceived lack of national unity, moral decay, and a desire to assert an indigenous, religiously-grounded identity against perceived foreign influences and colonial legacies.
% FOUNDING_PROBLEM_CORROBORATION: Islamic scholars and nationalist historians within the state corroborate the founding problem's historical context and ongoing relevance. Secular historians, human rights organizations, and international observers contest its current status, arguing that the original problems are largely solved and the framework now serves primarily to consolidate power and suppress dissent.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant transfer of power and resources from secular and minority groups to the Islamic-nationalist establishment. Suppression (0.85) is very high due to active enforcement mechanisms that curtail dissent, restrict alternative political and social organizations, and legally disadvantage non-conforming identities. The theater ratio (0.20) is relatively low because the enforcement of the religious-nationalist framework is genuinely active and effective, not merely performative. Accessibility collapse (0.70) is substantial as viable alternatives for political participation or social organization outside the framework are severely limited. Resistance (0.60) is moderate, indicating ongoing but often suppressed opposition from secular and minority groups, as well as international criticism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Islamic-nationalist elites and the religious establishment, the charter is a legitimate and necessary framework for national unity and moral order (a 'rope' or even 'mountain' of national identity). From the perspective of secular civil society and religious minorities, it is a coercive structure that extracts their rights and marginalizes their existence (a 'snare'). The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Islamic-nationalist elites and religious establishment are clear beneficiaries, with the former acting as the agenda-setter, controlling the interpretation and enforcement of the charter. Secular civil society and religious minorities are the primary targets, bearing the costs of constrained rights and systemic discrimination. The military leadership, while powerful, acts more as an observer of this specific constraint, with its own legitimacy derived from a different reading of the charter (military custodian).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this constraint as a pure Rope (which would ignore the substantial extraction and suppression) or a pure Snare (which would ignore the genuine, albeit contested, coordination function of national unity and identity formation). The 'contested' status of the founding problem further highlights the potential for mandatrophy, where the original problem of instability may have been superseded by the framework's function as a tool for power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_unity_vs_exclusion,
    'To what extent does the charter''s ''national unity'' function genuinely coordinate diverse elements of society, versus primarily serving as a cover for the exclusion and suppression of non-conforming groups?',
    'Empirical studies on social cohesion, political participation rates across different identity groups, and the extent of legal and social discrimination. If cohesion is achieved through forced conformity rather than genuine integration, the coordination claim is weakened.',
    'If the ''national unity'' claim is primarily cover, the constraint''s effective extractiveness and suppression are higher, pushing it closer to a pure Snare. If genuine, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_unity_vs_exclusion, empirical, 'Ambiguity between genuine coordination and exclusionary cover story for national unity.').

omega_variable(
    religious_identity_authenticity,
    'Is the ''religious identity'' grounding a genuine expression of popular piety and cultural heritage, or is it primarily a political tool used by elites to consolidate power and legitimize their rule?',
    'Sociological surveys of religious belief and practice, analysis of state-religious institution relationships, and historical studies of the charter''s drafting and implementation. Divergence between popular piety and state-imposed religious norms would indicate political instrumentalization.',
    'If primarily a political tool, the ''lineage'' authority grounding is undermined, and the ''extraction'' component of the constraint is amplified, strengthening the Snare-like aspects. If authentic, it reinforces the claimed legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_identity_authenticity, conceptual, 'Ambiguity of religious identity as genuine vs. political tool.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of secular and minority voices primarily structural (legal barriers, state coercion) or internalized (self-censorship, identity fusion with the dominant narrative)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., through self-censorship or social pressure) even if formal legal barriers are removed, it indicates a significant internalized component. Analysis of diaspora communities'' political activity could also provide insight.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them after any theoretical ''exit'' from direct state control. This makes the constraint more resilient to external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for secular and minority groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(july_tr_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement(july_tr_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.69).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(july_be_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(july_be_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(july_su_t25, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 25, 0.85).
narrative_ontology:measurement(july_su_t30, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
