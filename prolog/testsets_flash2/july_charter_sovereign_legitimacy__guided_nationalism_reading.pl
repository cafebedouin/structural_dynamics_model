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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: July Charter's Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This constraint describes the July Charter as interpreted through a
 *   'guided nationalism' lens, where religious identity is the foundational
 *   source of sovereign legitimacy. This reading prioritizes national unity
 *   and moral order as defined by Islamic principles, leading to the
 *   constitutional marginalization and suppression of secular institutions
 *   and religious minorities. The Charter, in this reading, functions as a
 *   snare, extracting conformity and resources from a broad victim set to
 *   benefit a narrow elite.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.85).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.9).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, snare).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter's Islamic-Nationalist Sovereign Legitimacy (Guided Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, '96d0cd4d-ba60-4f99-aa2e-cfffda567df1').
narrative_ontology:cs_kernel_codification('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', formalized).
narrative_ontology:cs_authority_grounding('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', extraction).
narrative_ontology:cs_interpretation_layer_present('96d0cd4d-ba60-4f99-aa2e-cfffda567df1').
narrative_ontology:cs_reading_relation('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', foundational, islamic_identity_as_sovereign_ground).
narrative_ontology:cs_axiom_status(islamic_identity_as_sovereign_ground, holdable).
narrative_ontology:cs_axiom_grounding('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', islamic_identity_as_sovereign_ground, theological).
narrative_ontology:cs_axiom('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', foundational, national_unity_through_religious_conformity).
narrative_ontology:cs_axiom_status(national_unity_through_religious_conformity, holdable).
narrative_ontology:cs_axiom_grounding('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', national_unity_through_religious_conformity, conventional).
narrative_ontology:cs_reference_frame('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', post_revolutionary_islamic_state).
narrative_ontology:cs_drift_state('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', contemporary_global_human_rights_norms, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('96d0cd4d-ba60-4f99-aa2e-cfffda567df1', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_institutions).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, political_opposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These elites drafted and implemented the Charter, consolidating power by framing national identity through religious terms. They benefit from the constitutional entrenchment of their ideology and control over state institutions, using it to suppress dissent and maintain their authority.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain significant influence and resources as the Charter elevates religious identity as a core component of sovereign legitimacy. Their interpretations of religious law often inform state policy, and they receive state support and protection.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Faces severe restrictions on freedom of expression, assembly, and association. Their advocacy for secular governance and human rights is often deemed un-Islamic or anti-nationalist, leading to arrests, censorship, and dissolution of organizations. Exit means exile or forced silence.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society, payer,
    powerless, biographical, trapped, national).

% Experience discrimination and marginalization as their religious identity is not aligned with the state's declared sovereign legitimacy. They face legal and social barriers, limited political representation, and sometimes violence. Identity-locked as their existence is tied to the nation, but their faith is not recognized as legitimate.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minorities, payer,
    powerless, generational, identity_locked, national).

% Any group challenging the Islamic-nationalist framework is systematically suppressed. Their political activities are criminalized, leaders are imprisoned, and their platforms are denied. Their options are capitulation, exile, or continued resistance with severe personal risk.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, political_opposition, payer,
    powerless, immediate, trapped, national).

% Monitor the human rights situation in the country, documenting abuses against secular civil society and religious minorities. They issue reports, advocate for international pressure, and provide legal aid, but have limited direct power to alter the Charter's enforcement.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_human_rights_organizations, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, islamic_nationalist_elites).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to unify the nation under a singular, religiously defined identity, providing a clear framework for governance and social cohesion after a period of political instability.
% TRANSFER_FUNCTION: Transfers political power, legal authority, and social legitimacy from secular and diverse groups to Islamic-nationalist elites and religious institutions, while extracting compliance and conformity from the populace.
% ABSENT_VOICES: Secular intellectuals, liberal reformers, and representatives of non-Islamic religious communities are systematically excluded from the constitutional discourse. They would advocate for a pluralistic, secular state, but their voices are silenced through legal and coercive means.
% DISAPPEARANCE_RATIONALE: If the Charter's Islamic-nationalist framework vanished, the state's entire legitimacy structure would collapse. Power would immediately fragment, secular and minority groups would reassert their rights, and the political landscape would undergo a profound and potentially violent reorganization.
% FOUNDING_PROBLEM: The Charter was established to address perceived national fragmentation, moral decay, and external cultural influence following a revolutionary period, aiming to re-establish a strong, unified national identity rooted in Islamic principles.
% FOUNDING_PROBLEM_CORROBORATION: Islamic-nationalist elites and state-controlled media consistently assert the founding problem is live, citing ongoing threats to national identity and stability. However, secular scholars and international observers (outside the benefiting parties) argue that the 'problem' is largely a construct used to justify authoritarian rule and that the original issues have been superseded by the Charter's own repressive effects.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high because the Charter, under this reading, systematically reallocates power and resources to the Islamic-nationalist establishment at the expense of other groups. Suppression is very high due to the active and often violent enforcement mechanisms used to quash dissent and maintain the religiously defined social order. The theater ratio is relatively low, as the state genuinely pursues its Islamic-nationalist agenda, with less performative cover and more direct coercion. The claimed type 'snare' reflects the clear victim set and the coercive nature of its persistence.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Islamic-nationalist elites, the Charter is a legitimate framework for national salvation and moral guidance (a 'rope' or even 'mountain' of divine law). From the perspective of secular civil society and religious minorities, it is a coercive instrument of control and discrimination (a 'snare'). The engine's computation will highlight this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic-nationalist elites and religious institutions are clear beneficiaries, as the Charter entrenches their power and influence. Secular civil society, religious minorities, and political opposition are direct victims, facing severe restrictions and extraction. The directionality for beneficiaries is low (subsidized), while for victims it is high (targeted).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charter_naturalness_vs_construction,
    'Is the Charter''s Islamic-nationalist framework a natural expression of the nation''s identity, or a constructed political tool to consolidate power?',
    'Historical analysis of pre-Charter political discourse, public opinion surveys (if feasible), and comparative constitutional studies of similar post-revolutionary states.',
    'If a natural expression, the constraint might lean towards a ''mountain'' or ''rope'' for some segments of the population. If a constructed tool, it reinforces the ''snare'' classification due to its instrumental nature for elite benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_naturalness_vs_construction, conceptual, 'Ambiguity regarding the inherent ''naturalness'' of the Islamic-nationalist framework versus its strategic political construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal barriers, state coercion) or internalized (self-censorship, fear-driven conformity)?',
    'Post-regime-change analysis: if suppression persists after the coercive state apparatus is removed, it indicates a significant internalized component. If it rapidly dissipates, it''s predominantly structural.',
    'If internalized, the effective suppression is higher and more resilient than structural measures suggest, making resistance harder. If purely structural, removing the coercive mechanisms would lead to faster liberalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of state-imposed religious nationalism.').

omega_variable(
    secular_democratic_reading_viability,
    'To what extent can the Charter be reinterpreted to support a secular democratic framework, given its current text and historical context?',
    'Legal scholarship and constitutional court rulings (if independent) exploring alternative interpretations, and the success or failure of political movements advocating for such a reinterpretation.',
    'If a secular democratic reading is viable, it suggests the Charter is a ''tangled rope'' with a suppressed coordination function. If not, the ''snare'' classification is reinforced, as the text itself is a tool of extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_democratic_reading_viability, conceptual, 'The potential for reinterpretation of the Charter towards secular democratic principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_tr_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(july_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(july_tr_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(july_be_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(july_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(july_be_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(july_su_t5, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement(july_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.87).
narrative_ontology:measurement(july_su_t15, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 15, 0.89).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_media_censorship).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_education_curriculum).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'july_charter_sovereign_legitimacy' kernel. This 'guided nationalism' reading emphasizes religious identity as the basis of state power, contrasting with secular democratic and military custodian interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
