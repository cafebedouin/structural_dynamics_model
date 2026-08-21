% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Expansionist Legalist Jihad Doctrine
 *   domain: Islamic Jurisprudence / Comparative Religious Law / Political Theology
 *
 * SUMMARY:
 *   This constraint represents the 'expansionist legalist' reading of Jihad
 *   within the Quranic corpus, which interprets it as an obligation to
 *   establish Islamic governance where it is absent, permitting offensive
 *   campaigns under specific jurisprudential conditions (invitation to Islam,
 *   imam authority, proportionality). This reading is distinct from purely
 *   defensive or spiritual interpretations and from revolutionary vanguard
 *   approaches that bypass state authority. The constraint's high
 *   extractiveness and suppression reflect the coercive nature of
 *   establishing governance through conquest, while its low theater ratio
 *   indicates a direct, functional application of force rather than
 *   performative maintenance. The claimed type is 'snare' because the
 *   coordination story (establishing justice/governance) serves as cover for
 *   the underlying extraction of sovereignty and resources from conquered
 *   populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.85).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.9).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Expansionist Legalist Jihad Doctrine").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "Islamic Jurisprudence / Comparative Religious Law / Political Theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '1a8de5b4-9875-4078-b99c-6f28fd97ad77').
narrative_ontology:cs_kernel_codification('1a8de5b4-9875-4078-b99c-6f28fd97ad77', fixed_text).
narrative_ontology:cs_authority_grounding('1a8de5b4-9875-4078-b99c-6f28fd97ad77', lineage).
narrative_ontology:cs_interpretation_layer_present('1a8de5b4-9875-4078-b99c-6f28fd97ad77').
narrative_ontology:cs_reading_relation('1a8de5b4-9875-4078-b99c-6f28fd97ad77', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('1a8de5b4-9875-4078-b99c-6f28fd97ad77', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('1a8de5b4-9875-4078-b99c-6f28fd97ad77', foundational, obligation_to_establish_islamic_governance_globally).
narrative_ontology:cs_axiom_status(obligation_to_establish_islamic_governance_globally, holdable).
narrative_ontology:cs_axiom_grounding('1a8de5b4-9875-4078-b99c-6f28fd97ad77', obligation_to_establish_islamic_governance_globally, deontological).
narrative_ontology:cs_axiom('1a8de5b4-9875-4078-b99c-6f28fd97ad77', foundational, jihad_declaration_monopoly_of_imam_or_state).
narrative_ontology:cs_axiom_status(jihad_declaration_monopoly_of_imam_or_state, holdable).
narrative_ontology:cs_axiom_grounding('1a8de5b4-9875-4078-b99c-6f28fd97ad77', jihad_declaration_monopoly_of_imam_or_state, conventional).
narrative_ontology:cs_reference_frame('1a8de5b4-9875-4078-b99c-6f28fd97ad77', classical_islamic_legal_tradition).
narrative_ontology:cs_drift_state('1a8de5b4-9875-4078-b99c-6f28fd97ad77', contemporary_nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a8de5b4-9875-4078-b99c-6f28fd97ad77', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_caliphate).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, conquered_rulers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority (historically a Caliphate or Islamic state) that declares and leads offensive jihad campaigns, establishes Islamic governance in conquered territories, and collects resources from them. Its legitimacy is tied to upholding this doctrine.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_caliphate, agenda_setter,
    institutional, generational, arbitrage, global).

% The global Muslim community, which benefits from the expansion of Islamic governance, the perceived establishment of justice, and the increased influence of Islamic law. Its collective identity is often fused with the success and expansion of the Islamic state.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma, beneficiary,
    organized, generational, identity_locked, global).

% Populations in conquered territories who are subjected to new Islamic governance, potentially granted dhimmi status (protected non-Muslims), required to pay specific taxes (jizya), and lose their prior political and legal autonomy. Their alternatives are conversion, subjugation, or flight.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, generational, trapped, regional).

% The former political and military leaders of territories subjected to offensive jihad. They lose their sovereignty, power, and resources, often facing death, exile, or integration into the new administrative structure at a subordinate level.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, conquered_rulers, payer,
    powerful, biographical, trapped, regional).

% The religious scholars and legal experts who interpret, codify, and transmit the jurisprudential conditions for jihad, providing the theological and legal legitimacy for the state's actions. They guide the application of the doctrine but are also bound by its tradition.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_jurists_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Adherents to interpretations of jihad as primarily internal spiritual struggle or strictly defensive warfare. Their views are marginalized or condemned by the expansionist legalist reading, which permits offensive campaigns, thus excluding them from the dominant discourse on state action.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_proponents, excluded,
    moderate, biographical, constrained, global).

% Advocates for individual, non-state-sanctioned jihad against perceived apostate rulers or occupiers. Their interpretation is explicitly rejected by the expansionist legalist reading, which insists on the sole authority of the imam or state for declaring jihad, thus excluding them from legitimate action.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_proponents, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal and military framework for the expansion of Islamic governance into non-Muslim lands, ensuring adherence to jurisprudential conditions such as prior invitation to Islam, imam authority, and proportionality in warfare.
% TRANSFER_FUNCTION: Transfers sovereignty, political authority, and material resources from non-Muslim polities to the Islamic state, and imposes Islamic law and social order on conquered populations, often through taxation (jizya) and land redistribution.
% ABSENT_VOICES: The non-Muslim populations and their former rulers, who would object to the loss of sovereignty, imposition of new rule, and the extractive nature of conquest. Also, proponents of purely defensive or spiritual jihad, whose interpretations are sidelined by this doctrine.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished, the historical and contemporary justifications for state-led offensive expansion would disappear, fundamentally altering the political theology of Islamic states and non-state actors, and removing a key framework for legitimizing conquest and the imposition of Islamic governance.
% FOUNDING_PROBLEM: The perceived absence of Islamic governance and justice in lands outside Muslim rule, and the theological obligation to extend God's law and the domain of Islam (Dar al-Islam) to all humanity, ensuring universal justice and submission to God.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within certain classical Islamic legal schools and historical narratives corroborate this founding problem and its ongoing relevance. Critics (e.g., modern international law scholars, some Muslim reformists, and proponents of defensive jihad) contest its contemporary applicability or original intent, arguing the problem is either solved or misidentified.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) stems from the fundamental transfer of sovereignty, resources, and legal systems from conquered entities to the Islamic state. Suppression (0.90) is necessary to overcome resistance, enforce new laws, and maintain control over diverse populations. The low theater ratio (0.10) reflects that the doctrine's function is direct and coercive; there is little performative maintenance when the goal is active expansion and imposition of rule. Accessibility collapse is high (0.80) for conquered populations, as their prior political and legal alternatives are systematically dismantled. Resistance is also high (0.75) given the inherent conflict in such expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Islamic state and the Muslim Umma, this doctrine is a legitimate and necessary mechanism for establishing divine justice and expanding the domain of Islam. From the perspective of conquered non-Muslim populations and their rulers, it is a mechanism of conquest and subjugation. The engine's classification will highlight this divergence, showing a 'snare' from the victim's seat and potentially a 'tangled_rope' or even 'rope' from the beneficiary's seat, depending on the perceived coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   The Islamic state/caliphate and the Muslim Umma are the primary beneficiaries, gaining expanded territory, resources, and the fulfillment of a theological obligation. Non-Muslim populations and their former rulers are the clear targets/victims, losing sovereignty, autonomy, and resources. Islamic jurists and scholars act as agenda-setters, providing the interpretive framework and legitimacy for the state's actions. Proponents of alternative jihad readings are excluded, as their interpretations are incompatible with the state-led offensive approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''expansionist legalist'' reading of the ''jihad_quranic_corpus'' kernel, distinct from its sibling readings?',
    'Comparative jurisprudential analysis of classical and modern Islamic legal texts, focusing on the conditions for offensive jihad, the role of state authority, and the treatment of non-Muslims.',
    'If misidentified, the classification of this constraint would be inaccurate, and its relationship to sibling readings (defensive_spiritual_reading, revolutionary_vanguard_reading) would be structurally incorrect, leading to flawed kernel-level analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the precise identification of this specific reading within the broader kernel of Jihad.').

omega_variable(
    legitimacy_of_expansion_vs_extraction,
    'Is the expansion of Islamic governance, as permitted by this reading, primarily driven by a theological imperative for justice, or by a desire for political power and material resources?',
    'Historical analysis of the motivations and outcomes of specific campaigns, examining resource transfers, treatment of conquered populations, and the long-term stability of established governance versus short-term gains.',
    'If primarily for power/resources, the ''snare'' classification is strongly reinforced. If genuinely for justice, the coordination function might be weighted higher, potentially shifting the classification towards a ''tangled_rope'' from some perspectives, though still extractive for the conquered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_expansion_vs_extraction, empirical, 'Distinguishing between theological justification and material/political drivers of expansion.').

omega_variable(
    contemporary_relevance_of_doctrine,
    'Is this expansionist legalist reading of jihad still a ''holdable'' and actively pursued doctrine by recognized state or non-state actors in the contemporary international system, or has it been largely superseded by defensive interpretations?',
    'Analysis of official state policies, military doctrines, and jurisprudential rulings from recognized Islamic authorities, as well as the stated goals and actions of non-state actors claiming adherence to this reading.',
    'If largely superseded, the ''founding_problem_status'' might shift from ''contested'' to ''dead'' for many actors, and the constraint''s overall influence and active enforcement would be significantly lower, potentially pushing it towards a ''piton'' or ''rope'' for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_relevance_of_doctrine, empirical, 'Assessing the contemporary holdability and active application of the expansionist legalist doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 622, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t622, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 622, 0.1).
narrative_ontology:measurement(jiha_tr_t800, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 800, 0.08).
narrative_ontology:measurement(jiha_tr_t1200, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1200, 0.07).
narrative_ontology:measurement(jiha_tr_t1600, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1600, 0.09).
narrative_ontology:measurement(jiha_tr_t1900, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t622, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 622, 0.7).
narrative_ontology:measurement(jiha_be_t800, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 800, 0.8).
narrative_ontology:measurement(jiha_be_t1200, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1200, 0.85).
narrative_ontology:measurement(jiha_be_t1600, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1600, 0.82).
narrative_ontology:measurement(jiha_be_t1900, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1900, 0.78).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t622, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 622, 0.8).
narrative_ontology:measurement(jiha_su_t800, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 800, 0.85).
narrative_ontology:measurement(jiha_su_t1200, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1200, 0.9).
narrative_ontology:measurement(jiha_su_t1600, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1600, 0.88).
narrative_ontology:measurement(jiha_su_t1900, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
