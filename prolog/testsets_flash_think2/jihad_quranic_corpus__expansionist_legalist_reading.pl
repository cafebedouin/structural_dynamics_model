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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Jihad as Expansionist Legal Obligation
 *   domain: islamic_jurisprudence/political_theology/comparative_religious_law
 *
 * SUMMARY:
 *   This constraint instantiates the 'expansionist legalist' reading of jihad
 *   from the Quranic corpus. It defines jihad as an obligation to establish
 *   Islamic governance where it is absent, permitting offensive campaigns
 *   under specific jurisprudential conditions: prior invitation to Islam,
 *   declaration by a legitimate imam/caliph, and adherence to
 *   proportionality. This reading legitimizes systematic expansion and
 *   conquest within a legal framework, placing non-Muslims in a liminal
 *   status as potential dhimmi or combatants. This is one reading of the
 *   'jihad_quranic_corpus' kernel, distinct from purely defensive or
 *   revolutionary interpretations.
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
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad as Expansionist Legal Obligation").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "islamic_jurisprudence/political_theology/comparative_religious_law").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'de6a9965-cae3-4e6a-9f22-49ad086e5cc9').
narrative_ontology:cs_kernel_codification('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', fixed_text).
narrative_ontology:cs_authority_grounding('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', lineage).
narrative_ontology:cs_interpretation_layer_present('de6a9965-cae3-4e6a-9f22-49ad086e5cc9').
narrative_ontology:cs_reading_relation('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', foundational, islamic_governance_is_obligatory).
narrative_ontology:cs_axiom_status(islamic_governance_is_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', islamic_governance_is_obligatory, deontological).
narrative_ontology:cs_axiom('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', foundational, offensive_jihad_is_permissible).
narrative_ontology:cs_axiom_status(offensive_jihad_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', offensive_jihad_is_permissible, conventional).
narrative_ontology:cs_axiom('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', secondary, imam_authority_is_sole_declarer).
narrative_ontology:cs_axiom_status(imam_authority_is_sole_declarer, holdable).
narrative_ontology:cs_axiom_grounding('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', imam_authority_is_sole_declarer, conventional).
narrative_ontology:cs_reference_frame('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', early_islamic_conquests_model).
narrative_ontology:cs_drift_state('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', contemporary_international_law_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('de6a9965-cae3-4e6a-9f22-49ad086e5cc9', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_caliphate).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_community).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, rival_political_entities).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, islamic_supremacy_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, divine_law_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The political entity (historical caliphate or modern state claiming its mantle) that declares and conducts offensive jihad, establishes Islamic governance, and collects resources from conquered territories. It benefits from expanded authority and resources.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_caliphate, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the expansion of Islamic governance, perceived divine justice, and potentially from resources acquired through conquest. Members are called upon to participate in jihad, but also receive protection and the benefits of the established order.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_community, beneficiary,
    organized, generational, constrained, global).

% Targeted by offensive campaigns, they face subjugation, conversion, or payment of jizya (poll tax) under dhimmi status. Their political autonomy and religious freedom are severely curtailed. Exit options are limited to flight, resistance, or submission.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, biographical, trapped, local).

% Existing states or empires whose territories are targeted for conquest. They bear the costs of war and risk losing sovereignty and resources. Their options are to resist militarily or negotiate terms of surrender/tribute.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, rival_political_entities, payer,
    powerful, biographical, constrained, regional).

% Scholars and legal experts who interpret the Quran and Sunnah to derive the jurisprudential conditions for jihad, including the permissibility of offensive campaigns, the necessity of invitation, and the role of the imam. They provide the theological and legal justification for the constraint.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, ulama_jurists, agenda_setter,
    institutional, generational, analytical, global).

% Proponents of interpretations that limit jihad to internal spiritual struggle or purely defensive warfare. Their views are marginalized or actively suppressed by the expansionist legalist framework, which views their position as undermining a divine obligation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_advocates, excluded,
    organized, biographical, constrained, global).

% Groups advocating for immediate, individual jihad against perceived apostate rulers, bypassing state authority. Their approach is condemned by the expansionist legalist reading, which insists on the sole authority of the legitimate imam/caliph for declaring offensive jihad.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_groups, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified Islamic political and legal order across diverse populations, ensuring adherence to divine law and providing security and justice for the Muslim community through the expansion of Islamic governance.
% TRANSFER_FUNCTION: Transfers sovereignty, political authority, and material resources (e.g., land, wealth, jizya) from non-Islamic entities and populations to the Islamic state/caliphate and the Muslim community.
% ABSENT_VOICES: Non-Muslim populations and rival political entities, whose perspectives on self-determination, religious freedom, and political autonomy are suppressed. Also, proponents of purely defensive or spiritual jihad readings, whose interpretations are marginalized by this framework.
% DISAPPEARANCE_RATIONALE: If this expansionist legalist interpretation of jihad vanished, the theological and jurisprudential justification for offensive campaigns and the establishment of Islamic governance by force would disappear. This would fundamentally reorder political theology, international relations, and the historical and contemporary status of non-Muslims in lands where Islamic governance was established through conquest.
% FOUNDING_PROBLEM: The perceived absence of comprehensive Islamic governance and the need to spread divine justice and the message of Islam to all humanity, removing obstacles to its acceptance and ensuring the supremacy of God's law.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within certain jurisprudential schools and political movements attest to its ongoing relevance, citing the continued existence of non-Islamic governance and perceived injustices. Critics (e.g., defensive-spiritual advocates, secular scholars, international law experts) dispute its legitimacy and contemporary applicability, arguing it's a historical interpretation no longer valid or applicable in modern contexts. The contestation itself confirms the 'live' status of the problem for its adherents.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading justifies the forceful acquisition of sovereignty, territory, and resources from non-Muslim entities. Suppression is very high (0.90) as it necessitates active military campaigns and subsequent enforcement to maintain control over conquered populations and prevent dissent or re-establishment of non-Islamic rule. The theater ratio is low (0.10) because the function is direct and coercive; while 'invitation to Islam' serves a rhetorical purpose, the underlying mechanism is military force and political subjugation, not persuasion. Accessibility collapse is high for non-Muslims, as their options are severely limited to submission, flight, or resistance. Resistance is also high, as such campaigns naturally provoke strong opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Islamic state/caliphate and its adherents, this constraint represents a divinely ordained obligation to establish justice and spread Islam, a righteous endeavor. From the perspective of non-Muslim populations and rival political entities, it is an act of aggression, conquest, and subjugation. The constraint's internal logic frames this as a necessary and just expansion, while external parties experience it as coercive extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The Islamic state/caliphate and the Muslim community are the primary beneficiaries, gaining expanded authority, resources, and the perceived fulfillment of a divine mandate. Non-Muslim populations and rival political entities are the clear targets and victims, bearing the costs of war, subjugation, and loss of autonomy. The ulama jurists act as agenda-setters by providing the authoritative legal and theological framework for this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_offensive_jihad,
    'Is offensive jihad, as defined by this reading, a primary and perpetual obligation in Islam, or a historically contingent interpretation that has been superseded or reinterpreted?',
    'Comprehensive re-evaluation of classical and contemporary Islamic jurisprudence, textual analysis of the Quran and Sunnah, and examination of historical practice in light of modern international law and ethical frameworks.',
    'If reclassified as historically contingent or superseded, the constraint''s legitimacy would collapse, reducing its extractiveness and suppression to near zero. If reaffirmed, its persistence would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_offensive_jihad, conceptual, 'Contestation over the theological and legal status of offensive jihad.').

omega_variable(
    imam_authority_scope,
    'How absolute is the imam''s (or state''s) authority to declare offensive jihad, and what are the internal jurisprudential checks and balances on this authority?',
    'Detailed analysis of classical Islamic political theory and legal opinions regarding the conditions for a legitimate declaration of jihad, including consultation, capacity, and the welfare of the Muslim community.',
    'If internal checks are found to be robust and frequently invoked, the ''agenda_setter'' power might be more constrained than currently assessed, potentially reducing the effective suppression. If authority is found to be largely unchecked, the current high suppression is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imam_authority_scope, empirical, 'The extent of the imam''s authority in declaring offensive jihad.').

omega_variable(
    proportionality_in_practice,
    'How are the jurisprudential conditions of proportionality and non-combatant immunity applied in practice during offensive campaigns sanctioned by this reading?',
    'Historical case studies and contemporary analysis of military campaigns conducted under this interpretation, examining civilian casualties, treatment of prisoners, and destruction of property.',
    'If practice consistently deviates from stated proportionality, the ''coordination story'' of rule-bound warfare is further exposed as cover, increasing the effective extractiveness and suppression. If adherence is demonstrated, it might slightly temper the perceived severity of extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_in_practice, empirical, 'Gap between stated rules of war and actual conduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 622, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t622, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 622, 0.1).
narrative_ontology:measurement(jiha_tr_t750, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 750, 0.08).
narrative_ontology:measurement(jiha_tr_t1200, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(jiha_tr_t1600, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1600, 0.12).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t622, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 622, 0.6).
narrative_ontology:measurement(jiha_be_t750, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 750, 0.8).
narrative_ontology:measurement(jiha_be_t1200, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1200, 0.85).
narrative_ontology:measurement(jiha_be_t1600, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1600, 0.75).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t622, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 622, 0.7).
narrative_ontology:measurement(jiha_su_t750, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 750, 0.85).
narrative_ontology:measurement(jiha_su_t1200, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1200, 0.9).
narrative_ontology:measurement(jiha_su_t1600, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1600, 0.8).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, dhimmi_status_legal_framework).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, islamic_law_governance_system).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'jihad_quranic_corpus' kernel. Its ε value and structural properties differ significantly from the 'defensive_spiritual_reading' and 'revolutionary_vanguard_reading', necessitating separate constraint stories. This reading's emphasis on state authority and offensive campaigns directly forecloses the core premises of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
