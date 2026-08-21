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
 *   human_readable: Jihad (Expansionist-Legalist Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents the expansionist-legalist reading of Jihad,
 *   which views it as a collective obligation to establish Islamic governance
 *   where it is absent, under specific jurisprudential conditions (invitation
 *   to Islam, Imam's authority, proportionality). This reading permits
 *   offensive military campaigns. It is one of several competing
 *   interpretations of the Quranic corpus on Jihad. The claimed type is
 *   'tangled_rope' because it genuinely coordinates the Umma for a collective
 *   goal (expansion of Islamic governance) but does so through asymmetric
 *   extraction from non-Muslim polities and populations, requiring active
 *   enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.7).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.8).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad (Expansionist-Legalist Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '1b2a0c33-f8db-450b-8fc3-63cce91b68d8').
narrative_ontology:cs_kernel_codification('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', fixed_text).
narrative_ontology:cs_authority_grounding('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', lineage).
narrative_ontology:cs_interpretation_layer_present('1b2a0c33-f8db-450b-8fc3-63cce91b68d8').
narrative_ontology:cs_reading_relation('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', foundational, jihad_as_collective_obligation_for_governance).
narrative_ontology:cs_axiom_status(jihad_as_collective_obligation_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', jihad_as_collective_obligation_for_governance, deontological).
narrative_ontology:cs_axiom('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', foundational, imam_monopoly_on_declaration_of_offensive_jihad).
narrative_ontology:cs_axiom_status(imam_monopoly_on_declaration_of_offensive_jihad, holdable).
narrative_ontology:cs_axiom_grounding('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', imam_monopoly_on_declaration_of_offensive_jihad, conventional).
narrative_ontology:cs_reference_frame('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', classical_islamic_legal_tradition_of_expansion).
narrative_ontology:cs_drift_state('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', contemporary_international_law_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1b2a0c33-f8db-450b-8fc3-63cce91b68d8', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_authorities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities (e.g., a Caliph or Imam) hold the sole legitimate power to declare and lead offensive jihad, ensuring it adheres to jurisprudential conditions like invitation to Islam and proportionality. They benefit from the expansion of their governance and the resources it brings.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% The broader Muslim community benefits from the expansion of Islamic governance, seen as fulfilling a collective religious obligation and establishing justice. Their identity is often tied to the success and spread of Islam, making exit from this framework difficult.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_umma, beneficiary,
    organized, generational, identity_locked, global).

% These are the target states or political entities that do not govern by Islamic law. They face the threat of military campaigns aimed at establishing Islamic governance, with options limited to conversion, submission (dhimmi status), or resistance leading to conflict.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_polities, payer,
    powerful, biographical, trapped, regional).

% Populations living under non-Islamic rule who become targets of offensive campaigns. They face the choice of conversion, accepting dhimmi status (with associated taxes and restrictions), or displacement/conflict. Their agency is severely constrained by the military and political power of the expanding Islamic state.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, biographical, trapped, local).

% Scholars who emphasize jihad as primarily defensive or spiritual struggle. While their views exist, the expansionist-legalist framework often marginalizes their interpretations in favor of state-sanctioned military action, limiting their influence on policy.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_scholars, excluded,
    moderate, generational, constrained, global).

% Groups advocating for individual, immediate, and often violent jihad against perceived apostate rulers or occupiers, bypassing state authority. This reading explicitly rejects their methods as illegitimate due to lack of proper jurisprudential authority, leading to their suppression by the state.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_groups, excluded,
    organized, immediate, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective effort of the Muslim community (Umma) under a legitimate Imam/Caliph to expand the domain of Islamic governance, ensuring military actions adhere to specific jurisprudential conditions (invitation, proportionality, non-combatant rules).
% TRANSFER_FUNCTION: Transfers political authority, territory, and resources from non-Muslim polities to the expanding Islamic state, and imposes dhimmi taxes or conversion on non-Muslim populations.
% ABSENT_VOICES: Scholars and communities advocating for purely defensive or spiritual interpretations of jihad are often marginalized or suppressed, as are revolutionary groups whose methods are deemed illegitimate by the state. They would argue for different conditions or prohibitions on offensive warfare.
% DISAPPEARANCE_RATIONALE: If this legal-theological framework for offensive jihad vanished, the justification for state-led expansionist military campaigns would collapse. Islamic states would lose a key ideological tool for territorial expansion, and non-Muslim polities would face less existential threat from this specific religious obligation, leading to a significant geopolitical rearrangement.
% FOUNDING_PROBLEM: The problem of establishing and expanding the domain of Islamic justice and governance (Dar al-Islam) in areas where it is absent, and ensuring the supremacy of God's law.
% FOUNDING_PROBLEM_CORROBORATION: Islamic state authorities and many traditional scholars attest that the problem of establishing Islamic governance remains live, citing the continued existence of non-Islamic rule. Critics (including some within the Muslim community and international observers) argue that the 'problem' is a pretext for expansion and resource acquisition, not a genuine unmet need for justice, but this counter-narrative is not universally accepted within the framework's adherents.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) due to the systematic transfer of sovereignty, territory, and resources from non-Muslim entities to the Islamic state, and the imposition of dhimmi status or conversion on populations. Suppression is also high (0.8) as the constraint relies on military force and political subjugation to achieve its aims, actively suppressing resistance and alternative governance structures. Theater ratio is low (0.1) because the stated goal of establishing Islamic governance is genuinely pursued through these means; there is little performative maintenance masking an atrophied function. Accessibility collapse is moderate (0.6) because while resistance is possible, the ultimate options for target populations are severely limited. Resistance is high (0.75) reflecting the active opposition from targeted polities and populations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Islamic state authorities and the Umma, this is a legitimate and necessary coordination mechanism for establishing divine law and justice. From the perspective of non-Muslim polities and populations, it is a highly extractive and suppressive mechanism of conquest. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic state authorities are the primary agenda-setters and beneficiaries, directing the campaigns and accruing the gains of expansion. The Muslim Umma is a beneficiary, fulfilling a collective religious duty and benefiting from the perceived expansion of justice. Non-Muslim polities and populations are the clear victims and targets, bearing the costs of conquest, subjugation, or conversion. Scholars of alternative readings and revolutionary groups are excluded, their interpretations or methods deemed illegitimate by the state-sanctioned framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_imam_authority,
    'Is the authority of the Imam/Caliph to declare offensive jihad universally accepted within Islamic jurisprudence, or is it contested by significant schools of thought?',
    'Comprehensive survey of classical and contemporary jurisprudential opinions, identifying the proportion of scholars who restrict offensive jihad to specific historical contexts or prohibit it entirely.',
    'If widely contested, the ''imam authority'' condition for offensive jihad would weaken, potentially reclassifying the constraint towards a snare (if the coordination function is undermined) or a piton (if the practice persists without broad jurisprudential backing).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_imam_authority, conceptual, 'Ambiguity regarding the universal legitimacy of state authority in declaring offensive jihad.').

omega_variable(
    proportionality_in_practice,
    'How consistently are the jurisprudential conditions of proportionality and non-combatant immunity applied in historical and contemporary offensive jihad campaigns?',
    'Empirical analysis of historical military campaigns and contemporary conflicts conducted under this framework, assessing adherence to proportionality and non-combatant rules through casualty data, targeting patterns, and post-conflict treatment of populations.',
    'If proportionality is frequently violated in practice, the constraint''s claimed coordination function (rule-bound expansion) would be undermined, increasing its effective extractiveness and potentially reclassifying it closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_in_practice, empirical, 'Gap between the jurisprudential ideal of proportionality and its practical application in offensive campaigns.').

omega_variable(
    natural_law_vs_constructed_obligation,
    'Is the obligation to establish Islamic governance a natural law inherent in divine will, or a constructed legal interpretation that benefits identifiable agents (Islamic state authorities)?',
    'Comparative theological and philosophical analysis of different religious and secular legal traditions regarding the relationship between divine command, human governance, and territorial expansion. Examination of the historical evolution of this specific jurisprudential reading.',
    'If primarily a constructed interpretation, the constraint''s ''naturalness'' claim would be weakened, highlighting the agency of the beneficiaries in its perpetuation and potentially reclassifying it as a snare or tangled rope, rather than a mountain-like obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_obligation, conceptual, 'Is the obligation to establish Islamic governance a natural law or a constructed legal interpretation?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(jiha_su_t50, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 50, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jihad_quranic_corpus' kernel. It represents the expansionist-legalist interpretation, which permits offensive campaigns under specific jurisprudential conditions. It coexists with and influences other readings, such as the defensive-spiritual and revolutionary-vanguard interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
