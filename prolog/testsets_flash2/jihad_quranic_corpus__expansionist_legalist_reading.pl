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
 *   human_readable: Jihad (Expansionist-Legalist Reading): Obligation to Establish Islamic Governance
 *   domain: islamic_jurisprudence/political_theology/comparative_religious_law
 *
 * SUMMARY:
 *   This constraint represents the 'expansionist-legalist' reading of jihad
 *   within the Quranic corpus, which views it as an obligation to establish
 *   Islamic governance where it is absent, under specific jurisprudential
 *   conditions (invitation to Islam, Imam authority, proportionality) but
 *   permitting offensive campaigns. This reading is distinct from purely
 *   defensive or spiritual interpretations and from revolutionary vanguardist
 *   approaches. It legitimizes systematic expansion and conquest within a
 *   rule-bound legal framework, creating a liminal status for non-Muslims
 *   (potential dhimmi or combatant) and centralizing the authority to declare
 *   jihad in the state/caliph.
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
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Jihad (Expansionist-Legalist Reading): Obligation to Establish Islamic Governance").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "islamic_jurisprudence/political_theology/comparative_religious_law").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, '7c8f2530-2273-459d-afaf-88626657d73e').
narrative_ontology:cs_kernel_codification('7c8f2530-2273-459d-afaf-88626657d73e', fixed_text).
narrative_ontology:cs_authority_grounding('7c8f2530-2273-459d-afaf-88626657d73e', lineage).
narrative_ontology:cs_interpretation_layer_present('7c8f2530-2273-459d-afaf-88626657d73e').
narrative_ontology:cs_reading_relation('7c8f2530-2273-459d-afaf-88626657d73e', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c8f2530-2273-459d-afaf-88626657d73e', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('7c8f2530-2273-459d-afaf-88626657d73e', foundational, establishment_of_islamic_governance_is_obligatory).
narrative_ontology:cs_axiom_status(establishment_of_islamic_governance_is_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('7c8f2530-2273-459d-afaf-88626657d73e', establishment_of_islamic_governance_is_obligatory, deontological).
narrative_ontology:cs_axiom('7c8f2530-2273-459d-afaf-88626657d73e', foundational, imam_has_sole_authority_to_declare_offensive_jihad).
narrative_ontology:cs_axiom_status(imam_has_sole_authority_to_declare_offensive_jihad, holdable).
narrative_ontology:cs_axiom_grounding('7c8f2530-2273-459d-afaf-88626657d73e', imam_has_sole_authority_to_declare_offensive_jihad, conventional).
narrative_ontology:cs_reference_frame('7c8f2530-2273-459d-afaf-88626657d73e', classical_islamic_legal_tradition).
narrative_ontology:cs_drift_state('7c8f2530-2273-459d-afaf-88626657d73e', contemporary_nation_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c8f2530-2273-459d-afaf-88626657d73e', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_authorities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, ulama_legal_scholars).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, dissenting_muslims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities (e.g., a Caliph or Imam) hold the sole legitimate power to declare and lead offensive jihad campaigns, framing them as necessary for establishing justice and Islamic governance. They benefit from the expansion of their territorial and political influence, and the collection of resources from newly governed lands.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, islamic_state_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Scholars who interpret and codify this reading of jihad gain authority and influence by providing the jurisprudential framework that legitimizes state actions. Their careers and social standing are often tied to the perpetuation and application of this legal tradition.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, ulama_legal_scholars, beneficiary,
    organized, generational, identity_locked, global).

% Individuals who participate in these campaigns bear the direct costs of warfare, including risk to life and limb. They are motivated by religious obligation and the promise of spiritual reward, but their agency is constrained by the authority of the imam and the legal framework.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_soldiers, payer,
    moderate, biographical, constrained, local).

% Populations in territories targeted by offensive campaigns face subjugation, conversion, or dhimmi status (protected but subordinate). They bear the costs of conquest, loss of autonomy, and potential religious discrimination. Their options are resistance, flight, or acceptance of the new order.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, generational, trapped, local).

% Muslims who adhere to purely defensive or spiritual interpretations of jihad are marginalized or suppressed by the dominant legalist framework. They may face accusations of apostasy or disloyalty if they openly challenge the state's declaration of offensive jihad.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, dissenting_muslims, excluded,
    powerless, biographical, identity_locked, regional).

% These bodies observe and critique the application of this doctrine, particularly concerning non-combatant immunity, proportionality, and the rights of conquered populations. Their influence is external and often limited by state sovereignty claims.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective action of the Muslim community under a unified command (the Imam/Caliph) to expand the domain of Islamic governance, ensuring a structured and rule-bound approach to warfare and territorial expansion.
% TRANSFER_FUNCTION: Transfers political authority, territorial control, and resources (e.g., taxes, spoils of war) from non-Islamic polities and populations to the Islamic state and its leadership, in exchange for the promise of justice and adherence to Islamic law.
% ABSENT_VOICES: Muslims adhering to purely defensive or spiritual interpretations of jihad, as well as non-Muslim populations who would advocate for self-determination and religious freedom, are structurally excluded from the decision-making process regarding offensive campaigns. Their perspectives are either suppressed or deemed irrelevant by the dominant legal framework.
% DISAPPEARANCE_RATIONALE: If this legalist reading of jihad vanished, the jurisprudential basis for state-led offensive expansion would collapse. Islamic states would lose a key ideological tool for legitimizing territorial claims and resource acquisition, leading to a significant reorientation of foreign policy, military doctrine, and internal governance structures. The geopolitical landscape of regions historically shaped by this doctrine would fundamentally shift.
% FOUNDING_PROBLEM: The early Muslim community faced the challenge of establishing and expanding a new religious and political order in a hostile environment, requiring a framework for both defense and the propagation of Islam's message and governance.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including many contemporary Islamic scholars and political movements, argue that the problem of establishing comprehensive Islamic governance and justice where it is absent remains live. Critics, including many secular scholars and some Muslim reformists, argue that the original context has changed, and the doctrine is now primarily used to justify expansionist political agendas rather than addressing a genuine founding problem. Corroboration for the 'live' status comes from within the benefiting parties; external corroboration is contested.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.85) is high due to the systematic transfer of sovereignty and resources from non-Muslim polities to the Islamic state, and the imposition of a subordinate status on non-Muslim populations. Suppression (0.90) is also high, reflecting the coercive force required to establish and maintain this order, including military campaigns and the suppression of internal dissent against the doctrine. Theater ratio (0.10) is low, as the stated purpose of establishing Islamic governance is genuinely pursued, even if the means are highly extractive. Accessibility collapse (0.75) is substantial for non-Muslim populations, as their existing political and social structures are largely dismantled or subordinated. Resistance (0.70) is also high, reflecting the historical and ongoing opposition from targeted populations and dissenting Muslim groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Islamic state authorities and supporting ulama, this reading of jihad is a legitimate and necessary mechanism for fulfilling a divine mandate and establishing justice. From the perspective of non-Muslim populations and dissenting Muslims, it is a highly extractive and suppressive mechanism for territorial expansion and political domination. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Islamic state authorities and ulama scholars are clear beneficiaries, gaining political power, resources, and jurisprudential authority. Non-Muslim populations and dissenting Muslims are clear victims, bearing the costs of conquest, subjugation, and suppression. Muslim soldiers are payers of direct costs (life, injury) but also beneficiaries of spiritual reward and participation in a religiously sanctioned endeavor. International human rights bodies are observers, analyzing the constraint's impact without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_imam_authority,
    'Is the authority of a single Imam/Caliph to declare offensive jihad universally accepted within Islamic jurisprudence, or is it contested by other schools of thought or historical precedents?',
    'Comparative jurisprudential analysis across major Sunni and Shia schools, examining historical fatwas and scholarly consensus (ijma) on the conditions for offensive jihad.',
    'If the Imam''s sole authority is widely contested, the constraint''s legitimacy and enforceability would be significantly weakened, potentially reclassifying it as a snare or piton due to lack of broad acceptance. If universally accepted, its rope-like coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_imam_authority, conceptual, 'The extent of jurisprudential consensus on the authority to declare offensive jihad.').

omega_variable(
    proportionality_in_practice,
    'To what extent has the jurisprudential condition of proportionality (e.g., non-combatant immunity, avoidance of excessive harm) been consistently applied in historical and contemporary offensive jihad campaigns?',
    'Empirical historical and contemporary case studies of military campaigns conducted under this doctrine, assessing adherence to proportionality principles as defined within Islamic law.',
    'If proportionality is frequently violated in practice, the ''legalist'' aspect of the reading becomes theatrical, increasing the theater_ratio and extractiveness, potentially shifting classification towards a snare. If consistently applied, it reinforces the claim of a rule-bound coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_in_practice, empirical, 'Gap between theoretical proportionality and practical application in offensive jihad.').

omega_variable(
    invitation_to_islam_efficacy,
    'Is the ''invitation to Islam'' (da''wah) prior to offensive campaigns a genuine opportunity for peaceful conversion/submission, or primarily a legal formality to legitimize military action?',
    'Analysis of historical records and contemporary accounts of da''wah preceding military engagements, assessing the sincerity of the offer and the practical options available to the invited populations.',
    'If primarily a formality, the suppression metric would be higher, as the ''choice'' offered is illusory, and the constraint''s coordination function would be further exposed as a cover for extraction. If genuine, it would slightly reduce the perceived suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(invitation_to_islam_efficacy, empirical, 'Sincerity and practical efficacy of the pre-campaign invitation to Islam.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jiha_tr_t350, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 350, 0.15).
narrative_ontology:measurement(jiha_tr_t700, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement(jiha_tr_t1050, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1050, 0.08).
narrative_ontology:measurement(jiha_tr_t1400, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(jiha_be_t350, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 350, 0.8).
narrative_ontology:measurement(jiha_be_t700, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 700, 0.85).
narrative_ontology:measurement(jiha_be_t1050, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1050, 0.88).
narrative_ontology:measurement(jiha_be_t1400, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1400, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(jiha_su_t350, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 350, 0.85).
narrative_ontology:measurement(jiha_su_t700, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 700, 0.9).
narrative_ontology:measurement(jiha_su_t1050, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1050, 0.92).
narrative_ontology:measurement(jiha_su_t1400, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1400, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
