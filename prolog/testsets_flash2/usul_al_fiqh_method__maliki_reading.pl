% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__maliki_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki Reading of Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and Urf
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint represents the Maliki school's methodology (usul
 *   al-fiqh), which uniquely elevates Medinan practice ('amal ahl al-Madina),
 *   unrestricted public interest (maslaha mursala), and local custom ('urf)
 *   as independent sources of law alongside the Quran and Hadith. This
 *   reading provides flexibility and contextual relevance, particularly in
 *   regions where Maliki jurisprudence is dominant. The claimed type is
 *   'rope' because it genuinely coordinates diverse legal sources for a
 *   functional legal system, with moderate extraction from purely textualist
 *   approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.45).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.3).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Reading of Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and Urf").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '6d3739b9-9a6f-401c-a961-e8ac84af400e').
narrative_ontology:cs_kernel_codification('6d3739b9-9a6f-401c-a961-e8ac84af400e', formalized).
narrative_ontology:cs_authority_grounding('6d3739b9-9a6f-401c-a961-e8ac84af400e', lineage).
narrative_ontology:cs_interpretation_layer_present('6d3739b9-9a6f-401c-a961-e8ac84af400e').
narrative_ontology:cs_reading_relation('6d3739b9-9a6f-401c-a961-e8ac84af400e', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d3739b9-9a6f-401c-a961-e8ac84af400e', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d3739b9-9a6f-401c-a961-e8ac84af400e', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('6d3739b9-9a6f-401c-a961-e8ac84af400e', foundational, medinan_practice_as_independent_source).
narrative_ontology:cs_axiom_status(medinan_practice_as_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('6d3739b9-9a6f-401c-a961-e8ac84af400e', medinan_practice_as_independent_source, conventional).
narrative_ontology:cs_axiom('6d3739b9-9a6f-401c-a961-e8ac84af400e', foundational, maslaha_mursala_validity).
narrative_ontology:cs_axiom_status(maslaha_mursala_validity, holdable).
narrative_ontology:cs_axiom_grounding('6d3739b9-9a6f-401c-a961-e8ac84af400e', maslaha_mursala_validity, instrumental).
narrative_ontology:cs_reference_frame('6d3739b9-9a6f-401c-a961-e8ac84af400e', early_medinan_consensus).
narrative_ontology:cs_drift_state('6d3739b9-9a6f-401c-a961-e8ac84af400e', contemporary_globalized_islam, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('6d3739b9-9a6f-401c-a961-e8ac84af400e', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_medinan_community).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_norms).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, muslim_laity_maliki_regions).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, public_interest_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, medinan_practice_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and promulgators of the Maliki school, who uphold the evidentiary weight of Medinan practice, public interest, and local custom. Their authority is grounded in this interpretive framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_scholars, agenda_setter,
    institutional, generational, identity_locked, regional).

% Their historical and ongoing practices in Medina are elevated to a source of law, giving their traditions significant weight and stability within the Maliki framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_medinan_community, beneficiary,
    organized, generational, constrained, local).

% Local customs ('urf) are integrated into legal reasoning, providing flexibility and relevance to diverse regional contexts, as long as they do not contradict explicit texts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_norms, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method__maliki_reading, regional_customary_norms).

% Scholars and movements that prioritize strict adherence to universal textual sources (Quran and Hadith) and view the elevation of regional practice or unrestricted public interest as a deviation or weakening of the textual tradition. They bear the cost of their preferred methodology being subordinated in this reading.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualists, payer,
    powerful, generational, identity_locked, global).

% Benefit from a legal system that often aligns with their established local customs and traditions, making religious law feel more accessible and less alienating than a purely textualist approach might be.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, muslim_laity_maliki_regions, beneficiary,
    powerless, biographical, constrained, regional).

% Analyze the Maliki methodology in comparison to other schools and secular legal systems, evaluating its coherence, adaptability, and impact on legal development.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for legal derivation within the Maliki school, integrating diverse sources like Medinan practice, public interest, and local custom to produce rulings relevant to specific communities.
% TRANSFER_FUNCTION: Transfers interpretive authority from purely textual sources to include regional practice and public interest considerations, from universalist textualists to Maliki scholars and local communities.
% ABSENT_VOICES: Strict Zahiri literalists or extreme textualists who reject any form of analogical reasoning, public interest, or customary law as sources would be absent; they would argue for an exclusive reliance on explicit texts.
% DISAPPEARANCE_RATIONALE: If the Maliki methodology vanished, the legal landscape in regions historically influenced by it would undergo significant upheaval. Local customs would lose their legal grounding, public interest considerations would be sidelined, and the interpretive authority of Medinan practice would cease, leading to a re-evaluation of countless legal rulings and a shift towards more textualist or analogical approaches.
% FOUNDING_PROBLEM: The need to reconcile universal Islamic texts with the specific practices and evolving needs of the early Muslim community in Medina, and later, diverse regional communities, ensuring the law remained relevant and just.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars attest the problem is live, citing ongoing needs for legal flexibility and contextual application. Comparative legal scholars and historians corroborate that the challenge of reconciling universal texts with local realities remains a persistent feature of Islamic legal development, validating the Maliki approach's continued relevance.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).
:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the interpretive 'cost' imposed on those who would prefer a more restrictive, textualist methodology, but it is not coercive. Suppression (0.30) is low, as alternative methodologies are not actively suppressed but rather operate in parallel or in different regions. Theater ratio (0.10) is low, indicating that the stated principles genuinely guide legal derivation rather than serving as mere cover. Accessibility collapse (0.60) is moderate, as it provides a clear, albeit distinct, path to legal understanding. Resistance (0.20) is low, as the Maliki school is a well-established and respected tradition.
 *
 * PERSPECTIVAL GAP:
 *   Maliki scholars perceive this methodology as a robust and necessary coordination mechanism for Islamic law, ensuring its relevance and justice across diverse contexts. Universalist textualists, however, may view it as an unwarranted departure from foundational texts, perceiving it as a form of extraction of interpretive authority from the primary sources. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki scholars, the local Medinan community, and regional customary norms are beneficiaries, as their interpretive authority and practices are elevated. Universalist textualists are victims, as their preferred, more restrictive methodology is subordinated. The Muslim laity in Maliki regions are also beneficiaries, as the law aligns with their customs. The analytical observer (comparative legal scholars) is symmetric.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_maslaha_mursala,
    'What are the practical limits and safeguards on the application of maslaha mursala (unrestricted public interest) to prevent arbitrary rulings?',
    'Analysis of historical Maliki fatwas and legal treatises that define the conditions and constraints for invoking maslaha mursala, and comparison with contemporary applications.',
    'If maslaha mursala is found to be applied with insufficient safeguards, the extractiveness from textualist approaches could be higher, as it would be perceived as an arbitrary override. If robust safeguards exist, it reinforces the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_maslaha_mursala, empirical, 'Ambiguity regarding the practical application and potential for abuse of public interest as a legal source.').

omega_variable(
    amal_ahl_al_madina_historical_continuity,
    'To what extent does ''amal ahl al-Madina (Medinan practice) represent an unbroken, authentic chain of transmission from the Prophet''s time, versus later scholarly constructions?',
    'Historical-critical scholarship examining the evidentiary basis and transmission chains of specific Medinan practices, distinguishing between early consensus and later scholarly interpretations.',
    'If the historical continuity is weaker than claimed, the ''naturalness'' of this source diminishes, potentially increasing the perceived extraction from those who prioritize textual evidence. If strong, it reinforces the unique evidentiary weight of this source.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amal_ahl_al_madina_historical_continuity, empirical, 'The historical authenticity and unbroken chain of transmission for Medinan practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(usul_tr_t350, usul_al_fiqh_method__maliki_reading, theater_ratio, 350, 0.07).
narrative_ontology:measurement(usul_tr_t700, usul_al_fiqh_method__maliki_reading, theater_ratio, 700, 0.08).
narrative_ontology:measurement(usul_tr_t1050, usul_al_fiqh_method__maliki_reading, theater_ratio, 1050, 0.09).
narrative_ontology:measurement(usul_tr_t1400, usul_al_fiqh_method__maliki_reading, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(usul_be_t350, usul_al_fiqh_method__maliki_reading, base_extractiveness, 350, 0.35).
narrative_ontology:measurement(usul_be_t700, usul_al_fiqh_method__maliki_reading, base_extractiveness, 700, 0.4).
narrative_ontology:measurement(usul_be_t1050, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1050, 0.43).
narrative_ontology:measurement(usul_be_t1400, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1400, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(usul_su_t350, usul_al_fiqh_method__maliki_reading, suppression_requirement, 350, 0.22).
narrative_ontology:measurement(usul_su_t700, usul_al_fiqh_method__maliki_reading, suppression_requirement, 700, 0.25).
narrative_ontology:measurement(usul_su_t1050, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1050, 0.28).
narrative_ontology:measurement(usul_su_t1400, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1400, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, islamic_finance_regulation).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, family_law_maliki_regions).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major school of Islamic jurisprudence. Each reading has a unique structural profile and set of beneficiaries/victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
