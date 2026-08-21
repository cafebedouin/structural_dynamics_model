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
 *   human_readable: Maliki Reading of Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and Urf as Sources
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Maliki school's methodology (usul al-fiqh)
 *   for deriving Islamic law, emphasizing the independent evidentiary weight
 *   of Medinan practice ('amal ahl al-Madina), the validity of unrestricted
 *   public interest (maslaha mursala), and the integration of local custom
 *   ('urf). This is one reading of the broader 'usul_al_fiqh_method' kernel,
 *   distinct from other Sunni schools. The Maliki approach elevates regional
 *   and practical considerations, which benefits local communities but is
 *   seen as a deviation by strict textualists.
 *
 * KEY AGENTS:
 *   - maliki_scholars: Agenda-setter (institutional/identity_locked) — uphold and interpret the Maliki methodology.
 *   - local_medinan_communities: Beneficiary (organized/constrained) — their practices are a source of law.
 *   - regional_customary_norms: Beneficiary (moderate/constrained) — local customs are integrated into legal rulings.
 *   - universalist_textualists: Payer (powerful/identity_locked) — bear the cost of perceived dilution of textual authority.
 *   - muslim_laity_maliki_regions: Beneficiary (powerless/constrained) — benefit from relevant and accessible jurisprudence.
 *   - comparative_legal_scholars: Observer (analytical/analytical) — analyze the methodology in a broader context.
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
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Reading of Usul al-Fiqh: Medinan Practice, Maslaha Mursala, and Urf as Sources").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '6c59a415-194a-42af-85bd-0a4f5c78d125').
narrative_ontology:cs_kernel_codification('6c59a415-194a-42af-85bd-0a4f5c78d125', formalized).
narrative_ontology:cs_authority_grounding('6c59a415-194a-42af-85bd-0a4f5c78d125', lineage).
narrative_ontology:cs_interpretation_layer_present('6c59a415-194a-42af-85bd-0a4f5c78d125').
narrative_ontology:cs_reading_relation('6c59a415-194a-42af-85bd-0a4f5c78d125', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c59a415-194a-42af-85bd-0a4f5c78d125', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c59a415-194a-42af-85bd-0a4f5c78d125', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('6c59a415-194a-42af-85bd-0a4f5c78d125', foundational, medinan_practice_as_independent_source).
narrative_ontology:cs_axiom_status(medinan_practice_as_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('6c59a415-194a-42af-85bd-0a4f5c78d125', medinan_practice_as_independent_source, conventional).
narrative_ontology:cs_axiom('6c59a415-194a-42af-85bd-0a4f5c78d125', foundational, maslaha_mursala_valid_source).
narrative_ontology:cs_axiom_status(maslaha_mursala_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('6c59a415-194a-42af-85bd-0a4f5c78d125', maslaha_mursala_valid_source, instrumental).
narrative_ontology:cs_reference_frame('6c59a415-194a-42af-85bd-0a4f5c78d125', early_medinan_consensus_and_practice).
narrative_ontology:cs_drift_state('6c59a415-194a-42af-85bd-0a4f5c78d125', contemporary_globalized_islamic_discourse, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('6c59a415-194a-42af-85bd-0a4f5c78d125', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_medinan_communities).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_norms).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, muslim_laity_maliki_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and promulgators of the Maliki school, they uphold the evidentiary weight of Medinan practice, public interest, and local custom. Their authority is grounded in this interpretive tradition.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_scholars, agenda_setter,
    institutional, generational, identity_locked, regional).

% Their historical and ongoing practices in Medina are recognized as a direct source of law, giving their customs and traditions significant legal weight within the Maliki framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_medinan_communities, beneficiary,
    organized, generational, constrained, local).

% Local customs ('urf) are integrated into legal rulings, provided they do not contradict explicit textual sources. This provides flexibility and relevance to diverse regional contexts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_norms, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method__maliki_reading, regional_customary_norms).

% Scholars and movements that prioritize strict adherence to universal textual sources (Quran and Hadith) above regional practice or unrestricted public interest. They view the Maliki approach as potentially diluting textual authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualists, payer,
    powerful, generational, identity_locked, global).

% Benefit from legal rulings that are often more aligned with their local customs and perceived public interest, leading to a sense of relevance and accessibility in jurisprudence.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, muslim_laity_maliki_regions, beneficiary,
    powerless, biographical, constrained, regional).

% Analyze the Maliki methodology in comparison to other schools of thought, evaluating its strengths and weaknesses in adapting Islamic law to diverse contexts and challenges.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for legal derivation within the Maliki school, integrating diverse sources like Medinan practice, public interest, and local custom to address legal questions and ensure relevance to specific communities.
% TRANSFER_FUNCTION: Transfers legal authority and interpretive flexibility to regional practices and scholarly discretion (maslaha mursala) from a purely textualist or analogical derivation, impacting the scope of acceptable legal reasoning.
% ABSENT_VOICES: Strict textualists who believe that any deviation from explicit Quranic or Hadith texts, even for public interest or custom, is an innovation that dilutes the purity of Islamic law. They are often marginalized in Maliki discourse.
% DISAPPEARANCE_RATIONALE: If this interpretive framework vanished, the Maliki school would lose its distinct identity. Legal rulings in Maliki-influenced regions would become inconsistent with local norms, leading to significant legal and social disruption as scholars would be forced to adopt other schools' methodologies, likely leading to a more textualist approach.
% FOUNDING_PROBLEM: To provide a robust legal methodology that balanced textual authority with the living tradition and practical needs of the early Muslim community, particularly in Medina, and to ensure the law remained relevant to evolving societal needs.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars attest that the problem of balancing textual fidelity with societal needs and local context remains live. Independent historians of Islamic law and comparative jurists corroborate that the Maliki school's distinct methodology continues to address these challenges effectively in its regions of influence, distinguishing it from other schools.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) is moderate, reflecting the 'cost' to universalist textualists who see their preferred methodology challenged, but also the coordination benefits for Maliki adherents. Suppression (0.3) is low, as this is an interpretive framework within a broader tradition, not a coercive enforcement mechanism; resistance is also low (0.2) as it's a well-established school. Accessibility collapse (0.6) is moderate, as it offers a distinct path but still operates within the broader Islamic legal tradition. Theater ratio (0.1) is low, indicating the framework is genuinely functional and not primarily performative. The temporal measurements show relative stability over a long historical period, reflecting the enduring nature of this school's methodology.
 *
 * PERSPECTIVAL GAP:
 *   Maliki scholars and local communities perceive this as a highly effective and legitimate method for deriving law, ensuring its relevance and justice. Universalist textualists, however, view it as a problematic expansion of sources that risks undermining the primacy of divine revelation. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki scholars, Medinan communities, and regional customs are beneficiaries, as their authority or practices are elevated. Universalist textualists are payers, as their preferred, more restrictive methodology is 'paid down' by the Maliki approach. The Muslim laity in Maliki regions are beneficiaries of a more contextually relevant law. Comparative legal scholars are observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The Maliki reading prevents mislabeling coordination as pure extraction by explicitly recognizing the genuine coordination function of integrating local practice and public interest into law, which serves the needs of specific communities. It avoids being a snare by not coercively suppressing alternative schools, but rather coexisting within a pluralistic legal tradition. The 'live' status of the founding problem and 'world_rearranges' verdict indicate it is not a piton; its function remains vital.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maslaha_mursala_scope,
    'What are the practical limits and potential for abuse of maslaha mursala (unrestricted public interest) as a source of law?',
    'Analysis of historical and contemporary Maliki fatwas (legal opinions) where maslaha mursala was invoked, assessing consistency with broader Islamic ethical principles and outcomes for justice.',
    'If maslaha mursala is found to be consistently applied within clear ethical bounds, it reinforces the rope-like coordination function. If it shows patterns of arbitrary application or serving narrow interests, it would increase the perceived extractiveness for those whose interests are overridden, pushing the classification towards tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_scope, empirical, 'The actual scope and application of public interest as a legal source.').

omega_variable(
    amal_ahl_al_madina_historical_accuracy,
    'To what extent does ''amal ahl al-Madina (Medinan practice) accurately reflect the consensus of the early Muslim community, versus being a specific regional tradition?',
    'Further historical and archaeological research into early Medinan society, alongside comparative analysis with early practices in other Islamic centers.',
    'If ''amal ahl al-Madina is shown to be a more localized tradition rather than a universal consensus, its evidentiary weight might be conceptually challenged by other schools, increasing the perceived extractiveness for universalist textualists. If its universal representativeness is strongly affirmed, it would strengthen the Maliki claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amal_ahl_al_madina_historical_accuracy, empirical, 'The historical basis and representativeness of Medinan practice.').

omega_variable(
    maliki_methodology_framing,
    'Is the Maliki methodology primarily a pragmatic adaptation to local context, or a principled theological stance on the nature of revelation and authority?',
    'Deep textual analysis of foundational Maliki works and contemporary scholarly debates on the philosophical underpinnings of the school''s usul al-fiqh.',
    'Framing it as pragmatic adaptation emphasizes its coordination function for diverse societies. Framing it as a principled theological stance highlights its distinct identity and potential for conceptual conflict with other schools, potentially increasing perceived extractiveness for those holding different theological principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maliki_methodology_framing, conceptual, 'The underlying philosophical and theological grounding of the Maliki methodology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t300, usul_al_fiqh_method__maliki_reading, theater_ratio, 300, 0.09).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__maliki_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__maliki_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__maliki_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(usul_be_t300, usul_al_fiqh_method__maliki_reading, base_extractiveness, 300, 0.42).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__maliki_reading, base_extractiveness, 600, 0.45).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__maliki_reading, base_extractiveness, 900, 0.44).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(usul_su_t300, usul_al_fiqh_method__maliki_reading, suppression_requirement, 300, 0.28).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__maliki_reading, suppression_requirement, 600, 0.3).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__maliki_reading, suppression_requirement, 900, 0.29).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1200, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major Sunni school of Islamic jurisprudence. Each reading has a unique set of source priorities and methodologies, leading to different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
