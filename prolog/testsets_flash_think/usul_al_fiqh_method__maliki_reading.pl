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
 *   constraint_id: usul_al_fiqh_method__maliki_reading
 *   human_readable: Maliki School's Jurisprudential Method
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint instantiates the Maliki reading of the usul al-fiqh
 *   (principles of Islamic jurisprudence) kernel. It emphasizes the
 *   independent evidentiary weight of 'amal ahl al-Madina (practice of the
 *   people of Medina), the validity of maslaha mursala (public interest
 *   unrestricted by textual evidence), and the integration of 'urf (local
 *   custom) where it does not contradict foundational texts. This approach
 *   provides a framework for legal derivation that is often seen as more
 *   flexible and responsive to local contexts compared to other schools.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.45).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.4).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki School's Jurisprudential Method").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, 'e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7').
narrative_ontology:cs_kernel_codification('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', formalized).
narrative_ontology:cs_authority_grounding('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', lineage).
narrative_ontology:cs_interpretation_layer_present('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7').
narrative_ontology:cs_reading_relation('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', foundational, medinan_practice_as_independent_source).
narrative_ontology:cs_axiom_status(medinan_practice_as_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', medinan_practice_as_independent_source, conventional).
narrative_ontology:cs_axiom('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', foundational, maslaha_mursala_unrestricted_by_text_validity).
narrative_ontology:cs_axiom_status(maslaha_mursala_unrestricted_by_text_validity, holdable).
narrative_ontology:cs_axiom_grounding('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', maslaha_mursala_unrestricted_by_text_validity, instrumental).
narrative_ontology:cs_reference_frame('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', early_medinan_consensus).
narrative_ontology:cs_drift_state('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', contemporary_islamic_legal_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e5aaf5a5-40ac-4cfa-922d-b1e346b49ef7', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, medinan_community).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_customary_norms).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, strict_hadith_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, muslim_laity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, public_interest_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, regional_legal_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and interpreters of the Maliki methodology. They benefit from the authority granted to Medinan practice, maslaha mursala, and 'urf, which allows them flexibility in legal derivation and strengthens their regional influence. They actively teach and apply this methodology.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Historically, the community whose established practices ('amal ahl al-Madina) are elevated to a source of law, granting their customs and consensus significant legal weight. This provides stability and legitimacy to their local norms.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, medinan_community, beneficiary,
    organized, generational, identity_locked, local).

% The body of established customs ('urf) in various regions where Maliki law is applied. These norms gain legal recognition and protection under the Maliki methodology, preventing their easy override by purely textual interpretations.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_customary_norms, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method__maliki_reading, local_customary_norms).

% Scholars and movements who prioritize strict, universal application of Quran and Hadith, often viewing regional practices or unrestricted public interest as deviations. They bear the 'cost' of having their preferred sources sometimes overridden or supplemented by Maliki-specific methodologies.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualists, payer,
    powerful, generational, constrained, global).

% Scholars who emphasize the primacy and authenticity of Hadith, often preferring even weak Hadith over other sources like qiyas or maslaha. They find their methodology challenged by the Maliki elevation of Medinan practice and public interest, which can lead to different legal outcomes.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, strict_hadith_scholars, payer,
    organized, biographical, constrained, global).

% The general Muslim population, particularly in regions historically influenced by the Maliki school. They benefit from a legal system that is often perceived as more flexible and responsive to local needs and public welfare, fostering a sense of belonging and practical applicability.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, muslim_laity, beneficiary,
    powerless, biographical, identity_locked, global).

% Scholars of the Hanafi school, who prioritize qiyas (analogical reasoning) and istihsan (juristic preference) differently. While coexisting, their specific methodological claims are not directly integrated into the Maliki framework, making them 'excluded' from this particular constraint's internal operation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hanafi_scholars, excluded,
    institutional, generational, mobile, global).

% Scholars of the Shafi'i school, known for its emphasis on rigorous Hadith authentication and systematized usul al-fiqh. Their distinct hierarchy of sources means their specific claims are not directly incorporated into the Maliki method.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, shafii_scholars, excluded,
    institutional, generational, mobile, global).

% Scholars of the Hanbali school, which emphasizes strict adherence to textual sources and minimizes qiyas. Their methodology stands in contrast to the Maliki school's broader acceptance of non-textual sources.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, hanbali_scholars, excluded,
    institutional, generational, mobile, global).

% Academics who study the different schools of Islamic law, analyzing their methodologies, historical development, and impact. They observe the Maliki method's operation without being subject to its internal enforcement or directly benefiting/paying.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a comprehensive methodology for deriving Islamic law that integrates foundational texts with the established practices of the Medinan community, considerations of public interest (maslaha mursala), and local customs ('urf), thereby allowing for legal flexibility and responsiveness to diverse contexts.
% TRANSFER_FUNCTION: Transfers legal authority and interpretive flexibility from purely textual sources (Quran and Hadith) to include regional practices and public interest considerations, primarily from universalist textualists and strict Hadith scholars to Maliki jurists and the communities whose customs are recognized.
% ABSENT_VOICES: Scholars from other schools (Hanafi, Shafi'i, Hanbali) who would argue for different hierarchies of sources or more restrictive applications of non-textual evidence. Their methodologies are distinct and not directly accommodated within the Maliki framework, though they coexist as valid schools.
% DISAPPEARANCE_RATIONALE: If the Maliki methodology vanished overnight, the legal systems and social norms in vast regions (e.g., North Africa, West Africa) would face a profound crisis. Many existing legal rulings, judicial precedents, and customary practices would lose their jurisprudential basis, necessitating a complete re-evaluation of legal sources and a reorganization of legal education and practice.
% FOUNDING_PROBLEM: The challenge of applying Islamic law in a way that is responsive to the needs and established practices of the community, particularly in Medina, while still adhering to foundational texts, and providing a framework for legal reasoning in novel situations not explicitly covered by revelation.
% FOUNDING_PROBLEM_CORROBORATION: Maliki jurists and communities in regions where the school is dominant attest to its ongoing relevance in addressing contemporary legal challenges, such as environmental law, bioethics, and modern financial transactions, by leveraging its principles of maslaha and 'urf. Independent legal historians and comparative law scholars also acknowledge its historical and ongoing role in Islamic legal development and its adaptive capacity.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates legal practice within the Maliki school and provides a framework for local communities to integrate their norms (beneficiary function). However, it also extracts from those who advocate for a more universalist, text-centric approach by sometimes overriding their preferred sources (extraction function). Extractiveness is moderate (0.45) as it creates a distinct interpretive space, but does not completely negate other approaches. Suppression is moderate (0.40) as other schools exist and are respected, but the Maliki method actively defends its own unique source hierarchy. Theater ratio is low (0.10) as the methodology is a genuine, functional legal system, not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Maliki scholars and communities, this methodology is a robust and necessary coordination mechanism for applying Islamic law in a practical, just, and locally sensitive manner. From the perspective of universalist textualists, it represents a deviation from the purity of textual sources, potentially leading to legal outcomes based on human discretion rather than divine revelation, thus experiencing it as an extractive imposition on their preferred method.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki scholars are clear beneficiaries, as their methodology grants them significant interpretive authority and influence. The Medinan community and local customary norms also benefit from the elevation of their practices. Universalist textualists and strict Hadith scholars are targets, as their preferred methods are sometimes subordinated or challenged. The Muslim laity benefits from a legal system perceived as responsive. Other schools are excluded, as their distinct methodologies are not integrated into this specific framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medinan_practice_authenticity,
    'To what extent does ''amal ahl al-Madina truly represent a direct, uncorrupted transmission of the Prophet''s practice, versus a later regional development or consensus?',
    'Historical-critical analysis of early Islamic legal texts and chains of transmission, comparing Maliki claims with independent historical accounts.',
    'If ''amal ahl al-Madina is found to be a later regional development, its evidentiary weight might be reduced, potentially shifting the Maliki reading closer to other schools that prioritize Hadith over regional practice, increasing extractiveness for Maliki scholars.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medinan_practice_authenticity, empirical, 'The historical and jurisprudential basis of Medinan practice as a source of law.').

omega_variable(
    maslaha_mursala_scope_limits,
    'What are the precise boundaries and safeguards for applying maslaha mursala (unrestricted public interest) to prevent arbitrary legal rulings or deviations from core Islamic principles?',
    'Further jurisprudential development and scholarly consensus on the conditions and limits of maslaha mursala, potentially through comparative analysis with other schools'' approaches to public interest.',
    'A clearer definition of maslaha''s scope would reduce the perceived ''extraction'' by universalist textualists, as their concerns about arbitrary rulings would be addressed. Conversely, an overly restrictive definition could reduce the Maliki school''s flexibility, increasing internal resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_scope_limits, conceptual, 'The extent to which public interest can be a source of law without textual basis.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of universalist textualism structural (due to institutional dominance of the Maliki school in certain regions) or internalized (due to scholarly acceptance of the Maliki methodology''s validity)?',
    'Analysis of legal education curricula, judicial appointments, and public discourse in Maliki-dominant regions: if alternative methodologies are actively marginalized, suppression is structural; if they are taught and debated respectfully, it''s more internalized.',
    'If primarily structural, the effective suppression is higher, indicating a more coercive aspect to the constraint. If primarily internalized, the constraint is more robustly accepted within the scholarly community, reducing its perceived extractiveness for those who disagree.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative legal methodologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(usul_tr_t10, usul_al_fiqh_method__maliki_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__maliki_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(usul_tr_t30, usul_al_fiqh_method__maliki_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__maliki_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__maliki_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__maliki_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__maliki_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__maliki_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__maliki_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__maliki_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__maliki_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__maliki_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__maliki_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__maliki_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__maliki_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four major Sunni jurisprudential readings of the 'usul_al_fiqh_method' kernel. Each reading represents a distinct methodology for legal derivation, with different hierarchies of sources and interpretive principles. They form a constraint family, linked by their shared kernel and ongoing scholarly dialogue/competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
