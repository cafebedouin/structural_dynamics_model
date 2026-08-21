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
 *   human_readable: Maliki Reading of Islamic Legal Methodology
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the Maliki reading of the usul al-fiqh
 *   (principles of Islamic jurisprudence) kernel. The Maliki school is
 *   distinguished by its emphasis on 'amal ahl al-Madina (the practice of the
 *   people of Medina) as an independent source of law, the acceptance of
 *   maslaha mursala (unrestricted public interest) as a valid legal proof,
 *   and the integration of 'urf (local custom) where it does not contradict
 *   textual sources. This approach provides flexibility and contextual
 *   relevance but also creates tension with more textualist methodologies.
 *   The constraint is claimed as a Tangled Rope because it genuinely
 *   coordinates legal practice while extracting from (or overriding) purely
 *   textualist interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, 0.6).
domain_priors:suppression_score(usul_al_fiqh_method__maliki_reading, 0.4).
domain_priors:theater_ratio(usul_al_fiqh_method__maliki_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__maliki_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__maliki_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__maliki_reading, "Maliki Reading of Islamic Legal Methodology").
narrative_ontology:topic_domain(usul_al_fiqh_method__maliki_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__maliki_reading, '13852f84-0f7a-4a89-a645-6e51309e25ba').
narrative_ontology:cs_kernel_codification('13852f84-0f7a-4a89-a645-6e51309e25ba', formalized).
narrative_ontology:cs_authority_grounding('13852f84-0f7a-4a89-a645-6e51309e25ba', lineage).
narrative_ontology:cs_interpretation_layer_present('13852f84-0f7a-4a89-a645-6e51309e25ba').
narrative_ontology:cs_reading_relation('13852f84-0f7a-4a89-a645-6e51309e25ba', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('13852f84-0f7a-4a89-a645-6e51309e25ba', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('13852f84-0f7a-4a89-a645-6e51309e25ba', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('13852f84-0f7a-4a89-a645-6e51309e25ba', foundational, amal_ahl_al_madina_as_independent_source).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_as_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('13852f84-0f7a-4a89-a645-6e51309e25ba', amal_ahl_al_madina_as_independent_source, conventional).
narrative_ontology:cs_axiom('13852f84-0f7a-4a89-a645-6e51309e25ba', foundational, maslaha_mursala_as_valid_source).
narrative_ontology:cs_axiom_status(maslaha_mursala_as_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('13852f84-0f7a-4a89-a645-6e51309e25ba', maslaha_mursala_as_valid_source, instrumental).
narrative_ontology:cs_reference_frame('13852f84-0f7a-4a89-a645-6e51309e25ba', early_medinan_practice_framework).
narrative_ontology:cs_drift_state('13852f84-0f7a-4a89-a645-6e51309e25ba', contemporary_islamic_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('13852f84-0f7a-4a89-a645-6e51309e25ba', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, local_communities_medina).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, regional_customary_norms).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, universalist_textualists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__maliki_reading, strict_hadith_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__maliki_reading, muslim_laity).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, public_interest_as_legal_source).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__maliki_reading, regional_legal_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The custodians and interpreters of the Maliki school, they apply its principles to derive legal rulings. They benefit from the authority granted to their methodology, which allows for flexibility in applying Islamic law to diverse contexts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, maliki_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Historically, the community whose established practices ('amal ahl al-Madina) are given significant evidentiary weight, providing stability and continuity for their local legal traditions within the broader Islamic framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, local_communities_medina, beneficiary,
    organized, biographical, constrained, local).

% The body of local customs ('urf) that are integrated into Maliki jurisprudence, providing a flexible and context-sensitive application of law. These norms benefit from being recognized as valid legal sources.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, regional_customary_norms, beneficiary,
    moderate, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(usul_al_fiqh_method__maliki_reading, regional_customary_norms).

% Scholars and movements who advocate for a strict, universal application of law derived solely from the Quran and authenticated Hadith. They bear the cost of their preferred sources being diluted or overridden by non-textual considerations like Medinan practice or public interest.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, universalist_textualists, payer,
    powerful, generational, constrained, global).

% Scholars who prioritize the authenticity and literal interpretation of Hadith, often viewing other sources with skepticism. They experience the Maliki methodology as a constraint on the absolute authority of Hadith, leading to a perceived loss of interpretive control.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, strict_hadith_scholars, payer,
    powerful, generational, constrained, global).

% The general Muslim population who benefit from a legal system that can adapt to their local customs and address their practical needs through principles like maslaha mursala, potentially leading to more relevant and less burdensome rulings.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, muslim_laity, beneficiary,
    powerless, biographical, constrained, global).

% Academics who study the different methodologies of Islamic law, analyzing their historical development, internal coherence, and practical implications without being bound by adherence to a particular school.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__maliki_reading, comparative_legal_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__maliki_reading, maliki_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a coherent and adaptable framework for deriving Islamic legal rulings that balances textual authority with the practical needs of communities, local customs, and the broader public interest, especially in cases where textual sources are silent or ambiguous.
% TRANSFER_FUNCTION: Transfers interpretive authority from a purely textualist approach to one that incorporates juristic discretion, historical community practice (Medina), and considerations of public welfare (maslaha mursala) and local custom ('urf). This effectively shifts the burden of proof and the scope of acceptable legal sources.
% ABSENT_VOICES: Strict textualist scholars and movements who reject the validity of non-textual sources like maslaha mursala or 'amal ahl al-Madina as independent legal proofs. They would argue for a more restrictive methodology, but their views are structurally marginalized within the Maliki framework.
% DISAPPEARANCE_RATIONALE: The Maliki school is one of the four major Sunni schools of law, deeply embedded in the legal systems and cultural practices of vast regions, particularly North Africa and parts of the Middle East. Its disappearance would create a massive legal and social vacuum, forcing a fundamental reorganization of jurisprudence, judicial practice, and community norms in these areas.
% FOUNDING_PROBLEM: The challenge of applying Islamic law in diverse and evolving contexts, particularly after the Prophet's era, where reliance solely on explicit texts proved insufficient for addressing novel issues and accommodating established, beneficial community practices.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law, comparative legal scholars, and contemporary legal practitioners (including those from other schools) acknowledge the ongoing relevance of balancing textual authority with practical considerations and local custom, confirming that the problem the Maliki school sought to address remains pertinent in modern Islamic legal discourse.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(usul_al_fiqh_method__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__maliki_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost borne by those who prioritize strict textualism, as their preferred sources are sometimes subordinated or balanced against other considerations. Suppression (0.4) is moderate; while other schools exist, within the Maliki tradition, these principles are upheld. Resistance (0.5) is significant, stemming from ongoing intellectual debates with other schools that challenge the evidentiary weight of non-textual sources. Theater ratio is low (0.1) as the methodology is a serious, functional framework for legal derivation. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting the hardening of positions and the institutionalization of the Maliki methodology.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Maliki scholars, this methodology is a robust and necessary framework for applying Islamic law justly and practically. From the perspective of strict textualists, it represents a deviation from foundational principles and an unwarranted expansion of legal sources, leading to a perceived loss of certainty and authority in the law. The engine's classification as Tangled Rope captures this inherent tension between coordination and asymmetric cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Maliki scholars, local communities in Medina, and regional customary norms are beneficiaries, as their authority and relevance are elevated by this methodology. Universalist textualists and strict Hadith scholars are victims, as their preferred sources of law are constrained or overridden by the Maliki approach. The Muslim laity are diffuse beneficiaries, gaining from a more adaptable and context-sensitive legal system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maslaha_mursala_abuse_potential,
    'To what extent can the principle of maslaha mursala (unrestricted public interest) be abused to justify rulings that serve specific interests rather than genuine public welfare?',
    'Analysis of historical and contemporary Maliki fatwas (legal opinions) and judicial rulings where maslaha mursala was invoked, assessing consistency with established ethical principles and absence of clear private benefit.',
    'If widespread abuse is demonstrated, the effective extractiveness of the Maliki reading would be higher, pushing it closer to a Snare, as the coordination story of public interest would serve as cover for private extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_mursala_abuse_potential, empirical, 'Potential for maslaha mursala to be used for private gain.').

omega_variable(
    amal_ahl_al_madina_weight_ambiguity,
    'What is the precise evidentiary weight of ''amal ahl al-Madina (practice of Medina) when it appears to contradict a strong, authentic Hadith?',
    'Detailed textual analysis of Maliki juristic debates and the hierarchy of proofs established within the school, particularly in cases of apparent conflict, and comparison with other schools'' approaches to such conflicts.',
    'If ''amal ahl al-Madina consistently overrides strong Hadith, it reinforces the Maliki reading''s distinctiveness and the cost to textualists. If Hadith often prevails, the extractiveness from textualists would be lower, making the constraint closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amal_ahl_al_madina_weight_ambiguity, conceptual, 'Relative authority of Medinan practice versus Hadith.').

omega_variable(
    urf_contradiction_boundary,
    'How is the boundary defined and enforced for when ''urf (custom) ''contradicts text'' and is therefore invalid as a legal source?',
    'Examination of Maliki legal theory on the conditions for ''urf''s validity and case studies of rulings where custom was rejected due to textual contradiction, identifying the interpretive criteria used.',
    'A broad and flexible interpretation of ''contradiction'' would increase the effective extractiveness from customary norms, making the constraint more suppressive of local variation. A narrow interpretation would reduce this extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(urf_contradiction_boundary, conceptual, 'Interpretive boundary for custom contradicting text.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__maliki_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t350, usul_al_fiqh_method__maliki_reading, theater_ratio, 350, 0.1).
narrative_ontology:measurement(usul_tr_t700, usul_al_fiqh_method__maliki_reading, theater_ratio, 700, 0.1).
narrative_ontology:measurement(usul_tr_t1050, usul_al_fiqh_method__maliki_reading, theater_ratio, 1050, 0.1).
narrative_ontology:measurement(usul_tr_t1400, usul_al_fiqh_method__maliki_reading, theater_ratio, 1400, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__maliki_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(usul_be_t350, usul_al_fiqh_method__maliki_reading, base_extractiveness, 350, 0.55).
narrative_ontology:measurement(usul_be_t700, usul_al_fiqh_method__maliki_reading, base_extractiveness, 700, 0.58).
narrative_ontology:measurement(usul_be_t1050, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1050, 0.59).
narrative_ontology:measurement(usul_be_t1400, usul_al_fiqh_method__maliki_reading, base_extractiveness, 1400, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__maliki_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(usul_su_t350, usul_al_fiqh_method__maliki_reading, suppression_requirement, 350, 0.35).
narrative_ontology:measurement(usul_su_t700, usul_al_fiqh_method__maliki_reading, suppression_requirement, 700, 0.38).
narrative_ontology:measurement(usul_su_t1050, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1050, 0.39).
narrative_ontology:measurement(usul_su_t1400, usul_al_fiqh_method__maliki_reading, suppression_requirement, 1400, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__maliki_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__maliki_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major Sunni school of Islamic law. Each reading has a unique set of foundational axioms and structural relationships to other sources of law, leading to different extractiveness profiles and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
