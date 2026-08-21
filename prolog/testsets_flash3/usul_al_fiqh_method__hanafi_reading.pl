% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Method of Islamic Jurisprudence (Qiyas, Ra'y, Istihsan)
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's methodology (usul al-fiqh)
 *   for deriving Islamic law, emphasizing expansive analogical reasoning
 *   (qiyas), reasoned opinion (ra'y), and juristic preference for public
 *   interest (istihsan). It is one reading of the broader kernel of Islamic
 *   legal methodology, distinguished by its rationalist bent and flexibility
 *   compared to other schools. The constraint is claimed as a Rope due to its
 *   genuine coordination function in legal derivation, but its metrics
 *   reflect a Tangled Rope due to the asymmetric benefits to the jurist class
 *   and the costs borne by textualist approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.65).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.4).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Method of Islamic Jurisprudence (Qiyas, Ra'y, Istihsan)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '5295b0fd-e622-4de8-a9f8-59ee1a6f0e30').
narrative_ontology:cs_kernel_codification('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', formalized).
narrative_ontology:cs_authority_grounding('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', lineage).
narrative_ontology:cs_interpretation_layer_present('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30').
narrative_ontology:cs_reading_relation('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', foundational, rational_inquiry_as_source).
narrative_ontology:cs_axiom_status(rational_inquiry_as_source, holdable).
narrative_ontology:cs_axiom_grounding('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', rational_inquiry_as_source, conventional).
narrative_ontology:cs_axiom('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', foundational, public_interest_as_juristic_preference).
narrative_ontology:cs_axiom_status(public_interest_as_juristic_preference, holdable).
narrative_ontology:cs_axiom_grounding('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', public_interest_as_juristic_preference, instrumental).
narrative_ontology:cs_reference_frame('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', early_hanafi_rationalist_tradition).
narrative_ontology:cs_drift_state('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', contemporary_islamic_revivalism, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5295b0fd-e622-4de8-a9f8-59ee1a6f0e30', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, public_interest_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_muslims_seeking_strict_textual_guidance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and developers of Hanafi law, who benefit from the expansive scope for rationalist reasoning (qiyas, ra'y, istihsan) which elevates their intellectual authority and allows for flexible application of law to new contexts. Their training and institutional position are tied to this methodology.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, agenda_setter,
    institutional, generational, constrained, global).

% Advocates for legal outcomes that prioritize public welfare and social utility, who find support in the principle of istihsan (juristic preference) to depart from strict analogy when it serves the greater good. They benefit from the flexibility this method offers.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, public_interest_advocates, beneficiary,
    organized, biographical, mobile, regional).

% Scholars who prioritize strict adherence to the literal text of the Quran and Sunnah, viewing expansive analogical reasoning and juristic preference as innovations that dilute divine law. They bear the cost of diminished authority for their textualist interpretations within the Hanafi framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_scholars, payer,
    moderate, generational, identity_locked, global).

% Individuals who prefer clear, unambiguous guidance derived directly from foundational texts, and may find the nuanced, context-dependent rulings produced by expansive Hanafi methods less accessible or authoritative. They bear the cost of increased interpretive complexity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_muslims_seeking_strict_textual_guidance, payer,
    powerless, biographical, constrained, local).

% Jurists of the Maliki school, who operate under a different set of methodological priorities (e.g., Medinan practice, maslaha mursala) and would challenge the Hanafi emphasis on ra'y and istihsan as primary sources, but are excluded from shaping the Hanafi framework itself.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, maliki_jurist_class, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic framework for deriving Islamic legal rulings (fatwas) in diverse and evolving contexts, ensuring consistency and adaptability across a vast geographical and temporal spread of Muslim communities.
% TRANSFER_FUNCTION: Transfers interpretive authority and flexibility from strict textual literalism to the trained jurist class, allowing for the application of reasoned opinion and public interest considerations in legal derivation.
% ABSENT_VOICES: Scholars from other schools of thought (e.g., Hanbali, Maliki, Shafii) who prioritize different sources or methodologies are structurally excluded from shaping the internal Hanafi framework. They would argue for different hierarchies of evidence and interpretive limits.
% DISAPPEARANCE_RATIONALE: If the Hanafi methodology vanished, the vast body of Hanafi jurisprudence would lose its foundational coherence, leading to legal chaos for millions of Muslims who adhere to this school. New interpretive frameworks would emerge, but the immediate impact would be a profound disruption of legal and social order.
% FOUNDING_PROBLEM: The need to apply divine revelation (Quran and Sunnah) to an ever-expanding range of novel legal questions and social realities across a vast and diverse Islamic empire, where direct textual answers were often absent.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law and contemporary legal practitioners attest to the ongoing challenge of applying classical texts to modern issues. Independent scholars of comparative law also corroborate the historical and contemporary need for interpretive flexibility in religious legal systems.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) arises from the concentration of interpretive authority within the Hanafi jurist class, whose intellectual labor and training are valorized by this methodology. Suppression (0.40) is moderate, as alternative methodologies exist but are institutionally marginalized within the Hanafi framework. Theater ratio (0.10) is low, indicating that the methodology is genuinely applied, not merely performed. The historical measurements show a gradual increase in extractiveness and suppression as the school matured and solidified its interpretive dominance over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Hanafi jurist class, this methodology is a highly effective Rope, providing necessary tools for legal adaptation and public welfare. From the perspective of textualist scholars, it functions as a Snare, extracting interpretive authority from the foundational texts and imposing a more subjective, jurist-driven approach. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hanafi jurist class is a clear beneficiary, as their expertise is central to the method's operation. Public interest advocates also benefit from the flexibility of istihsan. Textualist scholars and lay Muslims seeking strict textual guidance are victims, as their preferred mode of legal derivation is de-emphasized. The Maliki jurist class is an excluded party, representing an alternative methodology that is not integrated into the Hanafi framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (applying divine law to new contexts) remains live. The classification as a Tangled Rope (rather than a Snare) acknowledges the genuine coordination function of providing a coherent legal system, while also identifying the asymmetric extraction of interpretive authority by the jurist class. This prevents mislabeling a functional, albeit imbalanced, system as pure rent-seeking, while still flagging the extractive elements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalist_authority_legitimacy,
    'Is the expansive role of ra''y and istihsan a legitimate extension of divine law, or an unwarranted human innovation?',
    'Theological and philosophical debate, historical analysis of early Islamic legal practice, and comparative study of legal systems'' adaptation mechanisms.',
    'If deemed unwarranted, the extractiveness from textualist approaches would be reclassified as illegitimate, potentially shifting the constraint towards a Snare. If deemed legitimate, the coordination function would be emphasized, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationalist_authority_legitimacy, conceptual, 'Ambiguity regarding the theological legitimacy of rationalist legal reasoning.').

omega_variable(
    public_interest_definition_ambiguity,
    'How is ''public interest'' (maslaha) defined and adjudicated in practice, and whose interests does it primarily serve?',
    'Empirical analysis of legal rulings invoking istihsan, sociological study of the jurist class''s social and economic ties, and comparative legal analysis of ''public interest'' doctrines.',
    'If ''public interest'' is found to consistently align with the interests of the powerful or the jurist class itself, the extractiveness would be higher and the coordination function more suspect, pushing towards a Snare. If it genuinely serves broad societal welfare, the Rope aspect is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_interest_definition_ambiguity, empirical, 'Ambiguity in the practical application and beneficiaries of ''public interest'' (istihsan).').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''hanafi_reading'' of the ''usul_al_fiqh_method'' kernel. Sibling readings (maliki_reading, shafii_reading, hanbali_reading) would structurally alter the hierarchy of legal sources and the scope for juristic discretion.',
    'Comparative textual analysis of the foundational works of each school and their historical application.',
    'The classification of this constraint is specific to the Hanafi framework; adopting a different reading would instantiate a different constraint with distinct metrics and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one specific reading of a contested kernel, with distinct structural properties from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(usul_tr_t300, usul_al_fiqh_method__hanafi_reading, theater_ratio, 300, 0.07).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__hanafi_reading, theater_ratio, 600, 0.08).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__hanafi_reading, theater_ratio, 900, 0.09).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(usul_be_t300, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 300, 0.55).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 600, 0.6).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 900, 0.63).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(usul_su_t300, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 300, 0.32).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 600, 0.35).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 900, 0.38).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1200, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
