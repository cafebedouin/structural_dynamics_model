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
 *   human_readable: Hanafi Method of Islamic Jurisprudence (Usul al-Fiqh)
 *   domain: legal/religious/social
 *
 * SUMMARY:
 *   The Hanafi school's methodology (Usul al-Fiqh) emphasizes expansive
 *   analogical reasoning (qiyas), jurist's reasoned opinion (ra'y), and
 *   juristic preference for public interest (istihsan) when textual sources
 *   are silent or ambiguous. This reading instantiates the Hanafi approach as
 *   a constraint that coordinates legal derivation while extracting
 *   interpretive authority from strict textualism, benefiting the jurist
 *   class and public interest advocates, but imposing costs on textualist
 *   scholars and lay Muslims seeking simpler guidance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.65).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.7).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Method of Islamic Jurisprudence (Usul al-Fiqh)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "legal/religious/social").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '9d4a4521-cd47-430b-ba75-0142ff1ae34c').
narrative_ontology:cs_kernel_codification('9d4a4521-cd47-430b-ba75-0142ff1ae34c', formalized).
narrative_ontology:cs_authority_grounding('9d4a4521-cd47-430b-ba75-0142ff1ae34c', lineage).
narrative_ontology:cs_interpretation_layer_present('9d4a4521-cd47-430b-ba75-0142ff1ae34c').
narrative_ontology:cs_reading_relation('9d4a4521-cd47-430b-ba75-0142ff1ae34c', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d4a4521-cd47-430b-ba75-0142ff1ae34c', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d4a4521-cd47-430b-ba75-0142ff1ae34c', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('9d4a4521-cd47-430b-ba75-0142ff1ae34c', foundational, rational_juristic_discretion_is_valid_source).
narrative_ontology:cs_axiom_status(rational_juristic_discretion_is_valid_source, holdable).
narrative_ontology:cs_axiom_grounding('9d4a4521-cd47-430b-ba75-0142ff1ae34c', rational_juristic_discretion_is_valid_source, conventional).
narrative_ontology:cs_axiom('9d4a4521-cd47-430b-ba75-0142ff1ae34c', foundational, public_interest_can_override_strict_analogy).
narrative_ontology:cs_axiom_status(public_interest_can_override_strict_analogy, holdable).
narrative_ontology:cs_axiom_grounding('9d4a4521-cd47-430b-ba75-0142ff1ae34c', public_interest_can_override_strict_analogy, instrumental).
narrative_ontology:cs_reference_frame('9d4a4521-cd47-430b-ba75-0142ff1ae34c', early_hanafi_rationalist_expansion).
narrative_ontology:cs_drift_state('9d4a4521-cd47-430b-ba75-0142ff1ae34c', contemporary_islamic_legal_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9d4a4521-cd47-430b-ba75-0142ff1ae34c', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, public_interest_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_muslims_seeking_simple_textual_guidance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, state_legal_systems_adopting_hanafi_fiqh).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and developers of Hanafi law. They benefit from the expansive scope for rationalist reasoning (qiyas, ra'y, istihsan) which elevates their intellectual authority and allows for flexible application of law to new contexts. Their professional identity is deeply intertwined with this methodology.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurist_class, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the principle of istihsan, which allows jurists to depart from strict analogy for the sake of public interest (maslaha). This provides a mechanism for legal reform and adaptation to contemporary societal needs, aligning with their advocacy goals.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, public_interest_advocates, beneficiary,
    organized, biographical, constrained, national).

% Scholars from other schools (e.g., Hanbali) or within the Hanafi school who advocate for stricter adherence to textual sources. They bear the cost of the Hanafi method's expansive analogical and rationalist reasoning, which they perceive as diluting the authority of foundational texts and introducing unwarranted innovation. Their intellectual and professional standing is challenged by this approach.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_scholars, payer,
    powerful, generational, identity_locked, global).

% Individuals who prefer clear, direct guidance from the Quran and Sunnah. They may find the complex, jurist-driven reasoning of the Hanafi method opaque or overly flexible, leading to a sense of disconnect from the primary sources of their faith. They bear the cost of interpretive complexity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_muslims_seeking_simple_textual_guidance, payer,
    powerless, immediate, constrained, local).

% States that adopt Hanafi jurisprudence as their official legal system. They benefit from the flexibility and adaptability of the method, which allows for the development of comprehensive legal codes that can respond to evolving social and economic conditions, often under the guise of public interest.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, state_legal_systems_adopting_hanafi_fiqh, beneficiary,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic methodology for deriving Islamic legal rulings (fatwas) from foundational texts, ensuring consistency and adaptability across diverse contexts and new challenges, particularly where direct textual guidance is absent.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to shape legal outcomes from strict textual interpretation to the jurist class, who apply expansive analogical reasoning, reasoned opinion, and juristic preference for public interest.
% ABSENT_VOICES: Early Islamic communities who might have preferred simpler, more direct textual application without extensive juristic interpretation are absent from the contemporary discourse. Their perspective would likely challenge the expansive role of ra'y and istihsan.
% DISAPPEARANCE_RATIONALE: If the Hanafi method vanished, the legal systems and interpretive traditions of vast Muslim populations would collapse. There would be immense confusion in deriving new rulings, and a vacuum of authority that would be filled by other schools or entirely new interpretive frameworks, fundamentally reorganizing Islamic legal thought and practice.
% FOUNDING_PROBLEM: The need to apply Islamic principles to new situations and geographies not explicitly covered by the Quran or Sunnah, and to reconcile conflicting textual interpretations, particularly as the early Muslim empire expanded.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists and state legal systems attest that the problem of applying Islamic law to modern complexities is very much alive. Critics from other schools acknowledge the historical problem but dispute the Hanafi solution's methodology, arguing for different interpretive limits.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-high because the method grants significant interpretive discretion to jurists, which can be perceived as an 'extraction' of direct textual authority. Suppression is high because alternative, more textualist methodologies are actively challenged or marginalized within Hanafi-dominated legal systems. Theater ratio is low, as the method is genuinely applied, but some justifications for expansive reasoning may serve to maintain juristic authority. The historical measurements show a gradual increase in extractiveness and suppression as the school's methodology became more entrenched and faced internal and external challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanafi jurist's perspective, this is a sophisticated and necessary rope for legal development. From a textualist's perspective, it is a snare that elevates human reason over divine revelation. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hanafi jurist class and public interest advocates are beneficiaries, as the method empowers their roles and objectives. Textualist scholars and lay Muslims seeking simple guidance are victims, as their preferred modes of legal derivation are suppressed or complicated. State legal systems adopting Hanafi fiqh also benefit from its flexibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    istihsan_scope_ambiguity,
    'Is the application of istihsan (juristic preference for public interest) genuinely constrained by higher principles, or does it function as an unconstrained mechanism for juristic discretion?',
    'Comparative legal analysis of istihsan application across different Hanafi contexts and historical periods, identifying consistent limits or patterns of arbitrary application.',
    'If unconstrained, the extractiveness of the Hanafi method is higher, as ''public interest'' becomes a cover for juristic preference. If genuinely constrained, it functions more as a legitimate coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_scope_ambiguity, conceptual, 'Ambiguity in the scope and limits of juristic preference for public interest.').

omega_variable(
    textualist_resistance_efficacy,
    'How effective is the resistance from textualist scholars in limiting the expansive application of qiyas, ra''y, and istihsan within Hanafi legal discourse?',
    'Analysis of scholarly debates, fatwa issuance, and legal reforms within Hanafi-influenced regions to quantify the impact of textualist arguments on interpretive outcomes.',
    'If resistance is highly effective, the suppression metric is lower than currently estimated, and the constraint leans more towards a contested rope. If ineffective, the suppression is higher, reinforcing the tangled rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualist_resistance_efficacy, empirical, 'Efficacy of textualist resistance against expansive juristic reasoning.').

omega_variable(
    kernel_reading_structural_delta,
    'What are the precise structural differences in beneficiary/victim sets and power dynamics that distinguish the Hanafi reading from its sibling readings (Maliki, Shafi''i, Hanbali)?',
    'Detailed comparative analysis of each school''s usul al-fiqh, mapping specific methodological choices (e.g., source hierarchy, scope of analogy) to their impact on interpretive authority and distribution of benefits/costs among jurists and lay adherents.',
    'Clarifies the specific mechanisms by which each reading generates its unique constraint profile, enabling more precise cross-kernel comparison and identification of structural conflicts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural differences between usul al-fiqh kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t300, usul_al_fiqh_method__hanafi_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__hanafi_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__hanafi_reading, theater_ratio, 900, 0.19).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1200, 0.2).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(usul_be_t300, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 300, 0.5).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 600, 0.6).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 900, 0.63).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(usul_su_t300, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1200, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'usul_al_fiqh_method' kernel. Its expansive rationalist approach stands in contrast to the more textualist or practice-oriented sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
