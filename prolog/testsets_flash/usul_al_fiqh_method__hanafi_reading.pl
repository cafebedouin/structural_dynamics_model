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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi School's Jurisprudential Method (Qiyas, Ra'y, Istihsan)
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's method of Islamic
 *   jurisprudence, which emphasizes expansive analogical reasoning (qiyas),
 *   reasoned opinion (ra'y), and juristic preference for public interest
 *   (istihsan) when textual sources are silent or ambiguous. It represents a
 *   more rationalist and flexible approach compared to other Sunni schools,
 *   allowing jurists significant scope for interpretation and adaptation.
 *   This is one reading of the broader 'usul_al_fiqh_method' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.6).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.4).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi School's Jurisprudential Method (Qiyas, Ra'y, Istihsan)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '0ef84994-e0d3-4ab7-9742-25eedc08977c').
narrative_ontology:cs_kernel_codification('0ef84994-e0d3-4ab7-9742-25eedc08977c', formalized).
narrative_ontology:cs_authority_grounding('0ef84994-e0d3-4ab7-9742-25eedc08977c', lineage).
narrative_ontology:cs_interpretation_layer_present('0ef84994-e0d3-4ab7-9742-25eedc08977c').
narrative_ontology:cs_reading_relation('0ef84994-e0d3-4ab7-9742-25eedc08977c', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ef84994-e0d3-4ab7-9742-25eedc08977c', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ef84994-e0d3-4ab7-9742-25eedc08977c', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('0ef84994-e0d3-4ab7-9742-25eedc08977c', foundational, rational_inquiry_supplements_text).
narrative_ontology:cs_axiom_status(rational_inquiry_supplements_text, holdable).
narrative_ontology:cs_axiom_grounding('0ef84994-e0d3-4ab7-9742-25eedc08977c', rational_inquiry_supplements_text, deontological).
narrative_ontology:cs_axiom('0ef84994-e0d3-4ab7-9742-25eedc08977c', foundational, public_interest_justifies_juristic_preference).
narrative_ontology:cs_axiom_status(public_interest_justifies_juristic_preference, holdable).
narrative_ontology:cs_axiom_grounding('0ef84994-e0d3-4ab7-9742-25eedc08977c', public_interest_justifies_juristic_preference, instrumental).
narrative_ontology:cs_reference_frame('0ef84994-e0d3-4ab7-9742-25eedc08977c', early_hanafi_rationalist_tradition).
narrative_ontology:cs_drift_state('0ef84994-e0d3-4ab7-9742-25eedc08977c', contemporary_islamic_revivalism, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('0ef84994-e0d3-4ab7-9742-25eedc08977c', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, rationalist_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_muslims_seeking_direct_textual_guidance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, public_interest_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and enforcers of the Hanafi method. They apply qiyas, ra'y, and istihsan to derive legal rulings, shaping the legal landscape for millions. Their professional identity is deeply intertwined with this interpretive framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars who align with the rationalist tendencies of the Hanafi school, benefiting from the intellectual space and methodological tools it provides for legal innovation and adaptation. They contribute to the ongoing development of the school's jurisprudence.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, rationalist_scholars, beneficiary,
    organized, biographical, constrained, global).

% Scholars who prioritize strict adherence to the Quran and authenticated Hadith, viewing expansive rationalist methods as potentially deviating from divine revelation. They bear the cost of their preferred methods being subordinated or marginalized within Hanafi-dominated legal systems.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, textualist_scholars, payer,
    moderate, biographical, constrained, global).

% Individuals who prefer clear, direct guidance from primary texts and may find the nuanced, jurist-driven interpretations of the Hanafi school opaque or overly complex. They are subject to rulings derived through methods they may not fully understand or agree with.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_muslims_seeking_direct_textual_guidance, payer,
    powerless, immediate, trapped, local).

% Groups and individuals who champion the concept of 'public interest' (maslaha) and benefit from the Hanafi school's willingness to depart from strict analogy (istihsan) to achieve perceived societal good. They find an avenue for legal reform and adaptation within this framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, public_interest_advocates, beneficiary,
    organized, generational, mobile, national).

% Academics and researchers who study Islamic legal theory from a comparative perspective, analyzing the structural differences and implications of various schools of thought. They are external to the internal dynamics of the Hanafi school but provide critical analysis.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_legal_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a comprehensive and adaptable framework for deriving Islamic legal rulings, ensuring that new societal challenges and evolving circumstances can be addressed within an Islamic legal paradigm, even when primary texts are silent.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to shape legal outcomes from direct textual sources to the jurist class, in exchange for legal flexibility and the ability to address public interest concerns.
% ABSENT_VOICES: Strict Zahiri or literalist schools of thought, which would reject the expansive use of qiyas, ra'y, and istihsan, are largely absent from the mainstream discourse where Hanafi jurisprudence is dominant. They would argue for a return to exclusive textualism.
% DISAPPEARANCE_RATIONALE: If the Hanafi method vanished, the legal systems in many Muslim-majority regions would face a profound crisis, lacking a coherent framework for deriving new rulings. A vacuum would emerge, likely filled by other schools or secular legal systems, fundamentally altering the legal and social order.
% FOUNDING_PROBLEM: The need for a systematic method to derive legal rulings for novel situations not explicitly covered in the Quran or Hadith, while ensuring consistency and serving the public good.
% FOUNDING_PROBLEM_CORROBORATION: The problem of legal adaptation to modernity is widely acknowledged by scholars across all Islamic legal schools, as well as by secular legal experts and international organizations. While the Hanafi solution is contested, the underlying problem it addresses remains highly relevant.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates legal reasoning and adaptation to new circumstances (benefiting jurists and the public interest) while simultaneously extracting from those who prefer strict textual adherence and suppressing alternative, more restrictive interpretive methods. The extractiveness (0.6) comes from the power granted to jurists to interpret beyond explicit texts, which can be perceived as a cost by textualists. Suppression (0.4) is moderate, as alternative schools exist but the Hanafi method actively enforces its interpretive hierarchy within its sphere of influence. Theater ratio is low (0.1) as the methods are actively applied.
 *
 * PERSPECTIVAL GAP:
 *   Hanafi jurists experience this as a Rope, a robust system for legal adaptation and public welfare. Textualist scholars and lay Muslims seeking direct textual guidance experience it as a Snare or Tangled Rope, where their preferred interpretive methods are suppressed or overridden by juristic discretion.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists are primary beneficiaries (d=0.0-0.2) as the method grants them significant interpretive authority and intellectual space. Rationalist scholars also benefit from the emphasis on reason. Textualist scholars and lay Muslims seeking direct textual guidance are victims (d=0.8-1.0) as their preferred methods are subordinated. The 'public interest' is a conceptual beneficiary, but its benefits are mediated through juristic interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing legal guidance) is still live, but its method of achieving it (expansive rationalism) is contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction from textualists) or a pure Snare (ignoring its genuine coordination function for legal adaptation). The ongoing contestation over the balance between rationalism and textualism is key to its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of the Hanafi school''s method, or an idealized representation?',
    'Comparative historical analysis of Hanafi legal rulings across different eras, contrasting theoretical statements with actual judicial practice.',
    'If idealized, the actual constraint might exhibit higher or lower extractiveness and suppression depending on the historical context of its application, potentially shifting its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Identifies this constraint as the Hanafi reading of the usul_al_fiqh_method kernel.').

omega_variable(
    rationality_vs_textualism_balance,
    'What is the precise balance between rationalist methods (qiyas, ra''y, istihsan) and textual adherence in contemporary Hanafi jurisprudence?',
    'Empirical study of fatwas and court judgments from Hanafi-majority regions, quantifying the reliance on each source.',
    'If textual adherence has increased, the constraint''s extractiveness from textualist claims might decrease; if rationalist methods remain dominant, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_vs_textualism_balance, empirical, 'Ambiguity in the practical application of rationalist vs. textual sources.').

omega_variable(
    hanafi_vs_sibling_readings_delta,
    'How would the classification of this constraint change if a sibling reading (e.g., Hanbali''s textual restrictiveness) were adopted?',
    'Counterfactual analysis: re-evaluate extractiveness, suppression, and beneficiary/victim structures under the premises of a Hanbali reading.',
    'A Hanbali reading would likely result in lower extractiveness from textualist claims, higher suppression of jurist-driven innovation, and a shift in beneficiaries/victims, potentially reclassifying it as a Mountain (textual law) or a Rope (pure coordination around text).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanafi_vs_sibling_readings_delta, conceptual, 'Comparison of this Hanafi reading with sibling readings of the usul_al_fiqh_method kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(usul_be_t10, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(usul_be_t30, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_su_t10, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(usul_su_t30, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
