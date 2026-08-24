% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__sovereignty_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: article_27_veto_power__sovereignty_reading
 *   human_readable: P5 Veto as Westphalian Sovereignty Principle
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty_reading of the
 *   article_27_veto_power kernel. The reading holds that the P5 veto is not a
 *   designed institutional mechanism but a structural inevitability: in a
 *   world of nuclear-armed great powers with global-reach enforcement
 *   capacity, no international institution can compel a great power against
 *   its will. The veto simply formalizes what physics and power distribution
 *   already dictate — Westphalian sovereignty (no state bound without
 *   consent) applied to actors who can enforce that consent requirement
 *   materially. The constraint has near-zero extractiveness because it
 *   extracts nothing; it merely recognizes a boundary that exists
 *   independently of the Charter text. Suppression is low because no active
 *   enforcement maintains the veto — the veto IS the enforcement capacity.
 *   Accessibility collapse is near-total: no alternative institutional form
 *   can compel a nuclear great power. Resistance is near-zero because the
 *   constraint is not imposed but recognized.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__sovereignty_reading, 0.05).
domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, 0.1).
domain_priors:theater_ratio(article_27_veto_power__sovereignty_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__sovereignty_reading, mountain).
narrative_ontology:human_readable(article_27_veto_power__sovereignty_reading, "P5 Veto as Westphalian Sovereignty Principle").
narrative_ontology:topic_domain(article_27_veto_power__sovereignty_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__sovereignty_reading, 'ea0494d2-e808-4fcf-b5d4-f1a8b3671cee').
narrative_ontology:cs_kernel_codification('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', formalized).
narrative_ontology:cs_authority_grounding('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', lineage).
narrative_ontology:cs_interpretation_layer_present('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee').
narrative_ontology:cs_reading_relation('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', article_27_veto_power__coordination_reading, influences).
narrative_ontology:cs_reading_relation('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_axiom('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', foundational, westphalian_consent_principle).
narrative_ontology:cs_axiom_status(westphalian_consent_principle, holdable).
narrative_ontology:cs_axiom_grounding('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', westphalian_consent_principle, deontological).
narrative_ontology:cs_axiom('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', foundational, nuclear_asymmetry_entails_veto).
narrative_ontology:cs_axiom_status(nuclear_asymmetry_entails_veto, holdable).
narrative_ontology:cs_axiom_grounding('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', nuclear_asymmetry_entails_veto, empirically_contingent).
narrative_ontology:cs_reference_frame('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', westphalian_sovereignty_order).
narrative_ontology:cs_drift_state('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', contemporary_un_charter_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ea0494d2-e808-4fcf-b5d4-f1a8b3671cee', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__sovereignty_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, westphalian_sovereignty).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, consent_basis_of_international_law).
narrative_ontology:constraint_vindicates(article_27_veto_power__sovereignty_reading, nuclear_deterrence_as_structural_fact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No coordination function — the veto does not solve a coordination problem; it recognizes a structural boundary. Great powers cannot be compelled by any institution; the veto is the formal acknowledgment of this fact.
% TRANSFER_FUNCTION: No transfer — the constraint moves nothing from anyone to anyone. It is a recognition of pre-existing power distribution, not an arrangement that redistributes resources.
% ABSENT_VOICES: States that would prefer a compulsory international jurisdiction (many non-P5 states, international lawyers, cosmopolitan theorists) are not 'absent' in the exclusion sense — their preference is for an alternative that this reading holds is structurally impossible. Their objection is to physics, not to the Charter.
% DISAPPEARANCE_RATIONALE: If the veto disappeared overnight (Charter amended to remove Article 27(3)), the structural reality would not change: any Security Council resolution attempting to compel a nuclear great power would be ignored or trigger withdrawal. The world would rearrange into explicit great-power concert or unilateral action — the veto's disappearance would not create compulsory jurisdiction.
% FOUNDING_PROBLEM: The UN Charter's founders faced the League of Nations' failure: an institution with compulsory jurisdiction that great powers ignored or left. The veto was not 'built to solve' a problem — it was the condition on which great powers agreed to join, recognizing that no institution can compel them.
% FOUNDING_PROBLEM_CORROBORATION: The sovereignty reading's founding account is corroborated by the negotiating record (Dumbarton Oaks, San Francisco) where P5 representatives explicitly stated they would not join an organization that could compel them. Non-P5 delegates (e.g., Latin American states) objected but acquiesced — their objections are on record, confirming the structural recognition was not a P5 invention but a condition of great-power participation.
narrative_ontology:disappearance_verdict(article_27_veto_power__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__sovereignty_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(article_27_veto_power__sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(article_27_veto_power__sovereignty_reading),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(article_27_veto_power__sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(article_27_veto_power__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05) reflects only the minimal administrative overhead of the veto procedure itself — the constraint does not transfer resources from governed to governors. Suppression (0.1) captures the Charter amendment barrier (Article 108/109), but this is a procedural artifact, not active coercion. Theater ratio (0.05) is minimal because the veto's operation is transparent and its justification (great-power consensus requirement) matches its function. Accessibility collapse (0.95) is high because any institution claiming compulsory authority over nuclear great powers faces the same coordination failure — the alternative (compulsory jurisdiction) is structurally unavailable. Resistance (0.05) is near-zero because non-P5 states do not seriously resist the veto; they work around it or accept it as background condition.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading declares no beneficiaries or victims because the constraint is not an arrangement between parties — it is a structural fact of the international system. The P5 do not 'benefit' from the veto in the extraction sense; they simply possess the enforcement capacity that makes the veto a recognition rather than a grant. Non-P5 states are not 'victims'; they face the same structural constraint (no institution can compel a nuclear great power) regardless of Charter text. The engine's directionality derivation will find no structural extraction to distribute because base extractiveness is near-zero and no beneficiary/victim declarations exist.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the P5 veto a structural inevitability of nuclear power distribution (this reading), a designed coordination mechanism to prevent great-power war (coordination_reading), or an oligopolistic extraction device (oligopoly_reading)?',
    'Counterfactual institutional design: if a global institution with compulsory jurisdiction over great powers were proposed today, would nuclear-armed states join? Historical test: did the P5 accept the veto as a necessary concession (coordination) or as an assertion of pre-existing sovereign right (sovereignty)?',
    'If sovereignty_reading is correct, reform is structurally impossible — any institution with compulsory authority over great powers will fail the same way. If coordination_reading is correct, reform could preserve the coordination function while altering form. If oligopoly_reading is correct, reform is blocked by beneficiary resistance, not structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Which reading correctly captures the veto''s structural nature — this reading instantiates the sovereignty frame.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Does the veto derive from a genuine natural-law constraint (nuclear weapons make great-power compulsion physically impossible), or is ''structural inevitability'' a constructed narrative that serves P5 interests?',
    'Examine whether non-nuclear great powers (e.g., Germany, Japan, India pre-1998) faced the same compulsion-impossibility. Test whether the veto''s scope (all substantive SC matters) matches the nuclear-compulsion boundary or exceeds it.',
    'If the veto''s scope exceeds the nuclear-compulsion boundary, the ''natural law'' claim is falsified for the excess — those provisions become candidate tangled_rope or snare. If scope matches exactly, the mountain claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Whether the mountain claim''s scope matches the physical constraint it invokes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__sovereignty_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__sovereignty_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(arti_tr_t20, article_27_veto_power__sovereignty_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(arti_tr_t40, article_27_veto_power__sovereignty_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(arti_tr_t60, article_27_veto_power__sovereignty_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__sovereignty_reading, theater_ratio, 80, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__sovereignty_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(arti_be_t20, article_27_veto_power__sovereignty_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(arti_be_t40, article_27_veto_power__sovereignty_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(arti_be_t60, article_27_veto_power__sovereignty_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__sovereignty_reading, base_extractiveness, 80, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_27_veto_power__sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(arti_su_t20, article_27_veto_power__sovereignty_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(arti_su_t40, article_27_veto_power__sovereignty_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(arti_su_t60, article_27_veto_power__sovereignty_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(arti_su_t80, article_27_veto_power__sovereignty_reading, suppression_requirement, 80, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__sovereignty_reading, article_27_veto_power__oligopoly_reading).

% DUAL FORMULATION NOTE:
% This sovereignty_reading, the coordination_reading, and the oligopoly_reading form a constraint family decomposing the article_27_veto_power kernel. They differ in ε (near-zero vs moderate vs high), beneficiary/victim structure (none vs coordination beneficiaries vs P5 beneficiaries), and claimed type (mountain vs rope/tangled_rope vs snare/tangled_rope). The ε-invariance principle requires separate stories because the label 'P5 veto' conflates three structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
