% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text: Popular Sovereignty Reading
 *   domain: constitutional_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the popular sovereignty reading of
 *   constitutional_text: the claim that constitutional authority derives from
 *   the constituent power of the demos rather than from any institutional
 *   seat. Neither courts nor legislatures possess final interpretive
 *   supremacy; the people retain ultimate authority through amendment,
 *   convention, or revolutionary action. The constraint coordinates
 *   democratic participation by providing a meta-authority outside captured
 *   institutions, while extracting from institutional stability and expertise
 *   by subordinating courts, legislatures, and jurists to extra-institutional
 *   democratic expression. It is claimed as a rope by democratic theorists
 *   but operates as tangled rope because the coordination function
 *   (democratic self-rule) is inseparable from the asymmetric cost it imposes
 *   on institutional continuity and legal expertise.
 *
 * KEY AGENTS:
 *   - democratic_public: Primary beneficiary (powerful/generational) â holds ultimate constituent authority
 *   - popular_movements: Agenda setter (organized/biographical) â channels constituent power into enforcement
 *   - judicial_branch: Primary payer/target (institutional/generational) â bears loss of final interpretive authority
 *   - legislative_branch: Primary payer/target (institutional/generational) â bears loss of final lawmaking supremacy
 *   - constitutional_jurists: Secondary payer (organized/generational) â expertise subordinated to popular will
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.58).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.62).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text: Popular Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'ab17ea58-c662-490b-9a65-d25224632891').
narrative_ontology:cs_kernel_codification('ab17ea58-c662-490b-9a65-d25224632891', fixed_text).
narrative_ontology:cs_authority_grounding('ab17ea58-c662-490b-9a65-d25224632891', lineage).
narrative_ontology:cs_interpretation_layer_present('ab17ea58-c662-490b-9a65-d25224632891').
narrative_ontology:cs_reading_relation('ab17ea58-c662-490b-9a65-d25224632891', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('ab17ea58-c662-490b-9a65-d25224632891', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('ab17ea58-c662-490b-9a65-d25224632891', foundational, constituent_power_superior_to_constituted_power).
narrative_ontology:cs_axiom_status(constituent_power_superior_to_constituted_power, holdable).
narrative_ontology:cs_axiom_grounding('ab17ea58-c662-490b-9a65-d25224632891', constituent_power_superior_to_constituted_power, deontological).
narrative_ontology:cs_axiom('ab17ea58-c662-490b-9a65-d25224632891', foundational, popular_constituent_expression_binding).
narrative_ontology:cs_axiom_status(popular_constituent_expression_binding, holdable).
narrative_ontology:cs_axiom_grounding('ab17ea58-c662-490b-9a65-d25224632891', popular_constituent_expression_binding, conventional).
narrative_ontology:cs_reference_frame('ab17ea58-c662-490b-9a65-d25224632891', popular_constituent_authority).
narrative_ontology:cs_drift_state('ab17ea58-c662-490b-9a65-d25224632891', contemporary_institutional_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab17ea58-c662-490b-9a65-d25224632891', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_public).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_movements).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate constituent authority over constitutional meaning, exercising it episodically through amendment, convention, or revolutionary action. When mobilized, overrides institutional interpretations; when latent, the constraint operates as a background threat that keeps institutional power in check. Exit would require leaving the polity or accepting permanent subordination of popular will to institutional elites.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, democratic_public, beneficiary,
    powerful, generational, constrained, national).

% Channel constituent power into concrete constitutional demands, organizing conventions, amendment campaigns, or mobilizing extra-institutional pressure. Act as the episodic enforcement mechanism of popular sovereignty, translating diffuse democratic will into binding constitutional moments. Can demobilize or reorient to other causes.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, popular_movements, agenda_setter,
    organized, biographical, mobile, national).

% Exercises day-to-day constitutional interpretation but cannot claim final authority without risking popular override or legitimacy crisis. Maintains institutional prestige and legal expertise while remaining structurally subordinate to constituent power. Exit from this subordination is constrained by the legitimation structure of the constitutional order itself.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, judicial_branch, payer,
    institutional, generational, constrained, national).

% Exercises ordinary and constitutional lawmaking power but lacks final interpretive supremacy. Popular conventions or amendments can override legislative constitutional judgments. Cannot entrench its own authority against claims of constituent power without appearing anti-democratic.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislative_branch, payer,
    institutional, generational, constrained, national).

% Provide technical and doctrinal interpretation of constitutional text, but their authority is advisory when popular sovereignty is invoked. Expert methodologies are valued in periods of institutional routine but marginalized during constitutional moments. Can exit to other jurisdictions or fields, though professional identity is tied to the national constitutional tradition.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_jurists, payer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional course-correction when institutional interpreters diverge from popular will, allowing the polity to reclaim interpretive authority through amendment, convention, or revolutionary action.
% TRANSFER_FUNCTION: Moves final constitutional interpretive authority from institutional seats (courts, legislatures, experts) to the extra-institutional democratic public, concentrating democratic legitimacy in popular expression while dispersing institutional certainty.
% ABSENT_VOICES: Non-citizen residents and future generations are bound by popular constitutional moments but cannot participate in them. Institutional elites are present as payers but their supremacy claims are structurally subordinated.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, courts and legislatures would claim final interpretive supremacy, constitutional amendment would become ordinary institutional procedure rather than constituent expression, and the boundary between ordinary and constitutional politics would collapse â democratic legitimacy would reorganize around institutional rather than popular authority.
% FOUNDING_PROBLEM: How to prevent constitutional text from being captured by unaccountable institutional interpreters when both courts and legislatures claim final authority but neither embodies the democratic will of the constituent power.
% FOUNDING_PROBLEM_CORROBORATION: Democratic theorists (SieyÃ¨s, Ackerman, Tully) attest the problem from outside institutional power; courts and legislatures typically contest that the problem exists, asserting their own democratic accountability. No neutral institutional seat corroborates without also having a stake in the outcome.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the constraint systematically transfers interpretive authority from institutional seats to the extra-institutional demos. Suppression (0.62) reflects the active suppression of judicial and legislative supremacy claims, though these alternatives remain intellectually accessible. Theater ratio (0.35) captures the performative dimension of popular sovereignty rhetoric, which often substitutes for actual democratic mobilization. Resistance (0.68) is high because institutional actors (courts, legislatures, experts) actively resist the erosion of their authority. The measurement series shows extraction accumulating as constitutional orders mature and institutionalize, peaking at mid-interval when the gap between popular sovereignty theory and institutional practice is widest.
 *
 * PERSPECTIVAL GAP:
 *   The democratic public and popular movements experience this constraint as enabling â it grants them ultimate authority and a veto over institutional capture. Judicial and legislative branches experience it as extractive â it denies them the finality that their institutional role would otherwise confer. Constitutional jurists experience a diffuse cost to their epistemic authority. The engine computes this divergence from the structural data: beneficiaries (democratic_public, popular_movements) with constrained exit sit near the low-d end; payers (judicial_branch, legislative_branch) with constrained exit sit near the high-d end.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to democratic_public and popular_movements â these agents structurally collect the coordination benefit (democratic authority) and their directionality is damped toward the beneficiary pole. Victim declarations map to judicial_branch, legislative_branch, and constitutional_jurists â these agents bear the cost of subordination and their directionality is amplified toward the target pole. No override is needed because the structural derivation chain produces accurate d values: institutional payers with constrained exit are full targets, while diffuse democratic beneficiaries with constrained (but collective) exit sit nearer the beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this arrangement as pure extraction (snare) because a genuine coordination function is present: popular sovereignty provides a real mechanism for democratic course-correction when institutions diverge from popular will. Conversely, it prevents mislabeling as pure coordination (rope) because the asymmetric cost to institutional stability and expertise is structurally constitutive, not incidental. Without the victim seats (courts, legislatures, experts losing final authority), the constraint would be a different arrangement â either a rope or a mountain of democratic theory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is this constraint one reading of a contested kernel, and how would classification shift if judicial supremacy or legislative sovereignty were adopted instead?',
    'Compare the full constraint family (judicial_supremacy_reading, legislative_sovereignty_reading, popular_sovereignty_reading) and identify which seats bear costs versus benefits under each reading.',
    'Under sibling readings, the beneficiary/victim structure inverts: courts or legislatures become beneficiaries while the democratic public becomes payer. The popular sovereignty reading is distinctive in making the demos the beneficiary seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is the popular sovereignty reading of constitutional_text; siblings assign final authority to different seats.').

omega_variable(
    popular_sovereignty_authenticity,
    'Does popular mobilization genuinely express constituent power, or is it captured by political entrepreneurs who harness democratic rhetoric for majoritarian extraction?',
    'Historical case studies of constitutional conventions and amendment processes: measure the gap between popular preferences and elite agenda-setting within those processes.',
    'If captured, the beneficiary seat shifts from democratic_public to political_entrepreneurs, and the constraint approaches snare. If authentic, the coordination function is genuine and the classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_sovereignty_authenticity, empirical, 'Whether popular sovereignty is authentically exercised or elite-captured.').

omega_variable(
    institutional_subordination_or_extraction,
    'Is the subordination of courts and legislatures to constituent power a necessary democratic cost or genuine extraction from institutional stability?',
    'Comparative analysis of constitutional systems with varying degrees of popular override mechanisms, measuring institutional stability and democratic responsiveness outcomes.',
    'If subordination improves democratic responsiveness without institutional collapse, the extraction metric is overestimated. If it produces chronic instability, the victim status is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_subordination_or_extraction, preference, 'Whether institutional subordination counts as extraction or legitimate democratic cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__popular_sovereignty_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(cons_tr_t80, constitutional_text__popular_sovereignty_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(cons_tr_t100, constitutional_text__popular_sovereignty_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(cons_be_t60, constitutional_text__popular_sovereignty_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(cons_be_t80, constitutional_text__popular_sovereignty_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(cons_be_t100, constitutional_text__popular_sovereignty_reading, base_extractiveness, 100, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_text__popular_sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is the popular sovereignty reading of the constitutional_text kernel. Sibling readings (judicial_supremacy, legislative_sovereignty) instantiate mutually exclusive allocations of final interpretive authority from the same textual kernel. Each reading produces a different beneficiary/victim structure and different epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
