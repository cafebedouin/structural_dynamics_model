% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Laws Judicial Supremacy Reading
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint story captures the judicial_supremacy_reading of the
 *   contested kernel basic_law_interpretive_boundary. Under this reading,
 *   Israel's Basic Laws constitute a higher-order constitutional framework
 *   that the Supreme Court is authorized to interpret and enforce, including
 *   the power to invalidate Knesset legislation that contradicts Basic Law
 *   provisions. The Court's interpretive authority is treated as binding on
 *   the legislature, effectively transferring final constitutional authority
 *   from the elected Knesset to an unelected judiciary. Rights-claimants gain
 *   a litigation veto over legislation threatening court-protected liberties.
 *   This reading is structurally distinct from its sibling readings:
 *   parliamentary_sovereignty_reading (which locates ultimate authority in
 *   the Knesset) and balanced_contestation_reading (which posits bounded but
 *   co-equal authority). The constraint exhibits high extractiveness because
 *   it nullifies legislative output, and high suppression because legislative
 *   alternatives that infringe on judicially defined rights are foreclosed.
 *   It is claimed as tangled_rope because it simultaneously coordinates a
 *   rights-protective legal order and extracts legislative sovereignty from
 *   the elected majority.
 *
 * KEY AGENTS:
 *   - supreme_court: Primary agenda-setter and beneficiary (institutional/analytical) â gains binding interpretive authority
 *   - constitutional_rights_claimants: Primary beneficiaries (organized/constrained) â gain litigation veto
 *   - knesset_majority: Primary payer (institutional/constrained) â bears legislative nullification
 *   - executive_branch: Secondary payer (institutional/constrained) â policy blocked by judicial review
 *   - non_litigating_citizens: Excluded payer (moderate/trapped) â preferences overridden without standing
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) â compares models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.78).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Laws Judicial Supremacy Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '090897d2-b893-4ad4-b642-79491d4ec627').
narrative_ontology:cs_kernel_codification('090897d2-b893-4ad4-b642-79491d4ec627', formalized).
narrative_ontology:cs_authority_grounding('090897d2-b893-4ad4-b642-79491d4ec627', lineage).
narrative_ontology:cs_interpretation_layer_present('090897d2-b893-4ad4-b642-79491d4ec627').
narrative_ontology:cs_reading_relation('090897d2-b893-4ad4-b642-79491d4ec627', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('090897d2-b893-4ad4-b642-79491d4ec627', basic_law_interpretive_boundary__balanced_contestation_reading, forecloses).
narrative_ontology:cs_axiom('090897d2-b893-4ad4-b642-79491d4ec627', foundational, judicial_interpretation_final_and_binding).
narrative_ontology:cs_axiom_status(judicial_interpretation_final_and_binding, holdable).
narrative_ontology:cs_axiom_grounding('090897d2-b893-4ad4-b642-79491d4ec627', judicial_interpretation_final_and_binding, conventional).
narrative_ontology:cs_axiom('090897d2-b893-4ad4-b642-79491d4ec627', foundational, basic_laws_higher_order_status).
narrative_ontology:cs_axiom_status(basic_laws_higher_order_status, holdable).
narrative_ontology:cs_axiom_grounding('090897d2-b893-4ad4-b642-79491d4ec627', basic_laws_higher_order_status, conventional).
narrative_ontology:cs_reference_frame('090897d2-b893-4ad4-b642-79491d4ec627', entrenched_basic_law_framework).
narrative_ontology:cs_drift_state('090897d2-b893-4ad4-b642-79491d4ec627', contemporary_judicial_reform_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('090897d2-b893-4ad4-b642-79491d4ec627', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a higher-order constitutional framework and claims authority to invalidate Knesset legislation that contradicts its interpretation. Enforcement occurs through judicial review and binding judgments. The Court's institutional power and legitimacy grow as the scope of review expands.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, beneficiary).

% Utilize constitutional litigation to challenge legislation threatening liberties protected by Basic Laws. They gain a de facto veto over legislation through judicial process, but depend entirely on Court access, favorable doctrine, and legal resources.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Enacts legislation subject to potential judicial nullification. Within this reading, the Knesset cannot override Supreme Court interpretations of Basic Laws via ordinary legislation; its legislative sovereignty is structurally subordinated to judicial interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority, payer,
    institutional, generational, constrained, national).

% Implements policy and drafts legislation that must survive judicial review. Policy agendas can be blocked or reshaped by Court intervention, and executive discretion is subject to reasonableness review and rights-based constraints.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, generational, constrained, national).

% Citizens whose legislative preferences are encoded in laws later struck down by the Court, but who lack standing, resources, or structural access to shape constitutional jurisprudence. They bear the democratic cost of judicial override without the litigation benefits.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, non_litigating_citizens, excluded,
    moderate, biographical, trapped, national).

% Analyze the Israeli case as an instance of strong-form judicial review in an unconsolidated constitutional regime. They compare it to parliamentary sovereignty and weak-form review models without institutional stake in the outcome.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, rights-protective legal framework by subjecting ordinary and potentially Basic legislation to judicial review, ensuring conformity with entrenched constitutional principles and protecting minorities from legislative majorities.
% TRANSFER_FUNCTION: Transfers final interpretive authority over the constitutional framework from the elected Knesset to the judiciary; moves legislative power from the Knesset majority to the Supreme Court, and grants rights-claimants a litigation veto over legislation.
% ABSENT_VOICES: Knesset majority coalition members and populist sovereignty advocates who view judicial review as democratically illegitimate are formally heard but structurally disadvantaged; non-litigating citizens whose legislative preferences are nullified lack standing to contest the framework itself.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, Knesset legislation would operate without judicial nullification, rights-claimants would lose litigation veto, and the Supreme Court would revert to a weaker interpretive role; the constitutional structure would shift toward parliamentary sovereignty.
% FOUNDING_PROBLEM: The absence of a formal constitution and the risk of legislative majorities infringing fundamental rights or undermining democratic basics in a deeply polarized polity.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and civil rights organizations attest the problem remains live given ongoing legislative threats to judicial independence and minority rights. Knesset majority coalitions and parliamentary sovereignty advocates attest the problem is exaggerated or solved, arguing the arrangement now serves judicial aggrandizement. Comparative constitutional scholarship from outside Israel offers mixed corroboration.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers legislative sovereignty to the judiciary and grants rights-claimants a veto over legislation; the Knesset cannot override judicial nullification within this reading. Suppression (0.72) is high because the constraint actively forecloses legislative alternatives that conflict with judicial interpretation; the enforcement mechanism is judicial review itself. Theater_ratio (0.42) is moderate-high and rising because maintenance of judicial supremacy increasingly relies on public legitimacy ceremonies and interpretive performance as political contestation intensifies. Accessibility_collapse (0.75) is high because once the Basic Laws are understood as higher-order, parliamentary sovereignty collapses as a viable alternative. Resistance (0.70) is high because Knesset majorities and populist coalitions actively contest the framework. Measurements track the consolidation of judicial power from the enactment of the Basic Laws through the contemporary reform crisis.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court seat experiences the constraint as constitutional coordination (protecting rights and legal stability), computing toward rope. The Knesset majority seat experiences it as extraction of democratic sovereignty, computing toward snare. The non-litigating citizenry experiences diffuse disenfranchisement. The engine computes this divergence from the same structural data; the claim of tangled_rope reflects the author's judgment that both coordination and asymmetric extraction are structurally present and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court is a beneficiary-agenda_setter with analytical exit options, placing its directionality near the subsidy end (low d). Constitutional_rights_claimants are beneficiaries but with constrained exit (they depend on judicial access), yielding low-to-moderate d. Knesset_majority and executive_branch are payers with constrained exit (no legislative override within this reading), yielding high d near full-target. Non_litigating_citizens are excluded payers with trapped exit, yielding the highest effective extraction. The scope is national, amplifying extraction for the trapped and constrained seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â protecting fundamental rights in the absence of a formal constitution â is contested but not dead; the constraint therefore does not qualify as a piton. It is not a pure snare because the coordination function (rights protection, legal certainty) is genuine and not merely a cover story. It is not a pure rope because the transfer of sovereignty to an unelected body is asymmetric and enforced. The classification as tangled_rope prevents mislabeling: pure extraction would ignore the real coordination provided to rights-claimants, while pure coordination would ignore the democratic cost imposed on the legislative majority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is this constraint''s classification stable across all three readings of the basic_law_interpretive_boundary kernel, or does the sibling reading adopted change the epsilon and beneficiary structure?',
    'Generate the sibling constraints (parliamentary_sovereignty_reading and balanced_contestation_reading) and compare their base_extractiveness and victim sets.',
    'If classification varies widely by reading, the kernel is genuinely contested; if stable, the debate is merely normative preference over an agreed structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Stability of classification across kernel readings').

omega_variable(
    basic_law_amendment_review_scope,
    'Does judicial supremacy under this reading encompass review of Basic Law amendments themselves, or only sub-constitutional legislation?',
    'Analysis of Supreme Court jurisprudence on Basic Law amendments and nation-state law rulings.',
    'If amendments are reviewable, extraction from the Knesset is total (no legislative escape); if not, a constitutional amendment channel provides an exit, lowering effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(basic_law_amendment_review_scope, conceptual, 'Scope of judicial review over Basic Law amendments').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the Knesset''s submission to judicial review structurally enforced (no available override mechanism) or internalized (normative acceptance of judicial authority)?',
    'Observing Knesset behavior during high-salience crises: if it attempts override legislation despite judicial warnings, suppression is structural; if it defers despite having procedural capacity, suppression is partly internalized.',
    'If internalized, effective suppression exceeds structural measure; constraint may compute as more extractive for legislators than raw metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t8, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t16, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t32, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 32, 0.42).

% Extraction over time
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t8, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t16, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t32, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 32, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t8, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t16, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t32, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 32, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is the judicial_supremacy_reading of kernel basic_law_interpretive_boundary. It decomposes the colloquial label 'Basic Law interpretive boundary' into three structurally distinct claims: judicial supremacy (this file), parliamentary sovereignty, and balanced contestation. Each has a distinct epsilon, beneficiary/victim structure, and classification. The kernel conflates these into a single debate, but they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
