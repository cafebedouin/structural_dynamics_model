% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Expansive Federal Commerce Clause Reading
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint story models the expansive federal reading of the
 *   Commerce Clause kernel: the doctrine that Congress may regulate all
 *   economic activity with substantial aggregate effects on national markets,
 *   even when the activity is purely intrastate. Tracing from Wickard v.
 *   Filburn (1942) to the present, this reading converts the clause from a
 *   narrow interstate-trade barrier into a general federal police power. It
 *   is claimed as necessary national coordination but operates as asymmetric
 *   extraction from state autonomy. The claim (tangled_rope) and metrics are
 *   independently authored; the divergence between the two is the signal the
 *   engine measures.
 *
 * KEY AGENTS:
 *   - Federal administrative state (institutional beneficiary/arbitrage-grade exit nationally) â captures regulatory authority
 *   - National regulatory coalition (organized beneficiary/mobile exit nationally) â benefits from uniform standards
 *   - State governments (organized payer/constrained exit nationally) â lose plenary police powers to federal preemption
 *   - Local economic actors (moderate payer/constrained exit nationally) â bear compliance costs of federalized local regulation
 *   - Federal judiciary (institutional agenda-setter/constrained exit nationally) â maintains and enforces the interpretive framework
 *   - Constitutional originalists (organized excluded/constrained exit nationally) â see full structure but lack doctrinal majority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.78).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.78).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Expansive Federal Commerce Clause Reading").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, '9ac54c2f-f54e-4f3e-b56e-41541a18cd89').
narrative_ontology:cs_kernel_codification('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', fixed_text).
narrative_ontology:cs_authority_grounding('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', lineage).
narrative_ontology:cs_interpretation_layer_present('9ac54c2f-f54e-4f3e-b56e-41541a18cd89').
narrative_ontology:cs_reading_relation('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', foundational, aggregate_effects_test).
narrative_ontology:cs_axiom_status(aggregate_effects_test, holdable).
narrative_ontology:cs_axiom_grounding('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', aggregate_effects_test, conventional).
narrative_ontology:cs_axiom('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', foundational, jurisdictional_nexus_not_required).
narrative_ontology:cs_axiom_status(jurisdictional_nexus_not_required, holdable).
narrative_ontology:cs_axiom_grounding('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', jurisdictional_nexus_not_required, conventional).
narrative_ontology:cs_reference_frame('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', national_economic_integrity_framework).
narrative_ontology:cs_drift_state('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', post_new_deal_constitutional_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ac54c2f-f54e-4f3e-b56e-41541a18cd89', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_regulatory_coalition).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_economic_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wields constitutionally expanded authority to regulate local manufacturing, labor, agriculture, and crime under the aggregate effects test. Drafts and enforces rules that displace state legislation; the Commerce Clause is the primary hook for federal regulatory power.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, beneficiary,
    institutional, generational, constrained, national).

% Civil society groups, industry associations, and policy advocates that benefit from uniform national standards rather than fifty-state fragmentation. They file amicus briefs and lobby to preserve the broad reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_regulatory_coalition, beneficiary,
    organized, generational, mobile, national).

% Exercise plenary police powers that are preempted or invalidated when federal statutes rely on the expansive Commerce Clause. Must defend state laws in federal court and cannot opt out of the constitutional framework.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_governments, payer,
    organized, generational, constrained, national).

% Small producers and local businesses subject to federal regulation of ostensibly intrastate activity. Bear compliance costs and lose the ability to shop for favorable local regulatory environments.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_economic_actors, payer,
    moderate, biographical, constrained, national).

% Maintains the interpretive framework through precedent, validating or limiting federal statutes under the substantial effects test. Bound by stare decisis and institutional norms, but retains discretion to narrow or expand the doctrine in new cases.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Argue that the expansive reading exceeds the original meaning of the Commerce Clause. They dissent in legal scholarship and some judicial opinions but do not command the prevailing interpretive majority.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, constitutional_originalists, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:fixing_cost_class(commerce_clause_text__expansive_federal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents state-level trade barriers and regulatory balkanization by providing a single federal forum for economic regulation; coordinates a national common market for multi-state problems.
% TRANSFER_FUNCTION: Transfers regulatory authority and compliance burdens from state governments and local economic actors to federal agencies and courts; moves policy autonomy from subnational to national standard-setters.
% ABSENT_VOICES: State sovereignty advocates and constitutional originalists who argue the reading collapses the distinction between national and local authority; they are present in dissent but excluded from the prevailing doctrinal framework.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, federal statutes regulating local manufacturing, labor, agriculture, and environmental conditions would lose primary constitutional footing; states would regain plenary police powers and the national market would fragment into competing regulatory regimes.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states erected trade barriers and protectionist laws; the Commerce Clause was adopted to create a unified national market and prevent economic balkanization.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians attest to balkanization under the Articles. However, originalist scholars and dissenting federal judges attest from outside the beneficiary set that the current expansive reading far exceeds the scope necessary to solve that founding problem; corroboration is split.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the reading authorizes federal regulation of ostensibly local activity with only attenuated market connections, transferring vast policy autonomy to the center. Suppression is equally high (0.78) because the constraint persists through active judicial preemption of state law and rejection of state alternatives. Theater_ratio (0.48) reflects moderate performative adherence to the constitutional text ('commerce among the several states') while functionally stretching it to cover non-commercial local conduct. Accessibility_collapse is very high (0.88): once the aggregate effects framework is accepted, state legal alternatives are constitutionally foreclosed. Resistance (0.72) captures persistent state litigation and originalist dissent. The temporal series show a non-monotonic path: steady expansion through the mid-20th century, a slight contraction around the Lopez era (1995), and renewed expansion thereafter, reflecting the doctrine's contested but dominant status.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary and administrative state experience the constraint as a necessary coordinating authority they maintain; state governments experience it as a forced transfer of regulatory autonomy. The engine computes this divergence from structural data: same spatial scope and comparable institutional power, but diametrically opposed roles (beneficiary vs. payer) and similarly constrained exits produce divergent directionality and effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal administrative state and national regulatory coalition are structural beneficiaries (low d, subsidized by expanded authority). State governments and local economic actors are structural targets (high d, extraction amplified by organized/moderate power and national scope). The federal judiciary sits near symmetric as agenda-setter: it administers the constraint and is neither primarily subsidized nor extracted from, though it bears institutional maintenance costs. Constitutional originalists are excluded from the beneficiary structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading prevents mandatrophy mislabeling because it retains a genuine coordination function: it does solve multi-state collective action problems and prevents protectionist balkanization. However, the metrics and structural declarations (active enforcement, identifiable victims in state governments, concentrated authority gains in the federal administrative state) prevent it from being classified as pure rope. The coordination story is not cover â it is real â but the same structure also extracts asymmetrically, which is the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint the expansive federal reading of the Commerce Clause kernel, or would a sibling reading (originalist or limited) change the structural classification?',
    'Cross-reading comparison: compile parallel constraint stories for originalist_narrow_reading and substantial_effects_limited_reading and compare epsilon, beneficiary/victim structure, and computed per-seat types.',
    'If the originalist reading yields mountain or rope while this reading yields tangled_rope, the disagreement is located in interpretive method rather than empirical facts, confirming the kernel decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Structural location within Commerce Clause kernel family').

omega_variable(
    national_coordination_vs_federal_overreach,
    'Does the current expansive reading represent the necessary cost of national market coordination, or has it become a vehicle for federal power aggregation unrelated to interstate trade?',
    'Historical counterfactual and comparative federalism analysis: compare regulatory outcomes under the expansive reading against state laboratory performance in policy domains where federal authority is weak.',
    'If extraction exceeds coordination necessity, the tangled_rope classification strengthens; if inseparable, the constraint moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_coordination_vs_federal_overreach, empirical, 'Coordination-extraction boundary for Commerce Clause scope').

omega_variable(
    federal_administrative_capture,
    'Is the federal administrative state a neutral coordinator of national markets, or a concentrated beneficiary of expanded constitutional authority?',
    'Policy drift tracking: classify agency rulemaking under the Commerce Clause by whether it targets genuine multi-state externalities or intrastate preferences.',
    'If capture is concentrated, gain_flow is confirmed to federal_administrative_state; if diffuse, the extraction profile would flatten.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_administrative_capture, empirical, 'Beneficiary concentration in federal administrative state').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 0, 82).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_text__expansive_federal_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_text__expansive_federal_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_text__expansive_federal_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_text__expansive_federal_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_text__expansive_federal_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(comm_tr_t53, commerce_clause_text__expansive_federal_reading, theater_ratio, 53, 0.46).
narrative_ontology:measurement(comm_tr_t65, commerce_clause_text__expansive_federal_reading, theater_ratio, 65, 0.47).
narrative_ontology:measurement(comm_tr_t82, commerce_clause_text__expansive_federal_reading, theater_ratio, 82, 0.48).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_text__expansive_federal_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(comm_be_t10, commerce_clause_text__expansive_federal_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(comm_be_t20, commerce_clause_text__expansive_federal_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(comm_be_t30, commerce_clause_text__expansive_federal_reading, base_extractiveness, 30, 0.73).
narrative_ontology:measurement(comm_be_t40, commerce_clause_text__expansive_federal_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(comm_be_t53, commerce_clause_text__expansive_federal_reading, base_extractiveness, 53, 0.71).
narrative_ontology:measurement(comm_be_t65, commerce_clause_text__expansive_federal_reading, base_extractiveness, 65, 0.75).
narrative_ontology:measurement(comm_be_t82, commerce_clause_text__expansive_federal_reading, base_extractiveness, 82, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_text__expansive_federal_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(comm_su_t10, commerce_clause_text__expansive_federal_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(comm_su_t20, commerce_clause_text__expansive_federal_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(comm_su_t30, commerce_clause_text__expansive_federal_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(comm_su_t40, commerce_clause_text__expansive_federal_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(comm_su_t53, commerce_clause_text__expansive_federal_reading, suppression_requirement, 53, 0.7).
narrative_ontology:measurement(comm_su_t65, commerce_clause_text__expansive_federal_reading, suppression_requirement, 65, 0.74).
narrative_ontology:measurement(comm_su_t82, commerce_clause_text__expansive_federal_reading, suppression_requirement, 82, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% The Commerce Clause kernel decomposes into three structurally distinct readings. The expansive federal reading (this story) carries high extraction and broad federal authority. The originalist narrow reading limits the clause to cross-border trade and instrumentalities. The substantial effects limited reading accepts the effects test but requires jurisdictional nexus and non-pretextual economic regulation. Each reading has a different epsilon, different stakeholder directionalities, and different computed types; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
