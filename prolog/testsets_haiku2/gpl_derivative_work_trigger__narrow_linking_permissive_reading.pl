% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Narrow Linking Interpretation: Linking as Aggregation, Not Derivation
 *   domain: legal/open-source governance
 *
 * SUMMARY:
 *   The GPL's copyleft mechanism requires that derivative works disclose
 *   their source code. The contested question is whether software components
 *   dynamically linked to GPL-licensed libraries are derivative works. The
 *   narrow linking reading (this constraint) treats linking as aggregation of
 *   separate, independently licensed components—even if they function
 *   together—provided the proprietary code did not modify the GPL code
 *   itself. This reading permits proprietary software vendors to distribute
 *   closed-source modules linked to GPL libraries without triggering
 *   source-disclosure obligations. The FSF and GPL advocates argue this
 *   misinterprets the copyleft clause; courts and vendors increasingly adopt
 *   it as settled law. The claim/metric gap is deliberate and analytically
 *   productive: the narrow reading is CLAIMED as rope (a stable coordination
 *   rule reducing ambiguity) while the authored metrics describe
 *   substantially extractive, actively enforced operation—the engine measures
 *   this divergence and identifies where enforcement energy actually flows.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors (powerful, arbitrage exit) — the reading's primary beneficiary; extract ability to use GPL code without reciprocating
 *   - gpl_project_maintainers (organized, constrained exit) — bears the extraction; copyleft propagation goal frustrated
 *   - end_users (powerless, trapped exit) — structurally excluded from source availability when proprietary modules link GPL code
 *   - legal_interpretive_authorities (institutional, analytical) — the agenda-setter; their rulings sustain the reading as binding interpretation
 *   - free_software_advocates (moderate, constrained) — payer; their countervailing interpretation is overridden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.71).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Narrow Linking Interpretation: Linking as Aggregation, Not Derivation").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "legal/open-source governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '1258b47b-5c8c-4501-ac66-e2bed320678b').
narrative_ontology:cs_kernel_codification('1258b47b-5c8c-4501-ac66-e2bed320678b', fixed_text).
narrative_ontology:cs_authority_grounding('1258b47b-5c8c-4501-ac66-e2bed320678b', lineage).
narrative_ontology:cs_interpretation_layer_present('1258b47b-5c8c-4501-ac66-e2bed320678b').
narrative_ontology:cs_reading_relation('1258b47b-5c8c-4501-ac66-e2bed320678b', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('1258b47b-5c8c-4501-ac66-e2bed320678b', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('1258b47b-5c8c-4501-ac66-e2bed320678b', foundational, linking_is_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('1258b47b-5c8c-4501-ac66-e2bed320678b', linking_is_not_derivation, conventional).
narrative_ontology:cs_axiom('1258b47b-5c8c-4501-ac66-e2bed320678b', foundational, modification_not_aggregation_triggers_copyleft).
narrative_ontology:cs_axiom_status(modification_not_aggregation_triggers_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('1258b47b-5c8c-4501-ac66-e2bed320678b', modification_not_aggregation_triggers_copyleft, instrumental).
narrative_ontology:cs_reference_frame('1258b47b-5c8c-4501-ac66-e2bed320678b', gpl_copyleft_propagation_doctrine).
narrative_ontology:cs_drift_state('1258b47b-5c8c-4501-ac66-e2bed320678b', contemporary_proprietary_integration_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('1258b47b-5c8c-4501-ac66-e2bed320678b', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, closed_source_derivative_publishers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_project_maintainers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_seeking_source_availability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_license_advocates).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_aggregation_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, linking_not_derivation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Distribute proprietary closed-source modules dynamically linked to GPL-licensed libraries without releasing source code, provided they did not modify the GPL code itself. Under this reading, the linking relationship does not trigger copyleft obligations because linking is treated as aggregation of separate components, not creation of a derivative work.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Distributed GPL code intending to propagate source disclosure to any work that incorporates their code; under this reading, dynamic linking to proprietary modules does not trigger the copyleft obligation, meaning the propagation goal is frustrated when proprietary software uses their library without reciprocating source disclosure.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_project_maintainers, payer,
    organized, generational, constrained, global).

% Receive applications that link GPL libraries to proprietary modules but cannot access source code for the proprietary portions; their ability to inspect, modify, and redistribute is blocked by the boundary drawn by the narrow linking interpretation. They are excluded from the license negotiation that determines which reading applies.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_seeking_source_availability, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_seeking_source_availability, excluded).

% Oppose the narrow linking reading as it undermines the GPL's copyleft mechanism; they argue that any meaningful incorporation of GPL code should trigger source-disclosure reciprocity. Their interpretive position is overridden by the narrow reading's adoption in practice by proprietary software vendors.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_license_advocates, payer,
    moderate, biographical, constrained, global).

% Courts and licensing authorities that adjudicate which reading of the GPL's derivative-work clause applies. Their rulings determine whether proprietary modules can be shipped linked to GPL code without triggering disclosure obligations. The narrow linking reading is sustained by their interpretive authority.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, legal_interpretive_authorities, agenda_setter,
    institutional, generational, analytical, national).

% The Free Software Foundation and GPL drafting committees whose original intent was to propagate source disclosure through linking; they view the narrow linking reading as a misinterpretation that leaves a loophole their text did not intend, but have limited enforcement power against judicial precedent once established.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_license_drafters, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent interpretive rule for determining when linked software components trigger copyleft obligations, reducing ambiguity for vendors who integrate GPL libraries into commercial products.
% TRANSFER_FUNCTION: Transfers the burden of source-disclosure compliance from proprietary software vendors (who link without disclosing) to GPL maintainers and end-users (who lose the propagation guarantee they licensed to receive). Vendors extract the ability to use GPL code without reciprocating transparency.
% ABSENT_VOICES: End-users who would prefer source availability, and GPL drafting-intention holders who view the narrow reading as a misinterpretation, are structurally excluded from the interpretive process. Their objections are raised in litigation and advocacy but do not control the reading's adoption.
% DISAPPEARANCE_RATIONALE: If the narrow linking reading disappeared (replaced by a broader reading), proprietary vendors would need to either stop linking GPL libraries, release their modules' source code, or adopt permissive licenses for their proprietary code. The derivative-work landscape would reorganize around copyleft reciprocity rather than aggregation boundaries.
% FOUNDING_PROBLEM: Early GPL adoption faced ambiguity about whether dynamic linking to library code triggered derivative-work obligations. Vendors needed clarity on whether they could ship proprietary code linked to GPL libraries. The narrow reading emerged to resolve this ambiguity in a way that permits proprietary use.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary software vendors affirm the founding problem remains live: GPL code integration decisions depend on knowing whether linking triggers disclosure. Free Software Foundation and GPL advocates counter that the 'problem' was resolved intentionally in GPL v2's propagation design, and the narrow reading misrepresents that intent. Independent legal scholarship documents both interpretive traditions; neither side's self-attestation is external corroboration.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.38 to 0.68 over the interval as judicial precedent solidifies and proprietary vendors increasingly rely on the narrow reading to build closed-source derivatives. Suppression tracks upward (0.54→0.71) because maintaining the narrow reading requires active enforcement: licensing bodies must reject broader interpretations, courts must rule for vendors, and the definition of 'derivative work' must be defended against copyleft readings. Theater rises (0.18→0.42) as vendors frame the narrow reading as technical precision ('linking is not modification') when the interpretive choice is actually a policy boundary: it could easily be drawn otherwise (interface boundary reading) or rejected entirely (broad copyleft reading). Accessibility collapse is moderate (0.62): end-users are not offered an alternative reading; their exit from the narrow reading involves using non-GPL libraries or accepting proprietary code. Resistance is substantial (0.74) because GPL advocates, free software projects, and users actively contest the narrow reading as misinterpretation, though courts have increasingly sustained it.
 *
 * PERSPECTIVAL GAP:
 *   Proprietary vendors experience this reading as coordination—a clear rule that lets them integrate GPL components without unexpected compliance burdens. They computationally benefit (d near 0.0). GPL maintainers and end-users experience it as extraction—their copyleft propagation guarantee is overridden by a linking boundary they did not agree to. They computationally suffer (d near 1.0). Legal authorities frame it as doctrinal precision, but their interpretive authority sustains the boundary, and that authority is exercised on behalf of vendors. The engine computes these divergences from power, exit options, and beneficiary/victim status; do not reconcile them to a single seat's view.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors are beneficiaries (they extract the right to use GPL code without copyleft obligations) with powerful status and arbitrage-grade exit (they could switch to non-GPL libraries if the reading changed, but the reading's existence is their exit route). GPL maintainers are payers (their propagation goal is frustrated, they are organized but constrained—they cannot unilaterally redefine 'derivative work'). End-users are payers and excluded: they lose source availability and have no voice in the interpretive process. The narrow reading creates asymmetric directionality across these seats because it operates an aggregation boundary that only the beneficiary side wanted.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents misclassification of extraction as coordination: the narrow reading appears to solve a genuine problem (interpretive clarity), but the energy invested in enforcing it flows one direction (toward vendors). The founding_problem (ambiguity about linking) is LIVE because vendors legitimately need clarity, but the SOLUTION adopted is not neutral—it is the solution that benefits vendors most. The suppression metric (0.71) and rising theater (0.42) reveal that maintaining this reading requires active work to exclude the broad_copyleft_reading and interface_boundary_reading alternatives, which suggests the constraint is not natural or inevitable but politically sustained. The mandatrophy risk is minimal because the reading has not yet exhausted its function (vendors still depend on the clarity it provides), but the measurement trajectory shows extraction accumulating (rising by 30 percentage points over 35 units) while the copyleft movement ages, a late-game dynamic where the original problem may have been solved but the reading persists as rent collection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Does ''derived from the Program'' in GPL v2 refer only to source-code modifications, or does it include any software whose function depends on linking to modified or unmodified GPL code?',
    'Statutory clarification (e.g., GPL v3 explicit linking rule, or legislative definition of derivative work in copyright law) that closes the text''s indeterminacy.',
    'Narrow reading holds if the text is read to mean modification only; broad reading holds if the text is read to mean functional incorporation. No neutral reading of the ambiguous text exists; whichever reading courts adopt becomes binding precedent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Core interpretive ambiguity: what does ''derived from'' include?').

omega_variable(
    linking_vs_modification_boundary,
    'Is the boundary between ''modifying GPL code'' (triggering copyleft) and ''linking to GPL code'' (not triggering it under narrow reading) technically defensible, or is it an arbitrary policy choice?',
    'Technical analysis of what constitutes ''modification'' in the linking context: Are dynamic symbol resolution, interface-definition changes, and runtime patches modifications? Does the distinction hold as software coupling increases?',
    'If the boundary is arbitrary (modifications and linking have equivalent semantic effects), the narrow reading is policy-driven, not technical; if technically defensible, the narrow reading reflects an objective constraint. This affects whether it computes as coordination (objective boundary) or extraction (chosen boundary benefiting vendors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_vs_modification_boundary, empirical, 'Technical defensibility of the modification vs. linking distinction').

omega_variable(
    original_intent_vs_adopted_reading,
    'Did the GPL drafters intend the narrow reading (linking as aggregation), or do their statements, design discussions, and GPL v3 revisions show they intended broader copyleft coverage?',
    'Historical documentation: GPL drafting-process records, Stallman''s statements about intended propagation scope, GPL v3''s explicit linking rule (which would be unnecessary if narrow reading were original intent).',
    'If original intent was broad, narrow reading is a misinterpretation sustained by vendor interests and court deference. If original intent was genuinely ambiguous, both readings are defensible. Historical evidence favors broad intent; the narrow reading''s adoption reflects market power overriding original intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_adopted_reading, empirical, 'Whether narrow reading represents original GPL intent or later reinterpretation').

omega_variable(
    enforcement_authority_alignment,
    'Do the institutional authorities sustaining the narrow reading (courts, licensing bodies) choose it because they find it most defensible, or because they defer to vendor interests and resource asymmetry?',
    'Analysis of court opinions, licensing-authority rulings, and amicus support: Does technical or policy reasoning dominate? Do rulings cite FSF materials and copyleft-intent documentation, or only vendor arguments?',
    'If authority chooses narrow reading on technical grounds, it is a legitimate interpretation (rope). If authority defers to vendor power despite copyleft-intent evidence, the reading is an extraction mechanism sustained by institutional capture (tangled_rope or snare). Measurement of suppression (which measures enforcement work required) would be sensitive to this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_authority_alignment, empirical, 'Whether institutional authority reflects genuine interpretation or vendor capture').

omega_variable(
    dual_reading_coexistence_stability,
    'Can the narrow reading and the broad_copyleft_reading coexist indefinitely as competing license interpretations, or does institutional dominance of one reading eventually foreclose the other?',
    'Monitoring of licensing and litigation outcomes: If one reading achieves near-universal institutional adoption, the other becomes vestigial even if not logically foreclosed. Courts would stop entertaining the minority reading.',
    'If readings coexist, the constraint is Tangled Rope (coordination + extraction in the same mechanism). If narrow reading achieves dominance and broad reading is institutionally foreclosed (vendors stop offering GPL choice, courts stop accepting copyleft arguments), the constraint shifts toward Snare (pure extraction with coordination cover story). Temporal measurement tracks this drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_reading_coexistence_stability, empirical, 'Long-term stability of dual-reading coexistence vs. institutional dominance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(gpl__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(gpl__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t35, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(gpl__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.12).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% The GPL derivative-work-trigger kernel has three readings: narrow_linking_permissive (this constraint), broad_copyleft, and interface_boundary. Each reading instantiates a different constraint with the same contested text but different ε values, beneficiary/victim structures, and institutional outcomes. The readings coexist as live positions in ongoing license disputes; none is yet institutionally foreclosed, though narrow reading dominates in practice. The three constraints form a kernel family linked by network.affects_constraints. Decomposition is necessary because ε-invariance is violated under a unified story: the broad reading sees the GPL as inherently covering all functional incorporation (ε≈0.15 for GPL's propagation goal); the narrow reading sees linking as a legitimate aggregation boundary (ε≈0.68 for proprietary vendors). The observables differ (what counts as 'derivation') and change the classification structurally. Each story's ε is fixed relative to its own reading's referent (the standing linked-library arrangement as that reading sees it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
