% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Universal Basic Income as Political Ambiguity Vehicle
 *   domain: political_economy/social_policy/welfare_theory
 *
 * SUMMARY:
 *   Unconditional basic income appears on its surface as a unifying policy
 *   that appeals across ideological divides: left reads it as the foundation
 *   of a decommodified economy and maximized redistribution; right reads it
 *   as welfare consolidation with reduced paternalism; centrists read it as
 *   neutral, technocratic, ideologically uncontaminated. This narrative
 *   examines the universality-paradox reading: the appeal is not unifying but
 *   ambiguous, and the ambiguity itself is the mechanism that permits
 *   coalition-building across ideological incompatibility. Left and right do
 *   not actually agree on what UBI should do; they agree to defer
 *   implementation clarity and deploy universality rhetoric that permits each
 *   side to claim the policy advances its vision. The constraint extracts
 *   from ideological clarity (by obscuring incompatibility) and from targeted
 *   program recipients (by using universality to justify replacements).
 *   Extraction is moderate (0.42) because fiscal outcomes are similar across
 *   left-right implementations—the ambiguity is political, not financial; its
 *   cost is in ideology and transparency, not in redistribution magnitude.
 *   This reading instantiates one point on a contested kernel: it is NOT the
 *   freedom-floor reading (which emphasizes labor decommodification) nor the
 *   dependency-trap reading (which emphasizes incentive distortion), but the
 *   mechanism by which incompatible readings can coexist in a single policy
 *   vehicle.
 *
 * KEY AGENTS:
 *   - Political entrepreneurs: deploy universality rhetoric to build coalitions without ideological commitment
 *   - Policy designers: use ambiguity to maintain rhetorical flexibility while implementing specific distributional choices
 *   - Targeted program recipients: lose prior benefits under replacement mechanisms justified by universality
 *   - Ideological clarity (non-agent): the abstract good suppressed by the constraint's operation
 *   - Left and right coalitions: both claim the vehicle advances their incompatible visions
 *   - Fiscal analysts: expose post-implementation that different taxation-back mechanisms converge on similar outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.42).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.38).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Universal Basic Income as Political Ambiguity Vehicle").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy/welfare_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '0ac9ded8-ba2f-4482-88a6-3745f37943dd').
narrative_ontology:cs_kernel_codification('0ac9ded8-ba2f-4482-88a6-3745f37943dd', fixed_text).
narrative_ontology:cs_authority_grounding('0ac9ded8-ba2f-4482-88a6-3745f37943dd', distributed).
narrative_ontology:cs_reading_relation('0ac9ded8-ba2f-4482-88a6-3745f37943dd', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ac9ded8-ba2f-4482-88a6-3745f37943dd', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('0ac9ded8-ba2f-4482-88a6-3745f37943dd', foundational, universality_permits_coalition_across_incompatibility).
narrative_ontology:cs_axiom_status(universality_permits_coalition_across_incompatibility, holdable).
narrative_ontology:cs_axiom_grounding('0ac9ded8-ba2f-4482-88a6-3745f37943dd', universality_permits_coalition_across_incompatibility, conventional).
narrative_ontology:cs_axiom('0ac9ded8-ba2f-4482-88a6-3745f37943dd', foundational, ambiguity_suppresses_clarity_about_distributional_stakes).
narrative_ontology:cs_axiom_status(ambiguity_suppresses_clarity_about_distributional_stakes, holdable).
narrative_ontology:cs_axiom_grounding('0ac9ded8-ba2f-4482-88a6-3745f37943dd', ambiguity_suppresses_clarity_about_distributional_stakes, empirically_contingent).
narrative_ontology:cs_reference_frame('0ac9ded8-ba2f-4482-88a6-3745f37943dd', ambiguity_as_coalition_device).
narrative_ontology:cs_drift_state('0ac9ded8-ba2f-4482-88a6-3745f37943dd', post_implementation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ac9ded8-ba2f-4482-88a6-3745f37943dd', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, ideological_clarity).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, left_ideological_coalition).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, right_ideological_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates from left and right deploy universal basic income rhetoric to build cross-ideological coalitions without committing to specific implementation. A left-reading entrepreneur uses UBI as a pathway to full redistribution while claiming universality; a right-reading entrepreneur uses it as a welfare-replacement mechanism while claiming neutrality. Both benefit from the ambiguity because it permits coalition-building that would fracture under implementation pressure. They set the agenda for pilot design and legislative framing, choosing design parameters (payment level, taxation method, pilot geography) that remain compatible with multiple interpretations.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    institutional, generational, arbitrage, national).

% Design the constraint's operational form: which income sources are taxed back, what the payment level is, whether it phases out, what the administrative layer is. The ambiguity gives designers rhetorical flexibility—they can claim universality while implementing targeting through taxation, can claim neutrality while design choices embed distributional intent. They benefit from maintaining the ambiguity because clarity would expose the incompatibility between competing implementation visions and force designers to choose sides, narrowing their authority.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary).

% Existing recipients of means-tested welfare programs (TANF, housing assistance, disability) lose benefits as UBI is implemented as a replacement mechanism. Universality rhetoric frames their losses as elimination of stigmatizing targeting—a framing that obscures that their actual income may decline under a universal payment insufficient to meet their prior benefit level. They cannot exit the constraint because welfare receipt is identity-defining; they cannot dispute the framing without accepting the stigma the universality rhetoric purports to eliminate.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, biographical, trapped, national).

% The constraint's operation prevents coherent evaluation of what unconditional income support actually redistributes and from whom to whom. Because the same policy vehicle admits multiple incompatible implementations that produce similar fiscal outcomes (through different tax-back mechanisms, as taxing-back research shows), observers cannot determine which reading is operationally true without detailed post-implementation auditing. The ambiguity itself becomes a feature that prevents public deliberation about the actual distributional consequences.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, ideological_clarity, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(unconditional_income_support__universality_paradox_reading, ideological_clarity).

% Advocates for strong redistribution and labor-market decommodification read UBI as the opening to a universal basic services economy. The universality framing permits them to build coalitions with right-reading advocates; the ambiguity allows left advocates to claim the policy vehicle advances their full vision while accepting right-inflected design choices as merely transitional. They benefit from the constraint's ambiguity because it permits coalition-building with ideological opponents without requiring immediate clarification of their incompatible end-states.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, left_ideological_coalition, beneficiary,
    organized, generational, constrained, national).

% Advocates for welfare consolidation and reduced paternalism read UBI as replacing targeted benefits with a single cash transfer, with no new claims on redistribution. The universality framing permits them to build coalitions with left-reading advocates; the ambiguity allows right advocates to claim the policy vehicle is ideologically neutral while accepting left-inflected design choices. They benefit from the constraint's ambiguity because it permits coalition-building without requiring early commitment to specific implementation choices that would reveal the incompatibility with left readings.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, right_ideological_coalition, beneficiary,
    organized, generational, constrained, national).

% Measure the distributional consequences of different UBI designs post-implementation, discovering that taxation-back mechanisms (different on left and right readings) converge on similar fiscal outcomes—similar net redistribution, similar replacement of prior welfare spending. Their analysis creates pressure to acknowledge that the readings are incompatible in implementation while similar in effect, exposing the ambiguity as a mechanism for coalition-building that obscures rather than resolves fundamental disagreement about redistribution.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, fiscal_analysts, observer,
    institutional, biographical, analytical, national).

% Informal-sector workers, undocumented immigrants, and others outside formal program eligibility are neither beneficiaries of UBI (administration requires formal identity) nor included in either ideological reading's actual commitment (left and right both claim universality but implement eligibility gates). Their exclusion from the conversation is what permits the universality rhetoric to persist—the constraint works because the ambiguity is internal to the coalition; acknowledging this excluded set would force clarity about what 'universal' actually means.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, excluded_subsistence_workers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates multiple incompatible normative visions of income support (redistribution-maximizing, welfare-replacing, labor-market decommodifying) into a single policy vehicle that permits coalition-building across ideological divides by deferring implementation clarity. The coordination problem is: how to build a political majority large enough to enact income support reform when left and right hold incompatible visions of what it should do? The constraint solves this by making the ambiguity itself the coalition-building mechanism—both sides claim the vehicle advances their vision while accepting design compromises.
% TRANSFER_FUNCTION: Moves fiscal resources from tax bases to income support recipients under taxation schemes that admit multiple interpretations (progressive-redistributive or welfare-consolidating) that converge on similar net fiscal effects. Also moves political authority from program-specific welfare bureaucracies to a unified income-support administration, reducing the administrative surface where ideology is operationalized. Also transfers ideological authority from explicit redistributive or welfare-replacing commitments to the allegedly 'neutral' category of universality, which permits agents to claim the policy advances incompatible goals.
% ABSENT_VOICES: Targeted program recipients whose benefits would be cut under replacement mechanisms are present in pilot design conversations but have no veto over whether universality is used as the rationale for their exclusion from the new system. Formal-economy workers bearing the tax base are largely absent from design conversations and cannot object to taxation schemes they do not yet understand. Informal-economy and undocumented workers are structurally absent—their exclusion is what permits the universality framing to persist without immediate challenge. Economists and policy analysts who could expose the ambiguity are present but their post-implementation analysis arrives after political commitments are locked in.
% DISAPPEARANCE_RATIONALE: If the constraint (the ambiguity mechanism itself) disappeared and UBI had to be implemented with clear ideological commitment, the political coalition would fracture: left and right would advance incompatible versions that could not coexist in one policy vehicle, forcing a choice or gridlock. Existing welfare programs would persist or be replaced by whichever version won, with different distributional consequences. The ability to defer implementation clarity would vanish, and the constraint's primary function—enabling coalition-building across ideological incompatibility—would be gone.
% FOUNDING_PROBLEM: The founding problem is the political inability to build a majority coalition for income-support reform when left and right hold incompatible normative visions: left wants maximized redistribution and labor decommodification, right wants welfare consolidation and reduced paternalism. These visions are not easily reconciled because they rest on different empirical premises (about labor incentives, about the nature of deservingness) and different normative commitments (about the role of the state in redistribution). Prior policy attempts to build such coalitions either collapsed under implementation pressure or required one side to compromise in ways the other side experienced as defeat.
% FOUNDING_PROBLEM_CORROBORATION: Left and right ideological coalitions both attest the founding problem remains live—both claim their vision for income support is politically blocked by the other's incompatible vision. Fiscal analysts and policy design practitioners attest that the ambiguity mechanism is actively maintained: whenever implementation details threaten to clarify incompatibility, both sides defend the universality framing and defer specification. Legislative testimony from pilot-site advocates shows both left and right reading advocates claiming victory in the same policy design, evidence that the ambiguity is operational.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42, plateauing at 15) because the fiscal extraction is modest—taxation-back research shows different ideological implementations converge on similar net redistribution and welfare replacement, so no single party captures massive gains. However, extraction from ideological clarity is substantial: the constraint's operation actively suppresses acknowledgment that left and right visions are incompatible in implementation. Theater ratio is high (0.68 at interval end) and rising sharply (from 0.48 to 0.68 between t=0 and t=15), indicating that the constraint's primary function shifts from actual policy-making (t=0: diverse design choices, genuine uncertainty about direction) to maintenance of the ambiguity (t=15+: design choices converge but rhetoric claims universality remains). Suppression requirement rises (0.28 to 0.38) as implementation pressure increases—more active effort required to maintain the ambiguity as fiscal consequences become visible. Accessibility collapse is moderate (0.45): alternatives exist (left could choose explicit redistribution, right could choose explicit consolidation) but are politically suppressed because the coalition would fracture if either side committed to clarity.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (political entrepreneurs, policy designers) and the beneficiary seats (left and right coalitions) experience the constraint as genuine coordination; the victim seats (targeted program recipients, ideological clarity) experience it as extraction through ambiguity. The fiscal-analyst seat, entering post-implementation, computes it as a mechanism that obscured its own operation by maintaining ambiguity until analysis was too late to change course.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are beneficiaries of the ambiguity: they gain authority and coalition-building capacity by maintaining it. Left and right coalitions are complex: they benefit from the ambiguity (it permits coalition-building) but are also harmed by it (it suppresses clarity about the incompatibility of their visions and exposes them to the other side's implementation choices they did not endorse). Targeted program recipients are straightforward victims: they lose prior benefits justified by universality, and the universality rhetoric prevents them from objecting (to object would be to accept the stigma the universality framing purports to eliminate). Ideological clarity is a non-agent victim: it is suppressed by the constraint's operation. The constraint's extraction comes primarily from suppressing clarity and from using universality to justify targeted-program displacement; it is not massive fiscal extraction because left and right implementations converge.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (political inability to build a coalition across ideological divides) remains live, which would normally support a coordination (rope) classification. However, the constraint's operation is tangled: it achieves coordination (coalition-building) through extraction (suppressing clarity about incompatibility and using universality to justify targeted-program replacement). A pure-rope classification would miss the suppression mechanism; a pure-snare would miss the genuine coordination function. Tangled rope captures both: the constraint coordinates political actors across ideological divides while extracting from transparency and from those whose benefits are displaced under universality-justified replacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_divergence_vs_convergence,
    'Do left-inflected and right-inflected UBI implementations actually converge on similar fiscal distributions (net redistribution, welfare replacement), or do design parameters that appear similar mask divergent outcomes?',
    'Post-implementation audit of actual taxation-back mechanisms, benefit adequacy, and distributional incidence across left-inflected and right-inflected pilots. If taxation-back mechanisms produce similar marginal tax rates and similar poverty-line replacement across designs, convergence is confirmed; if divergent, the constraint''s extractive function (suppressing clarity) yields to actual policy divergence.',
    'If convergence confirmed: the constraint''s political function (enabling coalition-building by deferring clarity) masks that ideological incompatibility has no fiscal consequence—the readings are merely rhetorical. If divergence: the constraint''s ambiguity obscures real distributional stakes, and the extraction is greater than measured (clarity suppression has material consequence).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_divergence_vs_convergence, empirical, 'Whether UBI implementations converge or diverge in distributional consequences').

omega_variable(
    ideological_reading_stability,
    'Once UBI is implemented with specific design choices (payment level, taxation mechanism, eligibility gates), do left and right coalitions continue to claim the policy advances their incompatible visions, or does clarity force ideological repositioning?',
    'Track coalition rhetoric before, during, and after implementation; measure whether both sides continue to claim victory, diverge in their claims, or reposition their visions. If both sides continue claiming victory despite divergent design, the constraint''s ambiguity has survived implementation; if reposition occurs, clarity has forced ideological coherence.',
    'If stability: the constraint''s extraction from ideological clarity persists post-implementation (ambiguity becomes institutional feature). If repositioning: the constraint''s function is defeated by implementation pressure, and post-hoc coalition-building requires new ambiguity mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_reading_stability, empirical, 'Whether ideological ambiguity persists through implementation').

omega_variable(
    targeted_program_adequacy_vs_replacement,
    'When UBI replaces targeted welfare under universality rhetoric, is the replacement dollar-for-dollar (similar total benefit adequacy), or does universality permit replacement at lower aggregate adequacy for the most vulnerable?',
    'Compare aggregate benefit adequacy (prior targeted benefits + new UBI payment) for program recipients before and after implementation; stratify by vulnerability (disability, family size, regional cost-of-living). If adequacy is preserved, universality replacement is neutral; if reduced, targeted recipients are victims of the constraint''s operation.',
    'If adequacy is preserved: the constraint''s extraction from targeted recipients is rhetorical (universality removes stigma, which may be valuable); if reduced, the constraint extracts material harm (lower benefit) justified by rhetorical universality. This determines whether the constraint''s victim class is real or rhetorical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(targeted_program_adequacy_vs_replacement, empirical, 'Whether UBI replacement preserves or reduces benefit adequacy for vulnerable groups').

omega_variable(
    ambiguity_as_negotiation_device_vs_deception,
    'Is the maintained ambiguity a genuinely negotiated feature of the policy (both sides knowingly accept universality as the compromise language to enable coalition-building), or a device by which one side deceives the other about what the policy will actually do?',
    'Examine negotiation records, legislative testimony, and design-process interviews from implementation phase. If both sides'' advocates acknowledge the incompatibility and deliberately choose universality as the compromise language, it is negotiated; if evidence shows one side believed the other''s interpretation while agreeing to the policy, it is deceptive.',
    'If negotiated: the constraint is primarily tangled rope (coordination + suppression, both parties complicit). If deceptive: the constraint is primarily snare for the deceived party (extraction disguised as coordination). This determines the moral character of the ambiguity and whether it should be sustained or clarified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ambiguity_as_negotiation_device_vs_deception, conceptual, 'Whether ambiguity is a negotiated compromise or a device of deception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(unco_tr_t0, observed).
narrative_ontology:measurement(unco_tr_t5, unconditional_income_support__universality_paradox_reading, theater_ratio, 5, 0.56).
narrative_ontology:measurement_basis(unco_tr_t5, observed).
narrative_ontology:measurement(unco_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.63).
narrative_ontology:measurement_basis(unco_tr_t10, observed).
narrative_ontology:measurement(unco_tr_t15, unconditional_income_support__universality_paradox_reading, theater_ratio, 15, 0.67).
narrative_ontology:measurement_basis(unco_tr_t15, observed).
narrative_ontology:measurement(unco_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.68).
narrative_ontology:measurement_basis(unco_tr_t20, observed).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__universality_paradox_reading, theater_ratio, 25, 0.68).
narrative_ontology:measurement_basis(unco_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(unco_be_t0, observed).
narrative_ontology:measurement(unco_be_t5, unconditional_income_support__universality_paradox_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement_basis(unco_be_t5, observed).
narrative_ontology:measurement(unco_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(unco_be_t10, observed).
narrative_ontology:measurement(unco_be_t15, unconditional_income_support__universality_paradox_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(unco_be_t15, observed).
narrative_ontology:measurement(unco_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(unco_be_t20, observed).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__universality_paradox_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(unco_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(unco_su_t0, observed).
narrative_ontology:measurement(unco_su_t5, unconditional_income_support__universality_paradox_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement_basis(unco_su_t5, observed).
narrative_ontology:measurement(unco_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(unco_su_t10, observed).
narrative_ontology:measurement(unco_su_t15, unconditional_income_support__universality_paradox_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(unco_su_t15, observed).
narrative_ontology:measurement(unco_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(unco_su_t20, observed).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__universality_paradox_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement_basis(unco_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__universality_paradox_reading, 0.12).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'unconditional_income_support'. The kernel is the persisting commitment (enacting UBI in a particular jurisdiction) that different parties read through incompatible normative and empirical lenses. The universality_paradox_reading examines how the kernel's ambiguity functions as a political device enabling coalition-building across incompatible visions. The freedom_floor_reading emphasizes labor decommodification and autonomy; the dependency_trap_reading emphasizes incentive distortion and upward redistribution. These are not alternative measurements of the same constraint but distinct readings of a contested commitment, each with its own normative premises and political stakes. The reading_relations and axioms below map the structural relationships between readings without privileging any one as the 'true' reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__universality_paradox_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
