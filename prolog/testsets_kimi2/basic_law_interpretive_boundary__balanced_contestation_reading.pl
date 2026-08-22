% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Basic Law Interpretive Boundary - Balanced Contestation Reading
 *   domain: constitutional law / comparative constitutionalism / judicial review theory
 *
 * SUMMARY:
 *   This constraint story models the balanced contestation reading of the
 *   Israeli Basic Law interpretive boundary, wherein the Supreme Court and
 *   the Knesset each hold legitimate but bounded constitutional authority.
 *   The Court interprets Basic Laws within its jurisdictional domain; the
 *   legislature retains ultimate sovereign power but is constrained by
 *   international obligations and norms of judicial independence. Neither
 *   institution is fully dominant, producing an institutional dialogue regime
 *   characterized by triadic negotiation among court, executive, and
 *   legislature over the enforcement of constitutional boundaries. The
 *   reading treats this arrangement as generating genuine coordination
 *   (mutual checks against concentrated power) alongside asymmetric
 *   extraction (governance uncertainty and delayed rights resolution falling
 *   on citizens and the executive).
 *
 * KEY AGENTS:
 *   - supreme_court: Primary agenda-setter (institutional/constrained) â interprets Basic Laws, asserts judicial review authority, benefits from bounded autonomy
 *   - knesset: Primary agenda-setter (institutional/constrained) â legislates and amends Basic Laws, retains sovereign flexibility within international and judicial constraints
 *   - executive_branch: Primary target (powerful/constrained) â navigates triadic negotiations, bears costs of institutional deadlock and conflicting mandates
 *   - citizens_awaiting_resolution: Secondary target (powerless/constrained) â bear costs of prolonged constitutional uncertainty and delayed remedies
 *   - constitutional_scholars: Analytical observer â document and theorize the inter-institutional balance
 *   - international_human_rights_bodies: External observer (institutional/analytical) â monitor norms of judicial independence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Basic Law Interpretive Boundary - Balanced Contestation Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional law / comparative constitutionalism / judicial review theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, '47669b54-13d8-47ea-9d54-798c574e5551').
narrative_ontology:cs_kernel_codification('47669b54-13d8-47ea-9d54-798c574e5551', formalized).
narrative_ontology:cs_authority_grounding('47669b54-13d8-47ea-9d54-798c574e5551', distributed).
narrative_ontology:cs_reading_relation('47669b54-13d8-47ea-9d54-798c574e5551', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('47669b54-13d8-47ea-9d54-798c574e5551', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('47669b54-13d8-47ea-9d54-798c574e5551', foundational, bounded_reciprocal_authority).
narrative_ontology:cs_axiom_status(bounded_reciprocal_authority, holdable).
narrative_ontology:cs_axiom_grounding('47669b54-13d8-47ea-9d54-798c574e5551', bounded_reciprocal_authority, conventional).
narrative_ontology:cs_axiom('47669b54-13d8-47ea-9d54-798c574e5551', foundational, inter_institutional_dialogue_norm).
narrative_ontology:cs_axiom_status(inter_institutional_dialogue_norm, holdable).
narrative_ontology:cs_axiom_grounding('47669b54-13d8-47ea-9d54-798c574e5551', inter_institutional_dialogue_norm, conventional).
narrative_ontology:cs_reference_frame('47669b54-13d8-47ea-9d54-798c574e5551', dialogic_constitutionalism).
narrative_ontology:cs_drift_state('47669b54-13d8-47ea-9d54-798c574e5551', post_2023_judicial_overhaul_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('47669b54-13d8-47ea-9d54-798c574e5551', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, knesset).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, citizens_awaiting_resolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws within its jurisdictional domain, asserts authority to review legislation for constitutionality, and benefits from norms of judicial independence that preserve its institutional autonomy. Cannot unilaterally resolve constitutional crises without legislative or executive cooperation, and its authority is bounded by the Knesset's sovereign amending power.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, beneficiary).

% Retains ultimate sovereign lawmaking power and the capacity to amend Basic Laws, operating within constraints of international obligations and norms of judicial independence. Benefits from flexibility to respond to political majorities while facing judicial review that checks transient legislative excess.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, knesset, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, knesset, beneficiary).

% Navigates triadic negotiations between judicial mandates and legislative directives, bearing costs of institutional paralysis when the court and Knesset deadlock. Implements policy under conditions of conflicting authoritative claims and uncertain constitutional finality.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Seek final resolution on rights claims, policy challenges, and legal status. Bear costs of prolonged constitutional uncertainty, delayed judicial remedies, and shifting legal landscapes as the court and legislature contest interpretive boundaries without hierarchical finality.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, citizens_awaiting_resolution, payer,
    powerless, biographical, constrained, national).

% Analyze and theorize the inter-institutional dialogue, documenting shifts in the balance between judicial and legislative authority. Their frameworks influence constitutional discourse but do not determine institutional practice.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% Monitor compliance with norms of judicial independence and international treaty obligations, exerting soft pressure on domestic institutions. Shape legitimacy conditions for the balanced arrangement without directly enforcing the constitutional boundary.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, diffuse).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents concentration of constitutional authority in a single institution by establishing a contested but stable boundary between judicial interpretation and legislative sovereignty, enabling inter-institutional dialogue and mutual checks against tyranny by either unelected judiciary or transient majorities.
% TRANSFER_FUNCTION: Transfers decisive constitutional finality from citizens and the executive to the ongoing contestation between court and legislature; transfers institutional autonomy and legitimacy to both the judiciary and the Knesset at the cost of governance certainty and prompt rights resolution.
% ABSENT_VOICES: Radical constitutional reformers seeking clear hierarchical resolutionâwhether full judicial supremacy or absolute parliamentary sovereigntyâare structurally marginalized by the equilibrium. Ordinary citizens without sustained organizational backing lack voice in the inter-institutional negotiations that determine the scope of their rights.
% DISAPPEARANCE_RATIONALE: If the balanced contestation vanished overnightâreplaced by clear judicial supremacy or legislative sovereigntyâthe constitutional order would reorganize: rights claims would resolve through a single authoritative channel, executive planning would face fewer conflicting mandates, and the current dual-legitimacy equilibrium would collapse into hierarchical finality.
% FOUNDING_PROBLEM: Prevention of tyranny by either unelected judicial oligarchy or transient legislative majorities, and the need for constitutional continuity without a single ultimate sovereign in a divided society lacking a formal comprehensive constitutional settlement.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and comparative constitutional law experts attest that the problem of concentrated power remains live, though they dispute whether the balanced contestation arrangement effectively addresses it. The Knesset majority increasingly asserts the problem is dead and the arrangement has become judicial overreach; the Supreme Court maintains the problem is live. Independent academic analysis outside both institutions supports the continued risk of majority tyranny.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: the balanced regime genuinely prevents tyranny but extracts governance certainty from citizens and the executive. Suppression (0.58) reflects the active suppression of hierarchical resolution alternativesâboth judicial supremacy and parliamentary sovereignty are held at bay by the equilibrium. Theater ratio (0.45) captures the increasing performative dimension of institutional dialogue as political polarization grows. Accessibility collapse (0.70) is high because exit from this contested equilibrium to a clear constitutional hierarchy requires either revolutionary political change or comprehensive constitutional settlement, neither of which is presently achievable. Resistance (0.55) is moderate and bidirectional: the Court resists legislative override, and the legislature resists judicial invalidation. Temporal measurements show gradual intensification of all three tracked metrics as the constitutional revolution matured and political contestation over the Basic Laws escalated from the 1990s through the 2020s.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's seat, the constraint is a necessary safeguard of rights and constitutional continuity against majoritarian overreach; from the Knesset majority's seat, it is a legitimate expression of democratic sovereignty constrained by external norms. From the executive and citizen seats, the same structure produces friction, delay, and uncertainty. The engine computes this divergence from the structural data: both agenda-setting institutions enjoy low directionality (beneficiaries of the equilibrium) while executive and citizen payers sit at high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court and Knesset are declared beneficiaries because the balanced arrangement preserves their institutional autonomy and legitimacy. The executive branch and citizens awaiting resolution are declared victims (payers) because they bear the costs of institutional contestationâdelayed governance, uncertain rights, and triadic negotiation overhead. Neither agenda-setter fully captures the extraction; the gains of the constraint (checks and balances) are institutional-public goods, while the costs are concentrated on those requiring finality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing tyranny by either branchâremains contested. The Knesset majority increasingly claims the problem is solved and the arrangement has become judicial overreach (mandatrophy risk). However, independent corroboration from constitutional scholars and comparative constitutional evidence suggests the risk of concentrated power remains live. The constraint is not a piton because both institutions actively benefit and maintain it; it is not a snare because the coordination function is genuine and structurally separable from the extraction; it is not a scaffold because it carries no sunset clause. The temporal measurements show rising theater and extraction, indicating potential future mandatrophy if the dialogic function collapses into pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_balance,
    'Is the balanced contestation an inherent feature of Israeli constitutional design or an unstable temporary equilibrium produced by political deadlock?',
    'Comparative constitutional analysis of whether similar unwritten or divided constitutional orders stabilize into enduring dialogue regimes or collapse into hierarchical authority.',
    'If temporary equilibrium, reclassify toward scaffold or piton; if structurally inherent, validate tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_balance, conceptual, 'Whether the balanced contestation is stable constitutional architecture or transient deadlock').

omega_variable(
    extraction_vs_democratic_cost,
    'Do the costs borne by citizens and the executive represent extractive overhead of the balanced arrangement or necessary democratic friction inherent to constitutionalism?',
    'Cross-national comparison of governance efficiency, rights protection latency, and policy finality rates under balanced contestation regimes versus hierarchical constitutional models.',
    'If overhead exceeds comparative democratic baselines, reclassify toward snare; if within normal range, validate rope or tangled_rope coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_democratic_cost, empirical, 'Whether citizen and executive costs are extraction or normal democratic friction').

omega_variable(
    sibling_reading_boundary,
    'The sibling readings frame the same kernel as hierarchical rather than dialogic. Does the balanced contestation reading capture the operative constitutional structure, or is it a normative aspiration masking de facto movement toward one of the hierarchical poles?',
    'Empirical analysis of legislative override success rates, judicial invalidation rates, executive compliance patterns, and constitutional amendment frequency over the measurement interval.',
    'If operational practice trends toward one pole, the balanced reading''s epsilon may understate actual extraction by the dominant institution; if genuinely oscillating, the dialogic framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_boundary, empirical, 'Whether the balanced reading describes operative structure or aspirational masking').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(basi_tr_t6, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(basi_tr_t12, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(basi_tr_t18, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 24, 0.42).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(basi_be_t6, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(basi_be_t12, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(basi_be_t18, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 18, 0.45).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(basi_su_t6, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(basi_su_t12, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(basi_su_t18, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the basic_law_interpretive_boundary kernel. The kernel decomposes into three structurally distinct readingsâbalanced contestation, judicial supremacy, and parliamentary sovereigntyâbecause each assigns different epsilon values, beneficiary/victim structures, and directionalities to the same Basic Law textual framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
