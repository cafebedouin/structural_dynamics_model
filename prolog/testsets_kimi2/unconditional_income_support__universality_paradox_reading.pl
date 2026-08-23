% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Unconditional Income Support (Universality Paradox Reading)
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   Unconditional income support operates in this reading as a structurally
 *   ambiguous commitment device. Cross-ideological appealâleft-progressive
 *   redistribution, libertarian autonomy, technocratic simplificationâis
 *   enabled by under-specification of financing and targeting. Policy
 *   designers exploit taxing-back mechanisms to make net fiscal outcomes
 *   equivalent across radically different rhetorical frames. The constraint
 *   is the ambiguity itself: it coordinates a coalition that would otherwise
 *   fracture, while extracting clarity and targeted-program integrity from
 *   the policy space. Political entrepreneurs harvest coalition surplus;
 *   targeted program recipients bear the cost of reframing. This is a kernel
 *   reading: the same policy label instantiates a different constraint when
 *   viewed through the universality-paradox lens than through the
 *   freedom-floor or dependency-trap lenses.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: Primary beneficiary (powerful/mobile) â harvest political capital from ambiguous framing
 *   - policy_designers: Secondary beneficiary (moderate/constrained) â gain design flexibility from taxing-back mechanisms
 *   - targeted_program_recipients: Primary target (powerless/trapped) â lose in-kind protections to universal cash that may be net-reduced after taxation
 *   - welfare_state_analysts: Analytical observer (analytical/analytical) â document fiscal equivalence across designs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.32).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.55).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support (Universality Paradox Reading)").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, 'e380f0c4-b2d8-41a2-a04b-05f8b75a8734').
narrative_ontology:cs_kernel_codification('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', distributed).
narrative_ontology:cs_authority_grounding('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', distributed).
narrative_ontology:cs_reading_relation('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', unconditional_income_support__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', unconditional_income_support__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', foundational, universal_designs_converge_to_targeted_outcomes).
narrative_ontology:cs_axiom_status(universal_designs_converge_to_targeted_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', universal_designs_converge_to_targeted_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', foundational, ambiguity_structurally_necessary_for_cross_ideological_endorsement).
narrative_ontology:cs_axiom_status(ambiguity_structurally_necessary_for_cross_ideological_endorsement, holdable).
narrative_ontology:cs_axiom_grounding('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', ambiguity_structurally_necessary_for_cross_ideological_endorsement, instrumental).
narrative_ontology:cs_reference_frame('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', cross_ideological_ambiguity_frame).
narrative_ontology:cs_drift_state('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', post_taxing_back_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e380f0c4-b2d8-41a2-a04b-05f8b75a8734', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploit the polysemic nature of unconditional income support to assemble coalitions that would otherwise fracture on first principles. They benefit from the political capital of championing a popular-sounding universal program while deferring specification of financing and targeting that would alienate factions. They can exit the coalition if the ambiguity resolves against their interests.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from the design flexibility that taxing-back mechanisms provide under a universal cash framework. They can calibrate net transfers while preserving the universal label, allowing rhetorical claims of inclusion alongside fiscal precision. Their professional capital depends on maintaining the technical viability of the ambiguous frame.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    moderate, biographical, constrained, national).

% Bear the cost when universal programs replace targeted in-kind benefits such as housing, disability support, and nutrition assistance with cash transfers that may be net-reduced after taxing-back. They lack the political organization to block the reframing and depend on program-specific advocacy that the universalism narrative marginalizes as bureaucratic fragmentation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, national).

% Would argue that targeted programs deliver better outcomes for the poorest at lower fiscal cost, but are excluded from the coalition because their specificity breaks the cross-ideological ambiguity. Their voice is suppressed by the universalism frame's claim to be simpler and more inclusive.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, means_tested_advocates, excluded,
    moderate, biographical, constrained, national).

% Observe that net fiscal outcomes under competing unconditional-income designs converge when taxes are considered, yet the political packaging diverges sharply. They note the coalition is held together by label-sharing rather than by substantive policy agreement.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, welfare_state_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, diffuse).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a cross-ideological coalition-building problem by allowing parties with incompatible normative commitments to endorse the same policy label while imagining different implementation paths.
% TRANSFER_FUNCTION: Moves political capital and policy legitimacy from targeted program frameworks to universal-cash frameworks, while actual fiscal benefits move upward via taxing-back mechanisms that net out to targeted distributions anyway.
% ABSENT_VOICES: Targeted welfare advocates who would defend means-tested programs are sidelined by the universalism frame; libertarian purists who would reject taxing-back are ignored because the freedom-floor branding obscures the fiscal clawback.
% DISAPPEARANCE_RATIONALE: Without the ambiguity, the cross-ideological coalition could not hold: left proponents would demand higher net transfers without clawbacks, right proponents would resist any net redistribution, and the policy vehicle would collapse into explicit partisan contestation.
% FOUNDING_PROBLEM: How to build durable political support for income support in a polarized environment where left and right factions have incompatible visions of welfare state legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists studying welfare state coalition formation attest the problem is live. Fiscal economists and microsimulation researchers outside the benefiting political class corroborate that net outcomes converge under taxing-back, supporting the claim that the coordination problem has been subordinated to distributional obfuscation.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.32, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored low (0.32) because taxing-back research shows net fiscal outcomes converge across universal and targeted designs; the material extraction is modest but nonzero for targeted recipients who lose program-specific protections. Suppression (0.55) is structural and rhetorical: the ambiguity suppresses coherent evaluation by fragmenting opposition across incompatible projections. Theater ratio (0.60) is high because the cross-ideological appeal is performativeâeach faction projects its own meaning onto the same policy label while the fiscal core remains invariant. Accessibility collapse (0.55) reflects that once the taxing-back mechanism is understood, the 'universality' framing collapses as a distinct distributional offer, but the political vehicle has already moved. Resistance (0.40) is moderate and fragmented: targeted-welfare advocates resist, but their specificity is branded as bureaucratic obstruction.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (political entrepreneurs, policy designers) experience the constraint as a coordination successâa coalition held together by productive ambiguity. The payer seat (targeted program recipients) experiences the same structure as extraction: their programmatic protections are dissolved into a universal cash frame that nets out to less support after taxation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Political entrepreneurs and policy designers are declared beneficiaries: they collect political and professional surplus from the ambiguous frame, giving them low directionality and damping effective extraction. Targeted program recipients are declared victims (role: payer): they bear the cost of program consolidation and potential net benefit reduction, giving them high directionality and amplifying effective extraction. Welfare state analysts are observers with analytical exit: their directionality is neutral. The engine derives these positions from the beneficiary/victim declarations combined with exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this constraint would be misread either as a rope (pure coordination of a difficult cross-ideological coalition) or as a snare (cynical extraction by political elites). The tangled_rope gate captures that the coordination is genuineâthe coalition would indeed fracture without ambiguityâwhile the asymmetric extraction is equally real: targeted recipients pay for the coalition's stability with lost program integrity. The mandatrophy flag is not triggered because the founding problem (coalition-building under polarization) remains live, but the arrangement's function has shifted from solving the problem to obscuring distributional conflict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_equivalence_ambiguity,
    'If fiscal outcomes are equivalent across universal and targeted designs due to taxing-back, does the universal framing constitute a distinct constraint or merely a rhetorical wrapper around a targeted transfer?',
    'Comparative policy evaluation tracking net household disposable income before and after reform across universal and targeted regimes, controlling for labor supply responses.',
    'If the universal frame produces no distinct fiscal outcome, the extraction is primarily political (clarity and accountability) rather than material, supporting a lower epsilon; if net outcomes differ materially for subgroups, extraction is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_equivalence_ambiguity, empirical, 'Whether universal framing is a distinct constraint or rhetorical wrapper').

omega_variable(
    political_entrepreneur_extraction,
    'Do political entrepreneurs extract surplus from the ambiguity itself, or are they genuine coordinators of an otherwise impossible coalition?',
    'Process-tracing of coalition negotiations and public position-taking: do entrepreneurs actively suppress specification to maintain support, or merely broker good-faith disagreement?',
    'If the suppression of specification is strategic, the constraint tilts toward snare; if it is a good-faith response to coalition heterogeneity, it remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_entrepreneur_extraction, conceptual, 'Whether political surplus extraction is strategic or coordinative').

omega_variable(
    targeted_program_net_loss,
    'Are targeted program recipients net losers under universal designs, or does the universal floor plus taxing-back leave them materially indifferent while altering administrative form?',
    'Microsimulation of specific UBI proposals with full tax integration, comparing net disposable income for deciles and program-specific populations.',
    'If recipients are materially indifferent, the victim set shrinks and the constraint weakens toward rope; if net losses are concentrated, extraction is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeted_program_net_loss, empirical, 'Material impact of universal replacement on targeted recipients').

omega_variable(
    kernel_reading_boundary,
    'This reading treats policy ambiguity as the constraint; the freedom_floor reading treats autonomy as the constraint; the dependency_trap reading treats incentive distortion as the constraint. Which structural feature is the true referent of epsilon?',
    'Corpus-level comparison of the three sibling constraints: if their epsilon values diverge widely, the epsilon-invariance principle is validated and the kernel decomposition is structurally sound.',
    'Confirms that the kernel reading decomposition produced three distinct constraints rather than one constraint with measurement-dependent epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Epsilon referent ambiguity across kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(unco_tr_t8, unconditional_income_support__universality_paradox_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(unco_tr_t16, unconditional_income_support__universality_paradox_reading, theater_ratio, 16, 0.5).
narrative_ontology:measurement(unco_tr_t24, unconditional_income_support__universality_paradox_reading, theater_ratio, 24, 0.58).
narrative_ontology:measurement(unco_tr_t32, unconditional_income_support__universality_paradox_reading, theater_ratio, 32, 0.62).
narrative_ontology:measurement(unco_tr_t40, unconditional_income_support__universality_paradox_reading, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(unco_be_t8, unconditional_income_support__universality_paradox_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(unco_be_t16, unconditional_income_support__universality_paradox_reading, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(unco_be_t24, unconditional_income_support__universality_paradox_reading, base_extractiveness, 24, 0.28).
narrative_ontology:measurement(unco_be_t32, unconditional_income_support__universality_paradox_reading, base_extractiveness, 32, 0.3).
narrative_ontology:measurement(unco_be_t40, unconditional_income_support__universality_paradox_reading, base_extractiveness, 40, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(unco_su_t8, unconditional_income_support__universality_paradox_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(unco_su_t16, unconditional_income_support__universality_paradox_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement(unco_su_t24, unconditional_income_support__universality_paradox_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(unco_su_t32, unconditional_income_support__universality_paradox_reading, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(unco_su_t40, unconditional_income_support__universality_paradox_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is the universality_paradox_reading of the unconditional_income_support kernel, decomposed per the epsilon-invariance principle from sibling readings freedom_floor_reading and dependency_trap_reading because the epsilon referent (ambiguity as Trojan horse) differs structurally from the epsilon referents of the sibling readings (autonomy floor vs. incentive trap).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
