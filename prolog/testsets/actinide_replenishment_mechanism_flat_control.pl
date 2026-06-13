% ============================================================================
% CONSTRAINT STORY: actinide_replenishment_mechanism_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_actinide_replenishment_mechanism_flat_control, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: actinide_replenishment_mechanism_flat_control
 *   human_readable: Actinide Replenishment Mechanism in Przybylski's Star
 *   domain: astrophysics/nuclear_physics/stellar_spectroscopy
 *
 * SUMMARY:
 *   Przybylski's Star (HD 101065) is a chemically peculiar star whose
 *   atmosphere contains spectroscopic signatures of short-lived radioactive
 *   actinides including promethium-145 (half-life 17.7 years), americium,
 *   curium, berkelium, californium, and einsteinium. Given the star's age of
 *   approximately 1.5 billion years, these isotopes should have decayed
 *   completely unless continuously replenished. The physical mechanism
 *   responsible for this replenishment is unknown. Proposed explanations
 *   include ongoing neutron-capture nucleosynthesis in the stellar
 *   atmosphere, accretion from a neutron-rich companion or debris disk, or
 *   exotic nuclear reactions enabled by the star's strong magnetic field. No
 *   mechanism has achieved consensus. The constraint is the physical
 *   requirement that some process must be supplying these isotopes faster
 *   than they decay. KEY AGENTS (by structural relationship): - Nuclear
 *   astrophysics research programs: Beneficiary (organized/mobile) — gain
 *   research opportunities and empirical constraints from the phenomenon -
 *   Stellar spectroscopy observational campaigns: Beneficiary
 *   (organized/mobile) — gain telescope time justification and technical
 *   development drivers - Standard stellar evolution theorists: Observer
 *   (organized/analytical) — document the anomaly without bearing costs -
 *   Alternative nucleosynthesis theorists: Observer (moderate/mobile) —
 *   compete to explain the mechanism - Observational astronomers (general):
 *   Observer (organized/arbitrage) — can choose to study this target or
 *   others
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(actinide_replenishment_mechanism_flat_control, 0.03).
domain_priors:suppression_score(actinide_replenishment_mechanism_flat_control, 0.02).
domain_priors:theater_ratio(actinide_replenishment_mechanism_flat_control, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, extractiveness, 0.03).
narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(actinide_replenishment_mechanism_flat_control, mountain).
narrative_ontology:human_readable(actinide_replenishment_mechanism_flat_control, "Actinide Replenishment Mechanism in Przybylski's Star").
narrative_ontology:topic_domain(actinide_replenishment_mechanism_flat_control, "astrophysics/nuclear_physics/stellar_spectroscopy").

domain_priors:emerges_naturally(actinide_replenishment_mechanism_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(actinide_replenishment_mechanism_flat_control, actinide_replenishment_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(actinide_replenishment_mechanism_flat_control, nuclear_astrophysics_research_programs).
narrative_ontology:constraint_beneficiary(actinide_replenishment_mechanism_flat_control, stellar_spectroscopy_observational_campaigns).
narrative_ontology:constraint_vindicates(actinide_replenishment_mechanism_flat_control, exotic_nucleosynthesis_pathway_existence).
narrative_ontology:constraint_vindicates(actinide_replenishment_mechanism_flat_control, stellar_atmosphere_nuclear_process_diversity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the existence of an observable natural laboratory for exotic nuclear processes. The star's actinide signatures provide empirical constraints on r-process nucleosynthesis models, neutron-capture cross-sections, and stellar mixing theories that would otherwise remain purely theoretical. Research funding and publication opportunities flow from the puzzle's persistence.
narrative_ontology:constraint_stakeholder(actinide_replenishment_mechanism_flat_control, nuclear_astrophysics_research_programs, beneficiary,
    organized, generational, mobile, global).

% Benefit from having a unique spectroscopic target that justifies telescope time allocation and instrument development. The star's anomalous spectrum drives technical innovation in high-resolution spectroscopy and abundance analysis methods. The observational puzzle sustains careers and infrastructure investment.
narrative_ontology:constraint_stakeholder(actinide_replenishment_mechanism_flat_control, stellar_spectroscopy_observational_campaigns, beneficiary,
    organized, biographical, mobile, global).

% Observe the constraint as an anomaly that standard stellar evolution models cannot explain. The star's actinide content violates expectations from canonical nucleosynthesis and mixing timescales. They document the discrepancy but do not bear costs from its existence.
narrative_ontology:constraint_stakeholder(actinide_replenishment_mechanism_flat_control, standard_stellar_evolution_theorists, observer,
    organized, generational, analytical, global).

% Propose competing mechanisms: ongoing neutron-capture in the stellar atmosphere, accretion from a companion object, or exotic nuclear reactions in strong magnetic fields. Each proposed mechanism is testable but none has achieved consensus. They compete for explanatory priority but the physical process itself is indifferent to their theories.
narrative_ontology:constraint_stakeholder(actinide_replenishment_mechanism_flat_control, alternative_nucleosynthesis_theorists, observer,
    moderate, biographical, mobile, global).

% Can allocate telescope time to this target or to other science questions. The star's uniqueness makes it a compelling target but not a mandatory one. They experience the constraint as an interesting option in the target selection landscape.
narrative_ontology:constraint_stakeholder(actinide_replenishment_mechanism_flat_control, observational_astronomers_general, observer,
    organized, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The physical mechanism operates independently of any human coordination. The research community coordinates around studying the phenomenon, but that coordination is downstream of the constraint, not constitutive of it.
% TRANSFER_FUNCTION: No transfer between human agents. The mechanism transfers nuclear species from some production site (stellar interior, accreted material, or in-situ atmospheric reactions) to the observable photosphere, but this is a physical process, not a social arrangement.
% ABSENT_VOICES: No human voices are structurally excluded. The physical mechanism is accessible to any observer with appropriate instrumentation. Theoretical interpretations compete openly in the literature.
% DISAPPEARANCE_RATIONALE: If the replenishment mechanism ceased tomorrow, the actinide signatures would decay on timescales from years to millennia, the star would become spectroscopically normal, and the research puzzle would dissolve. But no human social arrangement depends on the mechanism's operation. Research programs would redirect to other targets; no institutional structure would collapse.
% FOUNDING_PROBLEM: Not applicable. The mechanism was not built to solve a problem. It is a physical process that exists independently of human purposes. The research interest it generates is an effect, not a cause.
% FOUNDING_PROBLEM_CORROBORATION: The physical puzzle remains unsolved. Multiple independent spectroscopic observations confirm the actinide presence; no proposed mechanism has achieved consensus acceptance. Corroboration comes from observational data published across decades by independent research groups with no stake in any particular theoretical resolution.
narrative_ontology:disappearance_verdict(actinide_replenishment_mechanism_flat_control, world_unchanged).
narrative_ontology:founding_problem_status(actinide_replenishment_mechanism_flat_control, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(actinide_replenishment_mechanism_flat_control, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(actinide_replenishment_mechanism_flat_control, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(actinide_replenishment_mechanism_flat_control_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, ExtMetricName, E),
    domain_priors:suppression_score(actinide_replenishment_mechanism_flat_control, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(actinide_replenishment_mechanism_flat_control),
    narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(actinide_replenishment_mechanism_flat_control, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(actinide_replenishment_mechanism_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.03) because the physical mechanism operates independently of human agency and imposes no costs on any party. The slight non-zero value reflects the opportunity cost of telescope time and research effort directed at this puzzle rather than other questions, but this is voluntary allocation, not extraction. Suppression is near-zero (0.02) because no alternatives are foreclosed: researchers can study other stars, propose alternative theories, or ignore the phenomenon entirely. The mechanism does not coerce participation. Theater ratio is very low (0.08) and slowly rising: the slight theatrical component reflects the accumulation of speculative theoretical papers that propose mechanisms without definitive tests, but the core observational and spectroscopic work remains functional. Accessibility collapse is very high (0.92) because once the actinide signatures are confirmed and the decay timescales understood, the physical necessity of a replenishment mechanism becomes inescapable — no alternative framework makes the observations go away. Resistance is very low (0.04) because the constraint is a physical fact that does not require defense; the only 'resistance' is the normal scientific process of checking observations and questioning interpretations, which is epistemic diligence rather than opposition to the constraint itself.
 *
 * PERSPECTIVAL GAP:
 *   All seats should compute as Mountain. The physical mechanism is a natural process independent of human framing. Beneficiaries gain research opportunities from its existence, but this does not make the mechanism itself extractive — the gains flow from studying a natural phenomenon, not from the phenomenon extracting from anyone. The constraint is the same physical requirement regardless of who observes it or benefits from studying it.
 *
 * DIRECTIONALITY LOGIC:
 *   Research programs and observational campaigns are beneficiaries: they gain funding, publication opportunities, and infrastructure justification from the phenomenon's existence, placing them at the low-d (beneficiary) end. Standard evolution theorists and alternative theorists are observers: they study the constraint but neither benefit asymmetrically nor bear costs from its operation, placing them near d=0.5 (symmetric/analytical). General observational astronomers have arbitrage-grade exit: they can allocate resources to this target or not, experiencing it as an option rather than an obligation. No agent is trapped or identity-locked to this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a genuine natural law constraint with identifiable beneficiaries, making it a candidate for false summit detection. The beneficiaries (research programs, observational campaigns) gain from the phenomenon's existence, but they did not construct the mechanism and cannot modify it. The constraint would persist even if all current beneficiaries disappeared. The FSM machinery should evaluate whether the research community's framing of the problem as 'exotic nucleosynthesis' versus 'standard processes in unusual conditions' serves to maintain research funding streams. However, the physical mechanism itself — the requirement that short-lived isotopes be continuously replenished — is observer-independent. The omega variables document the irreducible uncertainty about which specific physical process is responsible, but all proposed mechanisms are natural processes, not constructed constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_identity,
    'Which physical process is responsible for replenishing the actinides: in-situ neutron capture, accretion from a companion, or exotic magnetic-field-enabled reactions?',
    'High-resolution time-series spectroscopy to detect variability patterns, direct imaging to search for companions, or laboratory measurements of neutron-capture cross-sections under stellar conditions. Definitive resolution likely requires multiple independent lines of evidence.',
    'Identifying the mechanism would determine whether the constraint represents standard nucleosynthesis in an unusual environment (Mountain with lower research novelty value) or a genuinely new nuclear process (Mountain with higher research value and broader implications for r-process theory). The classification remains Mountain regardless, but the mechanism''s identity affects which research programs benefit most.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mechanism_identity, empirical, 'Which of several competing natural mechanisms is responsible').

omega_variable(
    false_summit_research_framing,
    'Does the research community''s framing of this phenomenon as ''exotic'' versus ''standard processes in unusual conditions'' serve to maintain funding streams and publication opportunities that would diminish if the mechanism were resolved as mundane?',
    'Sociological analysis of research funding patterns, citation networks, and theoretical paper production rates before and after potential mechanism resolution. Compare to historical cases where astrophysical anomalies were resolved as standard physics in unusual regimes.',
    'If the ''exotic'' framing is sustained by beneficiary interests rather than by the data, the research community''s treatment of the constraint would be a false summit — a natural phenomenon presented as more mysterious than warranted to justify continued resource allocation. This would not change the physical mechanism''s classification (still Mountain) but would reclassify the research community''s discourse about it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_research_framing, conceptual, 'Whether beneficiary framing amplifies the phenomenon''s apparent exoticism').

omega_variable(
    observational_completeness,
    'Are current spectroscopic observations sufficient to distinguish between proposed mechanisms, or does the ambiguity persist because definitive observations have not been funded/executed?',
    'Technical feasibility study: what observations would definitively test each mechanism, what telescope time and instrument capabilities are required, and whether such observations have been proposed and declined versus never attempted.',
    'If the ambiguity is sustained by incomplete observations rather than by genuine physical degeneracy, the constraint''s persistence as an open question reflects resource allocation decisions (which observations to fund) rather than irreducible physical complexity. This affects the theater_ratio: higher if the puzzle persists due to under-observation, lower if the data genuinely cannot yet distinguish mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_completeness, empirical, 'Whether the mechanism remains unknown due to physical degeneracy or observational gaps').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(actinide_replenishment_mechanism_flat_control, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acti_tr_t0, actinide_replenishment_mechanism_flat_control, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(acti_tr_t0, observed).
narrative_ontology:measurement(acti_tr_t10, actinide_replenishment_mechanism_flat_control, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(acti_tr_t10, observed).
narrative_ontology:measurement(acti_tr_t20, actinide_replenishment_mechanism_flat_control, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(acti_tr_t20, observed).
narrative_ontology:measurement(acti_tr_t30, actinide_replenishment_mechanism_flat_control, theater_ratio, 30, 0.07).
narrative_ontology:measurement_basis(acti_tr_t30, observed).
narrative_ontology:measurement(acti_tr_t40, actinide_replenishment_mechanism_flat_control, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(acti_tr_t40, observed).
narrative_ontology:measurement(acti_tr_t50, actinide_replenishment_mechanism_flat_control, theater_ratio, 50, 0.08).
narrative_ontology:measurement_basis(acti_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(acti_be_t0, actinide_replenishment_mechanism_flat_control, base_extractiveness, 0, 0.03).
narrative_ontology:measurement_basis(acti_be_t0, observed).
narrative_ontology:measurement(acti_be_t10, actinide_replenishment_mechanism_flat_control, base_extractiveness, 10, 0.03).
narrative_ontology:measurement_basis(acti_be_t10, observed).
narrative_ontology:measurement(acti_be_t20, actinide_replenishment_mechanism_flat_control, base_extractiveness, 20, 0.03).
narrative_ontology:measurement_basis(acti_be_t20, observed).
narrative_ontology:measurement(acti_be_t30, actinide_replenishment_mechanism_flat_control, base_extractiveness, 30, 0.03).
narrative_ontology:measurement_basis(acti_be_t30, observed).
narrative_ontology:measurement(acti_be_t40, actinide_replenishment_mechanism_flat_control, base_extractiveness, 40, 0.03).
narrative_ontology:measurement_basis(acti_be_t40, observed).
narrative_ontology:measurement(acti_be_t50, actinide_replenishment_mechanism_flat_control, base_extractiveness, 50, 0.03).
narrative_ontology:measurement_basis(acti_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(acti_su_t0, actinide_replenishment_mechanism_flat_control, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(acti_su_t0, observed).
narrative_ontology:measurement(acti_su_t10, actinide_replenishment_mechanism_flat_control, suppression_requirement, 10, 0.02).
narrative_ontology:measurement_basis(acti_su_t10, observed).
narrative_ontology:measurement(acti_su_t20, actinide_replenishment_mechanism_flat_control, suppression_requirement, 20, 0.02).
narrative_ontology:measurement_basis(acti_su_t20, observed).
narrative_ontology:measurement(acti_su_t30, actinide_replenishment_mechanism_flat_control, suppression_requirement, 30, 0.02).
narrative_ontology:measurement_basis(acti_su_t30, observed).
narrative_ontology:measurement(acti_su_t40, actinide_replenishment_mechanism_flat_control, suppression_requirement, 40, 0.02).
narrative_ontology:measurement_basis(acti_su_t40, observed).
narrative_ontology:measurement(acti_su_t50, actinide_replenishment_mechanism_flat_control, suppression_requirement, 50, 0.02).
narrative_ontology:measurement_basis(acti_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(actinide_replenishment_mechanism_flat_control, information_standard).
narrative_ontology:boltzmann_floor_override(actinide_replenishment_mechanism_flat_control, 0.02).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
