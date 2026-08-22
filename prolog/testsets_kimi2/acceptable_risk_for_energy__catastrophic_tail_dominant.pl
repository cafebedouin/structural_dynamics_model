% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Risk Dominance over Energy Acceptability Decisions
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint instantiates the catastrophic_tail_dominant reading of
 *   the acceptable_risk_for_energy kernel. Under this reading,
 *   low-probability high-consequence events dominate risk calculus for energy
 *   infrastructure, and irreversibility or intergenerational burden outweigh
 *   expected-value optimization. The reading entered policy orthodoxy through
 *   post-Chernobyl and post-Fukushima safety culture, was formalized in
 *   regulatory licensing criteria, and now operates as an institutionalized
 *   veto that suppresses probabilistic trade-off framing. Nuclear energy is
 *   structurally disadvantaged because its tail events are vivid and
 *   irreversible in the public imagination, while its climate benefits are
 *   probabilistic and diffuse. Waste disposal migrates from engineering
 *   problem to permanent social constraint, blocking projects even when
 *   technical solutions exist.
 *
 * KEY AGENTS:
 *   - Nuclear energy sector: Primary target (powerful/constrained) â bears extraction through blocked projects and excess compliance burden.
 *   - Future generations: Secondary target (powerless/trapped) â bear intergenerational climate and waste burdens without voice.
 *   - Natural gas sector: Primary beneficiary (powerful/mobile) â captures baseload share when nuclear is vetoed.
 *   - Renewable energy developers: Secondary beneficiary (moderate/constrained) â receive diverted investment in nuclear-excluded portfolios.
 *   - Energy regulators: Agenda setter (institutional/analytical) â administers the tail-risk criteria and derives mandate from it.
 *   - Probabilistic risk analysts: Excluded voice (moderate/analytical) â structurally absent from licensing discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.72).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Risk Dominance over Energy Acceptability Decisions").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6').
narrative_ontology:cs_kernel_codification('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', formalized).
narrative_ontology:cs_authority_grounding('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', lineage).
narrative_ontology:cs_interpretation_layer_present('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6').
narrative_ontology:cs_reading_relation('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', acceptable_risk_for_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', foundational, irreversibility_trumps_probability).
narrative_ontology:cs_axiom_status(irreversibility_trumps_probability, holdable).
narrative_ontology:cs_axiom_grounding('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', irreversibility_trumps_probability, deontological).
narrative_ontology:cs_axiom('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', foundational, intergenerational_burden_binding).
narrative_ontology:cs_axiom_status(intergenerational_burden_binding, holdable).
narrative_ontology:cs_axiom_grounding('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', intergenerational_burden_binding, deontological).
narrative_ontology:cs_reference_frame('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', post_accident_precautionary_frame).
narrative_ontology:cs_drift_state('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', climate_emergency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ed4f90dd-6734-4fc9-b9f6-a6777dbe0fa6', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, natural_gas_sector).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_developers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the cost of regulatory requirements that treat low-probability catastrophic tail risk as a veto regardless of expected value; projects face indefinite delays or cancellation when waste disposal is treated as a permanent social constraint rather than a solvable engineering problem; exit means abandoning the technology entirely.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_energy_sector, payer,
    powerful, generational, constrained, national).

% Cannot consent to geologic waste stewardship obligations or to the atmospheric carbon burden of foregone low-carbon baseload; the constraint imposes both a claimed protective duty and an actual climate cost on them without representation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, payer,
    powerless, civilizational, trapped, global).

% Administer licensing and safety criteria that institutionalize tail-risk dominance; derive institutional legitimacy and expanded mandate from the precautionary posture; could in principle shift to probabilistic criteria but face asymmetric political backlash if any accident occurs.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, energy_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Captures baseload market share and investment that would otherwise flow to nuclear projects blocked by tail-risk vetoes; benefits from the suppression of probabilistic comparisons that would favor nuclear on expected climate mortality.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, natural_gas_sector, beneficiary,
    powerful, biographical, mobile, national).

% Receives diverted policy support and investment in energy systems where nuclear is excluded by catastrophic-tail framing; competes for reliability roles but without the same waste-liability or accident-tail veto.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_developers, beneficiary,
    moderate, biographical, constrained, national).

% Would advocate for expected-value and comparative-risk frameworks that quantify annual mortality and climate benefits against accident probabilities; their methodological approach is structurally excluded from licensing hearings and political discourse by the dominance of irreversibility framing.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts, excluded,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents low-probability high-consequence energy catastrophes by institutionalizing precautionary decision rules that screen out projects with potentially irreversible catastrophic tails, independently of annual expected harm calculations.
% TRANSFER_FUNCTION: Moves investment, regulatory approval, and social license away from nuclear energy toward competing sources; imposes intergenerational waste stewardship obligations on unrepresented future populations; transfers the burden of proof to developers to demonstrate zero catastrophic tail risk.
% ABSENT_VOICES: Probabilistic risk analysts who would weight annual expected mortality against climate benefits; nuclear engineers who would reframe waste disposal as a solvable technical challenge; future generations who cannot contest the climate cost of foregone nuclear baseload.
% DISAPPEARANCE_RATIONALE: If the tail-risk dominance heuristic vanished overnight, regulatory frameworks would revert to expected-value or comparative-risk optimization, nuclear project economics would shift, baseload portfolios would rebalance toward low-carbon sources, and intergenerational waste framing would migrate back to engineering departments.
% FOUNDING_PROBLEM: Chernobyl, Fukushima, and earlier accidents created legitimate public demand for assurance that energy infrastructure would not produce catastrophic irreversible harm; the policy response generalized tail-risk aversion into an absolute decision rule.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear safety regulators and anti-nuclear movements attest the problem is still live. Climate scientists and energy economists outside the benefiting parties attest that the specific framing has outlived its founding crisis and now produces greater expected harm via climate-delay; independent intergovernmental assessments corroborate the shifted-function reading.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically blocks nuclear projects whose expected climate and mortality benefits exceed their tail risk, creating a deadweight loss that accrues to competing sectors. Suppression (0.78) is higher still because the constraint's persistence depends on actively excluding expected-value and comparative-risk framings from regulatory discourse. Theater ratio (0.55) reflects the migration of waste disposal from engineering challenge to performative political constraint â safety theater that signals virtue without solving the underlying technical problem. Accessibility collapse (0.68) is high: once the irreversibility framing is accepted, probabilistic alternatives collapse in policy space. Resistance (0.52) is moderate because the nuclear industry and a minority of economists mount sustained opposition, but they are outmatched by the precautionary coalition's institutional dominance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (nuclear sector, future generations) and the beneficiary seats (gas, renewables) should compute different constraint types. From the nuclear seat the arrangement reads as a snare â the coordination story (safety) is cover for targeted extraction. From the regulatory seat it reads as a necessary scaffold or rope protecting society from catastrophe. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the author's judgment that both functions are present and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   The natural_gas_sector and renewable_energy_developers are declared beneficiaries because they capture market share and investment diverted by nuclear exclusion; their directionality sits near the beneficiary end. The nuclear_energy_sector and future_generations are declared victims because they bear the costs of blocked low-carbon baseload and imposed stewardship obligations; their directionality sits near the target end. Energy_regulators administer the constraint but are not its financial beneficiaries; their directionality is intermediate. Probabilistic_risk_analysts are excluded, receiving no directionality weight because they are outside the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing catastrophic reactor accidents â is genuine and partially live. Mandatrophy would mislabel the constraint as pure extraction if it ignored the real coordination function. However, the specific reading that catastrophic tail risk dominates regardless of expected value has outlived its proportional justification: the constraint now blocks projects whose expected harm is lower than the coal and gas alternatives it implicitly endorses. The theater ratio above 0.5 signals that a growing share of activity is performative maintenance rather than genuine safety production.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nuclear_victim_legitimacy,
    'Is nuclear energy''s entry into the victim set a correct identification of uninsurable catastrophic tail risk, or a constructed extraction that serves competing energy interests?',
    'Systematic comparison of full-system expected mortality (including climate) across energy portfolios in jurisdictions with and without tail-risk vetoes.',
    'If nuclear''s tail risk is genuinely uninsurable and unique, the constraint''s extraction is lower than measured and the coordination function dominates; if the tail risk is comparable to or lower than alternatives, the constraint reads as snare-flavored extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_victim_legitimacy, empirical, 'Whether nuclear victim status is structurally warranted or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of expected-value framing structural (regulatory exclusion from licensing hearings) or internalized (policymakers genuinely believe tail risk trumps all probability analysis)?',
    'Post-reform discourse analysis: if probabilistic framing resurfaces immediately when regulatory rules change, suppression was structural; if it remains marginal, suppression is internalized.',
    'If internalized, effective suppression exceeds the structural measure because the constraint is self-maintaining even after formal barriers fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    intergenerational_climate_tradeoff,
    'Does the constraint protect future generations from radioactive waste burden or harm them via foregone nuclear baseload and continued fossil combustion?',
    'Integrated assessment models comparing climate damages under nuclear-excluded vs nuclear-inclusive pathways against geologic repository risk profiles.',
    'Resolution shifts the victim set composition and may reveal that future generations are net victims of the constraint rather than its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_climate_tradeoff, empirical, 'Net intergenerational effect of tail-risk dominance').

omega_variable(
    kernel_reading_foreclosure,
    'Does catastrophic_tail_dominant foreclose expected_value_dominant within single regulatory frameworks, or do these readings merely coexist across different institutional cultures?',
    'Jurisdictional case study: identify any regulatory body that simultaneously holds both framing schemes as formal decision criteria.',
    'If no such body exists, the readings are mutually exclusive as formal rules and foreclose is the correct relation; if mixed frameworks exist, coexists_with or influences is more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between catastrophic tail and expected-value readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.18).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 8, 0.27).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 16, 0.36).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 24, 0.45).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 32, 0.5).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the acceptable_risk_for_energy kernel, decomposed per the Îµ-invariance principle because catastrophic_tail_dominant, expected_value_dominant, and comparative_risk_dominant have different Îµ values, different victim/beneficiary structures, and different suppression profiles. They are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
