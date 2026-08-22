% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__deterrence_reading, []).

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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: Capital Punishment Authority — Deterrence Reading
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This story authors the deterrence reading of the state execution
 *   authority kernel: the claim that executing convicted capital offenders
 *   reduces future murder by raising the expected cost of the crime. Under
 *   this reading the executed offender is not an end in themselves but an
 *   instrumental cost paid to generate a public deterrent signal for the
 *   benefit of an unidentifiable population of future potential victims. This
 *   is one of three readings of the same kernel — the retributive reading
 *   (moral balance, proportionate punishment as end in itself) and the
 *   abolition reading (execution categorically impermissible) are separate
 *   constraints with their own ε, beneficiary structure, and type, linked via
 *   network.affects_constraints. Do not average this ε against the siblings'
 *   ε; they are different constraints.
 *
 * KEY AGENTS:
 *   - state_prosecutorial_apparatus: agenda_setter (institutional/analytical) — administers capital sentencing and cites deterrence to justify it
 *   - future_potential_murder_victims: beneficiary (powerless/analytical) — statistical, unidentifiable population claimed to be protected
 *   - executed_offenders: payer (powerless/trapped) — instrumental cost of the deterrent signal
 *   - wrongfully_convicted_defendants: payer (powerless/trapped) — pure utilitarian loss with no offsetting deterrent benefit
 *   - criminology_deterrence_researchers: observer (analytical) — contested empirical basis for the claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.52).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.68).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "Capital Punishment Authority — Deterrence Reading").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '8297ad1e-8fa4-4223-8345-04bd9c81b01f').
narrative_ontology:cs_kernel_codification('8297ad1e-8fa4-4223-8345-04bd9c81b01f', distributed).
narrative_ontology:cs_authority_grounding('8297ad1e-8fa4-4223-8345-04bd9c81b01f', distributed).
narrative_ontology:cs_reading_relation('8297ad1e-8fa4-4223-8345-04bd9c81b01f', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('8297ad1e-8fa4-4223-8345-04bd9c81b01f', state_execution_authority__abolition_reading, influences).
narrative_ontology:cs_axiom('8297ad1e-8fa4-4223-8345-04bd9c81b01f', foundational, marginal_deterrence_justifies_capital_severity).
narrative_ontology:cs_axiom_status(marginal_deterrence_justifies_capital_severity, holdable).
narrative_ontology:cs_axiom_grounding('8297ad1e-8fa4-4223-8345-04bd9c81b01f', marginal_deterrence_justifies_capital_severity, empirically_contingent).
narrative_ontology:cs_axiom('8297ad1e-8fa4-4223-8345-04bd9c81b01f', secondary, offender_may_be_instrumentalized_for_aggregate_welfare).
narrative_ontology:cs_axiom_status(offender_may_be_instrumentalized_for_aggregate_welfare, holdable).
narrative_ontology:cs_axiom_grounding('8297ad1e-8fa4-4223-8345-04bd9c81b01f', offender_may_be_instrumentalized_for_aggregate_welfare, instrumental).
narrative_ontology:cs_reference_frame('8297ad1e-8fa4-4223-8345-04bd9c81b01f', consequentialist_public_safety_calculus).
narrative_ontology:cs_drift_state('8297ad1e-8fa4-4223-8345-04bd9c81b01f', post_nrc_2012_meta_analysis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8297ad1e-8fa4-4223-8345-04bd9c81b01f', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_murder_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, public_safety_apparatus).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted_defendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, general_public).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, general_public).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, marginal_deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, rational_actor_crime_calculus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks capital sentences in qualifying cases, administers appeals and clemency review, and justifies the practice publicly by reference to its deterrent effect on future capital crimes. Controls charging decisions, plea bargaining leverage, and execution scheduling.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_prosecutorial_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% An unidentifiable statistical population whose lives the deterrence claim asserts are saved by the marginal disincentive effect of capital punishment on would-be capital offenders. They cannot be named, consulted, or counted with certainty — the entire beneficiary class exists only as a modeled counterfactual.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_murder_victims, beneficiary,
    powerless, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(state_execution_authority__deterrence_reading, future_potential_murder_victims).

% Convicted of capital crimes and put to death as the instrumental mechanism by which the deterrent signal is generated. Under this reading their execution is not deserved punishment for its own sake but a cost paid to produce a public disincentive; they have no exit once sentenced and appeals proceed on procedural rather than substantive grounds.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, local).

% A subset of the executed or death-sentenced population who did not commit the crime. Because deterrence theory treats execution as instrumentally justified by its aggregate effect, an erroneous execution is a pure utilitarian loss with no offsetting benefit — a cost the deterrence calculus must minimize but cannot eliminate given imperfect fact-finding.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted_defendants, payer,
    powerless, immediate, trapped, local).

% Produce econometric studies attempting to isolate a marginal deterrent effect of capital punishment relative to life imprisonment. Their findings are contested and methodologically fragile (small sample of executions, confounded state-level variation), yet are cited by the state to justify continued authority.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, criminology_deterrence_researchers, observer,
    analytical, generational, analytical, national).

% Serve as the counterfactual comparison class — if life imprisonment produces equivalent deterrence, their existence undercuts the necessity of execution. They have no voice in whether the state substitutes their sentence structure for capital punishment; the comparison is made about them, not with them.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, life_without_parole_populations, excluded,
    powerless, biographical, trapped, national).

% Receives the claimed safety benefit and also bears the fiscal and moral cost of maintaining capital trial and appeals infrastructure, which is substantially more expensive per case than life imprisonment. Can express preference through elections and juries but cannot directly test the deterrence claim.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, general_public, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, general_public, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, diffuse).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a credible, state-administered threat of maximal punishment intended to raise the expected cost of committing capital crimes above the offender's expected benefit, thereby reducing the incidence of murder across the jurisdiction.
% TRANSFER_FUNCTION: Moves the risk of violent death from an unidentifiable population of future potential victims onto a small, identifiable population of convicted (and occasionally wrongfully convicted) offenders, mediated through the state's prosecutorial and correctional apparatus.
% ABSENT_VOICES: The future potential victims whose lives are claimed to be saved cannot testify to having been saved — the beneficiary class is definitionally silent. Life-without-parole populations, who would show the marginal deterrent effect is zero if their sentence structure matches death-sentence outcomes, are not part of the policy conversation about capital sentencing.
% DISAPPEARANCE_RATIONALE: If capital punishment authority disappeared overnight, the deterrence reading holds that the murder rate would rise measurably (world_rearranges); the abolitionist and much empirical criminology literature holds the murder rate would be statistically indistinguishable from the status quo (world_unchanged). Under this reading's own lights the world rearranges, but that verdict rests entirely on the unresolved omega below.
% FOUNDING_PROBLEM: Capital crime was occurring at a rate the state judged unacceptable, and existing sanctions (imprisonment) were judged insufficiently costly to deter rational or semi-rational offenders from committing murder.
% FOUNDING_PROBLEM_CORROBORATION: State prosecutors and some criminologists (e.g. econometric deterrence studies from the 1970s-2000s) attest the problem remains live and the marginal deterrent effect is real. The National Research Council's 2012 review, life-without-parole comparative studies, and researchers outside the prosecutorial apparatus attest that decades of data show no reliable marginal deterrent effect distinguishable from life imprisonment — corroboration against the founding problem's continued relevance comes substantially from outside the beneficiary set.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, contested).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than high because the deterrence reading's own justificatory structure requires a genuine social-welfare payoff (fewer future murders) to offset the cost imposed on the executed population — this is a coordination claim, not naked extraction, if the deterrence-efficacy premise holds. But the premise is empirically contested (see omega below), and the cost of an execution falls entirely and irreversibly on an individual who, under this reading, has no independent right not to be used as an instrument of the state's public-safety signal. Suppression (0.68) reflects the appeals-exhaustion and procedural-finality machinery required to make executions legally irreversible once scheduled — the deterrent signal requires credibility, and credibility requires the state to suppress avenues that would make the sentence uncertain or revocable. Theater ratio (0.42) and its upward drift reflect a growing share of capital litigation devoted to procedural and symbolic legitimacy performance (extended appeals, execution-method litigation, clemency theater) relative to any function that could plausibly generate the marginal deterrent effect itself, which is empirically difficult to observe and improve upon. Accessibility collapse is moderate (0.4) — life-without-parole remains a visible, actively litigated alternative sentencing structure, so alternatives have not collapsed the way they would under a genuine mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Future potential murder victims are the structural beneficiary class under this reading, but they are a non-agent statistical population (agent: false) — they cannot collect, consent, or be consulted, which is itself diagnostic: the beneficiary of a deterrence claim is always counterfactual. Executed offenders and wrongfully convicted defendants sit at the extraction pole: trapped exit, powerless, and under this reading explicitly instrumentalized — their death is justified by its effect on a population other than themselves, which is the hallmark of the tangled-rope structure (coordination for one group achieved through payment extracted from another through the same mechanism). The general public is both beneficiary (safety) and payer (fiscal and moral cost of maintaining capital infrastructure), reflecting the diffuse and mixed position of the polity that authorizes the arrangement through elections and juries but cannot verify the deterrence premise directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insufficiently costly sanctions failing to deter capital crime) is contested rather than resolved: prosecutorial and some econometric sources hold it live, while the National Research Council and comparative life-without-parole data suggest the marginal deterrent effect the arrangement was built to produce cannot be reliably detected. This is exactly the mismatch the R5 genealogy interview is designed to surface — status=contested paired with a contested disappearance_verdict, rather than status=dead paired with world_rearranges, means this does not cleanly diagnose as mandatrophy under this reading, but the corpus should flag it for continued monitoring as deterrence research accumulates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_deterrence_empirical_status,
    'Does capital punishment produce a measurable marginal deterrent effect on murder rates beyond what life-without-parole sentencing achieves?',
    'Meta-analysis of comparative-jurisdiction murder rate data controlling for confounds (the approach taken by the 2012 National Research Council review), extended with newer state-level panel data as death penalty states abolish or reinstate the practice.',
    'If no marginal effect is found, the deterrence reading''s coordination function collapses entirely — the beneficiary class (future potential victims) does not actually receive the claimed benefit, and the constraint reduces to extraction from executed offenders with no offsetting social gain, converting this reading''s honest classification toward snare. If a robust marginal effect is found, the tangled_rope classification with genuine (if costly) coordination is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrence_empirical_status, empirical, 'Whether the deterrence premise this entire reading depends on is empirically true.').

omega_variable(
    substitutability_with_life_without_parole,
    'If life-without-parole produces equivalent deterrence to execution, does the deterrence reading retain any independent justification for the more severe and irreversible sanction?',
    'Direct comparison of jurisdictions using LWOP versus capital punishment as the maximum sentence, holding prosecutorial and sentencing practices otherwise constant.',
    'Equivalent deterrence with a reversible, less costly alternative available would mean the marginal extraction of execution over LWOP (the offender''s life, plus the wrongful-execution risk) purchases no additional coordination benefit — this is the specific substitution point the SOURCE MATERIAL flags as central to this reading''s structural delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitutability_with_life_without_parole, empirical, 'Whether execution''s severity premium over life imprisonment is justified by additional deterrent value.').

omega_variable(
    wrongful_execution_rate_and_error_correction,
    'What is the true rate of wrongful capital convictions that proceed to execution, and can the appeals and evidentiary system detect them before irreversibility?',
    'Post-execution DNA and case-reopening exoneration studies; comparison of exoneration rates in non-capital cases (where correction remains possible) against estimated wrongful-execution rates.',
    'A non-trivial wrongful-execution rate is, under the deterrence reading''s own utilitarian logic, a pure cost with no offsetting benefit for that individual case — the reading requires this rate be minimized, and a persistently non-negligible rate weakens the reading''s net social-welfare claim independent of whether deterrence itself is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wrongful_execution_rate_and_error_correction, empirical, 'The magnitude of irreversible error the deterrence framework''s own logic requires it to minimize.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the deterrence framing the operative public justification for a given jurisdiction''s capital punishment regime, or is it a post-hoc rationalization layered onto a retributive practice that would persist regardless of deterrence findings?',
    'Legislative history analysis and public-opinion research on whether death-penalty support tracks deterrence-evidence updates or remains stable regardless of empirical findings.',
    'If public and legislative support for capital punishment is insensitive to deterrence evidence, the deterrence_reading constraint described here may not be the operative constraint in practice even where deterrence is the stated justification — the retributive_reading sibling may be doing the actual structural work while this reading provides cover language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the deterrence justification is the reading actually operating, or a rationalization for a retributive practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(stat_tr_t24, state_execution_authority__deterrence_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(stat_tr_t32, state_execution_authority__deterrence_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__deterrence_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(stat_be_t24, state_execution_authority__deterrence_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(stat_be_t32, state_execution_authority__deterrence_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__deterrence_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement(stat_su_t24, state_execution_authority__deterrence_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(stat_su_t32, state_execution_authority__deterrence_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__deterrence_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language 'capital punishment' claim per the ε-invariance principle. state_execution_authority__retributive_reading treats the offender's death as an intrinsically deserved end (no future-victim beneficiary class, backward-looking justification). state_execution_authority__abolition_reading treats execution as categorically impermissible (near-maximal ε, sole victim class, no legitimate beneficiary). Each carries its own ε, beneficiary/victim structure, and computed type; they are linked here rather than merged because averaging or parameterizing a single constraint across these three premises would violate ε-invariance — changing which normative premise is in force changes what is being measured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
