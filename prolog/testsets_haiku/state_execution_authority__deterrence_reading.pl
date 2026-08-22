% ============================================================================
% CONSTRAINT STORY: state_execution_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: state_execution_authority__deterrence_reading
 *   human_readable: State Execution as Deterrence Against Capital Crimes
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   The deterrence reading of state execution authority frames capital
 *   punishment as a rational policy tool for crime prevention. The constraint
 *   operates as follows: the state maintains the legal authority and
 *   institutional capacity to execute convicted murderers, justified by the
 *   claim that execution deters capital crime by raising its cost beyond the
 *   threshold at which potential offenders would consider it rational to
 *   commit murder. Future potential victims are the beneficiaries (their
 *   security is protected by the deterrent); executed offenders and the
 *   wrongfully convicted are the costs. The empirical claim — that execution
 *   deters — is irreducible: if deterrence is negligible (as much recent
 *   research suggests), the constraint collapses into pure extraction (death
 *   without compensating benefit). This reading coexists with the retributive
 *   reading (punishment as moral balance restoration, independent of
 *   deterrent effect) and is foreclosed by the abolition reading (execution
 *   is categorically impermissible regardless of deterrent efficacy).
 *
 * KEY AGENTS:
 *   - state_execution_authority: institutional agenda-setter, claims deterrence justification, administers the mechanism
 *   - future_potential_victims: powerless, unorganized beneficiaries whose lives are protected (in theory) by the deterrent
 *   - executed_offenders: powerless, trapped victims who bear the ultimate cost as instrumental deterrent signal
 *   - wrongfully_convicted: powerless, trapped victims who represent system error — deaths that contradict the reading's own logic
 *   - crime_researchers: moderate-power observers whose empirical findings validate or refute the deterrence claim
 *   - competing_punishment_regimes: institutional actors (international legal systems) that reject the deterrence reading and exclude it from their frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__deterrence_reading, 0.62).
domain_priors:suppression_score(state_execution_authority__deterrence_reading, 0.45).
domain_priors:theater_ratio(state_execution_authority__deterrence_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__deterrence_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__deterrence_reading, rope).
narrative_ontology:human_readable(state_execution_authority__deterrence_reading, "State Execution as Deterrence Against Capital Crimes").
narrative_ontology:topic_domain(state_execution_authority__deterrence_reading, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_execution_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__deterrence_reading, '2fb31ca2-a6db-4e17-9471-1c9be42b9035').
narrative_ontology:cs_kernel_codification('2fb31ca2-a6db-4e17-9471-1c9be42b9035', formalized).
narrative_ontology:cs_authority_grounding('2fb31ca2-a6db-4e17-9471-1c9be42b9035', lineage).
narrative_ontology:cs_interpretation_layer_present('2fb31ca2-a6db-4e17-9471-1c9be42b9035').
narrative_ontology:cs_reading_relation('2fb31ca2-a6db-4e17-9471-1c9be42b9035', state_execution_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('2fb31ca2-a6db-4e17-9471-1c9be42b9035', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('2fb31ca2-a6db-4e17-9471-1c9be42b9035', foundational, execution_deters_future_murders).
narrative_ontology:cs_axiom_status(execution_deters_future_murders, holdable).
narrative_ontology:cs_axiom_grounding('2fb31ca2-a6db-4e17-9471-1c9be42b9035', execution_deters_future_murders, empirically_contingent).
narrative_ontology:cs_axiom('2fb31ca2-a6db-4e17-9471-1c9be42b9035', secondary, deterrent_benefit_justifies_error_cost).
narrative_ontology:cs_axiom_status(deterrent_benefit_justifies_error_cost, holdable).
narrative_ontology:cs_axiom_grounding('2fb31ca2-a6db-4e17-9471-1c9be42b9035', deterrent_benefit_justifies_error_cost, instrumental).
narrative_ontology:cs_reference_frame('2fb31ca2-a6db-4e17-9471-1c9be42b9035', execution_as_rational_policy_tool).
narrative_ontology:cs_drift_state('2fb31ca2-a6db-4e17-9471-1c9be42b9035', contemporary_criminological_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2fb31ca2-a6db-4e17-9471-1c9be42b9035', '').
narrative_ontology:cs_kernel_id(state_execution_authority__deterrence_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, law_abiding_citizens).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, wrongfully_convicted).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__deterrence_reading, murder_victims_families).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, murder_victims_families).
narrative_ontology:constraint_victim(state_execution_authority__deterrence_reading, defense_counsel_and_advocates).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, marginal_deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_execution_authority__deterrence_reading, rational_actor_criminology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the power to sentence and execute convicted murderers. Justifies execution as necessary deterrent to prevent future murders by raising the cost of capital crime. Administers the legal machinery, sets procedural standards, and makes final authorization decisions. Claims deterrence function vindicates the practice against moral objections.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, state_execution_authority, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who would otherwise be murdered by capital offenders if deterrence were absent or ineffective. The deterrence reading structures them as beneficiaries of the constraint — their lives are protected by raising the cost of murder through execution threat. They are not organized, not present in the legal proceedings, and cannot negotiate their position. Benefit is distributed across an unknown future population.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, future_potential_victims, beneficiary,
    powerless, immediate, trapped, national).

% General population whose security is claimed to be protected by the deterrent effect. They participate in the legal system through voting, jury service, and political voice. They are framed as collective beneficiaries of crime prevention, though the personal risk any individual faces from prevented capital murder is diffuse.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, law_abiding_citizens, beneficiary,
    organized, biographical, constrained, national).

% Individuals convicted of capital crimes and sentenced to death. Under the deterrence reading, they are instrumental costs — the visibility and severity of their execution creates the deterrent signal. They bear the ultimate cost (their life) to raise the cost for potential future offenders. No exit exists once conviction is finalized.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% Individuals convicted and executed for murders they did not commit. The deterrence reading acknowledges wrongful execution as a utilitarian loss — a cost incurred by the system's error rate. They are unintended victims whose deaths undermine the deterrence framework's own logic (innocent execution does not deter actual potential murderers, only creates injustice). Wrongful conviction may be discovered post-execution (too late) or discovered during appeal (complicating the cost-benefit calculus).
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, wrongfully_convicted, payer,
    powerless, immediate, trapped, national).

% Families of murder victims who lose loved ones to capital crimes. The deterrence reading positions them as beneficiaries insofar as execution of the murderer is claimed to deter future such losses. However, they also carry costs: the lengthy legal process, retrials, emotional toll of execution proceedings, and potential post-conviction exoneration that negates the closure execution promised. Some families oppose execution on moral or religious grounds, splitting their structural position.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, murder_victims_families, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(state_execution_authority__deterrence_reading, murder_victims_families, payer).

% Attorneys and advocacy organizations bearing the costs of death-penalty defense: massive legal labor for appeals, mitigation investigation, clemency petitions. These costs are structurally necessary to maintain the procedural legitimacy that the deterrence reading depends on. The deterrence framework requires error-minimization, which in turn requires expensive defense — making this group an unacknowledged cost-bearer supporting the system's moral claims.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, defense_counsel_and_advocates, payer,
    moderate, biographical, constrained, national).

% Criminologists and economists studying deterrence efficacy. The deterrence reading rests on empirical claims about marginal deterrence — that execution prevents murders that would otherwise occur. Researchers occupy the analytical seat: they measure the magnitude of deterrent effect, compare execution to life imprisonment, and can validate or refute the reading's central empirical premise. Their findings directly threaten or support the constraint's justification.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, crime_researchers, observer,
    moderate, biographical, mobile, national).

% International jurisdictions and legal traditions (European Union, Canada, many others) that have abolished execution, claiming life imprisonment achieves adequate deterrence and safety without extrajudicial killing. These reading-alternatives are excluded from the American legal framework not by logical necessity but by institutional choice; their existence demonstrates that the deterrence reading is not structurally inevitable — other readings can sustain functional justice systems.
narrative_ontology:constraint_stakeholder(state_execution_authority__deterrence_reading, competing_punishment_regimes, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_execution_authority__deterrence_reading, state_execution_authority).
narrative_ontology:fixing_cost_class(state_execution_authority__deterrence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified state mechanism for imposing the ultimate penalty on capital offenders, replacing individual revenge, mob justice, or vigilante killing with centralized, rule-governed sanction. Creates a public, predictable cost structure for the gravest crimes, enabling rational actors to incorporate that cost into their decision-making.
% TRANSFER_FUNCTION: Transfers the life of the convicted offender to the state as the price of committing a capital murder. The deterrent value transfers from the offender's execution to the wider population (future potential victims and law-abiding citizens) in the form of reduced murder probability, under the deterrence reading's empirical claim. Defense costs and procedural burdens transfer to the state and defense counsel.
% ABSENT_VOICES: Abolition-reading advocates, who hold that execution is categorically impermissible regardless of deterrent effect, are structurally excluded from the deterrence reading's framing — they reject the trade-off on deontological grounds that the reading cannot address within its own terms. Victims of wrongful execution cannot speak, and their interests are acknowledged only abstractly as 'error costs' rather than as voices in the decision. Some murder victims' families oppose execution but are often not centered in policy discourse.
% DISAPPEARANCE_RATIONALE: If state execution vanished overnight, the criminal justice system would reorganize around life imprisonment and other capital sanctions. The deterrent signal would shift — whether to equivalent magnitude (life-without-parole) or to lesser deterrence (imprisonment alone) is the empirical question the deterrence reading stakes its legitimacy on. Future murder rates would change (increase, stay flat, or decrease depending on deterrent efficacy), creating measurable rearrangement of the security landscape. The state would redirect capital-punishment budgets (lengthy appeals, death-row maintenance) to imprisonment or other purposes.
% FOUNDING_PROBLEM: Capital murders threaten the social order and inflict maximum harm. Individual and collective security require that the most severe punishment be available to deter the most severe crimes. The state needs a sanction whose cost is visibly high enough that rational potential murderers will refrain, reducing preventable deaths.
% FOUNDING_PROBLEM_CORROBORATION: Supporters of execution and some victims' families attest the founding problem is live — capital murder remains a threat and deterrence is necessary. Crime researchers (Ehrlich and others finding positive deterrent effects) corroborate the deterrence mechanism. However, opponents cite criminological research (Donohue & Wolfers, NAS reviews) showing negligible or nonexistent marginal deterrent effect of execution vs. life imprisonment, contradicting the founding problem's salience. International jurisdictions with abolished execution attest the founding problem can be managed without it — security and murder prevention persist without capital punishment.
narrative_ontology:disappearance_verdict(state_execution_authority__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__deterrence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_execution_authority__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__deterrence_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the deterrence reading depends entirely on an empirical claim (deterrence efficacy) that is contested and weakly supported by recent research. If deterrence is real, extractiveness is lower — the offender's death buys genuine future security, making the transfer a rational exchange. If deterrence is negligible, extractiveness approaches that of pure punishment extraction (higher). The temporal series shows a slight rise in extractiveness from 0.48 to 0.64 mid-interval (time 0-37), reflecting growing empirical doubt about deterrence as rigorous criminological studies accumulate, then a dip at interval end (0.62) as procedural legitimacy concerns partially offset the empirical weakening. Suppression is moderate (0.45) because the constraint's enforcement does not require continuous coercion of the general population — execution is rare, visible, and highly proceduralized. The high accessibility_collapse (0.72) reflects the fact that once convicted of a capital crime, alternatives to the state's sentence largely collapse: clemency is rare, execution is the final state. Theater ratio climbs slightly (0.18→0.28) as the ratio of procedural ritual (appeals, clemency hearings, symbolic processes) to actual deterrent output increases — the mechanism becomes more about demonstrating legitimacy and less about measurable crime prevention as doubt accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The state execution authority and crime-deterrence researchers occupy structurally different positions. From the state's institutional seat, execution is justified by deterrence and is a core function of justice administration — the constraint is presented as rope (genuine coordination solving a real security problem). From the researcher's analytical seat, the empirical claim is under empirical test, and recent data suggest negligible or nonexistent marginal deterrent effect. From the seats of executed and wrongfully convicted offenders, the constraint is pure extraction (death without compensating benefit). The engine should compute different types across these seats: rope from the state's position (coordination justified by deterrence), snare or tangled_rope from the payer seats (extraction inadequately justified). The authorship claim of rope reflects the deterrence reading's own frame, not the computed type — the gap between claim and computation is exactly the point the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution authority sits as a beneficiary-aligned actor (d toward 0.0) — it administers the system and benefits from its existence, and has ample exit options (can modify sentencing, commute, change procedures). Future potential victims are the nominal beneficiaries but structurally powerless, unorganized, and unable to negotiate — their d is inverted (they don't choose the benefit, it is imposed on them), placing them toward symmetric or even target-end if the benefit is illusory. Executed offenders are full targets (d = 1.0): powerless, trapped, paying the ultimate cost. Wrongfully convicted are also full targets (d = 1.0), with the additional structural problem that their cost directly contradicts the reading's own empirical premise. Murder victims' families carry mixed directionality: they are nominal beneficiaries (claim closure and deterrence benefit) but also potential payers (emotional cost, disruption of closure, procedural participation demands). Crime researchers are analytical (d undefined, external position). No overrides are needed — the derivation chain from beneficiary/victim + exit handles the directionality correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading's founding problem is 'capital crime remains a threat; deterrence prevents preventable murders.' Status is contested: empirical research from the past 50 years has accumulated evidence of negligible marginal deterrent effect, yet deterrence remains politically defended by some policymakers and supported by older econometric studies. Disappearance verdict is world_rearranges: the criminal justice system would reorganize around alternative capital sanctions (life imprisonment). The mismatch (founding_problem_status=contested + world_rearranges) triggers mandatrophy review. If the founding problem is dead (deterrence is negligible), but the arrangement persists and rearranges the world (imprisons people at high cost, requires expensive defense machinery, produces wrongful executions), the constraint becomes zombie-like: it persists as a ritual that vindicates a false founding claim, extracting costs that are no longer justified by the problem it was built to solve. The theater_ratio climb supports this reading. Author mandatrophy_resolved: false, because the empirical status is contested — the system could yet be reorganized if the foundational empirical claim is conclusively falsified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical,
    'Does execution produce a measurable marginal deterrent effect beyond what life imprisonment or other capital sanctions achieve?',
    'Criminological and econometric research (meta-analyses, causal identification studies, cross-jurisdictional comparisons). The National Academies'' 2012 review, Donohue & Wolfers, and subsequent studies directly measure this; replication and pre-registration of new studies would narrow the empirical range.',
    'If deterrent effect is negligible (near-zero), the constraint''s extractiveness approaches 1.0 (pure punishment, no security benefit); if effect is substantial (execution prevents many murders life imprisonment would not), extractiveness drops toward 0.4–0.5 (justified transfer for genuine benefit). Terminal classification hinges on this answer: rope vs. snare/tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical, empirical, 'The core empirical premise: does execution deter capital crime more effectively than alternatives?').

omega_variable(
    error_rate_acceptable_magnitude,
    'What is the threshold error rate (wrongful execution frequency) at which the deterrent benefit is no longer justified on utilitarian grounds, even if deterrent effect is substantial?',
    'Empirical audits of exoneration rates post-conviction (DNA, Brady violations, tunnel vision). Philosophical analysis of acceptable false-positive rates for irreversible punishment. Comparison to error rates in other high-stakes domains (medical error, aviation).',
    'If wrongful execution rate exceeds ~1-2 per thousand (unexecuted exonerations suggest higher), the system''s error cost may outweigh deterrent benefit even under the deterrence reading''s own utilitarian logic. This would convert the constraint toward snare (extracting deaths that contradict the reading''s own justification). If error rate is demonstrably below ~0.1%, the reading''s logic holds more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(error_rate_acceptable_magnitude, empirical, 'The utilitarian cost of system error: at what wrongful-execution frequency does the reading''s logic collapse?').

omega_variable(
    reading_logical_coherence_under_uncertainty,
    'Can the deterrence reading remain coherent if empirical uncertainty about deterrent effect is irresolvable — i.e., if the research is persistently inconclusive?',
    'If 30+ years of research produce no stable consensus on deterrent magnitude (as current state suggests), the reading must either (a) abandon empirical justification and drift toward the retributive reading, or (b) adopt a precautionary stance (execution is justified despite empirical uncertainty to prevent possible murders). Analysis of how other readings handle irreducible empirical uncertainty.',
    'If the reading drifts toward retributive logic (execution justified by moral balance, not deterrence), the constraint reclassifies from rope (coordination) to something closer to snare (extraction justified by deontological claim rather than utilitarian benefit). If the reading adopts precaution, extractiveness may rise (accepting error costs to achieve possible but unproven benefit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_logical_coherence_under_uncertainty, conceptual, 'Whether the deterrence reading can sustain itself under persistent empirical uncertainty about its core justification.').

omega_variable(
    committer_foreclosure_test_deterrence_vs_abolition,
    'Do the deterrence and abolition readings logically foreclose each other, or do they coexist as coherent positions held by different parties?',
    'Logical analysis: the deterrence reading asserts execution can be justified by deterrent effect (empirical claim + utilitarian logic). The abolition reading asserts execution is categorically impermissible (deontological claim independent of consequences). These are NOT logical contradictions — one party can accept the deterrence claim and still adopt abolition on deontological grounds (execution is wrong even if it deters). The readings coexist in actual legal/political space (US jurisdictions with execution, EU jurisdictions without, both functioning).',
    'If the readings coexist (neither forecloses the other), they are distinct constraints in a constraint family, not a binary choice. This affects how the engine models kernel dynamics: coexisting readings suggest institutional pluralism (different jurisdictions, coalitions can hold different readings), while foreclosing readings would suggest one must eventually dominate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_foreclosure_test_deterrence_vs_abolition, conceptual, 'Whether the deterrence and abolition readings logically foreclose each other or coexist as live positions.').

omega_variable(
    instrumental_vs_intrinsic_justification,
    'Can the deterrence reading sustain itself if the primary function shifts from instrumental deterrence (preventing future murders) to intrinsic justification (execution as moral expression of the state''s authority)?',
    'Historical and sociological analysis: when deterrence claims prove empirically weak, do systems that maintain execution shift their public and legal justification toward retributive or expressive rationales? Analysis of actual doctrine shifts in US capital punishment jurisprudence (Bowers studies, Supreme Court opinions).',
    'If the deterrence reading drifts toward expressive justification, it merges with the retributive reading and the distinction between them collapses. The constraint would reclassify from rope (deterrent coordination) to snare or tangled_rope (expressive extraction justified by deontological claim).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_vs_intrinsic_justification, conceptual, 'Whether deterrence justification can persist as primary if empirical support erodes, or if the reading drifts toward retributive/expressive justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__deterrence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t8, state_execution_authority__deterrence_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(stat_tr_t8, observed).
narrative_ontology:measurement(stat_tr_t16, state_execution_authority__deterrence_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(stat_tr_t16, observed).
narrative_ontology:measurement(stat_tr_t25, state_execution_authority__deterrence_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(stat_tr_t25, observed).
narrative_ontology:measurement(stat_tr_t37, state_execution_authority__deterrence_reading, theater_ratio, 37, 0.29).
narrative_ontology:measurement_basis(stat_tr_t37, observed).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__deterrence_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t8, state_execution_authority__deterrence_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(stat_be_t8, observed).
narrative_ontology:measurement(stat_be_t16, state_execution_authority__deterrence_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(stat_be_t16, observed).
narrative_ontology:measurement(stat_be_t25, state_execution_authority__deterrence_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(stat_be_t25, observed).
narrative_ontology:measurement(stat_be_t37, state_execution_authority__deterrence_reading, base_extractiveness, 37, 0.64).
narrative_ontology:measurement_basis(stat_be_t37, observed).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__deterrence_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__deterrence_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t8, state_execution_authority__deterrence_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(stat_su_t8, observed).
narrative_ontology:measurement(stat_su_t16, state_execution_authority__deterrence_reading, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(stat_su_t16, observed).
narrative_ontology:measurement(stat_su_t25, state_execution_authority__deterrence_reading, suppression_requirement, 25, 0.44).
narrative_ontology:measurement_basis(stat_su_t25, observed).
narrative_ontology:measurement(stat_su_t37, state_execution_authority__deterrence_reading, suppression_requirement, 37, 0.46).
narrative_ontology:measurement_basis(stat_su_t37, observed).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__deterrence_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_execution_authority__deterrence_reading, 0.12).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__retributive_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, state_execution_authority__abolition_reading).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, life_imprisonment_as_alternative_capital_sanction).
narrative_ontology:affects_constraint(state_execution_authority__deterrence_reading, wrongful_conviction_error_rate).

% DUAL FORMULATION NOTE:
% The state_execution_authority kernel decomposes into three constraint stories corresponding to three competing readings: deterrence_reading (empirical deterrence justification), retributive_reading (moral balance justification), abolition_reading (categorical impermissibility). The ε values differ substantially across readings. deterrence_reading's ε depends on empirical deterrent effect (moderate ~0.6 if effect is small, higher if effect is nil); retributive_reading's ε is independent of deterrence (justification rests on proportionality, higher ~0.7+); abolition_reading rejects the standing arrangement entirely (ε measured from the abolitionist reading's frame, ~0.95 — pure extraction from the abolition perspective). These are structurally distinct constraints sharing a kernel. The family also includes life_imprisonment_as_alternative_capital_sanction, which constrains the deterrence reading's substitutability claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__deterrence_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
