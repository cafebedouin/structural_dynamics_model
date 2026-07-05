% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__retributive_reading, []).

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
 *   constraint_id: state_killing_legitimacy__retributive_reading
 *   human_readable: Retributive Justification for State Execution (Lex Talionis Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This story instantiates the retributive reading of the contested kernel
 *   state_killing_legitimacy: the claim that a murderer's act morally
 *   forfeits their right to continued life, and that proportional exaction of
 *   that forfeiture (lex talionis) by the state is what makes capital
 *   punishment legitimate — independent of any deterrent effect and
 *   independent of dignity-based objections. This is a distinct constraint
 *   from the deterrence reading (which grounds legitimacy in future-crime
 *   prevention, an empirically falsifiable claim) and the abolition reading
 *   (which denies any desert-based or utility-based justification can license
 *   state killing). The three readings share a kernel — the legitimacy of the
 *   state's power to execute — but instantiate structurally different
 *   constraints with different beneficiary/victim structures, different
 *   failure modes, and different ε values. This story does not average across
 *   them or hedge; it commits to the retributive premise and traces its
 *   structural consequences alone.
 *
 * KEY AGENTS:
 *   - condemned_offenders: primary target (powerless/trapped) — bears the forfeiture extraction directly and irreversibly
 *   - wrongfully_convicted_death_row_inmates: limit case exposing the doctrine's core vulnerability — forfeiture applied without actual desert
 *   - victims_families_seeking_desert: primary beneficiary (moderate/constrained) — receives the promised moral closure
 *   - retributive_justice_apparatus: agenda-setter (institutional/arbitrage) — administers and legitimates the forfeiture doctrine
 *   - moral_order_constituency: diffuse beneficiary (organized/mobile) — vindicated moral cosmology, no personal exposure
 *   - constitutional_courts: analytical observer — adjudicates doctrine's constitutional limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, 0.71).
domain_priors:suppression_score(state_killing_legitimacy__retributive_reading, 0.62).
domain_priors:theater_ratio(state_killing_legitimacy__retributive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_killing_legitimacy__retributive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__retributive_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__retributive_reading, "Retributive Justification for State Execution (Lex Talionis Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__retributive_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__retributive_reading, '6fee36ca-0a7f-4d74-9f8d-9874e2daaa80').
narrative_ontology:cs_kernel_codification('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', distributed).
narrative_ontology:cs_authority_grounding('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', distributed).
narrative_ontology:cs_reading_relation('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', state_killing_legitimacy__deterrence_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', foundational, grave_wrongdoing_forfeits_life_right).
narrative_ontology:cs_axiom_status(grave_wrongdoing_forfeits_life_right, holdable).
narrative_ontology:cs_axiom_grounding('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', grave_wrongdoing_forfeits_life_right, deontological).
narrative_ontology:cs_axiom('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', secondary, punishment_must_be_proportionate_not_merely_useful).
narrative_ontology:cs_axiom_status(punishment_must_be_proportionate_not_merely_useful, holdable).
narrative_ontology:cs_axiom_grounding('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', punishment_must_be_proportionate_not_merely_useful, deontological).
narrative_ontology:cs_reference_frame('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', classical_desert_proportionality).
narrative_ontology:cs_drift_state('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6fee36ca-0a7f-4d74-9f8d-9874e2daaa80', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__retributive_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, moral_order_constituency).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, victims_families_seeking_desert).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__retributive_reading, retributive_justice_apparatus).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, condemned_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, wrongfully_convicted_death_row_inmates).
narrative_ontology:constraint_victim(state_killing_legitimacy__retributive_reading, offenders_with_diminished_capacity).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, proportional_desert_doctrine).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__retributive_reading, moral_forfeiture_of_life_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted of murder and sentenced to death on the premise that the killing has forfeited their moral claim to continued life. They bear the full weight of the retributive calculus with no capacity to exit, appeal beyond procedural limits, or renegotiate the desert-claim once the conviction stands. The proportionality claim is applied to them categorically, not case-by-case against their actual moral culpability at time of clemency review.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, condemned_offenders, payer,
    powerless, immediate, trapped, national).

% Sentenced under the same forfeiture logic despite factual innocence or wrongful conviction. The retributive framework's proportionality claim collapses entirely when guilt is mistaken, yet the execution apparatus proceeds unless exonerating evidence surfaces before the irreversible act. They have no capacity to test the desert-claim once procedural avenues are exhausted.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, wrongfully_convicted_death_row_inmates, payer,
    powerless, immediate, trapped, national).

% Individuals whose culpability is mitigated by intellectual disability, severe mental illness, or coercion, but who are nonetheless subjected to the same forfeiture logic. The proportional-desert claim requires full moral agency to be coherent; where agency is diminished, the desert calculus is applied anyway, extracting a life-right forfeiture disproportionate to actual culpability.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, offenders_with_diminished_capacity, payer,
    powerless, immediate, trapped, national).

% Receive the symbolic and psychological satisfaction the retributive framework promises: that the offender's forfeiture restores a moral balance disrupted by the murder. Their felt sense of justice is the primary lived benefit the doctrine delivers, though empirical closure outcomes are contested and some report the promised resolution does not materialize.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, victims_families_seeking_desert, beneficiary,
    moderate, biographical, constrained, national).

% The diffuse body of citizens, legislators, and moral communities who hold that proportional desert is a requirement of justice itself, independent of deterrent effect. They benefit from the doctrine's persistence as a vindication of a moral cosmology in which grave wrongs generate forfeiture. They are not individually coerced and can exit the belief without personal cost, unlike the condemned.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, moral_order_constituency, beneficiary,
    organized, generational, mobile, national).

% Prosecutors, capital-sentencing courts, and corrections systems that administer the forfeiture doctrine — seeking death sentences, defending them on appeal, and carrying out executions. They invoke proportional desert as the legitimating premise, control the machinery that determines whose desert-claim is tested, and are institutionally insulated from the doctrine's failure modes (wrongful conviction, diminished capacity) because reversal, when it occurs, is attributed to procedure rather than to the desert theory itself.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, retributive_justice_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue the forfeiture premise is incoherent or impermissible regardless of desert calculus, and are structurally excluded from the retributive framework's own internal adjudication — the doctrine treats their objection as a competing moral framework to be outvoted, not a defect to be corrected within retributivism itself.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Adjudicate whether the forfeiture-based execution regime satisfies constitutional constraints (proportionality, cruel-and-unusual standards, equal protection). They receive evidence from all other seats and can narrow or expand the doctrine's operative scope, but do not themselves hold a stake in the desert-claim's truth.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__retributive_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__retributive_reading, diffuse).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__retributive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared moral-accounting framework by which the state, victims' communities, and the broader polity can agree that a specific magnitude of wrongdoing generates a specific magnitude of forfeited right — coordinating collective judgment about proportionate response to the gravest crimes without requiring case-by-case ad hoc moral reasoning.
% TRANSFER_FUNCTION: Moves the offender's continued existence — framed as a forfeited right rather than a resource — from the offender to the state's execution apparatus, while moving symbolic/psychological closure and moral-order vindication to victims' families and the broader moral constituency.
% ABSENT_VOICES: The condemned offender's own account of proportionality (was the forfeiture actually earned, given specific mitigating facts) is structurally excluded once appeals are exhausted; abolitionist advocates who reject the forfeiture premise entirely are treated as a rival value system to be defeated rather than a challenge to the doctrine's internal coherence; exonerated-but-executed cases cannot testify at all.
% DISAPPEARANCE_RATIONALE: If the retributive-desert justification vanished overnight, capital sentencing would lose its primary moral warrant in retributivist jurisdictions; sentencing regimes would need to re-ground themselves in deterrence, incapacitation, or abolish the practice; condemned offenders currently facing execution under desert-based statutes would have grounds to challenge their sentences; victims' families currently promised moral closure through forfeiture would need a different vocabulary for that expectation.
% FOUNDING_PROBLEM: Societies historically lacked any principled, bounded way to respond to grave intentional killing that was neither unlimited private vengeance (blood feuds) nor total state indifference — lex talionis was constructed to bound the response ('an eye for an eye,' not more) and to supply moral legitimacy for the state's monopolization of violent response to violence.
% FOUNDING_PROBLEM_CORROBORATION: Retributive theorists (Kant, Moore, contemporary desert theorists) attest the problem is live: unbounded vengeance and moral non-response both remain real failure modes requiring a principled forfeiture theory. Independent penology research and comparative-law scholars outside the retributivist tradition attest the bounding function is now served by proportionate incarceration in the overwhelming majority of developed legal systems, and that the specifically lethal forfeiture claim persists primarily through inherited doctrine and political symbolism rather than continued functional necessity — this is not corroborated by anyone with a stake in maintaining capital sentencing regimes.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__retributive_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__retributive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__retributive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_killing_legitimacy__retributive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__retributive_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__retributive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71 at interval end) because the retributive reading licenses an irreversible transfer (life) justified purely by a backward-looking desert claim that cannot be verified against the offender's actual, present moral status — and because the doctrine's application does not scale down for wrongful conviction or diminished capacity, both of which break the proportionality premise the doctrine claims to rest on. Suppression (0.62) reflects the active machinery of capital sentencing, appellate foreclosure, and execution scheduling required to carry the forfeiture claim through to completion against strong and organized resistance. Theater ratio (0.38) captures a meaningful gap between the doctrine's stated function (precise moral accounting) and its administration (highly variable sentencing outcomes correlated with race, geography, and quality of counsel rather than culpability) — some of what presents as proportional desert-calculation is performing precision it does not possess. Resistance is high (0.72): abolitionist movements, wrongful-conviction exonerations, and constitutional litigation actively contest the doctrine, unlike a genuine mountain which would meet negligible resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the apparatus's seat, the doctrine is coherent coordination: it bounds vengeance, honors victims, and vindicates a moral order — a rope. From the condemned offender's seat, particularly the wrongfully convicted or diminished-capacity offender, the identical structure is pure extraction backed by irreversible coercion — a snare. The tangled_rope classification is authored because the story contains BOTH a genuine coordination function (bounding vengeance, providing shared moral vocabulary) AND asymmetric extraction requiring active enforcement (the execution apparatus, appellate foreclosure) with identifiable victims who do not merely experience it differently but structurally cannot exit or contest it once judgment is final.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned offenders sit at the extreme target end: trapped exit, powerless, and the constraint's operation is defined by extracting their life-right. Wrongfully convicted and diminished-capacity offenders are even more clearly victims because the desert-premise fails structurally in their cases yet the extraction proceeds identically. Victims' families and the moral-order constituency are beneficiaries, but asymmetrically: families bear real (if intangible) stakes and limited exit from their situational need for resolution, while the moral-order constituency benefits at no personal cost and can exit the belief freely — this is why they are given different power/exit profiles despite both being coded as beneficiaries. The retributive justice apparatus is the agenda-setter: it does not merely benefit passively but administers, defends, and reproduces the forfeiture doctrine, and is structurally insulated from bearing the costs of its failure modes (wrongful executions are absorbed as procedural tragedy, not doctrinal refutation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/status/corroboration fields expose a live contest rather than settling it: retributivists corroborate the doctrine from within (unbounded vengeance remains a real danger), while penological and comparative-law scholarship outside the retributivist tradition corroborates that the bounding function the doctrine was built for is now served by proportionate incarceration in the vast majority of comparable legal systems — leaving the specifically lethal forfeiture claim to persist on inherited doctrine and political symbolism. This is exactly the mismatch the R5 consumer is built to catch: founding_problem_status=contested paired with disappearance_verdict=world_rearranges signals that the doctrine's practical grip (sentencing regimes, statutes, active cases) currently exceeds what independent corroboration can support as still-necessary — a capture/zombie flag worth surfacing, not resolving by fiat in this story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    desert_coherence_under_error,
    'Can a desert-based forfeiture theory remain coherent once wrongful conviction and diminished-capacity cases are accounted for, or does the possibility of error at the rate observed in capital cases dissolve the proportionality claim the theory depends on?',
    'Compare the doctrine''s internal proportionality standard against documented exoneration rates and diminished-capacity sentencing outcomes; assess whether retributive theorists treat these as correctable procedural failures or as evidence undermining the forfeiture premise itself.',
    'If the theory cannot survive its own error rate without abandoning the strict-proportionality claim, the retributive reading''s legitimacy warrant weakens substantially and the extraction from wrongfully-convicted and diminished-capacity offenders becomes harder to distinguish from unjustified killing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desert_coherence_under_error, conceptual, 'Whether the desert-forfeiture premise survives known error rates in capital sentencing.').

omega_variable(
    kernel_reading_selection_basis,
    'Is the retributive reading the correct primary framing for a given real-world capital statute, or do most actual capital-sentencing regimes operate on a hybrid retributive/deterrence rationale that this decomposition has separated for analytical clarity but that no single jurisdiction cleanly instantiates?',
    'Examine legislative history and judicial opinions (e.g., Gregg v. Georgia and its progeny) for explicit invocation of desert versus deterrence rationales; many opinions cite both without distinguishing them.',
    'If most real jurisdictions operate a hybrid rationale, this story and its sibling deterrence_reading are each partial views of a single messier real-world constraint — informative as decomposed ideal types but requiring the reader to recombine them when assessing an actual statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether pure retributive framing matches real capital statutes or is an analytical idealization.').

omega_variable(
    moral_order_beneficiary_naturalness,
    'Is the ''vindication of moral order'' the moral_order_constituency receives a genuine collective good, or is it better described as a constructed satisfaction that could be provided by non-lethal proportionate punishment equally well?',
    'Comparative study of retributive satisfaction/closure outcomes in jurisdictions using life-without-parole versus capital punishment for equivalent offenses.',
    'If non-lethal proportionate punishment delivers equivalent moral-order vindication, the specifically lethal component of the forfeiture claim is extraction without a coordination function distinguishing it from incarceration-based retributivism, sharpening the tangled_rope classification toward snare for the lethal-specific increment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_order_beneficiary_naturalness, empirical, 'Whether lethal forfeiture provides coordination value beyond what non-lethal proportionate punishment already provides.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__retributive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__retributive_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(stat_tr_t8, state_killing_legitimacy__retributive_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(stat_tr_t16, state_killing_legitimacy__retributive_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(stat_tr_t24, state_killing_legitimacy__retributive_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(stat_tr_t32, state_killing_legitimacy__retributive_reading, theater_ratio, 32, 0.36).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__retributive_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__retributive_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(stat_be_t8, state_killing_legitimacy__retributive_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(stat_be_t16, state_killing_legitimacy__retributive_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(stat_be_t24, state_killing_legitimacy__retributive_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(stat_be_t32, state_killing_legitimacy__retributive_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__retributive_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__retributive_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(stat_su_t8, state_killing_legitimacy__retributive_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(stat_su_t16, state_killing_legitimacy__retributive_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(stat_su_t24, state_killing_legitimacy__retributive_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(stat_su_t32, state_killing_legitimacy__retributive_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__retributive_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__retributive_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the state_killing_legitimacy kernel, each authored as a separate ε-invariant constraint per the decomposition principle: retributive_reading (this file, desert-based forfeiture, tangled_rope), deterrence_reading (empirically-contingent future-crime-prevention claim), and abolition_reading (categorical dignity-based prohibition, independent of desert or utility). The three do not share an ε value because they rest on structurally distinct legitimating claims with distinct failure modes — a desert claim fails on proportionality-under-error; a deterrence claim fails on empirical non-support; a dignity claim fails only if dignity itself is denied moral priority. All three are linked here so contamination propagation analysis can trace how erosion of one reading's legitimacy (e.g., empirical refutation of deterrence) shifts argumentative weight onto the others without conflating their distinct structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
