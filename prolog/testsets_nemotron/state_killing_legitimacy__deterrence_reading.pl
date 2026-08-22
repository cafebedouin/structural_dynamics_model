% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Execution as Deterrent Signal
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story captures the deterrence reading of state killing
 *   legitimacy: the claim that execution functions as a rational signal that
 *   prevents future murders by deterring potential offenders. The reading
 *   instrumentalizes the executed offender as a means to a social end
 *   (deterrence) and identifies potential future victims as the primary
 *   beneficiaries. The empirical evidence for deterrence is contested —
 *   meta-analyses produce conflicting results, and natural experiments
 *   (moratoria, abolition/reinstatement cycles) show ambiguous effects. The
 *   constraint requires active enforcement (legal machinery for capital
 *   trials, appeals, execution protocols) and suppresses alternatives (life
 *   without parole, restorative justice) through institutional and cultural
 *   mechanisms. The claimed type is tangled_rope: a genuine coordination
 *   function (protecting potential victims) coexists with asymmetric
 *   extraction (the offender's life is taken on contested empirical grounds,
 *   families bear collateral harm).
 *
 * KEY AGENTS:
 *   - executed_offenders: Primary target (powerless/trapped) — bears the ultimate extraction
 *   - potential_future_victims: Primary beneficiary (powerless/identity_locked) — putative protection from deterrence
 *   - state_legitimacy_apparatus: Agenda setter (institutional/arbitrage) — administers the constraint, collects legitimacy capital
 *   - offender_families: Secondary victim (powerless/constrained) — collateral harm from execution
 *   - abolitionist_advocates: Excluded (organized/analytical) — would object but structurally excluded from the deterrence framework
 *   - criminologists: Observer (analytical/analytical) — assess empirical claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.58).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Execution as Deterrent Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, '005606ea-5333-4040-ad6d-24ed7058367e').
narrative_ontology:cs_kernel_codification('005606ea-5333-4040-ad6d-24ed7058367e', formalized).
narrative_ontology:cs_authority_grounding('005606ea-5333-4040-ad6d-24ed7058367e', extraction).
narrative_ontology:cs_interpretation_layer_present('005606ea-5333-4040-ad6d-24ed7058367e').
narrative_ontology:cs_reading_relation('005606ea-5333-4040-ad6d-24ed7058367e', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('005606ea-5333-4040-ad6d-24ed7058367e', state_killing_legitimacy__abolition_reading, coexists_with).
narrative_ontology:cs_axiom('005606ea-5333-4040-ad6d-24ed7058367e', foundational, execution_deters_future_murders).
narrative_ontology:cs_axiom_status(execution_deters_future_murders, holdable).
narrative_ontology:cs_axiom_grounding('005606ea-5333-4040-ad6d-24ed7058367e', execution_deters_future_murders, empirically_contingent).
narrative_ontology:cs_axiom('005606ea-5333-4040-ad6d-24ed7058367e', foundational, state_may_instrumentalize_offender_for_social_end).
narrative_ontology:cs_axiom_status(state_may_instrumentalize_offender_for_social_end, holdable).
narrative_ontology:cs_axiom_grounding('005606ea-5333-4040-ad6d-24ed7058367e', state_may_instrumentalize_offender_for_social_end, instrumental).
narrative_ontology:cs_reference_frame('005606ea-5333-4040-ad6d-24ed7058367e', classical_deterrence_legitimacy).
narrative_ontology:cs_drift_state('005606ea-5333-4040-ad6d-24ed7058367e', contemporary_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('005606ea-5333-4040-ad6d-24ed7058367e', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_legitimacy_apparatus).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, executed_offenders).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, offender_families).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_deterrence_theory).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The executed offender bears the full extraction — their life is taken as the instrument of the deterrent signal. They have no exit from the constraint once sentenced to death; the appeals process is a procedural delay, not a genuine alternative. The constraint operates on them as a pure target.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, executed_offenders, payer,
    powerless, immediate, trapped, national).

% The putative beneficiaries of the deterrent signal — citizens whose safety the execution claims to protect. They are identity-locked into the social contract that purports to protect them: they cannot opt out of the state's protection regime, and the deterrence claim is framed as operating for their benefit whether they endorse it or not. The empirical reality of that protection is contested and unverifiable from their position.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, biographical, identity_locked, national).

% The prosecutorial, judicial, and executive institutions that administer capital punishment. They set the agenda (charging decisions, sentencing recommendations, execution protocols) and collect legitimacy capital from maintaining the constraint. They have arbitrage-grade exit: they could abolish the death penalty legislatively or through prosecutorial discretion, and some jurisdictions have done so. The constraint persists because they choose to maintain it, not because they cannot escape it.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_legitimacy_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, state_legitimacy_apparatus, beneficiary).

% Families of executed offenders bear collateral harm: psychological trauma, social stigma, financial costs of legal representation and funeral arrangements, and the distinctive grief of state-sanctioned killing of a family member. Their exit is constrained — they cannot prevent the execution, but they can (and do) organize for abolition, seek clemency, or relocate.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, offender_families, payer,
    powerless, biographical, constrained, local).

% Organized opponents of capital punishment who argue the deterrence claim is empirically false and the constraint is a human rights violation. They are structurally excluded from the deterrence framework — the framework defines them as irrelevant to the calculation of social benefit. They have analytical exit (they can reject the framework entirely) but not structural exit (they remain subject to the state that maintains the constraint).
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, analytical, national).

% Researchers who study the deterrent effect of capital punishment. Their meta-analyses and natural experiment designs produce the contested evidence base. They have full analytical exit — they can follow the evidence wherever it leads — but their findings are filtered through the political and institutional apparatus that decides whether to act on them.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, criminologists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint claims to solve the coordination problem of deterring potential murderers through a credible, state-administered threat of execution — a centralized, public signal that raises the expected cost of murder above the threshold where rational actors would choose it.
% TRANSFER_FUNCTION: Moves the offender's life (and the offender's family's wellbeing) to the state's legitimacy account and the putative protection of potential future victims. The transfer is justified as a favorable exchange: one life (the offender's) for many potential lives saved (deterred murders).
% ABSENT_VOICES: The executed offender (silenced by the constraint itself), the offender's family (marginalized in policy discourse), and jurisdictions that have abolished capital punishment without rising homicide rates (their experience is excluded from the deterrence framework's evidentiary universe). Abolitionist advocates are structurally excluded as 'irrelevant to the social calculus.'
% DISAPPEARANCE_RATIONALE: If the death penalty vanished overnight, the carceral system would reorient to life without parole as the maximum sanction; prosecutorial resources would shift from capital litigation to clearance rates and victim services; the political discourse that uses 'tough on crime' signaling would lose a primary symbol; and the empirical question of deterrence would become a historical curiosity rather than live policy driver.
% FOUNDING_PROBLEM: In the mid-20th century, rising homicide rates and limited carceral alternatives created a perceived need for an ultimate sanction that could credibly threaten the worst offenders. The death penalty was maintained and reinstated (post-Furman v. Georgia, 1976) as a rational deterrent signal in an era when long-term incapacitation was seen as unreliable and rehabilitation had lost credibility.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state attorneys general, victims' rights organizations) attest the problem is live, citing ongoing murders and the need for ultimate accountability. Opponents (criminologists, human rights organizations, abolitionist legislators) attest the problem is substantially solved: homicide rates have declined globally independent of death penalty status, life without parole provides reliable incapacitation, and the deterrence evidence base does not support the claim. No independent corroborating source outside the benefiting parties confirms the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the contested empirical basis: if deterrence works, the offender's life buys real protection (coordination); if it doesn't, the extraction is pure waste (snare). The moderate ε encodes this uncertainty. Suppression (0.72) is high because the constraint's persistence depends on legal barriers to abolition, prosecutorial discretion seeking death sentences, and cultural narratives that frame execution as necessary for safety — not on voluntary participant preference. Theater ratio (0.31) captures the gap between the stated rationale (rational deterrence) and the operational reality (retributive satisfaction, political signaling, racialized application). The temporal series shows rising extractiveness and suppression from 1970–2025 as the empirical case weakened but institutional commitment hardened — the constraint became more extractive while its coordination justification eroded.
 *
 * PERSPECTIVAL GAP:
 *   From the state_legitimacy_apparatus seat, the constraint appears as genuine coordination (protecting citizens) with acceptable costs. From the executed_offender seat, it is pure extraction with zero exit. From the potential_future_victim seat, it is a claimed protection whose reality they cannot verify. The engine computes these divergent classifications from the structural data: the apparatus has arbitrage-grade exit (can abolish), the offender is trapped, the potential victim is identity-locked into the social contract that purports to protect them.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_legitimacy_apparatus is the structural beneficiary (d ≈ 0.15): it collects legitimacy, political capital, and institutional self-justification. Executed_offenders are the structural target (d ≈ 0.95): they bear the full extraction with no exit. Potential_future_victims are declared beneficiaries but their directionality is ambiguous (d ≈ 0.45): they receive putative protection but the empirical basis is contested and they cannot opt out of the social contract. Offender_families are secondary victims (d ≈ 0.8): collateral extraction with constrained exit. Abolitionist_advocates are excluded (d ≈ 0.7): they bear the cost of a system they oppose but have analytical exit. The derivation chain reads beneficiary/victim declarations + power + exit to produce these d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deterring murder in an era of high homicide rates and weak carceral alternatives) is contested — homicide rates have declined globally regardless of execution status, and life without parole provides incapacitation without killing. The arrangement persists despite the founding problem's erosion, suggesting mandatrophy: the constraint's mandate (deterrence) has outlived its empirical function but is maintained through institutional inertia and political theater. The tangled_rope classification prevents mislabeling this as pure coordination (rope) or pure extraction (snare) — it is a hybrid where the coordination story is the cover for extraction that has become partially autonomous from its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_evidence_base,
    'Does the empirical evidence actually support the claimed deterrent effect, or is the evidence base too weak to sustain the claim?',
    'Systematic meta-analysis of natural experiments (moratorium periods, cross-jurisdictional variation, within-state abolition/reinstatement cycles) controlling for confounding variables.',
    'If the deterrent effect is empirically unsubstantiated, the constraint''s coordination function collapses and it reclassifies toward snare; if substantiated, the tangled_rope classification holds with moderate ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_evidence_base, empirical, 'Whether the deterrence claim has empirical grounding or is a cover story').

omega_variable(
    kernel_reading_dispute,
    'Is state_killing_legitimacy a single kernel with competing readings, or are these structurally distinct constraints sharing only a label?',
    'Test whether ε and beneficiary/victim structure differ irreducibly across the three declared readings. If they do, the kernel decomposition is valid; if not, the readings are perspectival variations on one constraint.',
    'If the kernel is a single constraint, the engine should compute one classification with perspectival divergence; if three constraints, each has independent ε and classification. The current declaration assumes three distinct constraints linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_dispute, conceptual, 'Whether the kernel frame or the ε-invariance decomposition is correct').

omega_variable(
    instrumentalization_threshold,
    'At what point does instrumentalizing an offender as a deterrent signal cross from acceptable state action into prohibited use of a person as mere means?',
    'Normative analysis of the proportionality between the claimed deterrent benefit and the severity of instrumentalization, tested against constitutional and human rights jurisprudence across jurisdictions.',
    'If instrumentalization is categorically impermissible, the deterrence reading forecloses within a rights-based framework; if it is proportionally bounded, the reading coexists with retributive and abolitionist readings as a live policy option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumentalization_threshold, conceptual, 'The normative boundary of permissible instrumentalization').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers to abolition, institutional inertia) or internalized (public belief that execution is necessary for safety)?',
    'Post-abolition suppression trajectory: if public demand for execution persists after legal abolition, reclassify as partially internalized; if demand evaporates, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the public carries the suppression with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_killing_deterrence_tr_t1970, state_killing_legitimacy__deterrence_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(state_killing_deterrence_tr_t1985, state_killing_legitimacy__deterrence_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(state_killing_deterrence_tr_t1995, state_killing_legitimacy__deterrence_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(state_killing_deterrence_tr_t2005, state_killing_legitimacy__deterrence_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(state_killing_deterrence_tr_t2015, state_killing_legitimacy__deterrence_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(state_killing_deterrence_tr_t2025, state_killing_legitimacy__deterrence_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(state_killing_deterrence_be_t1970, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(state_killing_deterrence_be_t1985, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(state_killing_deterrence_be_t1995, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(state_killing_deterrence_be_t2005, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(state_killing_deterrence_be_t2015, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(state_killing_deterrence_be_t2025, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(state_killing_deterrence_su_t1970, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(state_killing_deterrence_su_t1985, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(state_killing_deterrence_su_t1995, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1995, 0.67).
narrative_ontology:measurement(state_killing_deterrence_su_t2005, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(state_killing_deterrence_su_t2015, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(state_killing_deterrence_su_t2025, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_legitimacy__deterrence_reading, 0.12).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial label 'death penalty legitimacy' into three structurally distinct claims with different ε values, beneficiary/victim structures, and empirical referents. The deterrence reading's coordination function is empirical (does it deter?); the retributive reading's is normative (does the offender deserve it?); the abolition reading denies any coordination function. They are linked because the deterrence claim is often cited as evidence for the retributive claim's policy implementation, and the abolition claim targets both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, powerless, 0.95).
constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, institutional, 0.15).
constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, organized, 0.7).
constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, analytical, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
