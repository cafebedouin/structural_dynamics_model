% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility â Triffin Structural Reading
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   The Bretton Woods system committed the United States to convert
 *   foreign-held dollars into gold at $35 per ounce. The Triffin structural
 *   reading treats this commitment not as a sustainable legal obligation or a
 *   conditional policy instrument, but as an inherently flawed design: the
 *   liquidity required by global trade growth could only be supplied through
 *   persistent US deficits, yet those same deficits eroded confidence in gold
 *   convertibility. Both the US monetary authority and creditor nations were
 *   trapped in an impossible trilemma until the constraint collapsed in 1971.
 *   This JSON instantiates the Triffin reading of the
 *   dollar_gold_convertibility kernel; sibling readings (strict legal
 *   convertibility, policy flexibility) are separate constraints in the same
 *   family.
 *
 * KEY AGENTS:
 *   - us_treasury: Agenda-setter and structural victim (institutional/generational/constrained) â administered the gold window while bearing the Triffin dilemma.
 *   - creditor_nations: Payer (organized/generational/constrained) â accumulated dollar reserves and faced the redemption/collapse tradeoff.
 *   - post_bretton_woods_floating_regime: Beneficiary (institutional/civilizational/analytical) â inherited monetary order after convertibility collapse.
 *   - imf: Agenda-setter secondary (institutional/generational/constrained) â enforced parity rules without authority to fix the structural flaw.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.85).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.82).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility â Triffin Structural Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, '2b1d8a92-05ba-417c-afa9-8ead7f09a58e').
narrative_ontology:cs_kernel_codification('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', formalized).
narrative_ontology:cs_authority_grounding('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', lineage).
narrative_ontology:cs_interpretation_layer_present('2b1d8a92-05ba-417c-afa9-8ead7f09a58e').
narrative_ontology:cs_reading_relation('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', foundational, liquidity_gold_convertibility_incompatible).
narrative_ontology:cs_axiom_status(liquidity_gold_convertibility_incompatible, holdable).
narrative_ontology:cs_axiom_grounding('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', liquidity_gold_convertibility_incompatible, empirically_contingent).
narrative_ontology:cs_axiom('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', foundational, systemic_revision_required).
narrative_ontology:cs_axiom_status(systemic_revision_required, holdable).
narrative_ontology:cs_axiom_grounding('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', systemic_revision_required, instrumental).
narrative_ontology:cs_reference_frame('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', fixed_par_value_system).
narrative_ontology:cs_drift_state('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', pre_nixon_shock_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('2b1d8a92-05ba-417c-afa9-8ead7f09a58e', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, us_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the dollar-gold convertibility window at $35 per ounce and bore the Triffin dilemma directly: domestic full-employment and balance-of-payments objectives became structurally incompatible as dollar liabilities to foreign central banks accumulated. Suspension was technically available but carried prohibitive credibility and alliance costs.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, us_treasury, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, us_treasury, payer).

% Accumulated dollar reserves under the Bretton Woods obligation and faced a structurally imposed choice: hold claims whose real gold backing was eroding by the Triffin dynamic, or demand conversion and trigger the systemic collapse their own reserve positions depended upon.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, payer,
    organized, generational, constrained, global).

% The floating exchange-rate order that replaced the Bretton Woods parity system. Gained institutional legitimacy, conceptual space, and policy autonomy from the demonstrated structural unsustainability of dollar-gold convertibility; its emergence required the collapse of this constraint as a causal precondition.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    institutional, civilizational, analytical, global).

% Supervised fixed-parity compliance and provided adjustment financing, but lacked authority to resolve the structural liquidity-confidence contradiction. Its enforcement activity increasingly served to delay the terminal crisis rather than to correct the underlying imbalance.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, imf, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a nominal anchor and international liquidity mechanism for post-war reconstruction and trade expansion by tying key currencies to the dollar and the dollar to gold at a fixed price.
% TRANSFER_FUNCTION: Transferred seigniorage and global liquidity-provision costs to the US balance of payments, while transferring purchasing-power risk and policy-constraint costs to creditor nations holding dollar reserves; ultimately transferred systemic legitimacy to the post-Bretton Woods floating order upon collapse.
% ABSENT_VOICES: Developing nations excluded from the original Bretton Woods design; domestic labor movements bearing the unemployment costs of balance-of-payments adjustment; gold-standard traditionalists who would have rejected the gold-exchange standard root and branch.
% DISAPPEARANCE_RATIONALE: Overnight disappearance of the convertibility commitment would have triggered immediate gold runs, parity realignments, and a scramble for alternative reserve assets; the entire fixed-exchange-rate architecture and the dollar-liquidity mechanism it housed depended on this constraint.
% FOUNDING_PROBLEM: Post-war absence of a stable international monetary standard capable of financing reconstruction and trade without returning to 1930s competitive devaluations, exchange controls, or closed currency blocs.
% FOUNDING_PROBLEM_CORROBORATION: Triffin (1960) attested the structural flaw from outside the US Treasury and IMF circles. Subsequent IMF historians, macroeconomic scholarship, and the 1971 collapse itself corroborate that the liquidity mechanism carried the seeds of its own destruction.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.40 to 0.85 over the interval as the liquidity-confidence contradiction deepened with Eurodollar growth and Vietnam-era deficits. Suppression is high (0.82) because the constraint's persistence depended on active US defense of the gold peg via the London Gold Pool, swap lines, and capital controls, plus coordinated creditor restraint from mass redemption. Theater ratio rises to 0.65 because an increasing share of official activity after 1960 served to maintain the pretense of a sustainable peg that structural analysis showed was doomed. Accessibility collapse is high (0.75) because institutional lock-in and ideological commitment to fixed rates made the floating alternative politically unavailable until forced collapse. Resistance is moderate (0.55): academic critics and some creditor governments (notably France) mounted sustained criticism, but creditor-nation coordination failed to produce a negotiated exit before the 1971 shock.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury seat in 1944 the arrangement appeared as a rope (liquidity provision for global recovery). From the same seat in 1968 it appeared as a snare (domestic policy held hostage by the foreign dollar overhang). From the creditor-nation seat it consistently appeared as a trap: financing US deficits or demanding gold and destroying the reserve asset they held. The engine computes these divergences from power and exit asymmetries without adjudicating which perception is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury sits as agenda-setter because it administered the gold window and set the rules, yet it is declared a victim because the convertibility commitment structurally constrained US macroeconomic policy (directionality near the target end despite administrative control). Creditor nations are clear targets (directionality near 1.0): they bore purchasing-power risk and faced the choice of holding depreciating claims or triggering systemic collapse. The post-Bretton Woods floating regime is the beneficiary (directionality near 0.0): it gained institutional space and legitimacy from the demonstrated unsustainability of the fixed peg. The IMF sits in between, enforcing a system it could not reform.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem â post-war liquidity shortage â was solved only transiently. By the Triffin reading the solution carried the seeds of its own destruction. Mandatrophy is resolved (the constraint collapsed in 1971) but the JSON records the pre-collapse state. The classification as tangled rope prevents mislabeling: it acknowledges the genuine coordination function (dollar liquidity enabled post-war trade reconstruction) while registering the asymmetric extraction (policy autonomy and wealth transferred into an unsustainable structure). A pure rope reading would ignore the Triffin extraction; a pure snare reading would deny the liquidity coordination that made the system politically possible in 1944.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_triffin,
    'Does the Triffin structural reading describe an inherent economic law of reserve-currency systems or a contingent institutional failure of the gold-exchange standard?',
    'Comparative historical analysis of reserve-currency systems beyond Bretton Woods (sterling, euro) to test whether the liquidity-confidence trilemma recurs independently of the gold-peg design.',
    'If the dilemma is inherent to any reserve-currency peg, the constraint approaches mountain-like status for monetary architects; if contingent on the gold-exchange design, it remains a constructed tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_triffin, conceptual, 'Whether Triffin dilemma is natural law or constructed flaw').

omega_variable(
    post_bw_beneficiary_ambiguity,
    'Can a successor regime that only fully exists after constraint collapse be a structural beneficiary of the constraint''s operation, or is it merely an institutional inheritor?',
    'Analyze whether the post-Bretton Woods order''s legitimacy and policy space required the convertibility collapse as a causal precondition, or whether the same regime could have emerged through negotiated reform.',
    'If merely inheritor, the beneficiary set during operation is effectively empty and the constraint reads closer to a piton or snare with diffuse capture; if true beneficiary, the tangled-rope framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_bw_beneficiary_ambiguity, conceptual, 'Temporal ambiguity of post-BW regime beneficiary status').

omega_variable(
    enforcement_mechanism_decomposition,
    'Was the constraint''s persistence driven by active institutional enforcement (IMF surveillance, gold pools, swap lines) or by the absence of a coordinated exit alternative among creditor nations?',
    'Game-theoretic analysis of creditor-nation coordination during the 1960s London Gold Pool and subsequent bilateral negotiations.',
    'If absence-of-exit dominated, creditor-nation directionality is higher (more trapped) and suppression may be overstated; if active enforcement dominated, the high suppression score is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_decomposition, empirical, 'Active enforcement versus coordination-failure trap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t0, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(doll_tr_t5, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(doll_tr_t10, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(doll_tr_t15, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(doll_tr_t20, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(doll_tr_t27, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 27, 0.65).

% Extraction over time
narrative_ontology:measurement(doll_be_t0, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(doll_be_t5, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(doll_be_t10, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(doll_be_t15, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(doll_be_t20, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(doll_be_t27, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 27, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t0, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(doll_su_t5, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(doll_su_t10, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(doll_su_t15, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(doll_su_t20, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(doll_su_t27, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 27, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the dollar_gold_convertibility kernel, decomposed per the epsilon-invariance principle. Sibling constraints (strict_convertibility_reading, policy_flexible_reading) instantiate structurally distinct claims from the same natural-language label. They are linked by the kernel committer frame rather than by causal influence edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
