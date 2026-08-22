% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Bretton Woods Suspension as Contingent Nixon Policy Choice
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint instantiates the CONTINGENT CHOICE reading of the
 *   transition_causality kernel applied to the August 1971 suspension of
 *   dollar-gold convertibility (the 'Nixon Shock'). Under this reading, the
 *   end of Bretton Woods was a discretionary policy decision made by a small
 *   executive group among several genuinely available alternatives
 *   (multilateral renegotiation, phased devaluation, continued gold-pool
 *   cooperation, domestic austerity), not a structurally overdetermined
 *   collapse. The reading centers Nixon's Camp David decision as the primary
 *   causal node and treats the counterfactual — that different choices would
 *   have produced a materially different monetary order — as highly viable.
 *   Beneficiary structure centers on U.S. policy autonomy gain: the Fed and
 *   Treasury acquired discretionary monetary flexibility as a direct
 *   consequence of an avoidable choice, while foreign reserve holders bore
 *   costs they had not consented to and could not have anticipated would be
 *   imposed unilaterally. This is one of three sibling readings of the same
 *   kernel; the overdetermined_collapse_reading treats the same 1971 event as
 *   structurally forced by accumulated contradictions (Triffin dilemma, U.S.
 *   deficit spending, gold-pool exhaustion), and the hybrid_trigger_reading
 *   treats structural pressure as necessary but not sufficient, requiring the
 *   contingent Camp David decision as the actualizing trigger. Each reading
 *   authors its own epsilon over the same standing historical episode; they
 *   are not measurement variants of one constraint but three structurally
 *   distinct constraints sharing a kernel.
 *
 * KEY AGENTS:
 *   - nixon_administration_officials: primary decision-making agent, chose among live alternatives
 *   - us_federal_reserve: institutional beneficiary of resulting monetary discretion
 *   - us_treasury_policy_apparatus: institutional beneficiary of deficit-financing flexibility
 *   - foreign_dollar_reserve_holders: bore uncompensated devaluation cost
 *   - bretton_woods_treaty_partners: excluded from the decision process despite treaty stake
 *   - economic_historians_counterfactual_analysts: analytical seat evaluating counterfactual viability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.58).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.42).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Bretton Woods Suspension as Contingent Nixon Policy Choice").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '551338e6-bee8-4347-a532-ee7192f9a583').
narrative_ontology:cs_kernel_codification('551338e6-bee8-4347-a532-ee7192f9a583', distributed).
narrative_ontology:cs_authority_grounding('551338e6-bee8-4347-a532-ee7192f9a583', distributed).
narrative_ontology:cs_reading_relation('551338e6-bee8-4347-a532-ee7192f9a583', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('551338e6-bee8-4347-a532-ee7192f9a583', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('551338e6-bee8-4347-a532-ee7192f9a583', foundational, policy_choice_was_avoidable_given_documented_alternatives).
narrative_ontology:cs_axiom_status(policy_choice_was_avoidable_given_documented_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('551338e6-bee8-4347-a532-ee7192f9a583', policy_choice_was_avoidable_given_documented_alternatives, empirically_contingent).
narrative_ontology:cs_axiom('551338e6-bee8-4347-a532-ee7192f9a583', secondary, decision_maker_bears_distributional_responsibility_for_chosen_path).
narrative_ontology:cs_axiom_status(decision_maker_bears_distributional_responsibility_for_chosen_path, holdable).
narrative_ontology:cs_axiom_grounding('551338e6-bee8-4347-a532-ee7192f9a583', decision_maker_bears_distributional_responsibility_for_chosen_path, deontological).
narrative_ontology:cs_reference_frame('551338e6-bee8-4347-a532-ee7192f9a583', bretton_woods_treaty_commitment_baseline).
narrative_ontology:cs_drift_state('551338e6-bee8-4347-a532-ee7192f9a583', post_1971_floating_regime_consolidation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('551338e6-bee8-4347-a532-ee7192f9a583', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_federal_reserve).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_treasury_policy_apparatus).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, nixon_administration_officials).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, bretton_woods_treaty_partners).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, gold_convertibility_claimants).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, executive_monetary_discretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convened at Camp David in August 1971 and chose, among several available domestic and international policy options (renegotiating gold price, imposing capital controls, seeking multilateral IMF reform, or maintaining the existing peg with domestic austerity), to unilaterally suspend dollar-gold convertibility. Treasury Secretary Connally and Under Secretary Volcker prepared alternative memos; the suspension was selected for its speed and its avoidance of politically costly domestic contraction ahead of the 1972 election. The decision was made by a small executive group without congressional vote or treaty renegotiation.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, nixon_administration_officials, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, nixon_administration_officials, beneficiary).

% Gained freedom to conduct monetary policy without the external gold-convertibility constraint that had bound its balance-sheet expansion. This freedom was not automatic — it followed directly from the specific choice made in 1971 rather than from any structural necessity, since the Fed had operated under the gold-linked regime for decades prior and other paths (renegotiated parity, controlled devaluation within the system) were live options at the time.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_federal_reserve, beneficiary,
    institutional, generational, arbitrage, global).

% Retained the ability to finance deficits and geopolitical commitments (Vietnam War spending, domestic Great Society programs) without the discipline of gold outflow constraints. This reading holds that Treasury's post-1971 flexibility resulted from the choice to close the gold window rather than from an unavoidable exhaustion of alternatives — foreign central banks had continued extending informal cooperation (gold pool arrangements, swap lines) that could plausibly have been extended further with different diplomacy.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_treasury_policy_apparatus, beneficiary,
    institutional, generational, arbitrage, global).

% Central banks in France, Germany, Japan, and elsewhere held large dollar reserves accumulated under the assurance of gold convertibility at $35/oz. The unilateral suspension immediately devalued the real backing of those reserves without their consent or a renegotiated settlement. Under this reading, these actors had reasonably relied on a treaty commitment that the U.S. could have honored, deferred, or renegotiated multilaterally — the suspension was avoidable, which is precisely what made it a breach rather than a natural consequence.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, foreign_dollar_reserve_holders, payer,
    institutional, generational, trapped, global).

% Signatory governments to the Bretton Woods arrangement had built domestic and international policy around the fixed-parity gold-dollar system. The August 1971 decision was taken without prior consultation through IMF channels, foreclosing the negotiated adjustment process the treaty itself provided for. Their exit options were constrained by dependency on dollar-denominated trade and reserves accumulated over two decades.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, bretton_woods_treaty_partners, payer,
    institutional, generational, constrained, global).

% Private and institutional holders who had structured contracts or expectations around dollar-gold convertibility lost that option overnight with no transition mechanism or compensation. From this reading's standpoint, alternative transition designs (phased devaluation, temporary suspension with a announced return date, multilateral renegotiation) were available and rejected in favor of the fastest unilateral option.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_convertibility_claimants, payer,
    moderate, biographical, trapped, national).

% Assess archival records (Camp David memos, Treasury option papers, Fed correspondence) to evaluate whether the alternatives Connally and Volcker drafted were genuinely viable or merely staged options. Their work is central to adjudicating this reading's central counterfactual claim.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, economic_historians_counterfactual_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, us_treasury_policy_apparatus).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The suspension decision, on this reading, coordinated a small executive circle's response to balance-of-payments pressure by selecting one of several genuinely available policy paths — it solved the immediate problem of avoiding a politically costly domestic austerity program before the 1972 election, not an unavoidable structural collapse.
% TRANSFER_FUNCTION: Moves the real burden of dollar devaluation from the U.S. domestic economy (which avoided austerity or a renegotiated parity) onto foreign reserve holders and treaty partners who held dollars in reliance on the pre-existing convertibility commitment.
% ABSENT_VOICES: Foreign central banks and IMF technical staff who had proposed multilateral adjustment mechanisms were not part of the Camp David decision group; their alternative proposals for phased or negotiated adjustment are largely absent from the standard causal narrative that treats the suspension as inevitable.
% DISAPPEARANCE_RATIONALE: If the contingent-choice reading is correct and the decision could have been avoided, then a different 1971 choice (renegotiated parity, multilateral reform, continued gold-pool cooperation) would have produced a materially different international monetary order — the world does not converge to the same floating-rate regime by a different route; the specific institutional and distributional consequences (dollar's continued reserve-currency dominance without gold discipline, U.S. deficit-financing flexibility) trace to this specific choice.
% FOUNDING_PROBLEM: The U.S. faced a growing gap between its gold reserves and outstanding dollar liabilities held abroad (the Triffin dilemma), combined with domestic inflationary pressure from Vietnam War and social program spending, creating balance-of-payments strain that some in the administration argued required a response before a full convertibility run occurred.
% FOUNDING_PROBLEM_CORROBORATION: Treasury officials who selected the suspension (Connally) and later commentary from Volcker (who drafted rejected multilateral alternatives at the time) both attest that other options were on the table and consciously set aside — corroboration from within the decision-making apparatus itself, not only from outside critics. External corroboration comes from economic historians (e.g., work drawing on declassified Camp David records) documenting the existence of the rejected alternative memos, which supports the claim from outside the beneficiary set that the decision was a choice among live options rather than a forced move.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 — moderate-to-substantial, reflecting a real transfer of cost from U.S. domestic policy space to foreign reserve holders, but tempered because this reading holds the transfer was avoidable rather than structurally forced (an avoidable extraction is still extraction, but the counterfactual availability of alternatives means the constraint's persistence today depends more on subsequent institutional path-dependency than on any ongoing coercive necessity). Suppression (0.42) is moderate: the initial decision was made without consultation (a suppression of alternative voices/process), but the post-1971 floating regime has not required substantial ongoing coercion to maintain — its persistence rests more on convenience and institutional lock-in than active suppression. Theater ratio rises from 0.2 to 0.4 over the measured interval as post-hoc historical narratives increasingly frame the decision as 'inevitable' (a legitimating performance) even though this reading holds the decision was genuinely contingent — the theater is in the retrospective inevitability narrative, not in the original decision-making process itself. Resistance (0.55) reflects substantial pushback from European governments (particularly France) both before and after the decision, and ongoing historiographical contestation about whether it was avoidable.
 *
 * PERSPECTIVAL GAP:
 *   From the Nixon administration's own seat (and much subsequent U.S.-centered historiography), the decision reads as prudent crisis management — a coordination achievement that prevented a disorderly collapse. From the foreign reserve holder and treaty partner seats, the same act reads as a unilateral breach imposed by a party with the power to choose otherwise and did not consult those who would bear the cost. The engine computes these as different seat-level classifications from the same structural data (power, exit options, beneficiary/victim declarations) — this reading does not adjudicate between them, only asserts the counterfactual structure (avoidability) that makes the divergence sharper than it would be under a pure-inevitability reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nixon administration officials and the institutions they controlled (Fed, Treasury) are the structural beneficiaries under this reading — they gained discretionary policy space through a choice they made and could have made differently. Foreign reserve holders and treaty partners are targets: they bore the devaluation cost without having chosen it and without an available exit (reserves already held could not be un-held; trapped exit options reflect this). This reading's directionality claim is sharper than the overdetermined_collapse reading's would be, precisely because contingency implies the U.S. actors bore responsibility for a choice rather than merely transmitting an unavoidable structural pressure — which is why this reading's beneficiary structure centers so specifically on U.S. policy autonomy gain rather than on all parties adjusting to unavoidable circumstance.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists two collapse errors: treating the 1971 decision as pure structural necessity (which would erase the contingent-choice agency and absolve the decision-makers of the distributional consequences of their specific choice) and treating it as pure arbitrary extraction with no underlying coordination problem (the Triffin dilemma and balance-of-payments pressure were real pressures, even if not individually sufficient to force this particular response). The tangled_rope classification captures both: a genuine coordination problem existed (managing an unsustainable gold-dollar peg), and a specific, avoidable choice about how to solve it produced asymmetric extraction that required active enforcement (unilateral declaration, no negotiated transition) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_alternative_viability,
    'Were the alternative policy paths documented in the Connally/Volcker memos (multilateral renegotiation, phased devaluation, continued gold-pool cooperation) genuinely viable, or were they staged options already foreclosed by structural pressure by August 1971?',
    'Detailed archival reconstruction of the diplomatic feasibility of multilateral alternatives — specifically whether France, Germany, and Japan would have cooperated with a negotiated adjustment, and whether U.S. gold reserves could have sustained the peg through such negotiations without a disorderly run.',
    'If the alternatives were genuinely viable, this reading''s contingent-choice framing holds and the beneficiary structure (U.S. policy autonomy gained through an avoidable choice) is well-grounded. If the alternatives were already foreclosed by underlying structural pressure, this constraint collapses into the hybrid_trigger_reading or overdetermined_collapse_reading, and the extraction attributed here to a specific U.S. choice would instead be attributed to structural forces acting through whichever party held the decision seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_alternative_viability, empirical, 'Whether the documented policy alternatives to the 1971 suspension were structurally available or already foreclosed.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s causal claim diverge from the hybrid_trigger_reading — is the disagreement about whether structural pressure was SUFFICIENT (this reading says no, pressure alone would not have produced this outcome) or about whether the specific TIMING and FORM of the response was contingent (a narrower claim compatible with some structural necessity)?',
    'Comparative reading of the three sibling constraints against the same archival record, focused specifically on whether each reading''s beneficiary/victim structure and epsilon value are sensitive to this distinction.',
    'If the disagreement is narrow (timing/form only), this reading and hybrid_trigger_reading may converge on similar epsilon and structural data despite differing causal narratives, which would suggest the kernel decomposition is finer-grained than the underlying structural facts warrant. If the disagreement is about sufficiency, the three readings should show more divergent epsilon values, validating the decomposition into three distinct constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement between this reading and the hybrid_trigger sibling.').

omega_variable(
    beneficiary_intent_vs_structure,
    'Did the Nixon administration intend the specific distributional outcome (U.S. gaining discretionary monetary policy at the expense of foreign reserve holders), or was this an unintended consequence of a decision made primarily to avoid domestic political costs before the 1972 election?',
    'Analysis of internal administration communications for evidence of anticipated versus unanticipated international consequences.',
    'If unintended, the beneficiary classification remains structurally accurate (the administration and its institutions did in fact benefit) but the moral/political valence of the ''contingent choice'' framing shifts — a contingent choice made for unrelated domestic reasons that happened to benefit U.S. institutions reads differently than a deliberate extraction strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intent_vs_structure, empirical, 'Whether the distributional benefit to U.S. institutions was an intended or incidental consequence of the 1971 decision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__contingent_choice_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(tran_tr_t0, observed).
narrative_ontology:measurement(tran_tr_t4, transition_causality__contingent_choice_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement_basis(tran_tr_t4, observed).
narrative_ontology:measurement(tran_tr_t8, transition_causality__contingent_choice_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(tran_tr_t8, observed).
narrative_ontology:measurement(tran_tr_t12, transition_causality__contingent_choice_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(tran_tr_t12, observed).
narrative_ontology:measurement(tran_tr_t16, transition_causality__contingent_choice_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(tran_tr_t16, observed).
narrative_ontology:measurement(tran_tr_t20, transition_causality__contingent_choice_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tran_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__contingent_choice_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(tran_be_t0, observed).
narrative_ontology:measurement(tran_be_t4, transition_causality__contingent_choice_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement_basis(tran_be_t4, observed).
narrative_ontology:measurement(tran_be_t8, transition_causality__contingent_choice_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement_basis(tran_be_t8, observed).
narrative_ontology:measurement(tran_be_t12, transition_causality__contingent_choice_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(tran_be_t12, observed).
narrative_ontology:measurement(tran_be_t16, transition_causality__contingent_choice_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(tran_be_t16, observed).
narrative_ontology:measurement(tran_be_t20, transition_causality__contingent_choice_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(tran_be_t20, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(transition_causality__contingent_choice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the transition_causality kernel applied to the 1971 Bretton Woods suspension. Each sibling authors a distinct epsilon and beneficiary/victim structure reflecting a different causal ontology: this reading (contingent_choice) treats the Camp David decision as the primary causal node with high counterfactual viability for alternatives (epsilon 0.58, beneficiary structure centered on U.S. policy autonomy); overdetermined_collapse_reading treats the transition as structurally forced by accumulated contradictions (expected lower counterfactual viability, more diffuse beneficiary structure since no single actor's choice is causally decisive); hybrid_trigger_reading treats structural pressure as necessary-but-not-sufficient, requiring the contingent decision as trigger (expected intermediate values). The three are linked via affects_constraints because each reading's persistence in historiographical and policy discourse creates downstream legitimacy pressure on the others — for instance, this reading's emphasis on U.S. agency and avoidability creates argumentative pressure supporting claims for negotiated redress or reform that the overdetermined_collapse reading would treat as moot.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
