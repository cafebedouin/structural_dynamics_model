% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
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
 *   human_readable: Contingent-Choice Reading of the Bretton Woods Suspension (1971)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the contingent-choice reading of the
 *   transition_causality kernel: the August 1971 suspension of dollar-gold
 *   convertibility ('the Nixon Shock') was a discretionary policy decision,
 *   not a structurally forced outcome. Under this reading, the Nixon
 *   administration had genuinely available alternatives — coordinated
 *   devaluation, a negotiated multilateral gold-price adjustment, continued
 *   rationing of gold sales — and chose unilateral, unannounced suspension
 *   for reasons of domestic political timing (the 1972 election cycle, the
 *   desire to avoid the political cost of overt devaluation). The reading
 *   treats Nixon's Camp David decision as the primary causal node: change
 *   that one weekend's choice and the transition path, timing, and
 *   distribution of costs would plausibly differ. This is explicitly ONE
 *   reading among three siblings (hybrid_trigger_reading,
 *   overdetermined_collapse_reading) that instantiate different, non-averaged
 *   constraints from the same historical episode; ε here (0.58) reflects the
 *   extraction this reading attributes to the standing arrangement — an
 *   autonomy gain purchased by unilaterally imposed, unconsulted
 *   cost-shifting onto treaty partners — not any endorsed counterfactual
 *   alternative.
 *
 * KEY AGENTS:
 *   - Nixon administration domestic agenda: primary agenda-setter and immediate beneficiary of the unilateral decision
 *   - US Treasury policy autonomy: institutional beneficiary of the resulting discretion
 *   - Bretton Woods partner central banks: primary payers, informed after the fact
 *   - Gold-pegged currency holders and fixed-exchange exporters: diffuse payers absorbing realignment costs
 *   - International monetary diplomats: excluded voices whose negotiated alternative was foreclosed
 *   - Monetary historians: analytical observers assessing counterfactual viability
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
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Contingent-Choice Reading of the Bretton Woods Suspension (1971)").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '68ce8859-59cb-4ea2-9ceb-75aae4115a60').
narrative_ontology:cs_kernel_codification('68ce8859-59cb-4ea2-9ceb-75aae4115a60', distributed).
narrative_ontology:cs_authority_grounding('68ce8859-59cb-4ea2-9ceb-75aae4115a60', distributed).
narrative_ontology:cs_reading_relation('68ce8859-59cb-4ea2-9ceb-75aae4115a60', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('68ce8859-59cb-4ea2-9ceb-75aae4115a60', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('68ce8859-59cb-4ea2-9ceb-75aae4115a60', foundational, counterfactual_alternatives_were_genuinely_executable).
narrative_ontology:cs_axiom_status(counterfactual_alternatives_were_genuinely_executable, holdable).
narrative_ontology:cs_axiom_grounding('68ce8859-59cb-4ea2-9ceb-75aae4115a60', counterfactual_alternatives_were_genuinely_executable, empirically_contingent).
narrative_ontology:cs_axiom('68ce8859-59cb-4ea2-9ceb-75aae4115a60', foundational, individual_decision_bears_primary_causal_weight).
narrative_ontology:cs_axiom_status(individual_decision_bears_primary_causal_weight, holdable).
narrative_ontology:cs_axiom_grounding('68ce8859-59cb-4ea2-9ceb-75aae4115a60', individual_decision_bears_primary_causal_weight, empirically_contingent).
narrative_ontology:cs_created_at('68ce8859-59cb-4ea2-9ceb-75aae4115a60', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_treasury_policy_autonomy).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, nixon_administration_domestic_agenda).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_dollar_denominated_debtors).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, bretton_woods_partner_central_banks).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, gold_pegged_currency_holders).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_exchange_rate_dependent_exporters).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, policy_contingency_thesis).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, great_man_causal_primacy_in_monetary_history).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faced a domestic re-election calendar, rising inflation, and unemployment; chose to suspend gold convertibility unilaterally over the weekend of August 1971 rather than pursue coordinated devaluation, deflation, or continued gold drain. Retained full discretion over timing, framing ('temporary'), and sequencing, and captured the domestic political benefit of appearing decisive without prior consultation with treaty partners.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, nixon_administration_domestic_agenda, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, nixon_administration_domestic_agenda, beneficiary).

% Gained freedom to run independent monetary policy and deficit-finance without the gold-convertibility constraint that had bound it to a fixed exchange commitment. This autonomy is the direct product of the choice being made when and how it was, rather than through a negotiated multilateral wind-down.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_treasury_policy_autonomy, beneficiary,
    institutional, generational, arbitrage, global).

% U.S. borrowers, including the federal government, benefited from the dollar's continued reserve-currency role even absent gold backing, effectively socializing the adjustment cost onto dollar-holding foreign creditors. This benefit depended on the specific unilateral form the decision took.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_dollar_denominated_debtors, beneficiary,
    powerful, generational, mobile, global).

% Held dollar reserves accumulated under the fixed-convertibility promise and were informed of the suspension after the fact, with no negotiated transition mechanism. Absorbed the resulting currency realignment and reserve-value uncertainty; had no advance opportunity to hedge or adjust because the decision was made and announced as a fait accompli by one party.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, bretton_woods_partner_central_banks, payer,
    organized, biographical, constrained, continental).

% Savers and institutions in gold-linked currency systems saw the peg's meaning collapse overnight. They had structured savings and contracts around convertibility that a different, more gradual policy path could plausibly have preserved or phased out with notice.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_pegged_currency_holders, payer,
    moderate, biographical, trapped, national).

% Export-dependent economies (notably Japan and parts of Europe) had priced competitiveness on the assumption of fixed parities. The sudden unilateral suspension forced abrupt currency appreciation and adjustment costs they had not built contingency plans for, because the alternative — a negotiated, telegraphed transition — was foreclosed by the manner of the decision.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, fixed_exchange_rate_dependent_exporters, payer,
    moderate, biographical, constrained, national).

% Multilateral institutions and allied finance ministries that could have been consulted on a phased transition were not brought into the decision before it was announced. Their exclusion is precisely what makes the contingent-choice reading legible: a different, more consultative process was institutionally available and was not used.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_diplomats, excluded,
    organized, biographical, trapped, global).

% Assess archival records of the Camp David meetings, Treasury memos, and available policy alternatives (e.g. Volcker's own contemporaneous proposals for a slower multilateral approach) to evaluate whether the suspension was the only viable path or one option among several that were seriously considered and rejected for political-timing reasons.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a mechanism by which the U.S. could unilaterally end a convertibility commitment that had become domestically costly, without needing prior multilateral agreement — solving the U.S. administration's immediate political-economic problem of gold drain and election-cycle pressure.
% TRANSFER_FUNCTION: Moves adjustment costs from the U.S. federal government and dollar-denominated borrowers onto foreign central banks holding dollar reserves and onto gold-linked and export-dependent economies who absorbed the resulting currency realignment without notice or negotiated compensation.
% ABSENT_VOICES: Allied finance ministries and multilateral monetary institutions (IMF staff, G10 counterparts) who had proposed phased or coordinated alternatives were not consulted before the announcement; their preferred alternative — a negotiated wind-down — was foreclosed by the timing and unilateral form of the decision.
% DISAPPEARANCE_RATIONALE: If this reading is correct — that a different set of choices was genuinely available and would have produced a materially different transition path (slower, negotiated, less costly to partner economies) — then the actual historical outcome (permanent float, absorbed reserve losses, U.S. policy autonomy gain) is not the only possible world; a counterfactual negotiated transition would have redistributed costs and benefits differently among the same set of parties.
% FOUNDING_PROBLEM: The U.S. needed to address a widening gap between its gold reserves and outstanding dollar claims abroad (the 'Triffin dilemma' made acute by Vietnam-era and Great Society deficit spending) without accepting the domestic political cost of deflation or devaluation negotiated through Bretton Woods machinery.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Federal Reserve and IMF historical retrospectives, along with declassified Treasury and NSC memoranda (including Paul Volcker's own later accounts and academic monetary historians outside the Nixon administration's circle), corroborate that the acute gold-drain problem the suspension was framed as solving no longer exists under the floating-rate regime it produced — the arrangement's original justification has been superseded by fifty years of a fiat-dollar reserve system that persists for different reasons (network effects, deep capital markets) than the ones that motivated 1971.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises sharply at 1971 (0.58) reflecting the moment of unilateral action and its immediate cost-shifting onto reserve-holding partners, then eases slightly through 1974-76 as the float regime stabilizes and some costs diffuse into normal exchange-rate risk rather than acute transfer. Theater ratio spikes at the announcement (0.55) — the 'temporary suspension' framing was itself theatrical cover for what this reading holds was a foreseeable-to-avoidable permanent shift — then declines as the float becomes the accepted steady state and less performative justification is needed. Suppression is moderate (0.42): there was no coercive apparatus preventing partner central banks from objecting, but the fait-accompli structure of the announcement functioned as a soft suppression of negotiated alternatives by foreclosing the time window in which they could have been raised.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nixon administration and U.S. Treasury sit at the beneficiary end: the decision was theirs to make, they captured the policy-autonomy gain, and their exit options were effectively arbitrage-grade (they set the terms of exit from the old regime). Partner central banks and gold-linked/export-dependent economies sit at the target end: they held reserves and contractual expectations structured around a promise unilaterally revoked, with constrained or trapped exit options because the decision was made and announced without their input. This reading's emphasis on contingency sharpens directionality relative to a structural-inevitability reading, because 'it could have gone otherwise' implies the cost distribution the actual choice produced is attributable to that specific choice rather than to impersonal forces.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (acute gold-drain threatening reserve adequacy) is genuinely dead — the floating-rate dollar system that emerged solves a different problem (liquidity and reserve-currency demand) via different mechanisms than convertibility ever did. Under this reading, the persistence of dollar hegemony past the original justification is not itself mandatrophic, because the reading holds that the choice-driven origin was a one-time discretionary act with lasting distributive consequences, not an ongoing coordination structure requiring continuous re-justification. The tangled_rope classification captures that a real coordination function existed at the founding (some resolution of the Triffin dilemma was needed) alongside asymmetric extraction in how that resolution was executed and who absorbed its costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_negotiated_transition,
    'Was a negotiated, multilateral wind-down of gold convertibility (e.g., along lines Volcker or IMF staff reportedly floated) a genuinely executable alternative in 1971, or would structural pressures (deficit financing needs, European dollar overhang, domestic political timing) have forced some form of unilateral break regardless of who held office?',
    'Comparative institutional analysis of contemporaneous multilateral proposals actually on the table in 1971 (declassified G10 and IMF records), assessed against the domestic political and fiscal constraints the administration faced, to determine whether the negotiated path was foreclosed by structural factors or merely unchosen.',
    'If the negotiated alternative was genuinely viable and merely unchosen, this reading''s contingent-choice framing holds and the extraction attributed to the unilateral form is fully attributable to the decision. If structural factors would have foreclosed any negotiated path regardless of the specific administration in office, this reading collapses toward the overdetermined_collapse_reading or hybrid_trigger_reading, and the beneficiary structure centered on ''Nixon''s choice'' becomes overstated relative to impersonal structural forces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_negotiated_transition, empirical, 'Whether a negotiated multilateral alternative to unilateral suspension was structurally available in 1971.').

omega_variable(
    great_man_historiography_selection_bias,
    'Does the contingent-choice framing reflect genuine causal analysis, or does it reflect a historiographic preference (common in diplomatic and political history) for personalizing structural monetary crises around identifiable decision-makers?',
    'Cross-disciplinary comparison between political-history accounts (which tend to center Nixon, Connally, and the Camp David weekend) and international-political-economy / monetary-economics accounts (which tend to center Triffin-dilemma structural dynamics), assessing whether the disciplinary framing itself predicts which causal reading is favored.',
    'If the contingent-choice reading is disproportionately favored by disciplines with a narrative/agency bias rather than by the underlying evidence, its claimed_type and beneficiary-centering should be read with that provenance in mind — the reading remains valid as one instantiation of the kernel, but its confidence should be discounted for disciplinary selection effects rather than treated as a neutral empirical conclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_man_historiography_selection_bias, conceptual, 'Whether the contingent-choice reading is partly an artifact of historiographic disciplinary framing rather than pure causal reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1965, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__contingent_choice_reading, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement(tran_tr_t1972, transition_causality__contingent_choice_reading, theater_ratio, 1972, 0.5).
narrative_ontology:measurement(tran_tr_t1974, transition_causality__contingent_choice_reading, theater_ratio, 1974, 0.35).
narrative_ontology:measurement(tran_tr_t1976, transition_causality__contingent_choice_reading, theater_ratio, 1976, 0.3).

% Extraction over time
narrative_ontology:measurement(tran_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(tran_be_t1968, transition_causality__contingent_choice_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement(tran_be_t1972, transition_causality__contingent_choice_reading, base_extractiveness, 1972, 0.6).
narrative_ontology:measurement(tran_be_t1974, transition_causality__contingent_choice_reading, base_extractiveness, 1974, 0.55).
narrative_ontology:measurement(tran_be_t1976, transition_causality__contingent_choice_reading, base_extractiveness, 1976, 0.5).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(transition_causality__contingent_choice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the transition_causality kernel concerning the 1971 Bretton Woods suspension. contingent_choice_reading treats Nixon's decision as the primary causal node with high counterfactual viability and centers beneficiary structure on U.S. policy-autonomy gain. hybrid_trigger_reading treats structural contradictions as necessary-but-not-sufficient, requiring a contingent trigger. overdetermined_collapse_reading treats the transition as structurally inevitable regardless of agency. Each reading authors its own ε, beneficiary/victim structure, and classification per DP-001 ε-invariance; they are linked here for contamination-propagation analysis, not averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
