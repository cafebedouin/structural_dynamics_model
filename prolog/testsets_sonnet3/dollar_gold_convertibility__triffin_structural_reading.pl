% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [RESOLVED MANDATROPHY]
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Bretton Woods Dollar-Gold Convertibility as Structurally Unsustainable Design (Triffin Dilemma Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   Between the late 1950s and 1971, the Bretton Woods system's core
 *   mechanism — the U.S. dollar fixed to gold at $35/oz with other currencies
 *   pegged to the dollar — came under mounting strain as U.S.
 *   balance-of-payments deficits (needed to supply global dollar liquidity)
 *   eroded confidence in the U.S. gold reserve's capacity to honor
 *   convertibility claims at scale. Robert Triffin's 1960 diagnosis held that
 *   this was not a failure of will or discipline but a structural
 *   impossibility: the reserve-currency issuer must run deficits to supply
 *   liquidity, which necessarily undermines the confidence the fixed
 *   convertibility rate depends on. This reading treats the eventual 1971
 *   suspension of convertibility (the Nixon Shock) as the predictable
 *   terminus of an internally contradictory design, not a policy betrayal.
 *
 * KEY AGENTS:
 *   - united_states_treasury: primary structural victim of the trilemma — cannot satisfy liquidity supply and confidence maintenance simultaneously
 *   - european_creditor_nations: secondary structural victim — hold convertibility claims that accelerate the very collapse they seek to guard against by demanding redemption
 *   - developing_country_dollar_holders: powerless, excluded from negotiation, absorb the eventual devaluation cost
 *   - post_bretton_woods_floating_regime_architects: beneficiary of the diagnosis and its resolution — gain institutional authority and vindication
 *   - imf_bretton_woods_secretariat: agenda-setter with formal administrative power but no enforcement capacity to resolve the underlying contradiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.81).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.62).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Bretton Woods Dollar-Gold Convertibility as Structurally Unsustainable Design (Triffin Dilemma Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'a89549fa-f075-4fff-8195-2e55fbcf3dd7').
narrative_ontology:cs_kernel_codification('a89549fa-f075-4fff-8195-2e55fbcf3dd7', formalized).
narrative_ontology:cs_authority_grounding('a89549fa-f075-4fff-8195-2e55fbcf3dd7', extraction).
narrative_ontology:cs_interpretation_layer_present('a89549fa-f075-4fff-8195-2e55fbcf3dd7').
narrative_ontology:cs_reading_relation('a89549fa-f075-4fff-8195-2e55fbcf3dd7', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('a89549fa-f075-4fff-8195-2e55fbcf3dd7', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('a89549fa-f075-4fff-8195-2e55fbcf3dd7', foundational, convertibility_structurally_impossible_under_reserve_currency_role).
narrative_ontology:cs_axiom_status(convertibility_structurally_impossible_under_reserve_currency_role, holdable).
narrative_ontology:cs_axiom_grounding('a89549fa-f075-4fff-8195-2e55fbcf3dd7', convertibility_structurally_impossible_under_reserve_currency_role, empirically_contingent).
narrative_ontology:cs_axiom('a89549fa-f075-4fff-8195-2e55fbcf3dd7', secondary, design_failure_precludes_culpability_attribution).
narrative_ontology:cs_axiom_status(design_failure_precludes_culpability_attribution, holdable).
narrative_ontology:cs_axiom_grounding('a89549fa-f075-4fff-8195-2e55fbcf3dd7', design_failure_precludes_culpability_attribution, conventional).
narrative_ontology:cs_reference_frame('a89549fa-f075-4fff-8195-2e55fbcf3dd7', bretton_woods_par_value_system).
narrative_ontology:cs_drift_state('a89549fa-f075-4fff-8195-2e55fbcf3dd7', smithsonian_agreement_1971, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('a89549fa-f075-4fff-8195-2e55fbcf3dd7', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, european_creditor_nations).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, developing_country_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, gold_bloc_speculators).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, impossible_trinity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound to redeem dollars for gold at a fixed price while simultaneously needing to run persistent balance-of-payments deficits to supply the world's reserve currency and liquidity. Cannot satisfy both the confidence requirement (limited dollar issuance) and the liquidity requirement (expanding dollar issuance) at once. Every path — deflate domestically, suspend convertibility, or keep issuing — imposes costs it cannot avoid once the system is running; by the late 1960s gold reserves are structurally insufficient to cover outstanding dollar liabilities regardless of policy choice.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, united_states_treasury, payer,
    institutional, generational, trapped, global).

% Accumulate dollar reserves as the price of participating in the postwar trade and payments system, but those reserves are convertibility claims on a gold stock that cannot possibly cover them. Nations like France press for redemption precisely because they see the arithmetic failing, which accelerates the collapse they are trying to protect themselves from. They cannot exit the dollar system without destabilizing their own trade financing, but staying in means holding claims of declining real value.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, european_creditor_nations, payer,
    institutional, generational, constrained, continental).

% Hold dollar reserves for trade settlement with no capacity to influence U.S. monetary policy, no gold-window access in practice comparable to major creditors, and no alternative reserve infrastructure. They absorb the eventual devaluation and the post-collapse volatility of floating rates without having had any voice in the system's design or its unwinding.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, developing_country_dollar_holders, payer,
    powerless, generational, trapped, global).

% Academic economists (Triffin among the first), central bank technocrats, and eventually the U.S. policy establishment who diagnosed the fixed-convertibility structure as unsustainable and whose preferred alternative — floating exchange rates freed from gold backing — became the system that replaced it after 1971. They gain intellectual vindication, institutional authority in the successor regime (IMF surveillance, floating-rate management), and freedom from the trilemma constraint that trapped the fixed system's operators.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects, beneficiary,
    institutional, civilizational, arbitrage, global).

% Private financial actors who correctly anticipated the convertibility structure's collapse and positioned in gold or against the dollar ahead of the 1971 suspension, profiting from the same structural failure that harmed sovereign holders. They had no formal seat in the Bretton Woods governance conversation but exploited its structural weakness from outside it.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, gold_bloc_speculators, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, gold_bloc_speculators, excluded).

% Administered the Article IV convertibility rules and par-value system, monitored deficits, and had formal authority to adjust the arrangement but lacked the enforcement power to compel the U.S. to resolve the trilemma or to compel creditor nations to stop pressing redemption. Positioned between the structural failure and the parties bearing its costs, without the tools to prevent the failure it could see building.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, imf_bretton_woods_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime_architects).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fixed dollar-gold convertibility arrangement solved the real postwar problem of establishing a trusted, universally accepted reserve and settlement medium after the collapse of the interwar gold standard and the wartime disruption of trade finance — it let nations trade and hold reserves without each bilateral pair renegotiating trust.
% TRANSFER_FUNCTION: The arrangement structurally transferred the cost of an internally contradictory design — the simultaneous demand for a stable reserve anchor and an expanding source of global liquidity — onto whichever party held the position (issuer or creditor) when the underlying arithmetic became unsustainable, with the residual cost falling hardest on non-reserve-currency dollar holders who had no negotiating leverage over the system's design or its unwinding.
% ABSENT_VOICES: Developing-country dollar holders and non-aligned states had no seat in the Bretton Woods governance structure or in the 1971 Smithsonian negotiations that repriced and eventually floated the dollar; they would have argued for a reserve asset not structurally dependent on any single nation's balance-of-payments position (echoing Triffin's own proposed remedies), but the negotiations were conducted among the major creditor governments and the United States.
% DISAPPEARANCE_RATIONALE: The convertibility mechanism did in fact disappear (August 1971, the Nixon Shock) and the world rearranged substantially: exchange rates floated, the IMF's surveillance function shifted from par-value administration to floating-rate oversight, gold was demonetized from the official reserve system over the following years, and reserve-currency-issuer discipline was no longer nominally anchored to a metallic constraint. This reading treats that rearrangement as the predictable resolution of a design that could not have persisted regardless of policy choices made within it.
% FOUNDING_PROBLEM: Postwar reconstruction needed a reserve and settlement system that avoided both the deflationary rigidity of the interwar gold standard and the chaos of freely floating, uncoordinated national currencies — a single trusted anchor asset (gold-convertible dollars) was meant to deliver stability without requiring a supranational currency.
% FOUNDING_PROBLEM_CORROBORATION: Robert Triffin's own 1960 congressional testimony and subsequent academic literature (corroborated independently by central bank historians and, retrospectively, by IMF Article IV consultation records documenting the 1960s gold-pool interventions) attest that the founding problem — providing a credible, elastic reserve asset — could not be solved by a single national currency pegged to a fixed gold stock; this diagnosis came from monetary economists and later multilateral historical review, not from the U.S. Treasury or the European creditor governments who were parties to the arrangement and had reasons to defer acknowledging the design flaw.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steadily across the interval (0.38 to 0.81) because under this reading the cost of the design flaw compounds as outstanding dollar liabilities grow relative to the fixed gold stock — the arithmetic gap widens mechanically regardless of any single actor's policy choice. Suppression is moderate and non-monotonic (peaking at 0.65 in 1971, easing slightly by 1973) because active suppression here means the coordinated efforts (gold pool interventions, swap lines, moral suasion on creditor nations not to redeem) used to postpone the collapse — suppression eases once convertibility is actually suspended and there is nothing left to defend. Theater ratio rises sharply (0.20 to 0.58) as the 1960s institutional response increasingly consisted of confidence-signaling measures (the London Gold Pool, special drawing rights negotiations, verbal commitments to defend the price) that could not address the underlying structural gap and were understood by sophisticated actors as performative even while officially maintained.
 *
 * PERSPECTIVAL GAP:
 *   The imf_bretton_woods_secretariat's seat is structurally distinct from the U.S. Treasury and creditor seats: it administers the rules and could in principle propose revision, but has no enforcement power over either party's underlying behavior — it experiences the constraint as agenda-setting without capacity to resolve the contradiction, closer to a helpless referee than to a beneficiary or a captured victim. The post_bretton_woods_floating_regime_architects seat computes very differently: for them the same historical episode is not extraction at all but the necessary and vindicating collapse of an unworkable design, which is exactly what this reading's classification captures — coordination framing (Bretton Woods solved a real trust problem) coexisting with high extraction (the design's internal contradiction made the coordination's costs fall on the very parties it was meant to serve) is the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Both the U.S. Treasury and the European creditor nations are coded as payers/victims under this reading — a structural departure from readings that cast the U.S. as a rule-violator extracting from compliant creditors. Here neither party is positioned as benefiting from the arrangement's persistence; both bear the cost of a design that cannot be fixed by either party's unilateral action. The beneficiary set instead comprises those external to the doomed arrangement's operation: the architects and advocates of the floating-rate successor regime, who gain both intellectual vindication and institutional position in what replaces Bretton Woods. Developing-country holders are the most powerless payers, bearing cost with zero negotiating leverage over either the original design or its unwinding.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a credible, elastic reserve asset without a supranational currency — is coded dead under this reading precisely because the diagnosis (Triffin's) demonstrated the problem was insoluble in the form originally attempted; the arrangement did not merely become obsolete through changed circumstances, it was structurally incapable of solving its own founding problem from inception. This distinguishes the triffin_structural_reading from a reading that would treat the founding problem as still-live-but-mismanaged (which would point toward policy failure, not design failure) and supports the status of RESOLVED MANDATROPHY: the mandate (fixed convertibility as reserve stabilizer) was retired via the 1971 suspension rather than persisting past its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    design_flaw_vs_policy_failure_attribution,
    'Was the Bretton Woods collapse the necessary consequence of an inherently contradictory design (this reading), or was it a contingent outcome of specific U.S. policy choices (deficit spending, Vietnam War financing, delayed devaluation) that a different policy path could have avoided (the sibling readings'' implicit premises)?',
    'Counterfactual macroeconomic modeling of alternative U.S. fiscal/monetary paths within the fixed-convertibility constraint, cross-checked against comparable fixed-exchange-rate episodes (e.g., later EMS crises) to test whether the trilemma binds independent of specific national policy choices.',
    'If the trilemma is structurally binding regardless of policy path, this reading''s classification (both U.S. and creditors as trilemma victims) is the correct structural account and the strict_convertibility_reading''s implicit U.S.-culpability framing is a category error. If policy choices could have preserved convertibility, this reading overstates structural inevitability and the flaw is partly attributable to specific actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_flaw_vs_policy_failure_attribution, conceptual, 'Whether the Triffin dilemma was a strict structural impossibility or a contingent policy-driven outcome.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one of three readings of the dollar_gold_convertibility kernel (strict_convertibility_reading, policy_flexible_reading, triffin_structural_reading). Where exactly do the readings diverge structurally: is it in who counts as a victim (this reading places both the U.S. and creditors as victims; strict_convertibility_reading places only creditors as victims of U.S. non-compliance), or in whether the obligation was binding at all (policy_flexible_reading treats Article IV as conditional)?',
    'Comparative analysis of the three readings'' beneficiary/victim sets and their treatment of Article IV''s legal bindingness, cross-referenced against the actual 1971-1973 negotiating record (Smithsonian Agreement, Committee of Twenty) to see which framing the negotiating parties themselves invoked.',
    'If the historical negotiating record shows parties invoking design-flaw language (Triffin-style) rather than legal-breach or policy-discretion language, this reading has stronger evidentiary support relative to its siblings; if legal-obligation language dominated the actual 1971 negotiations, the strict_convertibility_reading better captures how contemporaneous actors understood the constraint even if this reading is analytically superior in hindsight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating where the three kernel readings diverge and which the historical record best supports.').

omega_variable(
    beneficiary_temporal_horizon,
    'Is ''post_bretton_woods_floating_regime_architects'' a genuine beneficiary group with concentrated gains, or does floating-rate vindication constitute a diffuse intellectual/institutional benefit that does not rise to the level of extraction-collecting beneficiary status required for tangled_rope classification?',
    'Trace institutional appointments, IMF governance roles, and academic influence metrics for Triffin-school economists and floating-rate advocates in the 1970s-1980s to establish whether the benefit was concentrated (career/institutional capture) or genuinely diffuse (general intellectual vindication with no rent capture).',
    'If the benefit is genuinely diffuse, the tangled_rope classification weakens toward a piton or even mountain-adjacent reading (design flaw with no concentrated beneficiary); if concentrated in specific institutional actors, tangled_rope is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_temporal_horizon, empirical, 'Whether the floating-regime beneficiary group captured concentrated institutional gains or merely diffuse intellectual vindication.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1961, 0.28).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.36).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1968, 0.47).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.55).
narrative_ontology:measurement(doll_tr_t1973, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1973, 0.58).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.38).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1961, 0.48).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1968, 0.71).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.79).
narrative_ontology:measurement(doll_be_t1973, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1973, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1961, 0.4).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1968, 0.6).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.65).
narrative_ontology:measurement(doll_su_t1973, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1973, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__triffin_structural_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, imf_article_iv_surveillance_regime).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, post_1971_floating_rate_order).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the dollar_gold_convertibility kernel. strict_convertibility_reading treats Article IV as a binding legal obligation the U.S. violated (creditors as sole victims, U.S. as extractor). policy_flexible_reading treats convertibility as conditional on domestic stability (weaker victim set, U.S. policy discretion legitimate). This story (triffin_structural_reading) treats the arrangement as an inherently contradictory design with both the U.S. and creditor nations as structural victims and the post-1971 floating regime as the beneficiary. All three share the same historical episode but author different ε, different beneficiary/victim sets, and different classifications from different structural premises — they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
