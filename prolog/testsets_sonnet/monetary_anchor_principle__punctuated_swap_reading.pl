% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Bretton Woods Gold Convertibility Suspension as Discrete Sovereign Choice (Nixon Shock, Aug 15 1971)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the 'punctuated swap' reading of the Nixon Shock:
 *   the claim that the August 15, 1971 suspension of dollar-gold
 *   convertibility was a discrete, identifiable institutional act — a
 *   specific decision by a specific set of officials, announced on a specific
 *   evening, with a specific unilateral form (no advance consultation with
 *   treaty partners, no phased transition) — rather than the mechanical
 *   terminus of accumulating structural pressure. On this reading the Bretton
 *   Woods gold-dollar peg functioned as a genuine coordination mechanism (a
 *   Rope): it solved a real post-war problem of exchange-rate stability and
 *   gave trading nations a credible anchor. What happened on August 15, 1971
 *   was a unilateral U.S. defection from that coordination arrangement, timed
 *   and structured to shift adjustment costs onto foreign dollar holders who
 *   had no notice and no voice in the decision. This is a moderate-epsilon
 *   story: the underlying coordination function was real and largely benign,
 *   and the swap was, in principle, reversible or could have been executed
 *   cooperatively — the extraction lies specifically in the manner and timing
 *   of exit, not in the prior arrangement itself.
 *
 * KEY AGENTS:
 *   - us_treasury: agenda_setter, exercised unilateral discretion to close the gold window
 *   - us_fiscal_policymakers and us_export_sector: beneficiaries of the resulting policy flexibility and dollar depreciation
 *   - foreign_dollar_reserve_holders and foreign_central_banks: bore the valuation cost with no advance warning
 *   - bretton_woods_treaty_partners: excluded from the decision despite being party to the arrangement it dissolved
 *   - economic_historians: analytical observers debating discretion vs. inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.48).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.42).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods Gold Convertibility Suspension as Discrete Sovereign Choice (Nixon Shock, Aug 15 1971)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '2e317a9a-1566-4c0b-a9c3-eba498a6a6e5').
narrative_ontology:cs_kernel_codification('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', distributed).
narrative_ontology:cs_authority_grounding('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', distributed).
narrative_ontology:cs_reading_relation('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', foundational, unilateral_exit_form_is_severable_from_transition_necessity).
narrative_ontology:cs_axiom_status(unilateral_exit_form_is_severable_from_transition_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', unilateral_exit_form_is_severable_from_transition_necessity, conventional).
narrative_ontology:cs_axiom('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', secondary, sovereign_treaty_exit_requires_no_prior_multilateral_consent).
narrative_ontology:cs_axiom_status(sovereign_treaty_exit_requires_no_prior_multilateral_consent, holdable).
narrative_ontology:cs_axiom_grounding('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', sovereign_treaty_exit_requires_no_prior_multilateral_consent, conventional).
narrative_ontology:cs_reference_frame('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', bretton_woods_gold_convertibility_commitment).
narrative_ontology:cs_drift_state('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', post_nixon_shock_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('2e317a9a-1566-4c0b-a9c3-eba498a6a6e5', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_treasury).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_policymakers).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_export_sector).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_treaty_partners).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, executive_discretion_over_monetary_regime).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, sovereign_prerogative_to_exit_treaty_commitments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held the unilateral authority to suspend gold convertibility and exercised it in a single announced act on August 15, 1971, closing the gold window without prior multilateral consultation. Retained full policy discretion afterward and captured the benefit of no longer being bound to defend a fixed gold price against a growing external dollar liability.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).

% Freed from the discipline of gold convertibility, gained room to run deficits (Vietnam War spending, Great Society programs) without the constraint of a fixed external gold price. The swap converted an external hard constraint into a domestically adjustable one.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_policymakers, beneficiary,
    institutional, generational, mobile, national).

% Benefited from the dollar's subsequent depreciation against other currencies, which improved the price competitiveness of American exports relative to what a fixed gold-dollar peg would have permitted.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_export_sector, beneficiary,
    organized, biographical, mobile, global).

% Held dollar reserves accumulated on the explicit understanding that they were convertible to gold at $35/oz. When the window closed, the value of those holdings was effectively marked down as the dollar floated and depreciated. They had no advance notice and no seat at the decision; their exit option — converting dollars to gold before the announcement — was foreclosed by the surprise timing itself.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_reserve_holders, payer,
    moderate, biographical, trapped, global).

% Managed national reserves denominated in dollars under the Bretton Woods architecture. The unilateral suspension forced them to absorb valuation losses and to redesign reserve and exchange-rate policy on short notice. They could diversify reserves going forward but could not undo losses already incurred, and had no formal mechanism to contest the decision before it was made.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks, excluded).

% Signatories to the Bretton Woods arrangement whose consent was not sought before the U.S. exited the gold-convertibility commitment. They were presented with a fait accompli and negotiated the Smithsonian Agreement only after the fact, from a weaker bargaining position than if the change had been jointly deliberated.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_treaty_partners, excluded,
    institutional, generational, constrained, global).

% Debate whether the August 15 decision was a genuinely discretionary act or the terminal step of pressures already determining the outcome. This story takes the position that, whatever the background pressures, the specific act — timing, form, unilateralism, surprise — was itself a choice with an alternative (multilateral negotiation) that was not taken.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_treasury).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods solved a genuine post-war coordination problem: fixed exchange rates anchored to gold-convertible dollars gave trading nations a stable unit of account and prevented competitive devaluation, at the cost of binding U.S. monetary policy to gold reserve levels.
% TRANSFER_FUNCTION: The August 15, 1971 suspension moved the cost of resolving U.S. balance-of-payments pressure from the U.S. Treasury (which would otherwise have had to defend the gold price through contraction or negotiated devaluation) onto foreign holders of dollar reserves, whose holdings lost value as the dollar was allowed to float and depreciate.
% ABSENT_VOICES: Foreign governments and central banks holding dollar reserves under the Bretton Woods commitment were not consulted before the announcement; they would have objected to unilateral abrogation of a multilateral arrangement and pressed for a negotiated, phased transition that shared adjustment costs rather than concentrating them on reserve holders.
% DISAPPEARANCE_RATIONALE: If the August 15, 1971 decision had not been made as it was — if the U.S. had instead sought multilateral negotiation before suspending convertibility — the transition away from gold-dollar fixity would very likely still have occurred eventually (the pressures were real), but the distributional outcome would differ: adjustment costs could have been shared through negotiated devaluation or a jointly designed successor system rather than falling asymmetrically on reserve holders caught by surprise. The specific act of unilateral, undeclared timing is precisely what this reading isolates as contingent and reversible-in-principle, distinct from whether some transition was structurally coming.
% FOUNDING_PROBLEM: The dollar-gold peg was built to give the post-war international monetary system a credible, stable anchor so that trade and capital flows would not be disrupted by currency instability, while allowing the U.S., as reserve issuer, some flexibility relative to a pure gold standard.
% FOUNDING_PROBLEM_CORROBORATION: U.S. Treasury officials at the time and subsequent U.S.-aligned accounts characterize the closure of the gold window as a necessary correction to an untenable position forced by structural pressure. Independent corroboration from outside the U.S. policymaking apparatus — contemporaneous statements from French and other European finance ministries, and later academic monetary history (e.g. Eichengreen, Bordo) — treats the specific unilateral form and timing of the August 15 decision as a deliberate, avoidable choice among available alternatives, not a structurally compelled outcome; this reading is built on that second, external corroboration.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.48 peaking at the 1971 transition point itself, reflecting the concentrated cost imposed on reserve holders at the moment of surprise announcement, then gradually settling toward 0.35 as the floating-rate regime normalized and reserve holders adjusted portfolios. This is deliberately lower than a full snare reading would author — the underlying Bretton Woods arrangement had genuine coordination value, and the extraction here is bounded to the specific act of unilateral, undeclared exit rather than characterizing the whole monetary order as predatory. Suppression (0.42) reflects that reserve holders had no formal veto or consultation right, but were not physically coerced — the constraint here is informational and procedural exclusion, not force. Theater ratio rises modestly around 1971-1972 reflecting the diplomatic performance of the Smithsonian Agreement negotiations, which restored a fig leaf of multilateral process after the fact without reversing the substantive unilateral decision.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. Treasury's seat, this reads as sovereign prerogative exercised in a national emergency — decisive action to protect a currency under speculative attack. From the seat of a foreign central bank holding dollar reserves accumulated in good faith under an explicit convertibility promise, the same act reads as an uncompensated taking, executed with a timing and secrecy specifically designed to prevent affected parties from protecting themselves in advance. The engine should compute a real divergence here: powerful/institutional/arbitrage-exit U.S. seats and moderate/institutional/trapped-or-constrained-exit foreign seats sit far apart in directionality despite occupying nominally similar institutional power tiers.
 *
 * DIRECTIONALITY LOGIC:
 *   us_treasury and us_fiscal_policymakers sit near the full-beneficiary end: they set the timing, captured the policy flexibility, and faced no comparable loss. foreign_dollar_reserve_holders and foreign_central_banks sit near the full-target end: they bore a concentrated, uncompensated valuation loss and had no advance exit — the surprise announcement is precisely what forecloses the arbitrage they would otherwise have executed (converting to gold ahead of the decision). bretton_woods_treaty_partners are treated as excluded rather than symmetric participants because their consent was structurally bypassed, not merely disadvantaged by market outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists two mislabelings at once. First, it resists collapsing into a pure snare reading of the entire dollar system (that is the overdetermined_composite or Triffin readings' territory, not this one) — the prior Bretton Woods coordination function was real, so classifying the whole arrangement as extraction-only would erase genuine coordination value that existed for two and a half decades. Second, it resists the opposite error of treating the 1971 act as pure structural necessity requiring no accountability — by isolating the SPECIFIC unilateral, undeclared form of the exit as the extractive element (rather than the fact of exit itself), the story preserves the possibility that a cooperative, negotiated transition could have achieved the same monetary-policy freedom without the concentrated cost imposed on unconsulted reserve holders.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_vs_inevitability_locus,
    'Was the August 15, 1971 decision a genuinely contingent institutional choice among live alternatives, or was it the last domino in a sequence already determined by the Triffin dilemma and fiscal pressures that left no real alternative by 1971?',
    'Archival review of internal Treasury and Federal Reserve deliberations in 1970-1971 to determine whether cooperative or phased-transition alternatives were seriously considered and rejected, versus never having been live options given reserve depletion trajectories.',
    'If genuinely contingent, this reading''s rope-with-unilateral-defection classification holds and the extraction is properly located in the manner of exit. If the alternatives were not live by 1971, this reading collapses toward the triffin_inevitability_reading and the same event should be reclassified with lower attributable agency and correspondingly different epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_inevitability_locus, empirical, 'Whether the punctuated-swap framing survives scrutiny of what alternatives were actually available in 1971.').

omega_variable(
    reading_selection_and_evidentiary_basis,
    'What specific historical evidence favors treating the timing and unilateral form of the decision (rather than its underlying structural causation) as the primary object of analysis?',
    'Comparative reading of the diplomatic record: did other Bretton Woods signatories at the time characterize the decision as avoidable and improperly unilateral (supporting this reading), or as an understandable response to conditions they also recognized as untenable (supporting the composite or Triffin readings)?',
    'European finance ministry reactions in 1971 (particularly French objections to the ''exorbitant privilege'') are the primary evidentiary anchor for this reading''s claim that contemporaries treated the act as a choice, not a necessity. If that record is thinner or more ambivalent than assumed, the case for isolating discretion at the level of manner-and-timing weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_evidentiary_basis, conceptual, 'Documents the evidentiary basis for selecting this reading over its siblings, per the CS-framing under-determination guidance.').

omega_variable(
    beneficiary_boundary_within_us_institutions,
    'Is ''U.S. fiscal autonomy'' a coherent single beneficiary, or does it mask a narrower beneficiary set (the executive branch and Treasury specifically) versus costs later borne by U.S. domestic actors through 1970s inflation?',
    'Trace whether the policy flexibility gained in 1971 was primarily exercised for public benefit (macroeconomic stabilization) or produced the inflationary consequences of the mid-1970s that harmed U.S. domestic holders of dollar-denominated assets as well.',
    'If U.S. domestic actors also bore significant costs from subsequent inflation, the beneficiary/victim split is not purely a U.S.-vs-foreign-holders story, and the domestic beneficiary claim should be narrowed to specific institutional actors rather than the U.S. economy broadly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_boundary_within_us_institutions, empirical, 'Tests whether the declared U.S. beneficiary group is precisely bounded or artificially broad.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1969, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1969, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement(mone_tr_t1970, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.28).
narrative_ontology:measurement(mone_tr_t1972, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1972, 0.32).
narrative_ontology:measurement(mone_tr_t1974, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1974, 0.3).
narrative_ontology:measurement(mone_tr_t1976, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1976, 0.25).

% Extraction over time
narrative_ontology:measurement(mone_be_t1969, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1969, 0.22).
narrative_ontology:measurement(mone_be_t1970, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.48).
narrative_ontology:measurement(mone_be_t1972, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1972, 0.45).
narrative_ontology:measurement(mone_be_t1974, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1974, 0.4).
narrative_ontology:measurement(mone_be_t1976, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1976, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(monetary_anchor_principle__punctuated_swap_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__punctuated_swap_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the end of Bretton Woods' / 'the Nixon Shock' per the ε-invariance principle. Each sibling assigns a different locus of causation and therefore a different constraint type and epsilon to nominally the same historical event: this reading (punctuated_swap_reading) treats it as a discrete, largely reversible-in-principle institutional choice (rope, moderate epsilon, extraction concentrated in the manner/timing of unilateral exit); overdetermined_composite_reading treats it as the convergent product of independent structural pressures; triffin_inevitability_reading treats it as compelled specifically by the internal logic of reserve-currency-under-gold-standard dynamics. All three are linked via affects_constraints because they share the same event, the same stakeholder pool, and compete for explanatory priority over the same historical record — but each has its own stable epsilon and must not be averaged or blended with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__punctuated_swap_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
