% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Capacity-Bounded Reparations Regime (Limited Responsibility Reading)
 *   domain: international relations/legal history/political economy
 *
 * SUMMARY:
 *   After the Armistice, the victorious coalition faced a debtor it had
 *   beaten but not occupied to surrender-point: how to collect an indemnity
 *   from an economy that had to remain standing in order to pay. The
 *   Versailles clauses answered with a legal form — Article 231's
 *   responsibility declaration and an open-ended reparations charge — whose
 *   practical meaning was fought out for thirteen years across the London
 *   Schedule, the Ruhr occupation, the Dawes and Young Plans, and finally the
 *   Lausanne termination. This story instantiates ONE reading of that fight:
 *   the limited_responsibility_reading, under which the obligation is legally
 *   real but strictly bounded by Germany's demonstrated capacity to pay,
 *   Article 231 is bookkeeping rather than moral verdict, and any schedule
 *   outrunning viability is uncollectible by definition. KEY AGENTS (by
 *   structural relationship): - reich_finance_ministry: administering
 *   agenda-setter (organized/constrained) — funds and certifies each
 *   installment, wins precedent with every downward revision -
 *   german_industrial_elites: primary beneficiary (powerful/mobile) — claims
 *   capped, leverage compounded by capital-flight threats -
 *   french_creditor_state: primary payer-victim (institutional/constrained) —
 *   largest entitlement, largest gross receipts, capped far below claim -
 *   allied_war_pensioners: excluded victims (powerless/trapped) — earmarked
 *   funding trimmed without a seat - international_reparations_agent:
 *   enforcing agenda-setter (institutional/constrained) — the certification
 *   machinery that makes 'capacity' operational - american_banking_interests:
 *   arbitrage beneficiary (institutional/arbitrage) — finances the circular
 *   flow from outside the treaty - league_financial_committee: analytical
 *   observer (institutional/analytical). The sibling readings
 *   (punitive_liability_reading, repudiation_reading) are separate files with
 *   structurally different ε values and victim sets; this file does not
 *   average across them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.33).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.3).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.33).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Capacity-Bounded Reparations Regime (Limited Responsibility Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international relations/legal history/political economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'c1818d55-8f47-4fcd-be40-b707b017c00c').
narrative_ontology:cs_kernel_codification('c1818d55-8f47-4fcd-be40-b707b017c00c', fixed_text).
narrative_ontology:cs_authority_grounding('c1818d55-8f47-4fcd-be40-b707b017c00c', expertise).
narrative_ontology:cs_interpretation_layer_present('c1818d55-8f47-4fcd-be40-b707b017c00c').
narrative_ontology:cs_reading_relation('c1818d55-8f47-4fcd-be40-b707b017c00c', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1818d55-8f47-4fcd-be40-b707b017c00c', versailles_reparations_clauses__repudiation_reading, forecloses).
narrative_ontology:cs_axiom('c1818d55-8f47-4fcd-be40-b707b017c00c', foundational, reparation_obligation_bounded_by_payee_capacity).
narrative_ontology:cs_axiom_status(reparation_obligation_bounded_by_payee_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c1818d55-8f47-4fcd-be40-b707b017c00c', reparation_obligation_bounded_by_payee_capacity, empirically_contingent).
narrative_ontology:cs_axiom('c1818d55-8f47-4fcd-be40-b707b017c00c', foundational, article_231_legal_formality_not_moral_verdict).
narrative_ontology:cs_axiom_status(article_231_legal_formality_not_moral_verdict, holdable).
narrative_ontology:cs_axiom_grounding('c1818d55-8f47-4fcd-be40-b707b017c00c', article_231_legal_formality_not_moral_verdict, conventional).
narrative_ontology:cs_reference_frame('c1818d55-8f47-4fcd-be40-b707b017c00c', capacity_bounded_payment_regime).
narrative_ontology:cs_drift_state('c1818d55-8f47-4fcd-be40-b707b017c00c', lausanne_termination_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('c1818d55-8f47-4fcd-be40-b707b017c00c', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_reich_budget).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, american_banking_interests).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, british_export_interests).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, french_creditor_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, belgian_creditor_state).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_war_pensioners).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, keynesian_transfer_capacity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raises the domestic taxes and arranges the internal borrowing that fund each annual installment, certifies Germany's fiscal position to the international agents, and negotiates downward revisions whenever the schedule outruns collections. Each revision it wins becomes precedent for the next, so its leverage compounds with every reaffirmation of the capacity principle. Defaulting outright would invite renewed occupation, so it operates inside the regime it administers.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, reich_finance_ministry, agenda_setter,
    organized, biographical, constrained, national).

% Heavy industry and its financiers, whose liabilities are capped by the capacity assessments and whose threats of capital flight and production slowdown repeatedly moved the schedules downward. They profit from the ceiling on claims while supplying the political coalitions that defend it, and several of the same houses borrowed abroad against the stability the payment schedule provided.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites, beneficiary,
    powerful, biographical, mobile, national).

% The Reich's consolidated finances, spared the open-ended liability of the 1921 London Schedule. Every reaffirmation of the capacity limit converts a potential bottomless claim on the budget into a schedulable annuity, protecting spending headroom — though the budget also services the foreign loans that keep the payment flow liquid.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_reich_budget, beneficiary,
    organized, generational, trapped, national).

% New York lending houses financing the circular flow: dollars lent to Germany fund reparations to the Allies, which service war debts owed back to the United States. They sit entirely outside the treaty system, can reprice or withdraw their lending at will, and their participation is what makes the bounded schedule financially operable.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, american_banking_interests, beneficiary,
    institutional, biographical, arbitrage, global).

% Manufacturers and traders whose continental markets recover only if German purchasing power survives. They back the capacity limit as commercial self-interest and supplied much of its intellectual case, most prominently through Keynes's 1919 attack on the peace terms.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, british_export_interests, beneficiary,
    organized, biographical, mobile, global).

% Holds the largest assessed share of the damages and receives the largest gross share of every payment — yet the capacity bound caps those receipts far below the claimed entitlement, and its 1923 attempt to collect by occupying the Ruhr cost more than it recovered. It carries the gap between what the treaty promised its reconstruction funds and what actually arrives.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_creditor_state, payer,
    institutional, generational, constrained, national).

% Smaller creditor with proportionately heavier devastation per capita. Occupies the same capped-receipt position as France with less leverage to reopen the schedule, dependent on Franco-British alignment for any upward revision.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, belgian_creditor_state, payer,
    institutional, generational, trapped, national).

% Veterans' pension funds and widows' allowances in the creditor countries are earmarked against reparations receipts in public debate but hold no seat at any conference. When schedules shrink, their assumed funding shrinks silently; nobody at the table speaks for the trimmed entitlement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_war_pensioners, excluded,
    powerless, generational, trapped, continental).

% The Agent General for Reparations Payments established under the Dawes Plan, with the mixed-committee machinery around him: monitors Reich finances, certifies what Germany can carry, gates each transfer, and arbitrates between creditor demands and debtor appeals. His certifications are the operational meaning of 'capacity.'
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, international_reparations_agent, agenda_setter,
    institutional, biographical, constrained, continental).

% Parties and lobbies committed to overturning the entire settlement, who regard even the bounded schedule as theft. Excluded from the negotiation rooms their agitation shakes; abandoning opposition to Versailles is unthinkable inside their own politics, since anti-treaty commitment is constitutive of the movement.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_nationalist_opposition, excluded,
    organized, biographical, identity_locked, national).

% League of Nations financial committee and associated neutral economists, tracking whether the European credit system survives either escalation or collapse of the payments. Takes testimony from every other seat, decides nothing, and circulates memoranda among all parties.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, league_financial_committee, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, french_creditor_state).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts an open-ended, politically explosive indemnity into a scheduled, verifiable payment stream sized to the debtor economy's demonstrated capacity — solving the collection problem: obtaining compensation without destroying the economy that must produce it, and giving creditors a predictable receipt schedule in place of an unenforceable claim.
% TRANSFER_FUNCTION: Moves annual gold-mark payments from German taxpayers and industry to French, Belgian, and other Allied treasuries (thence to war-debt service, notably to the United States), with the volume set by expert-assessed capacity rather than assessed damage.
% ABSENT_VOICES: Allied war pensioners and devastated-region claimants, whose entitlements the cap trims without representation at any conference table; the German nationalist opposition, excluded from the settlements it would repudiate; ordinary German taxpayers, whose future burden was negotiated over their heads through foreign-loan structures.
% DISAPPEARANCE_RATIONALE: If the capacity bound vanished overnight, the open-ended London Schedule claims return: either collection escalates until the German economy collapses (the 1923 dynamic generalized), dragging European credit down with it, or the obligation becomes uncollectible and the regime dissolves into the repudiation outcome. Creditor budgets, the Dawes/Young loan architecture, and the currency stabilization built on scheduled payments all rearrange.
% FOUNDING_PROBLEM: How a victorious coalition collects compensation from a defeated continental economy without destroying that economy's ability to pay — and without the collection effort costing more than it recovers, the lesson of the Ruhr occupation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the German beneficiary set: Keynes, a creditor-nation economist, argued in The Economic Consequences of the Peace (1919) that demands exceeding capacity would fail; the Dawes Committee, an international panel including Allied nominees, concluded in 1924 that collection required first stabilizing German finances; Bank of England and US Treasury correspondence treated German solvency as the binding constraint. French officials disputed the bound's generosity, not the existence of a capacity constraint — the problem itself is corroborated, contested only at the margin.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.33, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).
:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.33 (end-state): real wealth left Germany annually under the bounded schedules, but assessed by this reading's own lights the bound is legitimacy-conferring, so ε sits well below the punitive reading's referent. Suppression ends at 0.30 as enforcement machinery — occupation threat, sanctions, agent certification — decayed through the depression; the 1923 Ruhr peak is visible in the suppression_requirement series (0.78 at t=4). Theater ends at 0.55: by the Hoover moratorium and Lausanne the viability machinery certified capacities everyone knew would not be honored — performative maintenance of a dying schedule. Accessibility_collapse is low (0.35): the alternatives, full-payment doctrine and outright repudiation, stayed live the whole interval, and repudiation ultimately prevailed. Resistance is high (0.72): French occupation, German passive resistance, nationalist campaigns, and American withdrawal each attacked the arrangement from a different seat. The three metric series share one eight-point grid (t=0..13); the extractiveness arc is deliberately non-monotonic — the 1921 London Schedule pushed the unbounded claim to its peak (0.63 at t=2) before Dawes-era bounding pulled it down. Claim/metric independence: claimed_type is tangled_rope for the arrangement's operative life (genuine collection-coordination plus an asymmetric shortfall borne by creditors and pensioners); the terminal metric profile (low suppression, high theater) drifts piton-ward as the function atrophied — the engine computes that transition, and this story does not reconcile the claim to it. Receipt surface: gain_flow names french_creditor_state, where gross receipts demonstrably accrued in the largest share, even though the same seat is a declared victim of the cap — receipt and benefit are different facts. fixing_cost is authored prohibitive on its own evidence: the one attempt to remove the bound by force (Ruhr occupation, 1923) cost France more than it recovered, and the depression-era alternative of writing the obligation down destroyed the asset's value anyway.
 *
 * PERSPECTIVAL GAP:
 *   From the French and Belgian treasury seats the same schedule that Berlin experiences as protection reads as confiscation of a sworn entitlement — identical institutional power atoms, opposite directionalities, differentiated by position in the transfer chain rather than by global standing. The American banking seat holds arbitrage-grade exit no treaty party possesses: it can reprice the circular flow that keeps the schedule liquid, placing it nearer the beneficiary pole than any signatory. The Agent General's seat is technical-administrative: he experiences neither the shortfall nor the burden, only the certification cycle. The German nationalist opposition is identity-locked into rejection — exit from anti-Versailles politics would dissolve the movement itself — so the bound's moderation purchases no legitimacy from that quarter at any terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (industrial elites, Reich budget, American finance, British exporters) derive low d — the bound subsidizes them by capping claims and lubricating the loan circular. Declared victims (French and Belgian creditor states, Allied pensioners) derive high d — the bound is the instrument that trims their recovery. The French seat is the structural paradox the receipt surface records: simultaneously the largest receiver of the extraction and a declared victim of the bound that caps those receipts below entitlement. Allied pensioners sit furthest toward the target end among victims: they bear the trim with no seat, no leverage, and no exit. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already place every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — collect compensation without destroying the payer — stayed live across the whole interval and remains the standing problem of sovereign-debt design, so the mismatch consumer finds status=live paired with verdict=world_rearranges and no zombie flag. The mandatrophy risk runs both directions here: labeling the bound pure extraction would erase the genuine collection-stabilization function that kept payments flowing 1924–29; labeling it pure coordination would erase the creditor shortfall and the elite leverage the same structure produced. Tangled rope holds both halves. The arrangement ended by obsolescence (Lausanne, 1932) rather than by mandate death — the problem outlived the mechanism, which is why the terminal theater ratio climbs while the founding problem stays live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint instantiates only the limited_responsibility_reading of the versailles_reparations_clauses kernel — which structural elements do the sibling readings relocate, and how would classification move under them?',
    'Authoring the sibling files (punitive_liability_reading, repudiation_reading) and comparing computed per-seat types across the family; the disagreement is located in the bindingness and extent of the Article 231 obligation — quasi-unlimited liability versus capacity-bounded obligation versus no obligation.',
    'Under the punitive reading the victim set expands to include the German economy as a whole and ε rises sharply; under the repudiation reading the obligation itself dissolves, the beneficiary structure empties, and the arrangement reads as bare imposition. Cross-reading comparison, not within-story revision, resolves it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame omega: this story is one reading of a three-reading kernel; sibling readings change victim sets and ε structurally.').

omega_variable(
    true_transfer_capacity,
    'What was Germany''s actual sustainable annual transfer capacity in gold marks, net of foreign loans that merely recycled the payments?',
    'Counterfactual balance-of-payments reconstruction separating genuine resource transfer from circular lending — the classical transfer-problem analysis.',
    'If true capacity sat near the Dawes/Young annuities, the bound was accurate coordination; if well below them, even the bounded schedule exceeded capacity and the reading''s own ceiling was fiction — pushing theater and extraction assessments upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_transfer_capacity, empirical, 'Whether the capacity bound tracked real transfer capacity or a negotiated fiction above it.').

omega_variable(
    vindication_or_refutation_at_collapse,
    'Did the 1931–32 suspension refute the capacity principle (even bounded payments proved unsustainable) or vindicate it (unbounded demands would have collapsed the system years earlier)?',
    'Comparative timing analysis: hyperinflation peaked under unbounded demands in 1923; stabilization held exactly while schedules tracked assessed capacity; breakdown coincided with the exogenous depression shock rather than schedule size.',
    'Vindication supports the coordination half of the tangled-rope structure; refutation would reweight the arrangement toward theatrical maintenance of an unpayable promise and darken the terminal classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vindication_or_refutation_at_collapse, empirical, 'Whether the terminal collapse indicts the bound or confirms it.').

omega_variable(
    elite_capture_of_the_bound,
    'Did the capacity bound protect German households and fiscal stability, or chiefly the negotiating position and tax exposure of heavy-industry elites who shifted the residual burden onto foreign lenders and future budgets?',
    'Incidence analysis of who bore the post-Dawes burden: domestic taxation versus Dawes/Young loan service versus deferred obligations.',
    'If capture dominated, the beneficiary declaration narrows to elite seats and the gain-flow picture darkens; if broad, the coordination function is more genuinely public than the elite-leverage reading suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_the_bound, empirical, 'Distribution of the bound''s protection between German publics and negotiating elites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(vers_tr_t0, observed).
narrative_ontology:measurement(vers_tr_t2, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement_basis(vers_tr_t2, observed).
narrative_ontology:measurement(vers_tr_t4, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(vers_tr_t4, observed).
narrative_ontology:measurement(vers_tr_t6, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(vers_tr_t6, observed).
narrative_ontology:measurement(vers_tr_t8, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(vers_tr_t8, observed).
narrative_ontology:measurement(vers_tr_t10, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(vers_tr_t10, observed).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement_basis(vers_tr_t12, observed).
narrative_ontology:measurement(vers_tr_t13, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 13, 0.55).
narrative_ontology:measurement_basis(vers_tr_t13, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(vers_be_t0, observed).
narrative_ontology:measurement(vers_be_t2, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 2, 0.63).
narrative_ontology:measurement_basis(vers_be_t2, observed).
narrative_ontology:measurement(vers_be_t4, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement_basis(vers_be_t4, observed).
narrative_ontology:measurement(vers_be_t6, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(vers_be_t6, observed).
narrative_ontology:measurement(vers_be_t8, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement_basis(vers_be_t8, observed).
narrative_ontology:measurement(vers_be_t10, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement_basis(vers_be_t10, observed).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement_basis(vers_be_t12, observed).
narrative_ontology:measurement(vers_be_t13, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 13, 0.33).
narrative_ontology:measurement_basis(vers_be_t13, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(vers_su_t0, observed).
narrative_ontology:measurement(vers_su_t2, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 2, 0.62).
narrative_ontology:measurement_basis(vers_su_t2, observed).
narrative_ontology:measurement(vers_su_t4, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 4, 0.78).
narrative_ontology:measurement_basis(vers_su_t4, observed).
narrative_ontology:measurement(vers_su_t6, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement_basis(vers_su_t6, observed).
narrative_ontology:measurement(vers_su_t8, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(vers_su_t8, observed).
narrative_ontology:measurement(vers_su_t10, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(vers_su_t10, observed).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(vers_su_t12, observed).
narrative_ontology:measurement(vers_su_t13, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 13, 0.3).
narrative_ontology:measurement_basis(vers_su_t13, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'German reparations under the Versailles Treaty' decomposes into three structurally distinct constraints held by different parties: the punitive_liability_reading (unlimited claim grounded in Article 231 guilt — high ε, the German economy as victim), this limited_responsibility_reading (binding but capacity-bounded obligation — moderate ε, the creditor shortfall as victim side), and the repudiation_reading (no binding obligation — the arrangement reads as bare imposition). The punitive reading is upstream: its textual authority is what the limited reading accepts as formality and the repudiation reading rejects outright. Each file carries its own ε, beneficiaries, and victims; this file instantiates only the limited reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
