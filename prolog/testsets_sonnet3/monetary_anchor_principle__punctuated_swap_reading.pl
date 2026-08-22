% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Bretton Woods Gold-Dollar Convertibility as a Reversible Institutional Choice (Nixon Shock, Aug 15 1971)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the punctuated-swap reading of the
 *   monetary_anchor_principle kernel: the claim that the end of dollar-gold
 *   convertibility was fundamentally a discrete institutional decision, made
 *   and announced by a small group over a single weekend, rather than a
 *   mechanical or overdetermined outcome. Under this reading, Bretton Woods
 *   functioned as genuine coordination (a rope) until the U.S. government
 *   unilaterally defected from it — a coordination failure through defection
 *   rather than a structural collapse. The beneficiary is U.S. fiscal and
 *   monetary autonomy; the victim is foreign holders of dollar reserves who
 *   had structured their institutions around a convertibility promise that
 *   was withdrawn without consultation or compensation. Epsilon is authored
 *   as moderate (0.52) because this reading treats the decision as reversible
 *   in principle — a policy choice among available alternatives, not an
 *   inevitability — which caps how purely extractive the underlying
 *   arrangement is judged to be, even though the transfer effected by the
 *   specific announcement was real and uncompensated.
 *
 * KEY AGENTS:
 *   - us_federal_government: agenda_setter/beneficiary (institutional/arbitrage) — made the unilateral decision and captured fiscal freedom
 *   - foreign_dollar_reserve_holders: payer (powerful/trapped) — bore the uncompensated devaluation with no warning
 *   - foreign_central_banks: payer (institutional/constrained) — forced into unplanned architecture renegotiation
 *   - monetary_historians: analytical observer — adjudicate discretion vs. inevitability across the kernel's sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.52).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.38).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods Gold-Dollar Convertibility as a Reversible Institutional Choice (Nixon Shock, Aug 15 1971)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '294d84ea-617b-4fba-9b5e-14ee831d3278').
narrative_ontology:cs_kernel_codification('294d84ea-617b-4fba-9b5e-14ee831d3278', distributed).
narrative_ontology:cs_authority_grounding('294d84ea-617b-4fba-9b5e-14ee831d3278', practice).
narrative_ontology:cs_reading_relation('294d84ea-617b-4fba-9b5e-14ee831d3278', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('294d84ea-617b-4fba-9b5e-14ee831d3278', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('294d84ea-617b-4fba-9b5e-14ee831d3278', foundational, decision_discretion_was_causally_load_bearing).
narrative_ontology:cs_axiom_status(decision_discretion_was_causally_load_bearing, holdable).
narrative_ontology:cs_axiom_grounding('294d84ea-617b-4fba-9b5e-14ee831d3278', decision_discretion_was_causally_load_bearing, empirically_contingent).
narrative_ontology:cs_axiom('294d84ea-617b-4fba-9b5e-14ee831d3278', secondary, regime_exit_was_reversible_in_principle).
narrative_ontology:cs_axiom_status(regime_exit_was_reversible_in_principle, holdable).
narrative_ontology:cs_axiom_grounding('294d84ea-617b-4fba-9b5e-14ee831d3278', regime_exit_was_reversible_in_principle, empirically_contingent).
narrative_ontology:cs_created_at('294d84ea-617b-4fba-9b5e-14ee831d3278', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_federal_government).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_policymakers).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held the unilateral authority to suspend gold convertibility by executive decision on a single day, without prior multilateral negotiation. Gained immediate fiscal and monetary autonomy — could run deficits and set exchange rates without gold-reserve discipline. Chose the swap as a discrete policy act, not because it was forced by any single mechanical constraint, but because the alternative (defending convertibility) was judged more costly than defection.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_federal_government, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_federal_government, beneficiary).

% Freed from the gold-convertibility discipline that had constrained deficit spending (Vietnam War, Great Society programs). Could now finance policy through currency issuance and inflation rather than gold outflows, at the cost of foreign holders of dollar claims.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_policymakers, beneficiary,
    institutional, generational, arbitrage, national).

% Held dollar-denominated reserves accumulated under the explicit promise of gold convertibility at $35/oz. The single announcement instantly and retroactively devalued those holdings with no consultation and no compensation. Could not have exited beforehand because the swap was announced as a fait accompli — the entire point of choosing a discrete Sunday-evening announcement was to prevent anticipatory exit.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_reserve_holders, payer,
    powerful, biographical, trapped, global).

% Had structured their own reserve and exchange-rate policies around dollar-gold convertibility as an institutional anchor. Forced to renegotiate the entire international monetary architecture (Smithsonian Agreement, then floating rates) in the aftermath, at a cost of years of instability they did not choose and were not warned of.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_central_banks, payer,
    institutional, generational, constrained, global).

% Benefited indirectly from continued deficit-financed spending and employment programs, but bore the domestic inflationary consequences of a currency no longer disciplined by gold convertibility, compounded by the 1973 oil shock's dollar-denominated price effects.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, domestic_us_consumers, beneficiary).

% Debate whether the August 15, 1971 announcement was itself the causal event (this reading) or merely the ratification of pressures already structurally determined years earlier (the sibling readings). Analyze Nixon's actual decision memos, timing, and available policy alternatives to adjudicate discretion versus inevitability.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_federal_government).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods solved a genuine post-WWII coordination problem: fixed exchange rates anchored to gold-backed dollar convertibility gave trading nations a stable unit of account and reduced competitive devaluation, at the cost of requiring the reserve issuer to maintain gold discipline.
% TRANSFER_FUNCTION: The August 15, 1971 announcement moved value from foreign holders of dollar claims (whose reserves were suddenly no longer convertible at the promised rate) to the U.S. government and domestic fiscal policymakers (who gained freedom from gold-outflow discipline and could continue deficit spending without immediate external correction).
% ABSENT_VOICES: Foreign governments and central banks were not consulted before the announcement — it was designed as a unilateral surprise specifically to prevent the kind of anticipatory capital flight that consultation would have triggered. Their absence from the decision room is definitional to this reading: the swap's efficacy as a discrete act depended on excluding them from deliberation.
% DISAPPEARANCE_RATIONALE: Had the August 15, 1971 announcement not occurred (or been reversed shortly after), the Bretton Woods gold-dollar peg would have persisted at least in the near term, foreign reserve holdings would have retained their convertibility guarantee, and the subsequent Smithsonian Agreement and float to fully flexible exchange rates would not have followed the same path or timeline. This reading holds that the specific date and manner of the decision mattered causally, not merely that some transition was coming eventually.
% FOUNDING_PROBLEM: Bretton Woods was built to prevent the competitive devaluations and monetary chaos of the interwar period by anchoring currencies to a gold-convertible dollar, giving the postwar trading system a stable reference point.
% FOUNDING_PROBLEM_CORROBORATION: Nixon administration officials (Connally, Volcker) attested at the time that the suspension was a deliberate, timed policy choice made over a single weekend at Camp David, not an automatic mechanical failure. Independent monetary historians outside the U.S. policymaking apparatus — and foreign central bank officials who testified afterward to national legislatures about being blindsided — corroborate that the announcement's timing and unilateral, uncoordinated character were themselves causally significant, distinct from the underlying pressures the sibling readings emphasize.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored as moderate-rising to 0.58 at the 1971 announcement itself, then settling to 0.52 by 1976 as the reading treats the shock as an acute event within a fundamentally coordinative arrangement rather than a permanently high-extraction structure. Suppression stays comparatively low (peaking 0.45 in 1971) because this reading holds there was no ongoing coercive enforcement machinery keeping the Bretton Woods peg alive by force — the system operated on voluntary participation until the U.S. defected, at which point the 'suppression' that spiked was the shock's non-consultative, surprise-announcement character itself, not standing coercion. Theater ratio stays low throughout (0.10–0.22) because this reading does not see the gold peg as performative — it was functionally real convertibility until the specific day it was withdrawn.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. federal government and its fiscal policymakers sit at the beneficiary end: they set the agenda, retained the most mobile exit options (arbitrage — able to reset the entire monetary order to their advantage), and captured the freed-up fiscal capacity. Foreign dollar reserve holders sit at the target end: trapped exit (holdings were already accumulated and could not be unwound before the announcement), no participation in the decision, and a real uncompensated loss in dollar-value terms. Foreign central banks are institutional but constrained rather than trapped — they had some capacity to renegotiate architecture afterward (Smithsonian Agreement) but bore real transition costs they did not choose.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling the entire Bretton Woods system as pure extraction: the pre-1971 coordination function (stable exchange rates, reduced competitive devaluation) was real and mutually beneficial, which is why the base classification is rope rather than snare. The extraction is located specifically in the punctuated act of defection — the swap — not in the underlying coordination logic, which is exactly what distinguishes this reading from a story that would classify the whole Bretton Woods order as tangled_rope or snare from inception. Declaring the founding_problem_status as contested captures that the coordination function (stable exchange architecture) was arguably still partly live in 1971, even as the specific mechanism serving it was unilaterally discarded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_vs_inevitability_locus,
    'Was the August 15, 1971 announcement a genuinely contingent policy choice among live alternatives, or was it merely the moment an already-determined structural collapse became visible?',
    'Examination of internal Nixon administration deliberation records (Camp David meeting minutes, Treasury memos) for evidence that non-suspension alternatives were seriously entertained as viable versus treated as already foreclosed by reserve exhaustion.',
    'If deliberation records show live alternatives were seriously weighed and gold reserves were not yet critically depleted, this reading''s moderate-epsilon, rope-coordination-failure classification holds. If records show officials believed abandonment was the only remaining option given reserve levels, the constraint''s true structural home is the triffin_inevitability_reading and this reading''s ε would be understated as a description of genuine choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_inevitability_locus, conceptual, 'Whether the 1971 decision was a live contingent choice or the visible endpoint of prior structural determination — the central dispute between this reading and its siblings.').

omega_variable(
    reversibility_claim_credibility,
    'Was a return to gold-dollar convertibility genuinely available as a policy option after 1971 (supporting this reading''s ''reversible in principle'' framing), or had structural conditions already made reversal practically impossible regardless of political will?',
    'Analysis of the Smithsonian Agreement''s failure (1973) and subsequent attempts to re-peg — did their failure stem from renewed political choice to float, or from structural inability to sustain any fixed rate given capital mobility and deficit trajectories?',
    'If reversal was genuinely available and rejected by subsequent choice, the epsilon-moderate/reversible framing is well-grounded. If reversal was structurally foreclosed by 1973 regardless of intent, this reading''s reversibility claim is weaker than authored and the constraint drifts toward the composite or Triffin reading''s higher structural determinism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_claim_credibility, empirical, 'Whether post-1971 reversal attempts failed due to choice or structural constraint, testing this reading''s core reversibility premise.').

omega_variable(
    compensation_counterfactual,
    'Could the U.S. have achieved the same fiscal-autonomy benefit through a consultative, compensated devaluation process rather than a unilateral surprise suspension — meaning was the uncompensated-expropriation character of the swap itself a further, separable choice beyond the coordination-exit decision?',
    'Comparison with historical precedent devaluations conducted with advance notice and negotiated terms (e.g., 1949 sterling devaluation) to assess whether comparable fiscal relief was achievable without the surprise/non-consultation element.',
    'If consultative devaluation could have achieved similar fiscal relief with lower victim cost, then the extractiveness measured here partly reflects an avoidable choice of method (surprise, non-compensation) layered on top of the necessary coordination exit, raising the case for treating the manner of exit as a distinct extractive add-on rather than an intrinsic feature of the regime swap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_counterfactual, conceptual, 'Whether the extraction measured is inherent to any regime exit or specifically to the unconsulted, surprise manner of this one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1958, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1965, 0.14).
narrative_ontology:measurement(mone_tr_t1970, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1973, 0.2).
narrative_ontology:measurement(mone_tr_t1976, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1976, 0.22).

% Extraction over time
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1958, 0.18).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1965, 0.28).
narrative_ontology:measurement(mone_be_t1970, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(monetary_anchor_epsilon_nixon_shock_1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.58).
narrative_ontology:measurement_basis(monetary_anchor_epsilon_nixon_shock_1971, observed).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1973, 0.55).
narrative_ontology:measurement(mone_be_t1976, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1976, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1958, 0.12).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(mone_su_t1970, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement(mone_su_t1973, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1973, 0.4).
narrative_ontology:measurement(mone_su_t1976, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1976, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the monetary_anchor_principle kernel, decomposed because the natural-language label 'the end of Bretton Woods' conflates structurally distinct causal claims with different epsilon profiles. This reading (punctuated_swap) treats the transition as a discrete, reversible-in-principle institutional choice with moderate extraction concentrated at the moment of announcement — closer to rope-with-defection than to structural inevitability. The overdetermined_composite_reading and triffin_inevitability_reading instead locate the extraction (if any) in accumulated structural pressures rather than in a specific decision, and would author different epsilon trajectories reflecting gradual rather than punctuated extraction. All three readings share the same historical event but disagree about where causal and moral responsibility for the transfer from foreign dollar holders to the U.S. government is located.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
