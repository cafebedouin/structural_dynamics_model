% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Bretton Woods Gold Convertibility Anchor (Punctuated Swap Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods gold-dollar anchor (1944-1971) is read here as a
 *   genuine coordination mechanism — a rope — that solved the post-war
 *   exchange rate problem. The Nixon Shock was a discrete institutional
 *   choice: the U.S. fiscal authority unilaterally defected from the
 *   coordination equilibrium, closing the gold window rather than adjusting
 *   policy. This reading treats the transition as contingent and reversible
 *   in principle, not structurally inevitable. The beneficiary is U.S. fiscal
 *   autonomy (freedom from gold discipline); the victims are foreign dollar
 *   holders who suffered expropriation via subsequent devaluation and
 *   inflation. Extraction is moderate and rising over the interval as U.S.
 *   deficits grew; suppression is moderate (capital controls, moral suasion,
 *   the London Gold Pool) and rises as the anchor strains. Theater remains
 *   low — the coordination function was real until the defection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.45).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.35).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods Gold Convertibility Anchor (Punctuated Swap Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, 'cc5976b1-10df-4918-8592-df7de914ee8e').
narrative_ontology:cs_kernel_codification('cc5976b1-10df-4918-8592-df7de914ee8e', formalized).
narrative_ontology:cs_authority_grounding('cc5976b1-10df-4918-8592-df7de914ee8e', lineage).
narrative_ontology:cs_interpretation_layer_present('cc5976b1-10df-4918-8592-df7de914ee8e').
narrative_ontology:cs_reading_relation('cc5976b1-10df-4918-8592-df7de914ee8e', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc5976b1-10df-4918-8592-df7de914ee8e', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('cc5976b1-10df-4918-8592-df7de914ee8e', foundational, discrete_institutional_choice).
narrative_ontology:cs_axiom_status(discrete_institutional_choice, holdable).
narrative_ontology:cs_axiom_grounding('cc5976b1-10df-4918-8592-df7de914ee8e', discrete_institutional_choice, conventional).
narrative_ontology:cs_axiom('cc5976b1-10df-4918-8592-df7de914ee8e', foundational, reversible_in_principle).
narrative_ontology:cs_axiom_status(reversible_in_principle, holdable).
narrative_ontology:cs_axiom_grounding('cc5976b1-10df-4918-8592-df7de914ee8e', reversible_in_principle, instrumental).
narrative_ontology:cs_reference_frame('cc5976b1-10df-4918-8592-df7de914ee8e', bretton_woods_coordination_framework).
narrative_ontology:cs_drift_state('cc5976b1-10df-4918-8592-df7de914ee8e', post_nixon_shock, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cc5976b1-10df-4918-8592-df7de914ee8e', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_citizens).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, institutional_choice_over_structural_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The U.S. Treasury and Federal Reserve jointly administered the gold exchange standard, setting the dollar-gold parity and controlling the supply of dollar liquidity. They benefited from seigniorage and the ability to run deficits without immediate gold loss, but faced political pressure to maintain convertibility. Exit meant abandoning the anchor — a choice they ultimately exercised unilaterally in 1971.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_fiscal_authority, beneficiary).

% Foreign central banks and governments held dollars as reserves under the commitment of $35/oz gold convertibility. They bore the adjustment burden of U.S. deficits — accumulating dollars they could not easily convert without triggering a run. Their exit options were constrained: selling dollars would devalue their own reserves; demanding gold conversion accelerated the drain. The 1971 closure expropriated the real value of their holdings via subsequent devaluation and inflation.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_holders, payer,
    powerful, generational, constrained, global).

% U.S. households and firms benefited from the stable dollar, low inflation (initially), and the fiscal space the anchor gave the government to fund domestic programs and Vietnam War spending without immediate tax increases. They bore costs later via the inflation that followed the anchor's removal. Exit from the dollar was costly — currency substitution was limited by legal and practical barriers.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_citizens, beneficiary,
    organized, biographical, constrained, national).

% The IMF and the collective of member states operated the adjustable-peg system atop the dollar-gold anchor. They monitored the constraint's operation, provided standby credit, and adjudicated parity changes. Their analytical seat reflects the system-level view: the anchor coordinated exchange stability but generated the Triffin tension between liquidity provision and confidence.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_monetary_system, observer,
    institutional, generational, analytical, global).

% Economists, policymakers, and political actors who argued for maintaining or restoring gold convertibility. They were structurally excluded from the 1971 decision — the Nixon administration acted without congressional authorization or international consultation. Their objections (that the anchor was a discipline device, not a coordination convenience) were overridden by the executive's unilateral action.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, gold_standard_advocates, excluded,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gold convertibility anchor solved the coordination problem of fixing exchange rates among sovereign currencies without a world central bank: by pegging the dollar to gold and other currencies to the dollar, it created a stable numeraire for international trade and finance, reducing transaction costs and exchange risk.
% TRANSFER_FUNCTION: The arrangement transferred the burden of adjustment from the U.S. fiscal authority (which could expand domestic credit and run deficits) to foreign dollar holders (who accumulated the resulting dollar claims at fixed parity, bearing the risk of devaluation or inflation). Seigniorage from global dollar demand accrued to the U.S.; the cost of maintaining parity fell on surplus countries.
% ABSENT_VOICES: Gold standard advocates (excluded stakeholder) and developing nations dependent on dollar stability for import capacity and debt service. The former were excluded from the decision; the latter had no seat at the table. Both would have objected to the unilateral closure — the advocates on disciplinary grounds, the developing nations on vulnerability grounds.
% DISAPPEARANCE_RATIONALE: When the gold window closed on August 15, 1971, the fixed-parity system collapsed. Within two years, major currencies floated, the IMF's par value system was abandoned, and a new regime of managed floating emerged. Global trade and finance reorganized around flexible exchange rates, dollar-hegemony without convertibility, and eventually the petrodollar recycling mechanism. The world did not stay the same — the monetary architecture was rebuilt.
% FOUNDING_PROBLEM: The post-WWII need for a stable international monetary system to facilitate reconstruction, trade, and capital flows without the competitive devaluations and protectionism of the 1930s. The Bretton Woods conference designed the gold-dollar anchor as the coordination mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Eichengreen, Bordo) and contemporary policymakers (Triffin, Roosa) attest the Bretton Woods system solved the immediate post-war coordination problem. The Triffin dilemma literature (Triffin 1960, later Mundell, Kenen) documents the structural tension that emerged as global dollar demand outpaced U.S. gold reserves — a problem distinct from the founding one, which the system's persistence despite this tension illustrates.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).
:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.45 as U.S. deficits expand and the gold cover ratio falls — the anchor extracts more from foreign holders over time. Suppression rises from 0.20 to 0.35 as capital controls and the Gold Pool are deployed to defend parity. Theater stays low (0.05-0.15) because the mechanism genuinely coordinated exchange stability; the performative element (pretending the anchor was sustainable) grows only late. Accessibility collapse is moderate (0.40) — alternatives (floating, SDRs, wider bands) existed but were politically costly. Resistance is moderate (0.50) — surplus countries resisted revaluation; the U.S. resisted adjustment. The constraint is a rope that became a tangled rope in its final phase; the claimed_type reflects the reading's structural judgment that the coordination function was primary and the extraction a growing distortion, not the essence.
 *
 * PERSPECTIVAL GAP:
 *   From the U.S. fiscal authority's seat, the anchor is a coordination tool it built and can modify — the defection is a sovereign choice. From foreign dollar holders' seat, the same anchor is a binding commitment whose unilateral breach is extraction. The engine computes this divergence from the structural data: the agenda-setter has arbitrage-grade exit (can change the regime); the payers have constrained exit (trapped in dollar holdings). The claimed_type (rope) reflects the coordination function; the metrics capture the extraction that grew within it.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. fiscal authority is the agenda-setter and primary beneficiary (d near 0.0) — it sets the rules, collects seigniorage, and ultimately chooses defection. Foreign dollar holders are payers (d near 1.0) — they hold the reserves, bear the devaluation risk, and have constrained exit. U.S. citizens are incidental beneficiaries (d ~ 0.3) — they gain from fiscal space but later pay via inflation. The international monetary system (IMF) is an analytical observer (d = 0.5). Gold standard advocates are excluded (d undefined) — their structural position is outside the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war monetary stability) was solved by the late 1950s — the system worked. The Triffin tension (liquidity vs. confidence) emerged as a new problem, but the arrangement persisted without addressing it. The 1971 defection was not mandatrophy resolution (the constraint didn't fade; it was actively abandoned) but a strategic exit by the agenda-setter. The constraint's mandate (gold convertibility) was formally suspended, not allowed to atrophy. The reading's axioms capture this: the anchor was a reversible institutional choice, not an inevitable collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural delta (discrete choice, reversible, rope-type) relate to the sibling readings of the same kernel?',
    'Comparative analysis of the three readings'' ε values, beneficiary/victim structures, and temporal profiles. The punctuated_swap reading has moderate ε (0.45) rising over 1944-1971; the triffin_inevitability reading would have higher ε (structural extraction) and different temporal shape; the overdetermined_composite reading would show multiple causal drivers.',
    'If the sibling readings produce substantially different ε and type classifications for the same historical episode, the kernel is genuinely contested — the label ''end of Bretton Woods'' covers multiple constraints. If they converge, the contest is verbal, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Structural relationship between this reading and its siblings in the monetary_anchor_principle kernel.').

omega_variable(
    coordination_extraction_boundary_1971,
    'Was the rising extraction after 1960 a distortion of the coordination function, or was extraction always latent in the dollar''s reserve role?',
    'Counterfactual: if the U.S. had adjusted fiscal policy in 1965-68 (tax increases, spending cuts), would the gold cover ratio have stabilized and the anchor persisted? Historical evidence on the Johnson administration''s policy choices and the gold market dynamics.',
    'If extraction was latent from the start (Triffin''s view), the constraint was always a tangled rope. If extraction emerged only when policy chose deficits over adjustment, the punctuated_swap reading''s rope→tangled_rope trajectory is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_1971, empirical, 'Whether the Bretton Woods anchor''s extractive component was structural or policy-contingent.').

omega_variable(
    reversibility_post_1971,
    'Could the gold convertibility anchor have been restored after August 1971 at a new parity, or was the defection irreversible?',
    'Analysis of the Smithsonian Agreement (Dec 1971) and subsequent attempts: the 1971-73 negotiation record, the 1973 float, and the 1976 Jamaica Agreement formalizing floating rates. Did any actor have the power and incentive to restore convertibility?',
    'If restorable, the punctuated_swap reading''s ''reversible_in_principle'' axiom holds and the constraint was a rope with a discretionary exit. If irreversible, the triffin_inevitability reading gains structural support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_post_1971, conceptual, 'Whether the 1971 defection was a reversible policy choice or a point-of-no-return.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(map_psr_tr_t1944, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(map_psr_tr_t1950, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(map_psr_tr_t1958, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(map_psr_tr_t1960, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(map_psr_tr_t1965, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1965, 0.13).
narrative_ontology:measurement(map_psr_tr_t1968, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1968, 0.14).
narrative_ontology:measurement(map_psr_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.15).

% Extraction over time
narrative_ontology:measurement(map_psr_be_t1944, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(map_psr_be_t1950, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1950, 0.18).
narrative_ontology:measurement(map_psr_be_t1958, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1958, 0.25).
narrative_ontology:measurement(map_psr_be_t1960, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(map_psr_be_t1965, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(map_psr_be_t1968, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(map_psr_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(map_psr_su_t1944, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(map_psr_su_t1950, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(map_psr_su_t1958, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1958, 0.28).
narrative_ontology:measurement(map_psr_su_t1960, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(map_psr_su_t1965, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1965, 0.33).
narrative_ontology:measurement(map_psr_su_t1968, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1968, 0.35).
narrative_ontology:measurement(map_psr_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__punctuated_swap_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'end of Bretton Woods' into three structurally distinct readings of the monetary_anchor_principle kernel. The punctuated_swap_reading (this file) has ε=0.45, rope type, discrete choice framing. The triffin_inevitability_reading has higher ε (structural necessity), tangled_rope type. The overdetermined_composite_reading has multi-causal ε trajectory. All three share the same historical referent but different structural decompositions — the ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__punctuated_swap_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
