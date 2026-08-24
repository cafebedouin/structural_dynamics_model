% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Article 127 Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   Article 127 TFEU establishes price stability as the ECB's primary
 *   objective and permits supporting 'the general economic policies in the
 *   Union' — including employment and growth — 'without prejudice to the
 *   objective of price stability.' This reading treats the 'without
 *   prejudice' clause as authorizing operational discretion: when inflation
 *   is at or below target, the ECB may weight employment and output gaps in
 *   its reaction function. The constraint is claimed as a tangled rope —
 *   genuine coordination (a single mandate balancing multiple objectives)
 *   with asymmetric extraction (discretionary balancing transfers real
 *   resources from savers/creditors to workers/debtors/governments). The
 *   engine computes per-seat classifications from the structural data; the
 *   authored claim does not adjudicate the divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.52).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.45).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.52).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Article 127 Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '747c92e1-9cee-4569-8420-dc06aa7553d1').
narrative_ontology:cs_kernel_codification('747c92e1-9cee-4569-8420-dc06aa7553d1', formalized).
narrative_ontology:cs_authority_grounding('747c92e1-9cee-4569-8420-dc06aa7553d1', lineage).
narrative_ontology:cs_interpretation_layer_present('747c92e1-9cee-4569-8420-dc06aa7553d1').
narrative_ontology:cs_reading_relation('747c92e1-9cee-4569-8420-dc06aa7553d1', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('747c92e1-9cee-4569-8420-dc06aa7553d1', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('747c92e1-9cee-4569-8420-dc06aa7553d1', foundational, conditional_dual_mandate_permissible).
narrative_ontology:cs_axiom_status(conditional_dual_mandate_permissible, holdable).
narrative_ontology:cs_axiom_grounding('747c92e1-9cee-4569-8420-dc06aa7553d1', conditional_dual_mandate_permissible, conventional).
narrative_ontology:cs_axiom('747c92e1-9cee-4569-8420-dc06aa7553d1', foundational, without_prejudice_authorizes_discretionary_balancing).
narrative_ontology:cs_axiom_status(without_prejudice_authorizes_discretionary_balancing, holdable).
narrative_ontology:cs_axiom_grounding('747c92e1-9cee-4569-8420-dc06aa7553d1', without_prejudice_authorizes_discretionary_balancing, conventional).
narrative_ontology:cs_reference_frame('747c92e1-9cee-4569-8420-dc06aa7553d1', maastricht_conditional_dual_mandate).
narrative_ontology:cs_drift_state('747c92e1-9cee-4569-8420-dc06aa7553d1', post_2008_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('747c92e1-9cee-4569-8420-dc06aa7553d1', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_debtors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_governments).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_creditors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, inflation_hawks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, ecb_staff).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, conditional_dual_mandate).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, without_prejudice_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and implements monetary policy under Article 127 TFEU. Interprets 'without prejudice to the objective of price stability' as authorizing operational weight on employment and growth when inflation is at or below target. Bears political and reputational risk if discretion is perceived as overreach; collects institutional authority from the mandate's ambiguity.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, analytical, universal).

% Prepares analytical frameworks and policy proposals that operationalize the expansive reading. Benefits from expanded analytical mandate and institutional relevance; bears career risk if policy frameworks are judged to have exceeded mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_staff, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, ecb_staff, beneficiary).

% Benefit from lower unemployment and stronger wage growth when the ECB weights employment in its reaction function. Exit is constrained — they cannot switch central banks, and labor mobility within the eurozone is limited by language, skills, and institutional barriers. Their gains are diffuse and indirect, mediated through labor market conditions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers, beneficiary,
    organized, biographical, constrained, continental).

% Households, firms, and governments with nominal debt benefit from lower real interest rates and higher nominal growth when secondary objectives receive weight. Exit is constrained by jurisdiction and contract denomination; they cannot easily re-denominate debts or move outside the eurozone monetary framework.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_debtors, beneficiary,
    moderate, biographical, constrained, continental).

% Benefit from lower debt service costs and stronger growth when the ECB's reaction function incorporates employment and output gaps. They influence the mandate through the European Council and Eurogroup but cannot unilaterally change treaty interpretation. Their exit would require treaty change — politically prohibitive.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_governments, beneficiary,
    institutional, generational, constrained, continental).

% Bear the inflation risk and financial repression costs when the ECB tolerates above-target inflation or maintains negative real rates to support employment/growth. Exit options are limited: moving savings abroad incurs currency risk and regulatory friction; holding cash has storage costs. Their losses are diffuse and indirect, eroding purchasing power over time.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_savers, payer,
    moderate, biographical, constrained, continental).

% Banks, insurers, pension funds, and bondholders bear mark-to-market losses and compressed net interest margins when policy rates are kept lower than a strict inflation mandate would imply. They have meaningful exit — can reallocate portfolios globally, hedge inflation, or shift to non-euro assets — but face regulatory capital requirements that constrain full exit.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_creditors, payer,
    powerful, biographical, mobile, global).

% Institutional and political actors (Bundesbank, German constitutional court, fiscal hawks in northern member states) who bear reputational and political costs when the mandate is read expansively. They invest in legal challenges, public campaigns, and appointment politics to constrain discretion. Exit is constrained — they are embedded in the eurozone governance structure and cannot credibly threaten withdrawal.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, inflation_hawks, payer,
    organized, biographical, constrained, continental).

% Provide the intellectual infrastructure for competing mandate interpretations. Their research shapes the legitimacy of the expansive reading but they neither collect rents nor bear direct policy costs. Exit is analytical — they can shift frameworks without material consequence.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, academic_economists, observer,
    analytical, generational, analytical, universal).

% Non-eurozone holders of euro-denominated assets who bear spillover effects from ECB discretion but have no voice in EU treaty interpretation or ECB governance. They can exit by selling euro assets, but at scale this moves markets — their exit option is real but costly.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, global_creditors, excluded,
    powerful, biographical, mobile, global).

% Inherit the debt dynamics, inflation expectations, and institutional path dependencies created by current mandate interpretation. They have no voice, no exit, and no organizational form to contest the constraint. Their situation is the paradigmatic excluded seat.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides operational framework for monetary policy that stabilizes prices while permitting discretionary weight on employment and growth when price stability is not threatened, solving the coordination problem of balancing multiple macroeconomic objectives under a single treaty mandate.
% TRANSFER_FUNCTION: Moves distributional weight from savers and creditors (who bear inflation risk and financial repression) to workers, debtors, and governments (who benefit from growth and employment emphasis) through discretionary interest rate policy, forward guidance, and balance sheet operations.
% ABSENT_VOICES: Global creditors holding euro-denominated assets, future generations who inherit debt and inflation dynamics, and non-eurozone EU members affected by monetary spillovers are structurally excluded from the mandate interpretation process. Their objections would challenge the legitimacy of discretionary balancing but they have no seat in the European Council, ECB Governing Council, or national ratification processes.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, the ECB would revert to an exclusive price stability focus. Interest rate paths would shift upward, debt service costs would rise for governments and households, the institutional equilibrium around 'without prejudice' discretion would collapse, and the eurozone's macroeconomic coordination framework would reorganize around a narrower mandate — fiscal policy would bear the full burden of employment stabilization, likely triggering sovereign stress in high-debt members.
% FOUNDING_PROBLEM: The Maastricht Treaty created a tension between an exclusive price stability mandate (Article 105 TEC, now Article 127 TFEU) and the political reality that monetary policy inevitably affects employment and growth. The 'without prejudice' clause was the compromise: secondary objectives are permitted but subordinated, operational only when price stability is not threatened.
% FOUNDING_PROBLEM_CORROBORATION: Maastricht negotiators' records and subsequent treaty amendments (Amsterdam 1997, Lisbon 2009) corroborate the conditional dual-mandate intent. However, Bundesbank and German Constitutional Court jurisprudence (e.g., PSPP ruling 2020) contest the operational scope of 'without prejudice.' No external non-beneficiary source affirms the current expansive reading as the sole legitimate interpretation; the corroboration is split along institutional lines.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects the growing gap between the ECB's actual reaction function and a counterfactual strict inflation-targeting rule — measured by the cumulative distributionary effect of negative real rates, APP/PEPP purchases, and TLTROs since 2014. Suppression (0.45) is moderate: alternatives (strict inflation targeting, rules-based policy) exist and are advocated by powerful actors, but the treaty text and institutional practice constrain their adoption. Theater ratio (0.28) is low-moderate: the coordination function (price stability + conditional secondary objectives) is real, but a growing share of policy communication performs 'dual mandate' framing to legitimize discretion. The measurement series run on one shared time grid (0-25, relative to 1999) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (ECB Governing Council), the constraint is genuine coordination — a mandate designed for conditional balancing. From the payer seats (savers, creditors, hawks), the same structure operates as enforced extraction — discretion that systematically transfers from them to beneficiaries. The engine computes this divergence from the structural data; the authored claim (tangled_rope) states the structural truth without resolving the seat-level disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council and staff are structural agenda-setters with analytical exit — they author the interpretation but bear reputational risk. Workers, debtors, and governments are beneficiaries with constrained exit: they gain from the discretionary weighting but cannot leave the monetary union. Savers are payers with constrained exit: they bear inflation erosion but cannot easily escape euro-denominated savings. Creditors are payers with mobile exit: they can reallocate globally, dampening their effective extraction. Inflation hawks are payers with constrained exit: they contest the reading institutionally but cannot exit the governance structure. Global creditors and future generations are excluded: they bear spillovers without voice. The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Maastricht's tension between exclusive price stability and political reality of monetary non-neutrality) remains contested — not dead. The arrangement persists because the 'without prejudice' ambiguity serves as a pressure valve: it lets the ECB respond to crises without treaty change. But the expanding beneficiary set (climate, financial stability, strategic autonomy) and rising extraction suggest mandatrophy risk: the coordination function may be atrophying into a cover for distributional discretion. The engine's false_summit_mountain detection does not apply here (claimed type is not mountain), but the temporal drift in extractiveness and theater_ratio flags the same dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the ''without prejudice'' clause a genuine coordination mechanism (solving the problem of how to balance objectives under one mandate) or a constructed ambiguity that benefits identifiable agents (governments, debtors) by permitting discretionary extraction?',
    'Counterfactual treaty negotiation: if the clause were removed, would the ECB''s operational framework converge on the orthodox reading, or would a new coordination mechanism emerge? Historical analysis of Maastricht drafting records for intent vs. strategic ambiguity.',
    'If constructed ambiguity, the constraint is a false summit candidate (though claimed as tangled_rope, not mountain) — the coordination story is cover for extraction. If genuine coordination, the extraction is the price of solving a real collective action problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the treaty text''s ambiguity is a feature (coordination) or a bug (extraction cover)').

omega_variable(
    discretionary_boundary_ambiguity,
    'Where does ''price stability not threatened'' end and discretionary balancing begin? The boundary determines how much weight secondary objectives can receive before violating the primary mandate.',
    'ECB case law and Governing Council minutes analysis: identify the inflation threshold and forecast horizon at which secondary objectives are operationally downweighted. Compare to the orthodox reading''s implied boundary (always binding).',
    'A wide boundary makes the constraint more extractive (more discretion = more distributional transfer). A narrow boundary converges toward the orthodox reading. The boundary is not fixed in treaty text — it is a live interpretive contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discretionary_boundary_ambiguity, empirical, 'The operational boundary between primary and secondary mandate objectives').

omega_variable(
    distributional_incidence_ambiguity,
    'Who actually bears the costs of the expansive reading? The standard account (savers/creditors pay, workers/debtors benefit) assumes transmission channels that may not hold — e.g., if lower rates boost asset prices, wealthy asset-holding households may be net beneficiaries.',
    'Distributional incidence analysis of ECB policy shocks (HFCS data, microsimulation models) separating direct interest rate effects from portfolio rebalancing and asset price channels.',
    'If the payer/beneficiary mapping is wrong, the structural data in this story misidentifies the extraction vector. The engine''s directionality computation would then produce incorrect per-seat classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distributional_incidence_ambiguity, empirical, 'Whether the assumed payer/beneficiary structure matches actual distributional incidence').

omega_variable(
    committer_structure_ecb_mandate,
    'This constraint is one reading (''expansive_secondary_objectives'') of the contested kernel ''ecb_mandate_article_127''. What structural elements differ across the sibling readings?',
    'Compare the three readings on: (1) beneficiary/victim sets, (2) suppression of alternatives, (3) interpretive authority grounding, (4) drift trajectory from Maastricht reference frame.',
    'The kernel decomposition requires separate constraint stories for each reading. This omega records the committer structure for cross-reading analysis. If the readings are not structurally distinct (same ε, same beneficiaries, same suppression), the kernel decomposition is unnecessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_ecb_mandate, conceptual, 'Committer frame: structural delta between this reading and its siblings (orthodox_price_stability, climate_incorporation)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_mandate_art127_expansive_tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.12).
narrative_ontology:measurement(ecb_mandate_art127_expansive_tr_t5, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ecb_mandate_art127_expansive_tr_t10, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ecb_mandate_art127_expansive_tr_t15, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 15, 0.22).
narrative_ontology:measurement(ecb_mandate_art127_expansive_tr_t20, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 20, 0.25).
narrative_ontology:measurement(ecb_mandate_art127_expansive_tr_t25, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(ecb_mandate_art127_expansive_be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ecb_mandate_art127_expansive_be_t5, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(ecb_mandate_art127_expansive_be_t10, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(ecb_mandate_art127_expansive_be_t15, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(ecb_mandate_art127_expansive_be_t20, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(ecb_mandate_art127_expansive_be_t25, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ecb_mandate_art127_expansive_su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(ecb_mandate_art127_expansive_su_t5, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(ecb_mandate_art127_expansive_su_t10, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(ecb_mandate_art127_expansive_su_t15, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(ecb_mandate_art127_expansive_su_t20, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(ecb_mandate_art127_expansive_su_t25, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 25, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ecb_mandate_article_127__expansive_secondary_objectives, 0.12).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ecb_mandate_article_127 kernel. The orthodox reading claims exclusive price stability focus (ε ≈ 0.15, mountain/tangled_rope boundary). The climate incorporation reading claims mandatory climate risk integration (ε ≈ 0.45, tangled_rope). This expansive reading claims conditional discretionary balancing (ε = 0.52, tangled_rope). The ε values differ because each reading authorizes a different operational framework with different distributional incidence. They are linked via affects_constraints because the expansive reading's discretionary space enables the climate incorporation reading's operationalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ecb_mandate_article_127__expansive_secondary_objectives, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
