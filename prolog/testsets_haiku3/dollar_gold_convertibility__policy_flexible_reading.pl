% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility (Policy-Flexible Reading): Conditional Obligation Subordinate to Domestic Stability
 *   domain: international_political_economy/monetary_policy
 *
 * SUMMARY:
 *   Under Bretton Woods (1944–1971), the U.S. declared the dollar convertible
 *   into gold at $35 per troy ounce. This reading interprets convertibility
 *   as a conditional obligation subordinate to U.S. domestic economic
 *   stability — the U.S. retained the implicit right to suspend or adjust the
 *   commitment if inflation, capital flight, or gold depletion threatened
 *   domestic growth or price stability. Dollar holders and foreign central
 *   banks accumulated reserves believing the commitment would hold, but faced
 *   subordination risk: their redemption right was not a binding legal
 *   guarantee but a policy preference, revokable if the U.S. prioritized
 *   domestic full employment over external discipline. The measured
 *   extractiveness (0.68 by 1971) reflects the growing asymmetry: the U.S.
 *   government collected the seigniorage benefit of reserve-asset status and
 *   monetary autonomy, while reserve holders bore the devaluation risk. The
 *   theater ratio (0.35 by 1971) reflects the divergence between official
 *   rhetoric ('the commitment is inviolable') and actual policy ('it is
 *   conditional and we will suspend it'). This is KERNEL READING 1 of 3: the
 *   policy-flexible reading of the contested kernel
 *   'dollar-gold-convertibility'. It instantiates a different constraint from
 *   the strict-convertibility reading (sibling 1) and the Triffin structural
 *   reading (sibling 2). All three are valid instantiations of the same
 *   ambiguous text, but they have different victim sets and different
 *   extractiveness profiles.
 *
 * KEY AGENTS:
 *   - United States government (agenda-setter, beneficiary): Retains monetary autonomy and collects seigniorage; implicitly reserves the right to suspend convertibility if domestic needs require it.
 *   - Dollar reserve holders (organized, payer/victim): Accumulate dollars believing they can redeem for gold, but face devaluation risk subordinate to U.S. policy choices.
 *   - Foreign central banks (institutional, beneficiary + payer): Gain a stable reserve asset and external anchor, but also face subordination risk and the Triffin dilemma (the system erodes as U.S. liabilities grow faster than gold).
 *   - International Monetary Fund (institutional, observer): Constitutionally endorses fixed convertibility but cannot enforce it against U.S. unilateral suspension.
 *   - Private exporters (moderate, payer): Face exchange-rate volatility tied to convertibility credibility; cannot influence the policy but depend on its stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.52).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility (Policy-Flexible Reading): Conditional Obligation Subordinate to Domestic Stability").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_policy").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, '21f78ae5-9e1e-426d-8f8d-f1aba1933967').
narrative_ontology:cs_kernel_codification('21f78ae5-9e1e-426d-8f8d-f1aba1933967', formalized).
narrative_ontology:cs_authority_grounding('21f78ae5-9e1e-426d-8f8d-f1aba1933967', extraction).
narrative_ontology:cs_interpretation_layer_present('21f78ae5-9e1e-426d-8f8d-f1aba1933967').
narrative_ontology:cs_reading_relation('21f78ae5-9e1e-426d-8f8d-f1aba1933967', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('21f78ae5-9e1e-426d-8f8d-f1aba1933967', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('21f78ae5-9e1e-426d-8f8d-f1aba1933967', foundational, external_obligation_conditional_on_domestic_autonomy).
narrative_ontology:cs_axiom_status(external_obligation_conditional_on_domestic_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('21f78ae5-9e1e-426d-8f8d-f1aba1933967', external_obligation_conditional_on_domestic_autonomy, instrumental).
narrative_ontology:cs_axiom('21f78ae5-9e1e-426d-8f8d-f1aba1933967', secondary, reserve_asset_seigniorage_justified_by_coordination_benefit).
narrative_ontology:cs_axiom_status(reserve_asset_seigniorage_justified_by_coordination_benefit, holdable).
narrative_ontology:cs_axiom_grounding('21f78ae5-9e1e-426d-8f8d-f1aba1933967', reserve_asset_seigniorage_justified_by_coordination_benefit, empirically_contingent).
narrative_ontology:cs_reference_frame('21f78ae5-9e1e-426d-8f8d-f1aba1933967', conditional_external_commitment_subordinate_to_domestic_stability).
narrative_ontology:cs_drift_state('21f78ae5-9e1e-426d-8f8d-f1aba1933967', id_1971_suspension, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('21f78ae5-9e1e-426d-8f8d-f1aba1933967', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_government).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, dollar_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, private_sector_exporters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_domestic_constituencies).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Unilaterally sets and enforces the convertibility commitment at $35/oz, retaining implicit authority to suspend or adjust if domestic economic stability is threatened. Collects seigniorage from reserve-asset status (the ability to spend dollars globally without equivalent goods backing) and preserves monetary autonomy — the U.S. can pursue full-employment policies without hard external constraint. The commitment is conditional on domestic needs: if inflation, capital flight, or gold depletion emerges, the U.S. can prioritize domestic recovery over external obligations.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_government, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dollar balances as reserves, believing they can convert to gold at $35/oz if needed, but facing subordination to U.S. policy: the conversion right is conditional on U.S. domestic priorities, not a binding legal guarantee. Bear devaluation risk — if the U.S. inflates domestically or suspends convertibility, their dollar holdings lose purchasing power and they cannot costlessly diversify away without triggering the run they fear. Their exit (converting dollars to other assets) is constrained because mass conversion would crash the system and destroy reserve holders' own positions.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, dollar_reserve_holders, payer,
    organized, biographical, constrained, global).

% Accumulate dollar reserves as the system's anchor, gaining a stable external reference for their own currency policies and a convenient store of value. Benefit from the coordination function (reduced transaction costs, aligned international pricing). Also carry subordination risk: their gold redemption rights are conditional on U.S. domestic choices, and as U.S. liabilities grow relative to gold, they face increasing devaluation risk and the dilemma of the Triffin problem (whether to demand conversion and crash the system or accept devaluation quietly).
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer).

% Face exchange-rate volatility tied to convertibility credibility. As doubts about U.S. commitment grow (visible in gold reserves declining, in inflation in the U.S., in recurring crises like the 1968 gold pool), they face uncertainty about the real value of foreign-currency revenues — a dollar earned today may be worth less tomorrow if the commitment breaks. Cannot influence the policy but depend on its stability for predictable international pricing. Their real costs rise as the constraint's credibility declines.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, private_sector_exporters, payer,
    moderate, biographical, constrained, global).

% Benefit indirectly from the monetary autonomy the policy-flexible reading preserves. The U.S. government can pursue full-employment and growth policies without the hard discipline a binding external commitment would impose — stimulus spending, investment in social programs, and inflation-accommodating policies are possible because convertibility is conditional, not absolute. They win economic growth and employment; the cost (subordinated external obligations and devaluation risk for reserve holders) is diffuse and externalized.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_domestic_constituencies, beneficiary,
    organized, biographical, mobile, national).

% Constitutionally established as the guardian of the convertibility commitment (Article IV), but lacks enforcement power against U.S. unilateral suspension. Watches as the policy-flexible reading transforms a legal obligation into a conditional commitment, effectively distributing systemic risk onto reserve holders and foreign central banks without formal amendment or IMF authorization. The institution's authority is subordinated to U.S. power.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_monetary_fund, observer,
    institutional, generational, analytical, global).

% The physical stock of gold backing the U.S. commitment. The constraint's persistence depends on this stock's ratio to potential redemption claims. As U.S. deficits accumulate and foreign dollar liabilities grow, the ratio declines (from ~60% of world gold in 1945 to ~50% by 1960 to ~35% by 1971). The U.S. faces a choice as the ratio approaches unsustainability: either enforce real policy discipline (reduce deficits, control inflation) to preserve the external commitment, or abandon the commitment and pursue domestic growth. The gold stock is not an agent but a constraint on the U.S. agenda-setter's options.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_gold_reserves_stock, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__policy_flexible_reading, us_gold_reserves_stock).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, united_states_government).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes the U.S. dollar as the international reserve asset backed by a declared gold commitment: provides a stable numeraire for international transactions, reduces transaction costs across currency pairs, and anchors foreign-currency policy to a known external reference point.
% TRANSFER_FUNCTION: Transfers the costs of dollar-devaluation risk and the discipline burden to dollar reserve holders (who face redemption risk if the U.S. prioritizes domestic growth) and to foreign exporters (whose revenues and competitiveness are exposed to exchange-rate movement tied to convertibility credibility), while transferring the benefit of monetary autonomy and seigniorage to the U.S. government and its domestic constituencies.
% ABSENT_VOICES: Gold miners and commodity producers outside the dollar zone are structurally excluded from the framework; so too are private individuals in the U.S. who might want to hold gold or exit the dollar system. The Soviet Union and non-aligned nations dispute whether dollar-based convertibility is a legitimate international framework at all. These voices are not in the negotiating room.
% DISAPPEARANCE_RATIONALE: If convertibility disappeared (suspended or replaced), foreign central banks would scramble to diversify reserves, exchanging dollars for gold, other assets, or national currencies; the anchor for international pricing would dissolve; U.S. domestic monetary policy would face different trade-offs (less external discipline but also less reserve-asset seigniorage); exchange rates would reprice as the stability the dollar provided evaporated; and international trade would reorganize around alternative reference points or barter arrangements.
% FOUNDING_PROBLEM: Post-WWII international commerce needed a stable, trusted medium of exchange for cross-border transactions and a neutral, binding reference point for currency pegs to replace the competitive devaluations and trade wars of the 1930s.
% FOUNDING_PROBLEM_CORROBORATION: The IMF and U.S. negotiators at Bretton Woods (1944) attested the founding problem; Keynes and White explicitly designed the system to avoid 1930s chaos. Structural economists and later historians (Triffin, Eichengreen, Steil) attest the problem shifted: by the 1960s, the binding commitment became a constraint on U.S. policy and an inherent instability (the Triffin dilemma) rather than a solution. Foreign central banks and the Bank for International Settlements attest that the policy-flexible reading emerged as the de facto framework in the late 1950s–early 1960s as the U.S. prioritized domestic growth over strict discipline, and the official fiction of a binding commitment persisted until 1971 suspension. No neutral external voice — all attestations come from parties experiencing the constraint's benefit or cost.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1944) to 0.72 (1971) because the asymmetry grows as U.S. gold reserves decline relative to dollar liabilities. The system was initially viable — the U.S. held 60% of world gold at the end of WWII — but as deficits mounted and foreign dollar holdings grew, the U.S. faced a choice: (1) enforce fiscal and monetary discipline to preserve the external commitment, or (2) continue domestic full-employment policy and risk devaluation. The policy-flexible reading asserts the U.S. implicitly chose (2) and subordinated the external commitment to domestic stability. Suppression stays moderate (0.25–0.55) because the constraint operated through indirect mechanisms: foreign central banks could technically demand gold, but doing so en masse would crash the system and destroy their own reserves' value, so they self-suppressed redemption demands until 1971. Theater rises from 0.05 (1944) to 0.35 (1971) as the gap widens between official assertion of commitment and actual policy flexibility — by the 1960s, the constraint was sustained mostly by international agreement to not test it (cooperative performance of a weakening story). Accessibility_collapse is moderate (0.41) because alternatives existed but were not costless: foreign central banks could diversify into gold, sterling, or other assets, but doing so would disrupt their own systems; dollar holders could demand redemption but would trigger the crisis they feared. Resistance is high (0.67) because the system faced constant pressure: the London Gold Pool failed in 1968, the two-tier market emerged that year, and the U.S. ultimately suspended convertibility in 1971 — the constraint did not persist through consensus but through explicit policy choice to withdraw from it.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is critical here. From the U.S. government seat, convertibility is a conditional commitment serving dual functions: (1) providing seigniorage and monetary autonomy domestically, (2) anchoring the international system externally. The U.S. position is that subordinating the external commitment to domestic stability is the correct policy — the constraint is justifiable as enabling broader macroeconomic coordination. From the dollar-holder seat, the constraint is a subordination: their redemption right is revoked at U.S. discretion, they bear devaluation risk, and they are trapped because diversifying away triggers the crisis they fear. From the foreign central bank seat, the constraint is initially attractive (a stable external anchor) but becomes extractive as it becomes clear the commitment is conditional on U.S. preferences. The engine computes different d values per seat: the U.S. government sits near d=0.2 (full beneficiary), dollar holders near d=0.8 (full target), foreign central banks near d=0.6 (split — they benefit from the coordination initially but become victims as the system degrades). These divergences emerge directly from the stakeholder declarations without tuning; they model the real structural asymmetry of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. government benefits from monetary autonomy and seigniorage — it retains the right to adjust policy domestically regardless of external gold constraints. Dollar holders and foreign reserve accumulations face subordination: their claims on gold are conditional on U.S. domestic priorities, not binding legal rights. The policy-flexible reading treats this asymmetry as the core structure: the constraint exists to enable U.S. domestic coordination (full employment, growth) while imposing costs on external parties (devaluation risk, limited policy space for other central banks). The beneficiary set is the U.S. government (decision-maker) and foreign central banks (at least initially, while the system functions). The victim set is dollar reserve holders (face subordination and devaluation risk) and exporters (exposed to exchange volatility). This directionality choice distinguishes policy_flexible from strict_convertibility: the strict reading would make the U.S. a constrained party (victim of external discipline) rather than a beneficiary of conditional escape clauses.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII trade chaos and the need for a stable numeraire) was genuinely live in 1944–1960. By the 1960s, it was contested: the system had accomplished its initial goal (stabilized trade) but accumulated a structural problem (Triffin dilemma: as the U.S. ran deficits and foreign dollar liabilities grew, the backing gold declined and convertibility became unsustainable). The policy-flexible reading's mandate split: one part (serve as international anchor) remained live; the other part (subordinate U.S. domestic policy to external discipline) was actively rejected by the U.S. government. By 1971, when convertibility was suspended, the founding problem was functionally dead (the system could no longer perform its coordination function) but the constraint persisted theatrically (official statements claimed commitment even as policy abandoned it). This mandatrophy — the constraint persisting while its justifying function eroded — is reflected in the rising theater_ratio and the gap between suppression (stable at ~0.5) and extractiveness (rising to 0.72). The constraint did not fail because no one believed in it; it failed because the U.S. chose a different policy path (prioritizing domestic autonomy over external commitment) and the system collapsed when the accumulated contradictions became unsustainable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_flexibility_vs_legal_obligation,
    'Is convertibility a binding legal obligation under Article IV of the IMF Articles of Agreement, or a unilateral conditional commitment the U.S. could suspend at will?',
    'IMF legal interpretation and U.S. statutory/constitutional analysis. The U.S. never formally amended its commitment but suspended convertibility in 1971 claiming emergency powers — did suspension violate Article IV or exercise an implicit right to subordinate external obligations to domestic stability?',
    'If binding, the constraint is a strict_convertibility_reading and the U.S. faces a hard discipline; if conditional, the U.S. retains monetary autonomy and the constraint is a tangled_rope where the conditionality itself is extractive (dollar holders bear devaluation risk subordinate to U.S. domestic needs). The reading classification turns on this distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_flexibility_vs_legal_obligation, conceptual, 'Whether convertibility is legally binding or conditionally declared.').

omega_variable(
    triffin_dilemma_inevitability,
    'Is the growth of dollar liabilities outpacing gold reserves an inevitable consequence of the system''s design (Triffin), or a policy failure (excessive U.S. deficits)?',
    'Structural analysis comparing Bretton Woods to alternative fixed-rate systems with different reserve arrangements. If all fixed-rate reserve-asset systems face similar pressures, Triffin''s diagnosis is validated; if other systems avoided it, the U.S. failed policy discipline.',
    'If inevitable, the policy-flexible reading is a temporary accommodation of an inherent flaw — extractiveness is built into the system and will eventually force suspension; if policy-failure, the extractiveness is contingent on U.S. choices (deficits, inflation) and could have been avoided through fiscal discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, empirical, 'Whether reserve-asset convertibility systems are structurally doomed or policy-dependent.').

omega_variable(
    suppression_internalization,
    'Do foreign central banks and reserve holders accept convertibility as conditional on U.S. domestic needs, or do they continue to believe the commitment is binding despite mounting evidence of policy flexibility?',
    'Post-1971 oral histories and archival evidence from central bank meetings in the 1960s show the growing awareness of the contradiction; the 1968 London Gold Pool collapse and the 1969 two-tier market are signals that some parties no longer believed the commitment. Track when belief shifted from ''this will hold'' to ''this will break when the U.S. decides.''',
    'If suppression was internalized (the commitment was believed binding even as the U.S. violated it in practice), the measured suppression understates the actual coercive asymmetry — the constraint worked through false expectations. If suppression was structural (external barriers to exit that persisted regardless of belief), the measured value reflects real constraint mechanics. The distinction affects whether the constraint is better classified as snare (false consciousness) vs. tangled_rope (structural subordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Suppression mechanism: internalized belief vs. structural barrier.').

omega_variable(
    kernel_reading_vs_strict_convertibility,
    'This reading (policy_flexible_reading) treats convertibility as conditional on domestic stability. The sibling strict_convertibility_reading treats it as a binding legal obligation. How do these readings relate structurally?',
    'The two readings rest on opposite interpretations of Article IV: one emphasizes the legal binding force; the other emphasizes the embedded exception clause (''to the extent permitted by its economic situation''). This is a genuine kernel contest — both readings cite the same text but extract different commitments from it. The policy-flexible reading coexists_with strict_convertibility because different parties held them simultaneously (the U.S. held the flexible reading internally; the IMF and foreign central banks formally endorsed the strict reading). Neither forecloses the other within a single framework — they are genuinely alternative readings of an ambiguous commitment.',
    'The two readings compute different victim sets: strict_convertibility makes the U.S. a victim (constrained by external discipline); policy_flexible makes dollar holders victims (subordinated to U.S. domestic needs). The kernel is the ambiguous text; the readings are institutional positions on what the text commits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_vs_strict_convertibility, conceptual, 'Kernel reading relationship: policy-flexible vs. strict-convertibility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(doll_tr_t1952, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1952, 0.08).
narrative_ontology:measurement(doll_tr_t1960, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1965, 0.22).
narrative_ontology:measurement(doll_tr_t1969, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1969, 0.28).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.35).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement(doll_be_t1952, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1952, 0.42).
narrative_ontology:measurement(doll_be_t1960, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1965, 0.62).
narrative_ontology:measurement(doll_be_t1969, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1969, 0.68).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.25).
narrative_ontology:measurement(doll_su_t1952, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1952, 0.32).
narrative_ontology:measurement(doll_su_t1960, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(doll_su_t1969, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1969, 0.52).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_system_overall).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, us_dollar_hegemony_seigniorage).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'dollar-gold-convertibility'. The policy-flexible reading emphasizes U.S. conditional commitment and subordinates external discipline to domestic stability. The strict-convertibility reading emphasizes the binding legal obligation and constrains U.S. policy from outside. The Triffin reading emphasizes the structural contradiction (reserves vs. gold growth) and treats the system as doomed. All three share the same text (Article IV) but extract different institutional commitments. The ε values diverge: policy-flexible registers high extractiveness (U.S. benefits from escape clause), strict-convertibility registers lower extractiveness for the U.S. (constrained by binding commitment), Triffin registers system-level incoherence. Siblings in the family are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, organized, 0.75).
constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
