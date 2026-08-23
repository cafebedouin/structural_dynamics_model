% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__strict_convertibility_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Convertibility as Binding Legal Obligation (Strict Reading)
 *   domain: international_law/monetary_history/political_economy
 *
 * SUMMARY:
 *   The strict_convertibility_reading treats Article IV of the IMF Articles
 *   of Agreement as a peremptory legal obligation: the United States, as the
 *   sole gold-convertibility guarantor, must maintain the $35/oz peg
 *   regardless of domestic economic conditions. This reading was championed
 *   by creditor nations (especially France under de Gaulle) and the IMF
 *   staff, and was the operative framework until the 1971 suspension.
 *   Structurally, it creates a Tangled Rope: the fixed-rate system
 *   coordinates global trade (genuine coordination function) while
 *   asymmetrically extracting policy autonomy from the U.S. to benefit
 *   creditor nations. The U.S. enters the victim set as the constrained
 *   issuer; creditor nations are beneficiaries with enforceable conversion
 *   rights. The constraint requires active enforcement (IMF surveillance,
 *   Gold Pool, diplomatic pressure) and suppresses alternatives (floating
 *   rates, SDR substitution). The measurement series shows rising
 *   extractiveness and suppression as European recovery shifted the balance
 *   from coordination to extraction.
 *
 * KEY AGENTS:
 *   - united_states_treasury_fed: Primary target (powerful/constrained) — bears extraction as constrained issuer
 *   - creditor_nations: Primary beneficiary (powerful/mobile) — holds enforceable gold claims
 *   - imf_institution: Agenda setter (institutional/analytical) — enforces the constraint
 *   - global_financial_markets: Observer (organized/arbitrage) — disciplines via capital flows
 *   - developing_countries: Excluded (powerless/trapped) — bears spillovers without voice
 *   - academic_economists: Observer (analytical/analytical) — diagnoses structural contradiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.75).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.82).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Convertibility as Binding Legal Obligation (Strict Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_law/monetary_history/political_economy").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, 'a8142839-0b8a-4ac8-aa63-68a0128865b6').
narrative_ontology:cs_kernel_codification('a8142839-0b8a-4ac8-aa63-68a0128865b6', formalized).
narrative_ontology:cs_authority_grounding('a8142839-0b8a-4ac8-aa63-68a0128865b6', lineage).
narrative_ontology:cs_interpretation_layer_present('a8142839-0b8a-4ac8-aa63-68a0128865b6').
narrative_ontology:cs_reading_relation('a8142839-0b8a-4ac8-aa63-68a0128865b6', dollar_gold_convertibility__policy_flexible_reading, forecloses).
narrative_ontology:cs_reading_relation('a8142839-0b8a-4ac8-aa63-68a0128865b6', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('a8142839-0b8a-4ac8-aa63-68a0128865b6', foundational, convertibility_as_peremptory_obligation).
narrative_ontology:cs_axiom_status(convertibility_as_peremptory_obligation, holdable).
narrative_ontology:cs_axiom_grounding('a8142839-0b8a-4ac8-aa63-68a0128865b6', convertibility_as_peremptory_obligation, conventional).
narrative_ontology:cs_axiom('a8142839-0b8a-4ac8-aa63-68a0128865b6', secondary, symmetric_adjustment_duty).
narrative_ontology:cs_axiom_status(symmetric_adjustment_duty, holdable).
narrative_ontology:cs_axiom_grounding('a8142839-0b8a-4ac8-aa63-68a0128865b6', symmetric_adjustment_duty, conventional).
narrative_ontology:cs_reference_frame('a8142839-0b8a-4ac8-aa63-68a0128865b6', bretton_woods_parity_system).
narrative_ontology:cs_drift_state('a8142839-0b8a-4ac8-aa63-68a0128865b6', contemporary_1971, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a8142839-0b8a-4ac8-aa63-68a0128865b6', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, imf_institution).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, united_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, united_states_treasury_fed).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_parity_system).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, international_monetary_cooperation_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, rule_of_law_in_monetary_affairs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dollar-gold peg and the Bretton Woods system as the reserve currency issuer. Simultaneously bears the asymmetric adjustment burden: must contract monetary policy to defend gold convertibility, limiting domestic employment and growth. Exit requires abandoning the system it created and leads — politically and institutionally costly. The Triffin dilemma makes the constraint structurally inescapable while the system persists.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, united_states_treasury_fed, agenda_setter,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, united_states_treasury_fed, payer).

% Major surplus countries (France, Germany, Italy, Japan) accumulate dollar reserves convertible into gold at $35/oz. They benefit from stable exchange rates for export-led growth and hold an enforceable legal claim on U.S. gold. Can threaten conversion (as France did under de Gaulle) to extract policy concessions. Exit is mobile — they can diversify reserves or demand gold — giving them leverage over the constraint's operation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    powerful, biographical, mobile, global).

% The institutional enforcer of Article IV obligations. Conducts surveillance, approves parity changes, and coordinates the Gold Pool (1961-1968) to defend the peg. Its authority derives from the treaty text and the consensus of member states. Does not extract for itself but sustains the constraint that distributes costs and benefits asymmetrically. Exit is analytical — it observes and adjudicates rather than participates in the extraction.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_institution, agenda_setter,
    institutional, generational, analytical, global).

% Private banks, arbitrageurs, and speculators who test the peg's credibility daily. Their capital flows enforce discipline: gold drain signals loss of confidence. They have arbitrage-grade exit — can move instantly — and their collective action determines the constraint's viability. Not a party to the treaty but a decisive structural actor.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, global_financial_markets, observer,
    organized, immediate, arbitrage, global).

% IMF members with no reserve currency role and limited gold holdings. Subject to the system's discipline (conditionality, parity rules) but excluded from the core bargain between the U.S. and creditor nations. Bear adjustment costs via trade and capital flow effects without the convertibility guarantee. Would object to asymmetric burden but lack voice in the governance structure.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, developing_countries, excluded,
    powerless, generational, trapped, global).

% Theorists (Triffin, Mundell, Johnson, Kindleberger) who diagnosed the system's structural contradiction. Their analyses shaped policy discourse and provided the intellectual framework for the policy_flexible_reading and triffin_structural_reading. No material stake in the constraint's operation; exit is analytical — they observe from outside the system.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, academic_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, rule-based international monetary order: fixed exchange rates enable predictable trade and investment flows; the dollar-gold anchor solves the n-th currency problem; IMF surveillance coordinates adjustment and prevents competitive devaluations.
% TRANSFER_FUNCTION: Moves policy autonomy from the United States to creditor nations. The U.S. bears the adjustment burden (deflationary pressure, gold loss, constrained domestic objectives) while creditor nations gain stable dollar reserves, export competitiveness, and an enforceable claim on U.S. gold. The IMF institution gains operational authority as enforcer.
% ABSENT_VOICES: Developing countries (excluded stakeholder) would object to the asymmetric adjustment burden and the system's bias toward creditor interests. Their voice was structurally absent from the Bretton Woods governance design — the Executive Board's weighted voting and the G10's informal dominance kept them out of core decisions.
% DISAPPEARANCE_RATIONALE: If the Article IV convertibility obligation vanished overnight, the Bretton Woods parity system would collapse. The U.S. would regain monetary autonomy; creditor nations would lose their gold claim and face currency appreciation; the IMF would lose its core enforcement mandate; global trade would shift to floating rates. The 1971 Nixon shock confirmed this: the world rearranged completely.
% FOUNDING_PROBLEM: Post-war monetary chaos: competitive devaluations of the 1930s, collapsed trade, absence of a stable reserve asset, and no institutional mechanism for coordinated adjustment. The Bretton Woods conference (1944) built the convertibility obligation to solve this.
% FOUNDING_PROBLEM_CORROBORATION: Eichengreen (Globalizing Capital), Bordo (Gold Standard), and James (International Monetary Cooperation) — historians outside the beneficiary set — document that the 1930s chaos was resolved by 1944 but the specific convertibility mechanism became obsolete as European economies recovered and the Triffin dilemma emerged. The U.S. Treasury's own 1960s internal memos (declassified) acknowledge the founding problem was solved but the constraint persisted.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__strict_convertibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) reflects the Triffin dilemma's maturity: by 1971, U.S. gold coverage of external liabilities fell below 25%, and the policy cost of defending the peg (recessionary pressure, forgone stimulus) was severe. Suppression (0.82) is high because the constraint's persistence depended on active enforcement — the Gold Pool (1961-68), IMF pressure on surplus countries to not convert, and U.S. diplomatic arm-twisting. Theater ratio (0.38) is moderate: the coordination function (stable trade) was real but a growing share of enforcement defended the payment exclusivity rather than the system's stability. Accessibility collapse (0.72) is high because floating rates were treated as illegitimate 'disorder' until 1971; resistance (0.62) reflects U.S. pushback (Roosa bonds, swap lines, SDR creation) that delayed but did not prevent collapse.
 *
 * PERSPECTIVAL GAP:
 *   The strict reading's own frame (binding legal obligation) produces a Mountain-like self-classification from the creditor/IMF seats: the law is the law, compliance is coordination. From the U.S. seat the same structure is a Snare — law as extraction tool. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) is the generating model's structural assessment, not the reading's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury/Fed is the primary target (d near 1.0): bears the conversion risk, policy constraint, and gold loss. Creditor nations are beneficiaries (d near 0.0): hold enforceable gold claims, gain stable reserves, can force adjustment via conversion threats. The IMF institution is an agenda_setter with analytical exit — it enforces but does not extract. Global markets have arbitrage exit — they discipline but do not bear the constraint. Developing countries are excluded — trapped by the system's rules but without voice. The dual role of the U.S. (agenda_setter + payer) creates the seat divergence the engine will compute: from the U.S. seat the constraint is extractive; from the creditor seat it is coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1930s monetary chaos) was solved by the late 1950s — European recover, current account convertibility restored. But the convertibility obligation persisted and intensified as extraction. The mandatrophy is resolved: the constraint outlived its coordination function and became a vehicle for creditor extraction. The 1971 suspension was the belated recognition. The founding_problem_status = dead + disappearance_verdict = world_rearranges mismatch flags this as a captured/zombie constraint in the engine's consumption logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_origin,
    'Was Article IV convertibility designed as a genuine coordination mechanism that became extractive, or was the asymmetric extraction built into the U.S. reserve-currency role from the start?',
    'Archival research on Bretton Woods negotiating records (White vs. Keynes plans), U.S. Treasury internal deliberations 1944-1945, and the drafting history of Article IV Sections 3-4.',
    'If extraction was original, the constraint is a Snare from inception; if coordination degraded into extraction, it is a Tangled Rope with a genuine coordination phase. Changes the mandatrophy analysis and the founding_problem_status assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_origin, conceptual, 'Whether the constraint''s extractive structure is original or accumulated.').

omega_variable(
    exorbitant_privilege_offset,
    'Did the U.S. ''exorbitant privilege'' (seigniorage, lower borrowing costs, ability to run deficits) materially offset the convertibility constraint''s extraction, making the net position closer to symmetric?',
    'Quantitative economic history: measure U.S. seigniorage gains, interest rate differentials, and deficit financing benefits against the gold loss and policy constraint costs, 1945-1971.',
    'If privilege offsets extraction, effective extraction for the U.S. seat drops, potentially reclassifying from payer toward symmetric. If not, the victim status is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exorbitant_privilege_offset, empirical, 'Whether reserve currency benefits cancel the convertibility constraint''s costs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of U.S. policy autonomy structural (IMF rules, gold pool mechanics, creditor conversion rights) or internalized (U.S. policy consensus that ''defending the dollar'' was the national interest)?',
    'Compare U.S. policy behavior when formal constraints were binding (1960s) vs. after 1971 when only internalized norms remained. If policy autonomy remained constrained post-1971, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure — the U.S. carried the suppression with it after formal exit. This affects the piton/tangled_rope boundary and the theater_ratio interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the constrained issuer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_scr_tr_t0, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(dgc_scr_tr_t5, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(dgc_scr_tr_t10, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(dgc_scr_tr_t15, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 15, 0.31).
narrative_ontology:measurement(dgc_scr_tr_t20, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(dgc_scr_tr_t25, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement(dgc_scr_tr_t27, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 27, 0.38).

% Extraction over time
narrative_ontology:measurement(dgc_scr_be_t0, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dgc_scr_be_t5, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(dgc_scr_be_t10, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(dgc_scr_be_t15, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(dgc_scr_be_t20, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(dgc_scr_be_t25, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(dgc_scr_be_t27, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 27, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(dgc_scr_su_t0, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dgc_scr_su_t5, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(dgc_scr_su_t10, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(dgc_scr_su_t15, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(dgc_scr_su_t20, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(dgc_scr_su_t25, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(dgc_scr_su_t27, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 27, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__strict_convertibility_reading, 0.12).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% BGS-pattern decomposition of the dollar_gold_convertibility kernel: this reading (strict_convertibility) is the downstream contested claim; the policy_flexible_reading is the operational compromise; the triffin_structural_reading is the structural diagnosis. All three share the Article IV referent but instantiate different constraints with different ε, beneficiaries, and victims. The strict reading's high extraction from the U.S. seat is what the Triffin critique identifies as the system's fatal flaw.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
