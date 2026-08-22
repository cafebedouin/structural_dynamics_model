% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Article IV Gold Convertibility as Binding Legal Constraint on U.S. Monetary Policy
 *   domain: international political economy / monetary history / international law
 *
 * SUMMARY:
 *   This story instantiates the strict-convertibility reading of the Bretton
 *   Woods kernel: Article IV of the IMF Articles of Agreement, and the
 *   parallel U.S. commitment to redeem officially held dollars for gold at
 *   $35/oz, is treated as a binding legal obligation — not a conditional
 *   policy preference and not merely a symptom of an unsustainable design. On
 *   this reading the obligation genuinely bound U.S. monetary and fiscal
 *   policy from the late 1950s through 1971: the Federal Reserve's rate
 *   decisions, Treasury's gold-pool interventions, and even domestic fiscal
 *   restraint were shaped by the need to defend a treaty-level commitment
 *   enforceable by any foreign central bank holding dollars. Rising foreign
 *   dollar liabilities against a roughly static U.S. gold stock (the 'dollar
 *   overhang') transformed a coordination device — a stable reserve anchor
 *   for postwar trade — into an instrument through which creditor nations
 *   held an enforceable claim against U.S. policy space. The extractiveness
 *   this story authors is high specifically because, under this reading, the
 *   obligation is a binding legal fact that subordinated U.S. domestic
 *   priorities to external claim-holders' redemption rights, not a
 *   discretionary courtesy the U.S. could unilaterally recalibrate.
 *
 * KEY AGENTS:
 *   - us_treasury: primary target/payer — bears the legal redemption obligation and reserve constraint
 *   - us_federal_reserve: agenda-setting instrument that administers defense of the parity, bearing the domestic policy cost
 *   - european_creditor_central_banks: primary beneficiary — holds enforceable conversion claims
 *   - gold_pool_surplus_nations: beneficiary with strategic leverage (notably France)
 *   - us_domestic_labor_market: diffuse victim bearing the real economic cost of externally-driven policy
 *   - imf_bretton_woods_secretariat: analytical observer administering the treaty framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.71).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.62).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Gold Convertibility as Binding Legal Constraint on U.S. Monetary Policy").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international political economy / monetary history / international law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0').
narrative_ontology:cs_kernel_codification('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', formalized).
narrative_ontology:cs_authority_grounding('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', lineage).
narrative_ontology:cs_interpretation_layer_present('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0').
narrative_ontology:cs_reading_relation('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', foundational, convertibility_is_enforceable_treaty_law).
narrative_ontology:cs_axiom_status(convertibility_is_enforceable_treaty_law, holdable).
narrative_ontology:cs_axiom_grounding('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', convertibility_is_enforceable_treaty_law, conventional).
narrative_ontology:cs_axiom('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', secondary, creditor_redemption_claims_bind_issuer_policy_space).
narrative_ontology:cs_axiom_status(creditor_redemption_claims_bind_issuer_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', creditor_redemption_claims_bind_issuer_policy_space, conventional).
narrative_ontology:cs_reference_frame('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', bretton_woods_treaty_obligation_framework).
narrative_ontology:cs_drift_state('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', nixon_shock_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('4a8aa8f7-f3cf-4140-b751-b483ed9d8cf0', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, european_creditor_central_banks).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_surplus_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_treasury).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_labor_market).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_treaty_obligation_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, fixed_parity_credibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound under the Bretton Woods Articles of Agreement to redeem officially held dollars for gold at $35/oz on demand from foreign monetary authorities. Must hold gold reserves against an ever-growing stock of dollar liabilities accumulated abroad. Domestic fiscal and monetary choices — deficit spending, interest rate policy — are read by creditor governments as convertibility risk, and Treasury must defend the parity even when doing so conflicts with domestic employment or growth objectives. Exit would mean unilateral suspension, breaking a signed treaty obligation.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_treasury, payer,
    institutional, generational, constrained, global).

% Must set interest rates partly to defend the external gold position (raising rates to attract capital and discourage conversion demands) even when domestic conditions call for easing. Administers the convertibility mechanism day to day but does not benefit from it — it is the instrument through which the obligation binds, not a collector of its proceeds.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve, agenda_setter).

% Hold large and growing dollar reserves from trade surpluses and can present them to the U.S. Treasury for gold redemption at any time under the treaty's terms. This enforceable claim gives them direct leverage over U.S. policy: they can extract gold, threaten to extract gold, or use the threat as bargaining power in monetary diplomacy. Their exit option — converting dollars to gold — is exactly the mechanism that constrains the U.S.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, european_creditor_central_banks, beneficiary,
    institutional, generational, arbitrage, global).

% Nations running persistent trade surpluses with the U.S. (notably France under de Gaulle's policy) accumulate dollars as a matter of course and hold the legal right, under Article IV, to convert them. They benefit from a stable gold price underwritten by U.S. commitment while retaining full freedom to press redemption claims for strategic or economic reasons.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, gold_pool_surplus_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Commercial banks and smaller monetary authorities holding dollars as a reserve asset benefit from the credibility the convertibility promise lends to the dollar, while facing none of the burden of defending the parity. They can shift reserve composition if confidence erodes, transferring pressure back onto the U.S. without bearing symmetric obligations themselves.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, foreign_dollar_reserve_holders, beneficiary,
    organized, generational, mobile, global).

% Bears the real economic cost when the Fed tightens policy to defend the gold parity rather than to address domestic unemployment or wage stagnation. Workers have no voice in international monetary diplomacy and no ability to exit the domestic labor market in response to policy set for external reasons.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_labor_market, payer,
    powerless, biographical, trapped, national).

% Legally empowered to legislate on monetary policy but structurally sidelined from the Article IV obligation, which was negotiated and is administered as an executive-branch treaty commitment enforced through Treasury and the Fed. Domestic political demands for expansionary policy routinely collide with the convertibility constraint without Congress having direct control over the international obligation itself.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_congress, excluded,
    institutional, biographical, constrained, national).

% Administers the treaty framework, monitors par values, and mediates disputes over convertibility obligations, without itself bearing the cost of U.S. gold outflows or collecting from them directly.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_bretton_woods_secretariat, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A fixed exchange rate anchored to gold-convertible dollars solves the genuine problem of exchange-rate uncertainty in postwar trade and investment by giving all participants a stable, treaty-guaranteed unit of account and reserve asset.
% TRANSFER_FUNCTION: Moves real policy sovereignty and, at the margin, physical gold reserves from the United States (which must subordinate domestic monetary choices to parity defense and can be drained of reserves on demand) to creditor and surplus nations (which hold enforceable conversion claims and benefit from dollar-based reserve stability without bearing the parity-defense cost).
% ABSENT_VOICES: U.S. domestic constituencies — labor, Congress acting on domestic priorities — are structurally absent from the diplomatic table where convertibility terms and gold-pool arrangements are negotiated and defended; they experience the policy consequences without a seat in the treaty-enforcement conversations.
% DISAPPEARANCE_RATIONALE: If the binding legal convertibility obligation vanished overnight (as it in fact did in August 1971), the Federal Reserve would no longer need to set policy defensively against gold-drain risk, creditor nations would lose their enforceable conversion claim and associated leverage, and the entire Bretton Woods par-value system would require replacement — which is exactly what happened, rearranging global monetary arrangements into the floating-rate era.
% FOUNDING_PROBLEM: Interwar currency instability, competitive devaluation, and the absence of a credible international reserve standard were seen as having contributed to the collapse of international trade and cooperation in the 1930s; convertibility was built to restore a credible, rules-based anchor.
% FOUNDING_PROBLEM_CORROBORATION: U.S. Treasury officials (Connally, and earlier Fowler) and independent monetary economists outside the beneficiary creditor governments (including contemporaneous IMF staff analyses of the dollar glut) attested by the late 1960s that dollar liabilities had grown so far beyond U.S. gold reserves that the original stable-anchor function could no longer be honored without domestic economic damage; creditor central banks themselves (France notably) continued to assert the obligation was live and enforceable up to the point of the 1971 suspension, which is the contested split this reading takes a side on.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.42 (1958, obligation still broadly sustainable) to 0.71 (1971, obligation clearly binding against a depleted reserve base) as the dollar overhang grew and the legal claim's bite intensified. Suppression is authored moderate-high (0.62) because the binding-treaty reading holds that the U.S. could not exit unilaterally without violating international law and damaging its credibility — the constraint held via legal and reputational force, not mere preference. Theater ratio stays comparatively low (0.28) because, on this reading, most of the activity (gold pool operations, Fed rate defense, Treasury swap lines) was functionally real defense of a live legal obligation rather than performance — though rising toward the end as defense mechanisms became increasingly symbolic relative to the underlying reserve shortfall.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury and Federal Reserve sit at the target end of directionality: they carry the legal obligation, cannot exit without breach, and absorb the policy cost. Creditor central banks and surplus nations sit at the beneficiary end: they hold the enforceable claim and can convert at will, extracting gold or leverage without symmetric obligation. Domestic labor bears cost with no voice and no exit — trapped, powerless — consistent with a high derived d despite not being a party to the treaty itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (postwar exchange-rate instability) as dead by the late 1960s while the legal obligation persisted, generating a tangled_rope classification: the coordination function (stable reserve anchor) was real and valuable for a period, but by the point of measurement the same structure had become predominantly an asymmetric transfer mechanism, requiring active defense (swap lines, gold pool, rate policy) to sustain a legal claim whose economic justification had eroded. This is precisely the mandatrophy the classification exists to catch: a coordination device whose mandate outlived its function but which persisted because it was a binding legal commitment, not because it continued to solve the coordination problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    convertibility_legal_bindingness_ambiguity,
    'Was Article IV convertibility genuinely a binding legal obligation enforceable against the United States, or was it always understood by all parties as a conditional commitment subordinate to U.S. domestic policy discretion?',
    'Close reading of Bretton Woods negotiating history, U.S. Treasury and State Department internal correspondence from 1958-1971, and comparison with actual enforcement practice — did creditor nations ever successfully compel U.S. policy change through legal or treaty channels, or only through market pressure (redemption threats)?',
    'If the obligation was never truly binding in an enforceable legal sense, this reading''s high extractiveness score and its designation of the U.S. as a constrained victim are overstated, and the policy_flexible_reading better describes the actual structure. If it was genuinely binding, this reading''s structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convertibility_legal_bindingness_ambiguity, conceptual, 'Whether convertibility was truly a binding legal constraint or a conditional policy commitment — the central premise this reading depends on.').

omega_variable(
    committer_kernel_reading_selection,
    'Given that the same convertibility text (''Article IV'' / the $35/oz gold-window commitment) supports at least three structurally distinct readings — strict legal obligation, conditional policy discretion, and inherently unsustainable design flaw — which reading best explains the actual pattern of U.S. and creditor-nation behavior from 1958 to 1971?',
    'This omega documents the committer structure rather than resolving it: the strict_convertibility_reading (this story) treats the obligation as enforceable law; the policy_flexible_reading treats it as subordinate to domestic stability, denying the U.S. victim role and substantially lowering ε; the triffin_structural_reading relocates the extraction into the system''s design itself rather than into a bilateral creditor/debtor claim, dissolving the beneficiary/victim pairing this story authors. Each is a separate constraint story, linked via network.affects_constraints, per DP-001 ε-invariance.',
    'The choice of reading determines whether the U.S. appears as a victim at all, whether ε is high (this reading, ~0.71) or low (policy_flexible_reading), and whether extraction is bilateral (this reading, triffin_structural_reading) or absent as a category (policy_flexible_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_selection, conceptual, 'Documents which of three sibling readings of the convertibility kernel this story instantiates, and what the alternative readings would change.').

omega_variable(
    gold_pool_participation_voluntariness,
    'Was European central bank participation in gold-pool interventions and dollar-holding patterns itself a form of coordinated support for the system (making creditor nations partial co-maintainers rather than pure beneficiaries), or was it purely self-interested exploitation of an enforceable legal claim?',
    'Examine whether European central banks (especially the Bundesbank) made voluntary commitments to limit gold conversion in exchange for other concessions, which would indicate a more symmetric coordination relationship than this reading''s beneficiary/victim framing suggests.',
    'If creditor nations were substantially self-restraining co-maintainers of the system (as some, like West Germany, arguably were through informal non-conversion pledges), the tangled_rope classification''s asymmetric extraction gate is weaker than authored here, and the coordination component may be larger relative to extraction than this story''s ε implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_pool_participation_voluntariness, empirical, 'Whether creditor central banks were pure beneficiaries or partial co-maintainers exercising voluntary restraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1961, 0.13).
narrative_ontology:measurement(doll_tr_t1964, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1964, 0.17).
narrative_ontology:measurement(doll_tr_t1967, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1967, 0.21).
narrative_ontology:measurement(doll_tr_t1969, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1969, 0.25).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.28).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.42).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1961, 0.5).
narrative_ontology:measurement(doll_be_t1964, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1964, 0.58).
narrative_ontology:measurement(doll_be_t1967, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1967, 0.64).
narrative_ontology:measurement(doll_be_t1969, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1969, 0.68).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.4).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1961, 0.46).
narrative_ontology:measurement(doll_su_t1964, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1964, 0.51).
narrative_ontology:measurement(doll_su_t1967, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1967, 0.56).
narrative_ontology:measurement(doll_su_t1969, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1969, 0.6).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the dollar_gold_convertibility kernel. strict_convertibility_reading (this file) authors high ε (~0.71) and a bilateral beneficiary/victim structure with the U.S. as constrained payer. policy_flexible_reading authors much lower ε and denies the U.S. victim role, treating convertibility as always-conditional. triffin_structural_reading relocates extraction into systemic design instability rather than a bilateral claim, producing a different beneficiary/victim topology entirely (or none). Each story's ε is stable and non-comparable across readings per DP-001; the three are linked here for contamination-propagation and family-tracking purposes only, not to average or reconcile their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
