% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__strict_convertibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Bretton Woods Article IV Convertibility — Strict Binding-Obligation Reading
 *   domain: economic/international_law
 *
 * SUMMARY:
 *   The IMF Articles of Agreement (signed 1944, obligations operative from
 *   the late 1940s, fully biting after restoration of European
 *   current-account convertibility in 1958) committed the United States to
 *   convert foreign official dollar holdings into gold at $35 per ounce. This
 *   file instantiates the STRICT READING of that commitment: an
 *   unconditional, binding legal obligation that subordinates United States
 *   monetary and fiscal policy to the redemption right. On this reading the
 *   issuer enters the victim set — its domestic policy space is what the
 *   arrangement extracts — while surplus creditor nations hold enforceable
 *   claims and dollar-accumulating export economies ride an undervalued peg
 *   underwritten by the issuer's guarantee. The arrangement retains a genuine
 *   coordination function (stable parities, credible reserve asset,
 *   anti-devaluation discipline), so the claim and the metrics are authored
 *   independently: claimed_type is tangled_rope because both coordination and
 *   asymmetric extraction are structurally real and actively enforced; the
 *   metric series describes intensifying extraction culminating in the August
 *   1971 suspension. Per the epsilon-referent rule, extractiveness assesses
 *   the STANDING arrangement (convertibility as binding) by this reading's
 *   own lights — not any preferred alternative regime.
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: constrained issuer (institutional/identity_locked) — bears the extraction while administering the machinery that imposes it
 *   - surplus_creditor_nations: primary beneficiaries (organized/arbitrage) — hold enforceable gold claims exercised selectively for bullion and leverage
 *   - dollar_accumulating_export_economies: secondary beneficiaries with payer residue (powerful/constrained) — undervalued-peg export gains offset by revaluation and restraint burdens
 *   - us_full_employment_constituencies: domestic transmission victims (organized/trapped) — absorb the contractionary policy the external defense forces
 *   - london_gold_market_participants: excluded actors (organized/arbitrage) — barred from the official window, expose the official price as fiction
 *   - imf_par_value_surveillance: administrative agenda-setter (institutional/constrained) — registers parities and presses compliance it cannot compel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.76).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.6).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Bretton Woods Article IV Convertibility — Strict Binding-Obligation Reading").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "economic/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, surplus_creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, dollar_accumulating_export_economies).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_full_employment_constituencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, dollar_accumulating_export_economies).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, monetary_treaty_binding_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__strict_convertibility_reading, classical_gold_discipline_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the Articles of Agreement at Bretton Woods and operates the New York gold window: redeems foreign official dollar balances at $35/ounce, manages Treasury bullion, and sets monetary and fiscal stance under the standing shadow of redemption demands. It administers the very machinery that binds it; abandoning convertibility would mean dismantling the keystone of the postwar order it built and equates with its own global leadership.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_authorities, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, us_monetary_authorities, agenda_setter).

% France, Switzerland, Belgium and the Netherlands run persistent external surpluses, accumulate official dollar claims carrying a legal redemption right at the fixed gold price, and choose when to present them — converting for bullion and negotiating leverage, or voluntarily restraining at ally request. Domestic political audiences reward visible gold repatriation, so the redemption option is exercised selectively for maximum effect.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, surplus_creditor_nations, beneficiary,
    organized, biographical, arbitrage, regional).

% West Germany and Japan grow behind undervalued parities, exporting into stable exchange rates and amassing dollar reserves guaranteed in gold. They bear periodic revaluation episodes (Deutsche Mark 1961 and 1969), diplomatic pressure to refrain from presenting dollar claims, and requests to cooperate with capital controls; floating or cashing out wholesale would upend the export-led growth model their development strategy depends on.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, dollar_accumulating_export_economies, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, dollar_accumulating_export_economies, payer).

% Workers, firms, and their congressional representatives absorb the domestic contraction undertaken to defend the external position — the 1969-70 credit squeeze, rising unemployment, deferred social spending. They cannot opt out of national monetary policy, and their objections that gold defense is outranking jobs register only as political pressure inside the issuer government.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_full_employment_constituencies, payer,
    organized, immediate, trapped, national).

% Private traders, speculators, and bullion houses are barred from the official $35 window reserved for monetary authorities. They price gold freely, and after the 1968 two-tier split their widening premium exposes the distance between official parity and market reality; their flows are precisely what the Gold Pool and subsequent controls exist to contain.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, london_gold_market_participants, excluded,
    organized, immediate, arbitrage, global).

% Administers the par-value system under Article IV: registers parity notifications, conducts consultations, attaches conditions to standby credits, and presses the largest shareholder toward compliance. Its leverage over the issuer is limited by dependence on United States quota support and by the absence of compulsory dispute mechanisms against a founding member.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, imf_par_value_surveillance, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interwar collective-action failure of competitive devaluation and reserve scarcity: fixed parities anchored by a gold-convertible dollar give traders stable exchange rates, give central banks a credible universally acceptable reserve asset, and give every member a common discipline against beggar-thy-neighbor depreciation.
% TRANSFER_FUNCTION: Moves policy autonomy and, on presentation, physical gold from the issuing country to foreign official holders: creditor states acquire enforceable-at-will claims on United States bullion at a fixed price, while the issuer transfers monetary discipline (foregone expansion, episodic contraction) and periodically bullion itself.
% ABSENT_VOICES: Private gold markets are structurally excluded from the official window and would argue the official price is a fiction sustained by pooling; United States full-employment constituencies were never seated at the design table or in Article IV consultations and would object that external parity is being ranked above domestic livelihoods. Both speak from outside the official club — markets through the free-price premium, domestic constituencies through congressional dissent.
% DISAPPEARANCE_RATIONALE: If the convertibility obligation vanished overnight, parities lose their anchor, the world's principal reserve asset loses its guarantee, trade finance and central-bank holdings reorganize around floating rates or rival blocs, creditor redemption claims extinguish, and the issuing government recovers domestic policy freedom at the cost of the system's credibility.
% FOUNDING_PROBLEM: The interwar monetary catastrophe: sequential competitive devaluations, discriminatory currency blocs, the collapse of the classical gold standard, and the deflationary spiral that fed depression and political extremism. The designers sought fixed-but-adjustable parities combining exchange stability, adequate liquidity, and discipline on national monetary discretion.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem was real and initially addressed is corroborated from outside the benefiting parties: State Department and Treasury archival record (FRUS volumes on Bretton Woods), parliamentary testimony in creditor states, Bundesbank and Bank of England records, and the scholarly consensus (Eichengreen, Bordo, the IMF's own histories). Whether the problem remained live in the strict binding form is disputed: United States officials from the mid-1960s onward attested the discipline had become asymmetric and obsolete, while European finance ministries and the IMF managing directorship attested it remained necessary against inflation — the status claim rests on contested testimony, with the strongest external corroboration attaching to the original-problem-reality half rather than the continued-necessity half.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored high (0.76, matching the terminal measurement) because the strict reading's core fact is the subordination of issuer policy autonomy: by the late 1960s United States interest rates, budget choices, and balance-of-payments programs were visibly bent around redemption-risk management. Suppression (0.60) is a raw structural property — deliberately NOT scaled by power or scope — reflecting a treaty obligation backed by surveillance machinery and alliance pressure, with formal exit that carried catastrophic reputational cost rather than legal impossibility. Theater (0.45) reflects the mature-era condition in which the official $35 price survived mainly through pooling, rhetoric, and controls while free-market gold traded far above parity; the scalar represents the 1960s enforcement plateau, whereas the terminal 1971 measurement (0.52) records the pre-suspension peak of official-price performance. Accessibility_collapse (0.62) is moderate: floating, devaluation, and default were all understandable alternatives, but each collapsed politically while the system endured because abandoning convertibility meant destroying the issuer's own institutional creation. Resistance (0.65) is substantial on both flanks — the issuer chafing (Operation Twist, the Interest Equalization Tax, restraint diplomacy, finally unilateral suspension) and creditors pressing harder (French gold diplomacy, Pool withdrawals). All three tracked series share one six-point grid (1945, 1950, 1958, 1963, 1968, 1971); suppression_requirement is tracked because enforcement capacity is the story's dynamic: it builds (Gold Pool, swap networks, capital controls peaking 1963-68) then decays abruptly (Pool collapse March 1968, two-tier market, allied refusal, August 1971 suspension) — a rise-and-fall arc, not monotonic ratcheting. The issuer's identity_locked exit encodes institutional identity fusion: the system was not merely a contract the United States signed but the organizational expression of its postwar hegemony, which is why fifteen years of visibly unsustainable defense preceded a decision every actor knew was available.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute radically different types from identical structural data. From the issuer's position the same articles read as enforced extraction — a standing lien on domestic policy exercised by allies at will. From the creditor arbitrage seat the identical text reads as a hard-won guarantee, an option worth holding precisely because someone else bears the reserve cost. Export-economy seats read it as subsidized stability with occasional invoices (revaluations, restraint requests). The IMF administrative seat experiences it as a maintenance duty it performs but cannot fully enforce against its largest shareholder. Excluded market participants experience the official price as a managed fiction. None of these perceptions is authored; each is computed from power, exit options, and declared position.
 *
 * DIRECTIONALITY LOGIC:
 *   Surplus creditor nations sit nearest the beneficiary pole (arbitrage-grade exit: they present claims when favorable, restrain when asked, and collect bullion and leverage either way). Dollar-accumulating export economies derive low-but-not-floor directionality — genuine subsidy from undervalued parities, damped by their payer residue. The United States monetary authorities derive high directionality from the payer declaration plus identity_locked exit: trapped-or-locked targets sit nearer the full-target end than mobile ones, and no override is entered because the structural data already places the seat correctly — the dual agenda_setter role does not dilute the target position, since the administration of the window was exercised in creditors' favor throughout. Full-employment constituencies inherit high directionality as the domestic transmission surface of the extraction. No directionality_overrides array is authored: the derivation chain from declared beneficiary/victim structure plus exit atoms reproduces the intended relationships without correction, and an override keyed only by power atom would misfire across seats sharing the institutional atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar competitive devaluation and reserve scarcity — was substantially solved by the late 1950s, yet the strict binding form persisted another decade past the point its own administrators privately judged sustainable, sustained by enforcement machinery whose intensity peaked (1963-68) exactly when its underlying justification was eroding, and ending in outright repudiation. Authoring founding_problem_status as contested rather than dead keeps the genealogy honest: creditor parties genuinely attested continued necessity (inflation discipline), so the mismatch flag, if it fires, is cross-checked against the theater and extraction trajectories rather than assumed. The tangled_rope classification is what prevents mislabeling in both directions: a pure-rope reading would erase the documented asymmetric extraction from the issuer's policy space; a pure-snare reading would erase the real coordination value every member including the issuer drew from stable parities and a credible reserve asset. The hybrid category holds both facts, and the per-seat computation lets the issuer's seat register snare-like experience without forcing the whole structure into the snare box.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'This constraint is one reading of the dollar_gold_convertibility kernel — how would the classification shift if instantiated under the sibling readings policy_flexible_reading or triffin_structural_reading?',
    'Author the sibling files and compare computed seats: the flexible sibling removes the unconditional-obligation premise (issuer exits the victim set, extractiveness drops toward coordination-cost floors); the Triffin sibling relocates the defect to the design itself (no concentrated capturer, persistence-by-flaw profile).',
    'Under the flexible sibling the United States seat loses full-target directionality and creditor enforceability weakens toward moral suasion; under the Triffin sibling gain_flow goes diffuse and the constraint migrates toward a degraded/transitional profile. The classification of this file is conditional on the strict premise holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Committer structure: one of three readings of a contested kernel; sibling deltas documented for cross-file comparison.').

omega_variable(
    legal_enforceability_of_article_iv,
    'Did Article IV create obligations that were legally enforceable against the United States, or did creditor rights in practice operate exclusively through diplomatic pressure and threat?',
    'Archival search for any formal invocation of IMF dispute or sanction mechanisms against the founding member, combined with treaty-practice scholarship on the absence of compulsory jurisdiction in the Articles; the finding either way is documentary, not speculative.',
    'If enforcement was purely diplomatic, the ''binding legal obligation'' core premise weakens: suppression reattributes from coercive law to reputational and alliance politics, and the reading slides toward the flexible sibling''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_enforceability_of_article_iv, empirical, 'Whether the strict reading''s bindingness was juridical fact or diplomatic convention wearing legal language.').

omega_variable(
    issuer_identity_lock_attribution,
    'Was the issuer''s prolonged non-exit driven by identity fusion (the system as the organizational expression of postwar hegemonic self-conception) or by ordinary cost-benefit calculation that happened to favor delay?',
    'Process-trace the 1965-1971 decision record (Treasury memoranda, Nixon-shock deliberations, Connancy-era reframings) and test the counterfactual: would a materially cheaper exit path have been taken earlier if one existed?',
    'The identity-lock attribution supports identity_locked exit and higher suppression attribution; a calculative attribution demotes the seat toward constrained exit and re-dates the suspension as rational option exercise rather than identity break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(issuer_identity_lock_attribution, empirical, 'Mechanism attribution behind the issuer''s exit behavior: fusion versus calculation.').

omega_variable(
    creditor_net_benefit_symmetry,
    'Did the beneficiary seats net-benefit from the arrangement, or did revaluation episodes, restraint diplomacy, and imported inflation offset the redemption-right advantage sufficiently to approach symmetry?',
    'Balance documented gold receipts and leverage gains against Deutsche Mark revaluations (1961, 1969), yen pressure, Bundesbank restraint-agreement costs, and the inflation the surplus economies imported under undervalued parities.',
    'Substantial offsets dampen beneficiary directionality toward the symmetric midpoint, weakening the asymmetric-extraction half of the tangled-rope structure and pulling the computed type toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_net_benefit_symmetry, empirical, 'Net-benefit audit of the creditor and export-economy seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strict_convertibility_tr_t1945, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement_basis(strict_convertibility_tr_t1945, observed).
narrative_ontology:measurement(strict_convertibility_tr_t1950, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(strict_convertibility_tr_t1950, observed).
narrative_ontology:measurement(strict_convertibility_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.25).
narrative_ontology:measurement_basis(strict_convertibility_tr_t1958, observed).
narrative_ontology:measurement(strict_convertibility_tr_t1963, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1963, 0.32).
narrative_ontology:measurement_basis(strict_convertibility_tr_t1963, observed).
narrative_ontology:measurement(strict_convertibility_tr_t1968, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1968, 0.45).
narrative_ontology:measurement_basis(strict_convertibility_tr_t1968, observed).
narrative_ontology:measurement(strict_convertibility_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.52).
narrative_ontology:measurement_basis(strict_convertibility_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(strict_convertibility_be_t1945, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement_basis(strict_convertibility_be_t1945, observed).
narrative_ontology:measurement(strict_convertibility_be_t1950, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1950, 0.47).
narrative_ontology:measurement_basis(strict_convertibility_be_t1950, observed).
narrative_ontology:measurement(strict_convertibility_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.6).
narrative_ontology:measurement_basis(strict_convertibility_be_t1958, observed).
narrative_ontology:measurement(strict_convertibility_be_t1963, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1963, 0.69).
narrative_ontology:measurement_basis(strict_convertibility_be_t1963, observed).
narrative_ontology:measurement(strict_convertibility_be_t1968, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1968, 0.73).
narrative_ontology:measurement_basis(strict_convertibility_be_t1968, observed).
narrative_ontology:measurement(strict_convertibility_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.76).
narrative_ontology:measurement_basis(strict_convertibility_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(strict_convertibility_su_t1945, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement_basis(strict_convertibility_su_t1945, observed).
narrative_ontology:measurement(strict_convertibility_su_t1950, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement_basis(strict_convertibility_su_t1950, observed).
narrative_ontology:measurement(strict_convertibility_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.42).
narrative_ontology:measurement_basis(strict_convertibility_su_t1958, observed).
narrative_ontology:measurement(strict_convertibility_su_t1963, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1963, 0.62).
narrative_ontology:measurement_basis(strict_convertibility_su_t1963, observed).
narrative_ontology:measurement(strict_convertibility_su_t1968, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1968, 0.66).
narrative_ontology:measurement_basis(strict_convertibility_su_t1968, observed).
narrative_ontology:measurement(strict_convertibility_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.38).
narrative_ontology:measurement_basis(strict_convertibility_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, global_infrastructure).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'Bretton Woods Article IV convertibility.' One label covered three structurally distinct constraints with different epsilon referents, different beneficiary/victim structures, and different classifications: the strict binding-obligation reading (this file — issuer in the victim set, creditors holding enforceable claims, high extraction from United States policy space), the policy-flexible reading (conditional obligation subordinate to domestic stability — issuer largely outside the victim set), and the Triffin structural reading (design flaw — extraction reframed as unsustainability, no concentrated capturer). The upstream reading (strict, highest enforcement content) feeds the downstream critiques: enforcement experience is the evidentiary base both siblings argue from. All three are linked via network edges; each file carries its own single, stable epsilon per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
