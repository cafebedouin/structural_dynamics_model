% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility as Structural Design Flaw (Triffin Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   This story instantiates the triffin_structural_reading of the
 *   dollar_gold_convertibility kernel: the Bretton Woods arrangement, under
 *   which the United States redeems dollars for gold at a fixed price while
 *   the dollar serves as the world's reserve asset, is assessed as an
 *   inherently unsustainable design flaw requiring systemic revision. The
 *   epsilon referent is the standing arrangement under contest, the
 *   dollar-gold convertibility order as it operated from the restoration of
 *   full convertibility in 1958 to the suspension of August 1971, assessed by
 *   this reading's own lights; the reading's endorsed alternative, the
 *   floating successor regime, is NOT the referent. Under this reading the
 *   arrangement transfers real resources for paper while imposing an
 *   unsupportable double bind on both principals: the issuer must supply
 *   reserves and thereby erode its own backing, and creditors must accumulate
 *   claims whose coverage thins yearly. The claim/metric relationship is
 *   deliberately unreconciled: the claimed type is what this reading asserts
 *   is structurally true, and the metrics describe the arrangement's actual
 *   operation as the historical record shows it. KEY AGENTS (by structural
 *   relationship): - us_monetary_authorities: Dual-positioned principal
 *   (institutional/constrained) — collects seigniorage while bearing the
 *   redemption burden; listed among victims because the reading's arithmetic
 *   nets the privilege against an unsupportable obligation -
 *   creditor_nations: Surplus-country targets (institutional/constrained) —
 *   accumulate claims of thinning backing - deficit_developing_economies:
 *   Adjustment-bearing targets (powerless/trapped) — absorb
 *   conditional-financing burdens - eurodollar_market_banks: Offshore
 *   beneficiaries (powerful/arbitrage) — profit from the reserve role outside
 *   the controls - post_bretton_woods_floating_regime: Successor arrangement,
 *   non-agent beneficiary — inherits the functions after collapse -
 *   international_monetary_fund: Administrative enforcer
 *   (institutional/constrained) - academic_monetary_critics: Analytical
 *   observers (analytical/analytical) — named the flaw in advance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.84).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.77).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility as Structural Design Flaw (Triffin Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'a5f593cf-6caa-4f97-bd75-11895d480ad1').
narrative_ontology:cs_kernel_codification('a5f593cf-6caa-4f97-bd75-11895d480ad1', formalized).
narrative_ontology:cs_authority_grounding('a5f593cf-6caa-4f97-bd75-11895d480ad1', lineage).
narrative_ontology:cs_interpretation_layer_present('a5f593cf-6caa-4f97-bd75-11895d480ad1').
narrative_ontology:cs_reading_relation('a5f593cf-6caa-4f97-bd75-11895d480ad1', dollar_gold_convertibility__strict_convertibility_reading, influences).
narrative_ontology:cs_reading_relation('a5f593cf-6caa-4f97-bd75-11895d480ad1', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_axiom('a5f593cf-6caa-4f97-bd75-11895d480ad1', foundational, reserve_growth_erodes_convertibility_arithmetically).
narrative_ontology:cs_axiom_status(reserve_growth_erodes_convertibility_arithmetically, holdable).
narrative_ontology:cs_axiom_grounding('a5f593cf-6caa-4f97-bd75-11895d480ad1', reserve_growth_erodes_convertibility_arithmetically, empirically_contingent).
narrative_ontology:cs_axiom('a5f593cf-6caa-4f97-bd75-11895d480ad1', foundational, systemic_revision_preferred_to_parity_defense).
narrative_ontology:cs_axiom_status(systemic_revision_preferred_to_parity_defense, holdable).
narrative_ontology:cs_axiom_grounding('a5f593cf-6caa-4f97-bd75-11895d480ad1', systemic_revision_preferred_to_parity_defense, instrumental).
narrative_ontology:cs_reference_frame('a5f593cf-6caa-4f97-bd75-11895d480ad1', designed_gold_exchange_architecture).
narrative_ontology:cs_drift_state('a5f593cf-6caa-4f97-bd75-11895d480ad1', august_1971_conversion_suspension, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('a5f593cf-6caa-4f97-bd75-11895d480ad1', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, eurodollar_market_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nations).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, deficit_developing_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__triffin_structural_reading, triffin_dilemma_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary reserve currency and promises to redeem dollars for gold at thirty-five dollars per ounce. Collects seigniorage: the rest of the world accepts dollar balances in payment for real goods and assets. At the same time, every dollar shipped abroad to meet world liquidity needs lowers the gold coverage behind the outstanding promise, so defending the promise requires capital controls, gold-pool operations, and pressure on allies, while honoring it drains the national gold stock. Tightening to defend the peg starves trading partners of reserves and invites recession; loosening to supply reserves undermines the peg. Suspending conversion remains legally available but would dismantle the monetary order the United States built and lead.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities, beneficiary).

% Run persistent surpluses against the United States and accumulate dollar reserves as the byproduct. Each year the gold backing behind their dollar claims thins further. Converting dollars to gold in volume accelerates the drain and risks breaking the system that anchors their export markets; holding dollars means accepting growing exposure to a promise that may not be kept. Unilateral revaluation is possible and was exercised, but each revaluation squeezes exporters and strains alliance relations.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nations, payer,
    institutional, generational, constrained, global).

% Draw on conditional financing when their currencies come under pressure. The financing arrives tied to conditions: devaluation, budget cuts, import compression. No comparable alternative lender exists and vote weight nowhere near matches the adjustment imposed; the burden of restoring external balance lands on their populations through unemployment and reduced consumption.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, deficit_developing_economies, payer,
    powerless, biographical, trapped, regional).

% Take dollar deposits and lend dollars outside national regulatory borders, centered in London. Official controls bind onshore activity, so cross-border borrowers and lenders migrate offshore, where spreads are wider and reserve requirements absent. Business grows with every restriction the authorities add and with every dollar the United States ships abroad; the franchise profits from the currency's reserve role whether or not the redemption promise survives.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, eurodollar_market_banks, beneficiary,
    powerful, biographical, arbitrage, global).

% Not an actor during the interval: the exchange-rate arrangement of generalized floating and national monetary autonomy adopted after August 1971. In this account it is the arrangement positioned to inherit the system's functions, liquidity provision without a redemption promise, once convertibility fails; it collects nothing while the parity system operates.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__triffin_structural_reading, post_bretton_woods_floating_regime).

% Administers the Articles of Agreement: approves par values, extends conditional financing to members in deficit, conducts surveillance over exchange arrangements. Its conditionality channels adjustment costs toward deficit members. It cannot alter the gold price, which remains the United States Treasury's prerogative, and its own resources depend on quotas pledged by the same governments whose policies it monitors.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Diagnose the arithmetic tension between world reserve growth and the finite gold stock, publish and testify, most prominently Robert Triffin before Congress in 1959-60, and propose remedies ranging from a new international reserve asset to a world central bank. They hold no enforcement power; their influence runs through elite opinion and the occasional official working party.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, academic_monetary_critics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchored postwar exchange rates to a common denominator and supplied the expanding world economy with reserve liquidity: a single internationally accepted money solved, centrally, the problem each country otherwise faced of settling trade in others' currencies under mutual distrust.
% TRANSFER_FUNCTION: Moves real goods, services, and title to assets from the rest of the world to the United States against newly created dollar balances; moves gold out of the United States stock to converting creditors as confidence erodes; moves deflationary adjustment onto deficit member economies through conditional financing.
% ABSENT_VOICES: Deficit developing economies sat in the Fund with votes too small to shape conditionality; gold-producing and commodity-exporting states had no seat in the parity architecture their earnings funded; domestic constituencies bearing austerity never appeared in surveillance; advocates of a supranational reserve asset were consulted late and overruled.
% DISAPPEARANCE_RATIONALE: When conversion stopped in August 1971 the parity grid dissolved within eighteen months: currencies floated, the gold price detached, reserve composition diversified, inflation accelerated through the 1970s, and subsequent arrangements had to be rebuilt around the wreckage, first the Smithsonian realignment, then generalized floating.
% FOUNDING_PROBLEM: The interwar sequence of competitive devaluation, discriminatory trade blocs, gold-standard collapse, and depression that the 1944 founders designed the parity system to make impossible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: economic historians of the interwar period, the contemporaneous testimony of non-American negotiators and central bankers, and the repeated post-1971 return of policymakers to interwar-chaos lessons when designing later coordination attempts. No party disputes that the founding problem was real; the dispute is over whether this arrangement could ever have solved it.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.84 at interval end) because the arrangement's operation moved real goods and assets against dollar liabilities while the redemption promise behind those liabilities weakened monotonically; both principals paid throughout, and the payment intensified as the gap between dollars outstanding and gold coverage widened. Suppression (0.77) reflects the enforcement machinery the arrangement required: the Interest Equalization Tax, voluntary foreign credit restraint programs, the London Gold Pool, and escalating capital controls — machinery that hardened as confidence fell, hence the rising suppression_requirement series. Theater rises steeply (0.14 to 0.61): after the Gold Pool's collapse and the March 1968 two-tier gold arrangement, a growing share of official activity maintained the appearance of the promise — communiques, reassurance operations, the pretense that the official price governed — while its substance drained. Accessibility_collapse is moderate (0.50): exits existed and were visible (unilateral revaluation, floating, the Special Drawing Right created in 1969), but each carried alliance-breaking or competitiveness costs that kept them from being freely workable. Resistance is substantial (0.66): French conversion campaigns and public attacks on the reserve-currency privilege, German revaluations, recurrent speculative attacks on sterling and the dollar, and congressional scrutiny of the gold drain. All three series run on one shared time grid (points 0, 3, 6, 9, 11, 13 of a 1958-1971 interval), every point observed from the historical record. The monotonic rise in base_extractiveness is the accumulation signature this corpus tracks; it terminates not in steady-state extraction but in the arrangement's own collapse, which is the Triffin reading's central claim made good.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical legal text. From the issuer's seat the arrangement reads as a burden it ultimately shed by unilateral decision, shot through with a privilege it was reluctant to surrender; from the creditor seats it reads as a depreciating promise they were paid to keep holding; from the deficit-economy seats it reads as an austerity channel whose conditions they could not vote down; from the offshore banking seats it is barely felt at all, a background rent that persists under either regime. The engine computes this divergence from the structural data — power, exit, and role — and the divergence between the issuer's experience and every other seat is the perspectival content of the Triffin critique itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The three payer seats derive high directionality from the victim declarations: deficit_developing_economies sit nearest the full-target end (victims with trapped exit and no coalition leverage inside weighted voting), creditor_nations slightly below them (victims with constrained exit — revaluation was available and used), and us_monetary_authorities high but damped by the secondary beneficiary declaration, encoding the reading's specific claim that the privilege sits inside the trap rather than beside it. eurodollar_market_banks derive near the beneficiary end (declared beneficiaries with arbitrage-grade exit outside the regulatory perimeter). post_bretton_woods_floating_regime is declared a beneficiary per the reading's structural map but carries agent=false, so it is excluded from directionality derivation — a successor arrangement cannot collect during the interval, and the engine should not treat it as a rent-collecting seat. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the intended profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is keeping the reading from collapsing into neighboring types. It is not a snare: no seat captures net — the largest receiver of gains (the issuer) is simultaneously the seat paying the trilemma, and the reading's whole point is that the arrangement harmed both principals. It is not a piton: nothing persisted by inertia; the arrangement ended by deliberate suspension while its function was still nominally demanded, the opposite of theatrical maintenance. It is not a scaffold: no sunset clause existed and none was contemplated; the founders claimed permanence. The R5 interview confirms the shape: the founding problem (interwar chaos) stayed live while the founding solution died — status live crossed with verdict world_rearranges yields no zombie mismatch, correctly, because this arrangement did not outlive its problem, it failed the problem. Mandatrophy resolution therefore routes through collapse, not atrophy: the measurement series show extraction accumulating past the point where any defense could hold, which is the abductive trigger for investigation rather than a reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the triffin_structural_reading of the dollar_gold_convertibility kernel; how would the sibling readings restructure the victim and beneficiary sets?',
    'Authoring the sibling stories: strict_convertibility_reading concentrates the burden on the United States as sole legal obligor with creditor nations as beneficiaries; policy_flexible_reading recasts the suspension episodes as lawful exercises of reserved discretion, lowering measured coercion across the board.',
    'Under the strict reading extraction concentrates on a single target and the creditor seats flip toward the beneficiary end; under the flexible reading the arrangement reads as discretionary policy rather than binding structure and effective extraction falls sharply. Cross-reading comparison is only valid because each reading holds a fixed referent and its own epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this is one of three readings of the convertibility kernel, with the structural deltas the siblings would introduce.').

omega_variable(
    inherent_vs_contingent_collapse,
    'Was the breakdown inherent in the reserve-supply-versus-redemption arithmetic, or contingent on particular policy choices — fiscal expansion, war spending, delayed adjustment?',
    'Counterfactual fiscal-monetary reconstruction of the 1960s: whether any sustainable United States policy path kept gold coverage above confidence thresholds while world trade and reserve demand grew at observed rates.',
    'If inherent, the arrangement is a design flaw and the tangled-rope reading stands with terminal collapse as the confirmation; if contingent, it is a workable coordination device mismanaged, and the classification drifts toward rope with enforcement failure rather than structural impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_contingent_collapse, empirical, 'Inherent Triffin arithmetic versus contingent policy failure as the cause of collapse.').

omega_variable(
    successor_regime_as_beneficiary,
    'Can a successor arrangement that held no seat during the interval legitimately occupy the beneficiary slot, or does beneficiary status require in-interval collection?',
    'A conceptual ruling on beneficiary-declaration semantics for non-agent successor entities, tested against the corpus convention that separates vindicated propositions (which collect no rents) from acting beneficiaries.',
    'If only in-interval actors qualify, beneficiaries reduce to the offshore banking seat and the extraction asymmetry sharpens; if successor arrangements qualify, the current declaration stands and the reading''s structural map is preserved intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_regime_as_beneficiary, conceptual, 'Whether the floating successor regime legitimately occupies the beneficiary declaration.').

omega_variable(
    seigniorage_netting_question,
    'Does United States seigniorage income net against its gold losses and defense costs across the interval, making the issuer a net gainer despite victim-listing?',
    'Full-period accounting of seigniorage flows, gold-stock depletion, and defense-machinery costs, audited against Treasury and Federal Reserve records and the parallel literature on reserve-currency privilege.',
    'If net-positive, the issuer seat''s directionality falls toward symmetry and the structure approaches center-periphery extraction with one identifiable capturer; if net-negative, the dual-victim framing holds and no seat captures, supporting the tangled-rope reading over a capture reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(seigniorage_netting_question, empirical, 'Whether the privileged seat nets positive across the interval, deciding between dual-victim and capturer framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t0, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(doll_tr_t0, observed).
narrative_ontology:measurement(doll_tr_t3, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement_basis(doll_tr_t3, observed).
narrative_ontology:measurement(doll_tr_t6, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 6, 0.29).
narrative_ontology:measurement_basis(doll_tr_t6, observed).
narrative_ontology:measurement(doll_tr_t9, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 9, 0.41).
narrative_ontology:measurement_basis(doll_tr_t9, observed).
narrative_ontology:measurement(doll_tr_t11, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 11, 0.53).
narrative_ontology:measurement_basis(doll_tr_t11, observed).
narrative_ontology:measurement(doll_tr_t13, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 13, 0.61).
narrative_ontology:measurement_basis(doll_tr_t13, observed).

% Extraction over time
narrative_ontology:measurement(doll_be_t0, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(doll_be_t0, observed).
narrative_ontology:measurement(doll_be_t3, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(doll_be_t3, observed).
narrative_ontology:measurement(doll_be_t6, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement_basis(doll_be_t6, observed).
narrative_ontology:measurement(doll_be_t9, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 9, 0.74).
narrative_ontology:measurement_basis(doll_be_t9, observed).
narrative_ontology:measurement(doll_be_t11, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 11, 0.8).
narrative_ontology:measurement_basis(doll_be_t11, observed).
narrative_ontology:measurement(doll_be_t13, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 13, 0.84).
narrative_ontology:measurement_basis(doll_be_t13, observed).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t0, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(doll_su_t0, observed).
narrative_ontology:measurement(doll_su_t3, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 3, 0.51).
narrative_ontology:measurement_basis(doll_su_t3, observed).
narrative_ontology:measurement(doll_su_t6, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 6, 0.59).
narrative_ontology:measurement_basis(doll_su_t6, observed).
narrative_ontology:measurement(doll_su_t9, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 9, 0.67).
narrative_ontology:measurement_basis(doll_su_t9, observed).
narrative_ontology:measurement(doll_su_t11, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 11, 0.72).
narrative_ontology:measurement_basis(doll_su_t11, observed).
narrative_ontology:measurement(doll_su_t13, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 13, 0.77).
narrative_ontology:measurement_basis(doll_su_t13, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, policy_flexible_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bretton Woods convertibility' covers three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story constraint family: strict_convertibility_reading (binding legal obligation), policy_flexible_reading (conditional obligation subordinate to domestic stability), and this file, triffin_structural_reading (inherently unsustainable design flaw). Each carries its own epsilon, victim set, and classification. The upstream strict reading supplies the legal text whose feasibility this reading disputes; the flexible reading describes the practice whose accumulation this reading predicted and whose collapse vindicated the triffin_dilemma_hypothesis listed in vindicated_propositions. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
