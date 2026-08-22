% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Fixed-Parity Regime as Constraint on International Capital Protecting Domestic Policy Space
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   Between 1944 and 1973 the Bretton Woods Articles of Agreement bound
 *   international monetary relations to a grid of fixed, adjustable parities
 *   supervised by the IMF, while expressly permitting, and in practice
 *   encouraging, member states to wall their domestic capital markets off
 *   from short-term foreign flows. This story instantiates the
 *   keynesian_embedded_liberalism reading of that treaty substrate: the
 *   binding edge falls on international capital, and the protected good is
 *   domestic policy space, meaning the ability of elected governments to run
 *   full-employment and welfare-state policy without answering to mobile
 *   savings. The claim/metric gap is deliberate and load-bearing: the
 *   constraint is CLAIMED as tangled_rope because this reading holds that a
 *   genuine coordination achievement (parity stability, pooled reserves, the
 *   taming of interwar-style currency wars) and a deliberate act of
 *   subordination (finance stripped of the disciplinary power it wielded
 *   under the classical gold standard) are the same structure, not competing
 *   descriptions. The metrics are authored independently as descriptive
 *   judgments about how the arrangement actually operated across its life.
 *   Per the epsilon-referent rule, extractiveness is assessed on the standing
 *   arrangement under contest, the fixed-parity capital-controlled regime as
 *   this reading sees it, never on the free-convertibility order the sibling
 *   reading would install. KEY AGENTS (by structural relationship): -
 *   national_governments: agenda-setting beneficiary
 *   (institutional/constrained) — authors and administers the controls,
 *   collects the policy-space dividend - us_treasury: dual-positioned anchor
 *   (institutional/arbitrage) — supplies the reserve currency, collects
 *   seigniorage, bears gold-defense costs - imf_secretariat: administrative
 *   enforcer (institutional/trapped) — runs surveillance and conditionality
 *   inside a treaty it cannot survive without - private_international_banks:
 *   primary target (powerful/constrained) — barred from onshore arbitrage,
 *   adapts offshore - currency_speculators: primary target
 *   (moderate/constrained) — the explicit object of Article VI permission -
 *   portfolio_investors: target (organized/constrained) — cross-border
 *   returns capped by controlled accounts - organized_domestic_labor:
 *   shielded beneficiary (organized/trapped) — holds the full-employment
 *   bargain behind the capital wall - exporting_manufacturers: beneficiary
 *   (organized/constrained) — buys parity predictability -
 *   developing_economies: excluded (powerless/trapped) — absent from the 1944
 *   table, inherits its weights - monetary_historians: analytical observer
 *   (analytical/analytical) — sees the full structure from the archive
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.58).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.55).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Fixed-Parity Regime as Constraint on International Capital Protecting Domestic Policy Space").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '1f637f53-19dd-462c-b985-32f56015b3a8').
narrative_ontology:cs_kernel_codification('1f637f53-19dd-462c-b985-32f56015b3a8', fixed_text).
narrative_ontology:cs_authority_grounding('1f637f53-19dd-462c-b985-32f56015b3a8', lineage).
narrative_ontology:cs_interpretation_layer_present('1f637f53-19dd-462c-b985-32f56015b3a8').
narrative_ontology:cs_reading_relation('1f637f53-19dd-462c-b985-32f56015b3a8', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_reading_relation('1f637f53-19dd-462c-b985-32f56015b3a8', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('1f637f53-19dd-462c-b985-32f56015b3a8', foundational, domestic_policy_autonomy_precedes_capital_mobility).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_precedes_capital_mobility, holdable).
narrative_ontology:cs_axiom_grounding('1f637f53-19dd-462c-b985-32f56015b3a8', domestic_policy_autonomy_precedes_capital_mobility, instrumental).
narrative_ontology:cs_axiom('1f637f53-19dd-462c-b985-32f56015b3a8', foundational, democracies_may_refuse_speculative_discipline).
narrative_ontology:cs_axiom_status(democracies_may_refuse_speculative_discipline, holdable).
narrative_ontology:cs_axiom_grounding('1f637f53-19dd-462c-b985-32f56015b3a8', democracies_may_refuse_speculative_discipline, deontological).
narrative_ontology:cs_reference_frame('1f637f53-19dd-462c-b985-32f56015b3a8', embedded_liberal_settlement).
narrative_ontology:cs_drift_state('1f637f53-19dd-462c-b985-32f56015b3a8', nixon_shock_and_generalized_float, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1f637f53-19dd-462c-b985-32f56015b3a8', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, organized_domestic_labor).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, exporting_manufacturers).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, private_international_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, currency_speculators).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, portfolio_investors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, embedded_liberal_compromise_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, keynesian_clearing_union_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wrote the Articles of Agreement and retain the treaty power to amend them. Administer exchange controls on cross-border capital movements, defend declared parities through central-bank intervention, and use the protected space to run countercyclical fiscal policy and full-employment commitments. What flows to them is discretion: they can set domestic interest rates without fearing that savings will leave overnight. Leaving the arrangement outright is possible in principle, but it means surrendering the parity grid and the Fund's credit line, so most stay and negotiate instead.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary).

% Anchors the system by promising to convert foreign-held dollars into gold at thirty-five dollars an ounce. Collects what other governments pay for the convenience of holding reserve dollars and can settle its own external deficits in its own currency. The same anchor obliges it to defend the gold price whenever foreign holders lose confidence, which grows more expensive as outstanding dollar liabilities accumulate. In August 1971 it resolves the bind unilaterally by closing the gold window.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, us_treasury, payer).

% Runs the day-to-day machinery: reviews members' exchange practices, rules on parity changes when fundamental disequilibrium is claimed, and lends reserve currencies to members running deficits, attaching policy conditions to the drawings. Its leverage exists only inside the treaty; without the Articles and the quotas there is no Fund.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_secretariat, agenda_setter,
    institutional, generational, trapped, global).

% Cross-border dealing is their business, and the arrangement restricts most of it: short-term foreign-exchange positions are curtailed, many currency markets are segmented by national controls, and new cross-border lending often requires authorization. They respond by building an offshore market in dollars held outside American jurisdiction, the Eurodollar market in London, which grows through the 1960s into a parallel system the controls cannot reach, and they press continuously for restoration of full convertibility.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, private_international_banks, payer,
    powerful, biographical, constrained, global).

% Live off anticipated parity changes, and the Articles expressly authorize members to block the capital movements they depend on. When a parity looks vulnerable, as sterling did in 1967, they can attack; between crises their access is thin and enforcement against them is the sharpest anywhere in the system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, currency_speculators, payer,
    moderate, immediate, constrained, global).

% Hold bonds and equities across borders and face approval requirements, quantitative ceilings, and measures such as the American Interest Equalization Tax of 1963. Their returns are capped by the controlled accounts; restructuring residence or vehicles to escape costs money and legal exposure.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, portfolio_investors, payer,
    organized, biographical, constrained, global).

% Strikes wage bargains inside economies where employers cannot credibly threaten to move capital abroad overnight. The full-employment commitment that keeps demand high depends on that insulation. They cannot leave the economy they bargain in, and they carry the inflation risk when governments spend the protected space too freely.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, organized_domestic_labor, beneficiary,
    organized, biographical, trapped, national).

% Sell into foreign markets under parities that change rarely and only by negotiated step. Predictable exchange rates let them quote contracts years out without hedging costs consuming the margin. They back the arrangement as long as adjustments come slowly and in their favor often enough.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, exporting_manufacturers, beneficiary,
    organized, biographical, constrained, global).

% Most were colonies or newly dependent states in 1944 and had little voice in the design. They inherit quota weights and voting shares fixed by Atlantic priorities, borrow later under conditionality written for European reconstruction, and face commodity-price swings the parity grid does little to cushion.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, developing_economies, excluded,
    powerless, generational, trapped, national).

% Reconstruct the drafting record, Keynes's clearing-union plan, White's scheme, and the compromise Articles, and trace how the offshore eurocurrency market hollowed out enforcement. They assess whether the postwar boom was caused by the design or merely coincided with it, working from archives none of the participants controlled.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains stable, adjustable exchange parities and pools reserve liquidity through the IMF so that trade can expand without each nation hoarding war-scale reserves, while giving members a sanctioned mechanism, Article VI, to sever short-term capital flows from domestic monetary policy.
% TRANSFER_FUNCTION: Moves monetary-policy autonomy from international capital markets to national governments; moves adjustment costs toward deficit-country austerity programs administered by the Fund and, in Keynes's original design, toward surplus-country restraint; moves seigniorage and reserve-currency privilege to the United States.
% ABSENT_VOICES: International financiers stood at the margins of the design and their preferred outcome, unconditional convertibility, was consciously excluded; colonial and developing economies were barely represented in 1944 and inherited quota weights fixed by Atlantic power; domestic savers and future taxpayers who would bear the inflation consequences of unconstrained policy space had no seat at the table.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, parities would go undefended, competitive devaluation would return as a tool of trade policy, and governments facing sudden capital flight would abandon full-employment commitments to defend their currencies, forcing the welfare-state settlement to renegotiate under market discipline. This is approximately the post-1973 trajectory, compressed into weeks instead of decades.
% FOUNDING_PROBLEM: The interwar monetary catastrophe: competitive devaluations, discriminatory currency blocs, the classical gold standard's deflationary discipline driving mass unemployment, and speculative capital flows destabilizing every government that attempted recovery.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic-historical scholarship reconstructing the gold standard's unemployment effects, the contemporaneous complaints of financial practitioners who chafed under the controls (their grievances confirm the constraint actually bound), and the post-1971 behavior of governments themselves, which kept invoking the interwar trauma whenever defending temporary controls. No attestation of the founding problem comes solely from the governments that collect the policy-space dividend.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.58: substantial, targeted, and by design. This reading itself places international finance in the victim set, so a low epsilon would contradict the reading's own structural claim; but the extraction is bounded, since current-account transactions stayed free, long-term investment was generally permitted, and the costs concentrate on a specific class rather than falling diffusely. Suppression is 0.55 as a raw structural property: legal bars on convertibility and active enforcement against evasion closed finance's preferred alternative, while current-account channels and most trade remained open, so alternatives narrowed without disappearing. Theater ends at 0.35: the golden-age functioning was real, but the terminal phase produced defensive performance, gold-pool communiques, the Smithsonian realignment celebrated as the greatest monetary agreement in history and abandoned within fourteen months. Accessibility_collapse is 0.45 because the alternative order, floating rates and free mobility, remained fully visible, theorized publicly from Friedman's 1953 case onward, and was eventually chosen; the old order never made its alternative unthinkable. Resistance is 0.6: continuous City and Wall Street lobbying for convertibility, the Eurodollar build-out as practical exit, and an academic campaign against the parity grid. The temporal series run on one shared eight-point grid, every tracked metric authored at every point. The suppression_requirement series is authored deliberately rather than left to the static scalar: this story specifically tracks enforcement-capacity change, a build-up from the Interest Equalization Tax (1963) through voluntary and then mandatory capital programs (1965-1968) to the gold-window closure (1971), followed by enforcement collapse as floats were accepted (1973). The trajectory is a ratchet then a cliff, not a cycle.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute different types from identical treaty text. From the negotiating governments' position the Articles read as the price of never repeating 1931; from the dealing rooms the same clauses read as confiscation of their trade. The Fund experiences the structure as stewardship; developing economies experience the same constitution as a document written in their absence that later governs their borrowing. Among nominally equal member governments, exit diverged sharply despite identical formal standing: the reserve-currency issuer could settle its deficits in its own currency, strong-currency countries absorbed imported inflation instead of discipline, and weak-currency countries faced conditionality, so the same constraint arrived at each seat with different force. The US Treasury is the hardest seat: it collects seigniorage as the system's anchor and pays the gold-defense bill as its guarantor, so its computed position should sit nearer the middle than any other governmental actor, and its eventual unilateral exit demonstrates that its exit option was categorically different from every other member's.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: national_governments sit nearest the subsidy end (they wrote the rules and collect the discretion), with organized_domestic_labor and exporting_manufacturers close behind as shielded riders who collect without administering. Victims derive high directionality with meaningful gradations: currency_speculators sit nearest the full-target end, being the named object of Article VI with the thinnest access; portfolio_investors follow; private_international_banks sit somewhat below full-target because constrained is not trapped, and the Eurodollar build-out returned part of what the controls took, making them targets who partially escaped into a profitable shadow system. The US Treasury lands mid-low through its dual agenda-setter/payer position. Suppression remains a raw structural property in the engine's arithmetic, unscaled by power or scope; only extractiveness is scaled by directionality and spatial scope, and the commentary respects that division. No directionality overrides are authored: the derivation chain from declared roles plus exit atoms reproduces the seat ordering above, including the banks' partial-arbitrage nuance, which is carried by their exit_options value rather than by an override entry.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification blocks two symmetrical mislabels. Celebratory historiography would call the arrangement a pure coordination device, but the subordination of finance was not overhead, it was the stated objective: Keynes designed the clearing union precisely to strip speculative capital of its veto over domestic policy, and a pure-coordination label would erase the identifiable payers. The libertarian counter-reading would call it pure extraction, but that erases the real coordination good, since parity stability underwrote the trade expansion and the Fund recycled surplus liquidity to deficit countries, and it misstates the alternatives, which were priced rather than suppressed for governments. On mandatrophy: the founding trauma receded by the 1960s while the enforcement machinery intensified, the classic signature of a mandate outliving its motivating function, but the arrangement resolved by collapse in 1971-73 rather than degenerating into theatrical self-maintenance, so no inertial phase is claimed. The theater_ratio rise at interval end records the defensive performance that preceded death, not a zombie afterlife; the mismatch consumer should read founding_problem_status=contested together with verdict=world_rearranges as a live-function arrangement that died fighting, not a captured shell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the binding structure of the Bretton Woods Articles correctly located by this reading, a constraint on capital protecting domestic policy space, rather than by the sibling readings, a constraint on governments enabling capital markets (neoliberal_convertibility) or a constraint on external discipline preserving monetary sovereignty (sovereignty_defense)?',
    'Drafting-history and Articles-text analysis (Keynes''s clearing union versus White''s plan versus the compromise text) combined with post-1971 practice comparison: which party''s behavior changed when the reading was abandoned identifies what the binding actually held.',
    'Adopting neoliberal_convertibility flips the victim and beneficiary sets, governments become targets and finance becomes beneficiary, and the same substrate recomputes as a different type; adopting sovereignty_defense narrows the protected good to monetary sovereignty and shifts beneficiaries toward treasuries and central banks qua sovereigns rather than domestic policy coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame ambiguity: which binding the treaty substrate primarily imposes, and therefore whose costs epsilon measures.').

omega_variable(
    controls_causality_vs_redundancy,
    'Did capital controls cause the golden-age policy autonomy, or were they redundant because profitable cross-border opportunities were scarce in a devastated world economy?',
    'Within-era comparison of countries with differing control strictness and reconstruction of hypothetical arbitrage spreads against realized onshore-offshore differentials once the Eurodollar market provides a live benchmark.',
    'If the controls were redundant, the coordination credit shrinks and the arrangement reads as a governments'' cartel against domestic savers, raising effective extraction and pulling the classification toward the extractive pole; if causal, the coordination component strengthens and the arrangement earns its hybrid character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(controls_causality_vs_redundancy, empirical, 'Whether the constraint''s coordination function was load-bearing or decorative during the golden age.').

omega_variable(
    collapse_driver_decomposition,
    'Was the 1971-73 breakdown driven by the Triffin dilemma''s internal contradiction, reserve provision requiring deficits that undermine confidence in the anchor, or by Eurodollar exit arbitrage eroding enforcement capacity faster than it could be rebuilt?',
    'Econometric and archival decomposition of the 1968-71 gold and dollar pressures, separating reserve-demand growth from offshore-market leakage.',
    'Internal contradiction implies the fixed-parity form was inherently transitional and misread as a steady state, a scaffold-like lesson; exit-arbitrage erosion implies a viable design defeated by enforcement failure, a different lesson for any restored variant of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_driver_decomposition, empirical, 'Which failure mode killed the arrangement, and therefore what a successor design must fix.').

omega_variable(
    finance_victim_homogeneity,
    'Is international finance a homogeneous victim class, given that private banks rebuilt profitability through the offshore Eurodollar market while speculators and portfolio investors bore the binding constraint onshore?',
    'Profit-and-flow reconstruction of onshore versus offshore financial activity from 1958 to 1973, testing whether the banking seats were net losers under the arrangement or net winners in exile.',
    'If banks were net beneficiaries-in-exile, the effective victim set narrows to speculators and portfolio investors, effective extraction concentrates on fewer seats, and per-seat classifications diverge more sharply than the aggregate metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_victim_homogeneity, empirical, 'Whether the declared victim class masks a seat that escaped into profit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.26).
narrative_ontology:measurement_basis(bret_tr_t1944, observed).
narrative_ontology:measurement(bret_tr_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1949, 0.22).
narrative_ontology:measurement_basis(bret_tr_t1949, observed).
narrative_ontology:measurement(bret_tr_t1954, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1954, 0.17).
narrative_ontology:measurement_basis(bret_tr_t1954, observed).
narrative_ontology:measurement(bret_tr_t1959, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1959, 0.14).
narrative_ontology:measurement_basis(bret_tr_t1959, observed).
narrative_ontology:measurement(bret_tr_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1964, 0.16).
narrative_ontology:measurement_basis(bret_tr_t1964, observed).
narrative_ontology:measurement(bret_tr_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1968, 0.22).
narrative_ontology:measurement_basis(bret_tr_t1968, observed).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.31).
narrative_ontology:measurement_basis(bret_tr_t1971, observed).
narrative_ontology:measurement(bret_tr_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1973, 0.35).
narrative_ontology:measurement_basis(bret_tr_t1973, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.5).
narrative_ontology:measurement_basis(bret_be_t1944, observed).
narrative_ontology:measurement(bret_be_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1949, 0.46).
narrative_ontology:measurement_basis(bret_be_t1949, observed).
narrative_ontology:measurement(bret_be_t1954, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1954, 0.41).
narrative_ontology:measurement_basis(bret_be_t1954, observed).
narrative_ontology:measurement(bret_be_t1959, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1959, 0.38).
narrative_ontology:measurement_basis(bret_be_t1959, observed).
narrative_ontology:measurement(bret_be_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1964, 0.44).
narrative_ontology:measurement_basis(bret_be_t1964, observed).
narrative_ontology:measurement(bret_be_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1968, 0.51).
narrative_ontology:measurement_basis(bret_be_t1968, observed).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.55).
narrative_ontology:measurement_basis(bret_be_t1971, observed).
narrative_ontology:measurement(bret_be_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1973, 0.58).
narrative_ontology:measurement_basis(bret_be_t1973, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement_basis(bret_su_t1944, observed).
narrative_ontology:measurement(bret_su_t1949, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1949, 0.53).
narrative_ontology:measurement_basis(bret_su_t1949, observed).
narrative_ontology:measurement(bret_su_t1954, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1954, 0.46).
narrative_ontology:measurement_basis(bret_su_t1954, observed).
narrative_ontology:measurement(bret_su_t1959, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1959, 0.4).
narrative_ontology:measurement_basis(bret_su_t1959, observed).
narrative_ontology:measurement(bret_su_t1964, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1964, 0.49).
narrative_ontology:measurement_basis(bret_su_t1964, observed).
narrative_ontology:measurement(bret_su_t1968, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1968, 0.61).
narrative_ontology:measurement_basis(bret_su_t1968, observed).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.72).
narrative_ontology:measurement_basis(bret_su_t1971, observed).
narrative_ontology:measurement(bret_su_t1973, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement_basis(bret_su_t1973, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Bretton Woods system' conflates three structurally distinct constraints read off one treaty substrate. Per the epsilon-invariance principle this corpus decomposes them: this story authors epsilon for the arrangement as a constraint on capital protecting policy space; bretton_woods_treaty_substrate__neoliberal_convertibility authors epsilon for the same substrate as a constraint on governments enabling markets; bretton_woods_treaty_substrate__sovereignty_defense authors epsilon for it as a constraint on external discipline preserving monetary sovereignty. The readings differ in victim set and therefore in epsilon; they are linked here as a constraint family. Upstream/downstream: this reading is the founding-era reading whose visible breakdown in 1971-73 shifted legitimacy conditions toward the convertibility reading, a dependency recorded in reading_relations as influences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
