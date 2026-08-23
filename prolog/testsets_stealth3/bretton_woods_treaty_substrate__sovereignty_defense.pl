% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Par-Value Order — Sovereignty-Defense Reading (Asymmetric Adjustment Regime)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading — sovereignty_defense — of the
 *   contested kernel bretton_woods_treaty_substrate (the 1944 Articles of
 *   Agreement and the par-value practice built on them). Under this reading
 *   the arrangement blocks external monetary discipline (speculative attack,
 *   creditor automaticity, gold-standard deflation) in the name of national
 *   monetary sovereignty, while distributing the costs of that protection
 *   asymmetrically: the reserve issuer settles its deficits in its own paper
 *   and escapes the discipline entirely; non-reserve deficit members purchase
 *   stability with conditionality, devaluation, and austerity; and the gold
 *   anchor's defense grows progressively theatrical until the issuer defaults
 *   on it unilaterally in August 1971. Per Rule 1 the constraint is authored
 *   clean: one epsilon (referent = the standing 1944-1971 arrangement,
 *   assessed by this reading's own lights — never the endorsed alternative),
 *   one beneficiary/victim structure, no hedging across sibling readings; the
 *   embedded-liberalism and convertibility readings are separate files linked
 *   through network.affects_constraints. Claim and metrics are authored
 *   independently: claimed_type is tangled_rope because the arrangement
 *   genuinely coordinated (parity stability, pooled crisis liquidity,
 *   current-account multilateralism) while demonstrably extracting
 *   (seigniorage, adjustment asymmetry, one-way discipline); the metric
 *   series shows extraction and enforcement ratcheting upward across the
 *   interval — a coordination structure drifting toward its extraction
 *   component. The engine computes per-seat classifications from the
 *   structural data; divergence between the authored claim and any computed
 *   seat type is the datum, not an error.
 *
 * KEY AGENTS:
 *   - - united_states_monetary_authorities: Agenda-setting collector (institutional/arbitrage) — writes the rules, issues the reserve asset, settles deficits in its own currency, unilaterally suspends convertibility in 1971; immune to every discipline mechanism the system operates on others
 *   - - imf_conditionality_administrators: Co-administrator (institutional/constrained) — translates creditor-majority preferences into loan conditions and surveillance; institutional identity fused with the Fund's interpretive practice
 *   - - non_reserve_deficit_countries: Primary target (moderate/constrained) — buys stability with conditionality, devaluation, and demand deflation; cannot invoice trade in its own money
 *   - - developing_commodity_exporters: Deepest target (powerless/trapped) — hardest terms, negligible quota weight, populations absorb austerity
 *   - - domestic_labor_in_adjusting_countries: Diffuse target (powerless/trapped) — bears wage freezes and spending cuts without a seat in any negotiating forum
 *   - - surplus_industrializers: Beneficiary turning partial payer (powerful/constrained) — undervalued pegs subsidize export growth, then forced dollar accumulation imports the issuer's inflation
 *   - - britain_sterling_area_center: Dual-positioned target (moderate/constrained) — junior reserve center and chronic deficit economy, devalued under pressure in 1949 and 1967
 *   - - private_capital_markets: Excluded constituency (powerful/arbitrage) — barred by controls, builds offshore routes around them; their grievance is the seedbed of the convertibility sibling reading
 *   - - symmetric_adjustment_advocates: Excluded founders (moderate/constrained) — the bancor/creditor-levy tradition outvoted in 1944, never seated in the operating rules
 *   - - monetary_historians: Analytical observer (analytical/analytical) — Triffin's congressional testimony and the post-collapse literature are the main outside check on participant self-description
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.66).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.72).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.66).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Par-Value Order — Sovereignty-Defense Reading (Asymmetric Adjustment Regime)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, 'd345b759-7d54-4cdf-a099-f32171fecdb0').
narrative_ontology:cs_kernel_codification('d345b759-7d54-4cdf-a099-f32171fecdb0', formalized).
narrative_ontology:cs_authority_grounding('d345b759-7d54-4cdf-a099-f32171fecdb0', extraction).
narrative_ontology:cs_interpretation_layer_present('d345b759-7d54-4cdf-a099-f32171fecdb0').
narrative_ontology:cs_reading_relation('d345b759-7d54-4cdf-a099-f32171fecdb0', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('d345b759-7d54-4cdf-a099-f32171fecdb0', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('d345b759-7d54-4cdf-a099-f32171fecdb0', foundational, domestic_monetary_authority_outranks_external_discipline).
narrative_ontology:cs_axiom_status(domestic_monetary_authority_outranks_external_discipline, holdable).
narrative_ontology:cs_axiom_grounding('d345b759-7d54-4cdf-a099-f32171fecdb0', domestic_monetary_authority_outranks_external_discipline, deontological).
narrative_ontology:cs_axiom('d345b759-7d54-4cdf-a099-f32171fecdb0', foundational, reserve_issuer_immunity_is_structural_extraction).
narrative_ontology:cs_axiom_status(reserve_issuer_immunity_is_structural_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d345b759-7d54-4cdf-a099-f32171fecdb0', reserve_issuer_immunity_is_structural_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('d345b759-7d54-4cdf-a099-f32171fecdb0', national_policy_autonomy_compact).
narrative_ontology:cs_drift_state('d345b759-7d54-4cdf-a099-f32171fecdb0', nixon_shock_august_1971, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d345b759-7d54-4cdf-a099-f32171fecdb0', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authorities).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, surplus_industrializers).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_deficit_countries).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_commodity_exporters).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, domestic_labor_in_adjusting_countries).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, britain_sterling_area_center).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, britain_sterling_area_center).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, surplus_industrializers).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, white_plan_creditor_dominance).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, dollar_key_currency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wrote the final Articles at the 1944 conference (the White plan displaced the Keynes plan), holds effective veto over Fund decisions through quota weight, issues the currency every other member pegs to, and alone may settle its external deficits in its own paper. Decides when gold convertibility binds — and suspended it in August 1971 by unilateral announcement. Finances overseas military spending and domestic programs by issuing claims foreigners must hold, and supplies the liquidity the system runs on. Leaving an arrangement it administers would mean dethroning its own currency, so no member can impose external discipline on it.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authorities, beneficiary).

% Administers the Fund's lending, surveillance, and the policy conditions attached to drawings. Interprets the Articles in day-to-day operation and translates creditor-majority preferences into loan terms. Depends on the major shareholders for funding and direction; staff careers are built inside the institution's interpretive practice, which makes the practice itself the standard its personnel apply.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_administrators, agenda_setter,
    institutional, generational, constrained, global).

% Governments running persistent external deficits (Britain through the 1950s-60s, India, Latin American economies) must finance gaps through Fund drawings carrying policy conditions, devalue under speculative pressure, or deflate domestic demand. Their currencies are not held abroad as reserves; they cannot pay foreign obligations in their own money. Formal exit — leaving the par-value system — forfeits access to dollar trade credit and Fund support, so membership persists even as terms harden.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_deficit_countries, payer,
    moderate, biographical, constrained, national).

% Primary-product exporters joining at independence or through postwar decolonization face export earnings that swing with commodity prices while imports are priced in dollars. They borrow under the hardest conditions, hold negligible quota weight, and their populations absorb the resulting austerity. There is no alternative lender of comparable scale and no way to invoice trade in their own currencies.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_commodity_exporters, payer,
    powerless, biographical, trapped, national).

% Workers and wage-earners in countries undergoing Fund-conditioned stabilization bear the front-line costs: wage freezes, public spending cuts, unemployment from deflationary programs. They did not negotiate the Articles, hold no vote in the Fund, and cannot relocate across borders at scale; their recourse is domestic politics, which the timing of adjustment programs often outmaneuvers.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, domestic_labor_in_adjusting_countries, payer,
    powerless, biographical, trapped, national).

% West Germany and Japan grow behind undervalued fixed parities that subsidize their export industries, converting the anchor into an industrial subsidy. By the late 1960s the same peg forces them to accumulate dollar claims of doubtful convertibility and to import American inflation; revaluation is possible (Germany moved in 1961 and 1969) but politically costly against their own exporters, and outright exit would upend their principal market.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, surplus_industrializers, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, surplus_industrializers, payer).

% Head of a residual empire-financial bloc whose members hold sterling reserves. Britain draws prestige and cheap finance from the bloc while repeatedly devaluing under American pressure (1949, 1967) and drawing Fund credits with conditions attached. Its dual position — junior reserve center and chronic deficit country — leaves it defending the system's rules while being disciplined by them.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, britain_sterling_area_center, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, britain_sterling_area_center, beneficiary).

% London and New York financiers are barred by member-state capital controls from moving money freely against the fixed parities; Eurodollar markets grow offshore precisely to route around the controls. Their stated position — that convertibility and floating prices would allocate capital better — finds no seat in the Articles' administration, though their offshore work steadily erodes the controls from below.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, private_capital_markets, excluded,
    powerful, biographical, arbitrage, global).

% The delegation tradition descending from Keynes's Clearing Union proposal argues for symmetric adjustment: automatic creditor levies and an international money (bancor) that would place discipline on surplus and deficit alike. Outvoted at the 1944 conference and without an institutional home afterward, their proposals resurface periodically in Fund reform debates without ever entering the operating rules.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, symmetric_adjustment_advocates, excluded,
    moderate, generational, constrained, global).

% Scholars reconstructing the negotiation record, the Fund archives, and the declassification trail assess what the arrangement actually did versus what its designers said it would do. Their testimony — Triffin before Congress, the academic post-mortems after 1971 — is the main outside check on the participants' self-descriptions.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authorities).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the interwar payments failure: fixed-but-adjustable parities anchored on a single convertible currency, a pooled fund for temporary imbalances, and member capital controls that keep speculative flows from destroying the pegs — exchange-rate stability and crisis liquidity produced once, centrally, instead of by each state defending itself alone.
% TRANSFER_FUNCTION: Moves monetary autonomy and adjustment costs asymmetrically: seigniorage, deficit-financing capacity, and immunity from external discipline flow to the reserve issuer; conditionality, devaluation, and austerity flow to non-issuing deficit members; forced claim-accumulation and imported inflation flow to surplus members late in the interval.
% ABSENT_VOICES: Symmetric-adjustment advocates (the Keynes Clearing Union tradition: bancor, automatic creditor levies) were outvoted at the 1944 conference and never seated — the operating rules embedded creditor dominance from birth. Private capital markets were barred by the controls themselves; their objection lives in the neoliberal sibling file. Wage-earners who absorb stabilization programs had no delegate in any negotiating room, then or after.
% DISAPPEARANCE_RATIONALE: Overnight removal strands trade finance, revives competitive devaluation, leaves deficit members without crisis liquidity, and deletes the reserve asset the reconstruction era ran on; the trading order reorganizes around bilateral clearing, early floating, or rival currency blocs within months.
% FOUNDING_PROBLEM: The interwar monetary collapse: competitive devaluations, the gold bloc's rigid deflation, the evaporation of trade credit, and the 1930s downward spiral — plus the wartime design question of how to reconcile exchange stability with national policy autonomy so that neither speculators nor creditors could again dictate domestic policy.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the beneficiary set: Triffin's 1959-60 congressional testimony documents the liquidity contradiction from the academic seat; Jacques Rueff's 'deficit without tears' indictment and the Bank of France's gold campaign attest from a creditor-government seat that the arrangement had become deficit-financing for the issuer; the British Treasury's own 1960s papers record the conditionality burden from a target seat. No source inside the U.S. beneficiary seat attests the shifted-function reading — the issuer's own record describes the system as pure stability provision to the very end, which is itself signal.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores describe the standing arrangement across the full interval, anchored to its terminal state. Extractiveness 0.66: the coordination was real — parity stability, Fund liquidity, and the current-account opening underwrote the postwar recovery — but the burden distribution was structurally lopsided and worsened as U.S. deficits financed Vietnam and the Great Society while adjustment pressure stayed pointed outward. Suppression 0.72 exceeds extraction deliberately: persistence required continuous legal machinery (member capital controls, parity-defense operations, cross-conditionality on Fund drawings) and culminated in overt coercion (the 1971 import surcharge and wage-price freeze imposed alongside the convertibility suspension). Theater_ratio 0.40 reflects the late-interval condition in which the official $35 gold price survived only as a central-bank fiction after the 1968 two-tier split — performative maintenance of an anchor nobody could redeem at — while earlier activity was predominantly functional. Accessibility_collapse 0.45: alternatives never fully vanished (Canada floated 1950-62, the European Payments Union cleared bilaterally, Germany revalued twice, the SDR was minted as a partial substitute), but exit carried forfeiture of dollar trade credit and Fund support. Resistance 0.55: real and escalating — the French gold campaigns and Rueff's 'deficit without tears' indictment, the 1968 gold pool collapse, the sterling crises — yet never sufficient to force symmetric reform before the issuer simply defaulted. The three tracked metrics share one six-point grid (1944, 1950, 1958, 1965, 1968, 1971) so no metric borrows another's timeline; the trajectories are monotonic, not cyclical — this is an enforcement ratchet, not an oscillation, so no intermittent-reinforcement reading applies. Suppression here is structural throughout (legal controls, conditionality, coercive fiscal measures), not internalized; no suppression-mechanism ambiguity omega is required.
 *
 * PERSPECTIVAL GAP:
 *   Five seats should compute differently from identical global facts. From the U.S. seat the arrangement is a public good it built and bankrolled — stability, liquidity, an umbrella under which allies rebuilt; the 1971 suspension reads as regrettable necessity against speculators. From the Fund administrator seat it is technical management; institutional identity has fused with the interpretive practice, so questioning the conditionality frame is experienced as professional self-negation. From the deficit-country seat the same rules are one-way discipline: autonomy is preached, conditionality is practiced, and the issuer who preaches is exempt. From the surplus-industrializer seat the bargain sours late — the peg that subsidized exports becomes a channel for imported inflation and unredeemable claims. From the excluded capital-market seat the entire structure is a price control awaiting demolition — the grievance that seeds the neoliberal sibling reading. Same-level divergence is equally sharp: Britain, Germany, and Japan held nominally equal sovereignty, but reserve-issuance status, creditor position, and bloc membership gave them categorically different exits.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural declarations drive the derivation and no directionality overrides are needed. The U.S. sits nearest the beneficiary pole (declared beneficiary, arbitrage-grade exit — it issues the settlement asset, so no member can squeeze it) and receives the gains; the receipt surface names that seat. Surplus industrializers derive low d from their beneficiary declaration; their late-period forced dollar accumulation is a time-varying-role fact the static role schema cannot carry (OQ-83), so it is documented here rather than papered over with an override. Deficit-country targets derive high d, amplified by constrained and trapped exits; Britain's dual position (payer with secondary beneficiary role) lands it mid-high rather than extreme. Labor in adjusting countries derives the highest d of any seat — full target, trapped, powerless — because it absorbs the costs without collecting anything. Excluded seats (capital markets, adjustment advocates) sit outside the d computation by design: exclusion is commentary-grade, not correction-grade. Scope is global for the system-level actors and national for the member seats, so verification difficulty scales the effective burden on diffuse targets harder than on the issuer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar chaos: competitive devaluation, gold-bloc deflation, collapsed trade credit — was substantially solved by the late 1950s, yet the arrangement persisted and hardened for another decade-plus in a new function: financing the issuer's deficits and administering asymmetric adjustment. founding_problem_status is authored 'contested' rather than 'dead' because the stability function never wholly atrophied (members still drew on it in crises), which is exactly the ambiguity the mismatch consumer should see: contested status paired with a world_rearranges verdict flags the zombie-risk without asserting it. The classification discipline cuts both ways: reading the whole 27-year arc as pure extraction erases the genuine early coordination that explains why sovereign members consented for a generation; reading it as pure coordination erases the documented asymmetry (Triffin's dilemma testimony, Rueff's indictment, the conditionality record). Tangled_rope with a rising extraction series holds both facts — coordination founded it, extraction came to run it — and the terminal theater spike marks the moment the coordination cover thinned enough that the issuer abandoned the anchor rather than submit to the discipline it administered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the treaty substrate best read as sovereignty-protection, embedded-liberalism capital containment, or convertibility-enablement — and does the choice change who counts as victim?',
    'Clause-by-clause comparison of which Articles provisions actually bound which parties in practice (application records for Articles IV, VIII, XIV), cross-read against the three readings'' predicted victim sets.',
    'Switching readings reassigns the U.S. between beneficiary and target seats and moves non-reserve members in and out of the victim set; this file''s epsilon is valid only under the sovereignty_defense assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the Bretton Woods kernel the structural data supports.').

omega_variable(
    exorbitant_privilege_net_transfer,
    'How large was the net transfer to the reserve issuer from seigniorage and adjustment immunity, net of the stabilization services and security umbrella it supplied?',
    'Seigniorage accounting against counterfactual reserve-asset simulations (bancor-style), plus valuation of the liquidity and stability services delivered to members.',
    'A small net transfer would push the reading toward rope (services roughly paying for the privilege); a large one confirms the extraction component and strengthens the drift-toward-the-extraction-pole trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_net_transfer, empirical, 'Magnitude of the issuer''s net gain from reserve-currency status.').

omega_variable(
    adjustment_asymmetry_design_or_power,
    'Was the asymmetric adjustment burden inherent to the Articles'' design (White-plan creditor dominance) or an artifact of U.S. power exceeding formally symmetric rules?',
    'Compare the Articles'' formal adjustment provisions against practice: cases where surplus members adjusted (German revaluations of 1961 and 1969) versus cases where discipline ran only one way.',
    'Design-inherent asymmetry makes the extraction component structural; power-artifact asymmetry locates the fault in enforcement rather than the rules, softening the structural verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjustment_asymmetry_design_or_power, empirical, 'Whether adjustment asymmetry was baked into the rules or imposed by hegemony.').

omega_variable(
    triffin_contradiction_endogeneity,
    'Did the system collapse because global liquidity growth required issuer deficits that undermined convertibility (an internal contradiction no governance could dodge), or because of discretionary issuer policy (Vietnam financing, delayed adjustment)?',
    'Counterfactual modeling of liquidity supply under alternative anchor rules, tested against the decomposed 1960s U.S. deficit record.',
    'An internal contradiction gives the arrangement a hard structural limit beneath the politics (collapse was fated); discretionary causation keeps it fully constructed and reformable — and makes the 1971 default a choice rather than a destiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_contradiction_endogeneity, empirical, 'Whether the anchor''s failure was structural contradiction or policy choice.').

omega_variable(
    hegemonic_consent_vs_coercion,
    'How much of member compliance rested on hegemonic consent (security umbrella, reconstruction finance, shared Cold War aims) rather than on the enforcement machinery itself?',
    'Archival study of member-government deliberations: did states comply because the rules bound them or because exit threatened the broader alliance relationship?',
    'Consent-heavy compliance lowers the effective suppression attributable to the arrangement itself; coercion-heavy compliance confirms the enforcement ratchet the suppression series records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_consent_vs_coercion, conceptual, 'Consent versus coercion as the binding mechanism for member states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_sov_def_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.12).
narrative_ontology:measurement_basis(bw_sov_def_tr_t1944, observed).
narrative_ontology:measurement(bw_sov_def_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.14).
narrative_ontology:measurement_basis(bw_sov_def_tr_t1950, observed).
narrative_ontology:measurement(bw_sov_def_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.21).
narrative_ontology:measurement_basis(bw_sov_def_tr_t1958, observed).
narrative_ontology:measurement(bw_sov_def_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.29).
narrative_ontology:measurement_basis(bw_sov_def_tr_t1965, observed).
narrative_ontology:measurement(bw_sov_def_tr_t1968, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1968, 0.37).
narrative_ontology:measurement_basis(bw_sov_def_tr_t1968, observed).
narrative_ontology:measurement(bw_sov_def_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.4).
narrative_ontology:measurement_basis(bw_sov_def_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(bw_sov_def_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.42).
narrative_ontology:measurement_basis(bw_sov_def_be_t1944, observed).
narrative_ontology:measurement(bw_sov_def_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.46).
narrative_ontology:measurement_basis(bw_sov_def_be_t1950, observed).
narrative_ontology:measurement(bw_sov_def_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.53).
narrative_ontology:measurement_basis(bw_sov_def_be_t1958, observed).
narrative_ontology:measurement(bw_sov_def_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.59).
narrative_ontology:measurement_basis(bw_sov_def_be_t1965, observed).
narrative_ontology:measurement(bw_sov_def_be_t1968, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1968, 0.63).
narrative_ontology:measurement_basis(bw_sov_def_be_t1968, observed).
narrative_ontology:measurement(bw_sov_def_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.66).
narrative_ontology:measurement_basis(bw_sov_def_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(bw_sov_def_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.5).
narrative_ontology:measurement_basis(bw_sov_def_su_t1944, observed).
narrative_ontology:measurement(bw_sov_def_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement_basis(bw_sov_def_su_t1950, observed).
narrative_ontology:measurement(bw_sov_def_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.55).
narrative_ontology:measurement_basis(bw_sov_def_su_t1958, observed).
narrative_ontology:measurement(bw_sov_def_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.61).
narrative_ontology:measurement_basis(bw_sov_def_su_t1965, observed).
narrative_ontology:measurement(bw_sov_def_su_t1968, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1968, 0.67).
narrative_ontology:measurement_basis(bw_sov_def_su_t1968, observed).
narrative_ontology:measurement(bw_sov_def_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.72).
narrative_ontology:measurement_basis(bw_sov_def_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'Bretton Woods' covers three structurally distinct claims that share one treaty substrate but differ in epsilon, beneficiary/victim structure, and classification. keynesian_embedded_liberalism (upstream, the designers' self-description, highest empirical confidence about intent) reads the rules as protecting domestic policy space; this sovereignty_defense reading documents the asymmetric burden the same rules produced (U.S. enters the beneficiary set via exorbitant privilege; non-reserve states enter the victim set; the anchor's defense turns extractive); neoliberal_convertibility (downstream) reads the rules as distortions of capital freedom — a reading whose plausibility conditions were created by this reading's collapse evidence. Each story carries its own epsilon and stakeholder set; the family is linked through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
