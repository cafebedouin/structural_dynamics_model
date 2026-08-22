% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Article 127 Mandate — Orthodox Price-Stability Reading (Exclusive 2% Operational Focus)
 *   domain: monetary policy/constitutional law/EU institutional governance
 *
 * SUMMARY:
 *   The ECB's Article 127 TFEU mandate names price stability as the primary
 *   objective, to be pursued 'without prejudice' to support for general EU
 *   policies. The orthodox reading — instantiated by this story — holds that
 *   the 2% inflation target commands exclusive operational focus: secondary
 *   objectives (employment, growth, climate) are acknowledged rhetorically
 *   but carry no operational weight in rate-setting, asset purchases, or
 *   collateral policy. This is ONE READING of the contested
 *   ecb_mandate_article_127 kernel; the sibling readings
 *   (expansive_secondary_objectives, climate_incorporation) are separate
 *   constraints in separate files. The ε referent is the standing orthodox
 *   arrangement itself — the mandate operation as it actually runs — never
 *   the siblings' endorsed alternatives. Descriptively, the arrangement
 *   delivers a real coordination good (eurozone-wide expectations anchoring)
 *   while transferring real purchasing power toward a narrow creditor seat,
 *   externalizing climate risk through market-neutrality, and suppressing
 *   mandate expansion through legal and institutional enforcement. Claim and
 *   metrics are authored independently: claimed_type tangled_rope reflects
 *   genuine coordination plus asymmetric extraction plus active enforcement;
 *   the metrics describe observed operation without being tuned to any
 *   predicted engine verdict.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Agenda setter (institutional/identity_locked) — administers the narrow reading, defends it legally and rhetorically, collects institutional insulation from it
 *   - savers_and_creditors: Primary beneficiary (organized/mobile) — receives the disinflationary transfer; can reprice and hedge
 *   - indebted_households: Primary target (powerless/trapped) — bear elevated real debt burdens under the disinflationary bias
 *   - unemployed_workers: Secondary target (powerless/trapped) — bear the employment cost of the anti-inflation priority in downturns
 *   - high_debt_member_states: Institutional target (powerful/constrained) — fiscal space compressed; litigate and negotiate but do not set the reading
 *   - climate_policy_institutions: Excluded party (institutional/trapped) — would operationalize Article 11 TFEU integration; hold no seat in mandate interpretation
 *   - future_generations: Diffuse target (powerless/trapped) — bear unpriced climate risk deferred by market-neutral balance-sheet allocation
 *   - monetary_economists: Analytical observer — model counterfactuals under alternative readings; no operational power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.66).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.7).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.66).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Article 127 Mandate — Orthodox Price-Stability Reading (Exclusive 2% Operational Focus)").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary policy/constitutional law/EU institutional governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, 'ef8af841-fee3-45bd-ab5d-f1cf3d022fef').
narrative_ontology:cs_kernel_codification('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', fixed_text).
narrative_ontology:cs_authority_grounding('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', extraction).
narrative_ontology:cs_interpretation_layer_present('ef8af841-fee3-45bd-ab5d-f1cf3d022fef').
narrative_ontology:cs_reading_relation('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', ecb_mandate_article_127__expansive_secondary_objectives, coexists_with).
narrative_ontology:cs_reading_relation('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', ecb_mandate_article_127__climate_incorporation, forecloses).
narrative_ontology:cs_axiom('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', foundational, price_stability_operational_exclusivity).
narrative_ontology:cs_axiom_status(price_stability_operational_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', price_stability_operational_exclusivity, empirically_contingent).
narrative_ontology:cs_axiom('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', secondary, without_prejudice_subordination).
narrative_ontology:cs_axiom_status(without_prejudice_subordination, holdable).
narrative_ontology:cs_axiom_grounding('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', without_prejudice_subordination, conventional).
narrative_ontology:cs_reference_frame('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', maastricht_price_stability_primacy).
narrative_ontology:cs_drift_state('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', contemporary_expansion_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ef8af841-fee3-45bd-ab5d-f1cf3d022fef', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, indebted_households).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, future_generations).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, time_inconsistency_commitment_theory).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, central_bank_independence_improves_outcomes).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__orthodox_price_stability, inflation_expectations_anchoring_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the euro area's key interest rates and asset-purchase frameworks and administers the narrow reading of Article 127: price stability is the operative objective and the 'without prejudice' clause is invoked rhetorically without altering rate decisions or collateral rules. The council defends the narrow reading before courts and parliaments, and the reading protects the bank's independence from fiscal and political direction — the insulation is the institution's core asset and its staff's professional inheritance from the Bundesbank tradition. Exit from the reading would mean re-founding the bank's legitimacy, not relocating it.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, beneficiary).

% Hold deposits, bonds, and pension and insurance claims whose real value depends on the inflation level. The 2% anchor protects their nominal returns, and organized asset managers and insurers lobby publicly for mandate fidelity. They can reprice new lending, hedge with inflation-linked instruments, or shift across asset classes and jurisdictions, so their position is protected by portfolio mobility as well as by the mandate itself.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors, beneficiary,
    organized, generational, mobile, continental).

% Carry mortgages and consumer debt written in nominal euros. A mandate that never tolerates above-target inflation keeps their real debt burdens at their contractual maximum, and refinancing terms are set by the same rate policy that protects creditors. They have no exit from the currency in which their debts and wages are denominated, and no seat in the forum where the mandate's reading is set.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, indebted_households, payer,
    powerless, biographical, trapped, continental).

% Bear the employment cost when demand support would require tolerating inflation above target: the mandate's priority rules out that tradeoff, so downturns are met with tools that do not purchase employment with the price level. Individual workers have no exit from the euro-area labor market's wage-setting and little mobility across member states; their influence runs through national governments, which do not set the mandate reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_workers, payer,
    powerless, biographical, trapped, continental).

% Sovereign governments whose debt-service costs and fiscal space depend on the inflation path and on the conditions attached to central-bank backstops. They litigated the reading before their own constitutional courts and negotiate its application in crisis facilities, but they cannot amend the treaty without unanimity and cannot leave the euro without prohibitive cost. Their finance ministries would weight employment and fiscal sustainability alongside prices if they held the pen.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, high_debt_member_states, payer,
    powerful, generational, constrained, continental).

% European Commission climate directorates and EU legislators would integrate climate criteria into collateral frameworks and purchase programs under Article 11 TFEU's environmental integration clause. They hold no seat in mandate interpretation; their levers are litigation, legislative pressure, and public argument, all of which the bank's legal service answers as mandate expansion. From their point of view the exclusion is the arrangement working as designed.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_policy_institutions, excluded,
    institutional, generational, trapped, continental).

% Bear the climate and transition costs deferred by balance-sheet allocation that follows market weights: funding flows toward incumbent carbon-intensive issuers because neutrality forbids weighting by transition need. They are present only through advocates — no seat, no vote, and no repricing mechanism reaches them.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, future_generations, payer,
    powerless, civilizational, trapped, global).

% Model the arrangement's operation, estimate the distributional effects of the disinflationary bias, and publish comparisons with dual-mandate central banks. They hold analytic standing but no operational power; their work feeds the litigation and political pressure that the bank's enforcement machinery answers.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__orthodox_price_stability, savers_and_creditors).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__orthodox_price_stability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors eurozone-wide inflation expectations on a single credible numerical target, solving the time-inconsistency problem of discretionary monetary policy and eliminating the pre-euro pattern of competitive devaluation and expectations divergence across member states. One institution solves once, centrally, what twenty national monetary authorities previously solved inconsistently.
% TRANSFER_FUNCTION: Moves real purchasing power from debtors to creditors through the disinflationary bias (real debt burdens stay higher than under an operationally balanced mandate); moves policy attention and balance-sheet allocation away from employment support and climate transition toward price-level control; allocates asset-purchase support across firms by market weights rather than transition need, channeling funding toward incumbent carbon-intensive issuers.
% ABSENT_VOICES: Climate policy institutions would operationalize Article 11 TFEU integration but hold no seat in mandate interpretation; indebted member-state finance ministries would weight employment and fiscal sustainability; labor representatives would weight the employment side of the tradeoff. Their absence is not incidental — the ECB's independence is precisely the institutional form their exclusion takes, and the enforcement machinery defends that exclusion as mandate protection.
% DISAPPEARANCE_RATIONALE: If the orthodox reading vanished overnight — if secondary objectives acquired operational weight — inflation expectations would re-anchor under a new framework, creditor portfolios would reprice as the disinflationary bias compressed, member-state fiscal space would expand in downturns, asset purchases would re-weight toward transition needs, and the bank's political accountability settlement would be renegotiated. The eurozone monetary constitution would rearrange around the new reading.
% FOUNDING_PROBLEM: Post-Bretton-Woods inflation credibility: after the 1970s Great Inflation and the collapse of fixed exchange rates, European monetary authorities lacked credibility; wage-price spirals, political business cycles, and competitive devaluation threatened the single market. Maastricht's answer was an independent central bank with a legally entrenched price-stability priority, inheriting the Bundesbank's settlement.
% FOUNDING_PROBLEM_CORROBORATION: Academic monetary economics (the Kydland-Prescott and Barro-Gordon time-inconsistency literature) attests the credibility problem independently of any creditor interest; the post-2021 inflation surge was experienced as costly by the arrangement's own payer seats — labor unions and indebted governments, who dispute the exclusivity but not the reality of inflation harm. Corroboration exists from outside the benefiting parties; what those sources contest is the exclusivity, not the founding problem's existence.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66 at interval end because the disinflationary bias transfers real purchasing power from debtors to creditors and the balance-sheet framework externalizes climate cost, while the expectations-anchoring good is real and broadly shared — the extraction is the asymmetry, not the whole arrangement. Suppression is 0.70 because the reading's persistence depends on active legal and institutional defense: the bank's legal service answers every expansion claim, the CJEU settlement is defended, and internal dissent is disciplined — an enforcement ratchet that rises with each expansion attempt. Theater is 0.42: the 'without prejudice' clause is invoked in every strategic communication while carrying no operational weight, and the 2021 strategy review's concessions (symmetry language, climate acknowledgment) changed rhetoric without changing rate-setting or collateral rules — a growing share of the arrangement's communicative activity performs a balance it does not practice. Accessibility collapse is 0.45: the alternative readings remain live in courts, parliaments, and academia, so alternatives are suppressed in operation but not foreclosed in discourse. Resistance is 0.58: German constitutional litigation, climate litigation, political pressure from indebted member states, and heterodox economics all contest the reading. The measurement series share one grid (t=0,5,9,13,17,21,23,26) so every metric is authored at every examined point. The extractiveness series is cyclical rather than monotonic: crisis episodes (the 2010-2015 austerity-era orthodoxy) push extraction up, crisis-response flexibility (QE-era flexibility rhetoric around 2016, the pandemic program's temporary breach of market-neutrality around 2020) pulls it down, and each episode ends with the baseline re-ratcheted higher — the oscillation is itself partly the enforcement mechanism, since flexibility is granted under exceptional powers and withdrawn afterward, with intermittent reinforcement disciplining expansion advocates. The suppression series is the enforcement ratchet proper and rises monotonically across the interval.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is the credibility settlement the institution exists to protect: the narrow reading is what makes independence possible, and every expansion claim is a threat to the anchor. From the creditor seat the arrangement is protection, not privilege — the 2% anchor is experienced as the defense of savings against the 1970s, and portfolio mobility makes the protection cheap to hold. From the trapped payer seats the same structure operates as enforced redistribution: households and workers bear costs they did not contract for and cannot exit, and the member-state seat experiences the reading as fiscal subordination enforced through backstop conditionality. The excluded climate seat experiences a fourth arrangement: a legal order that acknowledges its clause and never applies it. Coalition potential among the powerless payers exists on paper — indebted households and labor could align — but the agenda is set at a supranational level where neither holds a seat, and the member states who might carry their claim are themselves constrained payers. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain runs on the declared structure without overrides. savers_and_creditors sit at the beneficiary end (d near 0.0): the mandate subsidizes their nominal claims and their exit — repricing, hedging, cross-asset and cross-border shifts — is arbitrage-grade, so effective extraction damps toward subsidy for them. ecb_governing_council sits near the beneficiary end as well: it collects institutional insulation, and its identity is fused with the price-stability function (identity_locked exit — the institution has become its mandate). indebted_households, unemployed_workers, and high_debt_member_states sit near the full-target end (d near 1.0): they pay the transfer, and their exits — leaving the euro, escaping fixed nominal contracts — are trapped or prohibitively constrained; the powerful member-state seat is differentiated from the powerless household seat by bargaining leverage, not by exit. future_generations are full targets with no voice and no repricing mechanism reaching them. climate_policy_institutions are excluded rather than coordinated — the enforcement machinery exists partly to keep them outside the interpretive conversation, which is a different structural position from paying. No directionality_overrides are needed: the beneficiary/victim declarations plus exit options already produce the correct d for every seat, and the one genuinely dual-positioned agent (the ECB: agenda_setter and beneficiary) is captured by its secondary_role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two opposite mislabels. Reading the mandate as a pure rope (the orthodox self-description) would erase the distributional asymmetry — the narrow beneficiary set, the externalized climate risk, and the suppressed alternatives are structural, not incidental. Reading it as a pure snare would erase the genuine coordination achievement — expectations anchoring solves a real collective-action problem that predates and would survive any redistribution of the arrangement's costs. On mandatrophy: the founding problem (post-Bretton-Woods inflation credibility) is live, not dead — post-2021 inflation re-demonstrated it — so the arrangement is not a zombie and no dead-mandate declaration is authored. The theater_ratio (0.42, rising) tracks a specific partial atrophy — the 'without prejudice' clause's operational emptiness and the strategy review's rhetorical concessions — not general decay of the price-stability function, which remains fully operative. The classification also blocks the inverse error: because the coordination function is genuine, the payer seats' experience of pure extraction cannot be certified as the whole story; the engine's per-seat computation is what separates the creditor seat's rope-experience from the trapped payer seats' snare-experience within one structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the Article 127 kernel is legally operative — does ''without prejudice'' carry zero operational weight (this story''s orthodox reading), discretionary weight (expansive_secondary_objectives), or does Article 11 impose binding climate integration (climate_incorporation)?',
    'A definitive CJEU ruling on mandate interpretation or a treaty amendment; interim evidence from how the bank''s legal service answers expansion claims and how national constitutional courts receive them.',
    'This story instantiates only the orthodox reading; if a sibling reading prevails, the beneficiary and victim sets restructure (the creditor transfer compresses, climate risk internalizes) and the sibling''s own file carries the re-authored epsilon — this story''s classification would be superseded for the seats it misdescribes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three live readings of the Article 127 kernel; the disagreement is located in the operational weight of the secondary clauses.').

omega_variable(
    exclusivity_necessity,
    'Is exclusive operational focus on the inflation target technically necessary for expectations anchoring, or is it a distributional choice that a balanced mandate could make without de-anchoring expectations?',
    'Comparative central-banking evidence: whether dual-mandate central banks (the Federal Reserve, the Bank of England, RBNZ) show systematically weaker anchoring than single-target banks, controlling for credibility history.',
    'If balanced mandates anchor as well, exclusivity is a distributional choice rather than a technical necessity — the extraction component grows and the arrangement moves toward the snare end of the hybrid range; if exclusivity is necessary, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_necessity, empirical, 'Whether the mandate''s exclusivity is technically necessary or distributionally chosen.').

omega_variable(
    disinflation_transfer_magnitude,
    'How large is the actual purchasing-power transfer from debtors to creditors that the disinflationary bias produces, relative to the coordination value the anchor delivers?',
    'The ECB''s distributional wealth accounts plus counterfactual simulations of the inflation path under an operationally balanced mandate.',
    'Quantifies the extraction component of the base measure: a large transfer relative to coordination value supports drift toward the snare end; a small one supports the rope reading the orthodox seat claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disinflation_transfer_magnitude, empirical, 'Size of the debtor-to-creditor transfer under the disinflationary bias.').

omega_variable(
    climate_externalization_legality,
    'Does Article 11 TFEU''s environmental integration clause bind central-bank operations — making market-neutrality in purchases and collateral legally incomplete — or is climate policy a fiscal-domain matter the mandate properly excludes?',
    'A CJEU ruling on Article 11''s applicability to ESCB operations; in the interim, carbon-intensity comparisons between the collateral framework and the economy-wide credit stock.',
    'If Article 11 binds, the orthodox reading is legally incomplete rather than merely contested, and the climate_incorporation sibling gains decisive force; if it does not, the externalized climate cost is a policy gap, not a mandate violation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_externalization_legality, conceptual, 'Whether the externalized climate risk reflects a legally binding integration obligation the reading refuses.').

omega_variable(
    suppression_legitimacy,
    'Is the enforcement machinery that suppresses mandate expansion a legitimate commitment device protecting all seats from fiscal dominance, or the defense of creditor rents by an insulated administrator?',
    'Institutional analysis of accountability performance under alternative mandate settlements, plus the political-theory literature on independent authorities; no purely empirical resolution is available.',
    'If the suppression is legitimate insulation, its measured intensity is coordination cost and the arrangement sits nearer the rope end; if it is rent defense, the same intensity is enforcement of extraction and the arrangement sits nearer the snare end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_legitimacy, preference, 'Whether suppression of mandate expansion is commitment or rent defense.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(ecb__tr_t0, observed).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(ecb__tr_t5, observed).
narrative_ontology:measurement(ecb__tr_t9, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 9, 0.25).
narrative_ontology:measurement_basis(ecb__tr_t9, observed).
narrative_ontology:measurement(ecb__tr_t13, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 13, 0.3).
narrative_ontology:measurement_basis(ecb__tr_t13, observed).
narrative_ontology:measurement(ecb__tr_t17, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 17, 0.34).
narrative_ontology:measurement_basis(ecb__tr_t17, observed).
narrative_ontology:measurement(ecb__tr_t21, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 21, 0.38).
narrative_ontology:measurement_basis(ecb__tr_t21, observed).
narrative_ontology:measurement(ecb__tr_t23, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 23, 0.4).
narrative_ontology:measurement_basis(ecb__tr_t23, observed).
narrative_ontology:measurement(ecb__tr_t26, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 26, 0.42).
narrative_ontology:measurement_basis(ecb__tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(ecb__be_t0, observed).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(ecb__be_t5, observed).
narrative_ontology:measurement(ecb__be_t9, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 9, 0.52).
narrative_ontology:measurement_basis(ecb__be_t9, observed).
narrative_ontology:measurement(ecb__be_t13, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 13, 0.68).
narrative_ontology:measurement_basis(ecb__be_t13, observed).
narrative_ontology:measurement(ecb__be_t17, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 17, 0.58).
narrative_ontology:measurement_basis(ecb__be_t17, observed).
narrative_ontology:measurement(ecb__be_t21, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 21, 0.52).
narrative_ontology:measurement_basis(ecb__be_t21, observed).
narrative_ontology:measurement(ecb__be_t23, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 23, 0.62).
narrative_ontology:measurement_basis(ecb__be_t23, observed).
narrative_ontology:measurement(ecb__be_t26, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 26, 0.66).
narrative_ontology:measurement_basis(ecb__be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(ecb__su_t0, observed).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(ecb__su_t5, observed).
narrative_ontology:measurement(ecb__su_t9, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 9, 0.45).
narrative_ontology:measurement_basis(ecb__su_t9, observed).
narrative_ontology:measurement(ecb__su_t13, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 13, 0.55).
narrative_ontology:measurement_basis(ecb__su_t13, observed).
narrative_ontology:measurement(ecb__su_t17, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 17, 0.6).
narrative_ontology:measurement_basis(ecb__su_t17, observed).
narrative_ontology:measurement(ecb__su_t21, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 21, 0.65).
narrative_ontology:measurement_basis(ecb__su_t21, observed).
narrative_ontology:measurement(ecb__su_t23, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 23, 0.68).
narrative_ontology:measurement_basis(ecb__su_t23, observed).
narrative_ontology:measurement(ecb__su_t26, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 26, 0.7).
narrative_ontology:measurement_basis(ecb__su_t26, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% The colloquial label 'the ECB mandate' covers three structurally distinct claims that this family decomposes per the epsilon-invariance principle: the orthodox exclusivity reading (this story), the expansive 'without prejudice' balancing reading, and the climate-integration reading. Each carries its own stable epsilon, beneficiary/victim structure, and classification; all are linked via network blocks. The orthodox story is upstream in the empirical sense — it is the currently operative reading, so the arrangement's actual operation is best attested here — and it influences the downstream siblings because every expansion claim must argue within or against the orthodox settlement's legal terrain rather than on open ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
