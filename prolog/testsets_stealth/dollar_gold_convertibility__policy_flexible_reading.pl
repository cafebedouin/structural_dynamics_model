% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Conditional Dollar-Gold Convertibility (Domestic Primacy Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   Under the Bretton Woods Articles, the United States committed to convert
 *   foreign official dollar holdings into gold at $35/oz. The policy-flexible
 *   reading — the one the U.S. Treasury and Federal Reserve actually operated
 *   between the 1958 restoration of convertibility and the August 1971
 *   closure of the gold window — holds that commitment to be a conditional
 *   obligation, subordinate to domestic economic stability: full employment,
 *   war finance, and domestic price stability take precedence over the
 *   external gold promise. Operationally, the window stayed formally open
 *   while U.S. dollar liabilities expanded to finance deficits; foreign
 *   official and private holders bore the accumulating devaluation risk; and
 *   the enforcement machinery (London Gold Pool, capital controls, bilateral
 *   pressure on creditors) worked not to honor the commitment but to
 *   discourage its exercise. Constraint family: the colloquial label
 *   'dollar-gold convertibility' decomposes into three readings of one kernel
 *   — this policy-flexible reading (conditional obligation; dollar holders
 *   enter the victim set via devaluation risk, the U.S. exits the victim set
 *   and regains monetary autonomy, and the extraction is located at external
 *   creditors), the strict-convertibility reading (binding legal obligation;
 *   U.S. monetary policy is the target), and the Triffin structural reading
 *   (inherent design flaw; the system itself is the patient). Per the
 *   epsilon-invariance rule this file authors only the flexible reading as a
 *   clean, stable-epsilon constraint; the siblings are separate stories
 *   linked in network.affects_constraints, not hedges folded into this one.
 *   KEY AGENTS (by structural relationship): - us_monetary_authorities:
 *   agenda-setter and beneficiary (institutional/arbitrage) — administers the
 *   conditional window, collects seigniorage and domestic policy autonomy -
 *   us_fiscal_authorities: beneficiary (institutional/mobile) — receives the
 *   resource transfer as foreign-financed deficit capacity; the seat where
 *   the gains accrue - foreign_dollar_holding_central_banks: primary target
 *   (organized/constrained) — bear devaluation risk on official reserves,
 *   managed bilaterally and isolated from each other -
 *   private_dollar_reserve_holders: payer with secondary beneficiary position
 *   (moderate/mobile) — dollar network benefits with devaluation exposure;
 *   convertibility right removed outright in 1968 -
 *   gaullist_france_gold_advocates: excluded (organized/mobile) — reform
 *   program outside the operating consensus; the arrangement's most visible
 *   resistance - monetary_economists_triffin_tradition: analytical observer —
 *   maps the full structure from outside; collects nothing, bears nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.72).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.75).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Conditional Dollar-Gold Convertibility (Domestic Primacy Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'c6a7f611-17fe-429a-839e-c0005672f740').
narrative_ontology:cs_kernel_codification('c6a7f611-17fe-429a-839e-c0005672f740', fixed_text).
narrative_ontology:cs_authority_grounding('c6a7f611-17fe-429a-839e-c0005672f740', extraction).
narrative_ontology:cs_interpretation_layer_present('c6a7f611-17fe-429a-839e-c0005672f740').
narrative_ontology:cs_reading_relation('c6a7f611-17fe-429a-839e-c0005672f740', dollar_gold_convertibility__strict_convertibility_reading, influences).
narrative_ontology:cs_reading_relation('c6a7f611-17fe-429a-839e-c0005672f740', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('c6a7f611-17fe-429a-839e-c0005672f740', foundational, domestic_stability_primacy_over_convertibility).
narrative_ontology:cs_axiom_status(domestic_stability_primacy_over_convertibility, holdable).
narrative_ontology:cs_axiom_grounding('c6a7f611-17fe-429a-839e-c0005672f740', domestic_stability_primacy_over_convertibility, conventional).
narrative_ontology:cs_axiom('c6a7f611-17fe-429a-839e-c0005672f740', secondary, conditional_convertibility_preserves_system_liquidity).
narrative_ontology:cs_axiom_status(conditional_convertibility_preserves_system_liquidity, holdable).
narrative_ontology:cs_axiom_grounding('c6a7f611-17fe-429a-839e-c0005672f740', conditional_convertibility_preserves_system_liquidity, instrumental).
narrative_ontology:cs_reference_frame('c6a7f611-17fe-429a-839e-c0005672f740', domestic_primacy_conditional_convertibility).
narrative_ontology:cs_drift_state('c6a7f611-17fe-429a-839e-c0005672f740', two_tier_gold_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6a7f611-17fe-429a-839e-c0005672f740', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_fiscal_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_holding_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, private_dollar_reserve_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, private_dollar_reserve_holders).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, domestic_policy_supremacy_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, fundamental_disequilibrium_adjustment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the gold window and decide in practice when domestic economic conditions override the convertibility promise: Treasury and Federal Reserve operations (the London Gold Pool, swap lines, reserve-pressure diplomacy) manage foreign gold demands rather than simply satisfy them. They collect seigniorage on outstanding dollar liabilities and keep full domestic monetary freedom; the arrangement costs them only the burden of defending the window. Their exit is total — they set the terms and can suspend the commitment unilaterally, as August 1971 showed.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, us_monetary_authorities, beneficiary).

% Finance military and domestic spending by issuing dollar claims that foreign official holders absorb to keep the settlement system running. The resulting capacity lands here as spendable resources without matching domestic taxation or restraint. The gold promise disciplines this seat only if creditors actually demand gold, which the window's management is designed to discourage. Exit from any external discipline is effectively open so long as the window holds.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_fiscal_authorities, beneficiary,
    institutional, biographical, mobile, national).

% Accumulate dollar reserves as international settlement requires, bearing the devaluation risk of U.S. domestic policy choices they do not control. Demanding gold in size risks collapsing the system their export economies depend on and invites bilateral pressure — offset agreements, troop-cost linkage, as with Germany; diversifying into sterling or gold means accepting reserve losses or rupture. Their collective position is strong; individually they are isolated by Washington's bilateral management.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_holding_central_banks, payer,
    organized, generational, constrained, global).

% Hold and invoice in dollars for trade and liquidity convenience — the dollar's network benefits reach this seat — while carrying the same devaluation exposure as official holders with less protection. They can shift into other currencies or assets more easily than central banks, and the Eurodollar market grew as their arbitrage response. The 1968 two-tier gold market removed their convertibility right entirely, making their exposure explicit and purely holding-based.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, private_dollar_reserve_holders, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, private_dollar_reserve_holders, beneficiary).

% The French government and its economic advisors (the Rueff circle) demanded a return to gold-standard discipline, converted dollar holdings into gold aggressively from 1965 onward, and proposed systemic reform of the reserve system. Their diagnosis was dismissed as ideological and never entered the operating consensus; their conversion campaign was met with management and pressure rather than answered on the merits.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, gaullist_france_gold_advocates, excluded,
    organized, biographical, mobile, continental).

% Diagnose the structure from outside the operating consensus: dollar liabilities grow with world liquidity needs until confidence in the gold promise fails, so conditionality is not an accident of policy but the mechanism's normal operating mode. Congressional testimony (Triffin, 1960) and academic work map the full arrangement; this seat collects nothing and bears nothing.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, monetary_economists_triffin_tradition, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_fiscal_authorities).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies the non-communist world with reserve liquidity and a stable exchange-rate anchor: foreign central banks can hold dollars convertible-in-principle into gold, expanding trade without each country hoarding gold, while the United States runs the settlement mechanism and, under this reading, keeps domestic employment and war finance unconstrained by the external commitment.
% TRANSFER_FUNCTION: Moves real goods and financing capacity from foreign economies to the United States: foreign central banks absorb dollar claims (financing U.S. deficits) and bear the devaluation risk of U.S. domestic policy, in exchange for a reserve asset that is gold-backed only so long as U.S. domestic conditions permit.
% ABSENT_VOICES: Creditor central banks spoke only bilaterally and were managed individually — Germany via offset and troop-cost linkage; the 1968 two-tier gold market was imposed without creditor consent. The French reform program was excluded as ideology. Private holders had no seat at all: the 1968 suspension of their convertibility right was announced, not negotiated.
% DISAPPEARANCE_RATIONALE: If the conditional-convertibility arrangement vanished overnight, the dollar's reserve role would not survive in its then form: either the strict commitment binds (forcing immediate U.S. domestic contraction and gold drain) or no anchor remains (exchange rates float, reserve composition scrambles). U.S. deficit finance, creditor reserve strategies, and the Eurodollar market's growth all presuppose the conditional window.
% FOUNDING_PROBLEM: The interwar monetary chaos — competitive devaluation, gold shortage, deflationary adjustment — plus postwar liquidity scarcity: Bretton Woods was designed to combine gold-backed credibility with adjustable pegs and elastic reserves, with dollar-gold convertibility as the anchor that made the dollar a credible reserve asset.
% FOUNDING_PROBLEM_CORROBORATION: The IMF Articles of Agreement and founding-conference records attest the original convertibility design — a multilateral text outside the U.S. beneficiary set. Creditor central bank records and BIS archives attest the shift from dollar shortage to dollar glut. Triffin's 1960 congressional testimony and Rueff's published critiques — neither a U.S. beneficiary — attest that by the 1960s the founding problem had transformed from liquidity scarcity into a confidence problem.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.72 terminal) because the conditionality transferred devaluation risk to creditors without pricing: the U.S. expanded dollar liabilities to finance domestic priorities while the gold backing of those liabilities became progressively conditional, then residual. Suppression is higher still (0.75) because the arrangement's persistence depended on discouraging exercise of the convertibility right — gold-pool market management, the Interest Equalization Tax, voluntary and then mandatory capital controls, bilateral pressure on Germany and Japan, and finally the 1968 two-tier market that removed private holders' convertibility outright — rather than on participant preference. Theater rises 0.20 to 0.65: the 'dollar as good as gold' formula was maintained rhetorically throughout while the substance became conditional and then hollow; the terminal 1971 point captures the pre-closure phase, when the window was formally open, universally expected to close, and the performance was at its peak — the closure itself terminated the constraint rather than continuing it. Accessibility_collapse is 0.55: alternatives existed (gold demands, reserve diversification, SDR creation, floating) but each carried system-collapse risk, invited bilateral retaliation, or required U.S. consent. Resistance is 0.60: the French conversion campaign from 1965, creditor pressure after sterling's 1967 devaluation, and the gold runs of 1967-68 were real and partially effective. The claimed type is tangled_rope, authored independently of the metrics: the arrangement solved a genuine coordination problem (reserve liquidity provision, exchange-rate anchor, settlement mechanism that all parties consumed) AND carried asymmetric extraction through the same structure (creditors absorbed U.S. domestic-policy risk; enforcement held the asymmetry in place). All three tracked metric series run on one shared seven-point grid (1958, 1961, 1964, 1967, 1968, 1969, 1971); the 1969 dip in suppression_requirement is a real lull (the 1969 U.S. external surplus paused gold losses and enforcement intensity briefly) inside an overall enforcement-ratchet trajectory, not a grid artifact.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the U.S. seat the arrangement is a legitimate conditional obligation it administered in good faith — the Articles' own adjustment provisions contemplate par-value change in fundamental disequilibrium, and liquidity provision required the reserve-center not to deflate. From the creditor seats the same structure operated as uncompensated risk transfer: each creditor absorbed the devaluation consequences of U.S. domestic choices it could not vote on, while its contractual remedy (gold demand) was managed, pressured, and finally partitioned away. Same-level lateral divergence: creditor central banks held the same nominal power class but experienced different exit — Germany (troop-cost linkage and offset agreements made gold demands costly), France (converted aggressively and absorbed retaliation), the United Kingdom (sterling entanglement made diversification self-harming). Constraint-specific factors, not global standing, differentiated their positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low directionality: us_monetary_authorities (agenda-setter and collector, arbitrage-grade exit) and us_fiscal_authorities (receives the deficit-financing capacity, mobile exit) sit near the beneficiary end, so effective extraction is damped or inverted into subsidy for them. Victims map to high directionality: foreign_dollar_holding_central_bears sit near the full-target end — trapped by system-collapse risk and bilateral pressure, their constrained exit amplifies effective extraction — while private_dollar_reserve_holders are moderated by mobile exit and their genuine network benefits. Excluded and observer seats carry no extraction arithmetic. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled by the engine, via directionality and the arrangement's global spatial scope (larger scope, harder verification, modest amplification).
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid classification prevents mislabeling in both directions. Reading the arrangement as pure coordination (the U.S. official framing: a cooperative liquidity mechanism whose conditionality is priced-in system design) would erase the creditor-side asymmetry — the uncompensated risk transfer and the enforcement machinery that held it in place. Reading it as pure extraction (the Gaullist framing: an American default threat dressed as a reserve system) would erase the genuine coordination function every party consumed — liquidity provision, the exchange-rate anchor, the settlement mechanism — and the fact that creditors stayed in the system for two decades partly because it worked. The genealogy interview sharpens the mandatrophy question: the founding problem (dollar shortage, liquidity scarcity) transformed into its opposite (dollar glut) during the interval, while the arrangement persisted and its enforcement intensified. Founding_problem_status is authored contested rather than dead because the parties dispute the transformation — the U.S. attested liquidity provision remained the live problem; creditors and the Triffin-tradition economists attested the confidence problem had replaced it. If the corpus later resolves the status as dead against a world_rearranges verdict, the mismatch flag fires and should be cross-checked against the rising theater path documented in the measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the policy_flexible_reading of the dollar_gold_convertibility kernel; how would the sibling readings (strict_convertibility_reading, triffin_structural_reading) restructure the victim set and epsilon if instantiated instead?',
    'Author the sibling stories as separate constraints and compare computed classifications: the strict reading moves U.S. monetary policy into the victim set and removes creditor devaluation exposure; the triffin reading relocates the flaw from policy choice to system design.',
    'Cross-reading comparison isolates what the conditionality itself contributes to the measured extraction versus what the underlying gold commitment contributes; classification divergence between readings is the measurement the corpus exists to take, not an error to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the dollar-gold kernel: each reading is a separate constraint with its own epsilon, beneficiaries, and victims.').

omega_variable(
    conditionality_legitimacy,
    'Is the domestic-primacy conditionality a legitimate reading of the Bretton Woods Articles (whose adjustment provisions contemplate par-value change in fundamental disequilibrium) or a unilateral breach dressed as interpretation?',
    'Drafting history of the convertibility articles and founding-conference records; state practice and IMF executive-board deliberations 1961-1971; whether any multilateral body ever adjudicated the conditionality rather than the U.S. alone defining when the condition binds.',
    'If breach, the arrangement operates as extraction under default threat and the computed type moves toward the pure-extraction end; if legitimate, the condition is priced-in system design and the hybrid coordination-plus-asymmetry reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_legitimacy, conceptual, 'Whether the conditionality was legitimate interpretation of the kernel text or breach of it.').

omega_variable(
    devaluation_risk_compensation,
    'Did foreign official holders receive compensation for the devaluation risk they bore (offset payments, burden-sharing side-deals, preferential terms), or was the risk transfer uncompensated?',
    'Central bank reserve-management records 1958-1971; compare realized returns on dollar reserves against alternative reserve assets; declassified offset and troop-cost agreements with Germany and Japan.',
    'Full compensation would reframe the transfer as a priced exchange and lower the effective extraction attributable to the conditionality; uncompensated exposure confirms the extractive reading and raises it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(devaluation_risk_compensation, empirical, 'Whether creditor devaluation risk was compensated or extracted without pricing.').

omega_variable(
    creditor_exit_coercion,
    'Were creditors'' continued dollar holdings voluntary coordination or coerced by U.S. pressure (offset agreements, troop-cost linkage, gold-pool discipline, reserve-pressure diplomacy)?',
    'Declassified diplomatic and Treasury records on bilateral pressure campaigns 1961-1968; internal Bundesbank and Japanese finance-ministry deliberations on reserve composition decisions.',
    'Coerced holdings raise effective suppression and shift the computed type toward the pure-extraction end; voluntary holdings support the genuine-coordination half of the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_exit_coercion, empirical, 'Structural vs. coerced character of official dollar accumulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1958, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(doll_tr_t1961, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1961, 0.25).
narrative_ontology:measurement(doll_tr_t1964, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1964, 0.32).
narrative_ontology:measurement(doll_tr_t1967, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1967, 0.42).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1968, 0.55).
narrative_ontology:measurement(doll_tr_t1969, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1969, 0.6).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.65).

% Extraction over time
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1958, 0.45).
narrative_ontology:measurement(doll_be_t1961, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1961, 0.5).
narrative_ontology:measurement(doll_be_t1964, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1964, 0.55).
narrative_ontology:measurement(doll_be_t1967, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1968, 0.67).
narrative_ontology:measurement(doll_be_t1969, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1969, 0.7).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(doll_su_t1961, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1961, 0.4).
narrative_ontology:measurement(doll_su_t1964, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1964, 0.45).
narrative_ontology:measurement(doll_su_t1967, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(doll_su_t1969, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1969, 0.62).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'dollar-gold convertibility' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the strict legal reading (binding obligation; the U.S. monetary authorities are the target and the constraint's epsilon measures the domestic policy burden), the policy-flexible reading (this file; conditional obligation; external dollar holders are the target, the U.S. exits the victim set, and the epsilon measures the risk transfer to creditors), and the Triffin structural reading (inherent design flaw; the system itself is the patient and the epsilon measures design-level unsustainability). The upstream kernel text (Article IV) feeds all three. The flexible reading's operation — the accumulating dollar overhang — is the empirical material the Triffin reading cites and the pressure that made the strict reading materially unenforceable; each story links the others via affects_constraints and carries its own epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
