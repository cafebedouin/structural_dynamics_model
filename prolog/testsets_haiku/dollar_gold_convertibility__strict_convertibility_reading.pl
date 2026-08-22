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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Gold Convertibility as Binding Legal Obligation (Strict Reading)
 *   domain: international_political_economy/monetary_law
 *
 * SUMMARY:
 *   Article IV of the IMF Articles of Agreement (1944) establishes
 *   convertibility of the U.S. dollar into gold at a fixed parity ($35 per
 *   ounce) on demand by foreign central banks and governments. Under the
 *   strict reading instantiated here, this is a binding legal obligation that
 *   constrains U.S. monetary policy to the quantity of gold reserves
 *   available for redemption. As the U.S. balance-of-payments deficit widened
 *   through the 1950s and 1960s (driven by Cold War military spending,
 *   foreign aid, and domestic welfare expansion), the gap between dollar
 *   liabilities held abroad and gold reserves narrowed, creating an
 *   increasingly untenable position. The strict reading treats convertibility
 *   not as a policy flexibility to be adjusted to domestic conditions, but as
 *   an ironclad commitment that the U.S. cannot honorably abandon. This
 *   reading dominated official policy discourse until Nixon's suspension of
 *   convertibility on August 15, 1971. The measurement series captures the
 *   accumulating tension: as dollar-gold ratio worsened, extractiveness rose
 *   (the constraint increasingly forced the U.S. to subordinate domestic
 *   policy to reserve preservation), suppression intensified (diplomatic and
 *   institutional pressure to maintain the commitment grew more forceful),
 *   and theater increased (the commitment became increasingly performative as
 *   structural reality made it unsustainable).
 *
 * KEY AGENTS:
 *   - U.S. Treasury and Federal Reserve: institutional victim, trapped by the convertibility obligation, unable to expand money supply
 *   - Creditor nations (France, United Kingdom, other surplus nations): institutional beneficiaries with enforceable gold redemption rights
 *   - Gold-standard maintainers (IMF, central bankers, orthodox economists): agenda-setters enforcing the strict interpretation
 *   - U.S. domestic constituencies (workers, unemployed, growth-focused policymakers): excluded payers bearing the cost of monetary constraint
 *   - Heterodox economists and structural critics (Triffin, Kindleberger): observers questioning the sustainability of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.82).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.71).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Gold Convertibility as Binding Legal Obligation (Strict Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy/monetary_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, '04ca9a7a-eba8-4823-aaec-6c19d638da4d').
narrative_ontology:cs_kernel_codification('04ca9a7a-eba8-4823-aaec-6c19d638da4d', formalized).
narrative_ontology:cs_authority_grounding('04ca9a7a-eba8-4823-aaec-6c19d638da4d', extraction).
narrative_ontology:cs_interpretation_layer_present('04ca9a7a-eba8-4823-aaec-6c19d638da4d').
narrative_ontology:cs_reading_relation('04ca9a7a-eba8-4823-aaec-6c19d638da4d', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_reading_relation('04ca9a7a-eba8-4823-aaec-6c19d638da4d', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('04ca9a7a-eba8-4823-aaec-6c19d638da4d', foundational, article_iv_legally_binding).
narrative_ontology:cs_axiom_status(article_iv_legally_binding, holdable).
narrative_ontology:cs_axiom_grounding('04ca9a7a-eba8-4823-aaec-6c19d638da4d', article_iv_legally_binding, conventional).
narrative_ontology:cs_axiom('04ca9a7a-eba8-4823-aaec-6c19d638da4d', foundational, convertibility_non_subordinate).
narrative_ontology:cs_axiom_status(convertibility_non_subordinate, overridden).
narrative_ontology:cs_axiom_grounding('04ca9a7a-eba8-4823-aaec-6c19d638da4d', convertibility_non_subordinate, deontological).
narrative_ontology:cs_reference_frame('04ca9a7a-eba8-4823-aaec-6c19d638da4d', postwar_monetary_discipline).
narrative_ontology:cs_drift_state('04ca9a7a-eba8-4823-aaec-6c19d638da4d', nineteen_seventy_one, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('04ca9a7a-eba8-4823-aaec-6c19d638da4d', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, gold_standard_maintainers).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_issuer).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_monetary_policy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_constituencies).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_president_and_congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obligated under Article IV of the IMF Articles of Agreement to maintain convertibility of the dollar into gold at a fixed parity ($35/oz) on demand by foreign central banks and governments. Bears the cost of maintaining gold reserves sufficient to honor unlimited redemption requests. Constrained from expanding the monetary base beyond what gold reserves can support, preventing expansionary monetary policy to address domestic unemployment or growth.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_treasury_issuer, payer,
    institutional, generational, trapped, global).

% Hold dollar reserves backed by the legal guarantee of gold convertibility. Can redeem dollars for gold at the fixed rate, providing insurance against inflation or dollar devaluation. Benefit from the dollar's stability and reserve-currency status while retaining the option to claim physical gold. France, the United Kingdom, and other surplus nations use convertibility as leverage to enforce discipline on U.S. fiscal and monetary policy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    powerful, generational, arbitrage, global).

% International institutions (IMF, Bank for International Settlements) and the gold-standard-committed financial establishment (central bankers, orthodox economists) adjudicate and enforce the strict interpretation of Article IV. They defend convertibility as the cornerstone of monetary discipline and international stability, opposing any deviation as inflationary and irresponsible. Maintain the rule through institutional consensus and diplomatic pressure.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, gold_standard_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).

% American workers, businesses, and unemployed populations bear the costs of monetary constraint without representation in the international negotiation that binds U.S. policy. The constraint prevents the Federal Reserve from expanding money supply to lower unemployment below what gold reserves constrain, sacrificing domestic welfare to foreign creditor demands.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_constituencies, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_constituencies, excluded).

% Critiques and publishes analysis challenging the strict reading: argue that the legal obligation is unsustainable given structural payments imbalances, that it subordinates domestic policy autonomy to foreign creditor demands, and that it will eventually force either devaluation or abandonment. Observe from outside the policy consensus but lack institutional power to shift the dominant reading.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, heterodox_economists, observer,
    moderate, biographical, analytical, global).

% Hold de jure power to suspend convertibility, but are politically and institutionally identity-locked into the gold standard consensus. Breaking the commitment would require repudiating a foundational postwar agreement, inviting accusations of economic irresponsibility and undermining U.S. credibility. Can change the rule only by explicit treaty revision, which requires negotiating with creditors who benefit from the status quo.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_president_and_congress, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__strict_convertibility_reading, us_president_and_congress, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__strict_convertibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article IV convertibility coordinates international monetary expectations by fixing the dollar's value in gold, providing exchange-rate stability for international trade and capital flows. Establishes the U.S. dollar as the numeraire for global commerce, anchoring creditor confidence in the dollar reserve-system.
% TRANSFER_FUNCTION: Transfers monetary policy autonomy from the U.S. domestic sphere to the international convertibility obligation. Creditor nations gain the right to redeem dollars for gold, constraining U.S. money supply to match the gold stock. Redistributes the benefits of seigniorage (the privilege of note issuance) away from the U.S. Treasury toward foreign central banks and gold-holding nations.
% ABSENT_VOICES: U.S. workers and domestic economic stakeholders are excluded from the international bargaining that constrains their monetary policy. Labor unions, unemployed workers, and growth-focused fiscal authorities would argue for domestic policy flexibility but lack standing in the IMF governance structure. Triffin's structural critique is muted within official channels.
% DISAPPEARANCE_RATIONALE: If the strict convertibility obligation disappeared, the U.S. could expand its money supply to address domestic unemployment, alter the incentive structure for creditor nations' exchange-rate policies, and allow the dollar to depreciate against gold and other currencies. The entire post-war monetary order — fixed exchange rates, dollar hegemony, gold-backed stability — would reorganize into a new equilibrium.
% FOUNDING_PROBLEM: Post-war international monetary chaos, rampant devaluation, and the absence of a stable international medium of exchange. The Bretton Woods system (of which Article IV is the legal anchor) was designed to restore predictability, facilitate trade, and rebuild war-torn economies by fixing currencies to gold through the dollar.
% FOUNDING_PROBLEM_CORROBORATION: Architects of Bretton Woods (White, Keynes, Morgenthau) attested to the need for stability. But by the late 1960s, creditor nations (especially France) and heterodox economists (Triffin, Kindleberger) attest that the problem has shifted: the founding problem (chaos) is solved, but the solution (Article IV constraint) now generates a different problem (U.S. policy subordination and structural imbalance). The strict reading's framing of foundational problem-status is contested by those observing the emerging Triffin dilemma.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness climbs from 0.35 (1944) to 0.82 (1971) because the structural deficit between liabilities and reserves widens exponentially, making the constraint increasingly extractive from U.S. domestic policy autonomy. The U.S. must choose between defending convertibility (by deflating the economy, raising unemployment) or abandoning the commitment (admitting the constraint was unsustainable). Suppression increases from 0.42 to 0.71 as international pressure to maintain convertibility intensifies — creditor nations demand the U.S. honor the obligation while simultaneously pursuing policies that widen the deficit. Theater rises from 0.12 to 0.42 as the maintenance of convertibility becomes increasingly performative: the U.S. implements gold-pool arrangements (1961-1968), restricts gold sales, and imposes capital controls while publicly insisting on the sustainability of the parity — observable performance divorced from structural reality. The coercion grid captures level-differentiated dynamics: structural-level accessibility to alternatives remains high (the U.S. technically could suspend convertibility), but as constraints cascade downward to organizational (Federal Reserve operating constraints), class (workers experiencing unemployment to preserve the parity), and individual level, alternatives compress. Stakes inflation is highest at the structural level (geopolitical credibility, systemic stability) and falls as it reaches individuals who bear costs but lack voice.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary (creditor nation) and agenda-setter (gold-standard maintainer) perspective, the strict reading is a reasonable and honorable commitment — it disciplines U.S. fiscal excess and provides legitimate security for reserve holdings. From the payer (U.S. Treasury and domestic constituencies) perspective, the same reading is a structural trap: it forces the subordination of domestic employment and growth to a commitment that has become arithmetically unsustainable. The U.S. Treasury computed a different directionality (d closer to 1.0 — full target) than the beneficiary seats, which computed d closer to 0.2 (modest collector, stable coordination function). The engine should compute this divergence from the stakeholder structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Creditor nations have high exit options (arbitrage: they can redeem dollars for gold, diversify into other currencies, or adjust their reserve strategies) and low cost of exit (it strengthens their position). They benefit from the constraint without depending on it — they can leave at will. U.S. Treasury has trapped exit: it cannot abandon convertibility without repudiating the IMF agreement and inviting international condemnation and financial instability. U.S. domestic constituencies are excluded from the negotiation and cannot exit at all (they are bound by national monetary policy they did not choose). The directionality hierarchy: beneficiary nations near d=0.2 (modest benefit, high exit); U.S. institutional layer near d=0.85 (acute victimhood, trapped); U.S. domestic constituencies near d=0.95 (victimized, no exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The strict reading prevents misclassifying this as pure rope (coordination without extraction). The constraint has genuine coordination function (exchange-rate stability, trade facilitation), but it is asymmetrically enforced — creditors can redeem dollars for gold; the U.S. cannot demand gold redemption from creditors. The extraction arises not from a failure of coordination but from the fact that coordination benefits are distributed unequally and enforced unidirectionally. Tangled rope captures this: coordination (exchange-rate stability) rides on extraction (creditors' enforceable claims, U.S. policy constraint). The 'mandatrophy' element emerges by 1968: the founding problem (post-war chaos, lack of a stable numeraire) is solved, but the solution (Article IV) now generates a new problem (Triffin dilemma — the mechanism that provides liquidity also sows the seeds of the system's collapse). The constraint persists despite its obsolete founding rationale because the beneficiary seats have no incentive to revise it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_flexible_interpretation,
    'Is Article IV to be interpreted as a binding legal obligation that cannot be suspended except by formal treaty amendment, or as a policy commitment subordinate to compelling domestic economic necessity?',
    'Historical analysis of the negotiating record (Bretton Woods delegates'' intent), comparative interpretation of treaty language by international law scholars, and examination of whether other countries'' monetary commitments were treated as similarly binding or subject to emergency suspension.',
    'If binding-only, the constraint drives toward structural crisis (Triffin dilemma) and eventual rupture. If flexible, the constraint becomes a policy tool available to the U.S. when domestic conditions demand, reducing extractiveness to the U.S. and shifting the constraint type from tangled_rope toward rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_flexible_interpretation, conceptual, 'Whether Article IV is a binding legal obligation or a subordinate policy commitment.').

omega_variable(
    triffin_dilemma_inevitability,
    'Given the structural imbalance between dollar liabilities and gold reserves, is the eventual breakdown of convertibility arithmetically inevitable, or could policy adjustments have stabilized the system?',
    'Counterfactual analysis: modeling whether alternative policies (Tobin tax, adjustment of parities, or acceleration of reserve diversification) could have extended convertibility beyond 1971. Economic historians have debated whether Nixon''s 1971 decision was forced by structural necessity or was a discretionary policy choice.',
    'If breakdown is inevitable, the constraint is a Snare masquerading as Rope — the U.S. is entrapped in an unworkable system. If avoidable, the breakdown reflects policy failure rather than structural impossibility, and the strict reading''s interpretation of Article IV becomes a causative factor in the crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_inevitability, empirical, 'Whether the Triffin dilemma makes convertibility mathematically unsustainable or whether policy alternatives could have extended the system.').

omega_variable(
    creditor_intent_and_enforcement,
    'Did creditor nations (especially France) deliberately pursue policies to exhaust U.S. gold reserves, or did the reserve depletion result from structural imbalances despite their intentions to maintain the system?',
    'Analysis of French and UK monetary policy decisions (currency reserves, capital flows, gold demand) in the 1960s. Historical testimony and archival records from central bank decision-making. Distinguish between deliberate enforcement (France''s gold purchases and reserve diversification as deliberate pressure) versus structural unraveling (deficit spending and inflation driving the reserve loss regardless of creditor intent).',
    'If deliberate enforcement, the extraction by creditor nations is intentional and strategic — the beneficiary seats were actively enforcing the constraint''s extractiveness. If structural, the extractiveness emerges from the mathematical logic of the system rather than creditor agency, making the constraint more structurally determined and less interpersonally extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_intent_and_enforcement, empirical, 'Whether creditor-nation enforcement of convertibility was deliberate or an emergent outcome of structural dynamics.').

omega_variable(
    readings_logical_compatibility,
    'Do the three readings of the dollar-gold-convertibility kernel occupy genuinely distinct logical frameworks, or do they all derive from the same core commitment and merely disagree on application?',
    'Formal analysis of the axioms each reading holds: does the strict reading''s ''binding legal obligation'' necessarily foreclose the policy-flexible reading''s ''subordinate commitment,'' or can both be held within a coherent framework? Does the Triffin reading''s ''structural impossibility'' foreclose both the strict and flexible readings, or is Triffin''s claim a contingent empirical claim about the system''s sustainability?',
    'If the readings foreclose one another, they cannot coexist in a single framework — only one can be adopted. If they coexist (different parties hold different readings), the contest is inter-institutional rather than intra-logical. If Triffin forecloses the others, the strict and flexible readings are both ultimately unsustainable and the breakdown of the system is overdetermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_logical_compatibility, conceptual, 'Whether the three kernel readings are logically incompatible or can coexist within a single framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1944, 0.12).
narrative_ontology:measurement_basis(doll_tr_t1944, observed).
narrative_ontology:measurement(doll_tr_t1951, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1951, 0.18).
narrative_ontology:measurement_basis(doll_tr_t1951, observed).
narrative_ontology:measurement(doll_tr_t1958, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1958, 0.28).
narrative_ontology:measurement_basis(doll_tr_t1958, observed).
narrative_ontology:measurement(doll_tr_t1965, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1965, 0.38).
narrative_ontology:measurement_basis(doll_tr_t1965, observed).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1968, 0.4).
narrative_ontology:measurement_basis(doll_tr_t1968, observed).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 1971, 0.42).
narrative_ontology:measurement_basis(doll_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement_basis(doll_be_t1944, observed).
narrative_ontology:measurement(doll_be_t1951, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1951, 0.48).
narrative_ontology:measurement_basis(doll_be_t1951, observed).
narrative_ontology:measurement(doll_be_t1958, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1958, 0.62).
narrative_ontology:measurement_basis(doll_be_t1958, observed).
narrative_ontology:measurement(doll_be_t1965, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1965, 0.74).
narrative_ontology:measurement_basis(doll_be_t1965, observed).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1968, 0.78).
narrative_ontology:measurement_basis(doll_be_t1968, observed).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 1971, 0.82).
narrative_ontology:measurement_basis(doll_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1944, 0.42).
narrative_ontology:measurement_basis(doll_su_t1944, observed).
narrative_ontology:measurement(doll_su_t1951, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1951, 0.51).
narrative_ontology:measurement_basis(doll_su_t1951, observed).
narrative_ontology:measurement(doll_su_t1958, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1958, 0.62).
narrative_ontology:measurement_basis(doll_su_t1958, observed).
narrative_ontology:measurement(doll_su_t1965, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1965, 0.68).
narrative_ontology:measurement_basis(doll_su_t1965, observed).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement_basis(doll_su_t1968, observed).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 1971, 0.71).
narrative_ontology:measurement_basis(doll_su_t1971, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1971
narrative_ontology:measurement(doll_grid_01, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(class), 1944, 0.38).
narrative_ontology:measurement(doll_grid_02, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(class), 1971, 0.62).
narrative_ontology:measurement(doll_grid_03, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(individual), 1944, 0.22).
narrative_ontology:measurement(doll_grid_04, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(individual), 1971, 0.48).
narrative_ontology:measurement(doll_grid_05, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(organizational), 1944, 0.42).
narrative_ontology:measurement(doll_grid_06, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(organizational), 1971, 0.68).
narrative_ontology:measurement(doll_grid_07, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(structural), 1944, 0.58).
narrative_ontology:measurement(doll_grid_08, dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse(structural), 1971, 0.78).
narrative_ontology:measurement(doll_grid_09, dollar_gold_convertibility__strict_convertibility_reading, resistance(class), 1944, 0.15).
narrative_ontology:measurement(doll_grid_10, dollar_gold_convertibility__strict_convertibility_reading, resistance(class), 1971, 0.68).
narrative_ontology:measurement(doll_grid_11, dollar_gold_convertibility__strict_convertibility_reading, resistance(individual), 1944, 0.08).
narrative_ontology:measurement(doll_grid_12, dollar_gold_convertibility__strict_convertibility_reading, resistance(individual), 1971, 0.58).
narrative_ontology:measurement(doll_grid_13, dollar_gold_convertibility__strict_convertibility_reading, resistance(organizational), 1944, 0.18).
narrative_ontology:measurement(doll_grid_14, dollar_gold_convertibility__strict_convertibility_reading, resistance(organizational), 1971, 0.72).
narrative_ontology:measurement(doll_grid_15, dollar_gold_convertibility__strict_convertibility_reading, resistance(structural), 1944, 0.22).
narrative_ontology:measurement(doll_grid_16, dollar_gold_convertibility__strict_convertibility_reading, resistance(structural), 1971, 0.78).
narrative_ontology:measurement(doll_grid_17, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(class), 1944, 0.28).
narrative_ontology:measurement(doll_grid_18, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(class), 1971, 0.65).
narrative_ontology:measurement(doll_grid_19, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(individual), 1944, 0.15).
narrative_ontology:measurement(doll_grid_20, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(individual), 1971, 0.52).
narrative_ontology:measurement(doll_grid_21, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(organizational), 1944, 0.35).
narrative_ontology:measurement(doll_grid_22, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(organizational), 1971, 0.71).
narrative_ontology:measurement(doll_grid_23, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(structural), 1944, 0.48).
narrative_ontology:measurement(doll_grid_24, dollar_gold_convertibility__strict_convertibility_reading, stakes_inflation(structural), 1971, 0.82).
narrative_ontology:measurement(doll_grid_25, dollar_gold_convertibility__strict_convertibility_reading, suppression(class), 1944, 0.25).
narrative_ontology:measurement(doll_grid_26, dollar_gold_convertibility__strict_convertibility_reading, suppression(class), 1971, 0.62).
narrative_ontology:measurement(doll_grid_27, dollar_gold_convertibility__strict_convertibility_reading, suppression(individual), 1944, 0.18).
narrative_ontology:measurement(doll_grid_28, dollar_gold_convertibility__strict_convertibility_reading, suppression(individual), 1971, 0.48).
narrative_ontology:measurement(doll_grid_29, dollar_gold_convertibility__strict_convertibility_reading, suppression(organizational), 1944, 0.32).
narrative_ontology:measurement(doll_grid_30, dollar_gold_convertibility__strict_convertibility_reading, suppression(organizational), 1971, 0.68).
narrative_ontology:measurement(doll_grid_31, dollar_gold_convertibility__strict_convertibility_reading, suppression(structural), 1944, 0.38).
narrative_ontology:measurement(doll_grid_32, dollar_gold_convertibility__strict_convertibility_reading, suppression(structural), 1971, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__strict_convertibility_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% The dollar-gold-convertibility kernel decomposes into three structurally distinct constraint stories, each representing a different reading of Article IV and the IMF Articles. This story (strict_convertibility_reading) treats convertibility as a binding legal obligation; the policy_flexible_reading treats it as subordinate to domestic stability; the triffin_structural_reading treats it as an inherently unsustainable design flaw. Each reading has its own epsilon, beneficiary/victim structure, and timeline. They are not alternative measurements of a single constraint — they are different constraints instantiated from the same contested kernel. All three are linked bidirectionally via this network field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
