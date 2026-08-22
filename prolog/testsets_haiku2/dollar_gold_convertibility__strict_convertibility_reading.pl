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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: dollar_gold_convertibility__strict_convertibility_reading
 *   human_readable: Article IV Convertibility as Binding Legal Obligation (Strict Reading)
 *   domain: international_political_economy
 *
 * SUMMARY:
 *   Under the strict convertibility reading of Article IV, the United States
 *   is bound by enforceable legal obligation to convert dollars to gold at a
 *   fixed parity ($35 per ounce). This reading treats the Bretton Woods
 *   obligation not as a policy choice or conditional promise, but as a
 *   constitutional constraint on U.S. monetary sovereignty. From this
 *   vantage, creditor nations and international reserve holders possess a
 *   binding claim on U.S. gold reserves, and the Federal Reserve's domestic
 *   policy space is structurally subordinated to maintaining convertibility.
 *   The constraint emerges as a tangled rope: genuine international
 *   coordination function (fixed-rate system for postwar recovery) coupled
 *   with asymmetric extraction (U.S. domestic policy subordinated to
 *   international reserve defense, benefiting creditor nations with
 *   enforceable claims). The measurement series track the accumulating
 *   strain: extractiveness rises as gold outflows accelerate and the policy
 *   constraint tightens; suppression requirement grows as maintaining the
 *   fiction of convertibility demands ever-stronger defense; theater ratio
 *   remains low (the obligation is functionally binding, not yet purely
 *   performative) but increases modestly as the system's fragility becomes
 *   apparent and rhetorical justification grows more elaborate.
 *
 * KEY AGENTS:
 *   - U.S. Federal Reserve: constrained issuer bearing the structural obligation to maintain gold reserves and defend parity
 *   - Creditor nations (France, Germany, others): beneficiaries with enforceable redemption rights and leverage over U.S. policy
 *   - U.S. domestic economy: indirect victim, experiencing tight-money bias when gold outflows force policy tightening
 *   - Private gold traders: arbitrage beneficiaries of the price-control gap
 *   - Bretton Woods authority: agenda-setter administering the obligation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__strict_convertibility_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__strict_convertibility_reading, 0.72).
domain_priors:theater_ratio(dollar_gold_convertibility__strict_convertibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__strict_convertibility_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__strict_convertibility_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__strict_convertibility_reading, "Article IV Convertibility as Binding Legal Obligation (Strict Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__strict_convertibility_reading, "international_political_economy").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__strict_convertibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__strict_convertibility_reading, 'cca4d091-da85-4573-89f7-2440361a4466').
narrative_ontology:cs_kernel_codification('cca4d091-da85-4573-89f7-2440361a4466', fixed_text).
narrative_ontology:cs_authority_grounding('cca4d091-da85-4573-89f7-2440361a4466', lineage).
narrative_ontology:cs_interpretation_layer_present('cca4d091-da85-4573-89f7-2440361a4466').
narrative_ontology:cs_reading_relation('cca4d091-da85-4573-89f7-2440361a4466', dollar_gold_convertibility__policy_flexible_reading, coexists_with).
narrative_ontology:cs_reading_relation('cca4d091-da85-4573-89f7-2440361a4466', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('cca4d091-da85-4573-89f7-2440361a4466', foundational, article_iv_creates_binding_legal_obligation).
narrative_ontology:cs_axiom_status(article_iv_creates_binding_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cca4d091-da85-4573-89f7-2440361a4466', article_iv_creates_binding_legal_obligation, conventional).
narrative_ontology:cs_axiom('cca4d091-da85-4573-89f7-2440361a4466', secondary, fixed_parity_sustains_indefinitely).
narrative_ontology:cs_axiom_status(fixed_parity_sustains_indefinitely, holdable).
narrative_ontology:cs_axiom_grounding('cca4d091-da85-4573-89f7-2440361a4466', fixed_parity_sustains_indefinitely, empirically_contingent).
narrative_ontology:cs_reference_frame('cca4d091-da85-4573-89f7-2440361a4466', binding_postwar_monetary_law).
narrative_ontology:cs_drift_state('cca4d091-da85-4573-89f7-2440361a4466', mid_system_maturity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cca4d091-da85-4573-89f7-2440361a4466', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, international_reserve_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_policy_space).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, federal_reserve_discretion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, private_gold_traders).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__strict_convertibility_reading, reserve_currency_dependents).
narrative_ontology:constraint_victim(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legally bound under Article IV to convert dollars to gold at a fixed parity (originally $35/oz). Must maintain sufficient gold reserves to honor conversion claims from foreign governments and central banks. This obligation constrains domestic monetary policy — cannot freely expand money supply without threatening gold reserves depletion. Faces recurring pressure from gold outflows and must subordinate expansionary policy to reserve defense.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_federal_reserve, payer,
    institutional, generational, constrained, global).

% Hold dollar reserves as backing for their own currencies and can redeem dollars for gold at the fixed parity on demand. Gain a binding legal claim on U.S. gold reserves and the security of dollar-denominated assets. Can convert dollars to gold when confidence in the dollar weakens, creating leverage over U.S. policy. France and other nations periodically exercise this option to pressure U.S. policy concessions.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations, beneficiary,
    organized, generational, arbitrage, global).

% Experiences monetary policy subordinated to gold-reserve defense rather than optimal domestic conditions. When gold outflows accelerate, the Federal Reserve must tighten credit and raise interest rates to preserve reserves, even when domestic unemployment and growth would warrant expansion. This trade-off is structurally enforced by the legal obligation to convert at fixed parity.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__strict_convertibility_reading, us_domestic_economy).

% Profit from arbitrage between the official fixed price ($35/oz) and market prices. As the strain increases and gold outflows accelerate, private speculators and traders gain arbitrage opportunities by moving gold between London and official channels, accumulating private gold holdings at the artificially constrained price.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, private_gold_traders, beneficiary,
    moderate, biographical, mobile, global).

% Administers the convertibility obligation and its enforcement through Articles of Agreement. Can theoretically alter terms but in practice Article IV is treated as inviolable constitutional law of the system. Monitoring body for the strict reading of the obligation; maintains pressure on the U.S. to honor commitments.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, bretton_woods_authority, agenda_setter,
    institutional, generational, analytical, global).

% Holds formal power to alter U.S. participation in Bretton Woods, but politically constrained by the postwar commitment to multilateral institutions and Cold War alliance imperatives. Would object to the convertibility obligation's strictness in constraining fiscal and monetary freedom, but exit is politically infeasible given geopolitical stakes.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, us_congress, excluded,
    institutional, generational, constrained, national).

% Nations whose own currencies lack reserve status depend on dollar reserves for international trade settlement and balance-of-payments stabilization. The gold-backed dollar provides confidence and stability for this arrangement, though they must accept the dollar's constraints as structural limits on their own monetary freedom.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, reserve_currency_dependents, beneficiary,
    moderate, generational, constrained, global).

% Examines the constraint's structural effects on the international monetary system and U.S. domestic policy, noting the zero-sum dynamic: creditor-nation leverage increases as U.S. gold declines, forcing ever-tighter policy to defend reserves.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__strict_convertibility_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__strict_convertibility_reading, creditor_nations).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__strict_convertibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article IV convertibility at fixed parity stabilizes international exchange rates and provides a common numeraire for trade settlement: all nations peg to gold through the dollar, eliminating currency speculation and providing the monetary infrastructure for postwar multilateral trade recovery.
% TRANSFER_FUNCTION: Transfers control over U.S. domestic monetary policy to the constraint of maintaining a fixed gold parity. Transfers real economic value as foreign governments exercise conversion rights, depleting U.S. gold reserves. Transfers monetary policy discretion from Federal Reserve to the international obligation, converting domestic policy space into international claims.
% ABSENT_VOICES: Domestic U.S. labor unions, small farmers, and growth-dependent constituencies would object to the tight-money bias imposed by reserve defense, but they are excluded from international monetary negotiations. Only foreign governments and the Federal Reserve's institutional preferences are represented in the forum.
% DISAPPEARANCE_RATIONALE: If Article IV convertibility and its binding force vanished, the Federal Reserve would immediately adopt expansionary policy unconstrained by gold reserves, dollar exchange rates would float, other nations would either redenominate their currencies or manage independent floats, and the postwar fixed-rate international monetary system would collapse. The entire institutional architecture of Bretton Woods depends on this obligation's enforceability.
% FOUNDING_PROBLEM: After World War II, the international monetary system lay in chaos: currency values were undefined, trade was impossible without bilateral barter, and speculation destroyed any attempt at fixed rates. The strict convertibility obligation was designed to anchor the system: the U.S. gold backing the dollar provided a permanent anchor, making the dollar 'as good as gold' and enabling every other nation to peg to it with confidence.
% FOUNDING_PROBLEM_CORROBORATION: The Bretton Woods framers (from outside the U.S. domestic beneficiary set) attested that currency instability was the binding problem and that U.S. gold backing was the only solution they could construct with postwar credibility. Economists and policymakers outside the U.S. Federal Reserve (Keynes, Harrod, later critics from creditor nations) confirm the founding problem was real but increasingly dispute whether strict convertibility remains necessary after the system stabilized. U.S. domestic policymakers (administration, Congress) attest the problem is solved but the obligation now constrains policy; French officials attest the obligation remains live leverage. Independent economic historians document both the founding rationale and the later mounting strain.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__strict_convertibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__strict_convertibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__strict_convertibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__strict_convertibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__strict_convertibility_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises monotonically (0.35 → 0.78) as the system matures and the constraint's binding force becomes more evident. Early in the interval, the U.S. is still wealthy in gold reserves and the obligation feels coordinating; by the end, U.S. gold reserves are depleting and every policy choice is filtered through reserve defense. Suppression requirement tracks this: the Federal Reserve must actively defend the parity through tightening, foreign-exchange controls, and rhetorical commitment statements. Theater ratio remains relatively low (0.08 → 0.28) because the constraint is genuinely binding and functionally enforced; the rising curve reflects increasing rhetorical elaboration as the system approaches breaking point. Accessibility collapse is moderate (0.62): alternatives (floating rates, devaluation, exit from Bretton Woods) exist theoretically but are politically infeasible given Cold War alliance structure and postwar commitment to multilateralism. Resistance is substantial (0.71): the Federal Reserve and U.S. policymakers actively resist the constraint's domestic policy costs and explore workarounds (Roosa bonds, swap arrangements), but the legal obligation holds.
 *
 * PERSPECTIVAL GAP:
 *   From the Federal Reserve's institutional seat, the constraint is increasingly experienced as extraction: reserve defense trumps growth, employment, and fiscal space, and the burden is concentrated on the U.S. issuer. From creditor nations' seats, the constraint is a binding legal asset—convertibility provides reserve security and leverage. From the U.S. Congress and domestic growth constituencies (excluded), the constraint is an arbitrary subordination of domestic welfare to international obligations. From the international monetary authority's seat, the constraint is the foundation of systemic stability and must be maintained at all costs. The engine should compute these divergences from the structural data—they are not authorial claims but emergent consequences of asymmetric beneficiary/victim positions and differential exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. Federal Reserve and U.S. domestic policy enter the victim/payer set (high d, near 1.0) because their discretion is constrained by the binding obligation to maintain convertibility and gold reserves. Creditor nations occupy beneficiary seats (low d, near 0.0) because they hold enforceable claims and experience the constraint as leverage over U.S. policy, not as constraint on their own. Private gold traders sit near beneficiary (d ≈ 0.1) because they profit from the price-control gap. The Bretton Woods authority administers the obligation and is relatively insulated from its costs (d ≈ 0.3), though maintaining an increasingly strained system becomes administratively burdensome. U.S. Congress is excluded from the room where these decisions bind because they were made at Bretton Woods without legislative ratification in the strict legal sense; domestic political economy would shift the reading if Congress were seated.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading deliberately avoids mandatrophy resolution. The strict reading holds that Article IV's legal language creates a binding obligation that persists regardless of whether the founding problem (postwar currency chaos) remains live. The founding problem is contested: creditor nations and the Bretton Woods authority say currency stability is a permanent good requiring permanent obligation; U.S. policymakers say the problem is solved and the obligation now imposes only costs. The constraint is NEITHER a mountain (the obligation is human-made, not natural law) NOR a fully extractive snare (genuine coordination value is present, even if asymmetrically distributed). Tangled rope captures this: real coordination function (fixed-rate system) married to asymmetric extraction (U.S. policy subordination). The rising extractiveness series and the suppression trajectory reflect the system's drift toward instability—as U.S. gold reserves decline, the constraint's binding force paradoxically increases (because maintaining convertibility becomes harder, suppression requirement rises), even as the coordination rationale weakens. This is diagnostic of tangled rope under strain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_bindingness_vs_policy_flexibility,
    'Is Article IV convertibility truly binding in law, or is it a conditional commitment that the U.S. can suspend when domestic priorities require it?',
    'Historical test: does the U.S. actually suspend convertibility when economic strain becomes severe, or does it defend the obligation at domestic cost? If it suspends, the commitment was conditional (policy_flexible_reading wins). If it maintains despite strain until system collapse, the binding reading holds until the moment of rupture.',
    'If bindingness is genuine, the constraint is tangled rope (coordination + extraction) with U.S. as victim. If convertibility is conditional on domestic stability, the constraint weakens from tangled rope toward rope (coordination without binding extraction). The engine computes type from structural data; this omega flags the reading-dependence of the structural interpretation itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_bindingness_vs_policy_flexibility, conceptual, 'The kernel''s core ambiguity: does the formal legal text create binding obligation or conditional promise?').

omega_variable(
    sustainabilityof_fixed_parity,
    'Can the strict convertibility obligation be indefinitely maintained, or does it inevitably collapse as U.S. gold reserves deplete and the U.S. cannot simultaneously maintain the role of world banker and full convertibility?',
    'Empirical progression: track gold reserve depletion over time and observe whether policy adjustments (Roosa bonds, swap agreements, capital controls) delay or accelerate the terminal moment. The Triffin dilemma predicts fundamental unsustainability; if the system collapses despite all policy defenses, the structural reading gains weight.',
    'If the constraint is genuinely sustainable, it is binding law constraining policy. If it is structurally unsustainable despite legal form, the constraint is a zombie institution—performing bindingness while marching toward inevitable rupture. The theater ratio would then rise sharply near collapse as defensive rhetoric intensifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainabilityof_fixed_parity, empirical, 'Whether strict convertibility is a stable policy constraint or a doomed institutional fiction.').

omega_variable(
    creditor_nation_enforceability,
    'Do creditor nations genuinely possess the power to enforce Article IV convertibility claims, or is enforcement capacity illusory and the obligation only persists by U.S. choice?',
    'Examine instances of conversion demand by creditor nations (France''s 1965 conversion of dollars to gold) and the U.S. response: does the U.S. honor conversion demands at cost, or does it find ways to delay and eventually restrict them? If honored despite policy pain, enforcement is real. If restricted or delayed, enforcement capacity is illusory.',
    'If enforcement is real, creditor nations are genuine beneficiaries with leverage, and the constraint is extractive from the U.S. perspective. If enforcement is illusory (U.S. can refuse conversion whenever it chooses), the constraint is rhetorical rather than binding, and extractiveness is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creditor_nation_enforceability, empirical, 'Whether creditor-nation claims on convertibility are enforceable or merely asserted.').

omega_variable(
    reading_identity_in_kernel,
    'Is the strict convertibility reading coherently distinct from the policy_flexible reading, or do they collapse into the same structural analysis under scrutiny?',
    'Comparative analysis: build the policy_flexible_reading as a separate constraint story and examine whether the two readings generate different metrics, different beneficiary/victim assignments, and different type classifications. If they do, they are genuinely distinct readings of the kernel; if metrics converge, the reading distinction is rhetorical, not structural.',
    'If the readings are genuinely distinct (different ε, different victim sets), the kernel properly instantiates multiple constraints and the network linking strategy is justified. If readings collapse into one, the kernel concept is premature or the reading distinction is not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_in_kernel, conceptual, 'Whether the strict vs. flexible readings are structurally distinct constraint instantiations or rhetorical variants of a single underlying structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__strict_convertibility_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dgc_strict_tr_t0, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(dgc_strict_tr_t3, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 3, 0.11).
narrative_ontology:measurement(dgc_strict_tr_t6, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement(dgc_strict_tr_t10, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(dgc_strict_tr_t15, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement(dgc_strict_tr_t20, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(dgc_strict_tr_t25, dollar_gold_convertibility__strict_convertibility_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(dgc_strict_be_t0, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dgc_strict_be_t3, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(dgc_strict_be_t6, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(dgc_strict_be_t10, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(dgc_strict_be_t15, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(dgc_strict_be_t20, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(dgc_strict_be_t25, dollar_gold_convertibility__strict_convertibility_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dgc_strict_su_t0, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dgc_strict_su_t3, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(dgc_strict_su_t6, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(dgc_strict_su_t10, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(dgc_strict_su_t15, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(dgc_strict_su_t20, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(dgc_strict_su_t25, dollar_gold_convertibility__strict_convertibility_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__strict_convertibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__strict_convertibility_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__strict_convertibility_reading, dollar_gold_convertibility__triffin_structural_reading).

% DUAL FORMULATION NOTE:
% The dollar_gold_convertibility kernel admits three structurally distinct readings: strict_convertibility_reading (this story, high extraction from U.S. perspective, binding legal obligation), policy_flexible_reading (convertibility as conditional on domestic stability, lower extraction), and triffin_structural_reading (convertibility as inherently unsustainable design flaw, highest extractiveness but for structural reasons rather than enforcement). Each reading instantiates a different constraint with different ε, different beneficiary/victim assignments, and different terminal dynamics. They share the kernel (Article IV) but diverge on what the commitment MEANS and what happens when strain emerges. Link all three via affects_constraints to enable the engine's contamination propagation to track how destabilization in one reading cascades to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__strict_convertibility_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
