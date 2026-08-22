% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar Gold Convertibility as Policy-Flexible Conditional Obligation
 *   domain: international_political_economy/monetary_law
 *
 * SUMMARY:
 *   This constraint instantiates the policy-flexible reading of dollar-gold
 *   convertibility under the Bretton Woods system. The reading holds that the
 *   U.S. commitment to convert dollars to gold at $35/ounce is a conditional
 *   obligation, subordinate to the domestic U.S. priority of maintaining
 *   economic stability (full employment, price stability, financial system
 *   integrity). Under this reading, when domestic U.S. interests and the
 *   convertibility commitment conflict — such as the need to expand the money
 *   supply to fight recession or inflation — the U.S. retains the authority
 *   to reinterpret convertibility's obligations and constraints. The victim
 *   set shifts under this reading: dollar holders and foreign central banks
 *   enter the victim position (bearing devaluation risk and loss of
 *   enforcement authority), while the U.S. exits the victim position
 *   (regaining monetary autonomy). Extractiveness rises over the interval as
 *   the U.S. exercises this flexibility, and suppression intensifies as the
 *   system's formal rules are subordinated to U.S. policy preference. This
 *   reading coexists with two sibling readings: the
 *   strict_convertibility_reading (Article IV as an inviolable binding
 *   obligation) and the triffin_structural_reading (convertibility as an
 *   inherently flawed design). The constraint's type is tangled rope: real
 *   coordination function (settlement without supranational authority) +
 *   active enforcement + asymmetric extraction (U.S. gains autonomy,
 *   foreigners bear devaluation risk).
 *
 * KEY AGENTS:
 *   - United States (monetary authorities): Agenda-setter. Controls the legal interpretation of Article IV, the gold reserve account, and the enforcement machinery. Exercises flexibility to subordinate convertibility to domestic policy.
 *   - Foreign dollar holders and central banks: Payers. Bear devaluation risk when U.S. reinterprets convertibility. Their exit (demanding conversion, diversifying away from dollars) is constrained by the absence of superior alternatives and by U.S. retaliation threats.
 *   - International trading firms: Secondary beneficiary/payer. Benefit from dollar as settlement currency but also bear diffuse cost of dollar instability.
 *   - Other IMF members: Payers. Trapped by IMF membership; forced to accept U.S. reinterpretation of Article IV.
 *   - IMF Articles of Agreement (excluded, non-agent): The formal rule system whose authority is undermined by the flexible reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.62).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.41).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar Gold Convertibility as Policy-Flexible Conditional Obligation").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "international_political_economy/monetary_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'f90b5a09-44c7-446d-b273-cc7369ab9cce').
narrative_ontology:cs_kernel_codification('f90b5a09-44c7-446d-b273-cc7369ab9cce', fixed_text).
narrative_ontology:cs_authority_grounding('f90b5a09-44c7-446d-b273-cc7369ab9cce', extraction).
narrative_ontology:cs_interpretation_layer_present('f90b5a09-44c7-446d-b273-cc7369ab9cce').
narrative_ontology:cs_reading_relation('f90b5a09-44c7-446d-b273-cc7369ab9cce', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('f90b5a09-44c7-446d-b273-cc7369ab9cce', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('f90b5a09-44c7-446d-b273-cc7369ab9cce', foundational, domestic_stability_overrides_external_commitment).
narrative_ontology:cs_axiom_status(domestic_stability_overrides_external_commitment, holdable).
narrative_ontology:cs_axiom_grounding('f90b5a09-44c7-446d-b273-cc7369ab9cce', domestic_stability_overrides_external_commitment, instrumental).
narrative_ontology:cs_axiom('f90b5a09-44c7-446d-b273-cc7369ab9cce', foundational, us_retains_reinterpretation_authority).
narrative_ontology:cs_axiom_status(us_retains_reinterpretation_authority, holdable).
narrative_ontology:cs_axiom_grounding('f90b5a09-44c7-446d-b273-cc7369ab9cce', us_retains_reinterpretation_authority, conventional).
narrative_ontology:cs_reference_frame('f90b5a09-44c7-446d-b273-cc7369ab9cce', article_iv_convertibility_binding_initial_state).
narrative_ontology:cs_drift_state('f90b5a09-44c7-446d-b273-cc7369ab9cce', post_1963_policy_reinterpretation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f90b5a09-44c7-446d-b273-cc7369ab9cce', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, central_banks_with_dollar_reserves).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, international_trading_firms).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, domestic_us_economy).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, international_trading_firms).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, other_bretton_woods_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The U.S. Treasury and Federal Reserve set and enforce the convertibility commitment under Article IV of the IMF Articles of Agreement. They can reinterpret the obligation as conditional on domestic economic stability, allowing them to suspend or adjust convertibility to protect domestic inflation control, employment, and financial system stability. They control the legal frame (Article IV's text), the interpretation authority (Treasury international law office), and the enforcement machinery (gold reserve account, Foreign Exchange Operations Committee). Their exit consists of redefining what 'convertibility' means in practice.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Private investors, international corporations, and individuals holding dollar reserves face devaluation risk when the U.S. exercises the flexibility to subordinate convertibility to domestic policy. They hold dollars for international trade settlement and store-of-value purposes but cannot force conversion if the U.S. redefines its obligations. Their alternatives are limited: diversifying to other currencies (but no perfect substitute exists), reducing dollar holdings (incurring transaction costs), or negotiating bilaterally (but the U.S. sets the conversion rate unilaterally). They bear the cost as a diffuse erosion of purchasing power.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_dollar_holders, payer,
    moderate, biographical, constrained, global).

% Foreign governments holding dollar reserves in their central banks face devaluation risk on their largest reserve asset. Formally, they have the power to demand conversion under Article IV, but practically, demanding large conversions would trigger U.S. retaliation (freezing access to the Federal Reserve banking system, sanctions on financial institutions, diplomatic escalation). They hold dollars because they have no superior alternative for international settlement and emergency liquidity; their 'exit' is theoretically available but structurally prohibited by the threat of exclusive access denial. They bear extraction as hidden devaluation on their reserve holdings and as subordination to U.S. monetary autonomy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, central_banks_with_dollar_reserves, payer,
    powerful, generational, constrained, global).

% Multinational firms conducting international trade in dollars benefit from a widely-accepted medium of exchange and stable reference unit — convertibility signals that dollar claims are anchored to tangible value (gold). They also bear diffuse cost when convertibility subordinates to domestic U.S. policy and the dollar weakens, eroding the real purchasing power of their invoices and contracts. Their exit is limited: they need a settlement currency and have no practical alternative to the dollar.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, international_trading_firms, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, international_trading_firms, payer).

% IMF member countries (particularly those without significant reserve currencies) are structurally bound by the IMF charter and Article IV to respect the dollar-gold parity as the system's numeraire. When the U.S. reinterprets convertibility as conditional on its domestic policy, these members lose the option to hold the U.S. accountable via Article IV enforcement; they must accept the U.S. reinterpretation or exit the system entirely (which is prohibitively costly). Their exit is trapped: the alternative is bilateral arrangements outside the IMF, which carry higher transaction costs and less liquidity.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, other_bretton_woods_members, payer,
    organized, generational, trapped, global).

% The U.S. domestic economy benefits from monetary policy autonomy — the ability to expand the money supply, lower interest rates, and run deficits without the constraint of having to maintain a fixed gold conversion rate. This flexibility enables counter-cyclical fiscal and monetary response during recessions, inflation control independent of external demand for dollars, and financing of domestic programs without external pressure. The benefit is collective and diffuse.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, domestic_us_economy, beneficiary,
    institutional, generational, analytical, national).

% The IMF Articles of Agreement, particularly Article IV, codify convertibility as a binding obligation. Under this reading, the Articles themselves are superseded by U.S. reinterpretation; the rule system is present in the constraint as a background institution whose formal authority is undermined by the flexible reading. The IMF would have a voice if enforcement of Article IV against the U.S. were possible, but it is not.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, imf_rules_system, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__policy_flexible_reading, imf_rules_system).

% The doctrine that monetary systems should anchor to tangible value is an excluded voice in this reading. The policy-flexible reading treats gold convertibility as a means to an end (international stability), not as a constraint on policy autonomy. Ideological defenders of the gold standard would argue that the constraint should be inviolable; they are not present in this reading's stakeholder frame.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, gold_standard_ideology, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(dollar_gold_convertibility__policy_flexible_reading, gold_standard_ideology).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_authorities).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Dollar-gold convertibility solves the problem of international currency settlement without a supranational authority: foreign central banks and traders know their dollar holdings can be converted to tangible value (gold), which disciplines the U.S. from over-issuing currency and creates a common reference for exchange rates across trading partners.
% TRANSFER_FUNCTION: Transfers monetary policy autonomy from the U.S. government to the constraint of maintaining gold reserves and honoring conversion requests. Under this reading, the U.S. reinterprets the transfer: it retains autonomy by making convertibility conditional on domestic stability priorities, transferring the devaluation risk instead to dollar holders and foreign central banks.
% ABSENT_VOICES: Future generations (who will inherit the consequences of devaluation), gold-standard ideological defenders, and non-state actors in the trading system who lack institutional power to voice objections to the U.S. reinterpretation of its obligations. Also structurally excluded: the IMF's formal dispute-resolution machinery (it lacks enforcement against the U.S.) and alternative monetary systems advocates (they would argue for decentralized settlement mechanisms, but are not seated in the Bretton Woods negotiation).
% DISAPPEARANCE_RATIONALE: If the flexible reading disappeared and strict convertibility were restored and enforced, the U.S. would lose monetary autonomy, expansionary policy would be constrained by gold reserve holdings, inflation control would depend on foreign central banks' willingness to hold dollars, and the U.S. domestic economy would rearrange around the scarcity of fiat expansion. Alternatively, if convertibility itself were abandoned entirely (the system collapsed), the world would rearrange toward alternative settlement mechanisms (bilateral clearing, alternative reserve currencies, or decentralized digital currencies). The flexible reading's disappearance removes the U.S. escape valve — something rearranges in either direction.
% FOUNDING_PROBLEM: After World War II, the international economy needed a trusted settlement mechanism for cross-border trade and a reserve asset that could serve as a common numeraire across national currencies. Gold provided tangibility (countries could trust it would not be inflated away). The dollar provided liquidity (abundant, fungible, widely accepted). Combining them — dollar convertibility into gold at a fixed price — solved the coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: U.S. policymakers from Truman through Kennedy attested the founding problem was live: rapid rebuilding of war-torn economies required trust in international settlement, and gold-backed dollars provided that trust. Keynes and international economists attested to the coordination function. However, by the 1960s, the founding problem had shifted: the U.S. faced the Triffin dilemma (the more dollars circulated internationally, the more claims on U.S. gold existed), suggesting the founding solution was becoming its own problem. By the late 1960s, critics and foreign policymakers (particularly European central bankers) attested the founding problem had been solved but the constraint was now generating new problems — excessive U.S. monetary autonomy was creating dollar instability, not stability. The French government under de Gaulle explicitly challenged the arrangement, calling for a return to a true gold standard. U.S. policymakers reframed: they attested that domestic economic stability (preventing recessions, managing inflation) became the primary problem, which convertibility threatened. This reframing itself is the flexible reading.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness starts low (0.15 in 1944) because the system is new and symmetric: the U.S. is genuinely bound to convertibility and dollar holders believe the commitment is credible. By 1950, as U.S. gold reserves decline (Eisenhower Doctrine spending, Cold War military expenditure) and dollar inflation accelerates, extractiveness rises to 0.28 — the U.S. begins reinterpreting its obligations and foreign holders realize the binding commitment is being softened. By 1957, the British Suez crisis and European central bank demands for gold demonstrate the constraint is straining; extractiveness reaches 0.45. By 1963-1971, as the U.S. runs persistent deficits and the London Gold Pool attempts to manage conversion demand, extractiveness climbs to 0.62 — the U.S. openly reinterprets convertibility as conditional, effectively extracting seigniorage from dollar holdings. Theater ratio rises in parallel: initially the conversion mechanism is genuinely functional (1944-1950), but by the 1960s, the U.S. increasingly uses administrative measures (interest equalization tax, capital controls, gold pool operations) that are performative — they create the appearance of managing convertibility while actually suspending it in practice. Suppression rises as the U.S. constrains foreign central banks' options: they cannot demand large conversions without triggering exclusion from Federal Reserve credit lines, and the IMF cannot enforce Article IV against the largest member. Resistance stays high (0.68 average) because foreign governments and traders continuously challenge the reinterpretation, formally demand conversion, and eventually abandon the system. The temporal trajectory models a constraint whose real coordination function remains (trades still settle in dollars) but whose extraction function expands as the U.S. exercises flexibility.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (U.S. Treasury) perceives the constraint as a live coordination function with justified flexibility — domestic stability requires monetary autonomy, and subordinating convertibility to this priority is a rational adaptation, not extraction. The payer seats (foreign central banks, dollar holders) perceive the same constraint as a bait-and-switch: Article IV was sold as an inviolable commitment, but U.S. practice reveals it was always conditional on U.S. preference. The engine should compute this gap: from the U.S. institutional seat, directionality near 0.0 (beneficiary, autonomy-gaining); from the foreign-central-bank seat, directionality near 1.0 (target, losing enforcement authority and bearing devaluation). From the trading-firm seat (moderate power), directionality near 0.5 (genuine beneficiary from settlement, but also bearing hidden cost). The divergence in computed type is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. monetary authorities are the structural beneficiary: they gain the freedom to run policy independently, finance deficits by issuing currency, and export devaluation risk to dollar holders. Their exit options are arbitrage-grade (they can simply redefine the obligation or exit the system entirely — by 1971, they chose to do exactly that). Foreign central banks are trapped: they depend on the dollar for international settlement and emergency liquidity; demanding conversion triggers exclusion; holding dollars exposes them to devaluation. Exit options are formally 'constrained' but practically 'trapped' (the IMF system offers no alternative, and bilateral arrangements are inferior). Foreign dollar holders face similar dynamics: constrained by the absence of a substitute. Dollar traders are organized and powerful but constrained: they need the settlement currency and cannot realistically demand an alternative. The U.S. domestic economy (collective beneficiary) gains from monetary autonomy. The directionality divergence — U.S. near 0.0 (beneficiary), foreign central banks near 1.0 (target) — is the core asymmetry this reading models.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (international settlement without supranational authority) is live through 1950 but contested by 1963 and dead by 1968. By the late 1960s, the system has solved its original coordination problem — dollars are universally accepted, exchange rates are stable through parity commitment, trade flourishes. But the constraint persists because the U.S. has begun extracting seigniorage via the reinterpretation. A dead founding problem + persistent constraint = mandatrophy signal. This reading explicitly models mandatrophy resolution: the U.S. reinterprets the founding problem as 'internal stability' rather than 'international coordination,' which allows the constraint to persist as extraction. The flexibility reading IS the mandatrophy avoidance mechanism — by redefining what convertibility means, the U.S. allows the constraint to persist despite the founding coordination problem being solved. This is the distinctive feature of this reading: other readings (strict convertibility, Triffin structural) would classify the constraint as broken or unsustainable once the founding problem dies; the flexible reading extends its life by reinterpreting its purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_stability_priority_grounding,
    'Is the U.S. authority to subordinate convertibility to domestic stability derived from the IMF Articles themselves (as a legitimate reading of ''conditionality''), or is it an invented doctrine that contradicts Article IV''s plain binding language?',
    'Comparative textual analysis of Article IV preamble and enforcement history; legal scholarship on the original negotiators'' intent regarding the conditionality clause; examination of whether the Articles contain an explicit ''domestic stability override'' or whether the U.S. simply asserts one.',
    'If grounded in the Articles'' language, this reading is a defensible interpretation and the constraint is legitimately flexible. If invented, the reading is a false-summit reading of the Articles and the constraint is a snare (pure extraction masked by legal reinterpretation). The classification would shift from tangled_rope to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_stability_priority_grounding, conceptual, 'Whether the flexible reading is authorized by the IMF Articles or is an extra-legal reinterpretation.').

omega_variable(
    foreign_holder_exit_trap_mechanism,
    'Are foreign central banks genuinely trapped by structural dependence on the dollar, or do they possess realistic alternative settlement mechanisms (bilateral clearing, alternative currencies, decentralized systems) that the U.S. suppresses?',
    'Historical analysis of central bank requests for alternative arrangements and U.S. responses; examination of whether bilateral clearing, European Payments Union successors, or gold-backed alternatives were available but rejected; counterfactual: what happens if a coalition of central banks exits the dollar system.',
    'If alternatives exist but are suppressed, the exit_options for foreign central banks should be downrated from ''constrained'' to ''identity_locked'' or ''trapped,'' raising their d values and extractiveness. If alternatives do not exist, the trap is structural (the dollar is genuinely the only option), confirming exit_options=''trapped'' and validating the measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_holder_exit_trap_mechanism, empirical, 'Whether the foreign dollar holder exit trap is structural (no alternatives) or suppressed (alternatives exist but are blocked by the U.S.).').

omega_variable(
    extracted_seigniorage_measurement,
    'How much seigniorage did the U.S. extract via the flexible reinterpretation of convertibility — i.e., what is the dollar value of currency issued beyond the gold reserve base, distributed to whom, and realized by whom?',
    'Monetary history accounting of U.S. money supply, gold reserves, and currency creation 1944-1971; analysis of where the seigniorage accrued (U.S. Treasury, Federal Reserve, private financial sector); comparison to counterfactual of strict convertibility constraints.',
    'A large measured seigniorage confirms the extractiveness trajectory (rising from 0.15 to 0.62). A small or unmeasurable seigniorage would suggest the constraint operated closer to coordination (reinterpretation was genuine adaptation, not rent capture). The measurement also disambiguates whether extraction was incidental (an unfortunate side-effect of flexibility) or central (the primary motivation for reinterpreting the constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extracted_seigniorage_measurement, empirical, 'Quantifying the extraction: seigniorage extracted via flexible reinterpretation, 1944-1971.').

omega_variable(
    sibling_reading_coexistence_or_foreclosure,
    'Do the policy_flexible_reading and the strict_convertibility_reading genuinely coexist as competing live positions in the Bretton Woods negotiation and practice, or does the flexible reading logically foreclose the strict reading by redefining what convertibility means?',
    'Document the positions of the U.S., UK, and other negotiating parties at Bretton Woods 1944: did they explicitly assert conditional vs. inviolable interpretations, or did both interpretations emerge later? Examine whether the flexible reading is a later innovation that retroactively reinterprets the Articles, which would constitute foreclosure via text revision rather than coexistence.',
    'If coexistence: the readings occupy different parties'' positions (U.S. asserts flexible, foreign governments assert strict), and the engine should register in_contention/3 between the seats. If foreclosure: the flexible reading''s redefinition of ''convertibility'' as ''conditional'' logically rules out the strict reading''s claim that it is ''inviolable,'' and the readings should register in reading_relations as forecloses, not coexists_with. This affects the network topology between the sibling constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_or_foreclosure, conceptual, 'Whether the flexible and strict readings coexist or whether the flexible reading forecloses the strict reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t1944, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1944, 0.08).
narrative_ontology:measurement_basis(doll_tr_t1944, observed).
narrative_ontology:measurement(doll_tr_t1950, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(doll_tr_t1950, observed).
narrative_ontology:measurement(doll_tr_t1957, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1957, 0.18).
narrative_ontology:measurement_basis(doll_tr_t1957, observed).
narrative_ontology:measurement(doll_tr_t1963, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1963, 0.24).
narrative_ontology:measurement_basis(doll_tr_t1963, observed).
narrative_ontology:measurement(doll_tr_t1968, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1968, 0.28).
narrative_ontology:measurement_basis(doll_tr_t1968, observed).
narrative_ontology:measurement(doll_tr_t1971, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 1971, 0.29).
narrative_ontology:measurement_basis(doll_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(doll_be_t1944, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement_basis(doll_be_t1944, observed).
narrative_ontology:measurement(doll_be_t1950, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1950, 0.28).
narrative_ontology:measurement_basis(doll_be_t1950, observed).
narrative_ontology:measurement(doll_be_t1957, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1957, 0.45).
narrative_ontology:measurement_basis(doll_be_t1957, observed).
narrative_ontology:measurement(doll_be_t1963, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1963, 0.55).
narrative_ontology:measurement_basis(doll_be_t1963, observed).
narrative_ontology:measurement(doll_be_t1968, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement_basis(doll_be_t1968, observed).
narrative_ontology:measurement(doll_be_t1971, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 1971, 0.62).
narrative_ontology:measurement_basis(doll_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t1944, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1944, 0.12).
narrative_ontology:measurement_basis(doll_su_t1944, observed).
narrative_ontology:measurement(doll_su_t1950, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement_basis(doll_su_t1950, observed).
narrative_ontology:measurement(doll_su_t1957, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1957, 0.32).
narrative_ontology:measurement_basis(doll_su_t1957, observed).
narrative_ontology:measurement(doll_su_t1963, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1963, 0.38).
narrative_ontology:measurement_basis(doll_su_t1963, observed).
narrative_ontology:measurement(doll_su_t1968, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement_basis(doll_su_t1968, observed).
narrative_ontology:measurement(doll_su_t1971, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 1971, 0.41).
narrative_ontology:measurement_basis(doll_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(dollar_gold_convertibility__policy_flexible_reading, 0.18).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility__triffin_structural_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, bretton_woods_system_stability).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, seigniorage_extraction_via_reserve_currency).

% DUAL FORMULATION NOTE:
% This constraint is one reading (policy_flexible_reading) of the contested dollar_gold_convertibility kernel. The kernel decomposes into three structurally distinct constraints, each with its own ε, beneficiary/victim structure, and type. The policy_flexible_reading asserts that U.S. domestic stability priority overrides the binding convertibility commitment, shifting extraction from internal U.S. policy constraint to external creditors. The strict_convertibility_reading asserts that Article IV creates an inviolable obligation. The triffin_structural_reading asserts the system is inherently flawed. These three readings share a referent (the Bretton Woods convertibility arrangement) but have different ε values (low for strict, high for flexible and Triffin) and different victim sets. They are linked via network.affects_constraints and should be read as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__policy_flexible_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
