% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Repudiation Reading of the Versailles Reparations Clauses — Nullified Creditor Claims
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   A reading-level constraint story. The repudiation reading of the
 *   Versailles reparations clauses instantiates an arrangement in which the
 *   treaty's payment obligations are void from the outset — procured by
 *   blockade, starvation, and a five-day ultimatum — and Germany owes nothing
 *   beyond voluntary token gestures. Under this arrangement the entire
 *   war-cost residue lands on the creditor side: French and Belgian
 *   reconstruction borrowing, veteran and widow pensions, and municipal
 *   rebuilding debts sized to expected German transfers become unrecoverable,
 *   while the freed German fiscal capacity flows to domestic spending and,
 *   decisively after 1933, rearmament. The arrangement's juridical surface
 *   (consent-based obligation, the wrongfulness of diktat) covers a coerced
 *   claim-nullification whose persistence depended on German power growth and
 *   the collapse of enforcement coalitions. This is one member of a
 *   three-story constraint family decomposing the colloquial label
 *   'Versailles reparations'; the family links and epsilon differences are
 *   recorded in network.dual_formulation_note and the kernel-context omega.
 *   KEY AGENTS (by structural relationship): - german_revisionist_government:
 *   agenda-setter and principal beneficiary (institutional/arbitrage) —
 *   declares the instrument void, administers token gestures, converts fiscal
 *   headroom into rearmament - german_state_treasury: receipt seat
 *   (institutional/mobile) — receives the flows that would have serviced the
 *   claims - german_industrial_complex: secondary beneficiary
 *   (powerful/constrained) — procurement demand and protected markets from
 *   halted transfers - german_nationalist_movement: identity-fused
 *   beneficiary (organized/identity_locked) — draws legitimacy from denial of
 *   the instrument's validity - allied_creditor_states: primary target
 *   (institutional/trapped) — France and Belgium carrying unrecoverable
 *   claims and pension obligations - french_belgian_war_damage_claimants:
 *   diffuse target (powerless/trapped) — households, municipalities,
 *   pensioners owed compensation - american_interallied_debt_holders:
 *   excluded creditor (institutional/constrained) — outside the settlement
 *   conference; claims die with the inter-allied chain -
 *   treaty_law_regime_scholars: analytical observer — sees the full structure
 *   across seats
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.85).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.75).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Repudiation Reading of the Versailles Reparations Clauses — Nullified Creditor Claims").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '7910cca8-5304-4b72-816a-9ffe6576c7ef').
narrative_ontology:cs_kernel_codification('7910cca8-5304-4b72-816a-9ffe6576c7ef', fixed_text).
narrative_ontology:cs_authority_grounding('7910cca8-5304-4b72-816a-9ffe6576c7ef', practice).
narrative_ontology:cs_interpretation_layer_present('7910cca8-5304-4b72-816a-9ffe6576c7ef').
narrative_ontology:cs_reading_relation('7910cca8-5304-4b72-816a-9ffe6576c7ef', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('7910cca8-5304-4b72-816a-9ffe6576c7ef', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_axiom('7910cca8-5304-4b72-816a-9ffe6576c7ef', foundational, duress_vitiates_treaty_consent).
narrative_ontology:cs_axiom_status(duress_vitiates_treaty_consent, holdable).
narrative_ontology:cs_axiom_grounding('7910cca8-5304-4b72-816a-9ffe6576c7ef', duress_vitiates_treaty_consent, deontological).
narrative_ontology:cs_axiom('7910cca8-5304-4b72-816a-9ffe6576c7ef', secondary, article_231_carries_no_obligating_force).
narrative_ontology:cs_axiom_status(article_231_carries_no_obligating_force, holdable).
narrative_ontology:cs_axiom_grounding('7910cca8-5304-4b72-816a-9ffe6576c7ef', article_231_carries_no_obligating_force, conventional).
narrative_ontology:cs_reference_frame('7910cca8-5304-4b72-816a-9ffe6576c7ef', void_diktat_instrument).
narrative_ontology:cs_drift_state('7910cca8-5304-4b72-816a-9ffe6576c7ef', post_lausanne_rearmament_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7910cca8-5304-4b72-816a-9ffe6576c7ef', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_state_treasury).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_industrial_complex).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_nationalist_movement).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, french_belgian_war_damage_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, american_interallied_debt_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_revisionist_government).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, duress_vitiates_consent_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, war_guilt_clause_nullity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares the treaty void from the outset, publishes the token-gesture policy, and directs the fiscal headroom into domestic programs and rearmament contracts. Engages treaty frameworks opportunistically — signing Locarno, taking the League seat — while withholding performance on the cancelled obligations. Its exit is asymmetric: it can step back into diplomatic frameworks whenever useful, since it owes nothing that courts or councils can attach.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_revisionist_government, agenda_setter,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, german_revisionist_government, beneficiary).

% Receives the budgetary flows that would otherwise have serviced the payment schedule and redirects them at will; no counterclaim attaches to its accounts. Its planning horizon is the annual budget cycle extended by long-run rearmament programs.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state_treasury, beneficiary,
    institutional, biographical, mobile, national).

% Gains from halted external transfers and from the procurement demand the freed revenue generates; it lobbies for complete repudiation and supplies the rearmament program. Its fortunes are tied to domestic orders and protected markets, so abandoning the arrangement would cost it its principal customer.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_industrial_complex, beneficiary,
    powerful, generational, constrained, national).

% Draws legitimacy, membership, and mobilizing energy from the denial of the treaty's validity; the war-guilt question is constitutive of its identity rather than a position it happens to hold. Even a favorable settlement leaves it searching for the next grievance, because the grievance is the organization.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_nationalist_movement, beneficiary,
    organized, generational, identity_locked, national).

% France and Belgium levied taxes, issued bonds, and sized veteran and widow pension systems on the strength of enforceable German transfers. Their claims survive only as paper: the Ruhr occupation demonstrated that enforcement costs exceed recovery, alliance partners oppose coercion, and by the early 1930s no instrument remains to press. Pension statutes run for decades, locking the loss into their budgets.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_states, payer,
    institutional, generational, trapped, continental).

% Households, rebuilt towns, maimed veterans, and widows owed compensation for destroyed property and lost earners. Recovery ran exclusively through state channels that have been extinguished; they hold no independent claim they can press against anyone, and their losses are socialized onto their own taxpayers.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_belgian_war_damage_claimants, payer,
    powerless, biographical, trapped, regional).

% Financed the Allied war effort and held the downstream end of the reparations-debt chain, but never ratified the treaty and had no seat at the settlement conferences. When the reparations claims die, the inter-allied debts die with them — moratoria and the Lausanne linkage sweep the chain away. It can withhold future credit but cannot revive the cancelled paper.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, american_interallied_debt_holders, excluded,
    institutional, biographical, constrained, global).

% International lawyers and diplomatic historians assess what the episode does to the consent theory of treaty obligation and to the general presumption that signed instruments bind. They observe every seat at once, hold no stake in the flows, and their judgments feed treatises rather than treasuries.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, treaty_law_regime_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, german_state_treasury).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ends the reparations dispute by removing its object: instead of coordinating an annual payment schedule among creditor states, the arrangement cancels the claims outright, terminating the recurring negotiations over capacity, transfers, and defaults. It also coordinates German domestic politics around a shared account of how the war ended — a common grievance that stabilizes governing coalitions.
% TRANSFER_FUNCTION: Moves the burden of war costs from Germany back onto the creditor publics: French and Belgian reconstruction borrowing, veteran and widow pensions, and municipal rebuilding debts sized to expected German transfers become unrecoverable, while the freed German fiscal capacity flows to domestic spending and rearmament procurement. It also transfers status — recasting Germany from condemned debtor to aggrieved sovereign equal.
% ABSENT_VOICES: The individual claimants — rebuilt-town municipalities, maimed veterans, widows drawing pensions sized to expected German payments — were never seated at Lausanne or in the bilateral cancellations; the arrangement was settled among heads of government. Smaller Allied states with damage claims likewise had no seat when the chain collapsed. They would object that token gestures socialize their losses onto their own taxpayers.
% DISAPPEARANCE_RATIONALE: The inter-war financial architecture ran through the reparations-inter-allied-debt chain; extinguishing the claims rearranged European public finance overnight (creditor budgets rebalanced at Lausanne), removed the last material brake on German rearmament, and left the security arrangements tied to the settlement without their fiscal basis. Reviving enforceable claims would force a different European order than the one that actually formed.
% FOUNDING_PROBLEM: Escape an unpayable and politically intolerable liability: the 1921 London Schedule of Payments (132 billion gold marks) exceeded any plausible transferable surplus, and Article 231's war-guilt framing made performance read as confession. German governments needed a principled basis — short of admitted insolvency — to end the obligations.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the German beneficiary set: Seymour Parker Gilbert, the Agent-General for Reparations (an American official administering the settlement), reported the transfer problem as structurally insoluble at the scheduled rates; Keynes, a British critic of the treaty, documented the arithmetic impossibility in 1919; and the Lausanne Final Act — signed by the creditor states themselves — is the formal instrument attesting the claims' extinction. French and Belgian officialdom simultaneously attests the opposing fact (the claims' legitimacy and the injury of cancellation), so both the problem's reality and its death are corroborated by parties with no stake in the German position.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.85 for the standing arrangement under contest — the post-Versailles cost-bearing structure as instantiated by this reading, in which German payment obligations are void and creditor claims are uncollectible paper — assessed by this reading's own lights; the reading's endorsed alternative (a consent-based order with voluntary gestures) is not the referent. The arrangement allocates essentially the entire war-cost residue to the creditor side while the freed German fiscal capacity funds domestic spending and rearmament. Suppression (0.75, raw and unscaled — only extractiveness is scaled by directionality and scope) reflects the coercive work needed to hold the arrangement: the Ruhr occupation failed, enforcement coalitions fractured, and by 1933 the claims were kept dead by power asymmetry rather than consent. Theater (0.30) captures token gestures and juridical-historiographic performance that maintain diplomatic standing while the substance is refusal. Accessibility collapse (0.68): once the claims' unenforceability is understood, creditor alternatives — enforcement, arbitration, partial-compensation frameworks — collapse almost entirely; Germany retains opportunistic re-entry into diplomatic frameworks, which is why the value is not higher. Resistance (0.65): the creditor coalition fought continuously — Ruhr occupation, default pressure, diplomatic linkage — peaking in 1923. Claim and metrics are independent: claimed_type snare is asserted from the structural facts (a juridical cover story, coercive persistence, identifiable victims, suppressed alternatives); the engine computes per-seat classifications from the structural data. The measurement series share one time grid (points 0, 4, 8, 12, 16, 20); suppression_requirement is tracked because the story genuinely traces enforcement-capacity dynamics (blockade-era leverage, the Ruhr confrontation, the Dawes-era stabilization dip, Lausanne extinction, rearmament-backed deterrence), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the German seats compute opposite types from the same structure. From the allied_creditor_states and french_belgian_war_damage_claimants positions the arrangement is confiscation of compensated expectation: taxes were levied, bonds issued, and pensions sized on the strength of enforceable claims, and the doctrine extinguishes those claims retroactively. From the german_revisionist_government and german_nationalist_movement positions the same structure is restitution of sovereignty — release from a confession-extraction device imposed by blockade and ultimatum. The german_state_treasury experiences it as pure fiscal windfall; american_interallied_debt_holders experience it as collateral chain-collapse they had no seat to influence. The engine derives these divergences from power, exit, and directional position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the three German seats near the beneficiary end: the treasury receives the flows directly (d near 0.05), the industrial complex receives procurement and protected markets (d near 0.15), and the nationalist movement collects legitimacy and mobilizing grievance, with identity-lock amplifying how durably it holds that position. The revisionist government is agenda-setter and collector at once — it writes the nullification instruments and is their chief political beneficiary — so its derived d sits low despite its rule-setting role. Victim declarations place the creditor seats near the target end: allied_creditor_states (trapped exit — enforcement failed and alliance politics block coercion) sit near full-target; french_belgian_war_damage_claimants (powerless, no independent claim) sit at the extreme; american_interallied_debt_holders are indirect targets through the inter-allied debt chain (constrained exit, d somewhat lower). Continental-to-global scope scales effective extraction upward for the trapped creditor seats and does little for the mobile German treasury. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct ordering, and the available override keys (power atoms) are too coarse to differentiate same-power seats cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — escaping an unpayable, confession-framed schedule without admitted insolvency — was solved by the arrangement's own success: Lausanne (1932) extinguished the claims and the 1933 default ended even token performance. The arrangement then persisted as mobilization ideology rather than debt management, which is the signature the R5 mismatch consumer flags: founding_problem_status dead combined with disappearance_verdict world_rearranges routes this story to the capture/zombie check, cross-read against the theater path. The theater_ratio series shows the shift — token-performance theater peaks around the extinction (point 16) and then partially converts into historiographic and propagandistic maintenance. The classification prevents mislabeling in both directions: reading the arrangement purely through its juridical cover story would mistake a coerced claim-nullification for a consent principle; reading it purely as fiscal self-interest misses the identity-fused beneficiary whose attachment outlived the money. The identity-lock omega tracks whether the post-extinction persistence is ideological rather than interested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    versailles_kernel_reading_contestation,
    'This constraint is one reading (repudiation_reading) of the kernel versailles_reparations_clauses; what structurally changes under the sibling readings, and where exactly do the readings disagree?',
    'Comparative classification across the three reading-stories: differences in victim sets, directionality, and epsilon locate the disagreement — punitive_liability_reading inverts the victim set (German publics become the extracted-from side), limited_responsibility_reading produces a bounded-transfer regime with partial extraction on both sides.',
    'Under the punitive sibling, epsilon indexes extraction from Germany and the creditor seats become beneficiaries; under the limited sibling the arrangement approaches a coordination regime with capped transfers. The foreclosure relations authored in cs_structure would soften to influence-type edges if the readings prove reconcilable within one framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(versailles_kernel_reading_contestation, conceptual, 'Committer structure: one of three rival readings of the Versailles reparations kernel.').

omega_variable(
    duress_threshold_empirics,
    'Was the treaty''s imposition duress in the legally operative sense — continued naval blockade after the armistice, starvation conditions, the threat of renewed hostilities — or ordinary defeated-party peace terms?',
    'Archival reconstruction of the armistice and June 1919 negotiations (blockade continuation records, the five-day ultimatum), assessed against the later codified standard under which threat or use of force vitiates consent.',
    'If duress meets the modern threshold, the repudiation axiom gains colorable legal footing and the arrangement''s cover-story characterization weakens; if not, the doctrine is revealed as pure instrument and the extractive reading of the structure strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_threshold_empirics, empirical, 'Whether the factual predicate of the duress axiom holds.').

omega_variable(
    transfer_capacity_counterfactual,
    'Could Germany actually have transferred sums approaching the scheduled reparations, or was the schedule economically impossible, making nullification overdetermined?',
    'Economic-historical reconstruction of Weimar transfer capacity: balance-of-payments analyses, the Keynes-Mantoux controversy, and Agent-General reports on the transfer problem.',
    'If the schedule was unpayable, part of the creditor-side loss is misattributed — the claims were worthless regardless — and the arrangement''s epsilon overstates recoverable-value suppression; if transfer was feasible, nullification is closer to pure rent retention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_capacity_counterfactual, empirical, 'Counterfactual solvency of the cancelled claims.').

omega_variable(
    revisionist_identity_lock_persistence,
    'Does the german_nationalist_movement''s attachment to repudiation persist after the founding problem dies because of identity fusion rather than fiscal interest?',
    'Post-extinction trajectory analysis (1933-1939): if repudiation rhetoric intensifies after the claims are already dead and unrevivable, the driver is ideological identity, not interest.',
    'If identity-driven, the late-interval arrangement functions as mobilization infrastructure with theatrical maintenance — an inertial drift inside the extractive structure; the theater_ratio series should then be read as symptom rather than noise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revisionist_identity_lock_persistence, empirical, 'Identity-lock versus interest as the persistence mechanism after mandate death.').

omega_variable(
    kernel_codification_framing,
    'Is the contested kernel the treaty text itself (fixed_text) or the evolving settlement practice built atop it (the Dawes, Young, and Lausanne machinery)?',
    'Test which framing the readings'' disagreement actually runs through: if the dispute turns on clauses of the text, fixed_text holds; if it turns on successive administrative settlements, a distributed or practice framing fits.',
    'Under the practice framing the three readings become stages of one evolving arrangement rather than rival instantiations, converting the authored forecloses relations into influence-type edges and changing kernel_codification from fixed_text to distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_codification_framing, conceptual, 'Framing under-determination in the kernel''s codification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__repudiation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(vers_tr_t0, observed).
narrative_ontology:measurement(vers_tr_t4, versailles_reparations_clauses__repudiation_reading, theater_ratio, 4, 0.16).
narrative_ontology:measurement_basis(vers_tr_t4, observed).
narrative_ontology:measurement(vers_tr_t8, versailles_reparations_clauses__repudiation_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(vers_tr_t8, observed).
narrative_ontology:measurement(vers_tr_t12, versailles_reparations_clauses__repudiation_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(vers_tr_t12, observed).
narrative_ontology:measurement(vers_tr_t16, versailles_reparations_clauses__repudiation_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(vers_tr_t16, observed).
narrative_ontology:measurement(vers_tr_t20, versailles_reparations_clauses__repudiation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(vers_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(vers_be_t0, observed).
narrative_ontology:measurement(vers_be_t4, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 4, 0.25).
narrative_ontology:measurement_basis(vers_be_t4, observed).
narrative_ontology:measurement(vers_be_t8, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement_basis(vers_be_t8, observed).
narrative_ontology:measurement(vers_be_t12, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement_basis(vers_be_t12, observed).
narrative_ontology:measurement(vers_be_t16, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 16, 0.82).
narrative_ontology:measurement_basis(vers_be_t16, observed).
narrative_ontology:measurement(vers_be_t20, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement_basis(vers_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(vers_su_t0, observed).
narrative_ontology:measurement(vers_su_t4, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement_basis(vers_su_t4, observed).
narrative_ontology:measurement(vers_su_t8, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(vers_su_t8, observed).
narrative_ontology:measurement(vers_su_t12, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(vers_su_t12, observed).
narrative_ontology:measurement(vers_su_t16, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement_basis(vers_su_t16, observed).
narrative_ontology:measurement(vers_su_t20, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(vers_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, resource_allocation).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Versailles reparations' decomposes into three structurally distinct constraints (epsilon-invariance): the punitive_liability_reading instantiates a regime extracting quasi-unlimited transfers from Germany (victims: German publics); the limited_responsibility_reading instantiates a capacity-bounded transfer regime (partial extraction on both sides); this repudiation_reading instantiates a claim-nullification regime extracting from the creditor side (victims: Allied creditor publics). The readings are linked as a family; the punitive reading is the treaty-text baseline the other two modify or negate. Epsilon differs sharply across members because the victim set inverts — measuring one member with another's observable changes epsilon, which is why they are separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
