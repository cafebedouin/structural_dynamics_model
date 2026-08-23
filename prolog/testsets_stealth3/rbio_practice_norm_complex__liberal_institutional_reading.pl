% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Norm Complex — Liberal Institutional Reading
 *   domain: international relations / international law / political economy
 *
 * SUMMARY:
 *   A contested kernel — the rules-based international order centered on the
 *   UN Charter — is read here through the liberal-institutional lens: norms
 *   universal in scope, consent-based in origin, revisable through legitimate
 *   multilateral process, with enforcement selectivity attributed to capacity
 *   rather than legitimacy. This file authors THAT reading as a clean,
 *   epsilon-invariant constraint; the sibling readings are separate files
 *   linked through the network. The epsilon referent is the standing
 *   arrangement under contest — the actual operating order of treaties,
 *   Security Council machinery, sanctions regimes, and crisis lending —
 *   assessed by this reading's own lights, never the reformed order it
 *   endorses. The claim/metric gap is deliberate: the reading narrates
 *   legitimate coordination, while the authored metrics describe a construct
 *   with a real coordination core and substantial, actively enforced
 *   asymmetric costs — the engine measures the divergence; the claim is not
 *   tuned to the metrics. KEY AGENTS (by structural relationship): -
 *   permanent_five_members: agenda-setting beneficiary
 *   (institutional/arbitrage) — hold authorization, amendment, and immunity
 *   gates - creditor_institutions: agenda-setting beneficiary
 *   (institutional/arbitrage) — set conditionality terms in crises -
 *   intervening_coalition_states: primary beneficiary (powerful/arbitrage) —
 *   collect influence, basing, and precedent from authorized or
 *   atrocity-framed intervention - defense_and_reconstruction_contractors:
 *   secondary beneficiary (organized/mobile) — revenue tracks enforcement
 *   intensity - small_open_economy_states: incidental beneficiary
 *   (moderate/constrained) — purchase protection with agenda exclusion -
 *   targeted_state_populations: primary target (powerless/trapped) — bear
 *   sanctions costs without a seat - targeted_state_governments: target with
 *   partial insulation (moderate/constrained) — shift costs downward via
 *   evasion - debtor_developing_states: recurring target
 *   (moderate/constrained) — sign under narrowed alternative sets -
 *   humanitarian_organizations: excluded voice (organized/constrained) —
 *   document harm, hold no vote - emerging_power_blocs: excluded challenger
 *   (powerful/constrained) — build parallels rather than exit -
 *   international_compliance_scholars: analytical observer — test whether
 *   selectivity tracks capacity or interest
 *
 * KEY AGENTS:
 *   - permanent_five_members: agenda-setting beneficiary (institutional/arbitrage) — hold authorization, amendment, and immunity gates
 *   - creditor_institutions: agenda-setting beneficiary (institutional/arbitrage) — set conditionality terms in crises
 *   - intervening_coalition_states: primary beneficiary (powerful/arbitrage) — collect influence, basing, and precedent from authorized or atrocity-framed intervention
 *   - defense_and_reconstruction_contractors: secondary beneficiary (organized/mobile) — revenue tracks enforcement intensity
 *   - small_open_economy_states: incidental beneficiary (moderate/constrained) — purchase protection with agenda exclusion
 *   - targeted_state_populations: primary target (powerless/trapped) — bear sanctions costs without a seat
 *   - targeted_state_governments: target with partial insulation (moderate/constrained) — shift costs downward via evasion
 *   - debtor_developing_states: recurring target (moderate/constrained) — sign under narrowed alternative sets
 *   - humanitarian_organizations: excluded voice (organized/constrained) — document harm, hold no vote
 *   - emerging_power_blocs: excluded challenger (powerful/constrained) — build parallels rather than exit
 *   - international_compliance_scholars: analytical observer — test whether selectivity tracks capacity or interest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.65).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.55).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Norm Complex — Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international relations / international law / political economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, 'abf22f31-57f5-46cb-8e71-64e80410702b').
narrative_ontology:cs_kernel_codification('abf22f31-57f5-46cb-8e71-64e80410702b', fixed_text).
narrative_ontology:cs_authority_grounding('abf22f31-57f5-46cb-8e71-64e80410702b', lineage).
narrative_ontology:cs_interpretation_layer_present('abf22f31-57f5-46cb-8e71-64e80410702b').
narrative_ontology:cs_reading_relation('abf22f31-57f5-46cb-8e71-64e80410702b', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('abf22f31-57f5-46cb-8e71-64e80410702b', rbio_practice_norm_complex__sovereignty_maximalist_reading, forecloses).
narrative_ontology:cs_axiom('abf22f31-57f5-46cb-8e71-64e80410702b', foundational, consent_confers_binding_legitimacy).
narrative_ontology:cs_axiom_status(consent_confers_binding_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('abf22f31-57f5-46cb-8e71-64e80410702b', consent_confers_binding_legitimacy, conventional).
narrative_ontology:cs_axiom('abf22f31-57f5-46cb-8e71-64e80410702b', foundational, selective_enforcement_is_capacity_not_intent).
narrative_ontology:cs_axiom_status(selective_enforcement_is_capacity_not_intent, holdable).
narrative_ontology:cs_axiom_grounding('abf22f31-57f5-46cb-8e71-64e80410702b', selective_enforcement_is_capacity_not_intent, empirically_contingent).
narrative_ontology:cs_axiom('abf22f31-57f5-46cb-8e71-64e80410702b', secondary, grave_atrocity_authorizes_collective_coercion).
narrative_ontology:cs_axiom_status(grave_atrocity_authorizes_collective_coercion, holdable).
narrative_ontology:cs_axiom_grounding('abf22f31-57f5-46cb-8e71-64e80410702b', grave_atrocity_authorizes_collective_coercion, deontological).
narrative_ontology:cs_reference_frame('abf22f31-57f5-46cb-8e71-64e80410702b', consent_based_revisable_charter_order).
narrative_ontology:cs_drift_state('abf22f31-57f5-46cb-8e71-64e80410702b', contemporary_multipolar_contestation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('abf22f31-57f5-46cb-8e71-64e80410702b', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, permanent_five_members).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_coalition_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, defense_and_reconstruction_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, creditor_institutions).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, small_open_economy_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, debtor_developing_states).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, collective_security_doctrine).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, consent_based_treaty_obligation).
narrative_ontology:constraint_vindicates(rbio_practice_norm_complex__liberal_institutional_reading, liberal_institutional_effectiveness_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states hold veto power over Security Council authorization, sanctions imposition and termination, and Charter amendment ratification. They decide which violations reach the agenda, shield aligned governments from measures, and convert procedural position into enforcement immunity. Their consent is the effective ceiling on any revision of the order's decision rules.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, permanent_five_members, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, permanent_five_members, beneficiary).

% Multilateral lenders and creditor clubs that attach policy conditions to crisis financing. Terms are presented as contracts between consenting parties, but they are negotiated when the borrower's alternatives have collapsed; programs move fiscal, trade, and ownership policy to lender approval for their duration. Repayment streams and policy influence flow back to the institutions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, creditor_institutions, beneficiary).

% Governments that contribute forces to authorized operations or lead coalitions justified by grave-atrocity arguments. They gain basing rights, strategic positioning, doctrinal precedent, and first claim on post-conflict influence; they bear the operational costs of deployment. When authorization fails they can act anyway and renegotiate the narrative afterward, which few other seats can do.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_coalition_states, beneficiary,
    powerful, biographical, arbitrage, global).

% Firms supplying logistics, weapons, security services, and reconstruction. Their order books expand with each intervention and with each lifting of sanctions that opens rebuilt markets. They carry none of the measures' civilian costs and face little regulatory exposure from the decisions that generate their revenue.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, defense_and_reconstruction_contractors, beneficiary,
    organized, biographical, mobile, global).

% Trade-dependent states whose security and market access rest on rules restraining larger powers. They gain disproportionate protection from dispute settlement and from the norm against conquest, and they ratify nearly every revision — almost always text someone else tabled. Their voice in agenda-setting is close to nil.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, small_open_economy_states, beneficiary,
    moderate, generational, constrained, regional).

% Civilians living under comprehensive or sectoral sanctions. Medicine, spare parts, and income collapse faster than elite consumption does; measures are designed and renewed in committees they cannot address, and humanitarian exemptions arrive late and partial. Exit means displacement abroad or endurance at home; there is no opt-out.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_populations, payer,
    powerless, immediate, trapped, national).

% Governments under sanctions regimes. They lose reserves, trade partners, and international legitimacy, and respond with rationing, smuggling networks, and patronage that insulate ruling circles while shifting scarcity onto the population. Their consent to the measures was never sought; their main lever is endurance.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments, payer,
    moderate, biographical, constrained, national).

% Governments that sign structural adjustment and standby agreements during balance-of-payments crises. Signature is formally voluntary; the realistic alternative set — default, cutoff from markets — narrows precisely when terms are dictated. Policy autonomy transfers to lender review missions for the program period, and compliance history prices future access.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, debtor_developing_states, payer,
    moderate, biographical, constrained, regional).

% Agencies and NGOs that measure and publicize population-level harm from sanctions and intervention. They negotiate exemption carve-outs inside sanctions committees and brief the Council, but they hold no vote on measure design, renewal, or termination, and their findings enter the record after decisions are substantially made.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_organizations, excluded,
    organized, biographical, constrained, global).

% Coalitions of large middle powers contesting governance weights and enforcement symmetry. They fund parallel development banks, currency arrangements, and forums rather than leaving the order, seeking leverage for reform. Their objections reshape summit rhetoric; the veto-weighted agenda they object to remains closed to them.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, emerging_power_blocs, excluded,
    powerful, generational, constrained, continental).

% Researchers compiling enforcement datasets across decades of violations. They test whether measures correlate with violation severity, target capability, or sponsor alignment, and publish the results. Their work feeds legitimacy disputes in journals and hearings; it touches no enforcement lever and alters no committee vote.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_compliance_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, intervening_coalition_states).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common rulebook that lowers the cost of interstate interaction: registered treaties, predictable trade and finance terms, dispute-settlement forums, and a collective-security channel that requires authorization before force. States coordinate expectations about each other's conduct through it daily.
% TRANSFER_FUNCTION: Moves coercive discretion and economic concession upward and outward: sanctions committees redirect targeted-state trade and finance flows; crisis lending moves policy control from borrowing governments to creditor institutions for program duration; authorized interventions move procurement and reconstruction spending toward coalition members' suppliers; veto holders convert procedural position into enforcement immunity for aligned states.
% ABSENT_VOICES: Populations living under sanctions have no seat in the committees that design or renew the measures; humanitarian organizations submit impact findings but hold no vote; emerging powers contest governance weights from outside the veto circle; debtor parliaments often learn program terms only after executive signature.
% DISAPPEARANCE_RATIONALE: Trade, diplomacy, and collective security would reorganize around regional spheres, bilateral bargains, and raw capability within years: shipping and finance would reprice risk without treaty backstops, disputes would settle by power rather than procedure, and middle powers would scramble into new security alignments.
% FOUNDING_PROBLEM: After two world wars and the interwar collapse of collective security, the founders built a system to channel state violence through collective authorization, bind commerce to agreed rules, and give weaker states procedural protection against pure power politics.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the interwar institutional collapse and the archival record of the founding conferences attest the problem from outside the benefiting parties; notably, the order's sharpest critics — hegemonic-extraction and sovereignty-maximalist theorists — agree the founding problem was real while disputing whether the current arrangement solves it. No serious party claims great-power war prevention stopped mattering.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.65) sits in hybrid range: the arrangement delivers measurable public goods — trade predictability, dispute settlement, a channel restraining unilateral force — while transferring severe costs to seats that never consented to them: sanctions populations, program-country publics, states on the receiving end of authorized force. Suppression (0.55) is moderate and mostly structural: exit exists on paper (withdrawal clauses, default) but the alternative set narrows exactly when terms bite, and enforcement machinery (sanctions committees, asset freezes, interdiction) actively closes informal exits. Theater (0.44) is the fastest-moving metric: summitry, resolution passage, and rules-based-order affirmation grow while binding revision output stays flat, so a rising share of multilateral activity performs commitment rather than producing it. Accessibility collapse (0.45) and resistance (0.60) reflect a construct, not a natural law: parallel banks, regional blocs, and open great-power contestation persist, and enforcement selectivity draws continuous challenge. The three measurement series share one eight-point grid (1990–2026). The suppression_requirement series traces enforcement-machinery history — rapid build-up through the 1990s comprehensive-sanctions era, hardening through the counterterrorism-financing period, then partial refinement as targeted-sanctions technique matured — which is why it rises and plateaus rather than tracking extractiveness monotonically.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setting seats (permanent five, creditor institutions) the arrangement is the legitimate machine they operate: consent was given, procedures were followed, and shortfalls reflect capacity. From the trapped payer seat (targeted_state_populations) the same machinery arrives as siege: measures designed elsewhere, renewed over their objection, costing lives. Small open economies experience a third structure — genuine protection purchased with agenda exclusion. Emerging powers experience a fourth: bound by rules they helped neither write nor revise. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: permanent_five_members and creditor_institutions sit nearest the beneficiary pole (arbitrage-grade exit — they wrote the gates they stand behind); intervening_coalition_states and defense_and_reconstruction_contractors collect directly from enforcement events. small_open_economy_states derive real subsidy from rules that restrain larger powers, placing them low-d despite zero agenda access. Victim declarations drive high directionalities amplified by exit: targeted_state_populations are trapped (near full-target), debtor_developing_states and targeted_state_governments constrained. Global spatial scope scales effective extraction modestly upward for everyone — verifying compliance across a planetary order is hard. Suppression is authored unscaled, per the structural-property rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — channeling great-power violence and stabilizing commerce after interwar collapse — remains live, so the arrangement is not mandatrophy-resolved, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag. The drift risk is localized: the revision subsystem (Charter amendment, governance-weight reform, expansion of permanent membership) shows rising theater against flat output, the classic precursor of piton formation inside one component of a live constraint. If binding revision output stays flat for another interval while performative activity keeps growing, expect a component-level transition to date even while the security-and-trade core keeps functioning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates only the liberal_institutional_reading of kernel rbio_practice_norm_complex; which structural facts would change under the sibling readings?',
    'Cross-reading comparison: the hegemonic_extraction_reading raises epsilon by treating formal revisability as practically closed (P5 veto, institutional path-dependency) and selectivity as revealing intent; the sovereignty_maximalist_reading shrinks the legitimate-norm set to sovereignty-protective rules, reclassifies humanitarian exceptions as pretexts, and expands the victim set to any externally interfered state.',
    'Adopting the hegemonic sibling pushes classification toward snare; adopting the maximalist sibling dissolves the beneficiary/victim structure declared here and re-centers sovereignty itself as the protected good. The disagreement is located at two structural elements: whether formal revision channels are practically operable, and whether consent under material asymmetry counts as consent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: one reading of a three-reading kernel; sibling readings alter epsilon, victim sets, and classification.').

omega_variable(
    consent_under_asymmetry,
    'Is conditionality genuinely consensual (contract terms between willing parties) or coerced (terms accepted because the alternative set has collapsed)?',
    'Compare program acceptance and term severity across borrowers with strong outside options (alternative creditors, commodity windfalls) versus those without; if identical asymmetries of terms track outside-option weakness rather than policy merit, consent is nominal.',
    'If consent is nominal, the transfer function operates as extraction dressed as contract, epsilon rises toward the hegemonic sibling''s estimate, and the reading''s foundational consent axiom loses its warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_asymmetry, empirical, 'Whether crisis-lending consent survives scrutiny as consent.').

omega_variable(
    selectivity_capacity_vs_interest,
    'Does enforcement variance across violations track enforcer capacity (the reading''s claim) or sponsor alignment (the hegemonic sibling''s claim)?',
    'Panel analysis of enforcement actions regressed on violation severity, target capability, and sponsor alignment; capacity predicts response where capability is the binding constraint, interest predicts response where alignment does.',
    'If interest dominates, the capacity framing collapses, the foundational axiom selective_enforcement_is_capacity_not_intent is empirically overridden, and classification drifts toward the hegemonic sibling''s snare-leaning account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_capacity_vs_interest, empirical, 'The reading''s core empirical wager: selectivity as capacity shortfall versus interest expression.').

omega_variable(
    revisability_functionality,
    'Are the multilateral revision channels functionally open (binding amendments and new constraints on great powers actually pass) or formally open and practically closed?',
    'Inventory binding revisions since 1990 weighted by whether they constrained a great power against its revealed preference; near-zero yield indicates closure regardless of procedural availability.',
    'Closed channels mean theater_ratio understates the performative share, the scaffold-style transitional justification is unavailable, and the hegemonic sibling''s frozen-project description gains force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisability_functionality, empirical, 'Whether the revision promise is operable machinery or procedural theater.').

omega_variable(
    sanctions_harm_attribution,
    'Does civilian harm under sanctions flow from the measures themselves or from target-government diversion that shifts scarcity onto populations?',
    'Cross-episode welfare comparison conditioned on measured diversion propensity (smuggling dependence, patronage intensity) across sanctions regimes.',
    'Attribution reallocates victimhood between targeted_state_populations and targeted_state_governments, changing derived directionalities; heavy diversion partially rehabilitates the measures'' design while confirming the trapped-population cost structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_harm_attribution, empirical, 'Where sanctions harm causally originates: measure design or regime conduct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement_basis(rbio_tr_t1990, observed).
narrative_ontology:measurement(rbio_tr_t1995, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement_basis(rbio_tr_t1995, observed).
narrative_ontology:measurement(rbio_tr_t2000, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(rbio_tr_t2000, observed).
narrative_ontology:measurement(rbio_tr_t2005, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(rbio_tr_t2005, observed).
narrative_ontology:measurement(rbio_tr_t2010, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement_basis(rbio_tr_t2010, observed).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(rbio_tr_t2015, observed).
narrative_ontology:measurement(rbio_tr_t2020, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(rbio_tr_t2020, observed).
narrative_ontology:measurement(rbio_tr_t2026, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2026, 0.44).
narrative_ontology:measurement_basis(rbio_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement_basis(rbio_be_t1990, observed).
narrative_ontology:measurement(rbio_be_t1995, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(rbio_be_t1995, observed).
narrative_ontology:measurement(rbio_be_t2000, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement_basis(rbio_be_t2000, observed).
narrative_ontology:measurement(rbio_be_t2005, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement_basis(rbio_be_t2005, observed).
narrative_ontology:measurement(rbio_be_t2010, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement_basis(rbio_be_t2010, observed).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(rbio_be_t2015, observed).
narrative_ontology:measurement(rbio_be_t2020, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement_basis(rbio_be_t2020, observed).
narrative_ontology:measurement(rbio_be_t2026, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2026, 0.65).
narrative_ontology:measurement_basis(rbio_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement_basis(rbio_su_t1990, observed).
narrative_ontology:measurement(rbio_su_t1995, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement_basis(rbio_su_t1995, observed).
narrative_ontology:measurement(rbio_su_t2000, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement_basis(rbio_su_t2000, observed).
narrative_ontology:measurement(rbio_su_t2005, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement_basis(rbio_su_t2005, observed).
narrative_ontology:measurement(rbio_su_t2010, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement_basis(rbio_su_t2010, observed).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(rbio_su_t2015, observed).
narrative_ontology:measurement(rbio_su_t2020, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement_basis(rbio_su_t2020, observed).
narrative_ontology:measurement(rbio_su_t2026, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(rbio_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'RBIO' decomposes into three readings of one kernel, each with its own stable epsilon, beneficiary/victim structure, and classification. This liberal-institutional file is the order's official self-description (upstream: cited as evidence of legitimacy). The hegemonic-extraction sibling consumes this reading's enforcement record as its evidence base (downstream); the sovereignty-maximalist sibling consumes its intervention precedents likewise. All three files link mutually through network.affects_constraints; epsilon differs across them because the readings assess the same standing arrangement through incompatible premises, not because the arrangement changed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
