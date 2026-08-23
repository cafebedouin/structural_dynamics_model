% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__member_sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__member_sovereignty_primary, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_obligations__member_sovereignty_primary
 *   human_readable: National Welfare Closure Authority over Intra-EU Mobility (Member-Sovereignty Reading)
 *   domain: political economy/federalism/migration policy/welfare state theory
 *
 * SUMMARY:
 *   National welfare states inside the European Union retain the authority to
 *   close their insurance pools: access for workers who move between member
 *   states is conditioned on residence duration, habitual residence, resource
 *   sufficiency, and activity status, and member state legislatures
 *   collectively hold veto power over any reform that would widen access. The
 *   arrangement solves a real problem — redistributive pools whose
 *   contributors and claimants can decouple across borders are hard to
 *   finance and harder to defend politically — while simultaneously
 *   transferring resources from mobile workers, cross-border households, and
 *   sending states toward receiving-state budgets. This file instantiates the
 *   member_sovereignty_primary reading of the
 *   federation_membership_obligations kernel and authors epsilon for the
 *   standing arrangement by that reading's own lights; the sibling readings
 *   are separate files linked in network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - member_state_legislatures: primary agenda
 *   holder (institutional/constrained) — writes, amends, and defends the
 *   closure rules; answers to electorates that reward visible defense of
 *   national benefit systems - european_commission: supranational co-agenda
 *   setter (institutional/arbitrage) — proposes the EU-law frame and
 *   litigates against closure overreach while trading concessions -
 *   court_of_justice_of_the_eu: adjudicating agenda setter
 *   (institutional/analytical) — its case law is the operative boundary,
 *   alternately widening and upholding denial - mobile_eu_workers: principal
 *   target (powerless/constrained) — pays host taxes and contributions from
 *   day one, meets waiting periods and resource checks before matching
 *   entitlement - receiving_state_taxpayers: primary beneficiary
 *   (organized/constrained) — pool boundaries stabilize their contribution
 *   rates and shield benefit levels - domestic_low_wage_workers: contingent
 *   beneficiary (moderate/constrained) — protected from wage pressure at
 *   home, subject to the same rules if they move - sending_state_governments:
 *   secondary target (institutional/constrained) — loses contributors,
 *   absorbs returnee costs, outvoted in Council - cross_border_households:
 *   sharpest target (powerless/constrained) — full contributions, reduced or
 *   withheld family benefits for dependents residing elsewhere -
 *   social_policy_researchers: analytical observer (analytical/analytical) —
 *   documents claim rates and fiscal incidence that routinely contradict the
 *   public burden narrative
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__member_sovereignty_primary, 0.44).
domain_priors:suppression_score(federation_membership_obligations__member_sovereignty_primary, 0.57).
domain_priors:theater_ratio(federation_membership_obligations__member_sovereignty_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, extractiveness, 0.44).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(federation_membership_obligations__member_sovereignty_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__member_sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__member_sovereignty_primary, "National Welfare Closure Authority over Intra-EU Mobility (Member-Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__member_sovereignty_primary, "political economy/federalism/migration policy/welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__member_sovereignty_primary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, receiving_state_taxpayers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__member_sovereignty_primary, domestic_low_wage_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, cross_border_households).
narrative_ontology:constraint_victim(federation_membership_obligations__member_sovereignty_primary, sending_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, bounded_solidarity_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_obligations__member_sovereignty_primary, subsidiarity_in_social_security_organization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pass and amend the national statutes that define who may join the insurance pool: residence-duration tests, habitual-residence checks, resource-sufficiency thresholds, and export limits on benefits. They answer to domestic electorates that reward visible defense of national benefit systems, and they negotiate collectively in the Council where each government holds a veto over major change. Their room to concede anything is bounded by welfare-chauvinist competition at home; abandoning the framework entirely would mean leaving the union at prohibitive cost.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, member_state_legislatures, agenda_setter,
    institutional, biographical, constrained, national).

% Proposes the EU legislation that sets the outer limits of national closure — the citizens' rights directive and the social-security coordination regulations — opens infringement proceedings when national rules obstruct lawful movers, and packages concessions to keep the larger integration bargain alive. It alternates between pressing for wider portability and accepting closure wins in specific cases, and it can reframe, delay, or bundle issues in ways the national legislatures cannot.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, european_commission, agenda_setter,
    institutional, generational, arbitrage, continental).

% Decides case by case where national discretion ends. Its rulings have widened access for lawfully resident workers and upheld denials for economically inactive movers and for family benefits where children reside abroad. Its case law is the operative rulebook that both defending and challenging seats litigate over; it legislates nothing but resolves everything brought before it.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, court_of_justice_of_the_eu, agenda_setter,
    institutional, civilizational, analytical, continental).

% Move for work inside the union and pay host-country taxes and social contributions from their first day. During job search, employment gaps, or periods of low income they meet waiting periods, resource checks, and habitual-residence tests that delay or deny benefits nationals receive outright, and some entitlements are reduced or refused regardless of what they contributed. Their fallbacks — moving again, returning home, absorbing the gap — each cost networks, seniority, and savings. Collectively they hold coalition channels through unions and cross-border litigation that individual standing conceals.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, mobile_eu_workers, payer,
    powerless, biographical, constrained, continental).

% Fund the national insurance pools through payroll levies and general revenue. Pool boundaries keep the claimant population anchored to established residence and work histories, which stabilizes contribution rates and shields benefit levels in downturns, and the fiscal surplus left by working migrants flows into the same funds. They act through elections, where defense of the benefit system is a recurring promise, and cannot relocate away from their own tax system at reasonable cost.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, receiving_state_taxpayers, beneficiary,
    organized, biographical, constrained, national).

% Work in labor markets where inflows from poorer regions press on wages and on in-work benefit generosity. Conditional access slows competitor arrival and preserves the bargaining position of existing residents; the same rules, however, stand ready to bind them the day they move abroad for work, so the protection they enjoy is contingent on staying put.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, domestic_low_wage_workers, beneficiary,
    moderate, biographical, constrained, national).

% Lose prime-age contributors to richer member states and later absorb returnees whose contribution records are fragmented across systems. Access conditions abroad shift unemployment, family-support, and old-age costs back onto their budgets and households. Their remedies — infringement complaints, coalition voting in the Council, bilateral portability talks — move slowly against the weight of receiving-state vetoes.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, sending_state_governments, payer,
    institutional, generational, constrained, continental).

% Families whose earners work in one member state while children or dependents reside in another. Indexation and residence-linking rules reduce or withhold family benefits for children living abroad even when the earner contributes fully in the host state. Appeals mean navigating two administrations; reunifying the household in the host country is often unaffordable, and enduring the reduction is the default.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, cross_border_households, payer,
    powerless, biographical, constrained, continental).

% Track claim rates, net fiscal contributions, and administrative outcomes for movers versus nationals; publish findings that routinely contradict the public scale of the asserted burdens, and supply the evidence base that courts and commissions draw on. They collect nothing from the rules and bear none of them.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__member_sovereignty_primary, social_policy_researchers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains redistributive insurance pools whose contributors and claimants could otherwise decouple across borders: residence-and-contribution conditions stop claimants from drawing on systems they never funded, keep contribution rates predictable for existing members, give receiving labor markets time to absorb inflows, and preserve the domestic political bargain in which voters consent to compulsory pooling because the pool's boundaries match the political community's.
% TRANSFER_FUNCTION: Moves payroll contributions and taxes paid by mobile workers into host-state insurance pools on terms that delay, reduce, or refuse matching entitlements; shifts the cost of unemployment spells, family support, and eventual old-age care toward sending states and households; and preserves the resulting fiscal margin for domestic benefit levels and tax relief.
% ABSENT_VOICES: Mobile workers and cross-border families hold no seat in the parliaments that write closure rules and no vote in the Council that vetoes reform; sending-state governments attend Council sessions but are persistently outvoted by receiving-state coalitions; the future returnees and home communities who will absorb deferred costs are represented by no one at the table.
% DISAPPEARANCE_RATIONALE: If closure authority vanished overnight, entitlements would follow contributions across borders immediately: host pools would reprice their risk, the domestic coalitions consenting to compulsory pooling would fracture along taxpayer lines, receiving labor markets would absorb inflows faster, and sending states would gain sudden leverage in portability bargaining — the architecture of national social insurance would renegotiate within years rather than persist unchanged.
% FOUNDING_PROBLEM: National social insurance was built on territorial reciprocity: fund and draw within the same jurisdiction. Mass intra-European mobility broke that symmetry — workers could contribute in one system and claim in another, or arrive to claim without contributing — threatening both pool finances and voter consent for redistribution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Court of Justice rulings on both expansive and restrictive lines repeatedly reaffirm member-state competence to organize their own social security systems; OECD and Commission portability studies document the cross-border coordination costs the founding problem addresses; sending-state submissions in infringement proceedings attest that the access asymmetry is real. None of these corroborators collects the fiscal margin the closure preserves — that accrues to receiving states, whose own attestations are therefore discounted as interested.
narrative_ontology:disappearance_verdict(federation_membership_obligations__member_sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__member_sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__member_sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_obligations__member_sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__member_sovereignty_primary, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__member_sovereignty_primary_tests).
:- end_tests(federation_membership_obligations__member_sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. The claimed type is tangled_rope because the structure possesses both halves: a genuine coordination function (keeping compulsory pooling financially and politically sustainable under mobility) and asymmetric extraction running through the same rules (contributors denied commensurate access with no actuarial warrant), held together by active enforcement. The metrics describe actual operation without being tuned to that claim. Extractiveness 0.44 is reading-indexed: by this reading's lights the closure core is legitimate reciprocity, and the value concedes only the overreach even this reading acknowledges — non-portable contribution losses, family-benefit cuts to full contributors, and screening that deters eligible claimants. Suppression 0.57 is a raw structural property, unscaled: legal-administrative barriers dominate (roughly 70 percent), with an internalized deterrence component (roughly 30 percent) in which eligible movers self-select out before applying; it is bounded by free-movement law, hence short of coercion. Theater 0.35 blends real verification work with a public burden discourse whose claimed scale routinely exceeds measured claim rates. Accessibility collapse is low-moderate (0.38) because alternative architectures — full portability, contributory federalism, harmonized floors — remain visible and periodically proposed; they are politically blocked, not conceptually foreclosed. Resistance 0.62 reflects continuous infringement litigation, sending-state coalitions, worker challenges, and contrary scholarship. The temporal record shows a crisis-driven ratchet cycle: crisis, scapegoating, tightening, calm, accumulation — extractiveness peaks at the crisis years (points 16 through 24) and plateaus after, while enforcement infrastructure ratchets monotonically upward, which is why suppression_requirement is tracked deliberately alongside the other series on one shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute different types from the same structure. From the receiving-state taxpayer seat the arrangement is the thing that keeps contribution rates predictable and benefit levels defensible; from the mobile worker seat it is a wall encountered precisely when employment gaps arrive. The agenda-setting seats split: national legislatures administer and defend the closure and compute a defender's position, while the Commission litigates against it and the Court adjudicates case by case — three seats nominally at one institutional power level with opposed directionalities. The same-level lateral pair is receiving versus sending state governments: formally symmetric (both institutional, both Council votes) but positioned oppositely because the closure's costs fall asymmetrically — one side collects the fiscal margin, the other absorbs the adjustment. Identity-lock operates on the defending seats: governing parties have fused national identity with the welfare model itself, so conceding portability reads as surrendering the state's self-definition, which forecloses bargains that a purely fiscal calculus would accept. Mobile workers hold latent coalition potential — unions, cross-border litigation networks, and sending-state sponsorship — that individual powerlessness understates.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations drive the derivation. Receiving-state taxpayers sit near the beneficiary pole (pool stability and the fiscal margin flow to them); mobile workers sit near the target pole (they fund systems that delay or refuse matching entitlements), with the incidence slightly damped by the retained movement right and by equal treatment once firmly established. Cross-border households occupy the extreme target position — contributions paid in full, benefits reduced on residence-linking grounds regardless. Sending-state governments are institutional targets bearing displaced adjustment costs. Domestic low-wage workers are the one seat where the automatic derivation misfires: declared beneficiary, the derivation places them deep on the beneficiary side, but the protection they receive is contingent on staying put, and the identical rules stand ready to bind them the day they move abroad — hence a single directionality override lifting the moderate atom to 0.2. The split agenda-setting seats derive from their structural data rather than from any single declaration: legislatures administer the closure, the Commission contests its edges, the Court decides between them.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy declaration: the founding problem — reconciling territorial reciprocity of social insurance with mass mobility — remains live under demographic aging, sustained east-west flows, and recurring fiscal stress, and the corroboration record (judicial, statistical, diplomatic) supports liveness from outside the benefiting parties. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure extraction ignores the real sustainability function that keeps compulsory pooling politically possible at all; reading it as innocent coordination ignores contributor-class exclusions with no actuarial warrant. Watch-items for lifecycle drift: theater_ratio crossing 0.5 during successive crises would signal proxy displacement (burden rhetoric replacing verification as the operative justification), and continued monotonic rise in suppression_requirement without matching actuarial findings would signal an enforcement ratchet serving the transfer rather than the pool. Conversely, durable portability agreements would signal convergence toward a managed rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_epsilon_indexicality,
    'How would the sibling readings of the federation_membership_obligations kernel re-author epsilon over the same standing arrangement?',
    'Generate the integration_primary and selective_solidarity files over the identical referent and compare computed per-seat classifications; divergence across the family is the expected diagnostic, not an error.',
    'Epsilon is reading-indexed: this file authors approximately 0.44 by member-sovereignty lights, whereas an integration_primary file would author markedly higher extraction over the identical referent. Cross-family comparisons that average epsilon are invalid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_epsilon_indexicality, conceptual, 'This constraint is one reading of a contested kernel; its metric values belong to that reading alone.').

omega_variable(
    bounded_solidarity_causal_basis,
    'Does unbounded welfare access actually erode domestic consent to redistribution, or create adverse selection, at policy-relevant magnitudes?',
    'Quasi-experimental evidence from portability expansions, bilateral coordination changes, and access-extension episodes: measure contribution-rate stability, claimant composition shifts, and redistribution-support survey responses before and after.',
    'If erosion and selection effects are negligible, the closure''s coordination justification collapses and the arrangement drifts toward pure rent-keeping; if material, the tangled_rope coordination half is strengthened and the reading''s core axiom gains empirical footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bounded_solidarity_causal_basis, empirical, 'Whether the founding causal claim of bounded solidarity survives contact with data.').

omega_variable(
    migrant_net_fiscal_incidence,
    'Are intra-EU mobile workers net contributors or net claimants in receiving-state systems, and does the sign hold across member states and economic cycles?',
    'Longitudinal administrative microdata linking contributions and claims per cohort of movers, disaggregated by state and phase of cycle; existing cross-sectional studies indicate net contribution, but cycle-phase stability is unresolved.',
    'If mobiles remain net contributors through downturns, the transfer function extracts from them at the margin and payer-seat effective extraction rises; if they flip net-negative in recessions, conditionality gains actuarial warrant and the reading''s justification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_net_fiscal_incidence, empirical, 'The fiscal sign of the transfer the closure administers.').

omega_variable(
    deterrence_vs_actuarial_administration,
    'Is the eligibility-screening machinery calibrated to verify entitlement, or to deter claims beyond any actuarial justification?',
    'Audit appeal reversal rates, processing-time disparities between nationals and comparable movers, refusal reasons, and take-up gaps unexplained by ineligibility; hostile-administration drift shows up as reversals and unexplained refusals concentrating on mover cohorts.',
    'Separates the rope-side verification component of enforcement from extraction-amplifying deterrent design; determines how much of the measured suppression serves the coordination function versus the transfer function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_vs_actuarial_administration, empirical, 'Whether enforcement serves eligibility or deterrence.').

omega_variable(
    suppression_structural_internalized_split,
    'How much of mover-side non-take-up reflects legal-administrative barriers versus internalized deterrence that persists when formal rules are relaxed?',
    'Take-up trajectories following simplification episodes: if claim rates jump discontinuously when rules ease, the prior gap was carried internally (learned futility, anticipated humiliation); if take-up tracks eligibility mechanically, suppression is structural.',
    'If substantially internalized, the constraint''s effective suppression exceeds the structural measure — movers carry the deterrent with them across jurisdictions — and the payer-seat classification shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_split, empirical, 'Structural versus internalized components of measured suppression.').

omega_variable(
    kernel_codification_under_determination,
    'Is the kernel best framed as the treaty text and coordination regulations themselves, or as the parliamentary-sovereignty legitimacy narrative layered above them that member-state governments treat as the binding commitment in negotiation?',
    'Compare which formulation predicts actual negotiating red lines and compliance behavior: if governments concede on codified rules while treating the sovereignty narrative as inviolable, the narrative is the operative kernel; if codified text governs, the current fixed_text framing holds.',
    'Under the narrative framing, kernel_codification shifts from fixed_text toward implicit, authority disperses across capitals, and the drift vector reads as practice_drift rather than authority_erosion — changing the commitment-system classification of the whole family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_codification_under_determination, conceptual, 'Two coherent framings of the kernel yield different commitment-system patterns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__member_sovereignty_primary, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 0, 0.17).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t4, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(fede_tr_t4, observed).
narrative_ontology:measurement(fede_tr_t8, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 8, 0.21).
narrative_ontology:measurement_basis(fede_tr_t8, observed).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(fede_tr_t12, observed).
narrative_ontology:measurement(fede_tr_t16, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(fede_tr_t16, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(fede_tr_t24, observed).
narrative_ontology:measurement(fede_tr_t28, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 28, 0.36).
narrative_ontology:measurement_basis(fede_tr_t28, observed).
narrative_ontology:measurement(fede_tr_t32, federation_membership_obligations__member_sovereignty_primary, theater_ratio, 32, 0.35).
narrative_ontology:measurement_basis(fede_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t4, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 4, 0.31).
narrative_ontology:measurement_basis(fede_be_t4, observed).
narrative_ontology:measurement(fede_be_t8, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 8, 0.33).
narrative_ontology:measurement_basis(fede_be_t8, observed).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 12, 0.37).
narrative_ontology:measurement_basis(fede_be_t12, observed).
narrative_ontology:measurement(fede_be_t16, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 16, 0.42).
narrative_ontology:measurement_basis(fede_be_t16, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(fede_be_t24, observed).
narrative_ontology:measurement(fede_be_t28, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 28, 0.45).
narrative_ontology:measurement_basis(fede_be_t28, observed).
narrative_ontology:measurement(fede_be_t32, federation_membership_obligations__member_sovereignty_primary, base_extractiveness, 32, 0.44).
narrative_ontology:measurement_basis(fede_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t4, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 4, 0.27).
narrative_ontology:measurement_basis(fede_su_t4, observed).
narrative_ontology:measurement(fede_su_t8, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 8, 0.3).
narrative_ontology:measurement_basis(fede_su_t8, observed).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 12, 0.35).
narrative_ontology:measurement_basis(fede_su_t12, observed).
narrative_ontology:measurement(fede_su_t16, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 16, 0.41).
narrative_ontology:measurement_basis(fede_su_t16, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 24, 0.52).
narrative_ontology:measurement_basis(fede_su_t24, observed).
narrative_ontology:measurement(fede_su_t28, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 28, 0.55).
narrative_ontology:measurement_basis(fede_su_t28, observed).
narrative_ontology:measurement(fede_su_t32, federation_membership_obligations__member_sovereignty_primary, suppression_requirement, 32, 0.57).
narrative_ontology:measurement_basis(fede_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__member_sovereignty_primary, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__member_sovereignty_primary, federation_membership_obligations__selective_solidarity).

% DUAL FORMULATION NOTE:
% The colloquial label 'EU free movement versus the welfare state' decomposes into three structurally distinct constraints forming the federation_membership_obligations family, per the epsilon-invariance principle: integration_primary (mobility rights constitutive, boundaries yield), this file (closure authority retained, mobility conditional), and selective_solidarity (rights tiered by contribution history). Each authors its own reading-indexed epsilon over the shared referent — integration_primary the highest, this file near 0.44, selective_solidarity intermediate — and no file hedges across readings. Upstream/downstream: the integration_primary rights vocabulary supplies what this reading's defenders argue against, while this reading's retained-discretion settlement creates the operational space in which selective_solidarity's contributory filtering diffuses through national statute. Every family member links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__member_sovereignty_primary, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
