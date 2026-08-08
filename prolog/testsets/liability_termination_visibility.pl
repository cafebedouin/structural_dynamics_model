% ============================================================================
% CONSTRAINT STORY: liability_termination_visibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_termination_visibility, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_termination_visibility
 *   human_readable: Untraceable Loss Termination Through Chained Guarantees, Securitization, and Bankruptcy Discharge
 *   domain: constitutional_political_economy/corporate_law/monetary_theory
 *
 * SUMMARY:
 *   This constraint names the structural fact that limited liability,
 *   securitization, insurance/reinsurance chains, and bankruptcy discharge do
 *   not eliminate loss — they relocate it, often through enough intermediary
 *   layers that no external party can reconstruct, at the time of failure,
 *   who ultimately bears the cost. The claim under contest is that a
 *   constitutional system worthy of the name must make this final resting
 *   place traceable to a specific claimant (the 'Skolnick test'), rather than
 *   letting the loss diffuse into an unaccountable public backstop. This
 *   story evaluates the standing arrangement — chained guarantees without
 *   mandated end-to-end traceability — as it currently operates, not the
 *   traceability regime the constraint's advocates would install. It sits
 *   inside the broader kernel question of how a proposed future acquires
 *   present purchasing power (future_claims_present_resources);
 *   loss-termination opacity is a structural consequence of whichever
 *   issuance reading a system adopts, but is analytically distinct from any
 *   single reading — it concerns what happens after a claim on the future
 *   fails to be realized, not how the claim was issued.
 *
 * KEY AGENTS:
 *   - parties_able_to_externalize_tail_risk_via_recursive_composition: primary beneficiary (institutional/arbitrage) — collects fees and spread while retaining the ability to relocate downside losses through chained entities
 *   - diffuse_uninformed_bearers_of_socialized_loss_taxpayers_junior_creditors_uninsured_third_parties: primary victim (powerless/trapped) — bears losses with no visibility into the chain and no standing to trace it
 *   - bankruptcy_courts_and_insolvency_administrators: agenda-setter with jurisdictionally bounded visibility
 *   - prudential_regulators_and_deposit_insurers: agenda-setter and nominal backstop administrator, dependent on regulated-entity disclosure
 *   - reinsurance_and_swap_counterparties: secondary beneficiary further along the opacity chain
 *   - the_skolnick_auditor_analytical_observer: analytical seat representing the proposed traceability test itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_termination_visibility, 0.81).
domain_priors:suppression_score(liability_termination_visibility, 0.72).
domain_priors:theater_ratio(liability_termination_visibility, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_termination_visibility, extractiveness, 0.81).
narrative_ontology:constraint_metric(liability_termination_visibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(liability_termination_visibility, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_termination_visibility, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(liability_termination_visibility, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_termination_visibility, snare).
narrative_ontology:human_readable(liability_termination_visibility, "Untraceable Loss Termination Through Chained Guarantees, Securitization, and Bankruptcy Discharge").
narrative_ontology:topic_domain(liability_termination_visibility, "constitutional_political_economy/corporate_law/monetary_theory").

domain_priors:requires_active_enforcement(liability_termination_visibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_termination_visibility, '14acdf87-b8ab-4012-8353-751ec7f0f6bd').
narrative_ontology:cs_kernel_codification('14acdf87-b8ab-4012-8353-751ec7f0f6bd', distributed).
narrative_ontology:cs_authority_grounding('14acdf87-b8ab-4012-8353-751ec7f0f6bd', distributed).
narrative_ontology:cs_created_at('14acdf87-b8ab-4012-8353-751ec7f0f6bd', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_termination_visibility, parties_able_to_externalize_tail_risk_via_recursive_composition).
narrative_ontology:constraint_victim(liability_termination_visibility, diffuse_uninformed_bearers_of_socialized_loss_taxpayers_junior_creditors_uninsured_third_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_termination_visibility, reinsurance_and_swap_counterparties).
narrative_ontology:constraint_vindicates(liability_termination_visibility, limited_liability_promotes_investment).
narrative_ontology:constraint_vindicates(liability_termination_visibility, securitization_efficiently_disperses_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Large financial institutions, sponsors, and originators structure exposures through layered special-purpose vehicles, reinsurance treaties, credit-default swaps, and limited-liability wrappers such that any single failure's ultimate loss-bearer is obscured across multiple legal entities and jurisdictions. They collect origination fees, spread income, and upside during good years while the downside, when realized, lands several steps removed from the entity that took the risk. They retain the ability to dissolve, spin off, or discharge the liable entity through bankruptcy while continuing operations elsewhere.
narrative_ontology:constraint_stakeholder(liability_termination_visibility, parties_able_to_externalize_tail_risk_via_recursive_composition, beneficiary,
    institutional, generational, arbitrage, global).

% When a bank collapses, an insurer becomes insolvent, or a chain of reinsurance counterparties fails to pay out, the loss does not vanish; it lands on taxpayers via bailout or deposit-insurance backstops, on junior creditors and policyholders left holding worthless claims after senior claims are satisfied, or on third parties (tort victims, uninsured counterparties) who discover the responsible entity has been dissolved or discharged. They had no visibility into the chain at the time risk was assumed and no standing to trace it after failure; their only recourse is post-hoc, underfunded, and often politically contingent.
narrative_ontology:constraint_stakeholder(liability_termination_visibility, diffuse_uninformed_bearers_of_socialized_loss_taxpayers_junior_creditors_uninsured_third_parties, payer,
    powerless, biographical, trapped, national).

% Administer the formal discharge and priority-of-claims process. Their procedures determine which claims survive and which are extinguished, but their jurisdiction typically stops at the entity before them; they are not chartered or resourced to trace loss through parent guarantees, reinsurance treaties, or affiliated SPVs domiciled elsewhere, so the visibility gap is partly a product of jurisdictional design rather than administrative failure alone.
narrative_ontology:constraint_stakeholder(liability_termination_visibility, bankruptcy_courts_and_insolvency_administrators, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(liability_termination_visibility, bankruptcy_courts_and_insolvency_administrators, observer).

% Set capital, reinsurance-disclosure, and resolution requirements meant to make institutions' loss-absorbing capacity legible, and stand as the explicit or implicit backstop (deposit insurance funds, resolution authorities) when institutions fail. Their statutory mandate is to prevent losses from becoming untraceable, but they are structurally dependent on the disclosures of the entities they regulate and are frequently understaffed relative to the complexity of the chains they must supervise, and are politically pressured to socialize losses quickly rather than trace them fully during a crisis.
narrative_ontology:constraint_stakeholder(liability_termination_visibility, prudential_regulators_and_deposit_insurers, agenda_setter,
    institutional, generational, constrained, national).

% Sit further along the chain, collecting premium for assuming risk that has already been passed once or twice. They can themselves fail, retrocede, or become insolvent in a way that is not visible to the original policyholder or creditor at the base of the chain, and their own jurisdiction of domicile may shield them from the claims process the base creditor is relying on.
narrative_ontology:constraint_stakeholder(liability_termination_visibility, reinsurance_and_swap_counterparties, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(liability_termination_visibility, reinsurance_and_swap_counterparties, excluded).

% Represents the external test proposed by the constraint: given any financial failure, can a competent auditor with subpoena power reconstruct, entity by entity and guarantee by guarantee, the specific party who ultimately bears the loss? This seat has no material stake; it exists to make visible whether the chain terminates in an identifiable claimant or diffuses into an unaccountable public backstop before any resolution or bailout occurs.
narrative_ontology:constraint_stakeholder(liability_termination_visibility, the_skolnick_auditor_analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_termination_visibility, parties_able_to_externalize_tail_risk_via_recursive_composition).
narrative_ontology:fixing_cost_class(liability_termination_visibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Limited liability, securitization, and reinsurance genuinely solve real coordination problems: they allow risk to be pooled and diversified across many parties who could not individually bear a catastrophic loss, and they allow investors to commit capital to risky ventures without unlimited personal exposure, which mobilizes capital that would otherwise sit idle.
% TRANSFER_FUNCTION: The arrangement moves realized tail losses from the parties who assumed and were compensated for the risk (through fees, premiums, and spread income collected during solvent years) to diffuse, uninformed bearers who never priced or consented to the risk — taxpayers through bailouts, junior creditors and policyholders through discharge, and uninsured third parties through unrecoverable claims — while the origination gains remain with the risk-assuming parties.
% ABSENT_VOICES: The eventual loss-bearers — future taxpayers, junior creditors, and uninsured third parties — are not present when the chain of guarantees, reinsurance treaties, and liability shields is constructed; they only enter the picture after a failure has already occurred and the chain has already been designed to obscure where the loss will land. Legislatures voting on bailout mechanisms are themselves often informationally dependent on the failing institutions for an account of who bears what.
% DISAPPEARANCE_RATIONALE: If liability-termination opacity vanished — i.e., if every chain of guarantees, securitizations, and discharges were fully traceable to a final claimant in real time — risk would have to be priced and held by whoever the traceable chain revealed as the ultimate bearer, which would sharply raise the cost of tail-risk-bearing activities for the parties currently able to externalize it, and would make bailout politics far harder to conduct quietly. Insurance, securitization, and limited liability would not disappear, but their current capacity to terminate visibly at a point other than the true loss-bearer would.
% FOUNDING_PROBLEM: Limited liability and insurance/reinsurance were built to solve a genuine problem: without them, capital formation and risk pooling would be severely constrained because individual investors and insurers could not bear unlimited personal exposure to catastrophic loss. The constitutional traceability requirement (the Skolnick test) was proposed to solve a second, later problem — that these same mechanisms, chained together across entities and jurisdictions, had become vehicles for relocating losses to parties with no visibility or standing rather than merely pooling risk.
% FOUNDING_PROBLEM_CORROBORATION: Central banks, deposit insurers, and post-crisis inquiry commissions (e.g., legislative and judicial post-mortems following major bank and insurer failures) have attested, from outside the beneficiary set, that loss chains were frequently untraceable in real time and that public backstops absorbed losses whose origin could not be reconstructed until years after the fact. The beneficiary institutions themselves continue to attest that the original coordination function (capital mobilization, risk pooling) remains live and justifies the current opacity; no corroborating source outside the risk-assuming institutions has affirmed that the opacity itself, as opposed to the underlying pooling function, remains necessary.
narrative_ontology:disappearance_verdict(liability_termination_visibility, world_rearranges).
narrative_ontology:founding_problem_status(liability_termination_visibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_termination_visibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-08',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(liability_termination_visibility, 'none', 1).
narrative_ontology:epsilon_provenance(liability_termination_visibility, 0.81, 'claude-sonnet-5', 'c2_monetary_architecture_2026_20260808_170220', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_termination_visibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_termination_visibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_termination_visibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because the structure's core function — allowing risk-assuming parties to collect origination and spread income while the eventual loss lands on parties who never priced or consented to it — is a genuine, measurable transfer, not incidental friction. Suppression is authored high (0.72) but somewhat lower than extractiveness because the opacity is maintained less by direct coercion than by structural complexity, jurisdictional fragmentation, and the genuine difficulty of auditing multi-entity chains — the suppression is partly a byproduct of legitimate legal complexity (corporate personhood, private contract, cross-border reinsurance) being available as camouflage rather than a single enforced rule. Theater ratio rises over the measured interval (0.25 to 0.58) reflecting an accumulating layer of disclosure regimes, stress tests, and resolution-planning requirements (living wills, Dodd-Frank Title II, Solvency II reporting) that increasingly perform traceability without actually closing the chain-tracing gap the Skolnick test targets — regulatory theater substituting for the underlying capability. Accessibility collapse (0.62) and resistance (0.55) reflect that alternatives to opaque chaining (mandated end-to-end traceability registries, position-level transparency requirements) are technically available and have been proposed repeatedly but have not been adopted at scale, and resistance to opacity comes mainly from post-crisis reformers and academic critics rather than from the diffuse victim class itself, who typically cannot identify their exposure until after a failure occurs.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats (bankruptcy courts, prudential regulators) the arrangement looks like an imperfectly resourced but good-faith effort at traceability within jurisdictional limits; from the victim seat it looks like a structure specifically optimized to terminate visibly at a point other than the true loss-bearer. The engine should compute these as structurally different experiences of the same chain given the differing power, exit, and directionality inputs — the regulators' constrained exit and institutional power differ meaningfully from the victims' trapped exit and powerless standing, even though both nominally sit 'outside' the beneficiary group.
 *
 * DIRECTIONALITY LOGIC:
 *   Parties able to externalize tail risk via recursive composition sit at the low end of directionality: they are compensated during the solvent phase and retain arbitrage-grade exit (restructuring, spin-off, relocation of the liable entity) when failure approaches. The diffuse victim class sits at the high end: trapped exit options, no advance knowledge of the chain, and no standing to intervene before the loss crystallizes — this is a case where directionality derivation from beneficiary/victim declarations plus exit options should place this group very near full-target without needing an override. Reinsurance and swap counterparties occupy an intermediate beneficiary position — mobile rather than arbitrage-grade exit, since they can exit specific treaties but remain subject to counterparty and regulatory risk in ways the primary originators can better evade.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination function — pooling catastrophic risk so that capital formation is not paralyzed by unlimited personal liability — remains genuinely live; this constraint does not classify limited liability or insurance per se as extractive. What has drifted is the traceability of the chain built on top of that legitimate function: the original problem (mobilizing capital under bounded risk) persists, but the mechanism has been recursively composed (securitization of securitizations, reinsurance of reinsurance, guarantee chains across jurisdictions) far beyond what the pooling function requires, in ways that now serve primarily to relocate losses to parties with no visibility rather than to diversify risk efficiently. Classifying this as snare rather than tangled_rope requires care: a strict tangled_rope reading is defensible (there is a real coordination function — capital mobilization — running through the same structure that produces the extraction), but this story's authored judgment is that the coordination benefit has become sufficiently decoupled from the extraction mechanism, and the victim class sufficiently unable to consent, price, or exit, that the coordination story now functions predominantly as cover rather than as a live justification for the chain's opacity specifically (as opposed to for limited liability and insurance generally, which are not what this constraint targets).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_separability_in_recursive_chains,
    'Is the loss-obscuring effect of chained securitization, reinsurance, and limited-liability wrappers separable from the genuine risk-pooling function these mechanisms also perform, or is opacity a structurally necessary byproduct of pooling risk across many parties at scale?',
    'Compare jurisdictions or historical periods with mandated end-to-end position-level traceability registries (e.g., certain post-2008 derivatives reporting regimes) against those without, controlling for pooling volume, to see whether traceability materially degrades capital mobilization or merely removes the opacity.',
    'If separable, this constraint is closer to a clean snare riding on legitimate pooling infrastructure; if inseparable, the classification shifts toward tangled_rope, since the coordination and extraction functions would be running through the same structural mechanism rather than being merely co-located.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability_in_recursive_chains, empirical, 'Whether loss-obscuring opacity is a necessary feature of large-scale risk pooling or a severable, purely extractive addition.').

omega_variable(
    regulatory_capacity_vs_regulatory_capture,
    'Does the persistent untraceability of loss chains reflect a genuine technical/jurisdictional limit on regulators'' ability to trace multi-entity, cross-border chains, or does it reflect regulatory capture in which regulators could trace the chains but choose not to press disclosure requirements that would impose costs on politically powerful institutions?',
    'Examine instances where regulators were given expanded subpoena or disclosure authority (e.g., post-crisis resolution-planning mandates) and measure whether traceability improved commensurately with the expanded authority, or whether gaps persisted despite adequate legal tools.',
    'If a genuine capacity limit, the theater_ratio trend reflects honest but insufficient effort; if capture, the rising theater_ratio indicates deliberate performance substituting for available but unexercised capability, which would strengthen the snare classification over a tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_vs_regulatory_capture, conceptual, 'Whether opacity persistence is a capacity problem or a captured-incentive problem for regulators.').

omega_variable(
    victim_coalition_feasibility,
    'Can the diffuse, uninformed victim class (taxpayers, junior creditors, uninsured third parties) organize into an effective coalition to demand end-to-end traceability requirements, given that each individual instance of loss-bearing is small relative to the beneficiary concentration but the aggregate is large?',
    'Track post-crisis legislative reform efforts (e.g., living-will requirements, resolution authorities) to see whether diffuse public pressure following a visible failure event produced durable traceability mandates or whether reforms were captured/watered down before implementation.',
    'If coalition formation is feasible following salient failures, the constraint may be better modeled as a scaffold-adjacent temporary imbalance correctable through periodic reform waves; if coalition formation systematically fails, the snare classification with entrenched suppression is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_feasibility, empirical, 'Whether the powerless victim class can achieve coalition power sufficient to force traceability reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_termination_visibility, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_termination_visibility, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t8, liability_termination_visibility, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(liab_tr_t8, observed).
narrative_ontology:measurement(liab_tr_t16, liability_termination_visibility, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(liab_tr_t16, observed).
narrative_ontology:measurement(liab_tr_t24, liability_termination_visibility, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(liab_tr_t24, observed).
narrative_ontology:measurement(liab_tr_t32, liability_termination_visibility, theater_ratio, 32, 0.54).
narrative_ontology:measurement_basis(liab_tr_t32, observed).
narrative_ontology:measurement(liab_tr_t40, liability_termination_visibility, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(liab_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_termination_visibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t8, liability_termination_visibility, base_extractiveness, 8, 0.51).
narrative_ontology:measurement_basis(liab_be_t8, observed).
narrative_ontology:measurement(liab_be_t16, liability_termination_visibility, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(liab_be_t16, observed).
narrative_ontology:measurement(liab_be_t24, liability_termination_visibility, base_extractiveness, 24, 0.72).
narrative_ontology:measurement_basis(liab_be_t24, observed).
narrative_ontology:measurement(liab_be_t32, liability_termination_visibility, base_extractiveness, 32, 0.78).
narrative_ontology:measurement_basis(liab_be_t32, observed).
narrative_ontology:measurement(liab_be_t40, liability_termination_visibility, base_extractiveness, 40, 0.81).
narrative_ontology:measurement_basis(liab_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_termination_visibility, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t8, liability_termination_visibility, suppression_requirement, 8, 0.55).
narrative_ontology:measurement_basis(liab_su_t8, observed).
narrative_ontology:measurement(liab_su_t16, liability_termination_visibility, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(liab_su_t16, observed).
narrative_ontology:measurement(liab_su_t24, liability_termination_visibility, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(liab_su_t24, observed).
narrative_ontology:measurement(liab_su_t32, liability_termination_visibility, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(liab_su_t32, observed).
narrative_ontology:measurement(liab_su_t40, liability_termination_visibility, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(liab_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_termination_visibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(liability_termination_visibility, 0.12).
narrative_ontology:affects_constraint(liability_termination_visibility, future_claims_present_resources_issuance_as_deliberative_judgment).
narrative_ontology:affects_constraint(liability_termination_visibility, future_claims_present_resources_issuance_as_endogenous_credit_multiplication).
narrative_ontology:affects_constraint(liability_termination_visibility, bank_resolution_authority_backstop_design).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the future_claims_present_resources kernel but is not itself a reading of that kernel — it concerns loss-termination after a claim fails, not issuance legitimacy. It is linked because the endogenous-credit-multiplication reading structurally generates more chained private liability (and hence more opportunities for opacity) than the deliberative-judgment reading, which centralizes and makes visible the quantity decision. A full decomposition would also separate 'limited liability as capital-mobilization mechanism' (likely a rope or tangled_rope in its own right, largely uncontested) from 'chained cross-entity opacity in loss termination' (this story, evaluated as snare) — these are ε-distinct claims sharing a legal-institutional substrate but should not be merged into one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
