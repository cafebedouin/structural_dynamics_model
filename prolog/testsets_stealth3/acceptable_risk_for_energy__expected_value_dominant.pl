% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Energy Risk Acceptability Criterion
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   The expected-value-dominant acceptability regime determines whether
 *   energy facilities — above all nuclear — may be built and operated by
 *   comparing annualized expected costs against expected benefits including
 *   monetized climate benefits, with rare events entering strictly as
 *   probability-times-consequence products. The framework is presented as
 *   neutral decision science; its critics read it as an arithmetic that
 *   launders the transfer of concentrated tail risk onto parties who never
 *   agreed to hold it. KEY AGENTS (by structural relationship):
 *   energy_regulators: Agenda setter (institutional/constrained) —
 *   administers the determination and defends the annualized discipline;
 *   nuclear_operators: Primary beneficiary (powerful/constrained) — collects
 *   capped-liability licensure; baseload_capital_markets: Secondary
 *   beneficiary (institutional/arbitrage) — collects bankability;
 *   pra_professionals: Identity-locked beneficiary
 *   (organized/identity_locked) — careers fused to the method;
 *   climate_policy_bodies: Institutional beneficiary
 *   (institutional/constrained) — administers the climate-benefit term;
 *   fenceline_communities_near_reactors: Primary target (powerless/trapped) —
 *   absorbs concentrated tails; future_generations_liable_for_waste:
 *   Intergenerational target (powerless/trapped, civilizational horizon);
 *   taxpayer_liability_backstops: Residual target with indirect benefit
 *   (moderate/constrained); tail_risk_advocates_public_health_scientists:
 *   Excluded voice (moderate/mobile); independent_risk_analysts: Analytical
 *   observer. The claim/metrics split is deliberate: claimed_type is
 *   tangled_rope from my structural read, while the authored metrics describe
 *   the moderate extraction this reading honestly concedes — the engine
 *   computes each seat independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.38).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.45).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value-Dominant Energy Risk Acceptability Criterion").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'ecda49f1-b991-4b05-af34-68806bd7a8a8').
narrative_ontology:cs_kernel_codification('ecda49f1-b991-4b05-af34-68806bd7a8a8', formalized).
narrative_ontology:cs_authority_grounding('ecda49f1-b991-4b05-af34-68806bd7a8a8', expertise).
narrative_ontology:cs_interpretation_layer_present('ecda49f1-b991-4b05-af34-68806bd7a8a8').
narrative_ontology:cs_reading_relation('ecda49f1-b991-4b05-af34-68806bd7a8a8', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('ecda49f1-b991-4b05-af34-68806bd7a8a8', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('ecda49f1-b991-4b05-af34-68806bd7a8a8', foundational, expected_value_sufficiency_for_acceptability).
narrative_ontology:cs_axiom_status(expected_value_sufficiency_for_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('ecda49f1-b991-4b05-af34-68806bd7a8a8', expected_value_sufficiency_for_acceptability, instrumental).
narrative_ontology:cs_axiom('ecda49f1-b991-4b05-af34-68806bd7a8a8', secondary, rare_events_enter_as_probability_weighted_terms).
narrative_ontology:cs_axiom_status(rare_events_enter_as_probability_weighted_terms, holdable).
narrative_ontology:cs_axiom_grounding('ecda49f1-b991-4b05-af34-68806bd7a8a8', rare_events_enter_as_probability_weighted_terms, empirically_contingent).
narrative_ontology:cs_reference_frame('ecda49f1-b991-4b05-af34-68806bd7a8a8', expected_value_actuarial_baseline).
narrative_ontology:cs_drift_state('ecda49f1-b991-4b05-af34-68806bd7a8a8', contemporary_post_fukushima_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ecda49f1-b991-4b05-af34-68806bd7a8a8', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, baseload_capital_markets).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, pra_professionals).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_bodies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, fenceline_communities_near_reactors).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, future_generations_liable_for_waste).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, taxpayer_liability_backstops).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, energy_regulators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, taxpayer_liability_backstops).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, expected_utility_decision_theory).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, probabilistic_risk_assessment_methodology).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, monetized_climate_benefit_accounting).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and administers the acceptability criteria, commissions the probabilistic studies that feed them, helps legislate liability-cap parameters, and defends the annualized discipline against post-accident political surges. Collects a defensible, auditable mandate from running the framework. Cannot abandon the statutory mandate that binds it, and faces capture pressure simultaneously from licensees and from opposition publics after every visible accident.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, energy_regulators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, energy_regulators, beneficiary).

% Builds and operates reactors under licenses granted through the annualized determination. Statutory liability caps mean the worst-case costs of a severe accident stop at the cap; insurance premiums are sized to expected value, not to tail magnitude. Site-specific sunk capital and decades-long regulatory relationships make redeployment slow. Lobbies continuously to preserve the accounting that prices its residual tail exposure favorably.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators, beneficiary,
    powerful, generational, constrained, global).

% Finances plant construction and holds utility debt and equity. Expected-value licensing plus liability capping make long-horizon cashflows insurable and bankable. Capital can move to any jurisdiction or asset class quickly, so an unfavorable shift in the risk arithmetic simply redirects investment rather than trapping it.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, baseload_capital_markets, beneficiary,
    institutional, immediate, arbitrage, global).

% Consultancies, national-laboratory analysts, and agency technical staff produce the probabilistic models the determination runs on. Careers, methodological canons, peer standing, and standards-board seats are all invested in the annualized framework. Moving to a rival risk ontology would mean abandoning the professional identity built inside this one.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, pra_professionals, beneficiary,
    organized, biographical, identity_locked, global).

% Treaty bodies and national climate ministries book firm low-carbon output as a climate benefit inside the acceptability ledger. They depend on the framework keeping firm clean capacity financeable and report decarbonization progress against it. Leaving the framework would strip out the benefit term they administer and complicate portfolio commitments already made.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_bodies, beneficiary,
    institutional, generational, constrained, global).

% Lives within the evacuation shadow of licensed sites. Hosts plant employment, contractor wages, and the municipal tax base, so leaving carries economic ruin alongside radiation exposure. Their concentrated tail exposure enters the determination as a small annualized figure spread across the whole population served, while the actual low-probability high-consequence event would land here. Home equity, kin networks, and local wages are tied to the facility's continued operation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, fenceline_communities_near_reactors, payer,
    powerless, generational, trapped, regional).

% Inherits spent-fuel storage and geological repository stewardship obligations that today's determination books as a discounted engineering cost-line. Holds no seat in any acceptability proceeding, cannot consent to or refuse the burden, and cannot exit it. The discount rate applied to their liability is chosen entirely by the generation deciding now.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations_liable_for_waste, payer,
    powerless, civilizational, trapped, continental).

% Stands behind statutory liability caps: above the cap, severe-accident cleanup and compensation costs default to public balance sheets under Price-Anderson-type regimes. Receives cheap firm power and decarbonization as spillover benefits from the same arrangement. Cannot opt out of the backstop short of emigration.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, taxpayer_liability_backstops, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, taxpayer_liability_backstops, beneficiary).

% Publishes fat-tail, irreversibility, and intergenerational-equity critiques; testifies at hearings; files comments on licensing dockets. Their valuation basis is admissible as argument but carries no weight in the annualized arithmetic that decides the outcome. Journals, advocacy networks, and international forums remain open to them, so exit from the discourse is easy even though entry into the determination is blocked.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_advocates_public_health_scientists, excluded,
    moderate, biographical, mobile, global).

% Academic decision theorists and comparative-risk researchers audit the framework from outside licensing processes. They document where probability estimates diverge by orders of magnitude, how discount-rate choices embed distributional judgments, and how sibling risk ontologies would redistribute the same physical exposures across different bearer sets.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, independent_risk_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one commensurable, auditable metric for comparing heterogeneous energy hazards — reactor accidents, waste exposure, air pollution mortality, climate forcing — enabling consistent licensing, insurable financing, cross-jurisdictional benchmarking, and portfolio planning. Prevents both regulatory paralysis (no facility passes any intuitively stated bar) and single-event policy whiplash.
% TRANSFER_FUNCTION: Moves residual, uninsured risk-bearing from operators and ratepayers-at-large onto the least-represented bearers — fence-line communities absorbing concentrated tail exposure and future generations holding waste liabilities — while moving decision authority to technical-economic analysts. Moves capital toward technologies whose profiles price favorably under annualized accounting, and moves liability-cap headroom from public balance sheets to private operators.
% ABSENT_VOICES: Tail-risk and intergenerational-ethics advocates testify but enter without valuation weight; the unborn hold no seat at all; fence-line residents participate through comment periods that cannot alter the arithmetic pricing their exposure. They are located in hearing transcripts, ethics literature, and docket filings — adjacent to the process, outside the determination.
% DISAPPEARANCE_RATIONALE: Overnight removal would force licensing back to ad hoc political judgment per technology and per accident; nuclear financiers would withdraw absent actuarial cover; insurance pools and liability-cap regimes would lose their pricing basis; international benchmarking would fragment; and each jurisdiction would improvise thresholds after the next incident, restoring the whiplash cycle the framework was built to damp.
% FOUNDING_PROBLEM: Post-war deployment of hazardous energy technology outran intuitive moral judgment: how can a regulator justify accepting or rejecting facilities whose worst outcomes are rare, historically unprecedented, and unevenly distributed, with a warrant that survives legislative oversight, judicial review, and cross-technology consistency?
% FOUNDING_PROBLEM_CORROBORATION: National Academies institutional reviews of federal risk assessment and public-health scientific testimony attest from outside the benefiting parties that the heterogeneous-risk-comparison problem remains live; administrative-law proceedings in which agencies must defend their methodologies on the record corroborate it further. No attestation comes from the residual bearers themselves — fence-line and future-generation objections appear in dockets, but no voice from that seat affirms the founding framing; that absence is itself signal.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.38 at interval end) is moderate: the reading's own lights concede real but bounded transfer — liability caps leave residuals on public balance sheets, probability weighting books concentrated tails as dispersed annuities, and discount rates shrink inherited waste burdens — while the framework genuinely prices chronic risks (air pollution, carbon) that intuition underweights. Suppression (0.45) is deliberately authored BELOW the enforcement series endpoint (0.61) because the scalar is a raw, unscaled structural property mixing two components: compelled compliance inside the regulated domain (strong — no reactor operates outside the framework) versus weak suppression of rival risk framings outside it (German phase-out, Italian referenda, and tail-dominant scholarship all remained lawful). Theater ratio (0.36) reflects growing precision theater: decimal-point expected values convey certitude the underlying probability estimates cannot support. Accessibility collapse (0.28) is low — sibling readings remain fully accessible alternatives. Resistance (0.55) is substantial — referenda, phase-outs, and licensing litigation recur. All three series run on one shared grid (t=0..50, mapped to roughly 1975-2025) with every tracked metric authored at every point. The trajectories are shock-shaped rather than monotonic: enforcement steps up after TMI (t~4), Chernobyl (t~11), 9/11 (t~26), and Fukushima (t~36), then partially relaxes — a crisis-response-decay cycle in which the shock response is partly protective hardening and partly legitimacy maintenance. Base-properties scalars are measured at interval end (t=50), on the post-partial-relaxation phase of the most recent cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical structural data. From the operator seat the framework is actuarially fair coordination it helped design: premiums match expected value, caps are legislated openly. From the fenceline and future-generation seats the same arithmetic operates as enforced extraction: their exposure is discounted by factors they never chose, and their exit is trapped or nonexistent. The regulator seat experiences a defensible public mandate; the PRA-professional seat experiences professional vindication; the capital seat experiences ordinary bankability. None of these perceptions is authored here — each is computed from power, exit, and declared position, and their divergence is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: operators and capital markets collect directly from the framework's pricing; PRA professionals collect standing and fees; climate bodies collect the benefit term they administer. Victims derive high directionality: fenceline communities and future generations are trapped bearers of the discounted remainder, sitting near the full-target end; taxpayer backstops are also targets but with material secondary gains. One override is authored: taxpayer_liability_backstops would derive a near-full-target d (~0.82) from payer role plus constrained exit plus moderate power, but the derivation cannot see the cheap-firm-power and decarbonization spillovers flowing to the same seat, so d is corrected downward to 0.68 — an indirect-beneficiary correction, not a demotion of their target position. Fenceline communities receive job and tax-base offsets too, but these are locally captured and do not approach their tail exposure; no override is warranted there.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a defensible, repeatable warrant for accepting unevenly distributed catastrophic risk — is still live, so this is not a resolved-mandatrophy case. The tangled_rope claim is what prevents mislabeling in both directions: reading the framework as pure coordination (rope) would erase the measurable transfer of uninsured tails onto non-consenting bearers; reading it as pure extraction (snare) would erase the genuine commensuration function that keeps multi-technology regulation coherent, insurable, and resistant to single-event panic. The forward risk to watch is piton drift: if decisive licensing migrates to sibling readings or to jurisdictions abandoning the framework, the annualized apparatus could persist as precision performance — models maintained, decimals published, decisions made elsewhere. Rising theater_ratio alongside falling decision relevance is the signature; the current series shows mild upward drift only.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_reading_divergence,
    'This constraint is one reading (expected_value_dominant) of kernel acceptable_risk_for_energy; what structurally changes if a jurisdiction or era adopts catastrophic_tail_dominant or comparative_risk_dominant instead?',
    'Track adoption events: post-Fukushima German and Japanese policy shifts, SMR-era licensing reforms, IAEA guidance revisions; classify each jurisdiction-era by operative reading and compare victim sets, epsilon, and waste treatment across readings.',
    'Under catastrophic_tail_dominant, nuclear re-enters the victim set, waste becomes an intergenerational burden rather than an engineering cost-line, and suppression of expected-value framing rises sharply; under comparative_risk_dominant the absolute-threshold question dissolves and acceptability tracks the competitor mix. Classification of the whole family flips with adoption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_reading_divergence, conceptual, 'Committer structure: reading-indexed divergence across the acceptable-risk-for-energy kernel.').

omega_variable(
    rare_event_probability_uncertainty,
    'Core-melt frequencies and repository-breach probabilities carry order-of-magnitude epistemic uncertainty; is the probability-times-consequence product stable enough to bear acceptability determinations?',
    'Structured expert elicitations with convergence testing, accumulated operating experience, and sensitivity analysis of licensing outcomes across the published probability band.',
    'If licensing outcomes flip across the credible probability band, the framework''s effective behavior approaches the catastrophic_tail_dominant reading regardless of its formal commitment, and epsilon becomes unstable in the way DP-001 forbids for a single constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rare_event_probability_uncertainty, empirical, 'Whether p-by-c products are robust to heavy-tailed probability estimate uncertainty.').

omega_variable(
    residual_tail_consent_status,
    'Is bearing of residual uninsured tail risk by non-consenting fence-line populations extraction within this reading''s own lights, or legitimate actuarial pricing of a compensated position?',
    'Willingness-to-accept compensation studies in host communities, siting-conflict records, and analysis of whether existing property-value and tax-base offsets approach compensated-exposure benchmarks.',
    'If the residual is judged uncompensated and unconsented even by the reading''s own welfare standards, epsilon rises materially and the payer seats'' computed classifications harden toward snare-flavored extraction; if compensation is judged adequate, the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_tail_consent_status, conceptual, 'Consent and compensation status of the residual tail risk this framework transfers.').

omega_variable(
    liability_cap_residual_share,
    'What fraction of severe-accident total cost falls outside statutory liability caps onto public backstops?',
    'Forensic reconstruction of Price-Anderson-type regimes against realized costs (Fukushima cleanup, TMI litigation history) and probabilistic cost curves for severe scenarios.',
    'A large uncapped residual means the authored epsilon understates transfer to taxpayers; a small residual supports the reading''s claim that private pricing covers the relevant risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_cap_residual_share, empirical, 'Size of the publicly backstopped remainder behind liability caps.').

omega_variable(
    climate_displacement_counterfactual,
    'Does the climate-benefit term in the acceptability numerator depend on a displacement counterfactual (against gas or coal generation) whose size varies with grid mix and electrification pathway?',
    'Dispatch and capacity-expansion modeling under decarbonization scenarios with and without firm low-carbon capacity.',
    'If the displacement benefit shrinks as grids decarbonize, the framework''s favorable arithmetic for firm nuclear weakens, loosening the constraint''s grip on the victim set and accelerating migration toward sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_displacement_counterfactual, conceptual, 'Counterfactual dependence of the climate-benefit term that anchors this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.2).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 10, 0.23).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 20, 0.27).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 30, 0.29).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.33).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 50, 0.36).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 50, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'acceptable nuclear/energy risk'. The label conflates three structurally distinct claims with different epsilon, victim sets, and enforcement profiles: this file owns the expected-value reading (everything prices annually; tails enter as p-by-c); catastrophic_tail_dominant owns the tail-dominant claim (irreversibility and intergenerational burden dominate; epsilon authored far higher from its seat); comparative_risk_dominant owns the relative-threshold claim (no absolute bar). This reading sits upstream of the comparative reading — its annualized outputs supply the numbers comparative assessments tabulate — and stands in unresolved coexistence with the tail reading, which directly contests its foundational axiom rather than depending on it. Per the epsilon-invariance principle these are three constraints, not one constraint viewed from three angles; each is authored separately and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
