% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__shared_liability, []).

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
 *   constraint_id: liability_attribution__shared_liability
 *   human_readable: Joint AI Liability Distributed Along the Value Chain by Causal Contribution and Control
 *   domain: technology governance/legal theory/regulatory design
 *
 * SUMMARY:
 *   A regulatory regime assigns liability for AI-mediated harm jointly across
 *   the value chain, apportioning shares by assessed causal contribution and
 *   degree of control: capability creators, adapters, integrators, and
 *   deployers each answer for the portion of a failure attributable to their
 *   layer. Apportionment proceeds through technical contribution assessments,
 *   standardized contractual indemnification, and court adjudication;
 *   mandatory and voluntary insurance grew up around it immediately. The
 *   intended effect is that every participant holds a marginal safety
 *   incentive and no injured party is stranded behind a judgment-proof
 *   operator. The observed effect adds a second layer: an insurance and
 *   compliance economy has formed on top of the allocation machinery,
 *   apportionment increasingly settles by bargaining rather than evidence,
 *   and contractual allocation drifts toward weak bargaining positions. KEY
 *   AGENTS (by structural relationship): - ai_regulators: Agenda setter
 *   (institutional/analytical) - enacts and administers the attribution
 *   formula - foundation_model_developers: Primary target
 *   (powerful/constrained) - upstream capability creators bearing assessed
 *   shares - enterprise_ai_deployers: Primary target (organized/constrained)
 *   - integration-layer actors facing first contact with harm -
 *   small_value_chain_suppliers: Secondary target (powerless/constrained) -
 *   absorb contractual pass-through of allocated shares - liability_insurers:
 *   Principal collector (institutional/arbitrage) - premiums across both
 *   tiers; pricing shapes effective allocation - compliance_legal_complex:
 *   Secondary collector (organized/mobile) - runs the assessment and
 *   indemnification apparatus - ai_harm_claimants: Intended beneficiary
 *   carrying litigation burden (powerless/constrained) -
 *   open_source_contributors: Excluded voice (moderate/identity_locked) -
 *   reached by formulas they never helped write - academic_tort_theorists:
 *   Analytical observer (analytical/analytical) This story is one reading of
 *   the liability_attribution kernel; the sibling readings are separate files
 *   with their own epsilon and beneficiary/victim structures, linked via
 *   network.affects_constraints. The claim/metric relationship is deliberate:
 *   the reading is CLAIMED as tangled_rope on structural grounds while the
 *   authored metrics independently describe rising extractive operation - the
 *   divergence, if the engine computes one, is the datum.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.62).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.45).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.62).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint AI Liability Distributed Along the Value Chain by Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology governance/legal theory/regulatory design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, 'c6d58370-0139-4a1c-ad90-b43279eccb8e').
narrative_ontology:cs_kernel_codification('c6d58370-0139-4a1c-ad90-b43279eccb8e', formalized).
narrative_ontology:cs_authority_grounding('c6d58370-0139-4a1c-ad90-b43279eccb8e', lineage).
narrative_ontology:cs_interpretation_layer_present('c6d58370-0139-4a1c-ad90-b43279eccb8e').
narrative_ontology:cs_reading_relation('c6d58370-0139-4a1c-ad90-b43279eccb8e', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('c6d58370-0139-4a1c-ad90-b43279eccb8e', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('c6d58370-0139-4a1c-ad90-b43279eccb8e', foundational, responsibility_tracks_causal_contribution_and_control).
narrative_ontology:cs_axiom_status(responsibility_tracks_causal_contribution_and_control, holdable).
narrative_ontology:cs_axiom_grounding('c6d58370-0139-4a1c-ad90-b43279eccb8e', responsibility_tracks_causal_contribution_and_control, deontological).
narrative_ontology:cs_axiom('c6d58370-0139-4a1c-ad90-b43279eccb8e', secondary, multi_actor_harms_require_multi_pocket_recovery_paths).
narrative_ontology:cs_axiom_status(multi_actor_harms_require_multi_pocket_recovery_paths, holdable).
narrative_ontology:cs_axiom_grounding('c6d58370-0139-4a1c-ad90-b43279eccb8e', multi_actor_harms_require_multi_pocket_recovery_paths, instrumental).
narrative_ontology:cs_reference_frame('c6d58370-0139-4a1c-ad90-b43279eccb8e', proportional_attribution_by_contribution_and_control).
narrative_ontology:cs_drift_state('c6d58370-0139-4a1c-ad90-b43279eccb8e', contemporary_post_operationalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6d58370-0139-4a1c-ad90-b43279eccb8e', '2026-08-05T09:00:00Z').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, liability_insurers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, compliance_legal_complex).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, ai_harm_claimants).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, foundation_model_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, enterprise_ai_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, small_value_chain_suppliers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_harm_claimants).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, causal_contribution_apportionment_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, control_correlates_with_responsibility_principle).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, multi_pocket_recovery_solvency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and maintains the attribution formula built on contribution and control factors, issues apportionment guidance, accredits conformity-assessment bodies, and enforces through market surveillance and referral to courts. Collects nothing directly; its institutional standing depends on the regime functioning credibly. Exit is not a meaningful category from this seat - it is defined by administering the rule.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_regulators, agenda_setter,
    institutional, generational, analytical, continental).

% Build and license the underlying models. They bear assessed liability shares reflecting capability creation, mandatory insurance, documentation obligations covering training data and evaluations, and the indemnification demands of downstream customers. They can relocate research operations or withhold releases from strict jurisdictions, but cannot serve major markets without accepting the allocation; their scale buys caps, carve-outs, and clause-drafting leverage that smaller actors cannot obtain.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, foundation_model_developers, payer,
    powerful, biographical, constrained, global).

% Integrate models into products and services and stand first in line when users are injured. They carry allocated shares reflecting deployment decisions, plus duties to monitor, log, and report incidents. They buy cover, push indemnification upstream where bargaining strength allows, and absorb residual exposure where it does not. Leaving the arrangement would mean leaving the market for AI-augmented offerings.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, enterprise_ai_deployers, payer,
    organized, biographical, constrained, global).

% Data labelers, plugin authors, and niche integrators operating on take-it-or-leave-it contract terms. They receive indemnification clauses drafted by stronger counterparties that assign them shares poorly matched to their actual contribution or control, and they lack the audit trails and counsel needed to contest apportionments. Their realistic way out is abandoning AI-dependent revenue lines entirely.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, small_value_chain_suppliers, payer,
    powerless, immediate, constrained, regional).

% Underwrite the mandatory and voluntary cover the regime makes necessary, collecting premiums across both the developer and deployer tiers. They shape effective allocation through policy wording, exclusions, and risk pricing, and their actuarial assessments frequently become the de facto contribution estimates that courts and contracting parties rely on. They can withdraw from lines, reprice annually, and move capital between jurisdictions.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, liability_insurers, beneficiary,
    institutional, biographical, arbitrage, global).

% Law firms, audit shops, and consultancies that design indemnification webs, produce the contribution-and-control assessments, defend apportionment disputes, and certify conformity. Their revenue scales with the complexity of the allocation machinery, and their expertise concentrates in the few hubs that draft the standard clauses everyone else signs.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, compliance_legal_complex, beneficiary,
    organized, biographical, mobile, global).

% People physically, financially, or reputationally injured by AI-mediated failures. The arrangement offers them multiple solvent pockets to pursue instead of a single judgment-proof operator, but collecting means multi-party litigation against defendants backed by insurers and standard defenses, and outcomes turn on contribution assessments the claimants rarely control. After injury they cannot exit the dispute; before injury they cannot opt out of living alongside deployed systems.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_harm_claimants, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, ai_harm_claimants, payer).

% Volunteers and nonprofit labs whose weights, datasets, and components circulate through commercial chains without any contract. Allocation formulas written for commercial actors reach them through derivative deployments, exposing them to liabilities they cannot price or insure, yet they were absent from the consultations that shaped the formula. Their commitment to open release is constitutive of why they contribute at all, which makes withdrawal an identity cost rather than a merely financial one.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, open_source_contributors, excluded,
    moderate, civilizational, identity_locked, global).

% Scholars of tort law, AI governance, and economics who trace whether apportionments track contribution or bargaining power, run comparative analyses across attribution regimes, and supply the conceptual vocabulary in which courts and legislators argue. They neither collect nor pay; their stake is the coherence of the doctrine itself.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, academic_tort_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, liability_insurers).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the attribution and incentive problem in multi-actor AI production. Without a distribution rule, no single link in the chain internalizes system-level harm risk: the proximate operator may be judgment-proof, upstream contributors face no consequence, and safety investment is starved at every seam. Distributing liability by assessed contribution and control gives every participant a marginal incentive roughly proportional to its ability to prevent harm, and gives injured parties recovery paths spanning the whole chain.
% TRANSFER_FUNCTION: Moves money (damage awards, settlements, insurance premiums, compliance and documentation spend) and information (incident reports, evaluation logs, contribution assessments) from AI producers and deployers toward claimants, insurers, and the legal and compliance sector. Through contractual indemnification it also moves decision rights over risk allocation, typically from weakly positioned suppliers toward whichever counterparty drafts the master agreement.
% ABSENT_VOICES: Open-source contributors would object that allocation formulas written around commercial contracting reach them through derivative deployments while granting them no seat in the consultation process; they are absent because outreach ran through industry associations and member-state ministries. Below-threshold victims - people harmed in amounts that never justify multi-party litigation - would object that the regime multiplied procedure faster than recovery; they are scattered, unrepresented, and invisible at the negotiating table.
% DISAPPEARANCE_RATIONALE: If the allocation rule vanished overnight, the indemnification webs already priced into thousands of commercial contracts would unwind through renegotiation and litigation, insurers would reprice or suspend cover pending a replacement rule, pending apportionment disputes would lose their deciding framework, and injured parties would fall back on whatever single-seat rule each jurisdiction still holds on its books. Deployment decisions, product roadmaps, and insurance availability would all reorganize around the successor arrangement.
% FOUNDING_PROBLEM: Multi-actor AI production broke the single-point liability model: when a system trained by one firm, adapted by a second, and operated by a third injures someone, the proximate operator is often judgment-proof while upstream contributors face no consequence, leaving victims uncompensated and no participant holding a marginal incentive to invest in prevention.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: pre-regime court records and ombudsman reports documenting injured parties left uncompensated behind insolvent deployers; consumer-protection agencies' incident registries; and the academic tort-literature diagnosis of the liability gap. None of these sources sits inside the insurer or legal-services seats that profit from the regime.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__shared_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__shared_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__shared_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62 at interval end: even judged by this reading's own fairness-of-contribution lights, the standing arrangement imposes net transfers well beyond coordination cost - premiums sized to regulation rather than loss experience, compliance fixed costs that bite small actors hardest, and litigation expenditure that scales with apportionment disputes. Suppression is 0.45: the regime is mandatory where enacted, but suppression here is structural rather than internalized - mandates, contractual lock-in, and market-access dependence - while alternative attribution philosophies remain live political options and jurisdictional arbitrage persists, so alternatives are pressured, not eliminated. Theater ratio 0.32 reflects a specific mechanism: contribution assessments in opaque model chains are often settled by negotiation and precedent rather than measurement, so a growing share of the apportionment exercise is ritual confirming shares the parties had already agreed to fight about. Accessibility collapse is 0.42 - understanding the regime does not dissolve the alternatives, because single-seat attribution remains legislatively reachable. Resistance 0.55 tracks sustained producer lobbying, apportionment litigation, and relocation threats. The temporal series run on ONE shared grid (every tracked metric authored at t = 0,2,4,6,8,10,12) so the engine samples a complete matrix; the suppression_requirement series is authored because the story specifically traces enforcement-capacity build-out (guidance issuance, accreditation, contractual standardization) over the interval, not merely shifting extraction; the trajectories are monotonic - no cyclical dynamic is claimed, and none is asserted. Final series values equal the base_properties scalars by construction of the interval endpoint.
 *
 * PERSPECTIVAL GAP:
 *   The payer and collector seats should compute differently. From the regulator's seat the arrangement is a working solution to a real incentive failure; from the two primary payer seats it is a compulsory cost structure whose incidence they contest; from the small-supplier seat it is a regressive pass-through machine wearing a fairness formula; from the insurer seat it is a constructed market; from the claimant seat it is a promise of solvency wrapped in procedural burden. Identity-lock dynamics concentrate in the excluded open-source seat: the fusion is ideological - open release is constitutive of the contributor self-concept, so the relevant exit (withholding contributions, relicensing) is unthinkable in identity terms even when financially available, which is precisely why the seat was safe to leave out of the consultation. If that identity frame broke - if contributors began treating their output as priced labor - the excluded seat would convert into an organized payer bloc with litigation capacity, changing the resistance profile materially.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural declarations drive the computation: liability_insurers and compliance_legal_complex are declared beneficiaries with arbitrage/mobile exit, placing their derived directionality near the subsidized end; the three payer groups are declared victims with constrained exit, placing them near the full-target end, with the small suppliers' combination of powerless power and constrained exit sitting deepest. Ai_harm_claimants hold beneficiary as primary role with payer as secondary_role - their net position is positive (many pockets to pursue) but dragged down by litigation cost and assessment-dependence, and expressing that duality through secondary_role rather than a directionality override is deliberate: the override surface keys on power_atom, so overriding the powerless atom to lift the claimant seat would simultaneously distort the small-supplier seat, which shares that atom but sits at the opposite pole. No directionality_overrides are authored because the derivation from declarations plus exit options reproduces the true relationships for every seat; the one candidate (claimants) is better handled structurally, as described.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways here. Reading the arrangement as pure rope would erase the measurable rents - premium volumes untethered from loss curves, compliance revenue scaling with procedural complexity, indemnification clauses migrating burden to the weakest signatories - and would launder a constructed market as coordination cost. Reading it as a snare would erase the genuine coordination achievement: before allocation, no participant internalized chain-level harm risk, and victims routinely ate total losses behind judgment-proof operators; the founding problem is corroborated as live from outside the benefiting parties. Tangled_rope holds both facts visible: real coordination function, asymmetric extraction riding on it. Mandatrophy has not resolved - the founding problem persists (status live) and the arrangement still performs its core function, so the persistence question is about extraction growth, not vestigiality; the rising theater_ratio series is the early-warning channel to watch for the negotiated-apportionment ritual displacing evidentiary assessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does distributing liability by causal contribution and control (this reading) yield materially different total extraction than concentrating it on developers or on deployers?',
    'Comparative analysis across jurisdictions operating the three readings: measure aggregate compliance-plus-premium-plus-litigation cost per unit of covered harm, and victim recovery rates, under each allocation principle. Cross-reference the sibling stories liability_attribution__developer_liability and liability_attribution__deployer_liability.',
    'If total extraction is roughly invariant across readings, the kernel contest redistributes burden rather than creating or destroying it, and per-seat classification differences are the real signal; if shared_liability uniquely multiplies coordination overhead, the reading itself carries a distinct extraction penalty no sibling shares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is one reading of the liability_attribution kernel; sibling readings change the victim-set composition and the insurance-market consequences.').

omega_variable(
    apportionment_measurability,
    'Can causal contribution actually be measured in opaque multi-component AI chains, or is every apportionment a negotiated estimate dressed as measurement?',
    'Audit studies comparing formally assessed contribution shares against counterfactual ablation evidence and post-hoc incident reconstruction; track convergence or divergence as interpretability tooling matures.',
    'If contribution is measurable, the regime''s coordination function is genuine and its extraction is the contested part; if it is not, the apportionment layer is largely theatrical, theater_ratio is understated, and the arrangement drifts toward piton-like ritual resting on a snare-like contractual core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apportionment_measurability, empirical, 'Whether the attribution formula''s inputs are epistemically accessible or permanently estimated.').

omega_variable(
    allocation_regressivity,
    'Does contractual allocation systematically shift assigned shares toward weakly positioned value-chain actors, defeating the contribution-and-control principle in operation?',
    'Contract-population study: compare indemnification clauses and final settled allocations against independent contribution assessments across firm-size strata.',
    'Confirmed regressivity would establish that the effective constraint extracts disproportionately from the powerless seat, raising effective extraction at that seat far above the story-level scalar and supporting targeted-allocation remedies; absence of regressivity would strengthen the rope-side reading of the allocation machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allocation_regressivity, empirical, 'Whether bargaining power corrupts the contribution principle via contract design.').

omega_variable(
    insurance_pricing_welfare,
    'Do liability premiums track expected harm (efficient risk signaling that improves safety investment) or regulatory demand plus market concentration (rent)?',
    'Actuarial decompositions separating loss-cost components from expense and profit loadings; natural experiments from jurisdictions entering or leaving the regime.',
    'Rent-heavy pricing confirms the insurer seat as the principal capturer and supports the tangled_rope verdict with a named extraction channel; cost-reflective pricing would recast much of measured extraction as genuine coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_pricing_welfare, empirical, 'Welfare status of the insurance layer the regime calls into being.').

omega_variable(
    claimant_recovery_net_effect,
    'Does multi-pocket liability actually raise net recovery for injured parties once multi-party litigation cost and opacity-driven defense success are counted?',
    'Longitudinal claims data comparing realized recovery rates and time-to-recovery before and after regime adoption, stratified by harm severity.',
    'If net recovery falls, the declared beneficiary seat is nominal only and the claimant secondary_role dominates, weakening the coordination half of the tangled_rope structure; if it rises, the arrangement''s beneficiary declaration is confirmed as substantive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(claimant_recovery_net_effect, empirical, 'Whether the intended beneficiary seat realizes net benefit or bears hidden payer costs.').

omega_variable(
    cs_framing_underdetermination,
    'Is the commitment-system kernel the statutory attribution formula itself, or the interpretive doctrine of causal contribution that legitimizes the formula from above it?',
    'Examine whether courts and agencies adjudicate disputes by appealing to the statutory text or to the underlying fairness doctrine when text and doctrine diverge; if doctrine governs, the kernel sits one layer higher than declared.',
    'Adopting the doctrine-as-kernel framing would classify this as a lineage-grounded interpretive tradition with heavier drift absorption and would shift the computed CS pattern; the statute-as-kernel framing adopted here treats interpretation as a buffer below a formalized rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent framings of the same commitment structure produce different cs_structure classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liab_tr_t2, liability_attribution__shared_liability, theater_ratio, 2, 0.18).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__shared_liability, theater_ratio, 4, 0.21).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__shared_liability, theater_ratio, 6, 0.25).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__shared_liability, theater_ratio, 8, 0.28).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.3).
narrative_ontology:measurement(liab_tr_t12, liability_attribution__shared_liability, theater_ratio, 12, 0.32).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(liab_be_t2, liability_attribution__shared_liability, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(liab_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(liab_be_t6, liability_attribution__shared_liability, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(liab_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(liab_be_t12, liability_attribution__shared_liability, base_extractiveness, 12, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liab_su_t2, liability_attribution__shared_liability, suppression_requirement, 2, 0.33).
narrative_ontology:measurement(liab_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(liab_su_t6, liability_attribution__shared_liability, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(liab_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(liab_su_t12, liability_attribution__shared_liability, suppression_requirement, 12, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, resource_allocation).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'AI liability'. The label conflates three structurally distinct allocation claims: developer-primary, deployer-primary, and chain-wide proportional distribution. Each has its own stable epsilon, its own victim set, and its own insurance-market consequence, so each is a separate story per the epsilon-invariance principle. Family topology: the single-seat readings are upstream baselines (higher legislative familiarity, established doctrinal roots); this shared_liability story is downstream - it is argued FOR and AGAINST by citation to both single-seat baselines, and its contractual and insurance infrastructure creates structural conditions any sibling implementation would inherit or dismantle. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
