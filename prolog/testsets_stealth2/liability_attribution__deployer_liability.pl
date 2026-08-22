% ============================================================================
% CONSTRAINT STORY: liability_attribution__deployer_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_attribution__deployer_liability, []).

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
 *   constraint_id: liability_attribution__deployer_liability
 *   human_readable: Deployer-Primary Liability Allocation for AI Harms
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   Across 2018-2026, the allocation of liability for AI-caused harm
 *   crystallized around a deployer-primary reading: the party that selects
 *   the deployment context, configures the use, and decides oversight
 *   arrangements bears primary legal exposure, while capability providers cap
 *   their exposure contractually. The arrangement was codified in statutes
 *   and directives assigning deployer duties, hardened by standardized
 *   provider indemnity disclaimers, and serviced by a growing deployer-side
 *   insurance market. This story authors epsilon for THAT standing
 *   arrangement — the deployer-primary allocation as instantiated in law and
 *   contract practice — assessed by this reading's own lights: the reading
 *   regards context-control liability as largely correct incentive placement
 *   while conceding residual unfairness where deployer control is thinner
 *   than the theory assumes. KEY AGENTS (by structural relationship): see
 *   key_agents; the paying seats are the three deployer classes, the
 *   collecting seats are the shielded providers and the insurers who pool
 *   what deployers bear.
 *
 * KEY AGENTS:
 *   - foundation_model_providers: Primary beneficiary (institutional/arbitrage) — shielded from downstream harm by the allocation and by standardized disclaimers; revenue scales with deployment volume while capped exposure does not
 *   - enterprise_ai_deployers: Paying seat (powerful/constrained) — bears primary exposure, partially offsets it through negotiated indemnity and captive insurance
 *   - startup_deployers: Paying seat (moderate/trapped) — bears the same primary exposure on take-it-or-leave-it terms with a fraction of the defensive capacity
 *   - public_sector_civic_deployers: Paying and unrepresented seat (moderate/trapped) — assumes exposure under procurement and service pressure with no seat where the allocation was designed
 *   - ai_liability_insurers: Secondary beneficiary (organized/mobile) — prices and pools the deployer-borne exposure; premium income scales with the breadth of deployer duties
 *   - injured_third_party_claimants: Mixed seat (powerless/trapped) — gains a proximate, reachable defendant; recovery remains hostage to deployer solvency
 *   - ai_legislators_regulators: Agenda setter (institutional/constrained) — codified and administers the allocation; amendment is diplomatically and competitively costly once harmonized
 *   - courts_and_adjudicators: Analytical observer (institutional/analytical) — applies the allocation case by case and sets the precedents that harden or soften it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__deployer_liability, 0.55).
domain_priors:suppression_score(liability_attribution__deployer_liability, 0.58).
domain_priors:theater_ratio(liability_attribution__deployer_liability, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, extractiveness, 0.55).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(liability_attribution__deployer_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__deployer_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__deployer_liability, "Deployer-Primary Liability Allocation for AI Harms").
narrative_ontology:topic_domain(liability_attribution__deployer_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__deployer_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__deployer_liability, 'db562332-42b7-4d4c-8a77-3436ae0a16d2').
narrative_ontology:cs_kernel_codification('db562332-42b7-4d4c-8a77-3436ae0a16d2', formalized).
narrative_ontology:cs_authority_grounding('db562332-42b7-4d4c-8a77-3436ae0a16d2', lineage).
narrative_ontology:cs_interpretation_layer_present('db562332-42b7-4d4c-8a77-3436ae0a16d2').
narrative_ontology:cs_reading_relation('db562332-42b7-4d4c-8a77-3436ae0a16d2', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('db562332-42b7-4d4c-8a77-3436ae0a16d2', liability_attribution__shared_liability, influences).
narrative_ontology:cs_axiom('db562332-42b7-4d4c-8a77-3436ae0a16d2', foundational, context_control_grounds_primary_liability).
narrative_ontology:cs_axiom_status(context_control_grounds_primary_liability, holdable).
narrative_ontology:cs_axiom_grounding('db562332-42b7-4d4c-8a77-3436ae0a16d2', context_control_grounds_primary_liability, deontological).
narrative_ontology:cs_axiom('db562332-42b7-4d4c-8a77-3436ae0a16d2', secondary, opacity_shifts_due_diligence_to_deployer).
narrative_ontology:cs_axiom_status(opacity_shifts_due_diligence_to_deployer, holdable).
narrative_ontology:cs_axiom_grounding('db562332-42b7-4d4c-8a77-3436ae0a16d2', opacity_shifts_due_diligence_to_deployer, instrumental).
narrative_ontology:cs_reference_frame('db562332-42b7-4d4c-8a77-3436ae0a16d2', context_control_primacy).
narrative_ontology:cs_drift_state('db562332-42b7-4d4c-8a77-3436ae0a16d2', contemporary_foundation_model_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('db562332-42b7-4d4c-8a77-3436ae0a16d2', '').
narrative_ontology:cs_kernel_id(liability_attribution__deployer_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, ai_liability_insurers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, enterprise_ai_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, startup_deployers).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, public_sector_civic_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__deployer_liability, injured_third_party_claimants).
narrative_ontology:constraint_victim(liability_attribution__deployer_liability, injured_third_party_claimants).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, context_control_responsibility_principle).
narrative_ontology:constraint_vindicates(liability_attribution__deployer_liability, cheapest_cost_avoider_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Train and license general-purpose models sold through APIs and enterprise agreements whose standard terms disclaim responsibility for harms arising in customer deployments. They set model behavior through training and post-deployment updates and can revise both without renegotiating customer terms. Under the prevailing allocation they answer for defects in the capability itself but not for what customers do with it; their exposure is capped by contract while their revenue scales with deployment volume.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, foundation_model_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Underwrite deployer-side liability policies priced against the allocation in force. Premium income scales with the breadth of deployer duties; payout obligations scale with realized harms. They draft policy terms, exclusions, and diligence requirements that deployers must satisfy to obtain coverage, giving them a shaping hand in what deployer compliance means in practice.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_liability_insurers, beneficiary,
    organized, biographical, mobile, global).

% Integrate foundation models into products and internal operations at scale. They select use cases, configure safeguards, and staff oversight, and they carry the primary legal exposure when deployments cause harm. Their scale buys partial contractual indemnities and captive insurance capacity, but provider market concentration limits how far negotiation can shift exposure back upstream.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, enterprise_ai_deployers, payer,
    powerful, biographical, constrained, global).

% Build products on rented model access under standardized terms they cannot renegotiate. They bear the same primary exposure as large deployers with a fraction of the legal staff, insurance budget, or auditing capacity. Their realistic options — accepting the terms, delaying launch, or dropping model-dependent features — each carry existential cost.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, startup_deployers, payer,
    moderate, biographical, trapped, global).

% Hospitals, school systems, and municipal agencies adopting AI tools under procurement mandates and constituent service pressure. They assume deployment exposure with thin legal resources and had no seat in the consultations where the allocation was designed; their recourse is declining tools their constituents increasingly expect them to provide.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, public_sector_civic_deployers, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, public_sector_civic_deployers, excluded).

% People harmed by deployed AI systems. The allocation hands them a proximate, jurisdictionally reachable defendant in the deployer, which materially simplifies bringing a claim; their recovery nonetheless depends on the deployer's solvency and insurance depth, since the upstream capability provider is contractually insulated from their claims.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, injured_third_party_claimants, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__deployer_liability, injured_third_party_claimants, payer).

% Enact and administer the allocation: statutes and directives assigning deployer duties, supervisory authorities policing compliance, transposition deadlines binding subordinate jurisdictions. Once harmonized across trading partners, revisiting the allocation carries diplomatic and competitiveness costs that outlast any single government's term.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, ai_legislators_regulators, agenda_setter,
    institutional, generational, constrained, continental).

% Apply the allocation case by case: deciding whether a deployer's context choices suffice to ground primary responsibility, weighing model opacity against foreseeability, and setting the precedents that harden or soften the reading over time. They take testimony from every other seat and their docket is where the allocation's assumptions meet lived harms.
narrative_ontology:constraint_stakeholder(liability_attribution__deployer_liability, courts_and_adjudicators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__deployer_liability, foundation_model_providers).
narrative_ontology:fixing_cost_class(liability_attribution__deployer_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns harm-cost responsibility to the party that selects the deployment context, configures use, and decides oversight arrangements — creating a single insurable risk locus and concentrating care incentives where use-case knowledge is richest, instead of leaving every harm to be litigated from scratch against distant capability creators.
% TRANSFER_FUNCTION: Moves expected harm costs — premiums, reserves, and uncompensated tails — from foundation model providers, who would otherwise internalize downstream harm, onto deployers, via statutory duty assignment, insurance pricing, and non-negotiable provider indemnity disclaimers.
% ABSENT_VOICES: Public-sector and small deployers were scarcely represented when the allocation crystallized: technical consultation processes drew disproportionately on provider expertise, and the contract-law layer where disclaimers were standardized proceeded with no claimant or small-buyer voice at all. Injured-third-party advocates entered only after harms materialized, downstream of every term that determines their recovery.
% DISAPPEARANCE_RATIONALE: If the deployer-primary allocation vanished overnight, providers would face downstream claims they currently disclaim, deployer-side insurance products would lapse or reprice around the uncertainty, contracting across the value chain would shift to bespoke negotiated indemnity matrices, and care-investment incentives would migrate upstream — the entire risk architecture of the AI economy would reorganize around whichever allocation filled the vacuum.
% FOUNDING_PROBLEM: Responsibility for AI-caused harm was indeterminate: harms arise from intertwined capability choices and deployment choices, existing doctrine had no category for opaque automated systems, and no party had a settled duty to insure or take care — chilling deployment, deterring investment, and leaving injured parties without a reliable defendant.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: legislative recitals and regulatory impact assessments (seats that collect nothing from the allocation), deployer-side trade association submissions, and the academic law-and-economics literature all attest the indeterminacy problem. Notably, no deployer organization corroborates the specific deployer-primary ANSWER — they corroborate the problem while disputing this allocation of it, which is the expected signature of a live contest rather than a settled one.
narrative_ontology:disappearance_verdict(liability_attribution__deployer_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__deployer_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__deployer_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__deployer_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__deployer_liability, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_attribution__deployer_liability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_attribution__deployer_liability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(liability_attribution__deployer_liability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.55: the reading's own lights concede the core placement is sound (deployers genuinely choose context and oversight), so the extractive residue is what remains after efficient incentive pricing — adhesion disclaimers no buyer can negotiate away, uninsurable tail exposure, and due-diligence burdens that cannot be discharged over opaque models. That residue concentrates on the weakest deployers, which is why the series rises from 0.38 to 0.55 as disclaimers standardized and model opacity deepened faster than deployer assessment capacity. Suppression is 0.58 and is authored as a RAW structural property — the engine, not this story, scales extractiveness by directionality and scope; suppression here is legal-plus-contractual lock-in (no workable alternative allocation is available to an individual deployer), not physical coercion. Theater is 0.32 and rising: vendor questionnaires, boilerplate audit reports, and compliance rituals that everyone involved knows cannot penetrate model internals are displacing a growing share of diligence activity, though the underlying liability function remains real. Accessibility_collapse is 0.48 — self-building models is prohibitive, provider terms are industry-standardized so shopping around yields identical disclaimers, but jurisdictional variation and large-buyer negotiation keep the alternative space partly open. Resistance is 0.55: deployer trade coalitions, indemnity-scope litigation, and legislative contests are sustained and organized. All three series run on ONE shared time grid (2018, 2020, 2022, 2023, 2024, 2025, 2026) so no metric is sampled against another metric's end-state; the 2026 column is marked projected. The suppression_requirement series is included because the story specifically tracks enforcement-capacity maturation (supervisory authorities staffing up, transposition deadlines binding), not merely extraction drift. No cyclical dynamics are asserted — the drift is monotonic. Coalition note: the three deployer classes share an interest in re-allocation and have begun acting through trade associations; their combined latent power is the main counterweight to provider insulation, but it is currently spent defending against expansion rather than reversing the allocation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the provider seat the arrangement reads as near-pure coordination it did not even have to run: someone else insures the downstream, care incentives sit where use-case knowledge lives, and the provider's own exposure is contractually capped. From the trapped small-deployer seat the same structure reads as unbargainable cost imposition — identical exposure to a large enterprise with none of the defensive capacity, terms signed under existential pressure. The enterprise seat sits between: real offset capacity, real residual exposure. The engine computes these per-seat classifications from the structural data (power, exit, role); this story's claimed type does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundation model providers and insurers are declared beneficiaries and derive directionality near the beneficiary end: the arrangement subsidizes them (externalized harm costs; premium float on pooled deployer risk). The three deployer classes are declared victims and derive high directionality, modulated by exit: trapped small and civic deployers sit nearer the full-target end than constrained enterprise deployers, whose partial contractual arbitrage damps their effective burden. Injured claimants are structurally mixed — the proximate-defendant advantage pulls them toward the beneficiary end while insolvency residual pulls the other way — so they are carried as a stakeholder with dual positioning rather than forced into a single array. The agenda-setter seat administers the arrangement it wrote; its directionality reflects administration rather than collection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — indeterminate responsibility for harms produced by intertwined capability and deployment choices — is live, not dead: capabilities keep evolving and each new deployment class reopens the allocation question. The classification therefore guards against two opposite misreadings. Reading the arrangement as pure coordination (rope) would erase the measurable externalization onto deployers who cannot bargain or discharge their assigned diligence; reading it as pure extraction (snare) would erase the genuine coordination achievement — a single insurable risk locus and care incentives at the point of use-case knowledge — that makes the arrangement worth reforming rather than abolishing. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges: no zombie flag; the arrangement persists because the problem persists, not because its function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_primary_criterion_contest,
    'Which control point — deployment context (this reading), capability creation (developer_liability), or distributed causal contribution (shared_liability) — is the legitimate primary hook for AI-harm liability?',
    'Comparative institutional analysis: track case-law outcomes and legislative convergence or divergence across jurisdictions adopting different allocation criteria; observe which criterion survives contact with litigated harm cases.',
    'Adopting developer_liability would move foundation_model_providers into the paying set and subsidize deployers, flipping directionality across every seat; adopting shared_liability would dissolve the single-payer structure into proportional shares, lowering peak burden on any one seat. This file instantiates only the deployer reading with its own stable epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_primary_criterion_contest, conceptual, 'This constraint is one reading of the liability_attribution kernel; the sibling readings are separate constraint files with their own victim/beneficiary structures.').

omega_variable(
    opacity_due_diligence_feasibility,
    'Can deployers meaningfully discharge a due-diligence standard over opaque foundation models whose behavior the provider can alter after deployment?',
    'Audit-regime pilots and disclosure mandates: measure incident rates under deployer-side controls versus provider-side controls, and test whether deployer diligence instruments detect model-level risk at all.',
    'If diligence is infeasible, the reading charges compliance costs without enabling compliance — the burden concentrates on the smallest deployers and the arrangement drifts toward pure extraction at those seats; if feasible, the measured extraction is bounded incentive pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_due_diligence_feasibility, empirical, 'Whether the deployer''s assigned due-diligence burden is dischargeable given model opacity and provider-side post-deployment updates.').

omega_variable(
    claimant_recovery_residual,
    'Does routing injured parties'' claims to deployers leave them under-compensated when deployers are thinly capitalized?',
    'Claims data on deployer insolvency and insurance-depth shortfalls in AI-harm litigation; analysis of compulsory deployer insurance mandates as a corrective.',
    'A systematic recovery shortfall would add injured claimants to the paying set and raise measured extraction; adequate solvency and insurance depth keeps them on the benefited side of the ledger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(claimant_recovery_residual, empirical, 'Whether the proximate-defendant advantage for claimants survives deployer insolvency risk.').

omega_variable(
    context_control_rationale_genuineness,
    'Is deployment-context control a real decision-authority criterion, or a post-hoc rationalization of provider-favorable drafting?',
    'Code litigated and reported AI harms by whether deployment-parameter choices (use case, oversight configuration, human-in-the-loop design) or upstream capability choices (training data, model behavior) were the operative cause; compute the share attributable to each.',
    'If most harms trace upstream to capability choices, the context-control story thins toward cover for provider insulation and the arrangement''s character degrades at every deployer seat; if deployment choices dominate, the criterion is doing real allocative work.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(context_control_rationale_genuineness, empirical, 'Genuineness of the context-control criterion versus lobbying-driven rationalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__deployer_liability, 2018, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t2018, liability_attribution__deployer_liability, theater_ratio, 2018, 0.12).
narrative_ontology:measurement_basis(liab_tr_t2018, observed).
narrative_ontology:measurement(liab_tr_t2020, liability_attribution__deployer_liability, theater_ratio, 2020, 0.16).
narrative_ontology:measurement_basis(liab_tr_t2020, observed).
narrative_ontology:measurement(liab_tr_t2022, liability_attribution__deployer_liability, theater_ratio, 2022, 0.22).
narrative_ontology:measurement_basis(liab_tr_t2022, observed).
narrative_ontology:measurement(liab_tr_t2023, liability_attribution__deployer_liability, theater_ratio, 2023, 0.26).
narrative_ontology:measurement_basis(liab_tr_t2023, observed).
narrative_ontology:measurement(liab_tr_t2024, liability_attribution__deployer_liability, theater_ratio, 2024, 0.29).
narrative_ontology:measurement_basis(liab_tr_t2024, observed).
narrative_ontology:measurement(liab_tr_t2025, liability_attribution__deployer_liability, theater_ratio, 2025, 0.31).
narrative_ontology:measurement_basis(liab_tr_t2025, observed).
narrative_ontology:measurement(liab_tr_t2026, liability_attribution__deployer_liability, theater_ratio, 2026, 0.32).
narrative_ontology:measurement_basis(liab_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(liab_be_t2018, liability_attribution__deployer_liability, base_extractiveness, 2018, 0.38).
narrative_ontology:measurement_basis(liab_be_t2018, observed).
narrative_ontology:measurement(liab_be_t2020, liability_attribution__deployer_liability, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement_basis(liab_be_t2020, observed).
narrative_ontology:measurement(liab_be_t2022, liability_attribution__deployer_liability, base_extractiveness, 2022, 0.47).
narrative_ontology:measurement_basis(liab_be_t2022, observed).
narrative_ontology:measurement(liab_be_t2023, liability_attribution__deployer_liability, base_extractiveness, 2023, 0.51).
narrative_ontology:measurement_basis(liab_be_t2023, observed).
narrative_ontology:measurement(liab_be_t2024, liability_attribution__deployer_liability, base_extractiveness, 2024, 0.54).
narrative_ontology:measurement_basis(liab_be_t2024, observed).
narrative_ontology:measurement(liab_be_t2025, liability_attribution__deployer_liability, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(liab_be_t2025, observed).
narrative_ontology:measurement(liab_be_t2026, liability_attribution__deployer_liability, base_extractiveness, 2026, 0.55).
narrative_ontology:measurement_basis(liab_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t2018, liability_attribution__deployer_liability, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement_basis(liab_su_t2018, observed).
narrative_ontology:measurement(liab_su_t2020, liability_attribution__deployer_liability, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement_basis(liab_su_t2020, observed).
narrative_ontology:measurement(liab_su_t2022, liability_attribution__deployer_liability, suppression_requirement, 2022, 0.46).
narrative_ontology:measurement_basis(liab_su_t2022, observed).
narrative_ontology:measurement(liab_su_t2023, liability_attribution__deployer_liability, suppression_requirement, 2023, 0.5).
narrative_ontology:measurement_basis(liab_su_t2023, observed).
narrative_ontology:measurement(liab_su_t2024, liability_attribution__deployer_liability, suppression_requirement, 2024, 0.53).
narrative_ontology:measurement_basis(liab_su_t2024, observed).
narrative_ontology:measurement(liab_su_t2025, liability_attribution__deployer_liability, suppression_requirement, 2025, 0.56).
narrative_ontology:measurement_basis(liab_su_t2025, observed).
narrative_ontology:measurement(liab_su_t2026, liability_attribution__deployer_liability, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(liab_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__deployer_liability, resource_allocation).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__deployer_liability, liability_attribution__shared_liability).

% DUAL FORMULATION NOTE:
% The colloquial label 'AI liability' covers three structurally distinct allocations, decomposed per the epsilon-invariance principle into a constraint family: deployer_liability (this file — deployers pay, providers shielded), developer_liability (providers pay, deployers subsidized), and shared_liability (proportional distribution, no single capturer). Their epsilon values differ because their victim and beneficiary sets differ; measuring 'AI liability' with the wrong reading's observable changes epsilon and therefore the classification, which is the signature of three constraints sharing one label. Codification of this reading upstream changes the operating environment of both siblings — shared-liability proposals are drafted as corrections to a deployer-primary baseline — hence the family links in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
