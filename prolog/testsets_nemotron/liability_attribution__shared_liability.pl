% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Joint Liability Distributed Along AI Value Chain by Causal Contribution and Control
 *   domain: technology_governance/legal_theory/regulatory_design
 *
 * SUMMARY:
 *   This constraint models the 'shared liability' reading of the contested
 *   liability_attribution kernel: joint liability distributed along the AI
 *   value chain based on causal contribution and control. Unlike the
 *   deployer_liability reading (which concentrates liability on
 *   deployment-context controllers) or the developer_liability reading (which
 *   concentrates on capability creators), this reading distributes the burden
 *   across both developer and deployer seats, with contractual allocation
 *   mechanisms and emerging insurance markets as the coordination
 *   infrastructure. The constraint is claimed as a tangled rope — genuine
 *   coordination of risk distribution across a complex value chain, with
 *   asymmetric extraction where developers and deployers bear costs that
 *   partly flow to insurance intermediaries and regulatory enforcement
 *   apparatuses. The ε-invariance principle applies: this is ONE reading with
 *   its own stable ε (0.48), not a parameterization of a single
 *   liability_attribution constraint.
 *
 * KEY AGENTS:
 *   - ai_developers: Primary target (institutional/powerful) — bear liability for model capabilities and training decisions
 *   - ai_deployers: Primary target (organized/powerful) — bear liability for deployment context, use-case decisions, and monitoring
 *   - affected_persons: Primary beneficiary (powerless/moderate) — gain redress pathway through distributed liability pool
 *   - insurance_markets: Secondary beneficiary (institutional) — capture coordination surplus via premium structures
 *   - regulatory_authorities: Agenda setter (institutional) — set causal contribution standards and enforcement regimes
 *   - small_enterprise_adopters: Secondary victim (moderate) — disproportionately burdened by opacity and contractual complexity
 *   - forensic_experts: Tertiary beneficiary (organized) — extract rents from causal contribution disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.48).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.32).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.48).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint Liability Distributed Along AI Value Chain by Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, '6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92').
narrative_ontology:cs_kernel_codification('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', distributed).
narrative_ontology:cs_authority_grounding('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', extraction).
narrative_ontology:cs_interpretation_layer_present('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92').
narrative_ontology:cs_reading_relation('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', foundational, joint_and_several_value_chain_liability).
narrative_ontology:cs_axiom_status(joint_and_several_value_chain_liability, holdable).
narrative_ontology:cs_axiom_grounding('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', joint_and_several_value_chain_liability, conventional).
narrative_ontology:cs_axiom('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', foundational, causal_contribution_as_allocation_metric).
narrative_ontology:cs_axiom_status(causal_contribution_as_allocation_metric, holdable).
narrative_ontology:cs_axiom_grounding('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', causal_contribution_as_allocation_metric, empirically_contingent).
narrative_ontology:cs_reference_frame('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', pre_ai_liability_regime).
narrative_ontology:cs_drift_state('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', emerging_value_chain_standard, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6374aa7e-7772-4c8d-bcf1-3cc3ceb73d92', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, affected_persons).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_markets).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, regulatory_authorities).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_deployers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, small_enterprise_adopters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, ai_deployers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, forensic_experts).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, proportional_accountability_principle).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, value_chain_responsibility_doctrine).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, causal_contribution_as_liability_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and release foundation models and AI systems. Bear liability for model capabilities, training data choices, architecture decisions, and emergent behaviors. Cannot retract models once deployed. Contractual allocation clauses attempt to shift downstream liability but face enforceability challenges. Exit is constrained: they can stop releasing models but cannot undo existing deployments or escape legacy liability.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_developers, payer,
    institutional, generational, constrained, global).

% Integrate AI systems into products, services, and operational workflows. Bear liability for deployment context decisions, use-case selection, monitoring adequacy, and human-in-the-loop design. Their business models are built on deployment — exit means abandoning core revenue streams (identity_locked). They benefit from upstream developer liability sharing (secondary_role: beneficiary) but net pay into the liability pool.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_deployers, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, ai_deployers, beneficiary).

% Individuals and communities harmed by AI system outputs, decisions, or failures. Gain access to a distributed liability pool with multiple solvent defendants (developers + deployers + insurers). Can forum-shop across jurisdictions and defendants (arbitrage exit). Their benefit is real but mediated by litigation costs and causal contribution proof burdens.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, affected_persons, beneficiary,
    powerless, immediate, arbitrage, global).

% Provide liability insurance, reinsurance, and indemnification products to developers and deployers. Collect premia from both victim seats. Design policies around causal contribution metrics and allocation formulas. Capture coordination surplus through premium-to-loss ratios above actuarial fair value. Exit is arbitrage-grade: they can reprice, withdraw capacity, or restructure products as the liability regime evolves.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Define causal contribution standards, mandate liability insurance, approve allocation frameworks, and enforce compliance. Bear enforcement costs but gain regulatory legitimacy and mandate expansion. Near-symmetric position: they neither purely extract nor purely benefit — they administer the coordination infrastructure. Analytical exit: they observe and shape but are not personally subject to the liability transfers.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Small businesses adopting AI systems for operational use. Lack legal resources to negotiate favorable allocation clauses, lack capital for self-insurance, and lack lobbying power to shape standards. Disproportionately burdened by opacity: cannot evaluate model internals (developer-side) nor fully control deployment contexts (deployer-side). Effectively trapped — contractual allocation clauses are unenforceable against deep-pocketed counter-parties, and exit means foregoing AI productivity gains essential for competitiveness.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, small_enterprise_adopters, payer,
    moderate, biographical, trapped, regional).

% Economic consultants, technical experts, and legal specialists who litigate causal contribution shares. Extract fees from both developer and deployer seats for expert reports, deposition testimony, and allocation modeling. Mobile exit: skills transfer across liability regimes and jurisdictions. Their benefit is real but parasitic on the coordination metric's instability — stable causal contribution metrics would reduce their market.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, forensic_experts, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(liability_attribution__shared_liability, insurance_markets).
narrative_ontology:fixing_cost_class(liability_attribution__shared_liability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes AI harm liability across a fragmented value chain so that (a) victims have multiple solvent defendants to pursue, (b) developers and deployers internalize marginal risk decisions, and (c) insurance markets pool and price systemic risk. Solves the 'accountability gap' where single-party liability leaves harms uncompensated or risk decisions uninternalized.
% TRANSFER_FUNCTION: Moves liability payments from ai_developers and ai_deployers (and their insurers) to affected_persons as compensation, with a substantial slice captured by insurance_markets as premia and forensic_experts as litigation costs. Contractual allocation clauses attempt to rebalance shares between developers and deployers based on causal contribution metrics.
% ABSENT_VOICES: Open-source model maintainers (neither developers nor deployers in the commercial sense), academic researchers, and jurisdictions without mature AI liability regimes. They would object to liability structures that chill open research or impose extraterritorial standards, but are excluded from the standard-setting process.
% DISAPPEARANCE_RATIONALE: If shared liability vanished overnight, victims would lose the distributed defendant pool and face single-party liability barriers (judgment-proof developers, jurisdiction-shielded deployers). Developers and deployers would shed liability costs but lose the coordination infrastructure (insurance markets, allocation standards) that makes risk manageable. Insurance markets would lose a major product line. The AI value chain would reorganize around either developer-only or deployer-only liability — a contested political battle.
% FOUNDING_PROBLEM: How to allocate liability for AI harms across a value chain where neither the model creator nor the deployer alone has full visibility, control, or solvency to bear the full risk — and where victims fall through the cracks of single-party liability regimes.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as LIVE by: (1) OECD AI Principles implementation reports (2023-2025) documenting persistent accountability gaps; (2) EU AI Act recitals acknowledging value chain liability complexity; (3) independent academic literature (Calo, Selbst, Whittlestone) on distributed responsibility — all sources outside the benefiting parties (insurance markets, regulatory authorities). The benefiting parties themselves contest whether the problem remains live: insurance markets argue risk is now priced; regulatory authorities argue standards are converging.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.48) is substantial but not dominant: the constraint moves resources from developers/deployers to affected persons AND to insurance/forensic intermediaries. Suppression (0.32) is moderate — the constraint relies on legal enforcement and contractual allocation rather than raw coercion; alternatives (single-party liability, no-fault funds) exist but are legally foreclosed in this regime. Theater ratio (0.28) reflects real coordination infrastructure (causal contribution frameworks, insurance markets) with growing performative compliance. Accessibility collapse (0.42) is moderate — alternative liability architectures are discussable but legally marginalized. Resistance (0.55) is significant from both developer and deployer coalitions challenging causal contribution metrics and allocation formulas. The measurement grid shows all three metrics rising over the 0-10 interval: extraction accumulation as liability scopes expand, theater growth as compliance formalizes, suppression hardening as enforcement mechanisms mature.
 *
 * PERSPECTIVAL GAP:
 *   The developer seat and deployer seat are BOTH victims but experience different extraction vectors: developers face liability for upstream model properties (training data, architecture, capability emergence) with limited control over deployment; deployers face liability for downstream use decisions with limited visibility into model internals. The engine computes per-seat χ from the same ε but different directionality: developers have constrained exit (cannot un-deploy models already released), deployers have identity_locked exit (business models built on deployment), affected persons have arbitrage exit (can forum-shop for redress). Insurance markets sit at the beneficiary end (d ≈ 0.15) — they collect premia from both victim seats. Regulatory authorities are near-symmetric (d ≈ 0.5) — they bear enforcement costs but gain legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: affected_persons (direct redress recipients), insurance_markets (intermediary capture), regulatory_authorities (legitimacy/enforcement mandate). Victims declared: ai_developers (upstream causal contribution), ai_deployers (downstream control), small_enterprise_adopters (disproportionate opacity burden). The shared liability reading's structural asymmetry is that BOTH developer and deployer seats are in the victim set — unlike the sibling readings which concentrate victimhood on one seat. This creates a broader coordination base (more parties incentivized to negotiate allocation) but also broader extraction surface. Directionality derives from: who pays the liability transfers (victims), who receives them (beneficiaries), and exit options (trapped/identity_locked for victims, arbitrage for beneficiaries). No overrides needed — the derivation chain captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to allocate AI harm liability across a fragmented value chain) remains LIVE and contested — the shared liability reading was built for a world where neither pure developer nor pure deployer liability seemed adequate. The mandate has NOT atrophied: the coordination problem persists as value chains lengthen and capability opacity increases. However, the extraction vector is shifting: early coordination (victim compensation) is being overlaid with intermediary capture (insurance, forensic economics). This is the tangled rope signature — coordination function genuine but extraction accumulating. The classification prevents mislabeling because: (a) beneficiaries exist (affected persons genuinely gain redress) so it's not a pure snare; (b) victims exist on both sides so it's not a rope; (c) active enforcement required (causal contribution standards, allocation enforcement); (d) no sunset clause — the value chain liability problem is structural, not transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the shared liability reading a distinct constraint from developer_liability and deployer_liability, or do they represent different observable facets of the same underlying arrangement?',
    'Apply the ε-invariance test: if the extractiveness, beneficiary set, and victim set differ structurally across the three readings, they are distinct constraints. The shared liability reading places BOTH developers and deployers in the victim set and adds insurance markets as beneficiaries — a different structural signature.',
    'If distinct, each reading gets its own constraint story linked via network.affects_constraints. If not, the kernel label ''liability_attribution'' conceals a single constraint with observer-dependent classification — a category error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three liability readings are structurally distinct constraints or one constraint viewed from three angles').

omega_variable(
    causal_contribution_operationalizability,
    'Can causal contribution be operationalized in a way that produces stable, contestable liability shares across the value chain, or does the metric itself create a new extractive layer?',
    'Track litigation outcomes and contractual allocations over 3-5 years: if causal contribution metrics converge on stable shares, the coordination function is real; if they fragment into endless expert battles, the metric becomes an extraction engine for forensic economics.',
    'Stable operationalization → genuine tangled rope (coordination + extraction). Unstable → the coordination story is cover; the constraint is a snare for forensic economics and expert witnesses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_contribution_operationalizability, empirical, 'Whether the core coordination metric (causal contribution) is a real coordination device or an extraction enabler').

omega_variable(
    insurance_market_capture_risk,
    'Do emerging AI liability insurance markets function as risk pools that reduce systemic cost, or do they become rent-extracting intermediaries that capture the coordination surplus?',
    'Compare premium-to-loss ratios and market concentration indices over the interval. Concentrated markets with high premium-to-loss ratios indicate capture; competitive markets with ratios near actuarial fair value indicate genuine risk pooling.',
    'Insurance capture would shift the constraint toward snare — the coordination function (risk distribution) becomes the extraction mechanism. Genuine pooling sustains the tangled rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(insurance_market_capture_risk, empirical, 'Whether insurance/indemnification markets serve coordination or become extractive intermediaries').

omega_variable(
    small_enterprise_exit_viability,
    'Can small enterprises practically exit or limit exposure to the shared liability regime through contractual allocation, or does the opacity burden effectively trap them?',
    'Survey small enterprise adoption rates, contract negotiation outcomes, and bankruptcy filings attributable to liability exposure. Track whether contractual allocation clauses are enforced or overridden by courts.',
    'If small enterprises are effectively trapped (identity_locked exit), their victim status is structural and the constraint''s extraction is amplified. If they have mobile exit via contractual allocation, the coordination function is more balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_enterprise_exit_viability, empirical, 'Whether small enterprises have viable exit from the shared liability regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(liab_tr_t0, observed).
narrative_ontology:measurement(liab_tr_t2, liability_attribution__shared_liability, theater_ratio, 2, 0.16).
narrative_ontology:measurement_basis(liab_tr_t2, observed).
narrative_ontology:measurement(liab_tr_t4, liability_attribution__shared_liability, theater_ratio, 4, 0.21).
narrative_ontology:measurement_basis(liab_tr_t4, observed).
narrative_ontology:measurement(liab_tr_t6, liability_attribution__shared_liability, theater_ratio, 6, 0.25).
narrative_ontology:measurement_basis(liab_tr_t6, observed).
narrative_ontology:measurement(liab_tr_t8, liability_attribution__shared_liability, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(liab_tr_t8, projected).
narrative_ontology:measurement(liab_tr_t10, liability_attribution__shared_liability, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(liab_tr_t10, projected).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(liab_be_t0, observed).
narrative_ontology:measurement(liab_be_t2, liability_attribution__shared_liability, base_extractiveness, 2, 0.31).
narrative_ontology:measurement_basis(liab_be_t2, observed).
narrative_ontology:measurement(liab_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(liab_be_t4, observed).
narrative_ontology:measurement(liab_be_t6, liability_attribution__shared_liability, base_extractiveness, 6, 0.43).
narrative_ontology:measurement_basis(liab_be_t6, observed).
narrative_ontology:measurement(liab_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(liab_be_t8, projected).
narrative_ontology:measurement(liab_be_t10, liability_attribution__shared_liability, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(liab_be_t10, projected).

% Suppression requirement over time
narrative_ontology:measurement(liab_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(liab_su_t0, observed).
narrative_ontology:measurement(liab_su_t2, liability_attribution__shared_liability, suppression_requirement, 2, 0.22).
narrative_ontology:measurement_basis(liab_su_t2, observed).
narrative_ontology:measurement(liab_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.27).
narrative_ontology:measurement_basis(liab_su_t4, observed).
narrative_ontology:measurement(liab_su_t6, liability_attribution__shared_liability, suppression_requirement, 6, 0.3).
narrative_ontology:measurement_basis(liab_su_t6, observed).
narrative_ontology:measurement(liab_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.31).
narrative_ontology:measurement_basis(liab_su_t8, projected).
narrative_ontology:measurement(liab_su_t10, liability_attribution__shared_liability, suppression_requirement, 10, 0.32).
narrative_ontology:measurement_basis(liab_su_t10, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, resource_allocation).
narrative_ontology:boltzmann_floor_override(liability_attribution__shared_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, ai_insurance_market_formation).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, causal_contribution_standardization).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the liability_attribution kernel: three readings with distinct structural signatures. shared_liability distributes victims across developer+deployer seats and adds insurance markets as beneficiaries. developer_liability concentrates victimhood on developers, excludes deployers from primary liability. deployer_liability concentrates victimhood on deployers, treats developers as upstream suppliers with limited liability. All three claim to solve the same founding problem (AI harm accountability) but instantiate different constraints with different ε values — the ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
