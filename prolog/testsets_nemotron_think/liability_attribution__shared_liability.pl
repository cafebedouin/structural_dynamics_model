% ============================================================================
% CONSTRAINT STORY: liability_attribution__shared_liability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   This constraint story captures the 'shared_liability' reading of the
 *   liability_attribution kernel: joint liability distributed along the AI
 *   value chain based on causal contribution and control. It is one of three
 *   live readings — the others being developer_liability (developers bear
 *   primary liability as creators) and deployer_liability (deployers bear
 *   primary liability as context-controllers). The shared_liability reading
 *   claims to solve the accountability gap of single-party regimes by making
 *   liability track actual decision authority across the chain. In practice,
 *   the causal-attribution test generates substantial coordination overhead,
 *   spawns insurance and legal-intermediary markets that extract from both
 *   developers and deployers, and distributes the opacity burden through
 *   contractual indemnification cascades. The constraint is claimed as
 *   tangled_rope: genuine coordination function (closing the accountability
 *   gap) coexisting with asymmetric extraction (both developer and deployer
 *   seats pay; insurance/legal/compliance seats collect). Active enforcement
 *   is required — courts and regulators must continually adjudicate 'causal
 *   contribution' in novel technical contexts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_attribution__shared_liability, 0.65).
domain_priors:suppression_score(liability_attribution__shared_liability, 0.55).
domain_priors:theater_ratio(liability_attribution__shared_liability, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, extractiveness, 0.65).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(liability_attribution__shared_liability, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(liability_attribution__shared_liability, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_attribution__shared_liability, tangled_rope).
narrative_ontology:human_readable(liability_attribution__shared_liability, "Joint Liability Distributed Along AI Value Chain by Causal Contribution and Control").
narrative_ontology:topic_domain(liability_attribution__shared_liability, "technology_governance/legal_theory/regulatory_design").

domain_priors:requires_active_enforcement(liability_attribution__shared_liability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liability_attribution__shared_liability, 'f42575f1-4778-4865-a5e0-d01f88a2aa29').
narrative_ontology:cs_kernel_codification('f42575f1-4778-4865-a5e0-d01f88a2aa29', formalized).
narrative_ontology:cs_authority_grounding('f42575f1-4778-4865-a5e0-d01f88a2aa29', expertise).
narrative_ontology:cs_interpretation_layer_present('f42575f1-4778-4865-a5e0-d01f88a2aa29').
narrative_ontology:cs_reading_relation('f42575f1-4778-4865-a5e0-d01f88a2aa29', liability_attribution__developer_liability, coexists_with).
narrative_ontology:cs_reading_relation('f42575f1-4778-4865-a5e0-d01f88a2aa29', liability_attribution__deployer_liability, coexists_with).
narrative_ontology:cs_axiom('f42575f1-4778-4865-a5e0-d01f88a2aa29', foundational, liability_tracks_causal_contribution_and_control).
narrative_ontology:cs_axiom_status(liability_tracks_causal_contribution_and_control, holdable).
narrative_ontology:cs_axiom_grounding('f42575f1-4778-4865-a5e0-d01f88a2aa29', liability_tracks_causal_contribution_and_control, instrumental).
narrative_ontology:cs_axiom('f42575f1-4778-4865-a5e0-d01f88a2aa29', secondary, value_chain_accountability_closes_victim_gaps).
narrative_ontology:cs_axiom_status(value_chain_accountability_closes_victim_gaps, holdable).
narrative_ontology:cs_axiom_grounding('f42575f1-4778-4865-a5e0-d01f88a2aa29', value_chain_accountability_closes_victim_gaps, instrumental).
narrative_ontology:cs_reference_frame('f42575f1-4778-4865-a5e0-d01f88a2aa29', proportional_enterprise_liability).
narrative_ontology:cs_drift_state('f42575f1-4778-4865-a5e0-d01f88a2aa29', post_ai_act_liability_directives, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f42575f1-4778-4865-a5e0-d01f88a2aa29', '').
narrative_ontology:cs_kernel_id(liability_attribution__shared_liability, liability_attribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, insurance_providers).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, legal_intermediaries).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, compliance_industry).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_developers).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, ai_deployers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(liability_attribution__shared_liability, affected_public).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, affected_public).
narrative_ontology:constraint_victim(liability_attribution__shared_liability, open_source_maintainers).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, proportional_liability_principle).
narrative_ontology:constraint_vindicates(liability_attribution__shared_liability, value_chain_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build foundation models and AI components. Bear liability exposure for harms traceable to model capabilities, training data choices, and architectural decisions. Cannot fully control downstream deployment contexts. Face rising insurance premiums and contractual indemnification demands. Exit options limited by sunk R&D investment and path-dependent model architectures.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_developers, payer,
    powerful, biographical, constrained, global).

% Integrate and deploy AI systems in specific applications (healthcare, finance, public services). Bear liability for deployment-context decisions: use-case selection, guardrail configuration, human oversight design, monitoring. Liability allocation depends on proving causal contribution vs. developer-layer defects. Face parallel insurance costs and compliance burdens. Exit constrained by organizational commitment to AI integration.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, ai_deployers, payer,
    powerful, biographical, constrained, global).

% Underwrite new AI liability insurance lines: model-level policies for developers, deployment-context policies for deployers, umbrella policies covering value-chain gaps. Premiums reflect opaque causal attribution and correlated risk across the chain. Benefit from mandatory or de-facto-mandatory coverage requirements. Can re-price and exit specific segments faster than developers/deployers can exit AI.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, insurance_providers, beneficiary,
    institutional, generational, arbitrage, global).

% Litigation firms, expert witnesses, compliance consultancies specializing in AI liability allocation. Revenue scales with causal-attribution complexity: multi-party discovery, technical forensics, cross-jurisdictional coordination. Benefit from the inherent opacity of 'causal contribution' as a legal standard. Low exit barriers — can pivot to other complex litigation domains.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, legal_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Audit firms, red-team providers, documentation-tool vendors selling 'liability-readiness' to both developers and deployers. Revenue tied to the coordination overhead of proving causal contribution and control at each value-chain link. Benefit from standards that require continuous re-verification. Mobile across regulatory domains.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, compliance_industry, beneficiary,
    organized, biographical, mobile, global).

% Design and enforce liability frameworks (EU AI Act liability directives, US sectoral guidance, international standards bodies). Set the legal test for 'causal contribution and control' — the interpretive surface where extraction and coordination contest. Face pressure from all seats. Exit is analytical: they observe the regime's effects and can propose amendments.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, regulators, agenda_setter,
    institutional, generational, analytical, national).

% End users and subjects of AI decisions (loan applicants, patients, citizens). Benefit from stronger safety incentives distributed across the value chain. Bear costs indirectly through higher service prices, reduced innovation in high-liability domains, and access restrictions. Exit is constrained — cannot opt out of AI-mediated systems in essential services.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, affected_public, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(liability_attribution__shared_liability, affected_public, payer).

% Maintain open-source models and tools incorporated into commercial value chains. Face disproportionate liability exposure relative to resources: no insurance capacity, no legal teams, no contractual leverage to allocate opacity burden downstream. Exit means abandoning the project — identity-locked for many maintainers. Often excluded from policy negotiations.
narrative_ontology:constraint_stakeholder(liability_attribution__shared_liability, open_source_maintainers, payer,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates liability for AI harms across the value chain by tracing causal contribution and control at each link — developer-layer choices (architecture, data, training) vs. deployer-layer choices (use-case, guardrails, monitoring) — so that victims have a viable redress path and safety incentives align with actual decision authority.
% TRANSFER_FUNCTION: Moves liability costs from injured parties to developers and deployers in proportion to their causal contribution and control; moves insurance premiums and compliance spend from developers/deployers to insurance_providers, legal_intermediaries, and compliance_industry; moves regulatory attention and rulemaking capacity to the interpretive layer that defines 'causal contribution'.
% ABSENT_VOICES: Open-source maintainers and small-scale developers are structurally excluded from the liability-allocation negotiation — they lack the resources to participate in standard-setting, lobbying, or insurance-market design. Downstream workers displaced by liability-driven automation restrictions are not represented. Jurisdictions without developed AI liability regimes (global south) have no seat at the table where transnational standards emerge.
% DISAPPEARANCE_RATIONALE: If joint liability vanished overnight, developers would externalize deployment-context risks entirely; deployers would face undifferentiated strict liability regardless of upstream defects; victims would lose the only viable multi-party redress path; insurance markets would collapse to single-party policies leaving value-chain gaps uninsured; the coordination function of tracing causality across the chain would disappear, reverting to either developer-only or deployer-only regimes.
% FOUNDING_PROBLEM: Single-party liability regimes (developer-only or deployer-only) create accountability gaps: developer-only lets deployers offload context-specific risks; deployer-only lets developers offload capability-layer defects. Victims fall into the gap when neither party's control fully explains the harm. The shared-liability reading was built to close this gap by making liability follow causal contribution and control across the entire value chain.
% FOUNDING_PROBLEM_CORROBORATION: Academic literature on enterprise liability and value-chain regulation (outside the insurance/legal-intermediary beneficiary set) corroborates the gap problem. Industry submissions to EU AI Act liability consultations from both developer and deployer coalitions acknowledge the gap but dispute the shared-liability solution. No independent corroboration that the specific 'causal contribution and control' test is workable at scale — the operationalization remains contested.
narrative_ontology:disappearance_verdict(liability_attribution__shared_liability, world_rearranges).
narrative_ontology:founding_problem_status(liability_attribution__shared_liability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(liability_attribution__shared_liability, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(liability_attribution__shared_liability, 'none', 1).
narrative_ontology:epsilon_provenance(liability_attribution__shared_liability, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects that both developer and deployer seats bear substantial liability costs, insurance premiums, and compliance spend, while intermediary markets capture a growing share. The coordination function is real — victims gain a multi-party redress path — but the causal-attribution standard is inherently opaque, creating extraction surface. Suppression (0.55) is moderate: the regime is enforced through courts and regulators, but alternative liability regimes (developer-only, deployer-only) remain legally and politically live; parties can lobby for regime shifts. Theater_ratio (0.4) captures performative compliance: documentation, red-teaming, and audit trails that satisfy the 'causal contribution' test procedurally without necessarily improving safety outcomes. Accessibility_collapse (0.5) reflects that alternative liability allocations exist but are costly to switch to (legislative overhaul, contractual re-negotiation across value chains). Resistance (0.6) is significant: developer and deployer coalitions actively contest the shared-liability framing in legislatures and courts.
 *
 * PERSPECTIVAL GAP:
 *   From the developer seat, the constraint looks like extractive overreach: they control model-layer decisions but are held liable for deployment-context harms they cannot control. From the deployer seat, it looks like unfair burden-shifting: they control deployment context but are liable for upstream model defects they cannot audit. From the insurance/legal/compliance seats, it looks like genuine coordination creating viable markets. From the regulator seat, it looks like the only regime that closes the accountability gap. The engine computes these divergent per-seat classifications from the structural data — the claimed_type does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers and deployers are structural payers (victims in base_properties): they bear the liability exposure, insurance costs, and compliance burden. Their exit is constrained — developers by sunk model investment, deployers by organizational AI integration. Insurance_providers, legal_intermediaries, and compliance_industry are beneficiaries: they collect premiums, fees, and compliance spend with arbitrage/mobile exit. Regulators are agenda_setters: they define the causal-attribution test and enforcement intensity. Affected_public are beneficiaries of the coordination function (safer systems) but payers of passed-through costs — dual role. Open_source_maintainers are payers with trapped exit: disproportionate exposure, no insurance access, identity-locked to their projects. The directionality derivation from these structural positions drives the engine's per-seat χ computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accountability gap in single-party regimes) is contested as still live. If the gap is substantially solved by other means (technical standards, contractual allocation, sectoral regulation), the shared-liability regime risks becoming a piton: persistent coordination theater maintained by the intermediary markets it spawned. The mandatrophy question is whether the causal-attribution test still serves the coordination function or has become a rent-extraction mechanism for the beneficiary seats. Current metrics suggest active extraction growth (rising extractiveness, theater) — a warning signal for mandatrophy drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the shared_liability reading''s structural profile differ from its siblings developer_liability and deployer_liability, and where is the disagreement located?',
    'Author sibling constraint stories (developer_liability, deployer_liability) with their own ε, beneficiaries, victims, and metrics. Compare the three readings'' victim sets, beneficiary sets, extractiveness, and suppression profiles. The disagreement is located in: (1) which seats bear primary liability (victim set), (2) whether coordination overhead scales with value-chain length (extractiveness trajectory), (3) whether insurance/intermediary markets are necessary coordination infrastructure or extractive capture.',
    'If developer_liability shows lower extractiveness and fewer intermediary beneficiaries, the shared_liability reading''s extraction may be attributable to the coordination mechanism itself rather than the liability principle. If deployer_liability shows similar extraction, the extraction may be inherent to any regime that assigns liability to deep-pocketed parties. The kernel-level classification depends on whether the three readings compute to the same or different types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural divergence across the three liability_attribution readings').

omega_variable(
    causal_attribution_operationalizability,
    'Can ''causal contribution and control'' be operationalized as a legal standard without generating unbounded coordination overhead?',
    'Track litigation outcomes, insurance pricing convergence, and contractual standardization over the interval. If causal attribution converges to workable heuristics (e.g., model-card disclosures, deployment logs, red-team benchmarks), coordination overhead stabilizes. If it remains perpetually contested, overhead grows unbounded.',
    'If operationalizable, the constraint may stabilize as rope (coordination function dominates). If not, extractiveness and theater_ratio will continue rising — the coordination story becomes cover for intermediary extraction, pushing toward snare or piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_attribution_operationalizability, empirical, 'Whether the coordination mechanism itself generates its own extraction surface').

omega_variable(
    open_source_disproportionate_burden,
    'Does the shared-liability regime impose disproportionate, existence-threatening liability on open-source maintainers who lack insurance capacity and contractual leverage?',
    'Empirical study of open-source project abandonment rates, maintainer liability incidents, and insurance-market exclusion post-regime adoption. Compare to proprietary developer seats.',
    'If confirmed, the victim set expands to a structurally trapped class (open_source_maintainers) with identity-locked exit — this would increase the constraint''s effective extraction for that seat and strengthen the snare/piton signal. It would also create a false-summit dynamic if the regime is presented as ''fair allocation'' while systematically excluding the least-resourced contributors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_disproportionate_burden, empirical, 'Disproportionate impact on resource-constrained value-chain participants').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_attribution__shared_liability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liability_attribution__shared_liability_tr_t0, liability_attribution__shared_liability, theater_ratio, 0, 0.15).
narrative_ontology:measurement(liability_attribution__shared_liability_tr_t4, liability_attribution__shared_liability, theater_ratio, 4, 0.22).
narrative_ontology:measurement(liability_attribution__shared_liability_tr_t8, liability_attribution__shared_liability, theater_ratio, 8, 0.28).
narrative_ontology:measurement(liability_attribution__shared_liability_tr_t12, liability_attribution__shared_liability, theater_ratio, 12, 0.33).
narrative_ontology:measurement(liability_attribution__shared_liability_tr_t16, liability_attribution__shared_liability, theater_ratio, 16, 0.37).
narrative_ontology:measurement(liability_attribution__shared_liability_tr_t20, liability_attribution__shared_liability, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(liability_attribution__shared_liability_be_t0, liability_attribution__shared_liability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(liability_attribution__shared_liability_be_t4, liability_attribution__shared_liability, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(liability_attribution__shared_liability_be_t8, liability_attribution__shared_liability, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(liability_attribution__shared_liability_be_t12, liability_attribution__shared_liability, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(liability_attribution__shared_liability_be_t16, liability_attribution__shared_liability, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(liability_attribution__shared_liability_be_t20, liability_attribution__shared_liability, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(liability_attribution__shared_liability_su_t0, liability_attribution__shared_liability, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(liability_attribution__shared_liability_su_t4, liability_attribution__shared_liability, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(liability_attribution__shared_liability_su_t8, liability_attribution__shared_liability, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(liability_attribution__shared_liability_su_t12, liability_attribution__shared_liability, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(liability_attribution__shared_liability_su_t16, liability_attribution__shared_liability, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(liability_attribution__shared_liability_su_t20, liability_attribution__shared_liability, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_attribution__shared_liability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(liability_attribution__shared_liability, 0.12).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__developer_liability).
narrative_ontology:affects_constraint(liability_attribution__shared_liability, liability_attribution__deployer_liability).

% DUAL FORMULATION NOTE:
% Part of the liability_attribution kernel family. This reading (shared_liability) distributes liability by causal contribution and control. Sibling readings: developer_liability (developer-primary), deployer_liability (deployer-primary). The ε values differ: shared_liability has higher extractiveness (dual victim set + intermediary markets) than either single-party reading. The kernel contest is over which liability allocation principle governs — not over measurement of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liability_attribution__shared_liability, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
