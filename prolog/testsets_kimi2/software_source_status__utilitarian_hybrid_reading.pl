% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Software Licensing Regime
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the utilitarian_hybrid_reading of the
 *   contested software_source_status kernel. It models the standing
 *   arrangement â and the normative framework justifying it â in which
 *   software licensing is chosen to maximize aggregate welfare: open source
 *   by default for infrastructure, proprietary permitted for specialized
 *   tools. The arrangement coordinates a mixed ecosystem but extracts
 *   asymmetrically from open source maintainers (uncompensated infrastructure
 *   labor) and from users of proprietary specialized tools (monopoly
 *   pricing). There is no categorical victim set; costs are
 *   context-dependent. The reading forecloses both the freedom-imperative and
 *   property-rights readings on logical grounds, while coexisting with the
 *   pragmatic-development reading.
 *
 * KEY AGENTS:
 *   - open_source_maintainers: Primary payer (moderate/constrained) â bears uncompensated labor costs for infrastructure.
 *   - proprietary_tool_users: Primary payer (moderate/constrained) â bears monopoly pricing in specialized niches.
 *   - infrastructure_adopters: Primary beneficiary (organized/mobile) â receives subsidized open infrastructure.
 *   - specialized_vendors: Secondary beneficiary (powerful/arbitrage) â captures permitted monopoly rents.
 *   - welfare_policy_architects: Agenda-setter (institutional/analytical) â administers the evaluative framework.
 *   - digital_rights_advocates: Excluded voice (organized/constrained) â rejects welfare framing.
 *   - ip_rights_maximalists: Excluded voice (organized/constrained) â rejects welfare subordination of property rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.48).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.5).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Software Licensing Regime").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__utilitarian_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'ad1d1ea5-9038-4afb-ae6c-97273bb0b27b').
narrative_ontology:cs_kernel_codification('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', distributed).
narrative_ontology:cs_authority_grounding('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', expertise).
narrative_ontology:cs_reading_relation('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_axiom('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', foundational, aggregate_welfare_maximization).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization, holdable).
narrative_ontology:cs_axiom_grounding('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', aggregate_welfare_maximization, instrumental).
narrative_ontology:cs_axiom('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', foundational, contextual_model_neutrality).
narrative_ontology:cs_axiom_status(contextual_model_neutrality, holdable).
narrative_ontology:cs_axiom_grounding('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', contextual_model_neutrality, empirically_contingent).
narrative_ontology:cs_reference_frame('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', welfare_maximizing_mixed_regime).
narrative_ontology:cs_drift_state('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', post_cloud_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad1d1ea5-9038-4afb-ae6c-97273bb0b27b', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, infrastructure_adopters).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, specialized_vendors).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, open_source_maintainers).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, proprietary_tool_users).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, welfare_economics_in_ip).
narrative_ontology:constraint_vindicates(software_source_status__utilitarian_hybrid_reading, mixed_innovation_ecosystems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and maintain foundational open source infrastructure under permissive licenses without guaranteed compensation. Their unpaid or underpaid labor sustains the open-default side of the hybrid regime. Exit is constrained by community obligation, reputational sunk cost, and the absence of alternative funding mechanisms tied to the same project scope.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_maintainers, payer,
    moderate, biographical, constrained, global).

% License specialized proprietary software for domains deemed welfare-justified under the hybrid model. They bear monopoly pricing, usage restrictions, and vendor lock-in. Exit is constrained by narrow niche specialization and lack of viable open alternatives in those niches.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_tool_users, payer,
    moderate, biographical, constrained, global).

% Freely adopt open source infrastructure such as operating systems, libraries, and protocols without licensing fees, benefiting from the hybrid regime's open-default rule for foundational layers.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, infrastructure_adopters, beneficiary,
    organized, biographical, mobile, global).

% Develop and sell proprietary specialized tools in domains where the hybrid model permits restriction. They capture rents justified by innovation-incentive arguments and retain exclusive control over source code and modification rights.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, specialized_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Government procurement officers, foundation strategists, and academic economists who allocate policy domains to open or proprietary models based on aggregate welfare calculations. They set the evaluative framework but do not themselves bear the direct costs or collect the commercial rents.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, welfare_policy_architects, agenda_setter,
    institutional, generational, analytical, national).

% Assert software freedom as a moral imperative independent of welfare outcomes. Structurally excluded from policy tables that frame licensing solely in cost-benefit terms rather than rights-based discourse.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, digital_rights_advocates, excluded,
    organized, civilizational, constrained, global).

% Assert absolute creator property rights in software. Excluded from frameworks that subordinate intellectual property to aggregate welfare optimization.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, ip_rights_maximalists, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches software licensing model to production context: open source for infrastructure where inspection, auditability, and network effects dominate; proprietary for specialized tools where concentrated investment incentives and customization rents are judged welfare-enhancing.
% TRANSFER_FUNCTION: Moves uncompensated labor from open source maintainers to public infrastructure; moves monopoly rents from specialized tool users to proprietary vendors; mediated by policy architects who contextually allocate domains to each model.
% ABSENT_VOICES: Digital rights advocates who reject consequentialist framing, and intellectual property maximalists who reject welfare subordination of creator rights. Both are structurally excluded from utilitarian policy tables.
% DISAPPEARANCE_RATIONALE: If the hybrid evaluative framework vanished, procurement and investment would shift toward either pure open or pure proprietary defaults. Open infrastructure would lose its policy privilege and funding, while specialized vendors would face commoditization or stronger open-source mandates.
% FOUNDING_PROBLEM: How to structure software production and distribution to maximize aggregate welfare when pure open source fails to fund specialized tool development and pure proprietary chokes foundational infrastructure with duplication, secrecy, and lock-in.
% FOUNDING_PROBLEM_CORROBORATION: Development economists and public digital strategy offices attest from outside the vendor beneficiary set; however, open source maintainers contest the welfare assessment, citing their own unsustainability as evidence the hybrid fails even on its own terms.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate: the hybrid extracts maintainers' labor on the open side and users' surplus on the proprietary side, but neither extraction is total. Suppression (0.50) reflects copyright enforcement and license compliance sustaining the regime. Theater ratio (0.35) captures the performative welfare justification that obscures commercial capture of open infrastructure. Accessibility collapse (0.45) indicates alternatives to the hybrid are marginalized but not eliminated. Resistance (0.60) is elevated because all three excluded readings actively contest the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (policy architects), the arrangement is a technocratic coordination solving a genuine welfare problem. From the payer seats (maintainers locked into sustaining uncompensated infrastructure, users locked into proprietary specialized tools), the same structure reads as enforced extraction whose welfare rationale rationalizes their costs. The engine computes this divergence from structural data; the claim does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Infrastructure adopters and specialized vendors are beneficiaries (low d): they receive subsidized infrastructure or permitted monopoly rents. Open source maintainers and proprietary tool users are payers (high d): they bear uncompensated labor costs and monopoly pricing respectively. Policy architects sit near symmetric (d ~0.5): they neither collect rents nor bear the direct costs, but administer the framework. Because victimhood is context-dependent, a maintainer who is also an infrastructure adopter experiences a mixed directional signal; the structural derivation captures their net position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents mislabeling the hybrid as pure coordination (rope) by insisting on named payers and active enforcement. It also prevents mislabeling it as snare by recognizing the genuine coordination function of matching licensing to context. The utilitarian framing risks mandatrophy if welfare arguments persist after the empirical conditions supporting them dissolve; the contested founding_problem_status and rising theater_ratio are monitored for this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    context_dependent_victimhood,
    'Does the hybrid regime produce fixed victim categories, or are costs borne by whichever party occupies the context-specific losing position?',
    'Cross-context seat analysis: trace the same actor through infrastructure and specialized tool domains to see if they switch from beneficiary to payer.',
    'If victimhood is purely contextual, the constraint is better modeled as a rope with rotating seats; if structural groups consistently lose, it is tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(context_dependent_victimhood, conceptual, 'Whether victimhood is structurally fixed or contextually rotating').

omega_variable(
    welfare_optimality_unmeasurability,
    'Can aggregate welfare in software ecosystems be measured with sufficient precision to actually optimize licensing mix?',
    'Longitudinal comparative studies of innovation rate, accessibility, and maintainer sustainability across jurisdictions with different hybrid balances.',
    'If welfare is unmeasurable, the hybrid regime functions as an unverifiable justification for status-quo allocation, increasing theater_ratio and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_optimality_unmeasurability, empirical, 'Whether aggregate welfare is measurable enough to guide licensing policy').

omega_variable(
    sibling_reading_structural_pressure,
    'This constraint is the utilitarian_hybrid_reading of kernel software_source_status. How would adopting a sibling reading restructure the beneficiary-victim map?',
    'Generate the sibling constraint stories (freedom_imperative, pragmatic_development, property_rights) and compare per-seat computed classifications.',
    'Reveals whether the no-categorical-victim-set property is unique to this reading or an artifact of unresolved contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_pressure, conceptual, 'Structural delta between this kernel reading and its siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(software_source_status_util_hyb_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(software_source_status_util_hyb_tr_t6, software_source_status__utilitarian_hybrid_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(software_source_status_util_hyb_tr_t12, software_source_status__utilitarian_hybrid_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(software_source_status_util_hyb_tr_t18, software_source_status__utilitarian_hybrid_reading, theater_ratio, 18, 0.29).
narrative_ontology:measurement(software_source_status_util_hyb_tr_t24, software_source_status__utilitarian_hybrid_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(software_source_status_util_hyb_tr_t30, software_source_status__utilitarian_hybrid_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(software_source_status_util_hyb_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(software_source_status_util_hyb_be_t6, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(software_source_status_util_hyb_be_t12, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement(software_source_status_util_hyb_be_t18, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 18, 0.39).
narrative_ontology:measurement(software_source_status_util_hyb_be_t24, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(software_source_status_util_hyb_be_t30, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 30, 0.48).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_source_status__utilitarian_hybrid_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__utilitarian_hybrid_reading, software_source_status__property_rights_reading).

% DUAL FORMULATION NOTE:
% The software_source_status kernel decomposes into four structurally distinct constraints because the natural-language label 'software source status' conflates normative claims with different beneficiary/victim structures, epsilon values, and axiomatic foundations. Each reading instantiates a different constraint; they are linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
