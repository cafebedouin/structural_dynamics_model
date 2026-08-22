% ============================================================================
% CONSTRAINT STORY: ai_safety_commitment__dual_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_safety_commitment__dual_priority_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ai_safety_commitment__dual_priority_reading
 *   human_readable: Dual-Priority AI Safety Commitment (Existential + Near-term)
 *   domain: technological/governance/ethical
 *
 * SUMMARY:
 *   The AI safety commitment to address both existential risk and near-term
 *   harms simultaneously represents a genuine attempt to hold two legitimate
 *   safety concerns as non-competing priorities. This reading instantiates
 *   that dual-priority structure: the constraint's stated function is to
 *   allocate institutional attention and resources to both risks, treating
 *   them as components of a unified safety framework. However, the reading
 *   faces a structural coherence problem: existential risk and near-term harm
 *   reduction operate on incommensurable timescales, have different victim
 *   sets (future populations vs. present discriminated groups), and compete
 *   for finite research resources. The constraint's apparent breadth masks a
 *   zero-sum resource competition between two research communities, each
 *   claiming institutional vindication from the dual-priority framing while
 *   actively suppressing the other's research through editorial gatekeeping
 *   and funding prioritization. The measurement series tracks rising
 *   extractiveness and theater ratio as the commitment increasingly functions
 *   as political cover for allocation decisions rather than as a genuine
 *   coordination mechanism—governance institutions use the dual-priority
 *   language to justify funding both camps while avoiding the hard choice of
 *   which research stream to actually prioritize.
 *
 * KEY AGENTS:
 *   - Existential risk researchers: agenda-setters on superintelligence alignment; benefit from civilizational framing and long-term institutional commitment; mobile power but organized influence.
 *   - Near-term harms researchers: agenda-setters on algorithmic bias and labor displacement; benefit from empirical validation and policy urgency; organized but typically underfunded relative to existential risk.
 *   - Workers displaced by deployed AI: powerless, trapped victims bearing immediate income loss and identity disruption; dependent on resource allocation translating into protective regulation.
 *   - Discriminated groups from AI systems: powerless, identity-locked victims facing algorithmic bias in high-stakes domains; cannot escape discrimination by changing identity or geographic location.
 *   - Future populations: powerless, trapped victims of existential risk; have no voice in present policy; their harm is the annihilation of all possible futures.
 *   - Safety governance institutions: institutional agenda-setters; collect authority and funding flows from being the arbiter of dual priorities; structurally unable to resolve the coherence problem.
 *   - AI deployment corporations: excluded from the table; would benefit from existential-risk-focused framing that constrains near-term regulation while keeping deployment lanes open.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, 0.68).
domain_priors:suppression_score(ai_safety_commitment__dual_priority_reading, 0.71).
domain_priors:theater_ratio(ai_safety_commitment__dual_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(ai_safety_commitment__dual_priority_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_safety_commitment__dual_priority_reading, tangled_rope).
narrative_ontology:human_readable(ai_safety_commitment__dual_priority_reading, "Dual-Priority AI Safety Commitment (Existential + Near-term)").
narrative_ontology:topic_domain(ai_safety_commitment__dual_priority_reading, "technological/governance/ethical").

domain_priors:requires_active_enforcement(ai_safety_commitment__dual_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_safety_commitment__dual_priority_reading, 'ebf1ca95-bced-462a-8760-2e7c12c6d8bc').
narrative_ontology:cs_kernel_codification('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', distributed).
narrative_ontology:cs_authority_grounding('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', distributed).
narrative_ontology:cs_reading_relation('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', ai_safety_commitment__existential_risk_reading, coexists_with).
narrative_ontology:cs_reading_relation('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', ai_safety_commitment__near_term_harms_reading, coexists_with).
narrative_ontology:cs_axiom('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', foundational, safety_problems_are_multidimensional).
narrative_ontology:cs_axiom_status(safety_problems_are_multidimensional, holdable).
narrative_ontology:cs_axiom_grounding('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', safety_problems_are_multidimensional, deontological).
narrative_ontology:cs_axiom('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', foundational, both_timescales_matter).
narrative_ontology:cs_axiom_status(both_timescales_matter, holdable).
narrative_ontology:cs_axiom_grounding('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', both_timescales_matter, instrumental).
narrative_ontology:cs_reference_frame('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', unified_safety_commitment).
narrative_ontology:cs_drift_state('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', contemporary_resource_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ebf1ca95-bced-462a-8760-2e7c12c6d8bc', '').
narrative_ontology:cs_kernel_id(ai_safety_commitment__dual_priority_reading, ai_safety_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, safety_governance_institutions).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, research_communities_both_camps).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, workers_displaced_by_deployed_ai).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, discriminated_groups_from_ai_systems).
narrative_ontology:constraint_victim(ai_safety_commitment__dual_priority_reading, future_populations_existential_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, existential_risk_researchers).
narrative_ontology:constraint_beneficiary(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Prioritize superintelligence alignment and extinction-level risk reduction as the primary safety objective. Frame this as the foundational safety problem because the stakes are total; near-term harms are real but negligible next to existential annihilation. Their resource flows depend on the dual-priority commitment being taken seriously; they administer major research programs and advise policy bodies on safety.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, agenda_setter,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, existential_risk_researchers, beneficiary).

% Prioritize documented harms from deployed systems—bias, discrimination, labor displacement, misinformation—as the urgent safety problem. Frame this as the actual safety frontier because these harms are observable, measured, and affecting real people today; existential risk is speculative and serves as cover for inaction on present harms. Their institutional standing and funding depend on near-term harm reduction being treated as equivalent to existential risk.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ai_safety_commitment__dual_priority_reading, near_term_harms_researchers, beneficiary).

% Face immediate income loss, retraining costs, and identity disruption from automation. They depend on safety commitments translating into protective regulation of deployment, but find themselves in a policy queue behind existential risk frameworks that do not foreground their harm. Their exit option is geographic/sectoral relocation; many lack capital to exercise it.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, workers_displaced_by_deployed_ai, payer,
    powerless, biographical, trapped, national).

% Experience algorithmic bias in hiring, criminal justice, lending, and welfare determinations. Their identity (race, gender, disability status, immigration status) cannot be changed to escape the harm; the constraint's ability to allocate resources to bias mitigation determines whether they face systematic exclusion. They are doubly trapped: they cannot opt out of algorithmic systems, and they cannot change the identity dimensions the systems discriminate on.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, discriminated_groups_from_ai_systems, payer,
    powerless, biographical, identity_locked, national).

% Would bear the total loss of humanity and its future in the event of misaligned superintelligence. They have no voice in present policy and cannot exit the risk. Their harm is not observable today but represents the ceiling of catastrophic possibility—any failure in existential risk reduction annihilates their entire domain of possible futures.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, future_populations_existential_risk, payer,
    powerless, civilizational, trapped, universal).

% Would benefit from a safety commitment that emphasizes existential risk (abstract, long-term, compatible with continued near-term deployment) over near-term harm reduction (concrete, regulatory, constrains current business models). They are not at the table in safety discourse but are the structural subject the constraint is supposed to govern. Their preferred reading would place existential risk in the distant future and near-term harms in the acceptable-collateral-damage category.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, ai_deployment_corporations, excluded,
    institutional, biographical, arbitrage, global).

% Administer safety research funding, set policy priorities, and coordinate between existential and near-term research communities. They face institutional pressure to credibilize both research streams simultaneously while making zero-sum allocation decisions. They collect institutional authority and funding flows from being the arbiter of the dual priority, but they cannot resolve the underlying coherence problem (how to allocate finite resources between two incommensurable risk types).
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, safety_governance_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Receive institutional legitimacy and funding from the dual-priority commitment. Each camp benefits from the framing that BOTH priorities are necessary (which justifies funding both), but they compete for the same resource pool. The dual-priority language allows both to claim institutional vindication while the zero-sum competition drives suppression of the other camp's research via editorial gatekeeping, funding prioritization, and narrative marginalization.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, research_communities_both_camps, beneficiary,
    organized, biographical, constrained, global).

% Examine the dual-priority commitment structure and ask whether it is coherent or a cover story masking resource competition. They produce analyses showing the constraint faces irreducible coherence problems: how to allocate under scarcity, which populations count as victims, what timescales matter, how to weigh speculative existential risk against documented present harm.
narrative_ontology:constraint_stakeholder(ai_safety_commitment__dual_priority_reading, policy_analysts_and_ethicists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_safety_commitment__dual_priority_reading, safety_governance_institutions).
narrative_ontology:fixing_cost_class(ai_safety_commitment__dual_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Commits safety governance institutions to treat both existential risk from misaligned superintelligence AND near-term harms from deployed systems as legitimate safety priorities deserving institutional attention and resources, breaking the false choice between them.
% TRANSFER_FUNCTION: Moves research funding, institutional authority, policy attention, and legitimacy from some recipient constituencies to others: existential risk research gets civilizational stakes framing and long-term institutional support; near-term harm research gets empirical validation and urgency; governance institutions get the role of arbiter; the actual victims (displaced workers, discriminated groups, future populations) receive policy framing but often not material intervention.
% ABSENT_VOICES: AI deployment corporations are structurally excluded from the safety research community and policy table—they are the regulated subject, not participants. They would argue that existential risk is speculative and should not constrain near-term innovation and deployment, and that near-term harms are acceptable trade-offs for economic progress. Deployment-affected populations (workers, discriminated groups) have minimal voice in setting research priorities and resource allocation despite bearing the most immediate documented harms.
% DISAPPEARANCE_RATIONALE: If the dual-priority commitment vanished, resource allocation would reorganize around whichever research stream commanded the most institutional power (likely existential risk, given its civilizational framing). Near-term harm reduction would deprioritize unless separately institutionalized. Governance would fragment into separate existential-risk and near-term-harm communities with no common commitment structure. The absence would change which harms get research attention, how funding flows, and whether safety is treated as a unified commitment or as competing research domains.
% FOUNDING_PROBLEM: Early AI safety discourse was captured by existential risk framing, marginalizing documented near-term harms from deployed systems. The dual-priority commitment emerged to vindicate both as legitimate concerns and to prevent safety discourse from being read as covering up present-day algorithmic bias, labor displacement, and misinformation.
% FOUNDING_PROBLEM_CORROBORATION: Existential risk researchers attest the founding problem is that near-term harm discourse obscures the scale of existential stakes. Near-term harm researchers attest the founding problem is that existential risk framing enabled decades of near-term harm to accumulate without regulatory attention. Policy analysts and deployment-affected advocates attest both founding problem narratives are partly correct and that the dual-priority commitment was a genuine attempt to hold both—but document that the commitment is now incoherent under resource scarcity.
narrative_ontology:disappearance_verdict(ai_safety_commitment__dual_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(ai_safety_commitment__dual_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_safety_commitment__dual_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_safety_commitment__dual_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ai_safety_commitment__dual_priority_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_safety_commitment__dual_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_safety_commitment__dual_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_safety_commitment__dual_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the dual-priority commitment transfers authority to governance institutions and research communities while leaving actual deployment-affected populations without material intervention—they are named as priorities but deprioritized in resource allocation. Suppression (0.71) is high because the framework requires actively suppressing the competing research community's findings: existential risk researchers marginalize empirical near-term harms as distracting from the civilizational stakes; near-term researchers dismiss existential risk as speculative and self-interested. Theater ratio (0.42) tracks the rising share of the commitment's operation devoted to managing the appearance of balance rather than actually resolving allocation decisions. Accessibility collapse (0.58) is moderate because researchers in both camps have some exit option (can move between institutions, can publish outside the commitment structure), but once the dual-priority framework is culturally entrenched, alternatives are harder to articulate. Resistance (0.74) is high because multiple stakeholders actively resist the framework: near-term harm advocates argue it masks existential-risk capture; existential risk advocates argue it dilutes alignment urgency; deployment corporations would resist either prioritization that constrains their business. The temporal measurements show extractiveness and theater rising over 25 time units as the commitment's incoherence becomes more acute: early in the interval (t=0-5), the dual-priority language was genuinely felt as bridge-building; by t=15+, it increasingly functions as political theater masking resource competition.
 *
 * PERSPECTIVAL GAP:
 *   Existential risk researchers and near-term researchers experience this constraint differently because they have different time horizons, different conceptions of what counts as a safety problem, and different resource dependencies. From the existential risk seat, the dual-priority commitment is a genuine safety commitment that vindicates superintelligence alignment as the foundational problem while giving near-term research institutional legitimacy without proportional resource claims. From the near-term harm seat, the same constraint is a rhetorical containment strategy: it names near-term harms as legitimate while systematically underfunding intervention, leaving documented present-day harms to accumulate while resources flow to the speculative existential problem. Governance institutions experience it as a coherence puzzle they cannot solve—they cannot actually allocate equally to both because the timescales are incommensurable, but they cannot openly choose one over the other without breaking the commitment. Deployment-affected populations experience it as an absence: they are named in safety discourse but receive minimal material protection, because both research communities are oriented toward academic and policy impact rather than toward immediate harm reduction for workers and discriminated groups. The engine computes per-seat types from this structural data; the sitting institutional agenda-setter and the sitting near-term payer will compute to different types on the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Existential risk researchers (d ~0.25): benefit from the dual-priority framework, which vindicates their research as foundational; they have organized power and mobile exit options, so they are net beneficiaries. Near-term researchers (d ~0.35): benefit institutionally from the commitment but compete with existential risk for resources; their institutional power is lower and more dependent on the commitment itself, so they are ambiguous—partly beneficiary, partly payer of the competition cost. Workers displaced by AI (d ~0.95): fully targeted; they bear immediate costs (income loss, retraining burden) with no proportional mitigation because resource allocation prioritizes research over deployment protection. Discriminated groups (d ~0.92): fully targeted; they bear ongoing algorithmic bias with minimal resource allocation to bias mitigation because the commitment's actual allocation favors existential and near-term research over application-layer harm prevention. Future populations (d ~1.0): fully targeted; they have no voice and cannot exit; their entire future domain depends on existential risk being solved correctly. Safety governance institutions (d ~0.55): symmetric-to-slightly-extractive; they benefit from arbitrating the commitment but pay the cost of managing a structurally incoherent allocation problem under institutional pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is NOT dead: near-term harms from deployed systems are documented and accumulating; existential risk from misaligned superintelligence remains contested but substantively discussed. The commitment was built to hold both as legitimate (founding problem: 'safety discourse was captured by one framing, marginalizing the other'). However, the foundational mandates are increasingly in tension: the dual-priority framing requires treating both as equivalent in importance, but they operate on incommensurable timescales and compete for finite resources. The constraint is not yet a piton (the founding problem is still live and the commitment still performs genuine coordination work), but it is showing Goodhart dynamics: theater ratio is rising, extractiveness is rising, and the commitment's output (balanced-sounding statements and dual-priority rhetoric) increasingly diverges from its input (where the actual resources actually flow). Mandatrophy is NOT resolved, but the mechanism is visible: governance institutions use the dual-priority language to justify allocation decisions that systematically favor existential risk, while maintaining the rhetorical frame that both are equivalent. The constraint will either resolve into a piton (theater overwhelming function as the coherence problem becomes undeniable) or explicitly foreclose one reading in favor of the other (existential risk researchers might win the resource competition, or near-term harm reduction might be institutionalized as a separate commitment).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_incommensurability,
    'Can existential risk research and near-term harm reduction be genuinely held as equivalent priorities under conditions of resource scarcity, or does the dual-priority commitment inevitably collapse into hidden prioritization?',
    'Longitudinal tracking of actual resource allocation over 5+ years: if funding remains roughly balanced, the commitment is coherent; if funding systematically flows to one domain, the commitment is a cover story. Qualitative analysis of research communities: if both thrive as independent research streams, equivalence is real; if one is marginalized, the commitment is rhetorical.',
    'If the commitment is incoherent, the constraint should be reclassified as a snare (extraction through deceptive framing of equivalence) rather than a tangled rope (genuine coordination with asymmetric outcomes). The victim set would narrow to those actually harmed by the resource misallocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_incommensurability, empirical, 'Whether dual prioritization is structurally possible or whether scarcity forces hidden ranking.').

omega_variable(
    temporal_incommensurability_of_victims,
    'Are the victims of existential risk (future populations with no voice) and the victims of near-term harms (present discriminated groups and workers) in the same moral and decision-theoretic frame, or are they incommensurable?',
    'Ethical analysis: do standard utilitarian, rights-based, or capability-based frameworks place future existential risk and present algorithmic bias on the same moral axis, or do they operate on different frameworks that cannot be reconciled? What happens when the two victim sets are forced into the same resource decision?',
    'If incommensurable, the dual-priority commitment is attempting to coordinate across irreconcilable moral frames—a characteristic pattern of false coordination (tangled rope collapsing toward snare as incoherence becomes visible). If commensurable, the commitment is genuinely attempting to hold both victim sets as mattering equally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_incommensurability_of_victims, conceptual, 'Whether present and future victims are in the same moral frame.').

omega_variable(
    suppression_mechanism_research_gatekeeping,
    'Is the measured suppression (0.71) structural (resource constraints force allocation choices) or internalized/performative (research communities suppress each other''s findings through editorial gatekeeping, reputational pressure, and institutional control)?',
    'Analysis of publication and funding records: are papers from the marginalized camp systematically rejected from top venues, deprioritized in funding cycles, and framed as non-safety-relevant in governance discourse? If suppression persists after resource constraints are eased, it is internalized; if it decreases, it is primarily structural.',
    'If suppression is largely internalized (communities actively suppressing each other), the constraint carries more extractiveness than the resource-scarcity story alone predicts—each community is using safety governance institutions to extract institutional authority and legitimacy from the other. If suppression is primarily structural (imposed by scarcity), the constraint is a genuine but incoherent attempt at dual prioritization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_research_gatekeeping, empirical, 'Whether suppression is structural resource competition or internalized institutional gatekeeping.').

omega_variable(
    future_populations_representation_paradox,
    'Can powerless, voiceless future populations be represented as victims in a present-day safety commitment, or does their inclusion inevitably abstract them into a rhetorical category disconnected from concrete advocacy?',
    'Compare resources allocated to near-term harm reduction (where present victims can advocate) versus existential risk research (where future populations cannot advocate). If present victims receive less protection despite being more able to organize for their interests, the commitment is not genuinely holding them as equivalent.',
    'If future populations are structurally unable to advocate for their own interests, the dual-priority commitment may be biased toward existential risk research regardless of its stated equivalence—governance institutions may be using future populations as a rhetorical anchor point without actually allocating resources to preventing their harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_populations_representation_paradox, conceptual, 'Whether voiceless future victims can be genuinely represented in allocation decisions.').

omega_variable(
    reading_coherence_under_force,
    'When the dual-priority reading is forced to choose (in a binding resource decision, regulatory choice, or governance moment), does it foreclose one of the sibling readings, or does it preserve both as live options?',
    'Empirical investigation when a high-stakes decision actually requires priority ranking (e.g., a regulatory body must choose between existential-risk-focused AI governance standards or near-term-harm-focused ones, with resources for only one). If the dual-priority commitment dissolves into one of the sibling readings, coherence is false.',
    'If the reading cannot sustain both priorities under actual choice pressure, it is not a stable reading of the kernel—it is a temporary bridging position. Reclassification would depend on which sibling reading emerges as the chosen one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coherence_under_force, empirical, 'Whether dual priority is stable or a temporary compromise under scarcity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_safety_commitment__dual_priority_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_s_tr_t0, ai_safety_commitment__dual_priority_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_s_tr_t5, ai_safety_commitment__dual_priority_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(ai_s_tr_t10, ai_safety_commitment__dual_priority_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(ai_s_tr_t15, ai_safety_commitment__dual_priority_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(ai_s_tr_t20, ai_safety_commitment__dual_priority_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(ai_s_tr_t25, ai_safety_commitment__dual_priority_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(ai_s_be_t0, ai_safety_commitment__dual_priority_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ai_s_be_t5, ai_safety_commitment__dual_priority_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ai_s_be_t10, ai_safety_commitment__dual_priority_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(ai_s_be_t15, ai_safety_commitment__dual_priority_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(ai_s_be_t20, ai_safety_commitment__dual_priority_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(ai_s_be_t25, ai_safety_commitment__dual_priority_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(ai_s_su_t0, ai_safety_commitment__dual_priority_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ai_s_su_t5, ai_safety_commitment__dual_priority_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(ai_s_su_t10, ai_safety_commitment__dual_priority_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(ai_s_su_t15, ai_safety_commitment__dual_priority_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ai_s_su_t20, ai_safety_commitment__dual_priority_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(ai_s_su_t25, ai_safety_commitment__dual_priority_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_safety_commitment__dual_priority_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_safety_commitment__dual_priority_reading, 0.22).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__existential_risk_reading).
narrative_ontology:affects_constraint(ai_safety_commitment__dual_priority_reading, ai_safety_commitment__near_term_harms_reading).

% DUAL FORMULATION NOTE:
% The ai_safety_commitment kernel is instantiated by three constraint stories, each representing a different reading of what AI safety requires and how to prioritize research and governance. The dual_priority_reading (this story) attempts to hold both existential risk and near-term harm reduction as equivalent non-competing priorities. The existential_risk_reading privileges superintelligence alignment as the primary safety problem. The near_term_harms_reading privileges documented present-day harms from deployed systems. These are not the same constraint viewed from different seats—they are different constraints instantiating different readings of the same kernel, each with different victim sets, timescales, and resource allocation logics. This story links to both siblings because its coherence depends on both readings remaining live options; if the commitment forecloses one in favor of the other, this reading collapses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_safety_commitment__dual_priority_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
