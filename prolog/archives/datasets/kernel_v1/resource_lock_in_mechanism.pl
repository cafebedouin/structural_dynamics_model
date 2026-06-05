% ============================================================================
% CONSTRAINT STORY: resource_lock_in_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_resource_lock_in_mechanism, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: resource_lock_in_mechanism
 *   human_readable: Resource Lock-in Mechanism in AI Safety Research
 *   domain: ai_safety/technology_governance/risk_assessment
 *
 * SUMMARY:
 *   The resource lock-in mechanism in AI safety research creates a structural
 *   asymmetry between compute-rich institutions (large labs, well-funded
 *   safety organizations, cloud providers) and compute-poor actors
 *   (independent researchers, novel methodology developers, safety
 *   epistemology pluralists). This constraint combines genuine coordination
 *   problems — frontier safety research genuinely requires access to frontier
 *   models — with extractive dynamics: those who control compute access also
 *   shape what safety questions get asked, which methodologies get validated,
 *   and which researchers can participate. The constraint exhibits three
 *   distinct structural features: (1) a material barrier (you cannot verify
 *   GPT-4 safety claims on your laptop), (2) an institutional barrier
 *   (compute-holding organizations control which researchers gain access),
 *   and (3) an epistemic barrier (the field converges on
 *   compute-holder-aligned safety methodologies because only those
 *   methodologies can be empirically validated at scale). The theater_ratio
 *   (0.55) reflects that grant programs, conferences, and journals claim to
 *   evaluate safety research merit, but the actual constraint enforcement
 *   happens through proprietary compute access — the academic evaluation
 *   system is performatively independent while functionally downstream of
 *   industry resource control.
 *
 * KEY AGENTS:
 *   - Independent Researchers: Primary victims (powerless/trapped) — cannot access compute required for frontier-level safety research; face barriers to publication and funding
 *   - Novel Methodology Developers: Primary victims (moderate/constrained) — research approaches that don't align with compute-holder priorities cannot be empirically validated; face high barriers to proof-of-concept
 *   - Safety Epistemology Diversity: Collective victim (powerless/trapped) — the field cannot sustain multiple competing epistemologies for safety when only compute-holder-aligned methods can reach empirical maturity
 *   - Compute Resource Holders (Cloud Providers, AI Labs): Primary beneficiaries (institutional/arbitrage) — control research directions through access allocation; extract research insights for product development; shape industry narrative around safety priorities
 *   - Established Safety Labs: Secondary beneficiary (institutional/constrained) — receive substantial allocations and influence; constrained by dependency on continued compute access and implicit alignment with compute-holder priorities
 *   - Academic Grant System: Performative actor (institutional/arbitrage) — maintains appearance of research direction control but functionally downstream of proprietary resource allocation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent resource distribution as inherent scientific requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(resource_lock_in_mechanism, 0.58).
domain_priors:suppression_score(resource_lock_in_mechanism, 0.62).
domain_priors:theater_ratio(resource_lock_in_mechanism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(resource_lock_in_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(resource_lock_in_mechanism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(resource_lock_in_mechanism, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(resource_lock_in_mechanism, tangled_rope).
narrative_ontology:human_readable(resource_lock_in_mechanism, "Resource Lock-in Mechanism in AI Safety Research").
narrative_ontology:topic_domain(resource_lock_in_mechanism, "ai_safety/technology_governance/risk_assessment").

domain_priors:requires_active_enforcement(resource_lock_in_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(resource_lock_in_mechanism, compute_resource_holders).
narrative_ontology:constraint_beneficiary(resource_lock_in_mechanism, established_safety_labs).
narrative_ontology:constraint_victim(resource_lock_in_mechanism, independent_researchers).
narrative_ontology:constraint_victim(resource_lock_in_mechanism, novel_methodology_development).
narrative_ontology:constraint_victim(resource_lock_in_mechanism, safety_epistemology_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT RESEARCHER (SNARE) — Lacks access to GPU clusters, proprietary model weights, and institutional compute budgets. Cannot verify claims about frontier AI systems without resources only labs control. Faces publication barriers in venues dominated by resource-rich institutions. Maximum extraction: researchers must abandon ambitious safety questions or join large labs, surrendering research autonomy.
constraint_indexing:constraint_classification(resource_lock_in_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER SAFETY LAB (TANGLED ROPE) — Partially integrated into resource allocation (cloud credits, some model access) but constrained by funding rounds and corporate partnership requirements. Benefits from coordination around shared safety evaluation protocols, but extraction persists: research directions must align with compute-provider priorities; reproducibility becomes a negotiation with proprietary platform owners.
constraint_indexing:constraint_classification(resource_lock_in_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPUTE RESOURCE HOLDER (ROPE) — Cloud providers and AI labs control access to frontier model weights and GPU clusters. Experience the constraint as pure coordination: allocating research credits enables external validation of safety claims (coordination benefit). Net beneficiary through pricing leverage, data from research results, and control over research agendas.
constraint_indexing:constraint_classification(resource_lock_in_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED SAFETY LAB (TANGLED ROPE) — Receives substantial compute allocations and model access, enabling ambitious research. But also constrained: funding depends on continued partnership with compute holders; research agendas implicitly directed toward questions compute providers find strategically useful; publication of critical findings may risk access revocation. Mixed: genuine coordination on safety evaluation, but asymmetric extraction on research autonomy and agenda-setting.
constraint_indexing:constraint_classification(resource_lock_in_mechanism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC GRANT SYSTEM (PITON) — Traditional peer-review-based research funding is performatively relevant but functionally downstream of resource lock-in. Grant programs claim to evaluate safety research merit, but computational resource access is controlled elsewhere (industry labs, cloud providers). The grant system persists as a coordinating mechanism but has lost functional control over research directions. Theater ratio high because grant committees simulate independence while actual constraints are enforced by proprietary resource holders.
constraint_indexing:constraint_classification(resource_lock_in_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, frontier AI safety research requires frontier model access; this is inherent to the domain. Verification of safety claims about GPT-scale systems requires GPT-scale compute; you cannot test trillion-parameter models on academic hardware. From this view, the resource concentration is an immutable natural law of the field. However, structural data reveals this as a false summit: the lock-in is contingent on proprietary model architectures and cloud computing business models, not on physics.
constraint_indexing:constraint_classification(resource_lock_in_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(resource_lock_in_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(resource_lock_in_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(resource_lock_in_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(resource_lock_in_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(resource_lock_in_mechanism, TR),
    TR >= 0.70.

:- end_tests(resource_lock_in_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The resource dependency creates asymmetric benefits: compute holders gain research insights and directional influence from subsidized external research; researchers gain necessary access but lose autonomy and compete for scarce allocation. The value is not at snare-level (0.70+) because coordination genuinely occurs — safety evaluation is possible and valuable. But it exceeds rope-level (0.35) because access is contingent on alignment with compute-holder preferences. The upward trajectory (0.35 → 0.48 → 0.58) reflects accumulating lock-in as frontier models become more concentrated in industry hands and the technical barrier to entry grows. Suppression (0.62): Moderate-high. Barriers include: (1) cost of frontier compute ($100K+ for large-scale experiments), (2) proprietary model weight restrictions (researchers cannot legally train on GPT-4), (3) publication norms (journals expect frontier-scale experiments), (4) career incentives (researchers who depend on cloud credits cannot build independent reputations), (5) institutional barriers (small labs have weaker negotiating positions than consortium members). The upward trajectory (0.48 → 0.62) reflects tightening proprietary restrictions and increasing compute cost. Theater ratio (0.55): Moderate. Academic evaluation mechanisms (peer review, grant committees, conference acceptance) appear to control research directions but lack actual enforcement power — compute access is the real gating mechanism. Grant programs sponsor safety research but cannot fund the compute itself; conferences validate methodologies but those methodologies must be computable within proprietary constraints; journals publish findings but those findings must involve proprietary systems. The theater persists because it legitimizes the constraint ('peer review determines what's good research') while the actual constraint is enforced elsewhere ('you can only do good research if you have compute access').
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural reality produces radically different classifications depending on structural position. The independent researcher sees a snare: their research questions are permanently constrained by compute unavailability; they have no exit option and no benefit. The established lab sees a tangled rope: genuine safety coordination occurs (they participate in shared evaluation protocols) but with asymmetric extraction (their research autonomy is implicitly constrained). The compute holder sees a rope: providing safety researcher access is a coordination solution to the legitimate problem of validating frontier systems. The open-science coalition (if it existed as a mature force) would see a scaffold: open-weight models are building an alternative pathway with a sunset clause ('as open models mature, proprietary access becomes less necessary'). The academic grant system sees none of this — it operates at the level of performative peer review, unaware that its verdicts are downstream of resource constraints it doesn't control. The civilizational analytical observer risks seeing a mountain: frontier AI safety requires frontier models; this is inherent to the field. But the structural data reveals the false summit: the requirement for proprietary access is contingent on current industry structure, not on physics or mathematics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective experiences different directional pressure (d values). Independent researchers face maximum extraction (d ≈ 0.92, full victim + trapped exit) — they cannot exercise agency within the constraint. Established safety labs face moderate extraction despite beneficiary status (d ≈ 0.58) because their autonomy is constrained by compute dependency. Compute resource holders face minimal extraction (d ≈ 0.08, beneficiary + arbitrage exit) — they control the constraint and can always exit by withdrawing resources. The scaffold perspective (visible in some framings) would assign d ≈ 0.42 to the Open Science Coalition perspective if included — organized actors building open-weight model ecosystems have some agency but face structural headwinds from proprietary capability gaps. The perspectival gap between the powerless researcher's snare (maximum extraction experienced) and the compute holder's rope (coordination experienced) is the core diagnostic signal for this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing coordination (the genuine need to test safety claims against frontier models) from extraction (the use of compute access as leverage to shape research directions). A pure coordination view would classify as rope: safety researchers need compute, compute holders provide it, mutual benefit. A pure extraction view would classify as snare: researchers are trapped by compute dependency, compute holders extract research insights and directional control. The tangled rope classification captures both: genuine coordination exists (frontier safety evaluation is possible and valuable) and genuine extraction exists (research autonomy is constrained, field directions are shaped by compute allocation choices). The false summit analytical perspective is crucial: it reveals that 'frontier safety requires proprietary access' is a claim that naturalizes industry control rather than describing an inherent natural law. The constraint could be reorganized (public compute investment, mandatory researcher access to proprietary systems, open-source development converging to frontier capabilities) — which means the resource lock-in is not a mountain, it is a socially contingent tangled rope that benefits from being framed as a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_weight_sufficiency,
    'Do open-weight model releases (Llama, Mistral) provide sufficient frontier-capability access for independent safety research, or do they lag proprietary systems enough that researchers remain locked into commercial access?',
    'Comparative safety evaluation capability: can researchers verify specific safety properties (jailbreak resistance, alignment, interpretability) using open-weight models at equivalent scale to claims about proprietary systems? Longitudinal tracking of safety research productivity from open-weight era vs proprietary-only era.',
    'If open-weight models suffice: resource lock-in degrades to a tangled rope (some extraction remains, but alternatives exist). If open-weight models lag: lock-in remains snare-level for frontier research; open-weight enables only lower-tier safety work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_weight_sufficiency, empirical, 'Sufficiency of open-weight models for frontier safety research').

omega_variable(
    research_direction_capture,
    'To what extent are safety research agendas shaped by compute-holder priorities (interpretability over alignment, safety evals over mechanism design) versus researcher-driven scientific questions?',
    'Citation and publication pattern analysis: research directions that require proprietary access vs those that do not; funding flow analysis; interviews with researchers about agenda constraints; historical case studies of safety research directions that were abandoned due to compute unavailability.',
    'If capture is substantial (>60%): extraction mechanism is institutional (agenda-setting control). If capture is low (<20%): constraint is primarily a coordination problem. Magnitude directly affects whether constraint remains tangled rope or degrades to rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(research_direction_capture, empirical, 'Degree of research agenda capture by compute holders').

omega_variable(
    natural_law_or_artifact,
    'Is the requirement for proprietary model access an inherent property of frontier AI safety research, or a contingent artifact of the current industry structure (proprietary architectures, closed training data, commercial cloud compute)?',
    'Historical counterfactual: what safety research would be possible if architectures and training data were open? Design analysis: what would research need to look like to avoid proprietary access dependency? Long-term trajectory: do open-source models converge toward parity with proprietary systems, or do proprietary systems maintain permanent capability gap?',
    'If natural law: mountain classification is correct, constraint is immutable. If artifact: constraint is a false summit — it is socially contingent and could be reorganized through open-source development, public compute investment, or regulation requiring safety researchers'' access to proprietary systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_or_artifact, conceptual, 'Whether resource lock-in is inherent to AI safety research or contingent to industry structure').

omega_variable(
    suppression_mechanism_structural_vs_intentional,
    'Is the measured suppression (barriers to independent research) primarily structural (you genuinely cannot run GPT-4 scale experiments on consumer hardware) or intentional (compute holders actively restrict access)?',
    'Analysis of access policies: are compute holders proactively denying researchers access, or is access simply unavailable due to cost/capacity? Behavioral evidence: do researchers with legitimate safety purposes gain access if they request it? Do compute holders invest in improving access for safety research, or do access barriers persist despite stated commitment to AI safety?',
    'If structural: suppression is difficult to change and lock-in may be unavoidable absent public compute investment. If intentional: suppression is a deliberate enforcement mechanism, making the constraint a more severe extraction device and suggesting policy intervention points (mandatory access, research right-to-audit, public compute infrastructure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_intentional, empirical, 'Whether suppression is structural or intentionally enforced').

omega_variable(
    research_autonomy_vs_resource_necessity_tradeoff,
    'What degree of research autonomy loss is acceptable in exchange for access to frontier model safety research? At what point does resource dependency become extractive rather than coordinative?',
    'Researcher interviews and surveys: self-reported autonomy constraints; cases of research directions modified or abandoned due to compute provider preferences. Comparison to adjacent fields: how much autonomy loss do researchers accept in other high-cost domains (particle physics, astronomy)? Institutional analysis: governance structures in resource-constrained research.',
    'If autonomy loss is minimal: constraint is closer to rope (coordination with minor asymmetry). If autonomy loss is severe: constraint is deeper tangled rope or snare. This determines whether the constraint can be reframed as legitimate access pricing or whether it constitutes extractive research capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(research_autonomy_vs_resource_necessity_tradeoff, preference, 'Acceptable tradeoff between research autonomy and resource access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(resource_lock_in_mechanism, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rlockin_tr_t0, resource_lock_in_mechanism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rlockin_tr_t3, resource_lock_in_mechanism, theater_ratio, 3, 0.5).
narrative_ontology:measurement(rlockin_tr_t6, resource_lock_in_mechanism, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(rlockin_be_t0, resource_lock_in_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rlockin_be_t3, resource_lock_in_mechanism, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(rlockin_be_t6, resource_lock_in_mechanism, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(rlockin_su_t0, resource_lock_in_mechanism, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(rlockin_su_t3, resource_lock_in_mechanism, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(rlockin_su_t6, resource_lock_in_mechanism, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(resource_lock_in_mechanism, resource_allocation).
narrative_ontology:affects_constraint(resource_lock_in_mechanism, ai_safety_methodology_convergence).
narrative_ontology:affects_constraint(resource_lock_in_mechanism, interpretability_alignment_resource_bias).
narrative_ontology:affects_constraint(resource_lock_in_mechanism, frontier_model_access_governance).

% DUAL FORMULATION NOTE:
% The resource lock-in mechanism is upstream of specific safety research methodologies. Downstream constraints (interpretability vs alignment trade-offs, which safety properties get empirically validated) are affected by which research directions receive compute allocation. Network decomposition: lock-in mechanism (this story) determines access distribution; methodology convergence (sibling story) documents which safety questions get asked; governance structure (sibling story) describes institutional arrangements that could reshape the lock-in.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(resource_lock_in_mechanism, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
