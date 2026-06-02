% ============================================================================
% CONSTRAINT STORY: ai_capability_acceleration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_capability_acceleration, []).

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
 *   constraint_id: ai_capability_acceleration
 *   human_readable: AI Capability Acceleration Constraint
 *   domain: artificial_intelligence/technology_policy
 *
 * SUMMARY:
 *   AI capability acceleration represents a structural constraint where the
 *   institutional incentives, competitive dynamics, and funding mechanisms
 *   that reward advancing AI capabilities simultaneously suppress investment
 *   in safety alignment, public governance capacity, and long-term
 *   institutional coordination mechanisms. The constraint exhibits mixed
 *   coordination and extraction: genuine technical and competitive
 *   coordination occurs (the original research laboratory benefits from
 *   pursuing capability advancement as a scientific objective), but this
 *   coordination is paired with systematic suppression of alternative
 *   pathways (safety-focused research, capability-constrained development,
 *   international coordination structures). The constraint's evolution over
 *   the past six years shows increasing extractiveness (from 0.35 to 0.58)
 *   paired with rising theater ratio (0.38 to 0.55), indicating that policy
 *   responses (capability audits, safety frameworks, governance boards) are
 *   increasingly performative while the underlying acceleration dynamic
 *   strengthens. The theater increase suggests institutional capture: policy
 *   frameworks are being deployed as legitimacy theater for capability
 *   advancement rather than as functional constraints.
 *
 * KEY AGENTS:
 *   - Capability-Leading Laboratories (Anthropic, OpenAI, DeepMind, etc.): Primary beneficiaries (institutional/arbitrage) — capture first-mover advantage, talent concentration, compute access, and agenda-setting power over research direction and governance standards
 *   - Safety-Focused Researchers: Primary victims (powerless/trapped) — face career penalty for safety-first research priorities; constrained by funding structures that reward capability papers; structural exit option is departure from the field
 *   - Public Governance Institutions (governments, international bodies, regulatory agencies): Secondary victims (moderate/constrained) — systematically excluded from capability development decisions; experience information asymmetry; constrained by technical capacity gaps and jurisdictional fragmentation; some exit capacity through regulatory coordination but high coordination costs
 *   - Compute Governance Coalition (chip manufacturers, export control regimes, cloud providers): Organized actors (organized/constrained) — see capability acceleration as temporary and solvable through compute access controls; building alternative enforcement pathways with viable sunset
 *   - Policy Framework Maintainers (AI safety boards, responsible disclosure committees, capability audit systems): Institutional actors maintaining Piton dynamics (institutional/arbitrage) — benefit from legitimacy conferral while actual constraint enforcement remains minimal; performative role persists through institutional inertia
 *   - Accelerationist Researchers: Secondary beneficiaries (moderate/mobile) — benefit from capability-focused reward structures; some agency to redirect toward safety but face career opportunity costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_capability_acceleration, 0.58).
domain_priors:suppression_score(ai_capability_acceleration, 0.68).
domain_priors:theater_ratio(ai_capability_acceleration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_capability_acceleration, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_capability_acceleration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_capability_acceleration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_capability_acceleration, tangled_rope).
narrative_ontology:human_readable(ai_capability_acceleration, "AI Capability Acceleration Constraint").
narrative_ontology:topic_domain(ai_capability_acceleration, "artificial_intelligence/technology_policy").

domain_priors:requires_active_enforcement(ai_capability_acceleration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_capability_acceleration, capability_leading_laboratories).
narrative_ontology:constraint_beneficiary(ai_capability_acceleration, frontier_ai_companies).
narrative_ontology:constraint_beneficiary(ai_capability_acceleration, accelerationist_researchers).
narrative_ontology:constraint_victim(ai_capability_acceleration, safety_alignment_research).
narrative_ontology:constraint_victim(ai_capability_acceleration, public_coordination_capacity).
narrative_ontology:constraint_victim(ai_capability_acceleration, long_term_ai_governance_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SAFETY-CONSTRAINED RESEARCHERS (SNARE) — Trapped within funding and publication structures that reward capability advancement over safety validation. Career incentives enforce acceleration; exit from the field is the only available option. No meaningful alternatives to participation in the acceleration dynamic. Maximum experienced extraction.
constraint_indexing:constraint_classification(ai_capability_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC GOVERNANCE INSTITUTIONS (TANGLED ROPE) — Constrained by technical capacity gaps and jurisdiction boundaries; genuine coordination function exists (international safety standards, compute governance) but systematic extraction via information asymmetry and agenda-setting power held by capability labs. Significant exit costs but not insurmountable through coalition-building.
constraint_indexing:constraint_classification(ai_capability_acceleration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CAPABILITY-LEADING LABORATORIES (ROPE) — Primary beneficiaries experience the acceleration constraint as pure coordination: pursuing capability advancement enables scientific progress, attracts talent, secures funding, and establishes institutional dominance. Net beneficiary relationship with minimal coercion perception — they author the norms being enforced.
constraint_indexing:constraint_classification(ai_capability_acceleration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPUTE GOVERNANCE COALITION (SCAFFOLD) — Organized actors (export control regimes, chip manufacturers, compute cluster operators) see acceleration as a temporary coordination failure with emerging sunset: compute governance, model weight access controls, and hardware supply chain leverage are building alternative mechanisms that decouple capability release from institutional prestige. Constrained but with visible exit pathway.
constraint_indexing:constraint_classification(ai_capability_acceleration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPABILITY-NEUTRAL POLICY FRAMEWORKS (PITON) — Traditional AI policy (AI safety boards, capability audits, responsible disclosure guidelines) persists through institutional inertia despite low functional effect on capability acceleration. The policy frameworks are largely performative theater: they assess risks but do not slow the underlying capability drive. Theater maintained because alternative governance structures haven't fully replaced it, not because frameworks effectively coordinate.
constraint_indexing:constraint_classification(ai_capability_acceleration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capability acceleration is inherent to technological competition: any agent with access to AI research infrastructure will advance capabilities as a function of competitive dynamics and resource availability. This perspective naturalizes acceleration as an immutable constraint of the technology landscape. However, the structural data reveals this as potential false summit — the 'inevitability' framing naturalizes what are contingent institutional arrangements (priority-reward systems, prestige asymmetries, compute concentration).
constraint_indexing:constraint_classification(ai_capability_acceleration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_capability_acceleration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_capability_acceleration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_capability_acceleration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_capability_acceleration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_capability_acceleration, TR),
    TR >= 0.70.

:- end_tests(ai_capability_acceleration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The capability acceleration constraint extracts from safety research primarily through resource allocation, publication prestige, and career incentive structures. The extraction is significant but not maximal because genuine scientific coordination occurs — capability advancement is a legitimate research objective, not pure predation. The value reflects that extraction is paired with real coordination benefits. The six-year increase from 0.35 to 0.58 indicates that as AI systems have grown more capable and economically significant, institutional concentration has strengthened, raising the extraction component. Suppression (0.68): High. Multiple overlapping barriers constrain safety-first research: funding concentrated in capability labs, publication bias toward capability papers, career penalties for safety focus, technical barriers to safety validation at frontier scales, and institutional fragmentation preventing public coordination. However, suppression is not total — some safety research receives funding, some labs prioritize safety, and exit options exist (though costly). Theater ratio (0.55): Moderate. Policy frameworks (AI safety boards, capability audits, responsible disclosure) serve partly genuine coordination and partly legitimacy functions. The theater has increased over six years as policy responses have proliferated without proportional constraint enforcement — policy theater is being deployed to justify continued capability acceleration while appearing responsive to governance concerns.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that the same structural acceleration dynamic is experienced as pure extraction (safety researchers see Snare), mixed coordination-extraction (public institutions see Tangled Rope), pure coordination (capability labs see Rope), solvable temporary problem (governance coalition sees Scaffold), degraded ritual (policy framework actors see Piton), and immutable law (civilizational analyst risks seeing Mountain). The gap is widest between the beneficiary perspective (Rope — acceleration is natural scientific progress) and victim perspectives (Snare — acceleration suppresses alternative research pathways). The analytical observer risks naturalizing the acceleration as inherent to technology competition, but the structural data reveals that institutional arrangements (funding concentration, prestige asymmetries, compute access control) are the enforcement mechanism, not laws of physics. The Tangled Rope classification from the public governance perspective is analytically critical: it shows that genuine coordination (international safety standards, compute governance) is possible but is currently suppressed by information asymmetry and agenda-setting power held by capability labs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from structural position. Capability laboratories are beneficiaries with arbitrage options (low d ~0.15) — they can walk away from the constraint through independent research or private development, but doing so is profitable to avoid. Safety researchers are victims with trapped exit (high d ~0.95) — they cannot leave the field without losing their research identity and career trajectory. Public institutions are victims with constrained exit (d ~0.65) — they can theoretically regulate capability development but face high coordination costs, technical capacity gaps, and jurisdictional fragmentation. The governance coalition has organized power with constrained exit (d ~0.45) — they control compute supply chains but face defection incentives from non-coalition members. The formula χ = ε × f(d) × σ(S) produces higher effective extraction for trapped victims (f(d) high) and lower extraction for beneficiaries with escape options (f(d) low). The scope modifier σ(S) = 1.2 (global) amplifies χ, reflecting that capability acceleration affects all jurisdictions through technology diffusion and international competition effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the coordination function from the extraction overlay. The genuine coordination function is: institutional research labs require incentive structures that reward capability advancement to attract talent, secure funding, and maintain competitive position in a global research economy. This is legitimate coordination — all competitive research ecosystems have this structure. The extraction overlay is: these same incentive structures systematically suppress alternative research pathways (safety-first development, capability constraints, long-term institutional coordination) that would serve the public governance institution's legitimate interests. The Tangled Rope classification captures both: the constraint coordinates capability advancement AND extracts from safety research. The classification prevents miscategorizing the constraint as pure extraction (which would imply capability advancement has no legitimate function — false) or pure coordination (which would imply the suppression of safety research is incidental — false). The omega variables are critical: if safety research decoupling is illusory, the extraction component increases; if compute scarcity is not binding, the constraint is more contingent (more changeable); if frontier capability equivalence is violated, victims face genuine technical trade-off (justifying higher extraction as efficiency cost rather than predation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_alignment_decoupling,
    'Can AI safety and alignment research be structurally decoupled from capability acceleration, or is the decoupling illusory?',
    'Longitudinal analysis of safety-focused lab productivity and publication rates versus capability-focused labs; measurement of safety research downstream citations in capability research; historical comparison to other fields with dual-track research (e.g., bioweapons / public health)',
    'If decoupling is real: constraint is pure extraction (Snare persists). If decoupling is illusory: constraint is coordination problem with genuine safety trade-off (Tangled Rope justified). Classification changes from victims-perceive-snare to victims-perceive-genuine-cost-benefit asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_alignment_decoupling, empirical, 'Whether safety research can be genuinely decoupled from capability advancement').

omega_variable(
    compute_scarcity_binding,
    'Is the acceleration constraint enforced by genuine compute scarcity, or does it persist primarily through funding and prestige incentives even when compute is abundant?',
    'Comparison of capability advancement rates in high-compute vs resource-constrained institutions; measurement of safety research productivity with equivalent compute allocation; analysis of capability-to-safety ratio in funded projects versus technical feasibility ratio',
    'If scarcity is binding: constraint is structural and may be immutable at current scale (Mountain-adjacent). If incentive-driven: constraint is a Tangled Rope with redesignable enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compute_scarcity_binding, empirical, 'Whether acceleration is enforced by compute scarcity or institutional incentives').

omega_variable(
    collective_action_threshold,
    'What proportion of capability labs must commit to capability-constrained development before the defection incentive collapses?',
    'Game-theoretic analysis of prisoner''s dilemma payoff matrices; measurement of competitive advantage persistence in hypothetical coordination scenarios; historical precedent analysis from nuclear non-proliferation, bioweapons convention adherence',
    'If threshold is low (~30-40%): Scaffold sunset is achievable through minority coalition. If threshold is high (~80%+): coordination requires near-universal commitment, making Scaffold dynamics unrealistic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_threshold, conceptual, 'Critical mass threshold for capability-constraint coordination').

omega_variable(
    frontier_capability_equivalence,
    'Do safety alignment methods scale equivalently with capability scaling, or does safety research face intrinsic efficiency losses relative to capability development?',
    'Empirical measurement of safety technique computational requirements versus capability scaling laws; analysis of safety research ROI curves; comparison to fields with dual-track advancement (medical research safety vs efficacy track)',
    'If equivalent scaling: victims are experiencing extractive governance (Snare). If safety scales poorly: victims face genuine technical trade-off (Tangled Rope justified). Resolves whether suppression is coercive or technical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontier_capability_equivalence, empirical, 'Whether safety scaling matches capability scaling efficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_capability_acceleration, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aica_tr_t0, ai_capability_acceleration, theater_ratio, 0, 0.38).
narrative_ontology:measurement(aica_tr_t3, ai_capability_acceleration, theater_ratio, 3, 0.46).
narrative_ontology:measurement(aica_tr_t6, ai_capability_acceleration, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(aica_be_t0, ai_capability_acceleration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aica_be_t3, ai_capability_acceleration, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(aica_be_t6, ai_capability_acceleration, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_capability_acceleration, resource_allocation).
narrative_ontology:affects_constraint(ai_capability_acceleration, safety_alignment_technical_feasibility).
narrative_ontology:affects_constraint(ai_capability_acceleration, ai_governance_institutional_capacity).
narrative_ontology:affects_constraint(ai_capability_acceleration, compute_concentration_dynamics).

% DUAL FORMULATION NOTE:
% AI capability acceleration is downstream of compute availability and technical feasibility of capability scaling laws, and is upstream of safety alignment research productivity and public governance institution capacity. Decomposition into separate constraints reflects different ε values: technical feasibility of scaling (ε ~0.10, Mountain) upstream of acceleration constraint (ε ~0.58, Tangled Rope) upstream of governance capacity (ε ~0.72, Snare). The acceleration constraint is the institutional and incentive layer connecting technical possibility to governance failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_capability_acceleration, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
