% ============================================================================
% CONSTRAINT STORY: expert_disempowerment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expert_disempowerment, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: expert_disempowerment
 *   human_readable: Algorithmic Oversight Erosion: Expert Disempowerment
 *   domain: technological/social
 *
 * SUMMARY:
 *   Algorithmic oversight erosion represents the systematic replacement of
 *   expert discretionary judgment with rigid, automated decision-support
 *   systems across knowledge-intensive domains including medicine,
 *   engineering, finance, and law. The constraint exhibits genuine
 *   tangled-rope structure: algorithmic protocols provide real coordination
 *   benefits (standardization, consistency, reduced cognitive load,
 *   inter-institutional coordination) while simultaneously extracting the
 *   expert's ability to exercise contextual judgment, apply exception logic,
 *   and adapt to edge cases. The erosion is not sudden but gradual —
 *   initially systems are designed as decision-support (advising experts),
 *   but institutional pressure toward liability diffusion and efficiency
 *   optimization progressively converts them into decision-replacement
 *   systems (experts become implementers of algorithmic recommendations). The
 *   theater ratio (0.64) reflects that legacy oversight mechanisms
 *   (professional ethics boards, expert review committees, licensure bodies)
 *   persist in form while their functional authority has migrated to
 *   technical teams during system design. The extractiveness shows
 *   accumulation over the interval (0.28 → 0.52) as institutional lock-in
 *   increases and alternative pathways to professional authority narrow. The
 *   constraint exemplifies how coordination mechanisms can simultaneously
 *   serve extractive functions, creating genuine perspectival disagreement
 *   about whether the system is beneficial or exploitative.
 *
 * KEY AGENTS:
 *   - Domain Experts (physicians, engineers, loan officers, judges): Primary victims (powerless/trapped or identity_locked) — systematically devalued; discretionary authority extracted; career-dependent compliance required
 *   - Algorithm Developers: Primary beneficiaries (institutional/arbitrage) — develop and deploy systems; control technical choices; experience constraint as pure coordination
 *   - Institutional Authority (hospital administration, finance regulators, engineering management): Secondary beneficiaries (institutional/arbitrage) — gain efficiency, liability diffusion, reduced variance; benefit from standardization
 *   - Edge-Case Populations: Secondary victims (powerless/constrained) — underrepresented in training data; algorithmic decisions may misapply to their contexts; lack recourse through expert exception
 *   - Professional Organizations and Unions: Organized actors (organized/constrained) — can negotiate for governance, transparency, and override authority; see mixed coordination and extraction
 *   - Legacy Oversight Bodies: Institutional actors (institutional/arbitrage) — professional ethics boards, expert committees, licensure bodies; degraded to performative status; their functional authority has atrophied
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choices (scale, standardization) as inevitable technical necessities rather than decisions that could be made differently
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expert_disempowerment, 0.52).
domain_priors:suppression_score(expert_disempowerment, 0.58).
domain_priors:theater_ratio(expert_disempowerment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expert_disempowerment, extractiveness, 0.52).
narrative_ontology:constraint_metric(expert_disempowerment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(expert_disempowerment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expert_disempowerment, tangled_rope).
narrative_ontology:human_readable(expert_disempowerment, "Algorithmic Oversight Erosion: Expert Disempowerment").
narrative_ontology:topic_domain(expert_disempowerment, "technological/social").

domain_priors:requires_active_enforcement(expert_disempowerment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expert_disempowerment, algorithm_developers).
narrative_ontology:constraint_beneficiary(expert_disempowerment, institutional_efficiency_controllers).
narrative_ontology:constraint_beneficiary(expert_disempowerment, liability_shifting_agents).
narrative_ontology:constraint_victim(expert_disempowerment, domain_experts).
narrative_ontology:constraint_victim(expert_disempowerment, edge_case_populations).
narrative_ontology:constraint_victim(expert_disempowerment, contextual_judgment_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED EXPERT (SNARE) — Domain experts (physicians, engineers, loan officers) face institutional pressure to defer to algorithmic recommendations even when their judgment contradicts the system. Exit paths are structurally unavailable: questioning the algorithm jeopardizes employment, certification, and career trajectory. The expert's tacit knowledge and edge-case judgment capacity are systematically devalued. They perceive the constraint as pure extraction of their discretionary authority with minimal coordination benefit to themselves — the system coordinates institutional efficiency, not expert success.
constraint_indexing:constraint_classification(expert_disempowerment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CAUTIOUS PRACTITIONER (TANGLED ROPE) — Some experts retain partial discretion in practice: they can advocate for algorithm override in documented cases, propose exceptions, or work within margin-of-safety protocols. They experience genuine coordination benefits (consistency reduces communication overhead, standardization enables inter-institutional collaboration) alongside extraction of their judgment authority. Their exit options are constrained by career cost but not entirely foreclosed — senior experts with institutional standing can push back. Mixed experience: real benefit plus real extraction.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ALGORITHM DEVELOPER (ROPE) — Technical teams developing and deploying decision-support systems experience the constraint as pure coordination: the algorithm solves the institutional problem of standardizing complex judgment across thousands of practitioners. Developers have arbitrage options (exit into other domains, sell systems to competitors, influence upgrade cycles). They perceive genuine coordination function with no extraction cost to themselves — the extraction runs toward the experts, not toward the developers.
constraint_indexing:constraint_classification(expert_disempowerment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL AUTHORITY (ROPE) — Hospital administrators, finance regulators, engineering management see the algorithm as a coordination success: it reduces variance in individual expert judgment, improves consistency, enables liability diffusion (decision follows protocol, not individual discretion), and reduces training overhead. They have maximal arbitrage options — upgrade systems, change protocols, adjust weights. They perceive pure coordination benefit with minimal extraction cost. The constraint coordinates institutional needs exactly.
constraint_indexing:constraint_classification(expert_disempowerment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: IDENTITY-LOCKED SPECIALIST (TANGLED ROPE) — Senior experts whose professional identity is constituted through their judgment capacity face a specific trap: they are structurally mobile (could change careers, move to private practice, consult independently) but identity-fused with their role as authoritative decision-makers. The algorithm targets exactly this identity fusion — it says 'your judgment was valuable when expertise was scarce, but now the system has captured your knowledge; you are now a protocol follower.' Accepting the algorithm requires them to fundamentally reconstitute their professional self. They perceive both genuine coordination (reduced cognitive load, consistency) and severe extraction (identity dissolution), with the extraction mechanism being precisely the identity-lock — they cannot exercise their structural mobility because doing so would abandon the identity that gives them meaning.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY OVERSIGHT SYSTEM (PITON) — Traditional expert review boards, licensure bodies, and professional ethics committees are gradually degraded to performative status. They continue to exist (professional ethics approval required, expert review documented) but their function has been absorbed into the algorithm design phase. Actual oversight happens through algorithm design choices made by technical teams, not through expert committees reviewing decisions. The theater (the committee meeting, the ethics review, the expert sign-off) persists due to institutional inertia and regulatory habit, but the functional oversight has atrophied. Piton classification derives from theater_ratio 0.64 — most of the oversight appearance is ritual.
constraint_indexing:constraint_classification(expert_disempowerment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW / COMPUTATIONAL INEVITABILITY (MOUNTAIN) — From a computational-universalist perspective, algorithmic standardization is an inevitable consequence of scaling expertise to populations larger than can be served by human judgment alone. The constraint appears as a natural law: you cannot simultaneously maintain expert discretion and scale to millions of decisions. The choice is false — scale requires standardization, and standardization requires removing discretion. This perspective risks being a false summit: the 'scale' and 'standardization' may be institutional choices (we choose to deploy algorithms globally, we choose to require consistency) rather than natural limits.
constraint_indexing:constraint_classification(expert_disempowerment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: ORGANIZED RESISTANCE COALITION (TANGLED ROPE) — Professional organizations, unions, and expert guilds can collectively negotiate for algorithmic governance, transparency, and override authority. They see genuine coordination (standardization enables efficiency) but also real extraction (discretion removal), and they have partial agency through collective action. Exit paths include contract negotiation, regulatory capture, public pressure, and system redesign. The constraint from this view is mixedly beneficial and extractive, with active contestation over the terms.
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expert_disempowerment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expert_disempowerment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expert_disempowerment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expert_disempowerment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expert_disempowerment, TR),
    TR >= 0.70.

:- end_tests(expert_disempowerment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts expert discretion and judgment authority while providing genuine institutional coordination benefits. The score reflects that the extraction is substantial (experts lose override authority, career-dependent compliance required) but not total (some preserves discretion exist in bounded cases, senior experts with standing retain negotiation power). The upward trajectory (0.28 → 0.52 over the interval) reflects institutional lock-in: as more institutions deploy compatible systems, costs of maintaining alternative judgment-based pathways increase, and the extraction mechanism strengthens. Suppression (0.58): Moderate-high. Multiple suppression mechanisms operate simultaneously: (1) Career barrier — questioning algorithmic recommendations risks employment and certification; (2) Knowledge barrier — the algorithm is often opaque, making challenge difficult; (3) Institutional barrier — individual experts cannot override without bureaucratic process; (4) Collective-action barrier — experts lack coordination mechanisms to demand system change. Suppression is not absolute (some experts retain voice), but barriers are substantial. Theater ratio (0.64): Moderate-high. Professional ethics committees, expert review boards, and licensure bodies continue to exist and maintain ceremonial authority in algorithmic governance systems, but their functional oversight has been absorbed into technical teams during algorithm design. The apparent continued role of experts in governance is largely theatrical — the consequential decisions occur in feature engineering and model training, before any expert committee sees the system.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces profound perspectival divergence. Institutional authorities and algorithm developers see pure coordination (Rope) — the system solves the genuine problem of standardizing expert judgment across thousands of practitioners. They experience efficiency gains and liability benefit with no cost to themselves. The displaced expert sees pure extraction (Snare) — their judgment authority is stripped, their tacit knowledge is consumed and then discarded, and they are left as protocol followers with no discretion. They perceive zero coordination benefit and maximum extraction. The cautious practitioner with partial override authority sees mixed coordination and extraction (Tangled Rope) — they benefit from reduced cognitive load and consistency, but bear the cost of constrained discretion. The identity-locked specialist experiences the same tangled structure but through a different mechanism: they can structurally leave the field, but doing so would require abandoning the professional identity that constitutes their sense of self. The legacy oversight body sees its own degradation (Piton) — it continues to exist and perform ceremony, but its functional authority has atrophied. The organized coalition sees negotiable tangled rope (Tangled Rope) — genuine coordination exists, real extraction occurs, but collective power provides partial leverage to reshape terms. The analytical observer from a civilizational distance sees computational inevitability (Mountain) — that scale requires standardization and standardization requires removing discretion — but this risks naturalizing an institutional choice as a technical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position: beneficiary status (algorithm developers and institutional authorities benefit from standardization and liability diffusion); victim status (domain experts lose discretionary authority; edge-case populations lose contextual judgment); and exit options (institutional authorities have arbitrage options — switch systems, adjust weights, exit into other domains; displaced experts face trapped or constrained options — leaving means career cost; identity-locked experts are structurally mobile but identity-fused, so their d-value reflects the psychological binding not the material barriers). The algorithm developers' low d (beneficiary + arbitrage) produces negative or very low effective extraction (chi) from their perspective — they perceive the system as beneficial coordination with no cost. The displaced expert's high d (victim + trapped) produces high effective extraction (chi) — they perceive maximum extraction with minimal coordination benefit. The institutional authority's low d (beneficiary + arbitrage) produces negative chi — pure coordination benefit. The identity-locked specialist's d reflects the binding mechanism: structurally they should have moderate d (some exit options), but the identity fusion increases their experienced extraction — they cannot exercise structural mobility because doing so would dissolve their identity. The override was not applied for identity_locked cases because the derivation chain captures this through exit_options + beneficiary/victim declarations; identity_locked is handled through the exit_options field itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    edge_case_capture_vs_rare_outlier,
    'Are cases where expert judgment contradicts the algorithm predominantly genuine edge cases requiring contextual judgment, or are they predominantly rare statistical outliers where the algorithm is correct and the expert''s intuition is miscalibrated?',
    'Prospective audit: track cases where experts override algorithm recommendations; follow outcomes; measure success rates of expert judgment vs algorithmic judgment in those specific cases; distinguish systematic edge cases from individual expert bias',
    'If genuine edge cases: expert discretion should be preserved; constraint is extractive overreach (snare classification robust). If predominantly outliers: experts'' intuitions are unreliable; algorithm overrides risk introducing bias; constraint may be justifiably asymmetric (tangled_rope or rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(edge_case_capture_vs_rare_outlier, empirical, 'Whether algorithm contradictions reveal genuine edge cases or expert bias').

omega_variable(
    algorithm_knowledge_incorporation,
    'Does algorithmic decision-support actually incorporate domain expert knowledge (learned from historical expert judgments), or does it replace expert knowledge with statistical pattern-matching that mimics but does not preserve the causal reasoning underlying expertise?',
    'Explainability audit: extract decision-critical features from the algorithm; compare against domain causal models; measure correlation between algorithm feature importance and expert-identified causal mechanisms; test algorithm performance on novel scenarios outside training distribution',
    'If genuinely incorporates expert knowledge: the extraction is partial (experts'' knowledge IS in the system); constraint is tangled_rope with genuine coordination function. If pattern-matching only: expert knowledge is not preserved but consumed; the constraint is snare-like (extraction without compensation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_knowledge_incorporation, empirical, 'Whether algorithm incorporates causal expert knowledge or replaces it').

omega_variable(
    identity_lock_depth_and_reversibility,
    'For identity-locked experts (those whose professional identity is constituted through judgment authority), is the identity lock reversible through reframing (learning new professional role identity in system-collaboration rather than autonomous judgment), or does acceptance of the system permanently degrade their professional identity such that restoration requires exit from the field?',
    'Longitudinal study of experts post-system-adoption: track career satisfaction, sense of professional efficacy, identity narrative changes; identify cohorts who successfully reframe vs those who exit; measure reversibility of identity shifts through interviews with experts who moved to non-algorithmic contexts',
    'If reversible: identity-locked classification is provisional; experts can adapt. If permanent degradation: the identity lock is a severe extraction mechanism; identity-locked agents face snare-like constraints despite structural mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth_and_reversibility, empirical, 'Whether expert identity-lock is reversible through reframing').

omega_variable(
    oversight_theater_functionality,
    'Do the legacy expert review boards and ethics committees that persist in algorithmic-governance systems perform any genuine safety or accountability function, or are they purely performative — occurring post-hoc to document compliance rather than influencing actual algorithmic behavior?',
    'Process audit: track instances where expert committees recommend changes; measure adoption rate; compare algorithm behavior before/after review; identify whether reviews occur before or after deployment; measure feedback loop latency',
    'If genuinely functional: oversight is tangled with algorithmic governance, not piton; expert committees retain some decision power. If purely performative: piton classification confirmed; the theater maintains legitimacy while the functional oversight has migrated to technical design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oversight_theater_functionality, empirical, 'Whether legacy oversight committees perform genuine safety functions').

omega_variable(
    scale_necessity_vs_choice,
    'Is algorithmic standardization a necessary technical consequence of scaling to large populations, or is it an institutional choice that could be made differently (e.g., distributed expert networks, contextual decision-support rather than decision-replacement, slower scaling with human expert capacity growth)?',
    'Comparative institutional analysis: examine domains that scaled while preserving expert discretion (e.g., specialized medical consultations for complex cases); identify technical and institutional barriers to alternatives; model cost-trade-offs between expert-intensive and algorithmic approaches at different scales',
    'If necessary: the natural law perspective is partially justified; constraint may be mountain-like (inevitable cost of scale). If choice: the constraint is institutional (tangled_rope/snare), not natural; alternative arrangements are feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_necessity_vs_choice, conceptual, 'Whether algorithmic standardization is technically necessary or institutionally chosen').

omega_variable(
    liability_asymmetry_function,
    'Does institutional adoption of algorithmic decision-support genuinely reduce system-level liability and improve accountability, or does it primarily shift liability from institutions to algorithm developers (who disclaim responsibility) and experts (who lose authority over decisions), while preserving institutional risk through algorithm failure modes?',
    'Regulatory analysis: track liability allocation in contracts, legal precedent, and regulatory guidance; measure actual liability costs post-adoption; identify failure modes that algorithms don''t prevent; compare total system risk before/after',
    'If liability genuinely reduces: constraint provides coordination benefit to institutions; tangled_rope classification has merit. If liability shifts without reducing total risk: the benefit to institutions is illusory; extraction is higher (snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_asymmetry_function, empirical, 'Whether algorithmic governance reduces system liability or merely redistributes it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expert_disempowerment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expd_tr_t0, expert_disempowerment, theater_ratio, 0, 0.38).
narrative_ontology:measurement(expd_tr_t3, expert_disempowerment, theater_ratio, 3, 0.48).
narrative_ontology:measurement(expd_tr_t6, expert_disempowerment, theater_ratio, 6, 0.58).
narrative_ontology:measurement(expd_tr_t10, expert_disempowerment, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(expd_be_t0, expert_disempowerment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(expd_be_t3, expert_disempowerment, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(expd_be_t6, expert_disempowerment, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(expd_be_t10, expert_disempowerment, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(expd_su_t0, expert_disempowerment, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(expd_su_t5, expert_disempowerment, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(expd_su_t10, expert_disempowerment, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expert_disempowerment, enforcement_mechanism).
narrative_ontology:affects_constraint(expert_disempowerment, professional_deskilling).
narrative_ontology:affects_constraint(expert_disempowerment, liability_migration).
narrative_ontology:affects_constraint(expert_disempowerment, expertise_commodification).

% DUAL FORMULATION NOTE:
% Expert disempowerment decomposes into three structurally distinct constraints sharing the same domain: (1) professional_deskilling (ε ≈ 0.45) — the erosion of craft knowledge and judgment capacity through algorithmic labor division; (2) liability_migration (ε ≈ 0.38) — the shift of institutional risk to developers and experts while maintaining institutional benefit; (3) expertise_commodification (ε ≈ 0.52) — the transformation of expert knowledge into algorithmic training data and then its replacement with the algorithm. Expert_disempowerment is the overarching constraint capturing the joint extraction mechanism; the three decomposed stories trace distinct observable consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
