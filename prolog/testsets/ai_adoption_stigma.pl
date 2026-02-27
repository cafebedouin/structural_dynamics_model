% ============================================================================
% CONSTRAINT STORY: ai_adoption_stigma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_adoption_stigma, []).

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
 *   constraint_id: ai_adoption_stigma
 *   human_readable: Workplace Stigma Against Using Generative AI
 *   domain: technological/labor/organizational
 *
 * SUMMARY:
 *   Workplace stigma against generative AI use represents an emergent social
 *   norm that frames AI-assisted work as cheating, incompetence, or threat to
 *   job security. This constraint operates simultaneously as coordination
 *   (preserving shared epistemic standards for output quality), extraction
 *   (protecting incumbent workers and professional gatekeepers from
 *   competition), and temporary adaptation (as the labor market adjusts to
 *   AI-augmented workflows). The constraint exhibits high suppression (0.68)
 *   through multiple mechanisms: career risk for early adopters (performance
 *   suspicion, promotion bias), organizational policies that discourage AI
 *   use even where it improves efficiency, and professional standard bodies
 *   that frame AI as ethically inferior. The theater ratio (0.61) reflects
 *   that much of the professional objection is performative: substantive
 *   quality differences between AI-assisted and human-only work are often
 *   marginal, but the epistemic authority associated with 'human judgment'
 *   commands higher social valuation. The extractiveness (0.52) captures the
 *   intermediate position between pure extraction (preventing all adoption to
 *   protect jobs) and pure coordination (setting reasonable verification
 *   standards). The constraint's lifecycle shows rapid growth: theater and
 *   extractiveness both increased significantly over the interval as AI
 *   capabilities matured and workplace exposure expanded, suggesting the norm
 *   is still crystallizing rather than stable.
 *
 * KEY AGENTS:
 *   - Early Adopters: Powerless individual contributors (powerless/trapped) — bear full stigma cost; cannot exit without abandoning AI or leaving organization
 *   - Knowledge Worker Cohort: Moderate incumbents (moderate/constrained) — protected by stigma from displacement but constrained by limited mobility across resistant labor markets
 *   - Legacy Professional Services Firms: Institutional actors (institutional/arbitrage) — benefit from human-expertise positioning; can arbitrage between AI-adopting and AI-resistant market segments
 *   - AI-Native Tech Sector: Organized agents (organized/mobile) — see stigma as external temporary problem; have exit (talent mobility) and see sunset as inevitable
 *   - Professional Standard Bodies: Institutional gatekeepers (institutional/arbitrage) — maintain anti-AI rhetoric through certification, continuing education, and code of conduct enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional preference for human gatekeeping as irreducible epistemic limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_adoption_stigma, 0.52).
domain_priors:suppression_score(ai_adoption_stigma, 0.68).
domain_priors:theater_ratio(ai_adoption_stigma, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_adoption_stigma, extractiveness, 0.52).
narrative_ontology:constraint_metric(ai_adoption_stigma, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_adoption_stigma, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_adoption_stigma, tangled_rope).
narrative_ontology:human_readable(ai_adoption_stigma, "Workplace Stigma Against Using Generative AI").
narrative_ontology:topic_domain(ai_adoption_stigma, "technological/labor/organizational").

domain_priors:requires_active_enforcement(ai_adoption_stigma).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_adoption_stigma, incumbent_knowledge_workers).
narrative_ontology:constraint_beneficiary(ai_adoption_stigma, management_gatekeepers).
narrative_ontology:constraint_victim(ai_adoption_stigma, early_adopters).
narrative_ontology:constraint_victim(ai_adoption_stigma, productivity_optimization).
narrative_ontology:constraint_victim(ai_adoption_stigma, organizational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY ADOPTER (SNARE) — Powerless individual contributor who uses AI tools faces social ostracism, implicit performance suspicion, and career risk (marked as 'not a real worker', 'taking shortcuts'). Trapped by employment dependency and employment market reputation. Maximum extraction and suppression: cannot exit the workplace stigma without abandoning AI use or leaving the organization entirely. No coordination benefit perceived — pure extraction through social coercion.
constraint_indexing:constraint_classification(ai_adoption_stigma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: KNOWLEDGE WORKER COHORT (TANGLED ROPE) — Moderate power; constrained exit (can move to other employers but most face similar stigma). Experience mixed dynamics: some coordination benefit (shared epistemic standards preserve work quality perception) but also extraction (stigma protects against displacement, slowing adoption of more efficient tools). Benefits from coordination (job security during transition) but bears cost of constrained productivity. Partial agency and partial extraction.
constraint_indexing:constraint_classification(ai_adoption_stigma, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEGACY PROFESSIONAL SERVICES FIRM (ROPE) — Institutional beneficiary; arbitrage exit (can shift hiring, service delivery, and pricing based on AI policy). Experiences constraint as pure coordination: maintaining stigma against internal AI use allows firm to claim human expertise, justify premium pricing, and maintain human-centric positioning against rivals. No extraction — the firm gains through cultural differentiation. Net beneficiary.
constraint_indexing:constraint_classification(ai_adoption_stigma, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AI-NATIVE TECH SECTOR (SCAFFOLD) — Organized agents (startups, AI-first firms) see workplace AI stigma as a temporary coordination failure with a sunset. They experience low extraction because they have mobile exit: hiring AI-literate talent, rewarding AI integration, and capturing productivity gains. The stigma is external (legacy sector noise), not structural to their operations. The sunset clause is real: as labor markets tighten and productivity gaps widen, legacy firms will either adopt AI norms or lose talent. Estimated sunset: 3-7 years as generational workforce turnover accelerates.
constraint_indexing:constraint_classification(ai_adoption_stigma, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PROFESSIONAL STANDARD BODY (PITON) — Institutional actors (professional associations, certification bodies, industry councils) maintain anti-AI rhetoric and gatekeeping (e.g., CPA firms labeling AI-assisted tax prep as 'inadequate professional judgment', law firms claiming AI cannot understand client context). These standards are substantially performative: the substantive differences between AI-assisted and human-only work are often marginal, but professional bodies maintain the stigma to preserve membership value and licensing scarcity. Theater ratio high (0.61) reflects that much of the professional objection is about signaling rigor rather than demonstrable quality gaps. The institutional machinery (continuing education requirements, certification audits, professional codes) persists through inertia even as the functional justification erodes.
constraint_indexing:constraint_classification(ai_adoption_stigma, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some cognitive verification lag between tool output and human understanding is irreducible: AI systems cannot fully audit their own reasoning, and humans must bear epistemic responsibility for using them. The constraint appears as an immutable feature of tool-mediated knowledge work. However, structural data contradicts the mountain classification: the empirical threshold for 'verified AI output' vs 'unverified human intuition' is arbitrary and historically contingent. The engine will compute this as a false summit, revealing naturalization of institutional preference for human gatekeeping as inherent cognitive limit.
constraint_indexing:constraint_classification(ai_adoption_stigma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_adoption_stigma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_adoption_stigma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_adoption_stigma, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_adoption_stigma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_adoption_stigma, TR),
    TR >= 0.70.

:- end_tests(ai_adoption_stigma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, intermediate between pure coordination and pure extraction. The constraint does enforce some legitimate epistemic verification (AI output should be checked), but also extracts significant value from incumbents through job protection and from early adopters through stigma cost. The 0.52 value reflects that while there is real coordination content (quality standards), the extraction component is substantial and growing. Suppression (0.68): High. Multiple coercive mechanisms operate: career risk (performance suspicion, promotion bias against AI users), organizational policies (explicit or implicit rules against AI use), professional standard enforcement (certification loss, professional discipline), and social pressure (peer perception of 'cheating'). These are not formal barriers but collectively create high barriers to adoption. Theater ratio (0.61): Moderate-high and rising. Professional standard bodies maintain anti-AI positions through ritualized gatekeeping (certification audits, continuing education requirements) while substantive quality differences are often marginal. The growth trajectory (0.35 → 0.61) reflects increasing performative content as professional bodies formalize anti-AI positions despite insufficient empirical justification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The early adopter sees pure extraction (Snare): stigma is coercive with no coordination benefit. The knowledge worker sees hybrid dynamics (Tangled Rope): some coordination benefit (job security) mixed with extraction (constrained options). The legacy firm sees pure coordination (Rope): stigma solves their differentiation problem and captures premium pricing. The AI-native sector sees a temporary problem with exit (Scaffold): they can and do adopt, seeing the stigma as external legacy-sector inertia. The professional standard body sees degraded gatekeeping (Piton): the anti-AI rhetoric persists through institutional machinery even as empirical justification erodes. The analytical observer risks a false summit (Mountain): viewing epistemic verification requirements as immutable rather than historically contingent institutional choice. The presheaf over these perspectives reveals the constraint's structure: it is extraction for some, coordination for others, temporary for some, and degraded ritual for others — all simultaneously true.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply by agent structural position. Early adopters (powerless/trapped/low d potential) experience maximum extractiveness because they bear full stigma cost with no exit. Knowledge workers (moderate power/constrained exit) experience moderate extraction through job security trade-off: they benefit from stigma-based protection against displacement but pay in constrained career options and reduced productivity. Legacy professional services (institutional/arbitrage) experience negative extraction (pure benefit): they capture pricing premiums and client trust from human-expertise positioning. AI-native sector (organized/mobile) experiences low extraction because they have exit: they hire AI-literate talent, reward integration, and can compete on productivity. Professional standard bodies (institutional/arbitrage) experience negative extraction through gatekeeping: they extract membership value, certification scarcity, and professional prestige from maintaining anti-AI standards. The beneficiary-victim structure is clear: incumbents and gatekeepers benefit; early adopters and efficiency gains bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing the tangled rope classification is correct: the constraint has BOTH a genuine coordination function (epistemic verification, quality standards) AND asymmetric extraction (job protection for incumbents, stigma cost for early adopters). The coordination component is real but limited: reasonable verification standards exist, but the constraint goes well beyond verification into gatekeeping. The extraction component is also real and substantial: the constraint protects incumbent wages and professional prestige through artificial scarcity. The mandatrophy resolution is structural: if you remove the extraction component (stigma, career risk), the remaining coordination content (verification standards, quality checks) is modest and easily replaced by technical systems. If you remove the coordination framing entirely, the constraint appears as pure extraction (Snare). The tangled rope classification holds because both components are structurally required for the constraint to persist — remove either and the constraint destabilizes. The false summit risk from Perspective 6 highlights that framing the constraint as 'natural epistemic requirement' (Mountain) is itself a rhetorical extraction mechanism: it legitimizes the institutional gatekeeping by naturalizing it as inevitable. The analytical observer's danger is that the coordination and gatekeeping are deeply entangled in professional discourse, making the separation look impossible rather than contingent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_responsibility_boundary,
    'Where is the boundary between legitimate epistemic responsibility (humans verify AI output) and extractive gatekeeping (stigma prevents any AI contribution)?',
    'Comparative analysis of error rates and accountability outcomes in AI-assisted vs human-only workflows across domains (legal discovery, tax prep, medical coding, technical documentation); measurement of whether verification standards are applied uniformly to human vs AI work',
    'If boundary is clear and defensible: partial stigma may be justified coordination mechanism. If boundary is arbitrary: stigma is pure extraction protecting professional scarcity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_responsibility_boundary, empirical, 'Legitimate epistemic responsibility vs extractive gatekeeping threshold').

omega_variable(
    productivity_gain_distribution,
    'Do organizations that normalize AI use capture productivity gains through wage suppression, price reduction, or shared prosperity?',
    'Longitudinal wage and compensation analysis in AI-adopting vs AI-resistant sectors; comparison of service pricing and worker compensation in firms with vs without AI integration policies',
    'If gains are captured by capital: stigma in legacy firms is extraction that slows wage loss. If gains are shared: stigma is pure extraction that prevents worker benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productivity_gain_distribution, empirical, 'Distribution of AI productivity gains').

omega_variable(
    skill_obsolescence_rate,
    'Does AI adoption accelerate skill obsolescence for human workers, or does it shift skill requirements without net employment loss?',
    'Comparative career arc analysis: employment tenure and wage trajectories for workers in AI-adopting vs resistant organizations; industry wage premiums for AI-literacy vs traditional expertise',
    'If obsolescence is rapid: worker-side stigma is rational self-defense. If skill shift is manageable: stigma is institutional coordination that could be replaced by training systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_obsolescence_rate, empirical, 'Rate of skill obsolescence from AI adoption').

omega_variable(
    output_quality_parity,
    'For core professional tasks (legal research, medical coding, financial analysis), is AI-assisted work quality empirically indistinguishable from human-only work at equivalently careful standards?',
    'Blind comparison studies: external auditors evaluate work samples from AI-assisted and human-only workflows; statistical analysis of error, omission, and quality metrics without disclosure of production method',
    'If parity exists: stigma is pure extraction with no quality justification. If significant quality gaps exist: stigma may reflect real epistemic concerns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(output_quality_parity, empirical, 'Quality parity of AI-assisted vs human work').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_adoption_stigma, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aias_tr_t0, ai_adoption_stigma, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aias_tr_t3, ai_adoption_stigma, theater_ratio, 3, 0.52).
narrative_ontology:measurement(aias_tr_t6, ai_adoption_stigma, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(aias_be_t0, ai_adoption_stigma, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(aias_be_t3, ai_adoption_stigma, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(aias_be_t6, ai_adoption_stigma, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_adoption_stigma, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_adoption_stigma, knowledge_worker_wage_compression).
narrative_ontology:affects_constraint(ai_adoption_stigma, professional_licensing_scarcity).
narrative_ontology:affects_constraint(ai_adoption_stigma, organizational_productivity_ceiling).

% DUAL FORMULATION NOTE:
% The workplace AI stigma is downstream of general AI capability advances but upstream of specific labor market outcomes (wage structure, employment tenure, skill premiums). The constraint operates at the organizational and professional-norm level, distinct from technical capability constraints or individual adoption decisions. Network links track how stigma propagates through labor markets and professional institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_adoption_stigma, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
