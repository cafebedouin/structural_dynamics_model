% ============================================================================
% CONSTRAINT STORY: minority_representation_deficit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_minority_representation_deficit, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: minority_representation_deficit
 *   human_readable: Minority Representation Deficit in Decision-Making Institutions
 *   domain: political_economy/institutional_governance
 *
 * SUMMARY:
 *   Minority representation deficit in decision-making institutions
 *   (corporate boards, senior government, judiciary, academia, non-profit
 *   leadership) is a constraint that simultaneously appears as natural law
 *   (historical inertia), pure coordination problem (diversity improves
 *   decisions), temporary scaffolding (mandates build alternative pathways),
 *   degraded ritual (performative compliance), extraction mechanism
 *   (gatekeeping preserves majority control), and mixed hybrid (some
 *   opportunities created alongside suppression). The constraint exhibits
 *   seven distinct classification types from different structural positions,
 *   revealing how the same institutional phenomenon can be legitimately
 *   categorized across the taxonomy depending on observer position. The
 *   representation deficit persists despite decades of anti-discrimination
 *   law and growing diversity rhetoric, suggesting the underlying extraction
 *   mechanism adapts to enforcement rather than resolving. Theater ratio
 *   (0.64) reflects that diversity programs, training initiatives, and
 *   representation targets function primarily as compliance theater: visible
 *   reporting and policy announcements substitute for structural power
 *   redistribution. Extractiveness (0.58) indicates moderate-high extraction
 *   with partial coordination benefit — some minority professionals are
 *   genuinely advanced (coordination function), but the overall system
 *   maintains gatekeeping and informal exclusion (extraction function).
 *   Suppression (0.68) reflects multiple reinforcing mechanisms: pipeline
 *   effects, credential requirements, network exclusion, performance
 *   evaluation bias, and the identity-lock of being a 'diversity hire' that
 *   prevents many from exercising actual power even when formally present.
 *
 * KEY AGENTS:
 *   - Underrepresented Minorities: Primary victims (powerless/trapped) — excluded from decision-making pathways through credential gatekeeping, network effects, and hiring discrimination; no structural exit option within institution
 *   - Majority Demographic Gatekeepers: Primary beneficiaries (institutional/arbitrage) — capture decision-making control, economic benefits of centralized authority, and maintain informal network advantage
 *   - Institutional Gatekeepers (HR, Hiring Committees): Enforcing agents (institutional/constrained) — legally mandated to pursue diversity but retain hiring discretion through credential manipulation and informal preference
 *   - Minority Professional Cohort: Secondary agents (moderate/constrained) — intermediate group with constrained entry but not total exclusion; beneficiary of diversity programs, victim of tokenization and elevated performance scrutiny
 *   - Diversity Infrastructure (Committees, Training, Reporting): Performative actors (organized/constrained) — maintain theater of representation progress without substantive power redistribution
 *   - Mandated Representation Programs (Court Orders, Legislation): Coordinating force (organized/constrained) — force temporary structural change through legal scaffolding
 *   - Systemic Legitimacy and Decision Quality: Diffuse victim (powerless/trapped) — abstract collective benefit from diverse decision-making is suppressed by exclusion mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(minority_representation_deficit, 0.58).
domain_priors:suppression_score(minority_representation_deficit, 0.68).
domain_priors:theater_ratio(minority_representation_deficit, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(minority_representation_deficit, extractiveness, 0.58).
narrative_ontology:constraint_metric(minority_representation_deficit, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(minority_representation_deficit, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(minority_representation_deficit, tangled_rope).
narrative_ontology:human_readable(minority_representation_deficit, "Minority Representation Deficit in Decision-Making Institutions").
narrative_ontology:topic_domain(minority_representation_deficit, "political_economy/institutional_governance").

domain_priors:requires_active_enforcement(minority_representation_deficit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(minority_representation_deficit, majority_demographic_gatekeepers).
narrative_ontology:constraint_beneficiary(minority_representation_deficit, incumbent_institutional_holders).
narrative_ontology:constraint_victim(minority_representation_deficit, underrepresented_minorities).
narrative_ontology:constraint_victim(minority_representation_deficit, systemic_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MINORITY (SNARE) — Structural barriers to entry into decision-making positions are insurmountable within normal career pathways: pipeline effects, credential gatekeeping, network exclusion, and embedded hiring bias. Exit option is total relocation or organizational exit with severe economic penalty. Zero degree of freedom within the institution. Experiences maximum extraction: excluded from power, influence, and economic access while bearing costs of legitimacy deficits and policy misalignment.
constraint_indexing:constraint_classification(minority_representation_deficit, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MINORITY PROFESSIONAL COHORT (TANGLED ROPE) — Constrained by career risk (standing out as diversity hire, tokenization, performance scrutiny), but benefits from affirmative action policies, diversity initiatives, and coalition-building networks that lower entry barriers relative to excluded minorities. Mixed extraction: real barriers and career costs, but also real opportunities created by the coordination function of diversity programs. Generational horizon reflects that individual career advancement can occur but systemic representation change is slower.
constraint_indexing:constraint_classification(minority_representation_deficit, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJORITY DEMOGRAPHIC INCUMBENT (ROPE) — Experiences the constraint as pure coordination: minority representation enhances institutional legitimacy, reduces litigation risk, and improves decision quality through cognitive diversity. No extraction experienced by this group — beneficiary from the coordination mechanism. Can arbitrage out (transfer to institution with no diversity requirement) but benefits from staying. Net flow is toward this agent.
constraint_indexing:constraint_classification(minority_representation_deficit, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL GATEKEEPER (TANGLED ROPE) — Constrained by legal mandates, social pressure, and reputational requirements (ESG ratings, diversity reporting, customer sentiment). But also captures asymmetric benefits: controls hiring discretion, maintains informal network preference, and can performatively satisfy diversity requirements with minimal structural change. Requires active enforcement because gatekeepers must invest effort to prevent representation gains. Mixed extraction: enforcement burden on the gatekeeper, but discretion preserved through selective hiring and credential manipulation.
constraint_indexing:constraint_classification(minority_representation_deficit, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL DIVERSITY INFRASTRUCTURE (PITON) — Diversity committees, mandatory training, reporting requirements, and equity task forces persist through institutional inertia and legal compliance theatre. The primary function (changing culture, identifying barriers, enabling systemic change) is largely atrophied. The infrastructure is maintained through performative compliance: diversity training shows low behavioral change, committees meet but have limited budget authority, reporting creates appearance of progress without structural shifts. Theater ratio high because the visible work is disconnected from outcomes. Organized actors (internal equity staff, external consultants) maintain the infrastructure because it creates careers and justifies budget allocation.
constraint_indexing:constraint_classification(minority_representation_deficit, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGALLY MANDATED PROGRAM (SCAFFOLD) — Court-ordered or legislatively mandated representation programs (e.g., boards, civil service, contracting) function as temporary scaffolding: they force institutional change, build alternative networks, and create cohorts of minority decision-makers who then reshape institutions. Extraction is limited by legal enforcement ceiling and sunset clauses (program expires if target is met, or after specified timeframe). Theater is moderate — the program's existence is performatively visible, but enforcement is real. Generates genuine systemic change if sustained beyond initial targets. Organized actors (civil service commissions, judiciary) enforce the sunset mechanism.
constraint_indexing:constraint_classification(minority_representation_deficit, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational scope, some representation asymmetry might appear inevitable: baseline cognitive diversity costs (learning time for new cohorts), institutional inertia, and the structural reality that majority groups have historically controlled institutions. This view risks naturalizing what is actually a contingent power distribution as an inherent limit. The false summit detection flags this: representation deficit is not a natural law but an engineered institutional arrangement maintained through exclusion mechanisms.
constraint_indexing:constraint_classification(minority_representation_deficit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(minority_representation_deficit_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(minority_representation_deficit, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(minority_representation_deficit, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(minority_representation_deficit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(minority_representation_deficit, TR),
    TR >= 0.70.

:- end_tests(minority_representation_deficit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising over interval. The constraint extracts from excluded minorities (career opportunity, economic access, decision influence) and provides asymmetric benefit to majority incumbents (preserved control, uncontested authority). Theater has increased over the 10-year interval (0.48→0.64) as diversity programs multiply without proportional power shifts — the performative compliance creates appearance of progress while extraction mechanisms adapt. The rising extractiveness reflects Goodhart drift: diversity metrics (hiring numbers, board percentages) replace actual measures of power redistribution, and institutions optimize for metrics rather than outcomes. Suppression (0.68): High. Multiple reinforcing mechanisms prevent exit and mobility: (1) Structural pipeline effects (underrepresented minorities have lower access to credentialing pathways), (2) Credential gatekeeping (hiring committees can demand higher qualifications from minority candidates), (3) Network effects (majority incumbents control informal decision-making), (4) Performance scrutiny (minority hires are monitored more closely, creating psychological tax). The suppression mechanism is durable because it is distributed across institutional functions rather than centralized in explicit rules — hard to target for policy reform.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme: Snare → Rope → Tangled Rope → Piton → Scaffold → Mountain across positions. This range reveals that 'representation deficit' is not a unified phenomenon but a cluster of constraints experienced differently depending on structural position. For the excluded minority, it is a pure extraction mechanism (Snare). For the majority incumbent, it is a coordination benefit (Rope). For the mandated program, it is temporary scaffolding (Scaffold). For the diversity infrastructure, it is degraded ritual (Piton). For the analytical observer, it is naturalized as immutable (false Mountain). No single classification captures the constraint's structure across all positions — the presheaf of perspectives IS the answer. This gap is diagnostic: when a constraint produces this range of classifications, it indicates high-stakes asymmetry where one group's apparent law-of-nature is another group's extractive mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Excluded minorities (victims, trapped) derive d ≈ 0.95 → f(d) ≈ 1.42 → very high chi. Majority incumbents (beneficiaries, arbitrage) derive d ≈ 0.08 → f(d) ≈ -0.08 → negative chi (constraint benefits them). Minority professionals (mixed, constrained) derive d ≈ 0.62 → f(d) ≈ 0.95 → moderate chi. The distribution reflects asymmetric extraction: the beneficiaries experience low/negative chi (the constraint enables them), while the victims experience high chi (the constraint targets them). This asymmetry is the structural signature of Tangled Rope — the constraint both coordinates (creates benefits for majority) and extracts (targets minorities), with the asymmetry maintained through suppression and enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that representation deficit is fundamentally a Tangled Rope that is performing as Snare for excluded minorities. The diagnostic insight: when a constraint coordinates for one group (majority: diversity improves decisions, Rope perspective) but extracts from another group (minorities: excluded from participation, Snare perspective), the system cannot be classified as pure extraction (Snare, which requires extraction without coordination benefit) or pure coordination (Rope, which requires no asymmetric extraction). It is Tangled Rope by definition — active enforcement (hiring mandates, legal requirements) maintains both the coordination function (forced diversity does produce better decisions) and the extraction asymmetry (gatekeepers retain discretion, suppress informal inclusion). The mandatrophy resolves through recognizing that the constraint's function is mixed: it is NOT primarily a Snare (pure extraction) as the excluded minority experiences it, nor is it primarily a Rope (pure coordination) as the majority incumbent experiences it. It is Tangled Rope system-wide, with different agents positioned at different points in the extraction-coordination gradient. The theater ratio (0.64) indicates that the performative machinery (diversity programs, reporting, training) is substantial relative to the functional machinery (actual power redistribution). This is characteristic of Tangled Rope under enforcement: the system must appear to coordinate (hence theater) while maintaining extraction (hence suppression). If theater fell to <0.30, the system would approximate genuine Rope. If suppression rose to >0.85, the coordination function would be entirely obscured and the system would appear as Snare. Current values (theater 0.64, suppression 0.68) place it solidly in Tangled Rope territory, with enough theater to maintain legitimacy and enough suppression to maintain extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    representation_sufficiency_threshold,
    'At what minority representation percentage does the constraint transition from Snare to Rope across institutional perspectives?',
    'Longitudinal organizational studies; tracking of power structure changes, decision influence, and agent self-classification at different representation thresholds (10%, 20%, 30%, 40%)',
    'If threshold < 15%: minority agents remain structurally trapped regardless of formal representation. If threshold > 35%: representation gains may create false sense of resolution while underlying extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_sufficiency_threshold, empirical, 'Representation threshold for constraint type transition').

omega_variable(
    performance_evaluation_bias_persistence,
    'Do performance evaluation criteria for minority hired individuals systematically differ from majority incumbents, and does this persist after representation increases?',
    'Comparative analysis of evaluation rubrics, promotion timelines, termination rates, and performance ratings by demographic group over time; controlled for role and tenure',
    'If bias persists despite representation increase: constraint remains Snare for minority cohort despite numerical changes. If bias reduces: transition toward Rope is genuine rather than theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_evaluation_bias_persistence, empirical, 'Whether evaluation bias persists with representation gains').

omega_variable(
    diversity_program_budget_allocation,
    'Does budget allocation to diversity infrastructure increase or decrease relative to core institutional decision-making power when representation mandates are implemented?',
    'Tracking institutional budgets: diversity office funding vs core operations; hiring authority for diversity staff vs decision-making influence; program scope expansion or contraction',
    'If diversity budget increases but decision-making power allocation stays constant: constraint remains Tangled Rope with theatrical enforcement. If power allocation shifts: genuine structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_program_budget_allocation, empirical, 'Whether diversity programs receive substantive resource allocation').

omega_variable(
    informal_network_exclusion_mechanism,
    'Does exclusion from informal decision-making networks (senior leadership dinners, golf outings, alumni groups) persist despite formal representation, reducing effective power of minority decision-makers?',
    'Ethnographic institutional analysis; tracking of informal access patterns, decision influence outside formal meetings, mentorship relationships by demographic group',
    'If informal exclusion persists: representation is formal only (theater-heavy). If informal networks integrate: constraint transitions toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_network_exclusion_mechanism, empirical, 'Whether informal exclusion persists despite formal representation').

omega_variable(
    identity_locked_retention_mechanism,
    'For minority professionals in institutions with moderate representation, is exit constraint structural (economic dependency, credential lock) or identity-locked (internalized belief in diversity narrative, identity fusion with the breakthrough role)?',
    'Qualitative interviews with minority professionals tracking reasons for institutional commitment; exit cost analysis; post-exit trajectory analysis (rejoin same sector, switch sectors, change career)',
    'If identity-locked: minority professionals experience constraint as immutable even when exit is materially possible, perpetuating extraction. If structural: exit barriers are real economic constraints that can be policy-addressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_retention_mechanism, conceptual, 'Whether exit constraints are structural or identity-locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(minority_representation_deficit, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrd_tr_t0, minority_representation_deficit, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mrd_tr_t5, minority_representation_deficit, theater_ratio, 5, 0.58).
narrative_ontology:measurement(mrd_tr_t10, minority_representation_deficit, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(mrd_be_t0, minority_representation_deficit, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mrd_be_t5, minority_representation_deficit, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(mrd_be_t10, minority_representation_deficit, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(minority_representation_deficit, identity_coordination).
narrative_ontology:boltzmann_floor_override(minority_representation_deficit, 0.12).
narrative_ontology:affects_constraint(minority_representation_deficit, meritocratic_selection_myth).
narrative_ontology:affects_constraint(minority_representation_deficit, credential_gatekeeping_mechanism).
narrative_ontology:affects_constraint(minority_representation_deficit, informal_network_exclusion).

% DUAL FORMULATION NOTE:
% Minority representation deficit is upstream of specific institutional barriers (credential gatekeeping, network effects, pipeline effects). Each barrier has its own ε value reflecting domain-specific extraction mechanisms. The representation deficit story aggregates across barriers and focuses on the systemic suppression and coordination function. Linked stories model specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(minority_representation_deficit, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
