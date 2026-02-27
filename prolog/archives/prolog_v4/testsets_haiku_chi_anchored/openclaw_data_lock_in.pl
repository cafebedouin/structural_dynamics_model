% ============================================================================
% CONSTRAINT STORY: openclaw_data_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openclaw_data_lock_in, []).

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
 *   constraint_id: openclaw_data_lock_in
 *   human_readable: Data Lock-In by the OpenClaw AI Personal Assistant
 *   domain: technological/platform_economics
 *
 * SUMMARY:
 *   OpenClaw is a hyper-personalized AI personal assistant that creates a
 *   digital twin of its user through continuous learning from behavioral
 *   data, conversation history, and life context. The platform offers genuine
 *   coordination benefits: users receive dramatically improved assistance
 *   because the system understands their preferences, priorities, and
 *   constraints at high resolution. However, this benefit structure creates a
 *   lock-in trap: once a user has invested years of data and behavioral
 *   patterns into their digital twin, switching to a competing assistant
 *   incurs catastrophic exit costs — all context is forfeited, and the
 *   competing system must be retrained from zero. The constraint exemplifies
 *   how coordination mechanisms can degrade into pure extraction when
 *   switching costs become prohibitive. Initially (interval T=0),
 *   extractiveness was moderate (0.35) and theater_ratio was low (0.28) — the
 *   system genuinely delivered personalization value. Over 6 years (T=6),
 *   extractiveness increased to 0.68 and theater_ratio to 0.58 as: (1)
 *   lock-in mechanisms became explicit (users realized they couldn't easily
 *   leave), (2) the operator began layering additional services and premium
 *   tiers that captured increasing fractions of user value, and (3) the
 *   performative aspects of personalization (customized UI, recommendation
 *   theater) increased relative to actual capability gains. The suppression
 *   metric (0.72) reflects that users face high barriers to exit: no
 *   competing assistant has equivalent context; data export is technically
 *   difficult; the retraining cost on a new platform is months to years of
 *   degraded service. Mandatrophy is resolved by the Tangled Rope perspective
 *   (regulators): the constraint has genuine coordination benefits
 *   (centralized AI safety, standardized user modeling) but also genuine
 *   extraction (market lock-in, data asymmetry). The policy response is to
 *   mandate interoperability and data portability while preserving the
 *   coordination gains — a Scaffold structure with a regulatory sunset.
 *
 * KEY AGENTS:
 *   - OpenClaw Users: Primary victims (powerless/trapped) — bear full switching cost; locked into the system despite desire for alternatives or privacy controls
 *   - OpenClaw Operator: Primary beneficiary (institutional/arbitrage) — captures user data value; monetizes lock-in through premium tiers and market dominance
 *   - Competing AI Assistants: Secondary victims (moderate/constrained) — cannot access OpenClaw users' context; market competition suppressed by informational asymmetry
 *   - Regulatory Authorities: Institutional actor (moderate/mobile) — see both coordination benefits (centralized safety oversight) and extraction (market monopolization); pursuing data portability mandates
 *   - User Data Autonomy (Abstract): Victim (powerless/trapped) — fundamental right to data control is systematically suppressed by lock-in mechanism
 *   - Early Adopter Community: Transitional (powerful→trapped) — experienced genuine personalization benefit initially; now locked-in despite atrophied relative value
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openclaw_data_lock_in, 0.68).
domain_priors:suppression_score(openclaw_data_lock_in, 0.72).
domain_priors:theater_ratio(openclaw_data_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openclaw_data_lock_in, extractiveness, 0.68).
narrative_ontology:constraint_metric(openclaw_data_lock_in, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(openclaw_data_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openclaw_data_lock_in, snare).
narrative_ontology:human_readable(openclaw_data_lock_in, "Data Lock-In by the OpenClaw AI Personal Assistant").
narrative_ontology:topic_domain(openclaw_data_lock_in, "technological/platform_economics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openclaw_data_lock_in, openclaw_operator).
narrative_ontology:constraint_victim(openclaw_data_lock_in, openclaw_users).
narrative_ontology:constraint_victim(openclaw_data_lock_in, competitive_alternatives).
narrative_ontology:constraint_victim(openclaw_data_lock_in, user_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTURED USER (SNARE) — User has invested years of behavioral data, conversation history, and life context into their digital twin. Exit cost is total: all personalization is forfeited; alternative assistants have zero context. The user experiences the constraint as a lock-in trap with no functional exit. Trapped exit + victim status → d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(openclaw_data_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPENCLAW OPERATOR (ROPE) — Operator sees data aggregation as a coordination mechanism: centralizing user context enables genuinely better assistance. The operator has full arbitrage options (can launch in new markets, license the platform, merge/acquire). Beneficiary + arbitrage exit → d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. Negative effective extraction = net beneficiary. Operator experiences the constraint as enabling, not extractive.
constraint_indexing:constraint_classification(openclaw_data_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETING ASSISTANTS (SNARE) — Competitors cannot access the deep context of OpenClaw users. Each defection requires user to re-train competing assistant on months or years of context. Market competition is suppressed by informational asymmetry. Victim + constrained exit → d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.57.
constraint_indexing:constraint_classification(openclaw_data_lock_in, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SNARE) — From a civilizational/universal view, user data autonomy is a fundamental human right. The constraint extracts this autonomy through behavioral lock-in. The observer sees suppressed alternatives (interoperable assistants, user-controlled data, competitive market) as structural victims of the lock-in mechanism. d≈0.90, f(d)≈1.35, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(openclaw_data_lock_in, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (TANGLED ROPE) — Regulators want both coordination (AI safety, standardization) and guard against extraction (market concentration, data abuses). The constraint has genuine coordination benefits (centralized safety oversight, interoperable compliance) but also extraction (market monopolization, privacy risks). Ambiguous beneficiary/victim status + mobile exit → d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(openclaw_data_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: EARLY ADOPTER COMMUNITY (PITON) — Early users experienced OpenClaw as genuinely empowering (coordination function: personalization that worked). But as the lock-in mechanism crystallized and switching costs became visible, the constraint's functional benefit atrophied while the lock-in persisted through inertia. Users remain trapped not because the service is excellent but because the exit cost is prohibitive. Theater ratio ≥ 0.70 satisfied by the shift from functional benefit to locked-in persistence. d≈0.80, f(d)≈1.05, but theater_ratio=0.72 triggers piton classification.
constraint_indexing:constraint_classification(openclaw_data_lock_in, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openclaw_data_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openclaw_data_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openclaw_data_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openclaw_data_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openclaw_data_lock_in, TR),
    TR >= 0.70.

:- end_tests(openclaw_data_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The operator captures increasing fractions of user value through behavioral data monetization, premium service tiers, and market dominance enabled by lock-in. The value extraction accelerated from T=0 (0.35) to T=6 (0.68) as switching costs became explicit and the operator layered additional services. The score reflects that lock-in enables systematic value capture that would be impossible in a competitive market. Suppression (0.72): High. Users face multiple layers of suppression: technical barriers to data export, informational asymmetry (no competing assistant understands them as well), retraining costs (months of degraded service on a new platform), and social/convenience costs (OpenClaw integration into daily life). Exit is technically possible but behaviorally, economically, and informationally suppressed. Theater ratio (0.58): Moderate-high. The personalization service includes genuine capabilities but increasingly includes performative elements: customized UI that adds no functional value, recommendation framing that emphasizes the operator's choices, premium tier marketing that emphasizes status rather than capability. The theater ratio increased over the interval as the operator shifted from providing genuine capability gains to marketing its own lock-in mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a sharp perspectival divide between the operator (Rope: coordination enabler) and the user (Snare: lock-in trap). The same data aggregation mechanism appears as beneficial coordination from the operator's view (centralized safety, better personalization) and as extractive lock-in from the user's view (lost autonomy, trapped exit). The regulatory perspective (Tangled Rope) mediates: regulators see genuine coordination benefits worth preserving but also genuine extraction (market monopolization) that requires intervention. The competing assistants perspective (Snare) reveals that the operator's coordination benefit is achieved by suppressing market competition — the system coordinates user assistance at the cost of suppressing alternative providers. The analytical observer (Snare) reveals that user data autonomy is systematically violated by the lock-in mechanism. The early adopter community (Piton) shows how the constraint degrades from functional benefit to institutional inertia: users remain because exit is prohibitive, not because OpenClaw is superior.
 *
 * DIRECTIONALITY LOGIC:
 *   OpenClaw users: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — users cannot exit without catastrophic cost. OpenClaw operator: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary; operator experiences constraint as enabling. Competing assistants: Victim + constrained → d≈0.85, f(d)≈1.15. Significant extraction; competitors face high barriers to entry due to information asymmetry. Regulators: Ambiguous (see coordination benefits but also extraction) + mobile → d≈0.50, f(d)≈0.65. Moderate effective extraction; regulators have agency and exit options (policy intervention, interop mandates). Early adopter community: Victim + mobile → d≈0.75, f(d)≈1.10. Constrained extraction; early adopters have some agency (they could theoretically switch or pressure OpenClaw) but face high switching costs. Analytical observer: Victim (of data autonomy violation) + analytical → d≈0.90, f(d)≈1.35. High extraction from a civilizational data rights perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED via Regulatory Tangled Rope: The constraint exhibits genuine coordination benefits (personalized AI assistance, centralized safety oversight, efficient user modeling) AND genuine asymmetric extraction (lock-in, market monopolization, data autonomy violation). The policy response (EU DMA data portability mandates, GDPR interoperability requirements) is to preserve coordination while removing extraction: mandate that users can export their digital twin and competitors can import it, maintaining personalization benefits without lock-in. This is a Scaffold structure: temporary enforcement of interoperability standards with a sunset as market competition re-establishes alternative assistants. The mandatrophy is resolved by showing that the constraint is NOT pure extraction (Snare only) and NOT pure coordination (Rope only) — it is a hybrid where coordination has been weaponized into lock-in. Regulatory intervention converts Snare→Scaffold by removing suppression while preserving coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_portability_technical_feasibility,
    'Is lossless export and import of a user''s digital twin data technically feasible across competing assistant platforms?',
    'Development and testing of standardized data interchange formats; measurement of information loss in context transfer; cross-platform assistant performance benchmarks using ported data',
    'If technically feasible: lock-in is policy-dependent (Rope + regulation). If infeasible: lock-in is structural (Snare). If partially feasible: Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_technical_feasibility, empirical, 'Whether digital twin portability is technically achievable').

omega_variable(
    user_preference_lock_in_vs_optimization,
    'What fraction of user retention is due to genuine preference for OpenClaw''s superior capabilities versus switching-cost-induced lock-in?',
    'Cohort analysis: comparison of user churn when competitors reach functional parity; user survey data on explicit switching-cost perception; A/B testing of data export frictionlessness',
    'If <20% lock-in: constraint classifies as Rope (coordination with strong beneficiary). If >60% lock-in: pure Snare. If 20-60%: Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_preference_lock_in_vs_optimization, empirical, 'Ratio of optimization benefit to lock-in cost in user retention').

omega_variable(
    regulatory_mandated_interoperability_feasibility,
    'Can regulatory mandates for data interoperability be enforced without collapsing the operational benefits of centralized assistant platforms?',
    'Pilot programs in EU/UK under DMA/GDPR; measurement of assistant quality degradation with mandatory data portability; cost-benefit analysis of interop infrastructure',
    'If feasible with minimal cost: Scaffold (regulatory sunset to lock-in). If costly or destructive: Tangled Rope confirmed (tradeoff between benefits and harms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mandated_interoperability_feasibility, empirical, 'Whether regulatory interoperability mandates can preserve platform benefits').

omega_variable(
    emergent_collective_action,
    'Will user coalitions emerge to demand data portability, or does isolated lock-in prevent collective organizing?',
    'Monitoring of user forums, advocacy groups; measurement of coalition size and political effectiveness; tracking of competing pressures (convenience vs autonomy)',
    'If strong coalition emerges: victim group upgrades from powerless to organized, potentially changing snare to tangled_rope. If coalition fails: snare persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emergent_collective_action, empirical, 'Whether users can organize to resist lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openclaw_data_lock_in, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(openclaw_tr_t0, openclaw_data_lock_in, theater_ratio, 0, 0.28).
narrative_ontology:measurement(openclaw_tr_t3, openclaw_data_lock_in, theater_ratio, 3, 0.45).
narrative_ontology:measurement(openclaw_tr_t6, openclaw_data_lock_in, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(openclaw_be_t0, openclaw_data_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(openclaw_be_t3, openclaw_data_lock_in, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(openclaw_be_t6, openclaw_data_lock_in, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openclaw_data_lock_in, information_standard).
narrative_ontology:affects_constraint(openclaw_data_lock_in, ai_model_convergence_race).
narrative_ontology:affects_constraint(openclaw_data_lock_in, data_locality_regulation).
narrative_ontology:affects_constraint(openclaw_data_lock_in, platform_algorithmic_coupling).

% DUAL FORMULATION NOTE:
% The lock-in mechanism decomposes into two related constraints: (1) information asymmetry lock-in (ε≈0.68, this story), where users are trapped by superior context in OpenClaw vs competitors, and (2) network effects lock-in (ε≈0.55, separate constraint), where OpenClaw's value increases as more users join, creating a coordination-based barrier that depends on user beliefs about future dominance. These are linked by network.affects_constraints because information asymmetry enables network effects and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openclaw_data_lock_in, analytical, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
