% ============================================================================
% CONSTRAINT STORY: web_app_friction_tax
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_web_app_friction_tax, []).

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
 *   constraint_id: web_app_friction_tax
 *   human_readable: Web App Friction Tax: Usability Extraction via Intentional Friction
 *   domain: digital_economics/ui_ux_design
 *
 * SUMMARY:
 *   Web app friction tax describes the systematic extraction of user time,
 *   attention, and productivity through intentionally introduced friction
 *   mechanisms in digital interfaces. Platforms implement confirmation
 *   dialogs, nested navigation menus, modal interruptions, notification
 *   batching, auto-refocus behaviors, and dark patterns that slow user action
 *   completion while extracting behavioral data and inflating engagement
 *   metrics. The constraint operates across multiple institutional layers:
 *   individual users experience trapped extraction; moderate users experience
 *   coordination mixed with extraction; platform operators experience pure
 *   coordination; regulators experience a temporary extractive problem with
 *   sunset via DMA/GDPR enforcement; legacy enterprise software maintains
 *   friction through institutional inertia; and the analytical observer risks
 *   naturalizing contingent institutional choices as immutable design laws.
 *   The rising trajectory in measurements reflects the evolution of friction
 *   mechanisms: early platforms had modest friction (legitimate security and
 *   data integrity requirements); current platforms have engineered friction
 *   to maximize engagement metric inflation and behavioral data collection
 *   (0.58 extractiveness); regulatory pressure is forcing awareness and
 *   measurement of friction costs (pushing toward coordinated reduction).
 *   Theater ratio increase (0.52→0.68) indicates that friction has become
 *   increasingly performative: user-facing friction mechanisms are maintained
 *   despite alternatives that achieve the same security/integrity outcomes
 *   with lower cost.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — bear time tax, attention extraction, productivity loss, behavioral data leakage with no exit option except platform abandonment
 *   - Platform Operators (Google, Meta, Microsoft, Apple): Primary beneficiaries (institutional/arbitrage) — extract engagement metrics, behavioral data, and extended time-on-site for advertising; have full agency to reduce or eliminate friction
 *   - Advertising Networks: Secondary beneficiary (institutional/arbitrage) — depend on behavioral data extraction enabled by friction-driven engagement and time capture
 *   - Productivity-Conscious Users: Moderate victims (moderate/constrained) — experience friction as both service coordination (email delivery, file storage) and extraction (time tax, attention tax); can migrate at cost
 *   - User Protection Coalition: Organized actors (organized/constrained) — regulators (EU DMA), privacy advocates, open-source projects, accessibility standards bodies; see friction reduction as buildable through regulation and technical standards
 *   - Enterprise Software Vendors: Institutional actors (institutional/arbitrage) — maintain friction through lock-in and institutional inertia (Piton perspective); friction serves legacy approval-workflow purposes now mostly bypassed by users
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing friction as inherent to UX design rather than identifying it as a contingent extraction choice by platform operators
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(web_app_friction_tax, 0.58).
domain_priors:suppression_score(web_app_friction_tax, 0.62).
domain_priors:theater_ratio(web_app_friction_tax, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(web_app_friction_tax, extractiveness, 0.58).
narrative_ontology:constraint_metric(web_app_friction_tax, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(web_app_friction_tax, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(web_app_friction_tax, tangled_rope).
narrative_ontology:human_readable(web_app_friction_tax, "Web App Friction Tax: Usability Extraction via Intentional Friction").
narrative_ontology:topic_domain(web_app_friction_tax, "digital_economics/ui_ux_design").

domain_priors:requires_active_enforcement(web_app_friction_tax).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(web_app_friction_tax, platform_operator).
narrative_ontology:constraint_beneficiary(web_app_friction_tax, advertising_network).
narrative_ontology:constraint_victim(web_app_friction_tax, end_user_time).
narrative_ontology:constraint_victim(web_app_friction_tax, user_attention_commons).
narrative_ontology:constraint_victim(web_app_friction_tax, productivity_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED USER (SNARE) — End users cannot exit friction mechanisms without abandoning platform functionality entirely. Friction is embedded in the service delivery layer itself. Exit requires switching platforms (high switching cost due to network effects, data portability barriers, habit formation). The user bears full extraction cost: time tax, attention tax, productivity loss, behavioral data leakage. Zero degrees of freedom within the platform.
constraint_indexing:constraint_classification(web_app_friction_tax, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRODUCTIVITY-CONSCIOUS USER (TANGLED ROPE) — Users with moderate power (domain expertise, switching capacity, time to configure workarounds) experience both coordination and extraction. The platform does provide genuine service coordination: email delivery, file storage, collaborative editing. But friction extracts time and behavioral data on top of the coordination function. Exit is constrained but possible — migration costs are real but not impossible for technically sophisticated users.
constraint_indexing:constraint_classification(web_app_friction_tax, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences friction as a pure coordination mechanism: slowing user action completion forces users to remain in-app longer, generates engagement metrics that justify advertising rates, and captures behavioral data used for targeting. The operator has arbitrage options (can implement low-friction UX, can monetize through subscription instead, can pivot business model). Friction appears as a coordination function because it aligns incentives between the platform's revenue model and engagement metrics.
constraint_indexing:constraint_classification(web_app_friction_tax, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: USER PROTECTION COALITION (SCAFFOLD) — Organized actors (regulators, privacy advocates, open-source alternatives, accessibility standards bodies) see friction as a temporary extractive mechanism with a sunset: regulatory frameworks (DMA, GDPR, accessibility law), alternative platforms (open-source web apps, federated services), and technical standards (browser-level friction indicators, dark-pattern databases) are creating friction-reducing pathways. The sunset is enforced via regulation and technical standards proliferation. Effective extraction for this perspective is low because the coalition has agency and sees exit paths.
constraint_indexing:constraint_classification(web_app_friction_tax, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ENTERPRISE SOFTWARE ECOSYSTEM (PITON) — Legacy enterprise platforms (Salesforce, SAP, Oracle) maintain friction-based UX (complex navigation, modal confirmations, nested approval workflows) through institutional inertia. The original function — enforcing approval discipline and preventing accidental data mutation — has atrophied as users learned to work around friction through template scripts and third-party automation. The friction ritual persists despite low functional value, maintained because switching costs are high and the platforms have lock-in through customization and integration. Theater ratio is high because the formal UX friction does not prevent sophisticated users from bypassing it.
constraint_indexing:constraint_classification(web_app_friction_tax, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, friction is an immutable property of user interface design: all UX involves tradeoffs between functionality and complexity, and some friction is inherent to preventing accidental actions or maintaining security boundaries. This perspective sees the friction as a natural law of digital systems. However, the base properties contradict this — the suppression value (0.62) and beneficiary declarations reveal contingent institutional choices (modal confirmations CAN be streamlined, nested menus CAN be flattened, notifications CAN be batched by user preference), not structural necessity. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(web_app_friction_tax, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(web_app_friction_tax_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(web_app_friction_tax, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(web_app_friction_tax, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(web_app_friction_tax, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(web_app_friction_tax, TR),
    TR >= 0.70.

:- end_tests(web_app_friction_tax_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Friction mechanisms extract user time (minutes per day aggregate to hours per year), attention (cognitive switching costs, interruption tax), behavioral data (every click sequence, dwell time, hesitation patterns), and productivity (workflows optimized around low-friction UX are impossible on friction-heavy platforms). The value is not maximal (0.72+) because some friction is genuinely functional—authentication confirmations prevent account compromise, deletion confirmations prevent accidental data loss—but the functional friction is ~10-15% of total friction observed. The 0.58 value reflects that ~50-60% of implemented friction is extractive overhead above legitimate security/integrity requirements. Suppression (0.62): Moderate-high. Users face multiple suppression mechanisms: network effects create switching costs (data portability is technically hard, social graph is platform-specific); habit formation and muscle memory reduce perceived alternatives; mobile app distribution creates App Store gatekeeping; data exports are incomplete or cumbersome. These barriers are not absolute (users CAN switch platforms) but require significant cost. Theater ratio (0.68): Moderately high. Friction is partially performative: many implemented confirmations, nested menus, and modal dialogs do not actually prevent errors or breaches—users learn to click through them mindlessly (theater). Some friction serves real functions (password confirmation, two-factor auth), but much is theater designed to inflate engagement metrics rather than provide safety. Rising theater ratio over the interval reflects increasing sophistication of friction engineering: platforms have learned to design friction that *feels* functional (appears to prevent mistakes) while actually serving engagement extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the trapped user's snare classification and the platform operator's rope classification is maximal and diagnostic. The user experiences pure extraction: friction slows them down with no benefit. The operator experiences coordination: friction keeps users engaged, generates data for targeting, inflates time-on-site metrics that justify advertising rates. No objective fact about the friction mechanism itself drives this classification gap — the gap is entirely structural (power level, exit options, beneficiary/victim relationship). The user's snare is the operator's rope. The regulatory/coalition perspective (scaffold) sits between them, seeing the friction as a temporary extractive mechanism that can be solved through DMA-enforcement of user-control requirements and dark-pattern bans. The piton perspective reveals institutional inertia: enterprise software maintains friction-heavy workflows (approval chains, confirmation rituals) even as users have learned workarounds that render the friction performative. The mountain perspective risks naturalizing the friction as inherent to digital systems — a mistake that would block regulatory intervention.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position relative to the extraction flow. Trapped users with no exit and victim status: d ≈ 0.95 (near-total target). Constrained moderate users with partial exit and mixed benefit/harm: d ≈ 0.55 (mostly target, some beneficiary effects). Institutional operators with arbitrage exit and beneficiary status: d ≈ 0.15 (beneficiary with maximum agency). The sigmoid f(d) transforms these into effective extraction multipliers. The trapped user experiences the full extraction intensity because f(0.95) ≈ 1.42. The operator experiences negative effective extraction (benefits from the same friction the user is harmed by) because f(0.15) ≈ -0.01. The institutional victim (the user attention commons, the productivity commons) has no agent to express its experience, but if modeled as trapped + victim: d ≈ 0.98, f(d) ≈ 1.40, maximum experienced extraction. The scope modifier σ(S) = 1.2 for global scope increases effective extractiveness χ beyond base ε, reflecting that friction-tax extraction scales with platform ubiquity — as friction mechanisms spread globally, verification difficulty rises and extraction becomes harder to detect or regulate.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolves through perspectival pluralism. The snare classification (trapped user) is structurally real: users cannot exit friction without abandoning service. The rope classification (platform operator) is equally real: friction does coordinate user engagement with revenue capture. Both are correct from their respective positions. The tangled_rope classification (moderate user) is correct: they experience both coordination (service delivery) and extraction (time/attention tax) simultaneously. The scaffold classification (regulatory coalition) is correct: friction is a temporary extractive mechanism subject to sunset via regulation. The piton classification (enterprise software) is correct: friction persists through inertia despite low functional value. The mountain classification (natural law view) is the only incorrect one — it naturalizes a contingent institutional choice as immutable. The engine's false summit detector identifies this misclassification by noting that beneficiaries (platform operators) exist for this constraint, contradicting the natural-law profile. Resolution: the constraint is a tangled_rope with perspectival variance, not a mountain. The variance across perspectives is not uncertainty about which type is correct; it is structural reality of how different agents relate to the same extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    friction_necessity_threshold,
    'What friction level is genuinely required to prevent accidental data loss or security breaches versus what is extracted as time tax?',
    'Comparative UX analysis: A/B testing with reduced friction implementations; error/security-incident rates at different friction levels; user error correction behavior with streamlined vs friction-heavy interfaces',
    'If threshold is low (5-10% of current friction): most friction is extractive (snare), regulatory intervention justified. If threshold is high (70%+ of current friction): friction is largely functional (rope), extraction concern misplaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(friction_necessity_threshold, empirical, 'Necessity threshold for friction in digital UX').

omega_variable(
    behavioral_data_extraction_value,
    'What proportion of platform friction extraction value derives from behavioral data collection versus engagement metric inflation?',
    'Platform revenue attribution analysis: ad targeting revenue per behavioral data point; user-lifetime-value correlation with time-on-site metrics; platform A/B tests varying friction and data collection independently',
    'If behavioral data > 60% of extraction value: the constraint is fundamentally about surveillance capitalism (snare from powerless perspective). If engagement metrics > 60%: extraction is about advertising inventory (tangled rope, coordination function exists alongside extraction). Classification implications for both base-properties and perspective gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_data_extraction_value, empirical, 'Data extraction vs engagement metric value split').

omega_variable(
    regulatory_friction_ceiling,
    'Will DMA accessibility requirements, GDPR user-control mandates, and emerging dark-pattern regulations successfully enforce low-friction design as a public good?',
    'Regulatory compliance monitoring: friction metrics measured pre- and post-regulation; platform investment in friction reduction; enforcement severity and penalty efficacy; emergence of friction-reducing browser/client-side tools',
    'If regulations succeed (friction ceiling enforced): scaffold sunset is real, constraint degrades to rope within 5-10 years. If regulations fail or are circumvented: friction extraction remains snare for trapped users indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_friction_ceiling, preference, 'Whether regulations can enforce friction reduction as public good').

omega_variable(
    false_summit_natural_law,
    'Is friction in user interface design a natural immutable law or a contingent institutional choice by platform operators?',
    'Cross-platform friction comparison: open-source alternatives, federated services, subscription-model platforms with low friction; historical friction evolution in platforms; user preference studies when friction is optional',
    'If natural law: mountain classification is correct, regulation cannot and should not mandate low friction. If contingent: false summit triggers, constraint reclassifies as tangled_rope or snare depending on exit options, regulation is structurally justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether friction is inherent to UX or contingent extraction choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(web_app_friction_tax, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waft_tr_t0, web_app_friction_tax, theater_ratio, 0, 0.52).
narrative_ontology:measurement(waft_tr_t3, web_app_friction_tax, theater_ratio, 3, 0.6).
narrative_ontology:measurement(waft_tr_t6, web_app_friction_tax, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(waft_be_t0, web_app_friction_tax, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(waft_be_t3, web_app_friction_tax, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(waft_be_t6, web_app_friction_tax, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(waft_su_t0, web_app_friction_tax, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(waft_su_t3, web_app_friction_tax, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(waft_su_t6, web_app_friction_tax, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(web_app_friction_tax, resource_allocation).
narrative_ontology:affects_constraint(web_app_friction_tax, dark_pattern_engagement_lock).
narrative_ontology:affects_constraint(web_app_friction_tax, behavioral_data_extraction_infrastructure).
narrative_ontology:affects_constraint(web_app_friction_tax, advertising_metric_inflation).

% DUAL FORMULATION NOTE:
% Web app friction tax decomposes into three constraint family members: dark_pattern_engagement_lock (behavioral lock-in mechanisms, ε≈0.65); behavioral_data_extraction_infrastructure (surveillance mechanisms enabled by friction-driven dwell time, ε≈0.55); advertising_metric_inflation (engagement metric gaming, ε≈0.50). Each has its own extractiveness value reflecting distinct mechanisms. The friction tax is upstream and affects all three — reducing friction reduces engagement dwell time, which reduces behavioral data collection opportunities and lowers advertising metric value. Link them via affects_constraints to model the cascade.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(web_app_friction_tax, institutional, 0.15).
constraint_indexing:directionality_override(web_app_friction_tax, powerful, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
