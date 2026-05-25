% ============================================================================
% CONSTRAINT STORY: algorithmic_addiction_capture_u16
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_addiction_capture_u16, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: algorithmic_addiction_capture_u16
 *   human_readable: Algorithmic Addiction Capture of Under-16 Users
 *   domain: digital_social_media/behavioral_manipulation
 *
 * SUMMARY:
 *   Algorithmic addiction capture of users under 16 represents a structural
 *   extraction mechanism that operates through behavioral conditioning,
 *   psychological vulnerability exploitation, and systematic circumvention of
 *   parental oversight. The constraint exhibits identity-locking, where the
 *   target user's self-concept and social identity become fused with platform
 *   participation, making exit require not just leaving an app but abandoning
 *   the identity constructed within it. Platform operators benefit from
 *   attention harvesting and behavioral data collection; advertisers benefit
 *   from psychographically targeted audiences; users aged 16 and under bear
 *   the cost through cognitive capture, attention fragmentation, and
 *   developmental interference. The constraint's suppression is high (0.72)
 *   due to the combination of network effects (peer presence is on the
 *   platform), technical design choices that maximize engagement loops, and
 *   parental powerlessness (guardians lack effective monitoring tools and
 *   face relationship costs from restriction). The theater ratio (0.55)
 *   reflects that some child protection measures are genuine (notification
 *   limits, data transparency) while others are performative (age
 *   verification rituals that are trivially circumvented). The extractiveness
 *   trajectory shows accumulation over a 10-year interval as algorithmic
 *   sophistication increased and competitive engagement pressures escalated
 *   platform addiction mechanics. This is a diagnostic exemplar for
 *   identity-locking at scale and for how cognitive capture can constitute a
 *   snare even when structural exit barriers are technically surmountable.
 *
 * KEY AGENTS:
 *   - Under-16 Users: Primary victims (powerless/identity_locked) — identity and social status constituted through platform participation; withdrawal requires identity reconstruction
 *   - Parents/Guardians: Secondary victims (moderate/constrained) — face relationship costs and social isolation risks if they restrict access; parental authority systematically undermined
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture attention surplus, behavioral data, and advertising premiums; experience constraint as coordination problem solved efficiently
 *   - Digital Advertisers: Secondary beneficiaries (institutional/arbitrage) — benefit from psychographic targeting and behavioral prediction enabled by user data harvesting
 *   - Tech-Literate Parents: Tertiary actors (powerful/mobile) — can afford alternatives and monitoring tools; experience mixed coordination-extraction (tangled rope)
 *   - Regulatory/Advocacy Coalition: Organizing agents (organized/constrained) — building regulatory framework with sunset logic (EU DSA, age verification mandates); constraining extraction through transparency and design restrictions
 *   - Age-Verification Theater: Institutional ritual (institutional/arbitrage) — performative measures persisting through regulatory box-ticking despite minimal functional enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_addiction_capture_u16, 0.68).
domain_priors:suppression_score(algorithmic_addiction_capture_u16, 0.72).
domain_priors:theater_ratio(algorithmic_addiction_capture_u16, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_addiction_capture_u16, extractiveness, 0.68).
narrative_ontology:constraint_metric(algorithmic_addiction_capture_u16, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(algorithmic_addiction_capture_u16, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_addiction_capture_u16, snare).
narrative_ontology:human_readable(algorithmic_addiction_capture_u16, "Algorithmic Addiction Capture of Under-16 Users").
narrative_ontology:topic_domain(algorithmic_addiction_capture_u16, "digital_social_media/behavioral_manipulation").

domain_priors:requires_active_enforcement(algorithmic_addiction_capture_u16).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_addiction_capture_u16, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_addiction_capture_u16, digital_advertisers).
narrative_ontology:constraint_victim(algorithmic_addiction_capture_u16, under_16_users).
narrative_ontology:constraint_victim(algorithmic_addiction_capture_u16, child_cognitive_development).
narrative_ontology:constraint_victim(algorithmic_addiction_capture_u16, parental_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPTURED ADOLESCENT (SNARE) — The user is structurally mobile (could uninstall apps, could ask parents to restrict access) but identity-locked: peer identity, social status, self-worth are constituted through platform engagement. Withdrawal would require abandoning the identity they constructed within the app ecosystem. The binding is cognitive/identity rather than material, but the trap is real. Maximum experienced extraction — the platform harvests attention, behavioral data, and psychological vulnerabilities.
constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PARENT/GUARDIAN (SNARE) — Faces high-cost exit: restricting access damages social integration and parent-child relationship, creates peer isolation, triggers psychological resistance. The parent is not trapped but severely constrained. Also victim of the extraction — parental authority and child development oversight are systematically undermined. The constraint's suppression mechanism includes platform design that obscures addiction mechanics and regulatory arbitrage (platforms operate in jurisdictions with minimal child protection oversight).
constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Experience the constraint as pure coordination: engagement optimization enables personalized content distribution, creates network effects, and solves the matching problem between users and content/advertisers. Net beneficiary through arbitrage options (can migrate user data across jurisdictions, can shift business models). Extraction runs toward this agent — they capture attention surplus and behavioral data.
constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECH-LITERATE PARENTS / RESEARCHERS (TANGLED ROPE) — Powerful agents (income, education, social capital) can afford monitoring tools, private schools, alternative platforms, or enforce real restrictions. Experience mixed coordination (the platforms do enable legitimate social connection) and extraction (advertising targeting, data harvesting). Moderate extraction because they have agency and alternatives — some exit cost but surmountable. This perspective is smaller in population than perspective 2.
constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY/ADVOCACY COALITION (SCAFFOLD) — Organized actors (pediatric associations, child protection NGOs, jurisdictions like EU with GDPR/Digital Services Act) are building regulatory frameworks with sunset logic: age verification requirements, algorithmic transparency mandates, notification limits on under-16 accounts. These are coordination mechanisms with temporary enforcement — the coalition sees algorithmic addiction capture as a solvable problem with defined regulatory endpoints (the extraction mechanism loses force once transparency and design restrictions are mandatory). High suppression currently but declining under regulatory pressure.
constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AGE-VERIFICATION THEATER (PITON) — Many platforms have implemented age-verification rituals (asking birthdate on signup) that are performative — the verification mechanisms are trivial to circumvent and the enforcement is minimal. These rituals persist through institutional inertia (regulatory box-ticking) despite low functional effectiveness. Theater ratio reflects that stated child protection measures often lack real enforcement or technical robustness.
constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER WITH IDENTITY LOCK (SNARE) — The analyst examining this constraint may themselves be identity-locked in platform ecosystems (professional reputation, social presence, media channels require platform participation). The analytical position is compromised by institutional dependence. This perspective instantiates the oracle gap: the analyst's native instruments cannot detect the structure that cross-position analysis reveals because the analyst is caught in the same extraction mechanism. Acknowledging this identity lock is part of honest analytical positioning.
constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_addiction_capture_u16_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_addiction_capture_u16, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_addiction_capture_u16, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_addiction_capture_u16, TR),
    TR >= 0.70.

:- end_tests(algorithmic_addiction_capture_u16_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximum. Platform operators capture substantial attention surplus and behavioral data, with demonstrated impacts on sleep, mental health, and social development. The extraction is severe but not as total as a labor camp (where extractiveness approaches 0.95) — users retain some agency (albeit limited), genuine social coordination benefits exist alongside extraction, and regulatory pressure is reducing the floor. The trajectory shows acceleration from 0.35 to 0.68 over the interval as algorithmic optimization and competitive engagement pressures increased. Suppression (0.72): High. Suppression operates through multiple mechanisms: (1) network effects make exit socially costly; (2) algorithmic design creates physiological reward dependencies; (3) parental monitoring tools are inadequate or unavailable; (4) regulatory arbitrage allows platforms to operate with minimal enforcement in most jurisdictions; (5) the user population (under 16) has limited cognitive capacity for impulse control and addiction resistance. Suppression is both structural (technical, financial, social barriers) and internalized (users believe the platform is necessary, inevitable, superior to alternatives). Theater ratio (0.55): Moderate. Child protection mechanisms include genuine features (notification limits, data transparency in high-enforcement jurisdictions) alongside performative rituals (age verification that is trivially circumvented, privacy policies with no enforcement teeth). The ratio is lower than pure pitons because some regulatory pressure is producing real behavioral change in platforms, even if partial or incomplete.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. The platform operator sees rope (coordination: efficiently matching users to content and advertisers). The under-16 user sees snare (maximum extraction, identity-locked, no perceived exit). The parent sees snare (victim of undermined authority and child development interference). The tech-literate parent sees tangled rope (genuine social benefits + data extraction, but manageable through alternatives). The regulatory coalition sees scaffold (temporary problem being solved through regulatory framework with sunset). The age-verification theater sees piton (performative ritual persisting through institutional inertia). The analytical observer risks seeing a false mountain ('algorithmic engagement is just how social media works, inevitable and natural') — but the structural data reveals this as naturalization of design choices and regulatory arbitrage. The largest perspectival gap is between platform operators (rope) and under-16 users (snare) — a bifurcation driven by directionality: operators experience negative or minimal extraction (they are beneficiaries); users experience maximum extraction (they are targets). This gap is the hallmark of a snare: the beneficiary's experience diverges sharply from the victim's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators are beneficiaries with arbitrage options: they can shift business models, migrate jurisdictions, or adjust algorithmic parameters. Their structural position produces low directionality (d ≈ 0.15), yielding low or negative effective extraction from their perspective — they experience the constraint as coordination. Under-16 users are victims with identity-locked exit: they are psychologically mobile (could theoretically leave) but cognitively trapped (cannot perceive or execute exit without identity dissolution). Their structural position produces high directionality (d ≈ 0.88), yielding high f(d) and high experienced extraction. Parents face constrained exit (high-cost but surmountable) and victim status (parental authority undermined), producing moderate-high directionality (d ≈ 0.75). The regulatory coalition has organized power and constrained exit options, producing moderate directionality (d ≈ 0.58). No override is needed — the structural derivation chain captures the perspectival reality.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY (extractiveness > 0.70): This constraint is classified as SNARE, and mandatrophy is resolved by confirming that the classification is stable across the measured interval and that the extraction is NOT best understood as degraded coordination. The key evidence: (1) Beneficiaries and victims are clearly differentiated — operators extract, users bear costs with no coordination reciprocity; (2) The suppression mechanism is structural and intentional (algorithmic design choices, regulatory arbitrage) not accidental degradation of a formerly pure coordination mechanism; (3) The constraint's function is to enable extraction, not to coordinate. A false mandatrophy would arise if we tried to reframe this as 'algorithmic engagement is just coordination that has gotten a bit extractive over time' — but the evidence shows design intentionality and competitive pressure toward maximizing extraction, not drift toward extraction. The constraint is fundamentally extractive, not coordinatively degraded. Mandatrophy is confirmed as resolved: this is a snare, not a piton pretending to be a rope. The union of perspectives (powerless/identity_locked/snare, moderate/constrained/snare, powerful/mobile/tangled_rope, organized/constrained/scaffold, institutional/arbitrage/rope, analytical/analytical/mountain-false-summit) produces a heterogeneous presheaf that correctly identifies the snare at the powerless position and the false mountain at the analytical position — the structure is intact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained,
    'Is the under-16 user''s binding mechanism primarily identity-locked (cognitive/relational) or constrained (material/economic barriers to exit)?',
    'Longitudinal study: measure platform disengagement outcomes post-deletion; distinguish between those reporting identity-reconstructed elsewhere vs. those facing genuine social reintegration barriers. Interview data on self-concept dependence.',
    'If identity-locked: the constraint is a snare that requires identity frame disruption to break, not just access restriction. If constrained: high-cost but material barriers — regulatory restriction of access becomes more effective. Therapeutic/identity intervention vs. technical restriction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained, empirical, 'Whether user binding is identity-locked or materially constrained').

omega_variable(
    algorithmic_irreversibility,
    'Are the behavioral reinforcement patterns created by algorithmic optimization irreversible within the biographical timescale, or can user preferences/dopamine response be reset post-exit?',
    'Neuroscience and behavioral studies: measure dopamine/reward system recovery post-platform-exit; compare recovery trajectories between voluntary exit vs. forced restriction; assess persistent preference changes in high-engagement former users.',
    'If irreversible: the constraint''s suppression is structural (the user cannot recover normal reward baseline even after exit). If reversible: suppression is maintained through ongoing engagement — restriction becomes more effective. Informs whether post-exit intervention is needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_irreversibility, empirical, 'Whether algorithmic conditioning is irreversible post-exit').

omega_variable(
    network_effects_as_exit_barrier,
    'Do genuine network effects (peer presence requiring platform participation) constitute insurmountable exit barriers, or are they surmountable through alternative coordination mechanisms?',
    'Comparative analysis: adoption of alternative platforms (Discord, BeReal, Signal) among age cohorts with platform-exit attempts; measurement of social integration outcomes for non-users vs. users in the same geographic/demographic cohort.',
    'If insurmountable: exit is genuinely trapped at the social level — not just identity-locked but structurally embedded. If surmountable: regulatory/coordinated alternatives can shift network effects toward less extractive platforms. Affects whether the snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effects_as_exit_barrier, empirical, 'Whether network effects create insurmountable exit barriers').

omega_variable(
    parental_capacity_distribution,
    'What percentage of the under-16 population has parents/guardians with the cognitive/social/economic capacity to effectively monitor or restrict algorithmic engagement?',
    'Population survey: measure parental digital literacy, available monitoring time, income capacity for alternative services (private education, screening tools). Correlation with child engagement metrics.',
    'If < 30%: most under-16 users lack effective parental friction — the constraint is a snare for the majority. If > 70%: significant stratification — rich/educated children face tangled_rope, poor/unsupervised face snare. Affects classification uniformity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(parental_capacity_distribution, empirical, 'Distribution of parental capacity to restrict algorithmic engagement').

omega_variable(
    regulatory_arbitrage_persistence,
    'Can platforms maintain extractive algorithmic engagement mechanisms despite regulatory restrictions by operating in low-enforcement jurisdictions or using technical obfuscation?',
    'Regulatory tracking: compare platform behavior across high-enforcement (EU-GDPR, UK-Online Safety Bill) vs. low-enforcement jurisdictions. Measure effectiveness of regulatory controls in changing algorithmic behavior vs. compliance theater.',
    'If platforms maintain arbitrage: the scaffold perspective is aspirational, not structural — regulation creates theater without reducing extraction. If arbitrage fails: the sunset is real — extraction mechanisms lose force as regulatory floor rises globally. Critical for mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_persistence, empirical, 'Whether platforms can maintain extractive mechanisms through regulatory arbitrage').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.72) primarily structural (technical/financial barriers to alternatives) or internalized (users internalize platform framing as inevitable/necessary)?',
    'Qualitative analysis: user and parent interviews on perceived alternatives and necessity. Post-exit interviews on whether suppression persisted in their self-perception. A/B testing alternative platform designs with equivalent engagement mechanics vs. transparent engagement limits.',
    'If structural: removing barriers becomes the primary intervention. If internalized: users carry suppression with them post-exit — requires identity/cognitive intervention. Likely both — measure the ratio to calibrate post-exit support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Proportion of suppression that is structural vs. internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_addiction_capture_u16, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algadd_tr_t0, algorithmic_addiction_capture_u16, theater_ratio, 0, 0.38).
narrative_ontology:measurement(algadd_tr_t3, algorithmic_addiction_capture_u16, theater_ratio, 3, 0.48).
narrative_ontology:measurement(algadd_tr_t6, algorithmic_addiction_capture_u16, theater_ratio, 6, 0.54).
narrative_ontology:measurement(algadd_tr_t10, algorithmic_addiction_capture_u16, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(algadd_be_t0, algorithmic_addiction_capture_u16, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algadd_be_t3, algorithmic_addiction_capture_u16, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(algadd_be_t6, algorithmic_addiction_capture_u16, base_extractiveness, 6, 0.64).
narrative_ontology:measurement(algadd_be_t10, algorithmic_addiction_capture_u16, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_addiction_capture_u16, identity_coordination).
narrative_ontology:boltzmann_floor_override(algorithmic_addiction_capture_u16, 0.12).
narrative_ontology:affects_constraint(algorithmic_addiction_capture_u16, adolescent_mental_health_degradation).
narrative_ontology:affects_constraint(algorithmic_addiction_capture_u16, regulatory_arbitrage_digital_platforms).
narrative_ontology:affects_constraint(algorithmic_addiction_capture_u16, parental_authority_erosion).

% DUAL FORMULATION NOTE:
% Algorithmic addiction capture is a single constraint with network effects on mental health outcomes, regulatory vulnerability, and parental capacity. It does not decompose by observable — extractiveness is stable across measurement methodologies. The constraint family links three downstream constraints that share the extraction flow: mental health degradation is the immediate harm to the user; regulatory arbitrage is the mechanism maintaining suppression; parental authority erosion is the secondary harm to the oversight system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
