% ============================================================================
% CONSTRAINT STORY: youth_mental_health_surveillance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_youth_mental_health_surveillance, []).

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
 *   constraint_id: youth_mental_health_surveillance
 *   human_readable: Youth Mental Health Surveillance Systems
 *   domain: social/health/technology
 *
 * SUMMARY:
 *   Youth mental health surveillance systems create a structural tension
 *   between the institutional goal of early risk identification and the
 *   developmental need for youth autonomy, peer trust, and privacy. Schools,
 *   health systems, and tech platforms have implemented increasingly
 *   comprehensive monitoring through questionnaires, behavioral tracking,
 *   app-based mental health assessment, and algorithmic flagging. The
 *   constraint exhibits a mixed coordination-extraction dynamic: genuine
 *   mental health coordination function (identifying at-risk youth, enabling
 *   peer support training, consolidating fragmented health data) coexists
 *   with significant extraction mechanisms (behavioral conformity pressure,
 *   data collection and repurposing, suppression of peer-to-peer support
 *   networks, psychological overhead of being monitored). The surveillance
 *   apparatus is presented as scientifically necessary (actuarial risk
 *   assessment) and legally mandatory (duty of care, mandated reporting), but
 *   the empirical validity of risk prediction tools is weak and
 *   false-positive rates are high. The constraint's theater ratio (0.55)
 *   reflects that while coordination function is genuine, a substantial
 *   portion of the surveillance machinery is performative — documenting risk
 *   rather than meaningfully preventing harm. The extractiveness trajectory
 *   (0.35 → 0.58 over 6 years) shows accumulation as platforms expand data
 *   retention, institutions tighten reporting requirements, and algorithms
 *   become more intrusive.
 *
 * KEY AGENTS:
 *   - Surveilled Youth: Primary victim (powerless/trapped) — cannot exit surveillance; experience behavioral conformity pressure, psychological overhead, loss of developmentally necessary autonomy
 *   - Peer Trust Networks: Secondary victim (moderate/constrained) — benefit from collective mental health awareness but face chilling effect on honest peer support due to reporting obligations
 *   - Mental Health Provider System: Primary beneficiary (institutional/arbitrage) — gains data consolidation, early identification capabilities, treatment coordination
 *   - School Institution: Mixed actor (institutional/constrained) — provides genuine mental health support but also extracts behavioral control and liability documentation
 *   - Data Aggregation Platform: Secondary beneficiary (powerful/arbitrage) — consolidates youth psychological data; can extract value through profiling, algorithmic targeting, behavioral manipulation
 *   - Youth Privacy Movement: Organized alternative (organized/mobile) — building peer-led support systems, privacy-respecting architectures; represents exit pathway
 *   - Clinical Risk Assessment Ritual: Institutional performance (institutional/arbitrage) — maintains actuarial risk assessment tools; increasingly divorced from predictive utility; persists through liability theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent features of responsible caregiving
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(youth_mental_health_surveillance, 0.58).
domain_priors:suppression_score(youth_mental_health_surveillance, 0.68).
domain_priors:theater_ratio(youth_mental_health_surveillance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(youth_mental_health_surveillance, extractiveness, 0.58).
narrative_ontology:constraint_metric(youth_mental_health_surveillance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(youth_mental_health_surveillance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(youth_mental_health_surveillance, tangled_rope).
narrative_ontology:human_readable(youth_mental_health_surveillance, "Youth Mental Health Surveillance Systems").
narrative_ontology:topic_domain(youth_mental_health_surveillance, "social/health/technology").

domain_priors:requires_active_enforcement(youth_mental_health_surveillance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(youth_mental_health_surveillance, mental_health_providers).
narrative_ontology:constraint_beneficiary(youth_mental_health_surveillance, institutional_monitors).
narrative_ontology:constraint_beneficiary(youth_mental_health_surveillance, data_aggregators).
narrative_ontology:constraint_victim(youth_mental_health_surveillance, surveilled_youth).
narrative_ontology:constraint_victim(youth_mental_health_surveillance, peer_trust_networks).
narrative_ontology:constraint_victim(youth_mental_health_surveillance, developmental_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED YOUTH (SNARE) — Minors cannot legally or practically exit surveillance systems embedded in schools, apps, and family monitoring. Trapped by age, legal status, and geographic dependence on caregivers. Maximum extraction: behavioral conformity extraction (self-censorship), psychological overhead (knowing they are watched), and data extraction (psychological profiles sold or retained). No meaningful exit option.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PEER TRUST NETWORK (TANGLED ROPE) — Peers benefit from genuine coordination function (detecting serious suicide risk, substance abuse networks) but bear extraction costs (loss of privacy in friendships, social control through reporting obligations, chilling effect on peer support). Constrained exit: can partially opt out by avoiding institutional channels but cannot escape social pressure to report concerning behavior. Mixed coordination-extraction dynamic.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MENTAL HEALTH PROVIDER SYSTEM (ROPE) — Genuine coordination benefit: surveillance systems enable early identification of at-risk youth, data consolidation improves diagnostic accuracy, treatment coordination across schools and clinics. Providers experience the constraint as net-beneficial coordination. Arbitrage exit: can switch between surveillance platforms, adapt protocols, or disengage from specific systems if alternatives emerge. Net beneficiary.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SCHOOL INSTITUTION (TANGLED ROPE) — Schools coordinate genuine mental health support (early intervention, peer counseling training) but also extract administrative control, liability management (documenting that risk was identified), and student behavioral conformity. Constrained exit: schools cannot fully exit without legal/liability exposure; they must maintain some surveillance to meet duty-of-care obligations. Active enforcement of reporting requirements. Mixed function.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: YOUTH PRIVACY MOVEMENT (SCAFFOLD) — Organized agents (privacy advocates, digital rights organizations, youth networks) see surveillance as a temporary coordination failure being solved by alternative architectures: peer-led mental health support without institutional reporting, encrypted messaging, federated data systems with individual control. Mobile exit: these agents can build competing systems that reduce surveillance extraction. Sunset logic: privacy-respecting alternatives (e.g., consent-based data access, individual data ownership, peer-led crisis networks) create pathway to diminish the extractive constraint. High suppression tolerance acceptable because sunset is real.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CLINICAL RISK ASSESSMENT RITUAL (PITON) — The actuarial risk assessment tools (PHQ-9 screening, Columbia-Suicide Severity Rating Scale, algorithmic flagging) have largely become performative: they document that risk was assessed (liability theater) rather than meaningfully predicting which youth will attempt suicide. Predictive validity is weak; false-positive rates are high. The surveillance ritual persists through institutional inertia and liability fear, not because it works. Theater ratio high: the assessment machinery is maintained despite degraded function.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DATA AGGREGATION PLATFORM (TANGLED ROPE) — Tech platforms coordinating mental health data consolidation (school systems, therapy apps, health records) enjoy genuine coordination benefits but also extract significant value: behavioral data for algorithmic profiling, psychological profiles sold to advertisers/educators, surveillance moats that lock in institutions. Arbitrage exit: platforms can switch data aggregators, negotiate terms, or migrate to alternatives. High extractiveness but with agency — net beneficiary position.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, the framing naturalizes surveillance as inherent to responsible caregiving: 'parents have always monitored youth behavior; surveillance is just the modern form.' The constraint appears immutable — unavoidable cost of duty of care in a dangerous world. However, the structural data contradicts mountain classification. The engine identifies this as a false summit: the naturalization disguises contingent institutional arrangements (liability law, therapeutic models centered on risk, digital infrastructure) as inevitable features of how care works.
constraint_indexing:constraint_classification(youth_mental_health_surveillance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(youth_mental_health_surveillance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(youth_mental_health_surveillance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(youth_mental_health_surveillance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(youth_mental_health_surveillance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(youth_mental_health_surveillance, TR),
    TR >= 0.70.

:- end_tests(youth_mental_health_surveillance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The surveillance apparatus extracts behavioral conformity (youth self-censor to avoid detection), psychological overhead (cost of knowing one is watched), and valuable data (psychological profiles, behavioral patterns, vulnerability indicators). However, this is not maximal extraction because mental health providers genuinely coordinate improved outcomes, and some youth benefit from early intervention. The extractiveness trajectory rises from 0.35 to 0.58 as data aggregation platforms expand beyond clinical use and institutional scope creep occurs (behavioral control through mental health data). Suppression (0.68): High. Significant barriers to exit include legal age requirements, mandatory reporting obligations, school-embedded surveillance (unavoidable for students), app-based monitoring that family members control, and the framing of surveillance as 'care.' Youth cannot organize collective resistance without facing escalated intervention. Suppression operates through both structural barriers (legal/institutional) and internalization (youth internalize surveillance as protective, become identity_locked). Theater ratio (0.55): Moderate-high. Clinical risk assessment tools (PHQ-9, Columbia Scale, algorithmic flagging) are the performative core. These tools have weak predictive validity for suicide attempts (<10% sensitivity for actual attempts), generate high false-positive rates, and function primarily to document that risk was assessed (institutional liability theater) rather than to meaningfully prevent harm. However, the broader coordination function (peer mental health literacy, early access to treatment, data consolidation) is genuine, not purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the surveilled youth's Snare perception and the mental health provider's Rope perception is maximal. The youth see continuous extraction and no exit; the provider sees coordination benefit and multiple institutional alternatives. This gap reveals that the constraint is experienced as different phenomena depending on structural position: for the provider, it is a beneficial coordination tool; for the youth, it is a control mechanism. The school's Tangled Rope perception splits the difference — genuine coordination function (mental health support) yoked to extraction (behavior management, liability documentation). The youth privacy movement's Scaffold perception introduces an exit pathway that other perspectives cannot see — peer-led architectures that bypass institutional surveillance entirely. This perspectival family (Snare→Tangled Rope→Rope→Scaffold) shows the constraint's lifecycle: it begins as pure extraction (snare for early surveillance deployments), mixes with coordination as mental health functions integrate (tangled rope), appears beneficial to providers (rope), and faces organized alternatives seeking to sunset it (scaffold).
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality from beneficiary/victim declarations and exit options. Surveilled youth are declared victims with trapped exit, producing d ≈ 0.95 (full target). Mental health providers are declared beneficiaries with arbitrage exit, producing d ≈ 0.15 (near-full beneficiary). The peer network is both — they report peers (beneficiary position relative to risk management) but lose privacy in friendships (victim position relative to peer trust). This dual declaration produces d ≈ 0.60. Schools are both — they enforce surveillance (beneficiary) but are constrained by liability law (victim), producing d ≈ 0.50. The data platform is a beneficiary (aggregation value) with arbitrage exit (can switch platforms), producing d ≈ 0.25. These d values, multiplied by f(d) from the sigmoid and scaled by spatial scope σ(S=national=1.0), yield the effective extractiveness χ each agent perceives. The youth experience high χ (d ≈ 0.95 → f(d) ≈ 1.42 → χ ≈ 0.82 after scope scaling), confirming their Snare perception. Providers experience low χ (d ≈ 0.15 → f(d) ≈ -0.01 → χ ≈ -0.01), confirming their Rope perception. No directionality overrides are needed; the structural derivation produces perspectival divergence naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification at moderate agent power (peer networks, schools) and powerful agent power (data platforms) resolves the mandatrophy by acknowledging genuine coordination alongside genuine extraction. The constraint cannot be dismissed as pure coordination (Rope) because clear victims (youth, peer trust networks) face suppression and extraction. It cannot be labeled pure extraction (Snare) because mental health providers and peers genuinely benefit from early risk identification and support coordination. The classification shows that hybrid mechanisms — where beneficiaries organize surveillance that targets victims but genuinely prevents some harms — are structurally possible and empirically common in health and safety domains. The Scaffold perspective (organized privacy movements with exit pathways) prevents the classification from becoming a tragic inevitability: technological and architectural alternatives (peer-led support, privacy-preserving data systems, consent-based monitoring) represent genuine sunset possibilities if institutional will materializes. The Piton classification of the risk assessment ritual warns against assuming that surveillance machinery that feels legitimate (clinical tools, assessment protocols) is actually functional. The false summit (mountain from the analytical perspective) prevents naturalization of contingent institutional arrangements as immutable laws of care.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_harm_prevention_efficacy,
    'Do youth mental health surveillance systems actually prevent suicide attempts and self-harm, or do they primarily document risk while generating false positives and iatrogenic harms?',
    'RCT or natural experiment comparing surveilled vs unsurveilled cohorts on suicide attempt/self-harm rates; adjustment for confounders (access to treatment, socioeconomic status). Analysis of false-positive rates and downstream consequences of unnecessary intervention.',
    'If efficacy > 15% reduction in attempts: Rope/Scaffold from health perspectives gains validation. If efficacy < 5% or shows negative effects: Snare/Piton classification strengthened; constraints shift toward pure extraction. If effects conditional (works for high-risk, harms low-risk): decompose into separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_harm_prevention_efficacy, empirical, 'Whether surveillance prevents harm or primarily generates false positives').

omega_variable(
    peer_support_chilling_effect,
    'Does knowledge of institutional surveillance reduce peer-to-peer mental health support (friends talking friends through crises) by creating reporting obligations that chill trust?',
    'Longitudinal surveys of youth trust in peer relationships before/after surveillance implementation; analysis of peer support help-seeking behavior changes; qualitative interviews on how surveillance affects friendship disclosure.',
    'If chilling effect > 25%: Snare perspective strengthened; extraction mechanism is destruction of informal support networks. If chilling effect < 5%: Rope perspective gains traction; coordination benefits outweigh suppression. If heterogeneous (some youth increase disclosure, some decrease): identity_locked mechanism; some youth internalize surveillance as protective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peer_support_chilling_effect, empirical, 'Whether surveillance reduces informal peer mental health support').

omega_variable(
    developmental_autonomy_cost,
    'What is the long-term impact of continuous surveillance on youth developmental autonomy — the capacity to make decisions, take risks, and form independent identity?',
    'Longitudinal comparison of psychological autonomy, locus of control, and identity development in surveilled vs unsurveilled cohorts into adulthood. Analysis of decision-making patterns, risk assessment behavior, and vulnerability to anxiety/obsessive behaviors.',
    'If autonomy costs are severe (persistent external locus of control, decision paralysis, hypervigilance to perceived risks): Snare classification strengthened; extraction includes psychological development. If negligible: Rope classification gains validation. If heterogeneous by identity-locked group (some internalize surveillance as protective, some as oppressive): requires decomposition and identity_locked exit analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_autonomy_cost, empirical, 'Long-term impact of surveillance on developmental autonomy').

omega_variable(
    alternative_support_architecture_viability,
    'Do peer-led mental health models (crisis text lines, peer support groups, encrypted peer networks) achieve comparable early-detection and support outcomes without institutional surveillance?',
    'Comparative analysis of outcomes (access rates, help-seeking behavior, emotional support quality, crisis prevention) between surveillance-based vs peer-led systems. Barrier analysis: what prevents peer-led models from scaling? Investment/policy constraints vs structural limitations?',
    'If peer-led models achieve parity: Scaffold perspective confirmed; sunset is technically feasible. If peer-led underperform: constraint may be necessary. If peer-led perform better on some metrics (trust, autonomy) but worse on others (early detection): decompose into multiple constraints per domain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_support_architecture_viability, empirical, 'Whether peer-led support models provide comparable mental health outcomes').

omega_variable(
    data_retention_purpose_creep,
    'Are youth mental health surveillance data retained primarily for clinical purposes or are they increasingly used for behavioral control, academic/employment screening, or risk-profiling beyond the original clinical scope?',
    'Data access audit: track how mental health surveillance data are used across time; identify cases of scope expansion (clinical data used for discipline, academic placement, college admission screening). Policy analysis of data governance changes.',
    'If scope creep is documented: Snare/Tangled Rope perspectives strengthened; extraction mechanism expands from health coordination to behavioral control. If data retention is strictly clinical: Rope/Scaffold perspectives gain credibility. If purpose creep is accelerating: urgency of sunset mechanisms increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_retention_purpose_creep, empirical, 'Whether youth mental health surveillance data are repurposed beyond clinical use').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(youth_mental_health_surveillance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ymhs_tr_t0, youth_mental_health_surveillance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ymhs_tr_t3, youth_mental_health_surveillance, theater_ratio, 3, 0.48).
narrative_ontology:measurement(ymhs_tr_t6, youth_mental_health_surveillance, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(ymhs_be_t0, youth_mental_health_surveillance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ymhs_be_t3, youth_mental_health_surveillance, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(ymhs_be_t6, youth_mental_health_surveillance, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(youth_mental_health_surveillance, attachment_coordination).
narrative_ontology:boltzmann_floor_override(youth_mental_health_surveillance, 0.12).
narrative_ontology:affects_constraint(youth_mental_health_surveillance, algorithmic_mental_health_profiling).
narrative_ontology:affects_constraint(youth_mental_health_surveillance, school_discipline_surveillance).
narrative_ontology:affects_constraint(youth_mental_health_surveillance, family_monitoring_tech).

% DUAL FORMULATION NOTE:
% Youth mental health surveillance decomposes into three distinct constraints along functional lines: (1) school-based screening and reporting (ε≈0.45, local scope, focuses on identification), (2) therapeutic app data collection and algorithmic risk profiling (ε≈0.62, global scope, focuses on commercial extraction), (3) family monitoring technology (ε≈0.52, household scope, focuses on parental control). This constraint story represents the aggregate system. See network.affects_constraints for decomposed stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(youth_mental_health_surveillance, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
