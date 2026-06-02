% ============================================================================
% CONSTRAINT STORY: institutional_epistemic_authority_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_epistemic_authority_concentration, []).

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
 *   constraint_id: institutional_epistemic_authority_concentration
 *   human_readable: Institutional Epistemic Authority Concentration
 *   domain: epistemology/institutional_governance/knowledge_production
 *
 * SUMMARY:
 *   Institutional epistemic authority concentration describes the structural
 *   constraint that concentrates the power to define what counts as
 *   legitimate knowledge within credentialed institutions (universities,
 *   research centers, peer-reviewed journals, professional bodies). This
 *   constraint exhibits genuine coordination function — institutions
 *   standardize methodologies, enable specialization, and create
 *   infrastructure for verification — but this coordination is wrapped in
 *   asymmetric extraction: non-institutional knowledge producers,
 *   marginalized communities, and alternative epistemologies are excluded
 *   from legitimacy regardless of actual knowledge quality. The constraint
 *   has degraded over the past 40 years as institutional gatekeeping has
 *   shifted from quality assurance toward metric optimization (theater rise
 *   from 0.38 to 0.68), while extraction has simultaneously increased (from
 *   0.32 to 0.58) as credentialing costs have risen and institutional
 *   dependence has deepened. The constraint is neither a natural law nor pure
 *   coordination — it is a tangled rope that solves real coordination
 *   problems while enabling real extraction.
 *
 * KEY AGENTS:
 *   - Credentialed Institutions: Primary beneficiary (institutional/arbitrage) — capture epistemic authority, funding, and prestige; experience the constraint as pure coordination
 *   - Institutional Gatekeepers: Primary beneficiary (institutional/arbitrage) — journal editors, tenure committees, accreditation bodies; maintain monopoly on legitimacy definitions
 *   - Marginalized Knowledge Producers: Primary victim (powerless/trapped) — practitioners, traditional knowledge holders, non-Western epistemologies; cannot exit institutional authority regime
 *   - Epistemically Marginalized Groups: Secondary victim (powerless/trapped) — communities whose knowledge systems are excluded; tacit, experiential, or non-standardized knowledge is delegitimized
 *   - Alternative Knowledge Systems: Secondary victim (powerless/trapped) — indigenous epistemologies, craft practices, community-based research; structured out of institutional legitimacy
 *   - Junior Researchers: Tertiary victim (moderate/constrained) — early-career scholars dependent on institutional gatekeeping for career viability; bear tuition and publishing costs
 *   - Global South Institutions: Tertiary victim (institutional/constrained or identity_locked) — lower-tier universities in economically disadvantaged regions; credentialing dependent on Global North institutional standards
 *   - Open Knowledge Coalition: Organized agent (organized/mobile) — open-access movements, citizen science, Wikipedia, ResearchGate; building alternative epistemic authority structures
 *   - Academic Prestige System: Institutional degradation (institutional/arbitrage) — ranking apparatus maintains legitimacy theater while quality-assurance function atrophies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional concentration as inherent to knowledge production
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_epistemic_authority_concentration, 0.58).
domain_priors:suppression_score(institutional_epistemic_authority_concentration, 0.62).
domain_priors:theater_ratio(institutional_epistemic_authority_concentration, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_epistemic_authority_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_epistemic_authority_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(institutional_epistemic_authority_concentration, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_epistemic_authority_concentration, tangled_rope).
narrative_ontology:human_readable(institutional_epistemic_authority_concentration, "Institutional Epistemic Authority Concentration").
narrative_ontology:topic_domain(institutional_epistemic_authority_concentration, "epistemology/institutional_governance/knowledge_production").

domain_priors:requires_active_enforcement(institutional_epistemic_authority_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_epistemic_authority_concentration, credentialed_institutions).
narrative_ontology:constraint_beneficiary(institutional_epistemic_authority_concentration, institutional_gatekeepers).
narrative_ontology:constraint_victim(institutional_epistemic_authority_concentration, non_institutional_knowledge_producers).
narrative_ontology:constraint_victim(institutional_epistemic_authority_concentration, epistemically_marginalized_groups).
narrative_ontology:constraint_victim(institutional_epistemic_authority_concentration, alternative_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED KNOWLEDGE PRODUCER (SNARE) — Cannot exit the institutional authority regime. Tacit knowledge, community-based expertise, or non-Western epistemic traditions are dismissed as 'unverified' without access to credentialing channels. No alternative pathway to institutional legitimacy exists. Maximum extraction with no meaningful coordination benefit.
constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JUNIOR RESEARCHER (TANGLED ROPE) — Constrained by credentialing costs and institutional gatekeeping but benefits from the verification infrastructure and professional networks institutions provide. Significant asymmetric extraction (tuition, publication fees, citation dependence) alongside genuine coordination function (methodology standardization, peer review, funding allocation).
constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALED INSTITUTION (ROPE) — Benefits from monopoly on epistemic authority. Experiences the constraint as pure coordination: standardizing which knowledge counts solves collective action problems (curriculum design, hiring, funding allocation). Net beneficiary with arbitrage exit options.
constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN KNOWLEDGE COALITION (SCAFFOLD) — Organized agents (open-access movements, citizen science initiatives, peer-to-peer learning platforms, Wikipedia, ResearchGate) are building alternative epistemic authority structures with lower gatekeeping costs. Sees institutional concentration as a temporary coordination failure with sunset potential through distributed verification and decentralized credentialing.
constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC PRESTIGE SYSTEM (PITON) — The ranking and credentialing apparatus (journal impact factors, h-indices, university rankings) performs legitimacy theater while its original function (quality assurance) has degraded. Persist through institutional inertia despite widespread recognition that metrics poorly correlate with actual knowledge quality or societal value. Theater-driven maintenance with atrophied coordination function.
constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MARGINALIZED INSTITUTION (TANGLED ROPE) — Lower-tier universities, institutions in Global South, historically Black colleges coordinate knowledge through the same frameworks as elite institutions (peer review, journal publication, degree granting) but capture minimal epistemic authority. Both benefit from and are victimized by institutional standardization. Constrained by dependence on elite institution metrics for legitimacy.
constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some institutional authority concentration is inherent to knowledge production: complex claims require standardized verification, specialization requires credentialing, and coordination requires hierarchical validation. This perspective risks naturalizing what is actually a contingent institutional arrangement maintained by specific incentive structures and historical path-dependence.
constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_epistemic_authority_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_epistemic_authority_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_epistemic_authority_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_epistemic_authority_concentration, TR),
    TR >= 0.70.

:- end_tests(institutional_epistemic_authority_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Institutions genuinely solve coordination problems (standardizing curriculum, enabling specialization, funding research) but extract significantly through credentialing barriers (tuition, publishing fees, citation dependence). The extraction is not total — some coordination value is real. Extractiveness increased from 0.32 to 0.58 over 40 years as credentialing costs rose and institutional dependence deepened, and as the competitive prestige market encouraged cost-shifting to scholars. Suppression (0.62): Moderate-high. Significant barriers to knowledge legitimacy include credentialing requirements (degrees, institutional affiliation), publication gatekeeping (peer review, journal access), and epistemic framing (what counts as 'valid' knowledge is institutionally defined). But suppression is not total — some non-institutional knowledge does circulate, alternative systems are emerging, and institutional legitimacy is contestable. Theater ratio (0.68): High. Institutional metrics (h-index, journal impact factor, university rankings) have progressively substituted for actual quality verification. The prestige apparatus performs legitimacy theater: metrics are tracked obsessively while actual knowledge validity is poorly correlated with metrics. Theater increased from 0.38 to 0.68 over 40 years as quantification displaced qualitative judgment.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the credentialed institution's perception (Rope: coordination, no meaningful extraction) and the marginalized producer's perception (Snare: pure extraction, no coordination benefit) reflects a real structural difference: the institution controls the verification infrastructure and can make exit decisions (arbitrage), while the marginalized producer has no alternative pathway to legitimacy (trapped). The same institutional framework that coordinates knowledge standardization for insiders extracts epistemic authority from outsiders. The scaffold perspective (organized agents building alternatives) and piton perspective (prestige system degrading into theater) both predict that institutional concentration will decline over time, but through different mechanisms: the scaffold projects replacement through decentralization; the piton projects replacement through cumulative delegitimization. If both happen simultaneously, institutional epistemic authority could collapse faster than alternatives mature, creating a transitory knowledge-legitimacy crisis.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) encodes each agent's structural relationship to the extraction flow. Credentialed institutions as beneficiaries with arbitrage exit options derive d ≈ 0.05 (negative chi — they experience the constraint as enabling rather than extractive). Junior researchers as victims with constrained exit derive d ≈ 0.80 (high chi — significant extraction experienced). Marginalized producers as victims with trapped exit derive d ≈ 0.95 (maximum chi — complete extraction). Global South institutions as both victims (excluded from authority) and beneficiaries (use institutional frameworks to organize) derive d ≈ 0.55 (moderate chi). The open knowledge coalition as organized victims with mobile exit derive d ≈ 0.45 (moderate chi — can build alternatives but must compete with entrenched institutions). These directionality values explain why beneficiaries and trapped victims classify so differently: the same constraint is an enabling coordination mechanism for those with institutional power and an extractive cage for those without it. The directionality derivation shows that what institutions perceive as 'necessary standards' are vectors of extraction for those outside institutional boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that institutional epistemology serves dual functions simultaneously: genuine coordination (methodology standardization, verification infrastructure, specialization enabling) and systematic extraction (credentialing barriers, knowledge gatekeeping, prestige concentration). The mandatrophy is not 'is this a coordination or extraction mechanism?' but 'to whom does the coordination apply and from whom is extraction extracted?' For institutional insiders, the constraint is predominantly rope (coordination with net benefit). For outsiders, it is predominantly snare (extraction with no coordination benefit). For partially-integrated actors (junior researchers, Global South institutions), it is tangled rope (genuine coordination mixed with asymmetric extraction). The perspectivist resolution is not a collapse to one true type but an acknowledgment that the constraint IS coordination-for-some and extraction-from-others simultaneously. This is the definitive tangled rope: it is not that coordinative and extractive aspects are hard to separate, but that they are functionally inseparable — the coordination mechanism IS the extraction mechanism. Standardizing epistemology necessarily excludes non-standard knowledge. There is no way to have institutional verification without institutional gatekeeping. The mandatrophy resolves by showing that the constraint cannot be reformed toward pure coordination without losing the extraction, and cannot retain the coordination without accepting the extraction. This is the political-economic core of institutional epistemology: it is not a bug, it is the feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_necessity_threshold,
    'What proportion of epistemic authority concentration is genuinely necessary for verification vs. what is institutional gatekeeping rent-seeking?',
    'Comparative analysis of verification outcomes across credentialed vs non-credentialed knowledge systems; measurement of false positive rates, correction velocities, and knowledge quality metrics independent of credentialing source',
    'If threshold < 0.30: most institutional concentration is extractive (snare from more perspectives). If threshold > 0.70: institutional concentration is largely necessary (rope from more perspectives). The true value is likely in the 0.40-0.55 range, supporting tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_necessity_threshold, empirical, 'Proportion of epistemic authority concentration that is functionally necessary vs extractive').

omega_variable(
    alternative_credentialing_viability,
    'Can decentralized credentialing systems (peer networks, open review, reputation accrual outside institutions) achieve equivalent quality assurance at significantly lower extraction cost?',
    'Longitudinal tracking of open-access vs journal-published research quality, citation impact, error rates, correction frequency, and societal impact over 10+ year horizons; measurement of knowledge accessibility and utilization across populations with/without institutional access',
    'If viable: scaffold perspective is structural reality (sunset is real). If not viable: open knowledge coalition is aspirational (alternative systems degraded, tantamount to piton). Most likely: partial viability for some knowledge domains (computational, theoretical) but not others (clinical, engineering).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_viability, empirical, 'Whether decentralized credentialing can replace institutional gatekeeping').

omega_variable(
    epistemic_internalization_mechanism,
    'To what extent do marginalized knowledge producers internalize the institution''s frame that their knowledge is illegitimate without credentialing, vs maintain independent epistemic authority?',
    'Ethnographic study of knowledge producers outside institutional systems; measurement of identity-locked vs constrained vs trapped exit patterns; analysis of whether marginalized producers see exit as impossible (trapped) or unthinkable (identity_locked) or costly (constrained)',
    'If primarily identity_locked: the constraint''s power is cognitive (internalized authority frame), not purely structural. Remediation requires identity-frame shift, not just institutional reform. If primarily trapped/constrained: the constraint''s power is material (access barriers, funding dependencies), and structural reform addresses it directly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_internalization_mechanism, empirical, 'Whether marginalized epistemic authority is internalized or materially constrained').

omega_variable(
    global_south_institutional_dependence,
    'Are Global South institutions genuinely constrained by Global North credentialing standards (constrained exit), or has credentialing dependence become an identity-fused characteristic of institutional legitimacy (identity_locked exit)?',
    'Analysis of institutional sovereignty movements and de-linking initiatives; measurement of educational outcomes and societal knowledge utility when institutions prioritize local epistemic frameworks vs Global North alignment',
    'If constrained: material barriers (funding, publishing infrastructure, language, resource access) drive dependence; policy reform can change exit costs. If identity_locked: institutions'' self-concepts are constituted through Global North alignment; exit requires institutional identity dissolution, not just policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_institutional_dependence, empirical, 'Whether Global South institutional dependence is material or identity-based').

omega_variable(
    theater_metric_substitution,
    'Has institutional epistemology progressively substituted measurable indicators (publications, citations, h-indices) for actual knowledge quality, reversing the original purpose (verification) into proxy optimization?',
    'Historical correlation analysis between prestige metrics and actual knowledge validity, societal utility, correction rates, and reproducibility over 30+ year horizon; measurement of gaming behaviors (citation circles, salami publishing, metric manipulation) as proportion of institutional activity',
    'If confirmed: theater_ratio should increase over time, piton classification becomes stronger (institutional system is degraded). The ''natural law'' mountain perspective becomes more clearly a false summit — institutional concentration was originally functional (rope) and is now largely theatrical (piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_metric_substitution, empirical, 'Whether institutional epistemology has substituted metrics for actual quality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_epistemic_authority_concentration, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ieac_tr_t0, institutional_epistemic_authority_concentration, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ieac_tr_t20, institutional_epistemic_authority_concentration, theater_ratio, 20, 0.52).
narrative_ontology:measurement(ieac_tr_t40, institutional_epistemic_authority_concentration, theater_ratio, 40, 0.68).
narrative_ontology:measurement(ieac_tr_t10, institutional_epistemic_authority_concentration, theater_ratio, 10, 0.45).
narrative_ontology:measurement(ieac_tr_t30, institutional_epistemic_authority_concentration, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(ieac_be_t0, institutional_epistemic_authority_concentration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ieac_be_t20, institutional_epistemic_authority_concentration, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(ieac_be_t40, institutional_epistemic_authority_concentration, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(ieac_be_t10, institutional_epistemic_authority_concentration, base_extractiveness, 10, 0.39).
narrative_ontology:measurement(ieac_be_t30, institutional_epistemic_authority_concentration, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_epistemic_authority_concentration, information_standard).
narrative_ontology:boltzmann_floor_override(institutional_epistemic_authority_concentration, 0.12).
narrative_ontology:affects_constraint(institutional_epistemic_authority_concentration, peer_review_gatekeeping).
narrative_ontology:affects_constraint(institutional_epistemic_authority_concentration, citation_dependency_lock).
narrative_ontology:affects_constraint(institutional_epistemic_authority_concentration, global_south_knowledge_marginalization).
narrative_ontology:affects_constraint(institutional_epistemic_authority_concentration, verification_bottleneck).

% DUAL FORMULATION NOTE:
% Institutional epistemic authority concentration is upstream to specific gatekeeping mechanisms (peer review, citation networks, credentialing hierarchies). Each downstream constraint has its own extractiveness value reflecting domain-specific factors. This constraint represents the structural architecture that enables the downstream extraction mechanisms to function. The upstream/downstream relationship is causal: institutional authority concentration is a necessary condition for peer review gatekeeping, citation monopolization, and credentialing hierarchy to function as extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_epistemic_authority_concentration, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
