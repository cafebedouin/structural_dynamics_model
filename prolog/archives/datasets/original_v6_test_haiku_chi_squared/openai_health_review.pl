% ============================================================================
% CONSTRAINT STORY: openai_health_review
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_openai_health_review, []).

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
 *   constraint_id: openai_health_review
 *   human_readable: OpenAI ChatGPT Health Review Service
 *   domain: technological/economic/healthcare
 *
 * SUMMARY:
 *   OpenAI's ChatGPT health review service creates a structural extraction
 *   mechanism by placing unverified large language model advice in a domain
 *   (healthcare) where verification requires expert human judgment and
 *   carries significant liability. The constraint combines: (1) a genuine
 *   coordination function (providing accessible health review at scale), (2)
 *   severe suppression of alternatives (no comparable low-cost service), (3)
 *   differential extraction targeting health-literate users who can verify
 *   advice vs. health-illiterate users who cannot, and (4) a regulatory gap
 *   where traditional medical liability frameworks (informed consent, duty of
 *   care) cannot enforce because the advice originates from an AI, not a
 *   licensed physician. The service exhibits characteristics of a snare (for
 *   powerless/health-illiterate users), a tangled rope (for
 *   moderately-informed users balancing benefit and risk), a rope (from
 *   OpenAI's perspective as coordinator), and a piton (regulatory framework
 *   performing medical oversight theater without actual enforcement). The
 *   theater ratio (0.64) reflects the proliferation of disclaimers,
 *   terms-of-service waivers, and appeals to FDA oversight — performative
 *   medical regulation without substantive verification.
 *
 * KEY AGENTS:
 *   - Health Literacy Dependent Users: Primary victims (powerless/trapped) — cannot verify LLM advice; face medical risk and sunk cost if recommendations are incorrect
 *   - Chronically Ill Patients: Secondary victims (moderate/constrained) — benefit from accessible health review but face behavioral trap: convenient advice is unreliable but hard to ignore
 *   - Medical Liability Commons: Institutional victim (powerless/trapped) — absorbs liability costs when LLM health advice causes harm; cannot exit or organize
 *   - OpenAI: Primary beneficiary (institutional/arbitrage) — captures subscription revenue and user data; solves genuine health advisory coordination problem
 *   - Regulatory Medical Authority (FDA, medical boards): Inert institutional actor (institutional/arbitrage) — maintains nominal gatekeeping role but cannot enforce standards; persists through theater
 *   - Patient Advocacy / Public Health Coalition: Organized secondary actor (organized/constrained) — can constrain through litigation and regulation; sees sunset (eventual regulatory tightening)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(openai_health_review, 0.58).
domain_priors:suppression_score(openai_health_review, 0.68).
domain_priors:theater_ratio(openai_health_review, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openai_health_review, extractiveness, 0.58).
narrative_ontology:constraint_metric(openai_health_review, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(openai_health_review, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(openai_health_review, snare).
narrative_ontology:human_readable(openai_health_review, "OpenAI ChatGPT Health Review Service").
narrative_ontology:topic_domain(openai_health_review, "technological/economic/healthcare").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(openai_health_review, openai).
narrative_ontology:constraint_beneficiary(openai_health_review, subscription_tier_purchasers).
narrative_ontology:constraint_victim(openai_health_review, health_literacy_dependent_users).
narrative_ontology:constraint_victim(openai_health_review, users_with_chronic_conditions).
narrative_ontology:constraint_victim(openai_health_review, medical_liability_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEALTH LITERACY DEPENDENT USER (SNARE) — Users with low health literacy or chronic conditions face severe extraction. Cannot verify the validity of LLM-generated health advice; must either follow recommendations (medical risk) or ignore them (sunk cost). No alternative affordable health review service at comparable speed/cost. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MEDICAL LIABILITY COMMONS (SNARE) — The liability infrastructure (medical truth standards, legal duty of care) cannot organize or exit; bears the cost of LLM hallucinations manifesting as medical harm. When ChatGPT gives incorrect health advice, the commons absorbs liability friction without recourse. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(openai_health_review, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CHRONICALLY ILL USER / PATIENT ADVOCATE (TANGLED ROPE) — Some users benefit from the coordination function (24/7 health review without gatekeeping delay); also harmed by LLM unreliability and psychological pressure to trust AI-generated advice. Constrained exit: cannot fully abandon (too convenient) but cannot fully rely (too risky). d≈0.68, f(d)≈0.98, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(openai_health_review, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: OPENAI / SUBSCRIPTION BUSINESS (ROPE) — Captures pure coordination benefit: solves the problem of translating health test results into actionable lifestyle guidance. Users want this service; OpenAI solves it. The service has a genuine coordination function (reducing information asymmetry). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative effective extraction = beneficiary.
constraint_indexing:constraint_classification(openai_health_review, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY MEDICAL AUTHORITY (PITON) — Medical licensing and liability frameworks (FDA, medical boards, informed consent law) persists as inert theater. LLM health advisories operate in a regulatory gap where traditional medical liability standards are performatively invoked but not enforceable. Regulatory authority cannot update fast enough; maintains nominal gatekeeping through warnings while the actual verification mechanism (human medical review) is bypassed. theater_ratio=0.64 (warnings, disclaimers, appeals to FDA oversight that does not actually occur).
constraint_indexing:constraint_classification(openai_health_review, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC HEALTH / PATIENT SAFETY COALITION (SCAFFOLD) — Organized actors (patient advocacy groups, medical associations, public health agencies) see this as a temporary problem with a regulatory sunset: AI-generated medical advice will eventually be regulated, restricted, or integrated into formal medical workflows with legal accountability. The coalition can constrain the service through litigation, regulation, and norm-shifting (e.g., 'AI health advice is not a substitute for medical review'). d≈0.38, f(d)≈0.38, σ=1.0 → χ≈0.14. Low effective extraction because coalition has agency and sees an exit path (regulatory tightening).
constraint_indexing:constraint_classification(openai_health_review, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW HYPOTHESIS (MOUNTAIN) — From a civilizational perspective, the gap between human medical expertise and scalable health advisory creates an inherent natural constraint: any attempt to automate diagnosis or treatment recommendation will always face the principal-agent problem of verification (how to know the advice is safe without expert review?). This perspective risks naturalizing what is actually a contingent institutional choice: to permit unverified LLM advice in a liability-critical domain. Accessibility_collapse and resistance thresholds do NOT apply here; this is a false summit (the structural data contradicts the mountain classification).
constraint_indexing:constraint_classification(openai_health_review, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_health_review_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(openai_health_review, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_health_review, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(openai_health_review, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(openai_health_review, TR),
    TR >= 0.70.

:- end_tests(openai_health_review_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. OpenAI captures subscription revenue and user health data while users bear medical risk from potentially incorrect LLM-generated advice. The extraction is not maximal (0.70+) because the service has a genuine coordination function — users do benefit from accessible health guidance — and the magnitude of harm varies by user literacy. The value reflects that this is a mixed coordination-extraction hybrid, not pure extraction. Suppression (0.68): High. Barriers to exit include: (a) no comparable low-cost health advisory service, (b) information asymmetry (users cannot easily verify LLM advice accuracy), (c) psychological dependence (convenient advice is hard to ignore even if unreliable), (d) regulatory gaps (medical liability frameworks don't apply). Theater ratio (0.64): Moderate-high. The service is surrounded by performative regulation: disclaimers, terms of service, appeals to FDA oversight, physician-speak in the interface — but the actual verification mechanism (human medical expert review) is bypassed. As time progresses, the theater increases because disclaimers must become more elaborate as harm cases accumulate.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Health-illiterate users see a pure snare (no exit, high extraction, no coordination benefit). OpenAI sees a rope (solving a coordination problem). Regulatory authorities perform oversight theater (piton). Public health advocates see a temporary extraction with a sunset (scaffold). The analytical observer risks seeing an immutable constraint of medical knowledge (mountain) — the inherent difficulty of automating diagnosis — but the structural data (ε=0.58, suppression=0.68, theater=0.64) reveals this as a false summit: the 'difficulty' is actually a contingent institutional choice to permit unverified LLM advice in a liability-critical domain. If the constraint were truly natural law, it would classify identically across all perspectives; the perspectival range here (snare → rope → scaffold → piton) indicates a social/institutional constraint, not a physical limit.
 *
 * DIRECTIONALITY LOGIC:
 *   Health literacy dependent users: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Medical liability commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Chronically ill users (moderate/organized): Victim + constrained → d≈0.68, f(d)≈0.98. Mixed; some benefit, some cost. OpenAI: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Regulatory authority: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification derives from theater_ratio gate, not directionality. Public health coalition: Organized + constrained → d≈0.38, f(d)≈0.38. Low effective extraction; coalition has agency to constrain through regulation. The critical directionality insight: the service is NOT extracted equally from all users. It targets vulnerable users (health-illiterate, chronically ill, time-constrained) who cannot verify advice and have limited alternatives. This is differential extraction, making it a snare rather than a rope.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED (ε=0.58 > 0.46 but < 0.70). The constraint must be classified as either: (a) Snare (pure extraction targeting vulnerable users), or (b) Tangled Rope (hybrid coordination + asymmetric extraction). The mandatrophy resolution hinges on whether the service is merely providing accessible health guidance (rope/tangled rope) or systematically targeting users who cannot verify the advice (snare). Three factors support snare classification: (1) Differential extraction: the service attracts users with lower health literacy who cannot verify advice. (2) Suppression: no competitive alternative provides equivalent speed/cost. (3) Behavioral trap: convenient advice is psychologically difficult to ignore even when unreliable. However, tangled rope is partially defended by the genuine coordination function (users genuinely benefit from accessible health guidance) and the fact that some users can verify advice. The mandate resolves to snare when empirical evidence shows: (i) harm rates above 5%, (ii) systematic targeting of low-literacy users, (iii) disclaimers failing to protect users from behavioral compliance. If future data shows low harm rates or strong literacy-sorting to high-literacy users, reclassification to tangled rope or rope becomes defensible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    llm_health_advice_harm_threshold,
    'What rate of harmful health advice (% of users experiencing adverse outcomes from following ChatGPT recommendations) constitutes evidence of extraction vs. acceptable risk?',
    'Longitudinal epidemiological study: track users of the service and correlate health outcomes with recommendations; adverse event reporting; user surveys on harm perception',
    'If harm rate > 5%: snare classification confirmed. If harm rate < 1%: rope classification gains plausibility (coordination function outweighs risk).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(llm_health_advice_harm_threshold, empirical, 'Adverse outcome rate from LLM health advice').

omega_variable(
    regulatory_enforcement_velocity,
    'Will regulatory oversight (FDA, medical boards) move fast enough to constrain LLM health advisory before adoption reaches critical mass?',
    'Monitoring regulatory timelines: FDA guidance on AI medical devices, state medical board actions, legislative proposals; comparison with historical medical innovation regulation cycles (3-7 years typical)',
    'If regulatory gap > 5 years: scaffold perspective is aspirational (sunset clause is not real). If regulatory response < 2 years: scaffold confirmed (sunset is structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_velocity, empirical, 'Speed of regulatory response to AI health advisory').

omega_variable(
    user_literacy_sorting,
    'Does the service primarily attract health-literate users (who can verify recommendations) or health-illiterate users (who cannot)?',
    'User survey on health literacy, medical knowledge, baseline trust in AI; demographic analysis; comparison with medical error rates across literacy quartiles',
    'If literacy-sorted to high: extraction is lower (users can verify). If sorted to low: extraction is higher (users cannot verify). This determines whether the service is differential extraction (targeting vulnerable users) or universal coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_literacy_sorting, empirical, 'User health literacy distribution and verification capacity').

omega_variable(
    medical_liability_enforcement,
    'Can users or medical authorities establish legal liability against OpenAI for health advice harm, or does the disclaimerware wall hold?',
    'Litigation outcomes; analysis of OpenAI''s terms of service and liability waivers; state-level medical practice law interpretations; precedent from other AI medical product cases',
    'If liability enforcement succeeds: OpenAI bears cost (incentive realignment). If disclaimers hold: users/commons bear cost (extraction confirmed). This is the key institutional question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_liability_enforcement, empirical, 'Enforceability of liability waiver in medical context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(openai_health_review, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ohr_tr_t0, openai_health_review, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ohr_tr_t6, openai_health_review, theater_ratio, 6, 0.59).
narrative_ontology:measurement(ohr_tr_t12, openai_health_review, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(ohr_be_t0, openai_health_review, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ohr_be_t6, openai_health_review, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(ohr_be_t12, openai_health_review, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(openai_health_review, information_standard).
narrative_ontology:affects_constraint(openai_health_review, ai_medical_licensing_gap).
narrative_ontology:affects_constraint(openai_health_review, health_data_extraction).
narrative_ontology:affects_constraint(openai_health_review, clinical_liability_commons).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the AI medical licensing gap (the institutional fact that AI systems are not required to be licensed physicians to give medical advice). The upstream constraint has ε≈0.10 (structural regulatory gap); this constraint (openai_health_review) has ε=0.58 (operational extraction enabled by the gap). The two constraints are linked: closing the licensing gap would tighten the service's suppression and reduce extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(openai_health_review, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
