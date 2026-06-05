% ============================================================================
% CONSTRAINT STORY: happiness_of_others
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_happiness_of_others, []).

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
 *   constraint_id: happiness_of_others
 *   human_readable: The Social Responsibility for the Happiness of Others
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The social norm that one is responsible for, and can directly cause, the
 *   happiness of others represents a foundational constraint in intimate
 *   relationships, families, and broader social contexts. This constraint
 *   operates by establishing an asymmetric causal relationship — that the
 *   psychological state of one agent directly determines the wellbeing of
 *   another — despite empirical evidence that sustained happiness is
 *   primarily determined by genetic set-point, internal disposition, and
 *   circumstantial factors beyond any individual's control. The constraint
 *   extracts emotional labor from those who internalize the responsibility
 *   norm (emotional laborers, caregivers, empathic agents) while conferring
 *   benefits on those who can position themselves as happiness-demanders and
 *   on institutions (therapeutic, entertainment, relationship-advice
 *   industries) that profit from anxiety about happiness production. The norm
 *   persists through a combination of genuine but overstated causal claims
 *   (interpersonal relationships do influence mood and wellbeing, but not to
 *   the degree the norm suggests), theatrical narratives (romantic love
 *   mythology, therapeutic discourse affirming individual agency), and
 *   suppression of the alternative norm that one is responsible only for
 *   one's own emotional honesty, not others' happiness states. The
 *   theater_ratio has increased over the 50-year interval as self-help and
 *   therapeutic industries have professionalized and monetized the anxiety
 *   around happiness production, while the constraint's empirical foundation
 *   (causal influence on sustained happiness) has become increasingly
 *   contested in neuroscience and psychology.
 *
 * KEY AGENTS:
 *   - Emotional Laborers: Primary victims (powerless/trapped) — internalize the responsibility norm and experience chronic guilt and anxiety about their causal influence on others' happiness; bear unlimited emotional obligation with minimal causal control.
 *   - Empathic Agents: Primary victims (moderate/constrained) — possess genuine empathic capacity but are exploited by the constraint that frames empathy as causality and empathic feeling as responsibility; face social punishment for boundary-setting.
 *   - Emotional Demanders: Primary beneficiaries (institutional/arbitrage) — position others as responsible for their happiness; extract emotional labor and validation; benefit from the constraint's suppression of refusal.
 *   - Therapeutic Industries: Secondary beneficiaries (institutional/arbitrage) — profit from the anxiety and guilt produced by the constraint; monetize the desire to 'fix' oneself to make others happy or to stop others from being unhappy.
 *   - Cultural Establishment: Institutional actor (institutional/arbitrage) — maintains the norm through entertainment, relationship ideology, self-help narratives; sees constraint as degraded (Piton perspective) because empirical foundations are weak but institutional inertia sustains it.
 *   - Boundary-Setting Individuals: Secondary agents (moderate/mobile) — challenge the responsibility norm and experience both social backlash and genuine relational benefits from clarity; demonstrate that alternative norms are viable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(happiness_of_others, 0.58).
domain_priors:suppression_score(happiness_of_others, 0.72).
domain_priors:theater_ratio(happiness_of_others, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(happiness_of_others, extractiveness, 0.58).
narrative_ontology:constraint_metric(happiness_of_others, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(happiness_of_others, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(happiness_of_others, snare).
narrative_ontology:human_readable(happiness_of_others, "The Social Responsibility for the Happiness of Others").
narrative_ontology:topic_domain(happiness_of_others, "social/psychological").

domain_priors:requires_active_enforcement(happiness_of_others).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(happiness_of_others, emotional_demanders).
narrative_ontology:constraint_beneficiary(happiness_of_others, therapeutic_industries).
narrative_ontology:constraint_victim(happiness_of_others, emotional_laborers).
narrative_ontology:constraint_victim(happiness_of_others, empathic_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EMOTIONAL LABORER (SNARE) — Trapped in the belief that their psychological state determines others' happiness. Bears infinite responsibility for others' emotional states while having minimal causal control. Suppression: no socially acceptable exit without guilt or shame. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(happiness_of_others, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE GUILT-BOUND CAREGIVER (SNARE) — Constrained by emotional obligation and family dynamics. Experiences the responsibility as non-negotiable; exit is socially sanctioned only through additional guilt and emotional debt. Limited access to language for refusing happiness-production work. d≈0.85, f(d)≈1.22, σ=0.8 → χ≈0.57.
constraint_indexing:constraint_classification(happiness_of_others, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE THERAPEUTIC INDUSTRY (ROPE) — Benefits from the constraint by professionalizing emotional labor and monetizing the anxiety around happiness responsibility. Sees the norm as a coordination mechanism: helping people understand (and pay for) their role in others' happiness. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary; constraint subsidizes therapeutic markets.
constraint_indexing:constraint_classification(happiness_of_others, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE BOUNDARY-SETTING INDIVIDUAL (TANGLED ROPE) — Mobile agents who challenge the responsibility norm directly. Experiences both coordination (relationships benefit from clarified expectations) and extraction (social punishment for refusing the happiness-production role). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43. Medium effective extraction due to mobile exit option and partial social acceptance of boundary-setting.
constraint_indexing:constraint_classification(happiness_of_others, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CULTURAL ESTABLISHMENT (PITON) — Maintains the responsibility norm through entertainment, self-help narratives, and relationship ideology, despite contradictory evidence that individual happiness is primarily determined by internal factors and circumstance. The norm persists through theater: sentimental narratives, romantic mythology, therapeutic discourse that affirms individual agency over emotional states. theater_ratio=0.68 reflects the performative nature of happiness-production ideology. The cultural establishment sees this as degraded because psychology and neuroscience repeatedly demonstrate the constraint's false premises, yet the norm endures through institutional inertia.
constraint_indexing:constraint_classification(happiness_of_others, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE NEUROSCIENTIFIC OBSERVER (ANALYTICAL) — From a neurobiological perspective, individual happiness is determined primarily by internal hedonic set-point, genetic variance, and circumstantial change; interpersonal influence on another's sustained happiness is minimal and asymmetrically weak. This suggests the responsibility norm is a natural law limit: one cannot reliably cause another's happiness. However, this mountain classification is a FALSE SUMMIT. The structural metrics (ε=0.58, suppression=0.72, theater=0.68) indicate a snare, not a natural law. The constraint is a contingent social construction, not a scientific limit.
constraint_indexing:constraint_classification(happiness_of_others, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(happiness_of_others_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(happiness_of_others, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(happiness_of_others, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(happiness_of_others, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(happiness_of_others, TR),
    TR >= 0.70.

:- end_tests(happiness_of_others_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant emotional labor and produces psychological harm (anxiety, guilt, chronic responsibility for unmeasurable/uncontrollable outcomes) for those who internalize it. However, the extraction is not as severe as pure predatory mechanisms because the norm is internalized (self-enforced) and has some legitimate grounding in the fact that interpersonal relationships do influence emotional states. The 0.58 reflects this mixed causal status: genuine interpersonal influence exists but is vastly overstated by the norm. Suppression (0.72): High. Significant barriers to refusing the happiness-production role include: (1) cultural absence of an alternative norm, (2) social shame and guilt narratives that frame refusal as selfishness or cruelty, (3) family system dynamics that enforce the norm through implicit threat, (4) therapeutic language that reframes the constraint as self-improvement rather than extraction. Exit is socially available only through guilt and additional emotional debt. Theater ratio (0.68): Moderately high and increasing. The constraint operates substantially through performative narratives (romantic love myths, self-help ideology, therapeutic discourse affirming individual agency over happiness states) rather than through structural necessity. The theater has increased as therapeutic and entertainment industries have professionalized happiness anxiety, creating elaborate narratives and business models around the false premise that individuals can and should cause others' happiness. The ratio reflects that much of the norm's maintenance is narrative rather than structural — it persists because stories affirm it, not because empirical evidence supports it.
 *
 * PERSPECTIVAL GAP:
 *   The emotional laborer experiences a Snare: unlimited responsibility with zero control, suppressed exit, and chronic guilt. The boundary-setting individual experiences a Tangled Rope: genuine coordination benefits from clarity, but also social punishment and relational cost. The therapeutic industry experiences a Rope: profitable coordination mechanism that solves the 'problem' of happiness anxiety by selling solutions. The cultural establishment experiences a Piton: the constraint is performatively maintained through narrative and institutional inertia despite neuroscientific evidence that the causal premises are false. The analytical observer risks a false Mountain: seeing happiness causality as a natural law (limited interpersonal influence is inherent to consciousness), when the structural data reveals this as a contingent social construction exploiting the grain of truth that relationships do matter.
 *
 * DIRECTIONALITY LOGIC:
 *   Emotional laborers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction because they are fully internalized to the norm and have no legitimate exit. Empathic agents: Victim + constrained → d≈0.85, f(d)≈1.22. High extraction but some agency through boundary-setting and learning. Boundary-setting individuals: Mobile with beneficiary features (relational clarity) + victim features (social backlash) → d≈0.55, f(d)≈0.75. Medium extraction balanced by agency and partial norm rejection. Emotional demanders: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.08. Net beneficiary; extract without personal cost. Therapeutic industries: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; profit from the constraint's continuation. Cultural establishment: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification because of high theater, not because of low chi.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED. The constraint exhibits all snare characteristics: (1) high base extractiveness (0.58), (2) high suppression (0.72), (3) effective extraction χ ≥ 0.66 from the powerless perspective (0.58 × 1.40 × 0.8 ≈ 0.65, borderline). The mandatrophy is resolved by recognizing that the constraint's minimal coordination function (relationships do involve some mutual influence on emotional states) does not justify the norm's actual scope and intensity. The legitimate grain of truth — that we influence others' moods — is hyperextended into a false claim that we determine or are responsible for others' sustained happiness. The Snare classification captures this: it's extraction (emotional labor for unmeasurable outcomes) masked by a thin veneer of coordination (genuine relational influence). The Piton perspective (cultural establishment) sees the constraint as degraded because it maintains itself through narrative inertia despite empirical falsification, not because it serves genuine function. The false Mountain perspective reveals the risk of naturalizing the constraint: neuroscience does set limits on happiness causality, but these limits describe the problem, not justify the norm that creates the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_threshold_happiness,
    'What is the actual causal coefficient of interpersonal influence on sustained happiness relative to genetic set-point and circumstance?',
    'Longitudinal twin studies, adoptee studies, behavioral intervention meta-analyses; quantification of individual variance explained by social relationships vs. heritable factors vs. life events.',
    'If coefficient < 0.15: responsibility norm is entirely false and constraint is pure extraction (Snare confirmed). If coefficient 0.15-0.30: mixed causality, some responsibility legitimate but socially overstated (Tangled Rope for some perspectives). If coefficient > 0.30: responsibility norm has empirical basis, constraint shifts toward Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_threshold_happiness, empirical, 'The causal coefficient of interpersonal influence on sustained happiness').

omega_variable(
    exit_acceptability_variance,
    'How does cultural context affect the social acceptability of refusing the happiness-production role?',
    'Cross-cultural surveys of boundary-setting acceptance; analysis of shame/guilt narratives in different cultural contexts; rates of psychological harm from responsibility refusal.',
    'If exit broadly unacceptable: universal Snare across cultures (high suppression). If exit culturally variable: constraint is a Tangled Rope in some contexts, Rope in others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_acceptability_variance, empirical, 'Cross-cultural variation in exit acceptability for happiness-production refusal').

omega_variable(
    therapeutic_benefit_authenticity,
    'Does psychological treatment that reframes the responsibility norm as false causality actually improve outcomes, or does it simply shift guilt to new domains?',
    'Randomized controlled trials comparing responsibility-reframing therapy vs. standard CBT vs. no treatment; longitudinal follow-up on guilt/anxiety measures and relational quality.',
    'If reframing improves wellbeing: constraint is harmful fiction, Snare classification confirmed. If reframing merely displaces guilt: therapeutic industry extracts via pseudo-resolution, deepening the extraction (Piton confirmation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_benefit_authenticity, empirical, 'Whether therapeutic reframing of happiness responsibility improves psychological outcomes').

omega_variable(
    alternative_norm_viability,
    'Can functional relationships be maintained with an alternative norm: responsibility for one''s own emotional honesty, not others'' happiness states?',
    'Ethnographic study of communities that explicitly reject happiness-production responsibility; longitudinal outcomes in partnerships/families with boundary-clarity norms; qualitative analysis of relational satisfaction.',
    'If viable: constraint is contingent, not natural; Snare classification strengthens, sunset pathways possible (Scaffold analysis). If inviable: constraint has social coordination function, shifts toward Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_norm_viability, empirical, 'Viability of alternative relational norms without happiness-production responsibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(happiness_of_others, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hao_tr_t0, happiness_of_others, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hao_tr_t25, happiness_of_others, theater_ratio, 25, 0.55).
narrative_ontology:measurement(hao_tr_t50, happiness_of_others, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(hao_be_t0, happiness_of_others, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hao_be_t25, happiness_of_others, base_extractiveness, 25, 0.47).
narrative_ontology:measurement(hao_be_t50, happiness_of_others, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(happiness_of_others, enforcement_mechanism).
narrative_ontology:affects_constraint(happiness_of_others, caregiver_burnout).
narrative_ontology:affects_constraint(happiness_of_others, perfectionism_performance).
narrative_ontology:affects_constraint(happiness_of_others, relational_guilt_cycles).

% DUAL FORMULATION NOTE:
% This constraint decomposes from a natural-language concept ('social responsibility for others' happiness') into distinct causal claims: (1) that interpersonal relationships influence emotional states (true, modest effect), (2) that individuals can determine others' sustained happiness (false, overstated by 3-5x in cultural narratives), and (3) that individuals are morally responsible for outcomes they cannot control (contingent norm, not natural law). The Snare classification captures the extraction mechanism built on claim (3) combined with suppression of alternative norms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(happiness_of_others, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
