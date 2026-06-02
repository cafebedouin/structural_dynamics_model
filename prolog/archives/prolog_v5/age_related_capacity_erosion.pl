% ============================================================================
% CONSTRAINT STORY: age_related_capacity_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_age_related_capacity_erosion, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: age_related_capacity_erosion
 *   human_readable: Age-Related Capacity Erosion in Untrained Movement Patterns
 *   domain: exercise_physiology/gerontology/preventive_medicine
 *
 * SUMMARY:
 *   Age-related capacity erosion in untrained movement patterns represents a
 *   critical test case for false summit detection: a phenomenon widely
 *   perceived as immutable biological law that may be substantially
 *   modifiable through intervention. The constraint exhibits the classic
 *   false summit signature: (1) strong accessibility collapse (0.92) — the
 *   'aging is inevitable' narrative prevents consideration of alternatives,
 *   (2) identifiable beneficiaries (fitness industry, pharmaceutical
 *   companies, age-management clinics) who profit from the naturalized
 *   decline framing, (3) empirical evidence suggesting the constraint is
 *   partially constructed through systematic neglect of movement pattern
 *   diversity across the lifespan. The structural delta is progressive loss
 *   of neuromuscular capacity specifically in untrained movement patterns and
 *   planes — not global biological aging per se, but use-dependent atrophy
 *   that accumulates because modern sedentary lifestyles and conventional
 *   exercise programs fail to maintain capacity across the full movement
 *   repertoire humans are capable of. Longitudinal studies of aging athletes,
 *   dancers, and manual laborers show retained capacity in trained movement
 *   domains alongside steep decline in untrained domains within the same
 *   individuals, suggesting the loss is pattern-specific rather than
 *   domain-general. The constraint's low extractiveness (0.08) reflects that
 *   the biological component is real — some decline is genuinely inevitable —
 *   but the beneficiary presence and accessibility collapse indicate that the
 *   modifiable component is being naturalized.
 *
 * KEY AGENTS:
 *   - Aging Individuals: Primary victims (powerless/trapped) — experience capacity loss as inevitable; accessibility collapse prevents recognition of modifiable components
 *   - Healthcare Systems: Secondary victims (institutional/constrained) — bear costs of age-related disability and chronic disease that may be preventable through movement training
 *   - Fitness Industry: Primary beneficiary (institutional/arbitrage) — profits from anti-aging narrative while offering products that address symptoms rather than root cause (lack of movement pattern diversity)
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage) — profit from medical model that emphasizes pharmacological intervention over movement training
 *   - Age-Management Clinics: Primary beneficiary (institutional/arbitrage) — profit from hormone replacement and supplement protocols that treat decline as biological rather than use-dependent
 *   - Primary Care Physicians: Constrained institutional actors (moderate/constrained) — trained in pathology model; reimbursement structure and time constraints prevent movement prescription
 *   - Exercise Science Research Community: Organized agents (organized/mobile) — document both genuine biological constraints and modifiable components; career incentives favor pathology framing
 *   - Preventive Medicine Coalition: Organized agents (organized/mobile) — building alternative framework emphasizing prevention through lifespan movement training; see sunset as evidence accumulates
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing use-dependent atrophy as biological aging; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(age_related_capacity_erosion, 0.08).
domain_priors:suppression_score(age_related_capacity_erosion, 0.03).
domain_priors:theater_ratio(age_related_capacity_erosion, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(age_related_capacity_erosion, extractiveness, 0.08).
narrative_ontology:constraint_metric(age_related_capacity_erosion, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(age_related_capacity_erosion, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(age_related_capacity_erosion, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(age_related_capacity_erosion, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(age_related_capacity_erosion, mountain).
narrative_ontology:human_readable(age_related_capacity_erosion, "Age-Related Capacity Erosion in Untrained Movement Patterns").
narrative_ontology:topic_domain(age_related_capacity_erosion, "exercise_physiology/gerontology/preventive_medicine").

domain_priors:emerges_naturally(age_related_capacity_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(age_related_capacity_erosion, fitness_industry).
narrative_ontology:constraint_beneficiary(age_related_capacity_erosion, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(age_related_capacity_erosion, age_management_clinics).
narrative_ontology:constraint_victim(age_related_capacity_erosion, aging_individuals).
narrative_ontology:constraint_victim(age_related_capacity_erosion, healthcare_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AGING INDIVIDUAL (MOUNTAIN) — Experiences capacity loss as inevitable biological decline. No exit from aging process. Perceives the constraint as an immutable law of nature: 'You can't stop getting old.' Maximum accessibility collapse — the framing that decline is natural and universal prevents consideration of alternatives.
constraint_indexing:constraint_classification(age_related_capacity_erosion, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: PRIMARY CARE PHYSICIAN (MOUNTAIN) — Trained in pathology model where aging is degenerative process. Constrained by time, reimbursement structure, and medical education that emphasizes pharmacological intervention over movement training. Sees capacity loss as natural law with pharmaceutical management as only intervention.
constraint_indexing:constraint_classification(age_related_capacity_erosion, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FITNESS INDUSTRY (ROPE) — Benefits from framing age-related decline as natural but modifiable through products and services. Experiences constraint as coordination opportunity: selling anti-aging programs, supplements, specialized equipment. Net beneficiary of the naturalized decline narrative.
constraint_indexing:constraint_classification(age_related_capacity_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EXERCISE SCIENCE RESEARCH COMMUNITY (TANGLED ROPE) — Organized researchers see both genuine biological constraints (sarcopenia, neural degradation) AND modifiable components (use-dependent atrophy in untrained planes). Benefits from research funding tied to 'aging crisis' while also documenting that much decline is preventable. Mixed coordination (advancing knowledge) and extraction (career incentives favor pathology framing over prevention).
constraint_indexing:constraint_classification(age_related_capacity_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PREVENTIVE MEDICINE COALITION (SCAFFOLD) — Public health advocates, movement specialists, and preventive care practitioners building alternative framework: 'age-related decline' is substantially use-dependent atrophy that can be prevented through lifespan movement training. See current pathology model as temporary coordination failure with sunset as evidence accumulates. Organized agents with exit path toward prevention-first paradigm.
constraint_indexing:constraint_classification(age_related_capacity_erosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational perspective, some capacity loss with aging is genuinely biological: cellular senescence, mitochondrial dysfunction, telomere shortening, hormonal changes. These are immutable constraints on human lifespan and peak performance. However, the analytical classification risks naturalizing what is substantially use-dependent: the constraint conflates inevitable biological aging with preventable disuse atrophy. The engine's false summit detector will evaluate whether the beneficiary presence indicates naturalization of a modifiable phenomenon.
constraint_indexing:constraint_classification(age_related_capacity_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(age_related_capacity_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(age_related_capacity_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(age_related_capacity_erosion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(age_related_capacity_erosion, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(age_related_capacity_erosion, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(age_related_capacity_erosion, ExtMetricName, E),
    domain_priors:suppression_score(age_related_capacity_erosion, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(age_related_capacity_erosion),
    narrative_ontology:constraint_metric(age_related_capacity_erosion, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(age_related_capacity_erosion, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(age_related_capacity_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint has a genuine biological component — cellular senescence, mitochondrial dysfunction, hormonal changes are real and largely irreversible. However, current evidence suggests 50-70% of measured capacity loss is use-dependent atrophy in untrained movement patterns, which is preventable and partially reversible through comprehensive movement training. The extractiveness reflects the gap between the modifiable component and the naturalized framing: institutional beneficiaries profit from treating decline as inevitable rather than preventable. The value is low because the biological floor is real — this is not pure extraction like a debt trap. Suppression (0.03): Very low. No active enforcement mechanism prevents individuals from maintaining movement capacity. The constraint emerges from passive neglect (sedentary lifestyles, narrow exercise programs) rather than coercion. Individuals face practical barriers (time, knowledge, access to training) but not structural prohibition. Theater ratio (0.15): Low but non-zero. Some performative activity exists: anti-aging supplements with minimal efficacy, fitness programs marketed as 'functional' that train only sagittal plane movements, medical screenings that detect decline without offering effective intervention. The theater has increased slightly over the interval as the anti-aging industry has grown, but remains low because the biological component is real and measurable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the false summit pattern: the powerless agent (aging individual) and the analytical observer both classify as mountain, perceiving the constraint as immutable biological law, while organized agents with domain expertise (exercise science researchers, preventive medicine coalition) see substantial modifiable components. The gap reveals that accessibility collapse operates at multiple levels: the aging individual cannot imagine alternatives because the decline narrative is culturally ubiquitous; the analytical observer cannot see alternatives because the biological component is real and the use-dependent component requires longitudinal intervention studies to detect. The fitness industry sees rope (coordination opportunity) because they profit from the naturalized narrative. The primary care physician sees mountain because medical training emphasizes pathology over prevention. The exercise science community sees tangled rope because they document both genuine biological constraints and modifiable components, with career incentives favoring the pathology framing. The preventive medicine coalition sees scaffold because they are building the alternative paradigm and see a sunset as evidence accumulates. The perspectival diversity reflects that the constraint has both genuine biological components (mountain from some angles) and constructed institutional components (false summit from others).
 *
 * DIRECTIONALITY LOGIC:
 *   The aging individual (powerless/trapped/victim) experiences maximum accessibility collapse — the 'aging is inevitable' narrative is so deeply internalized that alternatives are literally unthinkable from within the frame. This produces high d (victim + trapped) but the constraint's low base extractiveness means even high d yields low chi. The primary care physician (moderate/constrained/victim) is structurally positioned between the patient and the institutional beneficiaries: trained in a medical model that emphasizes pharmacological intervention, constrained by reimbursement structures that don't compensate movement prescription, but not directly profiting from the decline narrative. The fitness industry (institutional/arbitrage/beneficiary) experiences the constraint as coordination opportunity: the naturalized decline narrative creates demand for anti-aging products and services. Low d (beneficiary + arbitrage) yields negative chi — they extract value from the constraint. The exercise science research community (organized/mobile/both) occupies a mixed position: benefits from research funding tied to the 'aging crisis' (beneficiary component) while documenting that much decline is preventable (victim component — their findings threaten the funding narrative). The preventive medicine coalition (organized/mobile/beneficiary) sees the constraint as temporary coordination failure with a sunset — they benefit from the emerging prevention paradigm. The analytical observer (analytical/analytical) risks naturalizing the constraint by conflating inevitable biological aging with preventable disuse atrophy — classic false summit pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   FALSE SUMMIT CANDIDATE: This constraint resolves the mandatrophy by revealing that the mountain classification at the powerless and analytical perspectives is a naturalization of a substantially modifiable phenomenon. The constraint has a genuine biological floor — some capacity loss with aging is inevitable — but the measured decline in population studies conflates this floor with use-dependent atrophy in untrained movement patterns. The beneficiary presence (fitness industry, pharmaceutical companies, age-management clinics) indicates that institutional actors profit from the naturalized decline narrative. The accessibility collapse (0.92) indicates that the 'aging is inevitable' framing prevents consideration of alternatives. The omega variables document the empirical uncertainties: What proportion of decline is biological vs. use-dependent? Is loss domain-general or pattern-specific? Is intervention effective across the lifespan or only during a critical period? Current evidence suggests 50-70% of measured decline is preventable, making this a contested boundary rather than a clear mountain or clear snare. The engine's false summit detector will evaluate whether the beneficiary presence and accessibility collapse indicate that a modifiable constraint is being naturalized as biological law. The constraint is not a pure mountain (some decline is genuinely inevitable) and not a pure snare (no active coercion), but a hybrid where institutional beneficiaries profit from conflating the inevitable component with the preventable component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_vs_disuse_partition,
    'What proportion of measured age-related capacity loss is irreversible biological aging versus reversible disuse atrophy in untrained movement patterns?',
    'Longitudinal intervention studies comparing capacity trajectories in age-matched cohorts: (1) sedentary controls, (2) conventional exercise (walking, light resistance), (3) comprehensive movement training across all planes and patterns. Measure capacity retention/recovery across movement domains.',
    'If >70% is disuse: mountain classification is false summit — constraint is substantially modifiable through lifespan training. If <30% is disuse: mountain classification is accurate — biological limits dominate. Current evidence suggests 50-70% is disuse, making this a contested boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_vs_disuse_partition, empirical, 'Partition between irreversible biological aging and reversible disuse atrophy').

omega_variable(
    movement_pattern_specificity,
    'Is capacity loss domain-general (global aging) or pattern-specific (use-dependent atrophy in untrained movements)?',
    'Cross-sectional studies of aging athletes, dancers, martial artists, and manual laborers: measure capacity retention in trained vs untrained movement domains within same individuals. If trained patterns show minimal decline while untrained patterns show steep decline, loss is use-dependent rather than global.',
    'If pattern-specific: supports scaffold perspective — decline is preventable through comprehensive movement training. If domain-general: supports mountain perspective — decline is biological and universal. Evidence from aging athletes (retained capacity in sport-specific patterns, lost capacity in untrained patterns) suggests pattern-specificity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(movement_pattern_specificity, empirical, 'Whether capacity loss is domain-general or pattern-specific').

omega_variable(
    critical_period_hypothesis,
    'Is there a critical period (age 40-60) where intervention prevents decline, or is capacity recoverable at any age through training?',
    'Intervention studies in 60+, 70+, and 80+ populations measuring capacity recovery in previously untrained movement patterns. Compare recovery rates and ceiling effects across age cohorts.',
    'If critical period exists: early intervention is essential, late intervention has limited value. If no critical period: capacity is recoverable at any age, undermining ''inevitable decline'' narrative. Current evidence shows significant recovery even in 80+ populations, but with lower ceilings than younger cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_period_hypothesis, empirical, 'Whether intervention has critical period or is effective across lifespan').

omega_variable(
    pharmaceutical_vs_movement_efficacy,
    'Do pharmaceutical interventions (hormone replacement, supplements, anti-aging compounds) produce capacity retention comparable to movement training?',
    'Head-to-head trials comparing functional capacity outcomes: (1) pharmaceutical intervention alone, (2) movement training alone, (3) combined intervention, (4) control. Measure across multiple movement domains, not just single-plane strength or endurance.',
    'If pharmaceutical efficacy is comparable: supports medical model and institutional beneficiaries. If movement training is superior: reveals pharmaceutical focus as extraction mechanism that diverts resources from effective intervention. Current evidence shows movement training produces superior functional outcomes, but pharmaceutical interventions receive disproportionate research funding and clinical emphasis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_vs_movement_efficacy, empirical, 'Comparative efficacy of pharmaceutical vs movement interventions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(age_related_capacity_erosion, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arce_theater_1950, age_related_capacity_erosion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arce_theater_1975, age_related_capacity_erosion, theater_ratio, 25, 0.12).
narrative_ontology:measurement(arce_theater_2000, age_related_capacity_erosion, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(arce_extract_1950, age_related_capacity_erosion, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(arce_extract_1975, age_related_capacity_erosion, base_extractiveness, 25, 0.06).
narrative_ontology:measurement(arce_extract_2000, age_related_capacity_erosion, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(age_related_capacity_erosion, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a single structural phenomenon (age-related capacity loss) that decomposes into two components with different epsilon values: (1) inevitable biological aging (epsilon ~0.02, genuine mountain), (2) use-dependent atrophy in untrained patterns (epsilon ~0.40-0.50, tangled rope or scaffold depending on perspective). The current story models the conflated phenomenon as presented in mainstream discourse. A full decomposition would require separate stories for the biological floor and the use-dependent component, linked via network.affects_constraints. The biological floor story would have no beneficiaries (genuine natural law). The use-dependent atrophy story would have the same beneficiaries as this story and would classify as tangled rope or scaffold from most perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
